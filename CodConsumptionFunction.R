
#'Calculate consumption of capelin by cod
#'@param agr is the age group of cod, e.g., 1gr, 2gr, etc.,
#'@param Size is capelin size: 0-5cm, 5-10cm, 10-15cm, 15-20cm, 20-30cm
#'@param Q is the period or quarter Q1-2 or Q3-4
#'@param VPA
#'@param Dist 
#'@param W is the weight 
#'@param WP
#'@param F(T,I)
#'@param T 
#'@param Wpred
#'@param Area is the area: There are fdistinct 3-areas-: II+IV, III+V, VI + VII
#'@param D is initial meal content (constant)
#'@param B is......(constant)
#'@param c is a constant (c =0.26)
#'@param b is  a constant (b =0.52)
#'#'@param C_a_Ai_Sj is the consumption by age-class and size-class in Area i (Area_i)
#'@param C_a_Sj is the consumption by age-class and size-class 
#'@param C_Sj is the consumption by size-class j (S_j)
#'@param C_a is the consumption by age-class
#'@param C_T is the total consumption by ages 3,4,5,6
#'
#'
library(data.table)
library(xtable)
############################
#Constants 
###########################

B = 43.8
D = 1.78
c = 0.26
b = 0.52

options(scipen = 999)
############################
# Read data files in R
###########################

#VPA
VPA <- as.data.table(read.table("VPA_file.txt", header = T, stringsAsFactors=F))
VPA[, vpa := VPA/1000]
VPA[, age := as.integer(age)]

#Wpred
Wpred <- as.data.table(read.table("Wpred_file.txt", header = T, stringsAsFactors=F))
Wpred[, Wpred := as.numeric(Wpred)]
Wpred[, age := as.integer(age)]

#VPA_Wpred
VPA_Wpred <- merge(VPA, Wpred, all = TRUE)
VPA_Wpred[Period == "01.apr", Period := "april"]
VPA_Wpred[Period == "01.okt", Period := "october"]

#Dist_W
Dist <-  as.data.table(read.table("Dist_file.txt", header = TRUE, stringsAsFactors=F))
W <-  as.data.table(read.table("W_file.txt", header = TRUE, stringsAsFactors=F))
Dist_W <- merge(Dist, W, all = TRUE)

#WP
WP <-  as.data.table(read.table("WP_file.txt", header = TRUE, stringsAsFactors=F))

#T
T <-  as.data.table(read.table("T_file.txt", header = TRUE, stringsAsFactors=F))

#FT1
FT1 <-  as.data.table(read.table("FT1.txt", header = TRUE, stringsAsFactors=F, fileEncoding="latin1"))
FT1 <- FT1[, c("Quarter", "Capelin", "Area")]


# All Merged
tmp <- merge(FT1, WP, by = c("Quarter", "Area"), all = TRUE)
tmp <- merge(tmp, T, by = c("Quarter", "Area"), all = TRUE )
tmp <- merge(tmp, Dist_W, by = c("Quarter", "age", "Area"), all = TRUE)

allMerged <- merge(tmp, VPA_Wpred, by = c("age", "Period"), all = TRUE)

# Sanity checks
## 1. Period vs Quarter (should be 2 unique combinations)
unique(allMerged[, c("Period", "Quarter")])

## 2. We use all = T when merge, check for any missing data
any(is.na(allMerged))

## 3. Check area uniqueness
unique(allMerged$Area)


# Now calculate Consumption by age, size-class, and area based on Period condition-------------

# Add Consumption column by age, area and size-class
allMerged[, C_a_Ai_Sj := Dist * W *WP * Capelin * (Wpred^{c})/(W^{b})/(D^{b})]

# compute C_a_Sj (age, size) for april or october and remove duplicated rows
C_a_Sj_april <- allMerged[Period == "april", .(C_a_Sj = sum(C_a_Ai_Sj)*vpa*B), by = c("age", "Size")]
C_a_Sj_april <- C_a_Sj_april[!duplicated(C_a_Sj_april),] 

C_a_Sj_october <- allMerged[Period == "october", .(C_a_Sj = sum(C_a_Ai_Sj)*vpa*B), by = c("age", "Size")]
C_a_Sj_october <- C_a_Sj_october[!duplicated(C_a_Sj_october),] 
merge(C_a_Sj_april, C_a_Sj_october, by = c("age", "Size"), all = TRUE)

# compute C_Sj (size) for april or october 
C_Sj_april   <- C_a_Sj_april[,.(C_Sj = sum(round(C_a_Sj),0)), by = c("Size")]
C_Sj_october <- C_a_Sj_october[,.(C_Sj = sum(round(C_a_Sj),0)), by = c("Size")]
merge(C_Sj_april, C_Sj_october, by = c("Size"), all = TRUE )



# compute C_a (age) for april or october 
C_a_april     <- C_a_Sj_april[,.(C_a = sum(round(C_a_Sj),0)), by = c("age")]
C_a_october   <- C_a_Sj_october[,.(C_a = sum(round(C_a_Sj),0)), by = c("age")]
merge(C_a_april, C_a_october , by = c("age"), all = TRUE )


# Compute total consumption by cod ages 3-6
C_T_april <- sum(C_a_april[3:6,])
C_T_october <- sum(C_a_october[3:6,])


