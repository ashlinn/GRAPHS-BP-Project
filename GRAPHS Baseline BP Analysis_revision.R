### GRAPHS Baseline BP Analysis - for revision
require(plyr)
require(dplyr)
require(reshape2)
require(lubridate)

####################### DATA ANALYSIS ------#####################
NAPerVariable <- function(x) {
  d <- is.na(x)
  e <- colSums(d)
  e
}

# requires: lastest baselinebpdata file
baselinebpdata <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/baselinedata_Sep30.rds")
nrow(baselinebpdata) #1183
length(unique(baselinebpdata$mstudyid)) #1183
NAPerVariable(baselinebpdata)

# Histogram of gestational age --------
pdf(file = "BaselineBP_gestage_n1183.pdf", width = 8, height = 6, bg = "white")
hist(baselinebpdata$gestwks, xlim = c(0,40),col = "aquamarine", main = "Gestational Age at Enrollment (n = 1183)", xlab = "Weeks of Gestation")
abline(v = mean(baselinebpdata$gestwks), col = "red", lwd = 3)
legend("topright", legend = paste0("mean (", round(mean(baselinebpdata$gestwks), digits = 1), " weeks)"), col = "red", lwd = 3, bty = "n")
dev.off()

## remove CO invalid
bad_co <- baselinebpdata[baselinebpdata$overall_valid !=1,]
nrow(bad_co) # 328
bad_table <- table(bad_co$duration, bad_co$visually_valid, bad_co$cf_conf)
ftable(bad_table)

nrow(baselinebpdata[is.na(baselinebpdata$overall_valid),]) #0 
nrow(baselinebpdata[baselinebpdata$overall_valid ==1,]) #855
nrow(baselinebpdata[baselinebpdata$overall_valid ==1 | (baselinebpdata$visually_valid == 2 & baselinebpdata$cf_conf == "hi" & baselinebpdata$duration_valid ==1 & !baselinebpdata$overall_valid ==4),]) #906
baselinebpdata <- baselinebpdata[baselinebpdata$overall_valid ==1 | (baselinebpdata$visually_valid == 2 & baselinebpdata$cf_conf == "hi" & baselinebpdata$duration_valid ==1 & !baselinebpdata$overall_valid ==4),]

table(baselinebpdata$overall_valid) # 855 1s, 50 2s
nrow(baselinebpdata) #905

# 
# # removing BP outliers and individuals who didn't cook
# 
# baselinebpdata[baselinebpdata$bp_validity ==3.5, c("sbp", "dbp")] #2: 95/162 (imposs for SBP < DBP and DBP outlying), 145/100 (DBP is outlying)
# nrow(baselinebpdata[is.na(baselinebpdata$bp_validity),]) #0
# baselinebpdata <- baselinebpdata[!baselinebpdata$bp_validity ==3.5,]
# nrow(baselinebpdata) # 903
# 
# nrow(baselinebpdata[baselinebpdata$cookingevents == 0 &!is.na(baselinebpdata$cookingevents),]) #11
# nrow(baselinebpdata[is.na(baselinebpdata$cookingevents),]) #13
# baselinebpdata[baselinebpdata$cookingevents ==0 &!is.na(baselinebpdata$cookingevents), c("cookingevents", "cookfood_days")]
# baselinebpdata <- baselinebpdata[!baselinebpdata$cookingevents == 0 | is.na(baselinebpdata$cookingevents),] 
# 
# nrow(baselinebpdata) # 892
# 
# bpdata <- baselinebpdata
# NAPerVariable(bpdata) # make sure additional NA's haven't jumped in
# 
# saveRDS(bpdata, file = paste0("bpdata", format(Sys.Date(), format = "%b%d"), ".rds")) # this dataset excludes BP outliers and those with no cooking events

# NEW for revision: leaving those who didn't cook in ----
baselinebpdata[baselinebpdata$bp_validity ==3.5, c("sbp", "dbp")] #2: 95/162 (imposs for SBP < DBP and DBP outlying), 145/100 (DBP is outlying)
nrow(baselinebpdata[is.na(baselinebpdata$bp_validity),]) #0
baselinebpdata <- baselinebpdata[!baselinebpdata$bp_validity ==3.5,]
nrow(baselinebpdata) # 903


# recode "salaryoth"
baselinebpdata[!is.na(baselinebpdata$salaryoth), c("salary", "salaryoth")]
table(baselinebpdata$salary)
baselinebpdata$salary[baselinebpdata$salaryoth == "NOT EMPLOYED" | baselinebpdata$salaryoth == "STUDENT" | baselinebpdata$salaryoth == "APPRENTICE"] <- 7 # not employed
baselinebpdata$salary[baselinebpdata$salaryoth == "BURNING CHARCOAL" | baselinebpdata$salaryoth == "BURNING OF CHARCOAL FOR SALE"] <- 5 # laborer
baselinebpdata$salary[baselinebpdata$salaryoth == "CATERER"] <- 3 # businesswoman



# # Add dummies for DOW and rainy season
bpdata <- baselinebpdata
head(bpdata$firstdate)
str(bpdata$firstdate) 
bpdata$months <- months(bpdata$firstdate)
bpdata$rainy <- ifelse(bpdata$months %in% c("April", "May", "June", "September", "October"), 1, 0)
bpdata$weekday <- as.character(wday(bpdata$firstdate, label = TRUE))# for CO
bpdata$usdatevisi <- mdy(bpdata$usdatevisi)
bpdata$weekday_bp <- as.character(wday(bpdata$usdatevisi, label = TRUE)) # for bp
bpdata$tobacco_bin <- ifelse(bpdata$tobacco > 0, 1, 0)


# recode salary (combine 1 and 2)
bpdata$salary[bpdata$salary == 2] <- 1
bpdata$salary <- as.factor(bpdata$salary)



saveRDS(bpdata, file = paste0("bpdata", format(Sys.Date(), format = "%b%d"), ".rds")) # this dataset excludes BP outliers but those who didn't cook are left in


### ANALYSIS --------
# Remove extraneous variables -------

# new revised with women who didn't cook left in
bpdata <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/bpdataSep30.rds")



data <- subset(bpdata, select = c("vil_code", "mstudyid", "sbp", "dbp", "bp", "diabetes", "smokecur", "smokpast", "shs", "age", "medlev", "married", "salary", "comptype", "ownhouse", "mean_corr", "q98_corr", "hours", "firstdate", "BMI", "gestwks", "asset_index", "cookingevents", "charcoal", "otherstoveuse", "mosqcoil", "tobacco", "mill", "threestone", "coalpot", "roadsale", "makecharcoal", "anthrop_validity", "bp_validity", "onelsecok", "coils", "invcom_sum", "invcoal_sum", "oth_smksources",  "vil_on_road", "people_all", "q90_corr", "overall_valid", "months", "rainy", "weekday", "weekday_bp", "tobacco_bin", "hours", "fuel1", "fuel2", "whercok1", "whercok2"))

summary(data$hours)
nrow(data) #903
nrow(data[data$hours >= 64.8,]) #880
880/903 #97.5 % at least 90% of 72-hour target

# 
# 
NAPerVariable(data) # 16 missing all stuff on econ form, 29 missing asset index, 18 missing other smoke sources
# 
# 
# 





# Cluster-robust standard errors --------
# from https://thetarzan.wordpress.com/2011/06/11/clustered-standard-errors-in-r/
# In R, you first must run a function here called cl() written by Mahmood Arai in Stockholm University – the backup can be found here and here. http://people.su.se/~ma/clustering.pdf
# http://www.ne.su.se/polopoly_fs/1.216115.1426234213!/menu/standard/file/clustering1.pdf

cl   <- function(data,fm, cluster){ # fm is the "fitted model" from a linear regression; use $ in the cluster ID
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL) }

# After running the code above, you can run your regression with clustered standard errors as follows:
# Run a plain linear regression
# regt = lm(nevermar ~ impdum, data = nmar)
# 
# # apply the 'cl' function by choosing a variable to cluster on.
# # here, we are clustering on state.
# cl(data = nmar, fm = regt, cluster = nmar$state)





# check for BMI outliers and set to NA
summary(data$BMI)
summary(data$anthrop_validity) # max is 2: ok








## CONFIDENCE INTERVALS-----
# 95% CI: coef +/- SE*t^alpha/2,n-k-1
# unadjusted: n = 855, k = 1
# t^0.025,816 = 1.963
# adjusted: n = 855, k = 6
# t^0.025,848 = 1.963 (same)

### FINAL MODEL -------

data <- subset(data, select = c("mstudyid", "sbp", "dbp", "mean_corr", "age", "BMI", "gestwks", "asset_index", "tobacco_bin", "vil_code", "overall_valid", "rainy", "weekday", "weekday_bp", "salary", "vil_on_road", "cookingevents"))

nrow(data) #903
nrow(data[data$overall_valid ==1,]) #853
# Unadjusted final model ------
bpdata_complete <- data[complete.cases(data),] 


nrow(bpdata_complete) #866 (includes some validity = 2's - for sensitivity)

bpdata_valid <- bpdata_complete[bpdata_complete$overall_valid ==1,]
nrow(bpdata_valid) # 817

# check which variables are associated with BP at p = 0.1
summary(fm <- lm(sbp~age, data = bpdata_valid)) # sig
summary(fm <- lm(sbp~BMI, data = bpdata_valid)) # sig
summary(fm <- lm(sbp~gestwks, data = bpdata_valid)) # sig
summary(fm <- lm(sbp~tobacco_bin, data = bpdata_valid)) # sig
summary(fm <- lm(sbp~weekday, data = bpdata_valid)) # one is sig

summary(fm <- lm(sbp~vil_on_road, data = bpdata_valid)) # not sig
summary(fm <- lm(sbp~vil_on_road, data = bpdata_valid)) # not sig
summary(fm <- lm(sbp~rainy, data = bpdata_valid)) #not sig

summary(fm <- lm(sbp~weekday_bp, data = bpdata_valid)) # not sig
summary(fm <- lm(sbp~salary, data = bpdata_valid)) # not sig
summary(fm <- lm(sbp~asset_index, data = bpdata_valid)) # not sig

summary(fm <- lm(dbp~age, data = bpdata_valid)) # not sig
summary(fm <- lm(dbp~BMI, data = bpdata_valid)) # sig
summary(fm <- lm(dbp~gestwks, data = bpdata_valid)) # sig
summary(fm <- lm(dbp~tobacco_bin, data = bpdata_valid)) # sig
summary(fm <- lm(dbp~weekday_bp, data = bpdata_valid)) #  sig

summary(fm <- lm(dbp~rainy, data = bpdata_valid)) #not sig
summary(fm <- lm(dbp~weekday, data = bpdata_valid)) # not sig
summary(fm <- lm(dbp~weekday_bp, data = bpdata_valid)) #  sig
summary(fm <- lm(dbp~salary, data = bpdata_valid)) # not sig
summary(fm <- lm(dbp~asset_index, data = bpdata_valid)) # not sig
summary(fm <- lm(dbp~vil_on_road, data = bpdata_valid)) # not sig

# ones to definitely include: age, BMI, gestwks, tobacco_bin, weekday_bp
# ones to check influence in model: rainy, weekday, salary, asset_index, vil_on_road


plot(bpdata_valid$mean_corr, bpdata_valid$sbp)
plot(bpdata_valid$mean_corr, bpdata_valid$dbp)
bpdata_valid$mean_corr[which.max(bpdata_valid$mean_corr)] #15.43 - check later that this is not driving the results

plot(density(bpdata_valid$mean_corr))

# UNADJUSTED MODELS ------
fm <- lm(sbp ~ mean_corr, data = bpdata_valid)
cl(fm = fm, data = bpdata_valid, cluster = bpdata_valid$vil_code) # for cluster-robust standard errors

#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 105.10496    0.58050 181.060  < 2e-16 ***
#   mean_corr     0.55646    0.27864   1.997  0.04615 * 

# CI: 0.55646 +/- 0.27864*1.963 [0.009, 1.10]
plot(fm)
cooksD <- cooks.distance(fm) 
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD) # 847 and 525 > 0.04
bpdata_valid_sens <- bpdata_valid[!row.names(bpdata_valid) %in% names(cooksD[cooksD > 0.04]),]
max(bpdata_valid_sens$mean_corr) #11.26
nrow(bpdata_valid_sens) # 815

fm <- lm(sbp ~ mean_corr, data = bpdata_valid_sens)
cl(fm = fm, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code) # this is probably better model

#              Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) 105.45024    0.59975 175.8232   <2e-16 ***
 #  mean_corr     0.30935    0.30532   1.0132   0.3113     
# CI: 0.30935 +/- 0.30532*1.963 [-0.28, 0.91]
plot(fm)

# sensitivity with bpdata_complete dataset
fm <- lm(sbp ~ mean_corr, data = bpdata_complete)
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code) # for cluster-robust standard errors

#              Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) 105.882970   0.416869 253.9956   <2e-16 ***
#  mean_corr     0.012504   0.131437   0.0951   0.9242    

plot(fm)
cooksD <- cooks.distance(fm) 
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD) # 513, 847, and 220 > 0.04
bpdata_complete_sens <- bpdata_complete[!row.names(bpdata_complete) %in% names(cooksD[cooksD > 0.04]),]
nrow(bpdata_complete_sens) #863

max(bpdata_complete_sens$mean_corr) #27.99

fm <- lm(sbp ~ mean_corr, data = bpdata_complete_sens)
cl(fm = fm, data = bpdata_complete_sens, cluster = bpdata_complete_sens$vil_code)

#               Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) 105.71140    0.47799 221.1597   <2e-16 ***
#  mean_corr     0.11241    0.18889   0.5951   0.5519    

# CI: 0.11241 +/- 0.18889 * 1.963 [-0.25, 0.48]

# DBP
fm <- lm(dbp ~ mean_corr, data = bpdata_valid)
cl(fm = fm, data = bpdata_valid, cluster = bpdata_valid$vil_code)
  
# Estimate Std. Error  t value  Pr(>|t|)    
# (Intercept) 62.14940    0.51453 120.7894 < 2.2e-16 ***
#   mean_corr    0.78157    0.25492   3.0659  0.002242 ** 

# CI: 0.78157 +/- 0.25492 * 1.963 [0.28, 1.28]
plot(fm)
cooksD <- cooks.distance(fm) 
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD) # 1 > 0.04,  max is 0.44, this is the maximum CO value

bpdata_valid_sens <- bpdata_valid[!row.names(bpdata_valid) %in% names(cooksD[cooksD > 0.04]),]
nrow(bpdata_valid_sens) #816

fm <- lm(dbp ~ mean_corr, data = bpdata_valid_sens)
cl(fm = fm, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code) # this is probably better model

# Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) 62.43518    0.47718 130.8418   <2e-16 ***
#  mean_corr    0.58771    0.22764   2.5818     0.01 *   
# CI: 0.58771 +/- 0.22764 * 1.963 [0.14, 1.03]
plot(fm)

# sensitivity with bpdata_complete
fm <- lm(dbp ~ mean_corr, data = bpdata_complete)
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)

#              Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) 63.00747    0.38016 165.7395   <2e-16 ***
#   mean_corr    0.24009    0.15192   1.5804   0.1144    


cooksD <- cooks.distance(fm) 
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD) # 4 > 0.04,  max is 0.32: 220, 513, 847, 780

bpdata_complete_sens <- bpdata_complete[!row.names(bpdata_complete) %in% names(cooksD[cooksD > 0.04]),]
nrow(bpdata_complete_sens) #862
fm <- lm(dbp ~ mean_corr, data = bpdata_complete_sens)
cl(fm = fm, data = bpdata_complete_sens, cluster = bpdata_complete_sens$vil_code)

# Estimate Std. Error  t value  Pr(>|t|)    
# (Intercept) 62.46026    0.43341 144.1151 < 2.2e-16 ***
#  mean_corr    0.57668    0.18199   3.1688  0.001585 ** 

# CI: 0.57668 +/- 0.18199 * 1.963 [0.22, 0.93]
plot(fm)


# Adjusted  model --------
# SBP
# ones to definitely include: age, BMI, gestwks, tobacco_bin, weekday_bp
# ones to check influence in model: rainy, salary, asset_index, vil_on_road

fm <- lm(sbp ~ mean_corr + age + BMI + gestwks + tobacco_bin + weekday_bp, data = bpdata_valid) 
cl(fm = fm, data = bpdata_valid, cluster = bpdata_valid$vil_code)

# coef: 0.440
.440-.0440 # 0.396


plot(fm)
cooksD <- cooks.distance(fm)
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD) 
bpdata_valid_sens <- bpdata_valid[!row.names(bpdata_valid) %in% names(cooksD[cooksD > 0.04]),] # none
nrow(bpdata_valid_sens) #817

fm <- lm(sbp ~ mean_corr + age + BMI + gestwks + tobacco_bin + weekday_bp, data = bpdata_valid_sens) 
cl(fm = fm, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code)

# Estimate Std. Error t value  Pr(>|t|)    
# (Intercept)     94.358737   3.176785 29.7026 < 2.2e-16 ***
#   mean_corr        0.440709   0.258685  1.7036 0.0888321 .  
# age             -0.172676   0.049975 -3.4552 0.0005786 ***
#   BMI              0.795700   0.106530  7.4692 2.099e-13 ***
#   gestwks         -0.219679   0.096088 -2.2862 0.0225001 *  
#   tobacco_bin      1.933073   0.680699  2.8398 0.0046274 ** 
#   weekday_bpMon    0.471100   0.849341  0.5547 0.5792774    
# weekday_bpSat    6.565229   0.815757  8.0480 3.008e-15 ***
#   weekday_bpThurs  1.078665   1.204166  0.8958 0.3706389    
# weekday_bpTues   1.225263   0.978499  1.2522 0.2108651    
# weekday_bpWed   -2.661255   1.236950 -2.1515 0.0317360 *  

# CI  0.440709 +/- 0.258685*1.963 [-0.06, 0.94]

.440-.0440 # .396
fm1 <- update(fm, .~. + rainy)
cl(fm = fm1, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code) # coef = 0.441

fm1 <- update(fm, .~. + salary)
cl(fm = fm1, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code) # coef = 0.389*

fm1 <- update(fm, .~. + asset_index)
cl(fm = fm1, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code) # coef = 0.447

fm1 <- update(fm, .~. + vil_on_road)
cl(fm = fm1, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code) # coef = 0.441

# salary changed it more than 10%

fm1 <- update(fm, .~. + salary)
cl(fm = fm1, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code) 

# Estimate Std. Error t value  Pr(>|t|)    
# (Intercept)     93.138621   4.776321 19.5001 < 2.2e-16 ***
#   mean_corr        0.389998   0.259316  1.5040  0.132987    
# age             -0.150018   0.059430 -2.5243  0.011786 *  
#   BMI              0.778011   0.105665  7.3630 4.465e-13 ***
#   gestwks         -0.235393   0.096647 -2.4356  0.015084 *  
#   tobacco_bin      1.988731   0.673641  2.9522  0.003247 ** 
#   weekday_bpMon    0.676527   0.879117  0.7696  0.441791    
# weekday_bpSat    7.444213   0.850129  8.7566 < 2.2e-16 ***
#   weekday_bpThurs  0.715782   1.234546  0.5798  0.562217    
# weekday_bpTues   1.110648   0.941793  1.1793  0.238632    
# weekday_bpWed   -2.718875   1.228835 -2.2126  0.027209 *  
#   salary3          2.470428   3.235788  0.7635  0.445408    
# salary4         -0.433602   3.216966 -0.1348  0.892815    
# salary5          0.438977   2.981986  0.1472  0.883004    
# salary7          2.361241   3.020102  0.7818  0.434539    

# CI: 0.389998 +/- 0.259316*1.963 [-0.12, 0.90]
plot(fm1)
cooksD <- cooks.distance(fm1)
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD)  # none > 0.04

# sensitivity with bpdata_complete
fm <- lm(sbp ~ mean_corr + age + BMI + gestwks + tobacco_bin + weekday_bp + salary, data = bpdata_complete) 
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: 0.002 (p-val = 0.98)


plot(fm)
cooksD <- cooks.distance(fm)
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD) # none > 0.04
bpdata_complete_sens <- bpdata_complete[!row.names(bpdata_complete) %in% names(cooksD[cooksD > 0.04]),]
nrow(bpdata_complete_sens) #866

# sensitivity with bpdata_valid, removing women who didn't cook
nrow(bpdata_valid[bpdata_valid$cookingevents == 0 &!is.na(bpdata_valid$cookingevents),]) #10
nrow(bpdata_valid[is.na(bpdata_valid$cookingevents),]) #0

bpdata_valid_cook <- bpdata_valid[!bpdata_valid$cookingevents == 0,] 

# crude
fm <- lm(sbp ~ mean_corr, data = bpdata_valid_cook)
cl(fm = fm, data = bpdata_valid_cook, cluster = bpdata_valid_cook$vil_code)
# coef = 0.3, p = 0.06

cooksD <- cooks.distance(fm)
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD) # 2 above 0.04 
bpdata_valid_sens <- bpdata_valid_cook[!row.names(bpdata_valid_cook) %in% names(cooksD[cooksD > 0.04]),]
nrow(bpdata_valid_sens) #805

fm <- lm(sbp ~ mean_corr, data = bpdata_valid_sens) 
cl(fm = fm, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code)
# coef: 0.27, p = 0.38
# CI: 0.27403 +/- 0.31161*1.963 [-0.33, 0.89]
nrow(bpdata_valid_sens)

# adjusted
fm <- lm(sbp ~ mean_corr + age + BMI + gestwks + tobacco_bin + weekday_bp + salary, data = bpdata_valid_cook) 
cl(fm = fm, data = bpdata_valid_cook, cluster = bpdata_valid_cook$vil_code)
# CO coef: 0.368 (p-val = 0.16)

nrow(bpdata_valid_cook)
plot(fm)
cooksD <- cooks.distance(fm)
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD) # none > 0.04

# Sensitivity - not excluding influential variables
fm <- lm(sbp ~ mean_corr + age + BMI + gestwks + tobacco_bin + weekday_bp + salary, data = bpdata_valid) 
cl(fm = fm, data = bpdata_valid, cluster = bpdata_valid$vil_code)

# coef = 0.39, p = 0.13
# CI = 0.389998 +/- 0.259316*1.963 [-0.12, 0.90]

# DBP Adjusted Model-----
fm <- lm(dbp ~ mean_corr + age + BMI + gestwks + tobacco_bin + weekday_bp, data = bpdata_valid) 
cl(fm = fm, data = bpdata_valid, cluster = bpdata_valid$vil_code)

# coef: 0.623


plot(fm)
cooksD <- cooks.distance(fm)
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD) # 847 is 0.059

bpdata_valid_sens <- bpdata_valid[!row.names(bpdata_valid) %in% names(cooksD[cooksD > 0.04]),]
nrow(bpdata_valid_sens) #816

fm <- lm(dbp ~ mean_corr + age + BMI + gestwks  + tobacco_bin + weekday_bp, data = bpdata_valid_sens) 
cl(fm = fm, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code)


# Estimate Std. Error t value  Pr(>|t|)    
# (Intercept)     55.807884   2.230099 25.0248 < 2.2e-16 ***
#   mean_corr        0.463766   0.212121  2.1863  0.029079 *  
#   age              0.015591   0.048882  0.3189  0.749850    
# BMI              0.337367   0.076679  4.3997 1.230e-05 ***
#   gestwks         -0.203781   0.079999 -2.5473  0.011041 *  
#   tobacco_bin      1.468869   0.867303  1.6936  0.090727 .  
# weekday_bpMon    2.400453   0.807971  2.9710  0.003057 ** 
#   weekday_bpSat    4.773146   0.930394  5.1302 3.629e-07 ***
#   weekday_bpThurs  2.049409   1.089309  1.8814  0.060280 .  
# weekday_bpTues   2.751939   0.967845  2.8434  0.004577 ** 
#   weekday_bpWed   -0.371815   1.115100 -0.3334  0.738892    

# testing adding other variables
0.463-0.0463 # 0.416


fm1 <- update(fm, .~. + rainy)
cl(fm = fm1, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code) # coef = 0.463

fm1 <- update(fm, .~. + salary)
cl(fm = fm1, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code) # coef = 0.430

fm1 <- update(fm, .~. + asset_index)
cl(fm = fm1, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code) # coef = 0.471

fm1 <- update(fm, .~. + vil_on_road)
cl(fm = fm1, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code)  # coef = 0.470


# include: salary, to match with sbp (should i?)
fm1 <- update(fm, .~. + salary)
cl(fm = fm1, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code) # coef = 0.430, p = 0.05
nrow(bpdata_valid_sens)

# Estimate Std. Error t value  Pr(>|t|)    
# (Intercept)     56.847961   3.320256 17.1216 < 2.2e-16 ***
#   mean_corr        0.430808   0.216531  1.9896  0.046976 *  
#   age              0.045066   0.055413  0.8133  0.416304    
# BMI              0.318777   0.071957  4.4301 1.073e-05 ***
#   gestwks         -0.216612   0.078110 -2.7732  0.005680 ** 
#   tobacco_bin      1.538313   0.850120  1.8095  0.070744 .  
# weekday_bpMon    2.524834   0.833256  3.0301  0.002523 ** 
#   weekday_bpSat    5.696103   0.976661  5.8322 7.932e-09 ***
#   weekday_bpThurs  1.771792   1.136790  1.5586  0.119488    
# weekday_bpTues   2.712081   0.972285  2.7894  0.005406 ** 
#   weekday_bpWed   -0.437747   1.145751 -0.3821  0.702517    
# salary3         -0.409456   2.021686 -0.2025  0.839552    
# salary4         -1.601454   2.219054 -0.7217  0.470700    
# salary5         -2.167347   2.054081 -1.0551  0.291679    
# salary7         -0.199383   2.206448 -0.0904  0.928021    


# CI: 0.430808 +/- 0.216531*1.963 [0.01, 0.86]
plot(fm)

fm <- lm(dbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin + weekday_bp + salary, data = bpdata_complete) 
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: 0.21, p-val = 0.09
cooksD <- cooks.distance(fm)
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD) # none above 0.04 

nrow(bpdata_complete)

# Sensitivity: Taking out women who didn't cook
# crude
fm <- lm(dbp ~ mean_corr, data = bpdata_valid_cook)
cl(fm = fm, data = bpdata_valid_cook, cluster = bpdata_valid_cook$vil_code)
# coef = 0.75, p = 0.004
# CI: 0.74677 +/- 0.25639*1.963 [0.24, 1.25]

cooksD <- cooks.distance(fm)
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD) # 847 is above 0.04 
bpdata_valid_sens <- bpdata_valid_cook[!row.names(bpdata_valid_cook) %in% names(cooksD[cooksD > 0.04]),]
nrow(bpdata_valid_sens) #806

fm <- lm(dbp ~ mean_corr, data = bpdata_valid_sens) 
cl(fm = fm, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code)
# coef: 0.55, p = 0.01
# CI: 0.54574 +/- 0.22345*1.963 [0.11, 0.98]
nrow(bpdata_valid_sens)

# adjusted

fm <- lm(dbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin + weekday_bp + salary, data = bpdata_valid_cook) 
cl(fm = fm, data = bpdata_valid_cook, cluster = bpdata_valid_cook$vil_code)
cooksD <- cooks.distance(fm)
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD) # 847 is above 0.04 
bpdata_valid_sens <- bpdata_valid_cook[!row.names(bpdata_valid_cook) %in% names(cooksD[cooksD > 0.04]),]
nrow(bpdata_valid_sens) #806


fm <- lm(dbp ~ mean_corr + age + BMI + gestwks + tobacco_bin + weekday_bp + salary, data = bpdata_valid_sens) 
cl(fm = fm, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code)
# coef = 0.395, p = 0.067
cooksD <- cooks.distance(fm)
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD)  # none above 0.04
nrow(bpdata_valid_sens) # 806

# Sensitivity, not excluding influential variables
fm <- lm(dbp ~ mean_corr + age + BMI + gestwks + tobacco_bin + weekday_bp + salary, data = bpdata_valid) 
cl(fm = fm, data = bpdata_valid, cluster = bpdata_valid$vil_code)
# coef = 0.59, p = 0.01
# CI = 0.585560 +/- 0.230066*1.963 [0.13, 1.04]

# # SBP, effect of influential variables on beta
# 
pdf(file = paste0("Figure S1_Effect of cooks D on beta for BP_", format(Sys.Date(), format = "%b%d"), ".pdf"), height = 10, width = 7)
par(mfrow = c(2,1))
coefs <- matrix(nrow = 11, ncol = 2)
fm <- lm(sbp ~ mean_corr + age + BMI + gestwks  + tobacco_bin + weekday_bp + salary, data = bpdata_valid)
cooksD <- cooks.distance(fm)
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
cooksD <- cooksD[1:10]
for (i in 0:10) { 
  data = bpdata_valid[!row.names(bpdata_valid) %in% names(cooksD)[0:i],]
  fm <- lm(sbp ~ mean_corr + age + BMI + gestwks + tobacco_bin + weekday_bp + salary, data = data) 
  coef <- cl(fm = fm, data = data, cluster = data$vil_code)[2,1] # pulling out the coefficient for mean_corr
  coefs[i+1,1] <- i
  coefs[i+1,2] <- coef
}
plot(coefs[,1], coefs[,2], ylim = c(0.2, 0.8),xlab = "Number of influential observations removed", ylab = "Beta coefficient for CO", main = "A. Effect of data point removal on regression slope for SBP", cex.main = 0.9, xaxp = c(0,10,10))
lines(coefs[,1], coefs[,2])



coefs <- matrix(nrow = 11, ncol = 2)
fm <- lm(dbp ~ mean_corr + age + BMI + gestwks + tobacco_bin + weekday_bp + salary, data = bpdata_valid)
cooksD <- cooks.distance(fm)
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
cooksD <- cooksD[1:10]
for (i in 0:10) { 
  data = bpdata_valid[!row.names(bpdata_valid) %in% names(cooksD)[0:i],]
  fm <- lm(dbp ~ mean_corr + age + BMI + gestwks + tobacco_bin + weekday_bp + salary, data = data) 
  coef <- cl(fm = fm, data = data, cluster = data$vil_code)[2,1] # pulling out the coefficient for mean_corr
  coefs[i+1,1] <- i
  coefs[i+1,2] <- coef
}
plot(coefs[,1], coefs[,2], ylim = c(0.2, 0.8),xlab = "Number of influential observations removed", ylab = "Beta coefficient for CO", main = "B. Effect of data point removal on regression slope for DBP", cex.main = 0.9, xaxp = c(0,10,10))
lines(coefs[,1], coefs[,2])
dev.off()
# 
# # Sensitivity: without outliers greater than 3x the IQR for CO -----
# bpdata_complete$q98_fix <- bpdata_complete$q98_corr + 0.1 # not nec if not on log scale
# bpdata_complete$q90_fix <- bpdata_complete$q90_corr + 0.1
# 
# 
# bpdata_rmout <-  bpdata_complete[!bpdata_complete$mean_corr %in% boxplot.stats(bpdata_complete$mean_corr, coef = 3)$out,]
# nrow(bpdata_rmout) #846
# 
# # plots
# pdf(file = "Testing outliers.pdf")
# par(mfrow = c(1,2))
# plot(bpdata_complete$mean_corr, bpdata_complete$sbp, main = "SBP", xlab = "Mean CO", ylab = "SBP")
# legend("topright", legend = c("beta = 0.03, p-val = 0.82"))
# 
# 
# plot(bpdata_rmout$mean_corr, bpdata_rmout$sbp, main = "SBP, no outliers",  xlab = "Mean CO", ylab = "SBP")
# legend("topright", legend = c("beta = 0.34, p-val = 0.25"))
# 
# 
# plot(bpdata_complete$mean_corr, bpdata_complete$dbp, main = "DBP", xlab = "Mean CO", ylab = "DBP")
# legend("topright", legend = c("beta = 0.23, p-val = 0.07"))
# 
# 
# plot(bpdata_rmout$mean_corr, bpdata_rmout$dbp, main = "DBP, no outliers",  xlab = "Mean CO", ylab = "DBP")
# legend("topright", legend = c("beta = 0.48, p-val = 0.04"))
# dev.off()
# 
# 
# 
# 
# 
# # SBP
# # crude
# fm <- lm(sbp ~ mean_corr, data = bpdata_rmout) 
# cl(fm = fm, data = bpdata_rmout, cluster = bpdata_rmout$vil_code)
# # CO coef: 0.27, p-val = 0.41
# # 95% CI: 0.26945 +/- 0.32867 * 1.963 = [-0.376, 0.915]
# 
# # adjusted
# fm <- lm(sbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_rmout) 
# cl(fm = fm, data = bpdata_rmout, cluster = bpdata_rmout$vil_code)
# # CO coef = 0.39, p-val = 0.21
# # 95% CI: 0.394676 - 0.317346 * 1.963,  0.394676 + 0.317346 * 1.963 = [-0.228, 1.018]
# 
# # DBP
# # crude
# fm <- lm(dbp ~ mean_corr, data = bpdata_rmout)
# cl(fm = fm, data = bpdata_rmout, cluster = bpdata_rmout$vil_code)
# # CO coef: 0.51, p-val = 0.04
# # 95% CI:  0.51419 +/- 0.24377*1.963 = [0.036, 0.993]
# 
# 
# fm <- lm(dbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_rmout) 
# cl(fm = fm, data = bpdata_rmout, cluster = bpdata_rmout$vil_code)
# # coef = 0.52, p-val = 0.03
# # 95% CI: 0.515821 - 0.239468*1.963; 0.515821 + 0.239468*1.963 = [0.046, 0.986]
# 
# 
# 
# 
# # Sensitivity with q98 -------
# # !! This is not at all the same. And not explained by different outliers. Why?
# which(bpdata_complete$mean_corr %in% boxplot.stats(bpdata_complete$mean_corr, coef = 3)$out)
# which(bpdata_complete$q98_corr %in% boxplot.stats(bpdata_complete$q98_corr, coef = 3)$out) # 17 including all but 1 of the 9 for mean_corr
# bpdata_rmout2 <-  bpdata_complete[!bpdata_complete$q98_corr %in% boxplot.stats(bpdata_complete$q98_corr, coef = 3)$out,]
# 
# # SBP
# # adjusted
# fm <- lm(sbp ~ q98_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_rmout2) 
# cl(fm = fm, data = bpdata_rmout2, cluster = bpdata_rmout2$vil_code)
# CO coef = 0.05, p-val = 0.25

# # DBP
# # adjusted
# fm <- lm(dbp ~ q98_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_rmout2) 
# cl(fm = fm, data = bpdata_rmout2, cluster = bpdata_rmout2$vil_code)
# # CO coef = 0.05, p-val = 0.15
# 
# 
# 
# # Calculate the ICC (http://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&ved=0CB8QFjAA&url=http%3A%2F%2Fdavidakenny.net%2Fpapers%2Fk%26h%2FMLM_R.pdf&ei=vV8UVfTIIcG1sASF-YDgBA&usg=AFQjCNHY-jZyh8vCLVCdCDExDDIaivYVoA&sig2=k-8M3kpbjXSiiZ9L1zEM3Q&bvm=bv.89217033,d.cWc)
# 
# # null model
# nullfit <- lme(fixed = sbp ~ 1, data = bpdata_rmout, random = ~1|vil_code)
# summary(nullfit)
# # Random effects:
# #Formula: ~1 | vil_code
# #(Intercept) Residual
# #StdDev:   0.6865927 10.14731
# (0.6865927^2)/( 0.6865927^2 + 10.14731^2) # use variance (SD^2) rather than SD, see link above
# # ICC = 0.005
# 
# nullfit <- lme(fixed = dbp ~ 1, data = bpdata_rmout, random = ~1|vil_code)
# summary(nullfit)
# # Random effects:
# #Formula: ~1 | vil_code
# #(Intercept) Residual
# #StdDev:  0.9680754 7.841403
# 0.9680754^2/(0.9680754^2 + 7.841403^2) # ICC = 0.015
# 
# # or from package multilevel
# aov.1 <- aov(sbp ~ vil_code, bpdata_rmout)
# ICC1(aov.1) # 0.018
# aov.1 <- aov(dbp ~ vil_code, bpdata_rmout)
# ICC1(aov.1) # 0.017
# 
# 
# 
# # using multilevel model instead of clustered errors (results practically identical)
# library(nlme)
# 
# # SBP, all data
# fit <- lme(fixed = sbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_complete, random = ~1|vil_code)
# summary(fit)
# # coef = 0.03, p-val = 0.83
# 
# # SBP, no outliers
# fit <- lme(fixed = sbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_rmout, random = ~1|vil_code)
# summary(fit)
# # coef = 0.34, p-val = 0.23
# 
# # DBP, all data
# fit <- lme(fixed = dbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_complete, random = ~1|vil_code)
# summary(fit)
# # coef = 0.24, p-val = 0.05
# 
# # DBP, no outliers
# fit <- lme(fixed = dbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_rmout, random = ~1|vil_code)
# summary(fit)
# # coef = 0.47, p-val = 0.04
# 
# # SBP, using 98th percentile
# bpdata_complete$q98_fix <- bpdata_complete$q98_corr + 0.1
# fm <- lm(sbp ~ log(q98_fix) + age + log(BMI) + gestwks + asset_index + tobacco_bin, data = bpdata_complete)
# cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# # CO coef: 0.09, p-val = 0.38
# 
# 
# # DBP, using 98th percentile
# bpdata_complete$q98_fix <- bpdata_complete$q98_corr + 0.1
# fm <- lm(dbp ~ log(q98_fix) + age + log(BMI) + gestwks + asset_index + tobacco_bin, data = bpdata_complete)
# 
# cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# # CO coef: 0.56, p-val = 0.03
# 
# # SBP, using 90th percentile
# bpdata_complete$q90_fix <- bpdata_complete$q90_corr + 0.1
# fm <- lm(sbp ~ log(q90_fix) + age + log(BMI) + gestwks + asset_index + tobacco_bin, data = bpdata_complete)
# cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# # CO coef: -0.01, p-val = 0.96
# 
# # DBP, using 90th percentile
# fm <- lm(dbp ~ log(q90_fix) + age + log(BMI) + gestwks + asset_index + tobacco_bin, data = bpdata_complete)
# 
# cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# # CO coef: 0.32, p-val = 0.16
# 
# # Sens: CO outliers - are these driving the results?--------
# boxplot.stats(bpdata_complete$mean_corr, coef = 3.5) # 8 outliers (above 7.2)
# boxplot.stats(bpdata_complete$mean_corr, coef = 3) # 9 outliers (above 6.7)
# boxplot.stats(bpdata_complete$mean_corr, coef = 2.5) # 15 ourliers (above 5.96)
# boxplot.stats(bpdata_complete$mean_corr, coef = 2) # 17 outliers (above 5.3)
# 
# # Sens: multilevel model instead of clustered SEs -----------
# 
# 
# # checking rainy and weekday
# bpdata_complete$months <- months(bpdata_complete$firstdate)
# bpdata_complete$rainy <- ifelse(bpdata_complete$months %in% c("April", "May", "June", "September", "October"), 1, 0)
# bpdata_complete$weekday <- wday(bpdata_complete$firstdate) # Sunday is 1
# 
# # sensitivity: only the overall_valid =1 -------
# baselinebpdata$tobacco_bin <- ifelse(baselinebpdata$tobacco > 0, 1, 0)
# sens <- baselinebpdata[baselinebpdata$overall_valid ==1 & baselinebpdata$mstudyid %in% bpdata_complete$mstudyid,] #807
# plot(sens$mean_corr, sens$sbp,  ylab = "SBP", xlab = "Mean CO", main = "only Validity = 1")
# plot(bpdata_complete$mean_corr, bpdata_complete$sbp, ylab = "SBP", xlab = "Mean CO", main = "all data")
# plot(sens$mean_corr, sens$dbp, ylab = "DBP", xlab = "Mean CO", main = "only Validity = 1")
# plot(bpdata_complete$mean_corr, bpdata_complete$dbp, ylab = "DBP", xlab = "Mean CO", main = "all data")
# # SBP
# # crude
# fm <- lm(sbp ~ mean_corr, data = sens) 
# cl(fm = fm, data = sens, cluster = sens$vil_code)
# #CO coef = 0.53, p=val = 0.07
# # adjusted
# fm <- lm(sbp ~ mean_corr + age + log(BMI) + gestwks + asset_index + tobacco_bin, data = sens) 
# cl(fm = fm, data = sens, cluster = sens$vil_code)
# # CO coef = 0.53, p-val = 0.05
# 
# 
# 
# # DBP
# # crude
# fm <- lm(dbp ~ mean_corr, data = sens) 
# cl(fm = fm, data = sens, cluster = sens$vil_code)
# # CO coef = 0.92, p-val = 0.01
# 
# fm <- lm(dbp ~ mean_corr, data = bpdata_complete) 
# cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# # CO coef = 0.23, p-val = 0.12
# 
# # adjusted
# fm <- lm(dbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = sens) 
# cl(fm = fm, data = sens, cluster = sens$vil_code)
# # CO coef: 0.90; p-val = 0.01
# 
# # diagnostics: Cook's Distance for influential observations
# # Identify largest Cook's D 
# # SBP
# 
# cooksD <- cooks.distance(lm_sbp) # using adjusted model with bpdata2
# max(cooksD) #Oct 4 0.038
# length(cooksD[cooksD > 0.04]) #1
# length(cooksD[cooksD > 0.02]) #3: 680, 855, 29
# which(cooksD > 0.02) 
# 
# bpdata3 <- bpdata2[-c(680),]
# bpdata4 <- bpdata2[-c(680, 855),]
# bpdata5 <- bpdata2[-c(680, 855, 29),]
# 
# 
# 
# 
# # Mccracken: age & BMI (linear); binary indicators for: smoking, SHS exposure, temescal use, household electricity. The asset index is the sum of binary indicators for having a bicycle, a radio, and a television, and was entered as categorical variable. To increase precision, we also considered time-varying covariates, such as apparent temperature, sea- son, day of the week, and time of day. We used linear terms to control for daily average apparent temperature and time of day and dummy variables for each day of the week and for rainy (1 May–31 October) versus dry season (1 November –30 April).


# Table 1 -----
# For comparison between originally enrolled women (1183) and final dataset (bpdata_valid, n = 817)
baselinebpdata <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/baselinedata_Feb12.rds")
nrow(baselinebpdata) #1183
baselinebpdata$final_analysis <- ifelse(baselinebpdata$mstudyid %in% bpdata_valid$mstudyid, 1, 0)
table(baselinebpdata$final_analysis) # 376 vs 817

# for continuous variables and NAs
library(pastecs)
options("scipen" = 6, "digits" = 3)
stat.desc(baselinebpdata[baselinebpdata$final_analysis == 1,c("age", "BMI", "gestwks", "asset_index")])
stat.desc(baselinebpdata[baselinebpdata$final_analysis == 0,c("age", "BMI", "gestwks", "asset_index")])


t.test(baselinebpdata$age[baselinebpdata$final_analysis ==1], baselinebpdata$age[baselinebpdata$final_analysis ==0])
t.test(baselinebpdata$BMI[baselinebpdata$final_analysis ==1], baselinebpdata$BMI[baselinebpdata$final_analysis ==0])
t.test(baselinebpdata$gestwks[baselinebpdata$final_analysis ==1], baselinebpdata$gestwks[baselinebpdata$final_analysis ==0])
t.test(baselinebpdata$asset_index[baselinebpdata$final_analysis ==1], baselinebpdata$asset_index[baselinebpdata$final_analysis ==0])

t.test(baselinebpdata$bp[baselinebpdata$final_analysis ==1], baselinebpdata$bp[baselinebpdata$final_analysis ==0]) # for means only, this is not a continuous variable
fisher.test(table(baselinebpdata$bp, baselinebpdata$final_analysis))




# for smokecur/smokpast
baselinebpdata$ever_smoker <- ifelse(baselinebpdata$smokecur == 1 | baselinebpdata$smokpast == 1, 1, 0)
t.test(baselinebpdata$ever_smoker[baselinebpdata$final_analysis == 1], baselinebpdata$ever_smoker[baselinebpdata$final_analysis == 0]) # for means only, this is not a continuous variable
fisher.test(baselinebpdata$ever_smoker, baselinebpdata$final_analysis)

# shs 
t.test(baselinebpdata$shs[baselinebpdata$final_analysis ==1], baselinebpdata$shs[baselinebpdata$final_analysis ==0]) # for means only, this is not a continuous variable
fisher.test(table(baselinebpdata$shs, baselinebpdata$final_analysis))

# medlev (1 = none, 2 = primary, 3 = middle, 4-7 = above middle)
table(baselinebpdata$medlev[baselinebpdata$final_analysis == 1 & !is.na(baselinebpdata$medlev)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 1 &!is.na(baselinebpdata$medlev),]) *100
table(baselinebpdata$medlev[baselinebpdata$final_analysis == 0 & !is.na(baselinebpdata$medlev)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 0 ,]) *100
medlev <- table(baselinebpdata$medlev[!is.na(baselinebpdata$medlev)], baselinebpdata$final_analysis[!is.na(baselinebpdata$medlev)])
fisher.test(medlev)

# married (1 = married, 2= living with partner, 3 = widowed, 4 = divorced, 5 = separated, 6 = single)
table(baselinebpdata$married[baselinebpdata$final_analysis == 1 & !is.na(baselinebpdata$married)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 1 &!is.na(baselinebpdata$married),]) *100
table(baselinebpdata$married[baselinebpdata$final_analysis == 0 & !is.na(baselinebpdata$married)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 0,]) *100
married <- table(baselinebpdata$married[!is.na(baselinebpdata$married)], baselinebpdata$final_analysis[!is.na(baselinebpdata$married)], useNA = "always")
fisher.test(married)

# salary (1 = professional, 2 = clerical, 3 = trader/food seller/businesswoman, 4 = seamstress/hairdresser, 5 = farmer/laborer/domestic worker, 6 = other, 7 = none)
table(baselinebpdata$salary[baselinebpdata$final_analysis == 1 & !is.na(baselinebpdata$salary)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 1 & !is.na(baselinebpdata$salary),]) *100
table(baselinebpdata$salary[baselinebpdata$final_analysis == 0 & !is.na(baselinebpdata$salary)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 0,]) *100
baselinebpdata$salary2 <- mapvalues(baselinebpdata$salary, from = c(1,2,4), to = c(6,6,6))
salary <- table(baselinebpdata$salary2[!is.na(baselinebpdata$salary2)], baselinebpdata$final_analysis[!is.na(baselinebpdata$salary2)])
fisher.test(salary)

# Cooking data -----
# Cooking <- read.csv("~/Dropbox/Ghana project/BP project/Baseline BP Paper/Final_Data/Update_Nov20/Cooking.csv", stringsAsFactors=FALSE)

# Analysis data
# # bpdata <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/bpdataSep23.rds")
# # studyids <- unique(bpdata$mstudyid) # 903
# 
# Cooking <- Cooking[Cooking$mstudyid %in% studyids,] # 897
# Cooking <- Cooking[!duplicated(Cooking),]
# 
# table(Cooking$whercok1)/nrow(Cooking)
# table(Cooking$whercok2)/nrow(Cooking)
# table(Cooking$whecok3)/nrow(Cooking)
# table(Cooking$whercok1, Cooking$whencok1)
# table(Cooking$whercok2, Cooking$whencok2)
# 
# table(Cooking$fuel1)/nrow(Cooking)
# table(Cooking$fuel2)/nrow(Cooking)

# Whercok1 (1 = totally open, 2 = roof only, 3 = veranda of house, 4 = partially enclosed w/no roof, 5 =partially enclosed with roof, 6 = open courtyard, 7 = fully enclosed, 8 = inside veranda of compound)
# 1+6 = totally open, 2-5 & 8 = partially enclosed, 7 = fully enclosed

nrow(baselinebpdata[is.na(baselinebpdata$whercok1) & baselinebpdata$final_analysis ==1,])/nrow(baselinebpdata[baselinebpdata$final_analysis ==1,])*100
nrow(baselinebpdata[is.na(baselinebpdata$whercok1) & baselinebpdata$final_analysis ==0,])/nrow(baselinebpdata[baselinebpdata$final_analysis ==0,])*100

baselinebpdata$whercok1_2 <- mapvalues(baselinebpdata$whercok1, from = 1:8, to = c(1,2,2,2,2,1,3,2))
table(baselinebpdata$whercok1_2[baselinebpdata$final_analysis == 1 & !is.na(baselinebpdata$whercok1_2)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 1 & !is.na(baselinebpdata$whercok1_2),]) *100
table(baselinebpdata$whercok1_2[baselinebpdata$final_analysis == 0 & !is.na(baselinebpdata$whercok1_2)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 0,]) *100

whercok1 <- table(baselinebpdata$whercok1_2, baselinebpdata$final_analysis)
fisher.test(whercok1)

# secondary (whercok2)
nrow(baselinebpdata[is.na(baselinebpdata$whercok2) & baselinebpdata$final_analysis ==1,])/nrow(baselinebpdata[baselinebpdata$final_analysis ==1,])*100
nrow(baselinebpdata[is.na(baselinebpdata$whercok2) & baselinebpdata$final_analysis ==0,])/nrow(baselinebpdata[baselinebpdata$final_analysis ==0,])*100

baselinebpdata$whercok2_2 <- mapvalues(baselinebpdata$whercok2, from = 1:8, to = c(1,2,2,2,2,1,3,2))
table(baselinebpdata$whercok2_2[baselinebpdata$final_analysis == 1 & !is.na(baselinebpdata$whercok2_2)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 1 & !is.na(baselinebpdata$whercok2_2),]) *100
table(baselinebpdata$whercok2_2[baselinebpdata$final_analysis == 0 & !is.na(baselinebpdata$whercok2_2)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 0,]) *100

whercok2 <- table(baselinebpdata$whercok2_2, baselinebpdata$final_analysis)
fisher.test(whercok2)

# fuel1 (1 = wood, 2 = crop residue, 3 = animal dung, 4 = charcoal, 5 = kerosene, 6 = other)
nrow(baselinebpdata[is.na(baselinebpdata$fuel1) & baselinebpdata$final_analysis ==1,])/nrow(baselinebpdata[baselinebpdata$final_analysis ==1,])*100
nrow(baselinebpdata[is.na(baselinebpdata$fuel1) & baselinebpdata$final_analysis ==0,])/nrow(baselinebpdata[baselinebpdata$final_analysis ==0,])*100

table(baselinebpdata$fuel1[baselinebpdata$final_analysis == 1 & !is.na(baselinebpdata$fuel1)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 1 & !is.na(baselinebpdata$fuel1),]) *100
table(baselinebpdata$fuel1[baselinebpdata$final_analysis == 0 & !is.na(baselinebpdata$fuel1)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 0,]) *100

fuel1 <- table(baselinebpdata$fuel1, baselinebpdata$final_analysis)
fisher.test(fuel1)

# fuel2 (1 = wood, 2 = crop residue, 3 = animal dung, 4 = charcoal, 5 = kerosene, 6 = other)
nrow(baselinebpdata[is.na(baselinebpdata$fuel2) & baselinebpdata$final_analysis ==1,])/nrow(baselinebpdata[baselinebpdata$final_analysis ==1,])*100
nrow(baselinebpdata[is.na(baselinebpdata$fuel2) & baselinebpdata$final_analysis ==0,])/nrow(baselinebpdata[baselinebpdata$final_analysis ==0,])*100

table(baselinebpdata$fuel2[baselinebpdata$final_analysis == 1 & !is.na(baselinebpdata$fuel2)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 1 & !is.na(baselinebpdata$fuel2),]) *100
table(baselinebpdata$fuel2[baselinebpdata$final_analysis == 0 & !is.na(baselinebpdata$fuel2)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 0,]) *100

fuel2 <- table(baselinebpdata$fuel2, baselinebpdata$final_analysis)
fisher.test(fuel2)

table(baselinebpdata$whercok2, baselinebpdata$whencok2) # 1 always, 2 when dry, 3 when raining, 4 for certain meals
# table(baselinebpdata$water[baselinebpdata$final_analysis == 0], useNA = "always")/nrow(baselinebpdata[baselinebpdata$final_analysis == 0,]) *100
# water <- table(baselinebpdata$water, baselinebpdata$final_analysis)
# water <- water[1:6,]
# fisher.test(water)
# 
# table(baselinebpdata$toilet[baselinebpdata$final_analysis == 0])/nrow(baselinebpdata[baselinebpdata$final_analysis == 0,]) *100
# toilet <- table(baselinebpdata$toilet, baselinebpdata$final_analysis)
# fisher.test(toilet)
# 
# 
# 
# names(bpdata_complete)[54:66]
# for (i in 54:66) {
#   print(names(bpdata_complete)[i])
#   print(table(bpdata_complete[, i], useNA = "always")/855*100)
# }
# 
# names(baselinebpdata)[183:203]
# for (i in 183:203) {
#   print(names(baselinebpdata)[i])
#   print(table(baselinebpdata[baselinebpdata$final_analysis == 0, i], useNA = "always")/nrow(baselinebpdata[baselinebpdata$final_analysis == 0,])*100)
# }
# 
# t.test(baselinebpdata$asset_index[baselinebpdata$final_analysis == 1], baselinebpdata$asset_index[baselinebpdata$final_analysis == 0])
# summary(bpdata$cookingevents)
# hist(bpdata$cookingevents)
# nrow(bpdata[bpdata$cookingevents ==0 & !is.na(bpdata$cookingevents),]) #10





###### FIGURE 1: Example CO Plot ----
# BM0748M in CU_CO_122
library(ggplot2)
library(scales)
CO_stacked <- readRDS("/Users/ashlinn/Dropbox/Ghana_exposure_data_SHARED (1)/CO_files_processed/29Jan2015/CO_stacked files/CO_stacked_CU_CO_122_11441.rds")
directory <- "/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Manuscript/Figures/"
pdf(file = paste(directory, "Fig1_CO_minute_avg.pdf", sep = ""))
print(ggplot(data=filter(CO_stacked, mstudyid == "BM0748M"))+ geom_line(aes(datetime,co*cf[1]), color="blue") + labs(title=paste("Minute-Averaged CO \n", "Session 1", data$session[1]))+xlab("Time")+ ylab("CO (ppm)") + scale_x_datetime(breaks = date_breaks("12 hours"), labels = date_format("%a %H:%M")) + expand_limits(y=0) +  theme(plot.title=element_text(size=rel(0.92), color = "darkgrey")) + theme_bw())
dev.off()


###### FIGURE 2: Graph of CO Kernel Density ----
# # q98
# pdf(file = paste0("Fig2_CO_Density_q98_", format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 6)
# d <- density(bpdata$q98_corr)
# plot(d, ylim = c(0, 0.1), main = "Distribution of 98th percentile of 72-hour-averaged CO", lwd = 2, xlab = "98th percentile of CO(ppm)", ylab = "Probability density", col = "coral3")
# abline(v = mean(bpdata$q98_corr), lty = "dotted")
# text(x = mean(bpdata$q98_corr), y = 0.09, pos = 4, label = paste("Mean 98th percentile CO:\n", round(mean(bpdata$q98_corr), digits = 2), "\U00b1", round(sd(bpdata$q98_corr), digits = 2), "ppm"))
# dev.off()

# mean_co
pdf(file = paste0("Fig2_CO_Density_meanCO_", format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 6)
d <- density(bpdata_valid$mean_corr)
plot(d, main = "", lwd = 2, xlab = "Mean CO(ppm)", ylab = "Probability density", col = "coral3")
abline(v = mean(bpdata_valid$mean_corr), lty = "dotted")
text(x = mean(bpdata_valid$mean_corr), y = 0.3, pos = 4, label = paste("Mean 72-hour-averaged CO:\n", round(mean(bpdata_valid$mean_corr), digits = 2), "\U00b1", round(sd(bpdata_valid$mean_corr), digits = 2), "ppm \n (range: ", round(range(bpdata_valid$mean_corr), digits = 3)[1], "to", round(range(bpdata_valid$mean_corr), digits = 1)[2], "ppm)"))
dev.off()


##### FIGURE 3: Graph of BP Kernel Density ----
pdf(file = paste0("Fig3_BP_Density", format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 6)
e <- density(bpdata_valid$sbp)
plot(e, ylim = c(0, 0.1), xlim = c(0, 200), main = "", lwd = 2, xlab = "BP (mmHg)", ylab = "Probability density",  col = "purple")
f <- density(bpdata_valid$dbp)
lines(f, col = "darkgreen", lwd = 2)
abline(v = mean(bpdata_valid$sbp), lty = "dotted")
abline(v = mean(bpdata_valid$dbp), lty= "dotted")

text(x = mean(bpdata_valid$sbp), y = 0.09, pos = 4, label = paste("Mean SBP:\n", round(mean(bpdata_valid$sbp), digits = 2), "\U00b1" , round(sd(bpdata_valid$sbp), digits = 2), "mmHg"))
text(x = mean(bpdata_valid$dbp), y = 0.09, pos = 2, label = paste("Mean DBP:\n ", round(mean(bpdata_valid$dbp), digits = 2),  "\U00b1", round(sd(bpdata_valid$dbp), digits = 2), "mmHg"))
legend ("topright", legend = c("SBP", "DBP"), col = c("purple", "darkgreen"), lwd = 2)
dev.off()


