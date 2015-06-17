### GRAPHS Baseline BP Analysis
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
baselinebpdata <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/baselinedata_Feb12.rds")
nrow(baselinebpdata) #1183
length(unique(baselinebpdata$mstudyid)) #1183
NAPerVariable(baselinebpdata)

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
# removing BP outliers and individuals who didn't cook

baselinebpdata[baselinebpdata$bp_validity ==3.5, c("sbp", "dbp")] #2: 95/162 (imposs for SBP < DBP and DBP outlying), 145/100 (DBP is outlying)
nrow(baselinebpdata[is.na(baselinebpdata$bp_validity),]) #0
baselinebpdata <- baselinebpdata[!baselinebpdata$bp_validity ==3.5,]
nrow(baselinebpdata) # 903

nrow(baselinebpdata[baselinebpdata$cookingevents == 0 &!is.na(baselinebpdata$cookingevents),]) #11
nrow(baselinebpdata[is.na(baselinebpdata$cookingevents),]) #13
baselinebpdata[baselinebpdata$cookingevents ==0 &!is.na(baselinebpdata$cookingevents), c("cookingevents", "cookfood_days")]
baselinebpdata <- baselinebpdata[!baselinebpdata$cookingevents == 0 | is.na(baselinebpdata$cookingevents),] 

nrow(baselinebpdata) # 892

bpdata <- baselinebpdata
NAPerVariable(bpdata) # make sure additional NA's haven't jumped in

saveRDS(bpdata, file = paste0("bpdata", format(Sys.Date(), format = "%b%d"), ".rds")) # this dataset excludes BP outliers and those with no cooking events



### ANALYSIS --------
# Remove extraneous variables -------

bpdata <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/bpdataApr08.rds")


data <- subset(bpdata, select = c("vil_code", "mstudyid", "sbp", "dbp", "bp", "smokecur", "smokpast", "shs", "age", "medlev", "married", "religion", "ethnic", "wownland", "farmln", "crops", "salary", "comptype", "ownhouse", "mean_corr", "q98_corr", "hours", "firstdate", "BMI", "gestwks", "asset_index", "cookingevents", "charcoal", "otherstoveuse", "mosqcoil", "tobacco", "mill", "threestone", "coalpot", "roadsale", "makecharcoal", "anthrop_validity", "bp_validity", "onelsecok", "coils", "invcom_sum", "invcoal_sum", "oth_smksources", "vil_pop", "vil_some_edu", "vil_modal_rel", "vil_modal_ethn", "village_ses2", "vil_hhsize", "vil_on_road", "people_all", "water", "toilet", "tabled_bin", "mattres_bin", "itn_bin", "radio_bin", "tv_bin", "fridge_bin", "bicycle_bin", "motorc_bin", "phone_bin", "fan_bin", "comput_bin", "car_bin", "semach_bin", "q90_corr", "overall_valid"))

data$tobacco_bin <- ifelse(data$tobacco > 0, 1, 0)
# 
# 
NAPerVariable(data) # 16 missing all stuff on econ form, 29 missing asset index, 18 missing other smoke sources
# 
# 
# 
# # Add dummies for DOW and rainy season
# library(lubridate)
# head(analysis_data$startdate)
# analysis_data$startdate <- ymd_hms(analysis_data$startdate, tz = "GMT")
# str(analysis_data$startdate) 
# analysis_data$months <- months(analysis_data$startdate)
# analysis_data$rainy <- ifelse(analysis_data$months %in% c("April", "May", "June", "September", "October"), 1, 0)
# analysis_data$weekday <- wday(analysis_data$startdate) # Sunday is 1
# 




# Cluster-robust standard errors --------
# from https://thetarzan.wordpress.com/2011/06/11/clustered-standard-errors-in-r/
# In R, you first must run a function here called cl() written by Mahmood Arai in Stockholm University – the backup can be found here and here. http://people.su.se/~ma/clustering.pdf

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
summary(bpdata$BMI)
summary(bpdata$anthrop_validity) # max is 2: ok








## CONFIDENCE INTERVALS-----
# 95% CI: coef +/- SE*t^alpha/2,n-k-1
# unadjusted: n = 855, k = 1
# t^0.025,854 = 1.963
# adjusted: n = 855, k = 6
# t^0.025,848 = 1.963 (same)

### FINAL MODEL -------

data <- subset(data, select = c("mstudyid", "sbp", "dbp", "mean_corr", "q98_corr", "age", "BMI", "gestwks", "asset_index", "tobacco_bin", "vil_code", "overall_valid"))

nrow(data) #892

# Unadjusted final model ------
bpdata_complete <- data[complete.cases(data),] 


nrow(bpdata_complete) #855 (includes some validity = 2's - for sensitivity)

bpdata_valid <- bpdata_complete[bpdata_complete$overall_valid ==1,]
nrow(bpdata_valid) # 807

plot(bpdata_valid$mean_corr, bpdata_valid$sbp)
plot(bpdata_valid$mean_corr, bpdata_valid$dbp)
bpdata_valid$mean_corr[which.max(bpdata_valid$mean_corr)] #15.43 - check later that this is not driving the results

plot(density(bpdata_valid$mean_corr))

fm <- lm(sbp ~ mean_corr, data = bpdata_valid)
cl(fm = fm, data = bpdata_valid, cluster = bpdata_valid$vil_code) # for cluster-robust standard errors

#              Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) 105.15121    0.59462 176.8383  < 2e-16 ***
#  mean_corr     0.52849    0.28578   1.8493  0.06478 . 

# CI: 0.52849 +/- 0.28578*1.963 [-0.03, 1.09]
plot(fm)
cooksD <- cooks.distance(fm) 
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD) # 847 and 525 > 0.04
bpdata_valid_sens <- bpdata_valid[!row.names(bpdata_valid) %in% names(cooksD[cooksD > 0.04]),]
max(bpdata_valid_sens$mean_corr) #11.26
nrow(bpdata_valid_sens) # 805

fm <- lm(sbp ~ mean_corr, data = bpdata_valid_sens)
cl(fm = fm, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code) # this is probably better model

# Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) 105.50720    0.61537 171.4533   <2e-16 ***
#  mean_corr     0.27403    0.31161   0.8794   0.3794    
# CI: 0.27403 +/- 0.31161*1.963 [-0.34, 0.89]
plot(fm)

# sensitivity with bpdata_complete dataset
fm <- lm(sbp ~ mean_corr, data = bpdata_complete)
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code) # for cluster-robust standard errors

#               Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 1.0590e+02 4.2017e-01 252.038   <2e-16 ***
#  mean_corr   2.7469e-03 1.3090e-01   0.021   0.9833    

plot(fm)
cooksD <- cooks.distance(fm) 
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD) # 513, 847, and 220 > 0.04
bpdata_complete_sens <- bpdata_complete[!row.names(bpdata_complete) %in% names(cooksD[cooksD > 0.04]),]
nrow(bpdata_complete_sens) #852

max(bpdata_complete_sens$mean_corr) #27.99

fm <- lm(sbp ~ mean_corr, data = bpdata_complete_sens)
cl(fm = fm, data = bpdata_complete_sens, cluster = bpdata_complete_sens$vil_code)

#               Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) 105.738854   0.485173 217.9407   <2e-16 ***
#  mean_corr     0.095028   0.189755   0.5008   0.6166    

# CI: 0.095028 +/- 0.189755 * 1.963 [-0.28, 0.48]

# DBP
fm <- lm(dbp ~ mean_corr, data = bpdata_valid)
cl(fm = fm, data = bpdata_valid, cluster = bpdata_valid$vil_code)

# Estimate Std. Error  t value  Pr(>|t|)    
# (Intercept) 62.21991    0.50975 122.0599 < 2.2e-16 ***
#  mean_corr    0.74677    0.25639   2.9126  0.003684 ** 
# CI: 0.74677 +/- 0.25639*1.963 [0.24, 1.25]
plot(fm)
cooksD <- cooks.distance(fm) 
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD) # 1 > 0.04,  max is 0.44, this is the maximum CO value

bpdata_valid_sens <- bpdata_valid[!row.names(bpdata_valid) %in% names(cooksD[cooksD > 0.04]),]
nrow(bpdata_valid_sens) #806

fm <- lm(dbp ~ mean_corr, data = bpdata_valid_sens)
cl(fm = fm, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code) # this is probably better model

# Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) 62.51645    0.46573 134.2338  < 2e-16 ***
#  mean_corr    0.54574    0.22345   2.4423  0.01481 *  
# CI: 0.54574 +/- 0.22345 * 1.963 [0.11, 0.98]
plot(fm)

# sensitivity with bpdata_complete
fm <- lm(dbp ~ mean_corr, data = bpdata_complete)
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)

#             Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) 63.04015    0.37212 169.4086   <2e-16 ***
#  mean_corr    0.22796    0.14831   1.5371   0.1246  


cooksD <- cooks.distance(fm) 
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD) # 4 > 0.04,  max is 0.32: 220, 513, 847, 780

bpdata_complete_sens <- bpdata_complete[!row.names(bpdata_complete) %in% names(cooksD[cooksD > 0.04]),]
nrow(bpdata_complete_sens) #851
fm <- lm(dbp ~ mean_corr, data = bpdata_complete_sens)
cl(fm = fm, data = bpdata_complete_sens, cluster = bpdata_complete_sens$vil_code)

# Estimate Std. Error  t value  Pr(>|t|)    
# (Intercept) 62.51741    0.42762 146.1975 < 2.2e-16 ***
#  mean_corr    0.54817    0.18017   3.0425  0.002419 ** 

# CI: 0.54817 +/- 0.18017 * 1.963 [0.19, 0.90]
plot(fm)


# Adjusted final model --------
# SBP
fm <- lm(sbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_valid) 
cl(fm = fm, data = bpdata_valid, cluster = bpdata_valid$vil_code)

# Estimate Std. Error t value  Pr(>|t|)    
# (Intercept) 95.742176   3.222366 29.7118 < 2.2e-16 ***
#  mean_corr    0.522559   0.269428  1.9395 0.0527902 .  
#  age         -0.167535   0.050199 -3.3374 0.0008846 ***
#  BMI          0.732552   0.105121  6.9687 6.691e-12 ***
#  gestwks     -0.215822   0.099767 -2.1633 0.0308172 *  
#  asset_index -0.156031   0.186673 -0.8358 0.4034893    
#  tobacco_bin  1.783923   0.677388  2.6335 0.0086133 ** 
 
# CI:  0.522559 +/- 0.269428* 1.963 [-0.01, 1.05]
plot(fm)
cooksD <- cooks.distance(fm)
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD) # max is 0.04 
bpdata_valid_sens <- bpdata_valid[!row.names(bpdata_valid) %in% names(cooksD[cooksD > 0.04]),]
nrow(bpdata_valid_sens) #806

fm <- lm(sbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_valid_sens) 
cl(fm = fm, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code)

# Estimate Std. Error t value  Pr(>|t|)    
# (Intercept) 96.043827   3.189093 30.1163 < 2.2e-16 ***
#  mean_corr    0.561222   0.263971  2.1261  0.033803 *  
#  age         -0.161139   0.050679 -3.1796  0.001532 ** 
#  BMI          0.726039   0.104721  6.9331 8.494e-12 ***
#  gestwks     -0.239813   0.097452 -2.4608  0.014072 *  
#  asset_index -0.171514   0.183412 -0.9351  0.350004    
# tobacco_bin  1.480618   0.672660  2.2011  0.028012 *  
  
# CI 0.561222 +/- 0.263971*1.963 [0.04, 1.08]
plot(fm)

fm <- lm(sbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_complete) 
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: 0.03 (p-val = 0.82)


plot(fm)
cooksD <- cooks.distance(fm)
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD) # nothing greater than 0.037 so same result
bpdata_complete_sens <- bpdata_complete[!row.names(bpdata_complete) %in% names(cooksD[cooksD > 0.04]),]
nrow(bpdata_complete_sens) #855

# CI: 0.025062 +/- 0.110204*1.963 [-0.19, 0.24]


# DBP
fm <- lm(dbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_valid) 
cl(fm = fm, data = bpdata_valid, cluster = bpdata_valid$vil_code)

#              Estimate Std. Error t value  Pr(>|t|)    
# (Intercept) 58.197970   2.361435 24.6452 < 2.2e-16 ***
#   mean_corr    0.702466   0.228729  3.0712 0.0022044 ** 
#   age          0.016020   0.048561  0.3299 0.7415682    
#  BMI          0.281950   0.076901  3.6664 0.0002622 ***
#  gestwks     -0.200237   0.083069 -2.4105 0.0161551 *  
#  asset_index -0.141490   0.119097 -1.1880 0.2351757    
#  tobacco_bin  1.414879   0.894510  1.5817 0.1141051   

# CI:  0.702466 +/- 0.228729 * 1.963 [0.25, 1.15]
plot(fm)
cooksD <- cooks.distance(fm)
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD) # 847 is 0.09

bpdata_valid_sens <- bpdata_valid[!row.names(bpdata_valid) %in% names(cooksD[cooksD > 0.04]),]
nrow(bpdata_valid_sens) #806

fm <- lm(dbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_valid_sens) 
cl(fm = fm, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code)

# Estimate Std. Error t value  Pr(>|t|)    
# (Intercept) 58.500340   2.310669 25.3175 < 2.2e-16 ***
#  mean_corr    0.541661   0.216491  2.5020 0.0125480 *  
#  age          0.020885   0.048083  0.4344 0.6641400    
#  BMI          0.269454   0.073938  3.6443 0.0002854 ***
#  gestwks     -0.193264   0.083544 -2.3133 0.0209579 *  
#  asset_index -0.124676   0.119269 -1.0453 0.2961874    
# tobacco_bin  1.326704   0.897941  1.4775 0.1399370 

# CI: 0.541661 +/- 0.216491*1.963 [0.12, 0.97]
plot(fm)

fm <- lm(dbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_complete) 
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: 0.23, p-val = 0.06
cooksD <- cooks.distance(fm)
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD) # 4 values above 0.04 : 847, 220, 780, 513

bpdata_complete_sens <- bpdata_complete[!row.names(bpdata_complete) %in% names(cooksD[cooksD > 0.04]),]
nrow(bpdata_complete_sens) # 851
fm <- lm(dbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_complete_sens) 
cl(fm = fm, data = bpdata_complete_sens, cluster = bpdata_complete_sens$vil_code)
# CO coef = 0.53, p-val = 0.002
# CI: 0.533418 +/- 0.170825 * 1.963 [0.20, 0.87]
plot(fm)

# Sensitivity with q98: NOT HELPFUL-----
bpdata_complete$q98_fix <- bpdata_complete$q98_corr + 0.1 # don't have to do this anymore if not using log
bpdata_valid <- bpdata_complete[bpdata_complete$overall_valid ==1,]

# sbp

# crude
fm <- lm(sbp ~ q98_corr, data = bpdata_valid)
cl(fm = fm, data = bpdata_valid, cluster = bpdata_valid$vil_code)

# (Intercept) 105.488748   0.536882 196.4841   <2e-16 ***
# q98_fix       0.039677   0.028692   1.3828   0.1671   

cooksD <- cooks.distance(fm)
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD) # 2 values above 0.04

bpdata_valid_sens <- bpdata_valid[!row.names(bpdata_valid) %in% names(cooksD[cooksD > 0.04]),]

fm <- lm(sbp ~ q98_corr, data = bpdata_valid_sens)
cl(fm = fm, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code)

# (Intercept) 105.620203   0.614956 171.753   <2e-16 ***
#   q98_fix       0.027941   0.036098   0.774   0.4391  

plot(fm)

fm <- lm(sbp ~ q98_corr, data = bpdata_complete)
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: -0.005, p-val 0.56
cooksD <- cooks.distance(fm)
bpdata_complete_sens <- bpdata_complete[!row.names(bpdata_complete) %in% names(cooksD[cooksD > 0.04]),]
fm <- lm(sbp ~ q98_corr, data = bpdata_complete_sens)
cl(fm = fm, data = bpdata_complete_sens, cluster = bpdata_complete_sens$vil_code)


# adjusted
fm <- lm(sbp ~ q98_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_valid) 
cl(fm = fm, data = bpdata_valid, cluster = bpdata_valid$vil_code)
# Co coef: 0.04, p-val 0.13

cooksD <- cooks.distance(fm)
bpdata_valid_sens <- bpdata_valid[!row.names(bpdata_valid) %in% names(cooksD[cooksD > 0.04]),]
fm <- lm(sbp ~ q98_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_valid_sens) 
cl(fm = fm, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code)
# CO coef: 0.04, p-val 0.13

fm <- lm(sbp ~ q98_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_complete) 
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: -0.002, p-val -.73

cooksD <- cooks.distance(fm)
bpdata_complete_sens <- bpdata_complete[!row.names(bpdata_complete) %in% names(cooksD[cooksD > 0.04]),]
fm <- lm(sbp ~ q98_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_complete_sens) 
cl(fm = fm, data = bpdata_complete_sens, cluster = bpdata_complete_sens$vil_code)


# DBP
fm <- lm(dbp ~ q98_corr, data = bpdata_valid)
cl(fm = fm, data = bpdata_valid, cluster = bpdata_valid$vil_code)

# (Intercept) 62.649010   0.462930 135.3315  < 2e-16 ***
#  q98_fix      0.059806   0.026983   2.2164  0.02694 * 

cooksD <- cooks.distance(fm)
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
head(cooksD) # 2 values above 0.04

bpdata_valid_sens <- bpdata_valid[!row.names(bpdata_valid) %in% names(cooksD[cooksD > 0.04]),]
fm <- lm(dbp ~ q98_corr, data = bpdata_valid_sens)
cl(fm = fm, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code)

# (Intercept) 62.824244   0.472689 132.9082   <2e-16 ***
#  q98_fix      0.044209   0.027469   1.6094   0.1079   

fm <- lm(dbp ~ q98_corr, data = bpdata_complete)
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: 0.008 p-val 0.34

cooksD <- cooks.distance(fm)
bpdata_complete_sens <- bpdata_complete[!row.names(bpdata_complete) %in% names(cooksD[cooksD > 0.04]),]
fm <- lm(dbp ~ q98_corr, data = bpdata_complete_sens)
cl(fm = fm, data = bpdata_complete_sens, cluster = bpdata_complete_sens$vil_code)
# CO coef: 0.04, p-val 0.01

plot(fm) 

# adjusted

fm <- lm(dbp ~ q98_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_valid) 
cl(fm = fm, data = bpdata_valid, cluster = bpdata_valid$vil_code)
# Co coef: 0.06, p-val 0.02
cooksD <- cooks.distance(fm)

bpdata_valid_sens <- bpdata_valid[!row.names(bpdata_valid) %in% names(cooksD[cooksD > 0.04]),]
fm <- lm(dbp ~ q98_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_valid_sens) 
cl(fm = fm, data = bpdata_valid_sens, cluster = bpdata_valid_sens$vil_code)
# CO coef: 0.04, p-val 0.1

fm <- lm(dbp ~ q98_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_complete) 
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# Co coef: 0.009, p-val 0.26

cooksD <- cooks.distance(fm)
bpdata_complete_sens <- bpdata_complete[!row.names(bpdata_complete) %in% names(cooksD[cooksD > 0.04]),]

fm <- lm(dbp ~ q98_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_complete_sens) 
cl(fm = fm, data = bpdata_complete_sens, cluster = bpdata_complete_sens$vil_code)
# Co coef: 0.04, p-val 0.01



# Testing robust regression --------
# see http://www.ats.ucla.edu/stat/r/dae/rreg.htm
# This led to results very similar to using the complete data set
library(MASS)
# SBP
# crude
fm <- rlm(sbp ~ mean_corr, data = bpdata_valid)
cl(fm = fm, data = bpdata_valid, cluster = bpdata_valid$vil_code)
# CO coef: 0.55, p-val = 0.09

fm <- rlm(sbp ~ mean_corr, data = bpdata_complete)
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: -0.04, p-val = 0.69

# adjusted
fm <- lm(sbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_valid) 
cl(fm = fm, data = bpdata_valid, cluster = bpdata_valid$vil_code)
# CO coef: 0.52, p-val = 0.05

fm <- lm(sbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_complete) 
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: 0.03, p-val = 0.82

# DBP
# crude
fm <- rlm(dbp ~ mean_corr, data = bpdata_valid)
cl(fm = fm, data = bpdata_valid, cluster = bpdata_valid$vil_code)
# CO coef: 0.80   p-val =  0.001

fm <- rlm(dbp ~ mean_corr, data = bpdata_complete)
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: 0.19, p-val = 0.21

# adjusted
fm <- rlm(dbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_valid) 
cl(fm = fm, data = bpdata_valid, cluster = bpdata_valid$vil_code)
# CO coef: 0.80, p-val = 0.0008

fm <- rlm(dbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_complete) 
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: 0.20, p-val = 0.06

hweights <- data.frame(mstudyid = bpdata_complete$mstudyid, mean_corr = bpdata_complete$mean_corr, resid = fm$resid, weight = fm$w)
hweights2 <- hweights[order(fm$w), ]
hweights2[1:15, ]

d1 <- cooks.distance(fm) # must do from an lm not an rlm
r <- stdres(fm)
a <- cbind(bpdata_complete, d1, r) 
a <- a[order(a$d1, decreasing = TRUE),]
a[d1 > 4/855,c("mstudyid", "mean_corr", "d1", "r") ]

# ## TESTING INFLUENCE OF OUTLIERS -------
# # Getting rid of outliers one by one 
# outliers <- boxplot.stats(bpdata_complete$mean_corr, coef = 2)$out
# outliers <- outliers[order(outliers, decreasing = TRUE)]
# coefs <- matrix(nrow = length(outliers), ncol = 2)
# 
# # SBP
# for (i in 1:length(outliers)) { #17
#   data = bpdata_complete[bpdata_complete$mean_corr < outliers[i],]
#   fm <- lm(sbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = data) 
#   coef <- cl(fm = fm, data = data, cluster = data$vil_code)[2,1] # pulling out the coefficient for mean_corr
#   coefs[i,1] <- i
#   coefs[i,2] <- coef
# }
# plot(coefs[,1], coefs[,2], ylim = c(0, 0.5),xlab = "Number of CO outliers removed", ylab = "Beta coefficient for CO", main = "Effect of outlier removal on regression slope for SBP", cex.main = 0.9)
# lines(coefs[,1], coefs[,2])
# abline(v = length(boxplot.stats(bpdata_complete$mean_corr, coef = 3)$out), col = "blue", lty = "dotted", lwd =2)
# abline(v = length(boxplot.stats(bpdata_complete$mean_corr, coef = 3.5)$out), col = "red", lty = "dotted", lwd =2)
# abline(v = length(boxplot.stats(bpdata_complete$mean_corr, coef = 4)$out), col = "green", lty = "dotted", lwd =2)
# legend("topright", legend = c("3 x IQR", "3.5 x IQR", "4 x IQR"), col = c("blue", "red", "green"), lty = "dotted", lwd =2)
# 
# 
# # DBP
# pdf(file = "Effect of outliers on beta for DBP.pdf")
# for (i in 1:length(outliers)) { #17
#   data = bpdata_complete[bpdata_complete$mean_corr < outliers[i],]
#   fm <- lm(dbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = data) 
#   coef <- cl(fm = fm, data = data, cluster = data$vil_code)[2,1] # pulling out the coefficient for mean_corr
#   coefs[i,1] <- i
#   coefs[i,2] <- coef
# }
# plot(coefs[,1], coefs[,2], ylim = c(0.2, 0.8),xlab = "Number of CO outliers removed", ylab = "Beta coefficient for CO", main = "Effect of outlier removal on regression slope for DBP", cex.main = 0.9, xaxp = c(1,17,16))
# lines(coefs[,1], coefs[,2])
# abline(v = length(boxplot.stats(bpdata_complete$mean_corr, coef = 3)$out), col = "blue", lty = "dotted", lwd =2)
# 
# legend("topright", legend = c("3x IQR"), col = c("blue"), lty = "dotted", lwd =2)
# dev.off()
# 
# length(boxplot.stats(bpdata_complete$mean_corr, coef = 3)$out) # 9
# # for final model, 9 outliers removed at >3x IQR
# 
# # SBP, effect of influential variables on beta
# 
pdf(file = "Figure S1_Effect of cooks D on beta for BP.pdf", height = 10, width = 7)
par(mfrow = c(2,1))
coefs <- matrix(nrow = 11, ncol = 2)
fm <- lm(sbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_valid)
cooksD <- cooks.distance(fm)
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
cooksD <- cooksD[1:10]
for (i in 0:10) { 
  data = bpdata_valid[!row.names(bpdata_valid) %in% names(cooksD)[0:i],]
  fm <- lm(sbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = data) 
  coef <- cl(fm = fm, data = data, cluster = data$vil_code)[2,1] # pulling out the coefficient for mean_corr
  coefs[i+1,1] <- i
  coefs[i+1,2] <- coef
}
plot(coefs[,1], coefs[,2], ylim = c(0.2, 0.8),xlab = "Number of influential observations removed", ylab = "Beta coefficient for CO", main = "A. Effect of data point removal on regression slope for SBP", cex.main = 0.9, xaxp = c(0,10,10))
lines(coefs[,1], coefs[,2])



coefs <- matrix(nrow = 11, ncol = 2)
fm <- lm(dbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_valid)
cooksD <- cooks.distance(fm)
cooksD <- cooksD[order(cooksD, decreasing = TRUE)]
cooksD <- cooksD[1:10]
for (i in 0:10) { 
  data = bpdata_valid[!row.names(bpdata_valid) %in% names(cooksD)[0:i],]
  fm <- lm(dbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = data) 
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
# For comparison between originally enrolled women (1183) and final dataset (bpdata_valid, n = 807)
baselinebpdata <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/baselinedata_Feb12.rds")
nrow(baselinebpdata) #1183
baselinebpdata$final_analysis <- ifelse(baselinebpdata$mstudyid %in% bpdata_valid$mstudyid, 1, 0)
table(baselinebpdata$final_analysis) # 376 vs 807

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

# medlev
table(baselinebpdata$medlev[baselinebpdata$final_analysis == 1 & !is.na(baselinebpdata$medlev)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 1 &!is.na(baselinebpdata$medlev),]) *100
table(baselinebpdata$medlev[baselinebpdata$final_analysis == 0 & !is.na(baselinebpdata$medlev)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 0 ,]) *100
medlev <- table(baselinebpdata$medlev[!is.na(baselinebpdata$medlev)], baselinebpdata$final_analysis[!is.na(baselinebpdata$medlev)])
fisher.test(medlev)

table(baselinebpdata$married[baselinebpdata$final_analysis == 1 & !is.na(baselinebpdata$married)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 1 &!is.na(baselinebpdata$married),]) *100
table(baselinebpdata$married[baselinebpdata$final_analysis == 0 & !is.na(baselinebpdata$married)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 0,]) *100
married <- table(baselinebpdata$married[!is.na(baselinebpdata$married)], baselinebpdata$final_analysis[!is.na(baselinebpdata$married)], useNA = "always")
fisher.test(married)

table(baselinebpdata$salary[baselinebpdata$final_analysis == 1 & !is.na(baselinebpdata$salary)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 1 & !is.na(baselinebpdata$salary),]) *100
table(baselinebpdata$salary[baselinebpdata$final_analysis == 0 & !is.na(baselinebpdata$salary)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 0,]) *100
salary <- table(baselinebpdata$salary[!is.na(baselinebpdata$salary)], baselinebpdata$final_analysis[!is.na(baselinebpdata$salary)])
fisher.test(salary)

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