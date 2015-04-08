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

nrow(baselinebpdata[is.na(baselinebpdata$overall_valid),]) #0 
nrow(baselinebpdata[baselinebpdata$overall_valid ==1,]) #855
nrow(baselinebpdata[baselinebpdata$overall_valid ==1 | (baselinebpdata$visually_valid == 2 & baselinebpdata$cf_conf == "hi" & baselinebpdata$duration_valid ==1),]) #906
baselinebpdata <- baselinebpdata[baselinebpdata$overall_valid ==1 | (baselinebpdata$visually_valid == 2 & baselinebpdata$cf_conf == "hi" & baselinebpdata$duration_valid ==1),]

nrow(baselinebpdata) #906

# removing BP outliers and individuals who didn't cook

baselinebpdata[baselinebpdata$bp_validity ==3.5, c("sbp", "dbp")] #2: 95/162 (imposs for SBP < DBP and DBP outlying), 145/100 (DBP is outlying)
nrow(baselinebpdata[is.na(baselinebpdata$bp_validity),]) #0
baselinebpdata <- baselinebpdata[!baselinebpdata$bp_validity ==3.5,]
nrow(baselinebpdata) # 904

nrow(baselinebpdata[baselinebpdata$cookingevents == 0 &!is.na(baselinebpdata$cookingevents),]) #11
nrow(baselinebpdata[is.na(baselinebpdata$cookingevents),]) #13
baselinebpdata[baselinebpdata$cookingevents ==0 &!is.na(baselinebpdata$cookingevents), c("cookingevents", "cookfood_days")]
baselinebpdata <- baselinebpdata[!baselinebpdata$cookingevents == 0 | is.na(baselinebpdata$cookingevents),] 

nrow(baselinebpdata) # 893

bpdata <- baselinebpdata
NAPerVariable(bpdata) # make sure additional NA's haven't jumped in

saveRDS(bpdata, file = paste0("bpdata", format(Sys.Date(), format = "%b%d"), ".rds")) # this dataset excludes BP outliers and those with no cooking events



### ANALYSIS --------
# Remove extraneous variables -------

bpdata <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/bpdataFeb12.rds")


data <- subset(bpdata, select = c("vil_code", "mstudyid", "sbp", "dbp", "bp", "smokecur", "smokpast", "shs", "age", "medlev", "married", "religion", "ethnic", "wownland", "farmln", "crops", "salary", "comptype", "ownhouse", "mean_corr", "q98_corr", "hours", "firstdate", "BMI", "gestwks", "asset_index", "cookingevents", "charcoal", "otherstoveuse", "mosqcoil", "tobacco", "mill", "threestone", "coalpot", "roadsale", "makecharcoal", "anthrop_validity", "bp_validity", "onelsecok", "coils", "invcom_sum", "invcoal_sum", "oth_smksources", "vil_pop", "vil_some_edu", "vil_modal_rel", "vil_modal_ethn", "village_ses2", "vil_hhsize", "vil_on_road", "people_all", "water", "toilet", "tabled_bin", "mattres_bin", "itn_bin", "radio_bin", "tv_bin", "fridge_bin", "bicycle_bin", "motorc_bin", "phone_bin", "fan_bin", "comput_bin", "car_bin", "semach_bin", "q90_corr"))

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
# 
# write.csv(analysis_data, file = paste0("data_bpanalysis_", format(Sys.Date(), format = "%b%d"), ".csv"), row.names = FALSE)
# 




# sbp and dbp data bell-shaped but with some outliers. CO data extremely right-skewed (some extreme outliers)

# log-transform all CO data. Don't log-transform the sbp and dbp data? (still think about outliers)

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


# bpdata$occ_exp <- ifelse(bpdata$invcom_sum ==1 | bpdata$invcoal_sum ==1, 1, 0)
# summary(bpdata$occ_exp)
# sum(bpdata$occ_exp[!is.na(bpdata$occ_exp)]) #271





# Adjusted analyses -------
# stepwise selection -----

library(MASS)



# for sbp
fit <- lm(sbp ~ log(mean_corr) + age + log(BMI)  + asset_index + gestwks + shs  + cookingevents+ onelsecok + invcom_sum + invcoal_sum + oth_smksources + as.factor(vil_modal_rel) + as.factor(vil_modal_ethn) + vil_on_road + log(people_all) + cookingevents + charcoal + otherstoveuse + mosqcoil + tobacco + mill + coalpot + roadsale + makecharcoal, data = bpdata_complete)
stepS <- stepAIC(fit, direction="both", na.action = na.omit)
stepS$anova # display results



# Stepwise Final Model:
# sbp ~ age + log(BMI) + asset_index + gestwks + vil_pop + vil_some_edu + vil_hhsize + tobacco + threestone
# but keep CO in since it's our predictor of interest


# for dbp
fit <- lm(dbp ~ log(mean_corr) + age + log(BMI)  + asset_index + gestwks + shs  + cookingevents+ onelsecok + invcom_sum + invcoal_sum + oth_smksources + as.factor(vil_modal_rel) + as.factor(vil_modal_ethn) + vil_on_road + log(people_all) + cookingevents + charcoal + otherstoveuse + mosqcoil + tobacco + mill + coalpot + roadsale + makecharcoal, data = bpdata_complete)
stepD <- stepAIC(fit, direction="both", na.action = na.omit)
stepD$anova # display results

# Stepwise Final Model:
#   dbp ~ log(mean_corr) + log(BMI) + gestwks + vil_pop + vil_some_edu + village_ses2 + vil_hhsize + tobacco + threestone + roadsale 
# (but include asset index rather than village SES?)


# variables to keep in final model: 
# ~ log(mean_corr) + age + log(BMI)  + asset_index + gestwks + tobacco_bin 


## CONFIDENCE INTERVALS-----
# 95% CI: coef +/- SE*t^alpha/2,n-k-1
# unadjusted: n = 855, k = 1
# t^0.025,854 = 1.963
# adjusted: n = 855, k = 6
# t^0.025,848 = 1.963 (same)

### FINAL MODEL -------

# Unadjusted final model ------
bpdata_complete <- data[complete.cases(data),] 
# tobacco_bin
bpdata_complete$tobacco_bin <- ifelse(bpdata_complete$tobacco > 0, 1, 0)

nrow(bpdata_complete) #855


fm <- lm(sbp ~ log(mean_corr), data = bpdata_complete)
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)


# Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)    105.885036   0.385522 274.6536   <2e-16 ***
#   log(mean_corr)   0.093744   0.362705   0.2585   0.7961    
# 95% CI: 0.093744 +/- 0.362705 * 1.963 = [-0.62, 0.81]



fm <- lm(dbp ~ log(mean_corr), data = bpdata_complete)
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)

# Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)    63.30449    0.31863 198.6750  < 2e-16 ***
#   log(mean_corr)  0.69297    0.30455   2.2754  0.02313 *  
# 95% CI: 0.69297 +/- 0.30455 * 1.963 = [0.10, 1.29]


# Adjusted final model --------
# SBP
fm <- lm(sbp ~ log(mean_corr) + age + log(BMI) + gestwks + asset_index + tobacco_bin, data = bpdata_complete) 

cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: 0.28
# 95% CI: 0.284560 +/- 0.325529 * 1.963 = [-0.35, 0.92]
# interp: for a 10% increase in CO, SBP increases by 0.28*log(1.1) = 0.03 points.
# for a doubling in CO, SBP increases by 0.28*log(2) = 0.2 points



# DBP
fm <- lm(dbp ~ log(mean_corr) + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_complete) 
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: 0.71, p-val = 0.01
# 95% CI: 0.7073621 +/-0.2779468 * 1.963 = [0.16, 1.25]
# interp: for a 10% increase in CO, DBP increases by 0.71*log(1.1) = 0.07 points.
# for a doubling in CO, DBP increases by 0.71*log(2) = 0.5 points


# Sensitivity with q98: -----
bpdata_complete$q98_fix <- bpdata_complete$q98_corr + 0.1

# sbp
fm <- lm(sbp ~ log(q98_fix), data = bpdata_complete)
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)

# Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)  105.816873   0.912500 115.9637   <2e-16 ***
#   log(q98_fix)   0.038604   0.366320   0.1054   0.9161 


# dbp
fm <- lm(dbp ~ log(q98_fix), data = bpdata_complete)
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)

# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  62.21011    0.71204 87.3687  < 2e-16 ***
#   log(q98_fix)  0.54767    0.29628  1.8485  0.06488 . 

# Testing on natural scale ------
# Must deal with outliers
# SBP, crude
fm <- lm(sbp ~ mean_corr, data = bpdata_complete)
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: .0028, p-val = 0.98

# adjusted
fm <- lm(sbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_complete) 
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: 0.03, p-val 0.82
# Interp: SBP goes up 0.03mmHG for every 1ppm increase in mean CO

# DBP, neither exposure nor outcome on log scale
# crude
fm <- lm(dbp ~ mean_corr, data = bpdata_complete)
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: 0.23, p-val = 0.13

# adjusted
fm <- lm(dbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_complete) 
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: 0.23, p-val 0.07
# interp: DBP goes up 0.23 mmHG for every 1ppm increase in mean CO

# Testing robust regression --------
# see http://www.ats.ucla.edu/stat/r/dae/rreg.htm
# This led to results very similar to using the complete data set
library(MASS)
# SBP, neither exposure nor outcome on log scale
# crude
fm <- rlm(sbp ~ mean_corr, data = bpdata_complete)
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: -0.04, p-val = 0.69

# adjusted
fm <- lm(sbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_complete) 
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: -0.02, p-val 0.83

# DBP
# crude
fm <- rlm(dbp ~ mean_corr, data = bpdata_complete)
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: 0.19, p-val = 0.21

# adjusted
fm <- lm(dbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_complete) 
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

## TESTING INFLUENCE OF OUTLIERS -------
# Getting rid of outliers one by one 
outliers <- boxplot.stats(bpdata_complete$mean_corr, coef = 2)$out
outliers <- outliers[order(outliers, decreasing = TRUE)]
coefs <- matrix(nrow = length(outliers), ncol = 2)

# SBP
for (i in 1:length(outliers)) { #17
  data = bpdata_complete[bpdata_complete$mean_corr < outliers[i],]
  fm <- lm(sbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = data) 
  coef <- cl(fm = fm, data = data, cluster = data$vil_code)[2,1] # pulling out the coefficient for mean_corr
  coefs[i,1] <- i
  coefs[i,2] <- coef
}
plot(coefs[,1], coefs[,2], ylim = c(0, 0.5),xlab = "Number of CO outliers removed", ylab = "Beta coefficient for CO", main = "Effect of outlier removal on regression slope for SBP", cex.main = 0.9)
lines(coefs[,1], coefs[,2])
abline(v = length(boxplot.stats(bpdata_complete$mean_corr, coef = 3)$out), col = "blue", lty = "dotted", lwd =2)
abline(v = length(boxplot.stats(bpdata_complete$mean_corr, coef = 3.5)$out), col = "red", lty = "dotted", lwd =2)
abline(v = length(boxplot.stats(bpdata_complete$mean_corr, coef = 4)$out), col = "green", lty = "dotted", lwd =2)
legend("topright", legend = c("3 x IQR", "3.5 x IQR", "4 x IQR"), col = c("blue", "red", "green"), lty = "dotted", lwd =2)


# DBP
pdf(file = "Effect of outliers on beta for DBP.pdf")
for (i in 1:length(outliers)) { #17
  data = bpdata_complete[bpdata_complete$mean_corr < outliers[i],]
  fm <- lm(dbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = data) 
  coef <- cl(fm = fm, data = data, cluster = data$vil_code)[2,1] # pulling out the coefficient for mean_corr
  coefs[i,1] <- i
  coefs[i,2] <- coef
}
plot(coefs[,1], coefs[,2], ylim = c(0.2, 0.8),xlab = "Number of CO outliers removed", ylab = "Beta coefficient for CO", main = "Effect of outlier removal on regression slope for DBP", cex.main = 0.9, xaxp = c(1,17,16))
lines(coefs[,1], coefs[,2])
abline(v = length(boxplot.stats(bpdata_complete$mean_corr, coef = 3)$out), col = "blue", lty = "dotted", lwd =2)

legend("topright", legend = c("3x IQR"), col = c("blue"), lty = "dotted", lwd =2)
dev.off()

length(boxplot.stats(bpdata_complete$mean_corr, coef = 3)$out) # 9
# for final model, 9 outliers removed at >3x IQR

# Sensitivity: without outliers greater than 3x the IQR for CO -----
bpdata_complete$q98_fix <- bpdata_complete$q98_corr + 0.1 # not nec if not on log scale
bpdata_complete$q90_fix <- bpdata_complete$q90_corr + 0.1


bpdata_rmout <-  bpdata_complete[!bpdata_complete$mean_corr %in% boxplot.stats(bpdata_complete$mean_corr, coef = 3)$out,]
nrow(bpdata_rmout) #846

# plots
pdf(file = "Testing outliers.pdf")
par(mfrow = c(1,2))
plot(bpdata_complete$mean_corr, bpdata_complete$sbp, main = "SBP", xlab = "Mean CO", ylab = "SBP")
legend("topright", legend = c("beta = 0.03, p-val = 0.82"))


plot(bpdata_rmout$mean_corr, bpdata_rmout$sbp, main = "SBP, no outliers",  xlab = "Mean CO", ylab = "SBP")
legend("topright", legend = c("beta = 0.34, p-val = 0.25"))


plot(bpdata_complete$mean_corr, bpdata_complete$dbp, main = "DBP", xlab = "Mean CO", ylab = "DBP")
legend("topright", legend = c("beta = 0.23, p-val = 0.07"))


plot(bpdata_rmout$mean_corr, bpdata_rmout$dbp, main = "DBP, no outliers",  xlab = "Mean CO", ylab = "DBP")
legend("topright", legend = c("beta = 0.48, p-val = 0.04"))
dev.off()





# SBP
# crude
fm <- lm(sbp ~ mean_corr, data = bpdata_rmout) 
cl(fm = fm, data = bpdata_rmout, cluster = bpdata_rmout$vil_code)
# CO coef: 0.27, p-val = 0.41
# 95% CI: 0.26945 +/- 0.32867 * 1.963 = [-0.376, 0.915]

# adjusted
fm <- lm(sbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_rmout) 
cl(fm = fm, data = bpdata_rmout, cluster = bpdata_rmout$vil_code)
# CO coef = 0.39, p-val = 0.21
# 95% CI: 0.394676 - 0.317346 * 1.963,  0.394676 + 0.317346 * 1.963 = [-0.228, 1.018]

# DBP
# crude
fm <- lm(dbp ~ mean_corr, data = bpdata_rmout)
cl(fm = fm, data = bpdata_rmout, cluster = bpdata_rmout$vil_code)
# CO coef: 0.51, p-val = 0.04
# 95% CI:  0.51419 +/- 0.24377*1.963 = [0.036, 0.993]


fm <- lm(dbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_rmout) 
cl(fm = fm, data = bpdata_rmout, cluster = bpdata_rmout$vil_code)
# coef = 0.52, p-val = 0.03
# 95% CI: 0.515821 - 0.239468*1.963; 0.515821 + 0.239468*1.963 = [0.046, 0.986]




# Sensitivity with q98 -------
# !! This is not at all the same. And not explained by different outliers. Why?
which(bpdata_complete$mean_corr %in% boxplot.stats(bpdata_complete$mean_corr, coef = 3)$out)
which(bpdata_complete$q98_corr %in% boxplot.stats(bpdata_complete$q98_corr, coef = 3)$out) # 17 including all but 1 of the 9 for mean_corr
bpdata_rmout2 <-  bpdata_complete[!bpdata_complete$q98_corr %in% boxplot.stats(bpdata_complete$q98_corr, coef = 3)$out,]

# SBP
# adjusted
fm <- lm(sbp ~ q98_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_rmout2) 
cl(fm = fm, data = bpdata_rmout2, cluster = bpdata_rmout2$vil_code)
# CO coef = 0.05, p-val = 0.25

# DBP
# adjusted
fm <- lm(dbp ~ q98_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_rmout2) 
cl(fm = fm, data = bpdata_rmout2, cluster = bpdata_rmout2$vil_code)
# CO coef = 0.05, p-val = 0.15



# Calculate the ICC (http://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&ved=0CB8QFjAA&url=http%3A%2F%2Fdavidakenny.net%2Fpapers%2Fk%26h%2FMLM_R.pdf&ei=vV8UVfTIIcG1sASF-YDgBA&usg=AFQjCNHY-jZyh8vCLVCdCDExDDIaivYVoA&sig2=k-8M3kpbjXSiiZ9L1zEM3Q&bvm=bv.89217033,d.cWc)

# null model
nullfit <- lme(fixed = sbp ~ 1, data = bpdata_rmout, random = ~1|vil_code)
summary(nullfit)
# Random effects:
#Formula: ~1 | vil_code
#(Intercept) Residual
#StdDev:   0.6865927 10.14731
(0.6865927^2)/( 0.6865927^2 + 10.14731^2) # use variance (SD^2) rather than SD, see link above
# ICC = 0.005

nullfit <- lme(fixed = dbp ~ 1, data = bpdata_rmout, random = ~1|vil_code)
summary(nullfit)
# Random effects:
#Formula: ~1 | vil_code
#(Intercept) Residual
#StdDev:  0.9680754 7.841403
0.9680754^2/(0.9680754^2 + 7.841403^2) # ICC = 0.015

# or from package multilevel
aov.1 <- aov(sbp ~ vil_code, bpdata_rmout)
ICC1(aov.1) # 0.018
aov.1 <- aov(dbp ~ vil_code, bpdata_rmout)
ICC1(aov.1) # 0.017



# using multilevel model instead of clustered errors (results practically identical)
library(nlme)

# SBP, all data
fit <- lme(fixed = sbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_complete, random = ~1|vil_code)
summary(fit)
# coef = 0.03, p-val = 0.83

# SBP, no outliers
fit <- lme(fixed = sbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_rmout, random = ~1|vil_code)
summary(fit)
# coef = 0.34, p-val = 0.23

# DBP, all data
fit <- lme(fixed = dbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_complete, random = ~1|vil_code)
summary(fit)
# coef = 0.24, p-val = 0.05

# DBP, no outliers
fit <- lme(fixed = dbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = bpdata_rmout, random = ~1|vil_code)
summary(fit)
# coef = 0.47, p-val = 0.04

# SBP, using 98th percentile
bpdata_complete$q98_fix <- bpdata_complete$q98_corr + 0.1
fm <- lm(sbp ~ log(q98_fix) + age + log(BMI) + gestwks + asset_index + tobacco_bin, data = bpdata_complete)
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: 0.09, p-val = 0.38


# DBP, using 98th percentile
bpdata_complete$q98_fix <- bpdata_complete$q98_corr + 0.1
fm <- lm(dbp ~ log(q98_fix) + age + log(BMI) + gestwks + asset_index + tobacco_bin, data = bpdata_complete)

cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: 0.56, p-val = 0.03

# SBP, using 90th percentile
bpdata_complete$q90_fix <- bpdata_complete$q90_corr + 0.1
fm <- lm(sbp ~ log(q90_fix) + age + log(BMI) + gestwks + asset_index + tobacco_bin, data = bpdata_complete)
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: -0.01, p-val = 0.96

# DBP, using 90th percentile
fm <- lm(dbp ~ log(q90_fix) + age + log(BMI) + gestwks + asset_index + tobacco_bin, data = bpdata_complete)

cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef: 0.32, p-val = 0.16

# Sens: CO outliers - are these driving the results?--------
boxplot.stats(bpdata_complete$mean_corr, coef = 3.5) # 8 outliers (above 7.2)
boxplot.stats(bpdata_complete$mean_corr, coef = 3) # 9 outliers (above 6.7)
boxplot.stats(bpdata_complete$mean_corr, coef = 2.5) # 15 ourliers (above 5.96)
boxplot.stats(bpdata_complete$mean_corr, coef = 2) # 17 outliers (above 5.3)

# Sens: multilevel model instead of clustered SEs -----------


# checking rainy and weekday
bpdata_complete$months <- months(bpdata_complete$firstdate)
bpdata_complete$rainy <- ifelse(bpdata_complete$months %in% c("April", "May", "June", "September", "October"), 1, 0)
bpdata_complete$weekday <- wday(bpdata_complete$firstdate) # Sunday is 1

# sensitivity: only the overall_valid =1 -------
baselinebpdata$tobacco_bin <- ifelse(baselinebpdata$tobacco > 0, 1, 0)
sens <- baselinebpdata[baselinebpdata$overall_valid ==1 & baselinebpdata$mstudyid %in% bpdata_complete$mstudyid,] #807
plot(sens$mean_corr, sens$sbp,  ylab = "SBP", xlab = "Mean CO", main = "only Validity = 1")
plot(bpdata_complete$mean_corr, bpdata_complete$sbp, ylab = "SBP", xlab = "Mean CO", main = "all data")
plot(sens$mean_corr, sens$dbp, ylab = "DBP", xlab = "Mean CO", main = "only Validity = 1")
plot(bpdata_complete$mean_corr, bpdata_complete$dbp, ylab = "DBP", xlab = "Mean CO", main = "all data")
# SBP
# crude
fm <- lm(sbp ~ mean_corr, data = sens) 
cl(fm = fm, data = sens, cluster = sens$vil_code)
#CO coef = 0.53, p=val = 0.07
# adjusted
fm <- lm(sbp ~ mean_corr + age + log(BMI) + gestwks + asset_index + tobacco_bin, data = sens) 
cl(fm = fm, data = sens, cluster = sens$vil_code)
# CO coef = 0.53, p-val = 0.05



# DBP
# crude
fm <- lm(dbp ~ mean_corr, data = sens) 
cl(fm = fm, data = sens, cluster = sens$vil_code)
# CO coef = 0.92, p-val = 0.01

fm <- lm(dbp ~ mean_corr, data = bpdata_complete) 
cl(fm = fm, data = bpdata_complete, cluster = bpdata_complete$vil_code)
# CO coef = 0.23, p-val = 0.12

# adjusted
fm <- lm(dbp ~ mean_corr + age + BMI + gestwks + asset_index + tobacco_bin, data = sens) 
cl(fm = fm, data = sens, cluster = sens$vil_code)
# CO coef: 0.90; p-val = 0.01

# diagnostics: Cook's Distance for influential observations
# Identify largest Cook's D 
# SBP

cooksD <- cooks.distance(lm_sbp) # using adjusted model with bpdata2
max(cooksD) #Oct 4 0.038
length(cooksD[cooksD > 0.04]) #1
length(cooksD[cooksD > 0.02]) #3: 680, 855, 29
which(cooksD > 0.02) 

bpdata3 <- bpdata2[-c(680),]
bpdata4 <- bpdata2[-c(680, 855),]
bpdata5 <- bpdata2[-c(680, 855, 29),]




# Mccracken: age & BMI (linear); binary indicators for: smoking, SHS exposure, temescal use, household electricity. The asset index is the sum of binary indicators for having a bicycle, a radio, and a television, and was entered as categorical variable. To increase precision, we also considered time-varying covariates, such as apparent temperature, sea- son, day of the week, and time of day. We used linear terms to control for daily average apparent temperature and time of day and dummy variables for each day of the week and for rainy (1 May–31 October) versus dry season (1 November –30 April).

# OLD (NOT CLEAN) DATA SET--------
# complete case analysis

# linear variables: sbp, dbp, asset_index, gestwks, crops, farmln
# variables to log transform:
## age, mean_co, q90, max_co, BMI 
# factor variables: married, religion, ethnic, comptype, ownhouse, salary, weekday, [comptype]
# binary variables: bp, smokhh, smokcc, wownland, rainy


# # Sensitivity analysis - influential variables -----
# # final model variables - with the influential variable, bpdata2
# lm_sbp <-  lm(sbp ~ log(q98) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata2)
# summary(lm_sbp)
# nrow(bpdata2) - 131 #742
# 
# lm_dbp <- lm(dbp ~ log(q98)+ log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata2)
# summary(lm_dbp)
# 
# # testing for influential observations: use bpdata 3,4,5,6,7,8 and compare to bpdata2
# lm_sbp <-  lm(sbp ~ log(q98) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata5)
# lm_result(lm_sbp)
# summary(lm_sbp)
# 
# lm_dbp <- lm(dbp ~ log(q98)+ log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata8)
# lm_result(lm_dbp)
# 
# 
# 
# 
# ##### FINAL MODEL -----------
# bpdata$q98_fix <- bpdata$q98_corr + 0.1
# 
# # Unadjusted model
# #SBP
# # q98
# fm <- lm(sbp ~ log(q98_fix), data = bpdata)
# cl(fm = fm, data = bpdata, cluster = bpdata$vil_code)
# # Estimate Std. Error  t value Pr(>|t|)    
# # (Intercept)  105.816873   0.912500 115.9637   <2e-16 ***
# #   log(q98_fix)   0.038604   0.366320   0.1054   0.9161    
# 
# # mean_co
# fm <- lm(sbp ~ log(mean_corr), data = bpdata)
# cl(fm = fm, data = bpdata, cluster = bpdata$vil_code)
# # Estimate Std. Error  t value Pr(>|t|)    
# # (Intercept)    106.000473   0.418996 252.9871   <2e-16 ***
# #   log(mean_corr)   0.012884   0.318080   0.0405   0.9677    
# 
# #DBP
# # q98 - .
# fm <- lm(dbp ~ log(q98_fix), data = bpdata)
# cl(fm = fm, data = bpdata, cluster = bpdata$vil_code)
# 
# # mean_co - *
# fm <- lm(dbp ~ log(mean_corr), data = bpdata)
# cl(fm = fm, data = bpdata, cluster = bpdata$vil_code)
# 
# 
# # Adjusted model
# 
# # Q98
# 
# lm_sbp <-  lm(sbp ~ log(q98) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata6)
# summary(lm_sbp)
# lm_result(lm_sbp)
# # Call:
# #   lm(formula = sbp ~ log(q98) + log(age) + log(BMI) + shs + asset_index + 
# #        gestwks + invcoal_sum, data = bpdata6)
# # 
# # Residuals:
# #   Min     1Q Median     3Q    Max 
# # -28.45  -6.49  -0.46   6.65  46.38 
# # 
# # Coefficients:
# #   Estimate Std. Error t value Pr(>|t|)    
# # (Intercept)  72.2579     9.5783    7.54  1.4e-13 ***
# #   log(q98)      0.4543     0.4519    1.01  0.31510    
# # log(age)     -5.5946     1.4327   -3.91  0.00010 ***
# #   log(BMI)     17.7143     2.8416    6.23  7.7e-10 ***
# #   shs           2.3778     0.9135    2.60  0.00943 ** 
# #   asset_index  -0.1528     0.1767   -0.86  0.38767    
# # gestwks      -0.3182     0.0882   -3.61  0.00033 ***
# #   invcoal_sum   1.2529     0.9261    1.35  0.17651    
# # ---
# #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# # 
# # Residual standard error: 10 on 733 degrees of freedom
# # (131 observations deleted due to missingness)
# # Multiple R-squared:  0.0796,  Adjusted R-squared:  0.0708 
# # F-statistic: 9.06 on 7 and 733 DF,  p-value: 9.38e-11
# 
# 
# lm_dbp <- lm(dbp ~ log(q98)+ log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata6)
# summary(lm_dbp)
# 
# # Call:
# #   lm(formula = dbp ~ log(q98) + log(age) + log(BMI) + shs + asset_index + 
# #        gestwks + invcoal_sum, data = bpdata6)
# # 
# # Residuals:
# #   Min     1Q Median     3Q    Max 
# # -23.89  -4.62  -0.48   3.97  35.29 
# # 
# # Coefficients:
# #   Estimate Std. Error t value     Pr(>|t|)    
# # (Intercept)  45.8082     7.6428    5.99 0.0000000032 ***
# #   log(q98)      0.9509     0.3606    2.64      0.00854 ** 
# #   log(age)     -0.4504     1.1432   -0.39      0.69372    
# # log(BMI)      6.6749     2.2674    2.94      0.00334 ** 
# #   shs           0.5461     0.7289    0.75      0.45398    
# # asset_index  -0.2779     0.1410   -1.97      0.04914 *  
# #   gestwks      -0.2487     0.0704   -3.53      0.00044 ***
# #   invcoal_sum   0.2987     0.7390    0.40      0.68619    
# # ---
# #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# # 
# # Residual standard error: 8.01 on 733 degrees of freedom
# # (131 observations deleted due to missingness)
# # Multiple R-squared:  0.0376,  Adjusted R-squared:  0.0284 
# # F-statistic: 4.09 on 7 and 733 DF,  p-value: 0.000202
# 
# ## MEAN CO
# lm_sbp_mean <-  lm(sbp ~ log(mean_co) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata6)
# summary(lm_sbp_mean)
# 
# # Call:
# #   lm(formula = sbp ~ log(mean_co) + log(age) + log(BMI) + shs + 
# #        asset_index + gestwks + invcoal_sum, data = bpdata6)
# # 
# # Residuals:
# #   Min      1Q  Median      3Q     Max 
# # -28.562  -6.445  -0.368   6.576  46.579 
# # 
# # Coefficients:
# #   Estimate Std. Error t value Pr(>|t|)    
# # (Intercept)  72.93298    9.54414   7.642 6.72e-14 ***
# #   log(mean_co)  0.45626    0.41941   1.088 0.277014    
# # log(age)     -5.53736    1.42905  -3.875 0.000116 ***
# #   log(BMI)     17.75278    2.84212   6.246 7.12e-10 ***
# #   shs           2.36169    0.91379   2.584 0.009945 ** 
# #   asset_index  -0.14718    0.17665  -0.833 0.405037    
# # gestwks      -0.32215    0.08815  -3.654 0.000276 ***
# #   invcoal_sum   1.26368    0.92596   1.365 0.172759    
# # ---
# #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# # 
# # Residual standard error: 10.04 on 733 degrees of freedom
# # (131 observations deleted due to missingness)
# # Multiple R-squared:  0.07985,  Adjusted R-squared:  0.07107 
# # F-statistic: 9.087 on 7 and 733 DF,  p-value: 8.659e-11
# 
# 
# lm_dbp_mean <- lm(dbp ~ log(mean_co)+ log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata6)
# summary(lm_dbp_mean)
# 
# # Call:
# #   lm(formula = dbp ~ log(mean_co) + log(age) + log(BMI) + shs + 
# #        asset_index + gestwks + invcoal_sum, data = bpdata6)
# # 
# # Residuals:
# #   Min      1Q  Median      3Q     Max 
# # -23.854  -4.760  -0.507   4.086  35.813 
# # 
# # Coefficients:
# #   Estimate Std. Error t value Pr(>|t|)    
# # (Intercept)  47.19669    7.60251   6.208 8.98e-10 ***
# #   log(mean_co)  1.03923    0.33408   3.111 0.001939 ** 
# #   log(age)     -0.34060    1.13833  -0.299 0.764861    
# # log(BMI)      6.77411    2.26393   2.992 0.002863 ** 
# #   shs           0.50590    0.72789   0.695 0.487265    
# # asset_index  -0.26598    0.14071  -1.890 0.059123 .  
# # gestwks      -0.25722    0.07022  -3.663 0.000267 ***
# #   invcoal_sum   0.32191    0.73759   0.436 0.662649    
# # ---
# #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# # 
# # Residual standard error: 7.999 on 733 degrees of freedom
# # (131 observations deleted due to missingness)
# # Multiple R-squared:  0.04115,  Adjusted R-squared:  0.032 
# # F-statistic: 4.494 on 7 and 733 DF,  p-value: 6.466e-05
# 
# # Sensitivity analysis - different percentiles CO -----
# #SBP
# #q95
# lm_sbp95 <- lm(sbp ~ log(q95) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata6[!bpdata6$q95==0,])
# summary(lm_sbp95) 
# 
# #q97
# lm_sbp97 <- lm(sbp ~ log(q97) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata6[!bpdata6$q97==0,])
# summary(lm_sbp97) 
# 
# #q99
# lm_sbp99 <- lm(sbp ~ log(q99) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata6)
# summary(lm_sbp99) 
# 
# #DBP
# #q95
# lm_dbp95 <- lm(dbp ~ log(q95) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata6[!bpdata6$q95==0,])
# summary(lm_dbp95) 
# 
# #q97
# lm_dbp97 <- lm(dbp ~ log(q97) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata6[!bpdata6$q97==0,])
# summary(lm_dbp97) 
# 
# #q99
# lm_dbp99 <- lm(dbp ~ log(q99) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata6)
# summary(lm_dbp99) 
# 
# 
# 
# 
# 
# ### Multiple imputation for missing values --------------
# # Multiple imputation ---------
# 
# bpdata6$complete <- complete.cases(bpdata6)
# # table of t.test p-values
# pval <- matrix(nrow = 35, ncol = 2)
# # pval[,1] <- names(analysis_data[2:36])
# for (i in 8:35) {
#   subset <- cbind(bpdata6[,i], bpdata6$complete)
#   colnames(subset) <- c("variable", "complete")
#   ttest <- t.test(variable~ complete, data = subset)
#   pval[i,1] <- colnames(bpdata6[i])
#   pval[i,2] <- round(ttest$p.value, digits = 3)
# }
# pval
# # bp, medlev, asset index look significantly different between complete cases and missing? Others look ok
# 
# # correlations between CO and other variables
# library(Hmisc)
# 
# rcorr(as.matrix(analysis_data[c(3:25, 27:30)]))
# 
# # q98 is correlated with: married (neg), ownhouse (neg), other CO measures
# # q97 is correlated with: married, ownhouse, other CO measures
# # q95 is correlated with: married, other CO measures
# # q90 is correlated with: age, married, ethnic, comptype, other CO measures
# 
# # asset index is correlated with: medlev, ethnic, crops, salary, comptype, wtkg (not with age, married, wownland, farmln, ownhouse) - strange
# 
# # Using Amelia --------------------
# # See http://www.unt.edu/rss/class/Jon/Benchmarks/MissingValueImputation_JDS_Nov2010.pdf and pdf vignettes from the Amelia package
# 
# library(mi)
# library(Amelia)
# 
# imp_data <- subset(bpdata6, select = -c(smokecur, smokpast, startdate, htcm, wtkg, months, rainy, weekday, min_co, mean_co)) # remove current and past smoking since so few smokers, and other extraneous data
# 
# 
# # remove some variables to reduce collinearity and only include the final analysis variables
# imp_data98 <- subset(imp_data, select = -c(anthrop_validity, bp_validity, co_validity, q90, q95, q97, q99, max_co, hours, bp,  vname, ethnic, religion, comptype, farmln, salary, medlev, ownhouse, cookingevents, occ_exp, complete))
# 
# names(imp_data98)
# 
# # [1] "mstudyid"       "sbp"            "dbp"            "shs"            "age"           
# # [6] "married"        "wownland"       "crops"          "q98"            "BMI"           
# # [11] "gestwks"        "asset_index"    "onelsecok"      "coils"          "invcom_sum"    
# # [16] "invcoal_sum"    "oth_smksources"   
# 
# 
# # # same for q97
# # imp_data97 <- subset(imp_data, select = -c(anthrop_validity, bp_validity, co_validity, q90, q95, q98, bp, crops, vname, ethnic, religion, comptype,wownland, farmln, salary, medlev, married, ownhouse, cookingevents))
# # names(imp_data97)
# # 
# # # same for q95
# # imp_data95 <- subset(imp_data, select = -c(anthrop_validity, bp_validity, co_validity, q90, q97, q98, bp, crops, vname, ethnic, religion, comptype,wownland, farmln, salary, medlev, married, ownhouse, cookingevents))
# # names(imp_data95)
# # 
# # # same for q90
# # imp_data90 <- subset(imp_data, select = -c(anthrop_validity, bp_validity, co_validity, q97, q95, q98, bp, crops, vname, ethnic, religion, comptype,wownland, farmln, salary, medlev, married, ownhouse))
# # names(imp_data90)
# 
# # getting some info from mi on transformations
# info <- mi.info(imp_data98)
# processed <- mi.preprocess(imp_data98, info)
# processed # look at these transformations from the mi package
# 
# 
# set.seed(721)
# a.out98 <- amelia(x = imp_data98, idvars = "mstudyid", logs = c("sbp", "dbp", "age", "BMI", "q98", noms = "married"))
# 
# # # add 0.1 to all CO values
# # imp_data97$q97 <- imp_data97 + 0.1
# # set.seed(721)
# # a.out97 <- amelia(x = imp_data97, idvars = "mstudyid", logs = c("sbp", "dbp", "age", "BMI", "q97", noms = "married"))
# # 
# # imp_data95$q95 <- imp_data95 + 0.1
# # set.seed(721)
# # a.out95 <- amelia(x = imp_data95, idvars = "mstudyid", logs = c("sbp", "dbp", "age", "BMI", "q95", noms = "married"))
# 
# 
# 
# # variables treated as continuous or quasi-continuous (including binary variables): shs,   gestwks, asset_index
# summary(a.out98)
# plot(a.out98)
# 
# summary(a.out97)
# plot(a.out97)
# 
# summary(a.out95)
# plot(a.out95)
# 
# summary(a.out90)
# plot(a.out90)
# 
# 
# 
# # imputes: shs, age, BMI, asset_index, invcoal_sum
# 
# save(a.out98, file = paste0("imputations98_", format(Sys.Date(), format = "%b%d"), ".RData"))
# # save(a.out97, file = paste0("imputations97_", format(Sys.Date(), format = "%b%d"), ".RData"))
# # save(a.out95, file = paste0("imputations95_", format(Sys.Date(), format = "%b%d"), ".RData"))
# # save(a.out90, file = paste0("imputations90_", format(Sys.Date(), format = "%b%d"), ".RData"))
# 
# # Analysis with imputed data
# library(Zelig) 
# 
# # checking
# z.out1 <- zelig(sbp ~ log(mean_co) + log(age) + log(BMI) + factor(married) + factor(religion) + factor(ethnic) + factor(ownhouse) + factor(salary) + asset_index + gestwks, model = "ls", data = bpdata)
# summary(z.out1) # results from zelig with the complete cases are exactly the same as for lm with same model
# 
# #using imputed dataset
# 
# # SBP
# sbp_imp_98 <- zelig(sbp ~ log(q98) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, model = "ls", data = a.out98$imputations)
# summary(sbp_imp_98) # not signif, coef = 0.262
# 
# 
# 
# # DBP
# dbp_imp_98 <- zelig(dbp ~ log(q98) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, model = "ls", data = a.out98$imputations)
# summary(dbp_imp_98) 
# 
# 
# 
# 
# 
# # time trends: 
# ## apparent temperature? How would I get this info and how much does it vary across time independently of rainy vs dry season?
# ## day of week (of first sampling day?) - dummies
# ## rainy vs dry season - dummies
# 
# # Using Zelig to combine imputations in later analysis:
# 
# # Running the same model with imputed data is almost identical. Simply replace the original data set with the imputations from the amelia output:
# # z.out.imp <- zelig(tariff ~ polity + pop + gdp.pc + year +country, data =  a.out$imputations, model = "ls")
# 



# Table 1 -----



# for continuous variables and NAs
library(pastecs)
options("scipen" = 6, "digits" = 3)
stat.desc(baselinebpdata[baselinebpdata$final_analysis == 1,c("age", "BMI", "gestwks", "asset_index")])
stat.desc(baselinebpdata[baselinebpdata$final_analysis == 0,c("age", "BMI", "gestwks", "asset_index")])


t.test(baselinebpdata$age[baselinebpdata$final_analysis ==1], baselinebpdata$age[baselinebpdata$final_analysis ==0])
t.test(baselinebpdata$BMI[baselinebpdata$final_analysis ==1], baselinebpdata$BMI[baselinebpdata$final_analysis ==0])
t.test(baselinebpdata$gestwks[baselinebpdata$final_analysis ==1], baselinebpdata$gestwks[baselinebpdata$final_analysis ==0])


t.test(baselinebpdata$bp[baselinebpdata$final_analysis ==1], baselinebpdata$bp[baselinebpdata$final_analysis ==0]) # for means only, this is not a continuous variable
fisher.test(table(baselinebpdata$bp, baselinebpdata$final_analysis))

t.test(baselinebpdata$shs[baselinebpdata$final_analysis ==1], baselinebpdata$shs[baselinebpdata$final_analysis ==0]) # for means only, this is not a continuous variable
fisher.test(table(baselinebpdata$shs, baselinebpdata$final_analysis))


# for smokecur/smokpast
baselinebpdata$ever_smoker <- ifelse(baselinebpdata$smokecur == 1 | baselinebpdata$smokpast == 1, 1, 0)
t.test(baselinebpdata$ever_smoker[baselinebpdata$final_analysis == 1], baselinebpdata$ever_smoker[baselinebpdata$final_analysis == 0]) # for means only, this is not a continuous variable
fisher.test(baselinebpdata$ever_smoker, baselinebpdata$final_analysis)


table(baselinebpdata$medlev[baselinebpdata$final_analysis == 0 & !is.na(baselinebpdata$medlev)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 0 &!is.na(baselinebpdata$medlev),]) *100
medlev <- table(baselinebpdata$medlev[!is.na(baselinebpdata$medlev)], baselinebpdata$final_analysis[!is.na(baselinebpdata$medlev)])
fisher.test(medlev)


table(baselinebpdata$married[baselinebpdata$final_analysis == 0 & !is.na(baselinebpdata$married)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 0 &!is.na(baselinebpdata$married),]) *100
married <- table(baselinebpdata$married[!is.na(baselinebpdata$married)], baselinebpdata$final_analysis[!is.na(baselinebpdata$married)], useNA = "always")
fisher.test(married)

table(baselinebpdata$salary[baselinebpdata$final_analysis == 0 & !is.na(baselinebpdata$salary)])/nrow(baselinebpdata[baselinebpdata$final_analysis == 0 & !is.na(baselinebpdata$salary),]) *100
salary <- table(baselinebpdata$salary[!is.na(baselinebpdata$salary)], baselinebpdata$final_analysis[!is.na(baselinebpdata$salary)])
fisher.test(salary)

table(baselinebpdata$water[baselinebpdata$final_analysis == 0], useNA = "always")/nrow(baselinebpdata[baselinebpdata$final_analysis == 0,]) *100
water <- table(baselinebpdata$water, baselinebpdata$final_analysis)
water <- water[1:6,]
fisher.test(water)

table(baselinebpdata$toilet[baselinebpdata$final_analysis == 0])/nrow(baselinebpdata[baselinebpdata$final_analysis == 0,]) *100
toilet <- table(baselinebpdata$toilet, baselinebpdata$final_analysis)
fisher.test(toilet)



names(bpdata_complete)[54:66]
for (i in 54:66) {
  print(names(bpdata_complete)[i])
  print(table(bpdata_complete[, i], useNA = "always")/855*100)
}

names(baselinebpdata)[183:203]
for (i in 183:203) {
  print(names(baselinebpdata)[i])
  print(table(baselinebpdata[baselinebpdata$final_analysis == 0, i], useNA = "always")/nrow(baselinebpdata[baselinebpdata$final_analysis == 0,])*100)
}

t.test(baselinebpdata$asset_index[baselinebpdata$final_analysis == 1], baselinebpdata$asset_index[baselinebpdata$final_analysis == 0])
summary(bpdata$cookingevents)
hist(bpdata$cookingevents)
nrow(bpdata[bpdata$cookingevents ==0 & !is.na(bpdata$cookingevents),]) #10

# For excluded people (1183-855 = 328)
baselinebpdata <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/baselinedata_Feb12.rds")
baselinebpdata$final_analysis <- ifelse(baselinebpdata$mstudyid %in% bpdata_complete$mstudyid, 1, 0)

library(pastecs)
options("scipen" = 6, "digits" = 3)
stat.desc(bpdata_complete[,c("age", "BMI", "gestwks")])

table(bpdata_complete$bp, useNA = "always")/855*100
table(bpdata_complete$shs, useNA = "always")/855*100


# for smokecur/smokpast
nrow(bpdata_complete[which(bpdata_complete$smokecur ==1 | bpdata_complete$smokpast ==1),])/nrow(bpdata_complete) * 100


table(bpdata_complete[, "medlev"], useNA = "always")/855*100
table(bpdata_complete[, "married"], useNA = "always")/855*100
table(bpdata_complete[, "salary"], useNA = "always")/855*100
table(bpdata_complete[, "water"], useNA = "always")/855*100
table(bpdata_complete[, "toilet"], useNA = "always")/855*100

names(bpdata_complete)[54:66]
for (i in 54:66) {
  print(names(bpdata_complete)[i])
  print(table(bpdata_complete[, i], useNA = "always")/855*100)
}


summary(bpdata$cookingevents)
hist(bpdata$cookingevents)
nrow(bpdata[bpdata$cookingevents ==0 & !is.na(bpdata$cookingevents),]) #10

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
d <- density(bpdata_complete$mean_corr)
plot(d, main = "", lwd = 2, xlab = "Mean CO(ppm)", ylab = "Probability density", col = "coral3")
abline(v = mean(bpdata_complete$mean_corr), lty = "dotted")
text(x = mean(bpdata_complete$mean_corr), y = 0.3, pos = 4, label = paste("Mean 72-hour-averaged CO:\n", round(mean(bpdata_complete$mean_corr), digits = 2), "\U00b1", round(sd(bpdata_complete$mean_corr), digits = 2), "ppm \n (range: ", round(range(bpdata_complete$mean_corr), digits = 3)[1], "to", round(range(bpdata_complete$mean_corr), digits = 1)[2], "ppm)"))
dev.off()

# sens (overall-valid = 1)
d <- density(sens$mean_corr)
plot(d, main = "", lwd = 2, xlab = "Mean CO(ppm)", ylab = "Probability density", col = "coral3")
abline(v = mean(sens$mean_corr), lty = "dotted")
text(x = mean(sens$mean_corr), y = 0.3, pos = 4, label = paste("Mean 72-hour-averaged CO:\n", round(mean(sens$mean_corr), digits = 2), "\U00b1", round(sd(sens$mean_corr), digits = 2), "ppm \n (range: ", round(range(sens$mean_corr), digits = 3)[1], "to", round(range(sens$mean_corr), digits = 1)[2], "ppm)"))

plot(sens$mean_corr, sens$sbp)
plot(bpdata_complete$mean_corr, bpdata_complete$dbp)

##### FIGURE 3: Graph of BP Kernel Density ----
pdf(file = paste0("Fig3_BP_Density", format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 6)
e <- density(bpdata_complete$sbp)
plot(e, ylim = c(0, 0.1), xlim = c(0, 200), main = "", lwd = 2, xlab = "BP (mmHg)", ylab = "Probability density",  col = "purple")
f <- density(bpdata_complete$dbp)
lines(f, col = "darkgreen", lwd = 2)
abline(v = mean(bpdata_complete$sbp), lty = "dotted")
abline(v = mean(bpdata_complete$dbp), lty= "dotted")

text(x = mean(bpdata_complete$sbp), y = 0.09, pos = 4, label = paste("Mean SBP:\n", round(mean(bpdata_complete$sbp), digits = 2), "\U00b1" , round(sd(bpdata_complete$sbp), digits = 2), "mmHg"))
text(x = mean(bpdata_complete$dbp), y = 0.09, pos = 2, label = paste("Mean DBP:\n ", round(mean(bpdata_complete$dbp), digits = 2),  "\U00b1", round(sd(bpdata_complete$dbp), digits = 2), "mmHg"))
legend ("topright", legend = c("SBP", "DBP"), col = c("purple", "darkgreen"), lwd = 2)
dev.off()