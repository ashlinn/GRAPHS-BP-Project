### GRAPHS Baseline BP Analysis


####################### DATA ANALYSIS ------#####################

# requires: lastest baselinebpdata file

# # remove extraneous variables
analysis_data <- subset(baselinebpdata, select = c("mstudyid", "vname", "sbp", "dbp", "bp", "smokecur", "smokpast", "shs", "age", "medlev", "married", "religion", "ethnic", "wownland", "farmln", "crops", "salary", "comptype", "ownhouse", "min_co", "mean_co", "q90", "q95", "q97", "q98", "q99", "max_co", "hours", "startdate", "htcm", "wtkg", "BMI", "gestwks", "asset_index", "cookingevents", "anthrop_validity", "bp_validity", "co_validity", "onelsecok", "coils", "invcom_sum", "invcoal_sum", "oth_smksources"))

names(analysis_data)
# [1] "mstudyid"         "vname"            "sbp"              "dbp"              "bp"              
# [6] "smokecur"         "smokpast"         "shs"              "age"              "medlev"          
# [11] "married"          "religion"         "ethnic"           "wownland"         "farmln"          
# [16] "crops"            "salary"           "comptype"         "ownhouse"         "min_co"          
# [21] "mean_co"          "q90"              "q95"              "q97"              "q98"             
# [26] "q99"              "max_co"           "hours"            "startdate"        "htcm"            
# [31] "wtkg"             "BMI"              "gestwks"          "asset_index"      "cookingevents"   
# [36] "anthrop_validity" "bp_validity"      "co_validity"      "onelsecok"        "coils"           
# [41] "invcom_sum"       "invcoal_sum"      "oth_smksources"  


NAPerVariable(analysis_data) # 95 missing age, 3 each missing: bp, smokecur, smokpast, shs,  31 missing all stuff on econ form, and asset index, 46 missing other smoke sources



# Add dummies for DOW and rainy season
library(lubridate)
head(analysis_data$startdate)
analysis_data$startdate <- ymd_hms(analysis_data$startdate, tz = "GMT")
str(analysis_data$startdate) 
analysis_data$months <- months(analysis_data$startdate)
analysis_data$rainy <- ifelse(analysis_data$months %in% c("April", "May", "June", "September", "October"), 1, 0)
analysis_data$weekday <- wday(analysis_data$startdate) # Sunday is 1








write.csv(analysis_data, file = paste0("data_bpanalysis_", format(Sys.Date(), format = "%b%d"), ".csv"), row.names = FALSE)





### ANALYSIS -----------
## TRY Q90, Q95, Q97 ----



# sbp and dbp data bell-shaped but with some outliers. CO data extremely right-skewed (some extreme outliers)

# log-transform all CO data. Don't log-transform the sbp and dbp data? (still think about outliers)

# robust regression? see http://www.ats.ucla.edu/stat/r/dae/rreg.htm


# Unadjusted analysis

analysis_data <- data_bpanalysis_Sep11


# set BMI outliers to NA
bpdata <- analysis_data
summary(bpdata$BMI)
bpdata$BMI <- ifelse(bpdata$anthrop_validity > 0, NA, bpdata$BMI)

# remove BP outliers from analysis
bpdata <- bpdata[bpdata$bp_validity < 3.5,] #883
row.names(bpdata) <- NULL

bpdata$occ_exp <- ifelse(bpdata$invcom_sum ==1 | bpdata$invcoal_sum ==1, 1, 0)
summary(bpdata$occ_exp)
sum(bpdata$occ_exp[!is.na(bpdata$occ_exp)]) #271

# exclude those who reported no cooking events
nocooking <- bpdata$mstudyid[bpdata$cookingevents ==0 &!is.na(bpdata$cookingevents)] #10
bpdata2 <- bpdata[!bpdata$mstudyid %in% nocooking,] #873
row.names(bpdata2) <- NULL


write.csv(bpdata2, file = paste0("final_analysis_data_", format(Sys.Date(), format = "%b%d"), ".csv"), row.names = FALSE) # this dataset excludes BP outliers and those with no cooking events



# Adjusted analyses -------
bpdata2 <- final_analysis_data_Oct04 # these values are not lining up with the ones from the analysis!

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


# DBP
lm_dbp <- lm(dbp ~ log(q98), data = bpdata2)
cooksD <- cooks.distance(lm_dbp) # using adjusted model with bpdata2 
max(cooksD) #.0923
length(cooksD) #744/ # Oct 4 742
tail(cooksD) # but the numbers go up to 870 (so these are row numbers in bpdata2 - the last 5 were excluded because of NAs)


length(cooksD[cooksD > 0.04]) #1
length(cooksD[cooksD > 0.03]) #3: 269, 663, 680
cooksD[cooksD > 0.03] # 269, 663, 680
bpdata6 <- bpdata2[-c(269),]
bpdata7 <- bpdata2[-c(269, 663),]
bpdata8 <- bpdata2[-c(269,663, 680),]


# Mccracken: age & BMI (linear); binary indicators for: smoking, SHS exposure, temescal use, household electricity. The asset index is the sum of binary indicators for having a bicycle, a radio, and a television, and was entered as categorical variable. To increase precision, we also considered time-varying covariates, such as apparent temperature, sea- son, day of the week, and time of day. We used linear terms to control for daily average apparent temperature and time of day and dummy variables for each day of the week and for rainy (1 May–31 October) versus dry season (1 November –30 April).

# complete case analysis

# linear variables: sbp, dbp, asset_index, gestwks, crops, farmln
# variables to log transform:
## age, mean_co, q90, max_co, BMI 
# factor variables: married, religion, ethnic, comptype, ownhouse, salary, weekday, [comptype]
# binary variables: bp, smokhh, smokcc, wownland, rainy


# Sensitivity analysis - influential variables -----
# final model variables - with the influential variable, bpdata2
lm_sbp <-  lm(sbp ~ log(q98) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata2)
summary(lm_sbp)
nrow(bpdata2) - 131 #742

lm_dbp <- lm(dbp ~ log(q98)+ log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata2)
summary(lm_dbp)

# testing for influential observations: use bpdata 3,4,5,6,7,8 and compare to bpdata2
lm_sbp <-  lm(sbp ~ log(q98) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata5)
lm_result(lm_sbp)
summary(lm_sbp)

lm_dbp <- lm(dbp ~ log(q98)+ log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata8)
lm_result(lm_dbp)




##### FINAL MODEL -----------
bpdata6 <- bpdata2[-c(269),]

# Unadjusted model
#SBP
# q98
lm1 <- lm(sbp ~ log(q98), data = bpdata6)
summary(lm1) # not signif, coef = 0.1226

# mean_co
lm2 <- lm(sbp ~ log(mean_co), data = bpdata6)
summary(lm2) # not signif, coef = 0.1658

#DBP
# q98 - *
lm4 <- lm(dbp ~ log(q98), data = bpdata6)
summary(lm4) # marginally signif, coef = 0.8133

# mean_co - **
lm5 <- lm(dbp ~ log(mean_co), data = bpdata6)
summary(lm5) # signif, coef = 0.8217


# Adjusted model

# Q98

lm_sbp <-  lm(sbp ~ log(q98) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata6)
summary(lm_sbp)
lm_result(lm_sbp)
# Call:
#   lm(formula = sbp ~ log(q98) + log(age) + log(BMI) + shs + asset_index + 
#        gestwks + invcoal_sum, data = bpdata6)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -28.45  -6.49  -0.46   6.65  46.38 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  72.2579     9.5783    7.54  1.4e-13 ***
#   log(q98)      0.4543     0.4519    1.01  0.31510    
# log(age)     -5.5946     1.4327   -3.91  0.00010 ***
#   log(BMI)     17.7143     2.8416    6.23  7.7e-10 ***
#   shs           2.3778     0.9135    2.60  0.00943 ** 
#   asset_index  -0.1528     0.1767   -0.86  0.38767    
# gestwks      -0.3182     0.0882   -3.61  0.00033 ***
#   invcoal_sum   1.2529     0.9261    1.35  0.17651    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 10 on 733 degrees of freedom
# (131 observations deleted due to missingness)
# Multiple R-squared:  0.0796,  Adjusted R-squared:  0.0708 
# F-statistic: 9.06 on 7 and 733 DF,  p-value: 9.38e-11


lm_dbp <- lm(dbp ~ log(q98)+ log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata6)
summary(lm_dbp)

# Call:
#   lm(formula = dbp ~ log(q98) + log(age) + log(BMI) + shs + asset_index + 
#        gestwks + invcoal_sum, data = bpdata6)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -23.89  -4.62  -0.48   3.97  35.29 
# 
# Coefficients:
#   Estimate Std. Error t value     Pr(>|t|)    
# (Intercept)  45.8082     7.6428    5.99 0.0000000032 ***
#   log(q98)      0.9509     0.3606    2.64      0.00854 ** 
#   log(age)     -0.4504     1.1432   -0.39      0.69372    
# log(BMI)      6.6749     2.2674    2.94      0.00334 ** 
#   shs           0.5461     0.7289    0.75      0.45398    
# asset_index  -0.2779     0.1410   -1.97      0.04914 *  
#   gestwks      -0.2487     0.0704   -3.53      0.00044 ***
#   invcoal_sum   0.2987     0.7390    0.40      0.68619    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 8.01 on 733 degrees of freedom
# (131 observations deleted due to missingness)
# Multiple R-squared:  0.0376,  Adjusted R-squared:  0.0284 
# F-statistic: 4.09 on 7 and 733 DF,  p-value: 0.000202

## MEAN CO
lm_sbp_mean <-  lm(sbp ~ log(mean_co) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata6)
summary(lm_sbp_mean)

# Call:
#   lm(formula = sbp ~ log(mean_co) + log(age) + log(BMI) + shs + 
#        asset_index + gestwks + invcoal_sum, data = bpdata6)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -28.562  -6.445  -0.368   6.576  46.579 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  72.93298    9.54414   7.642 6.72e-14 ***
#   log(mean_co)  0.45626    0.41941   1.088 0.277014    
# log(age)     -5.53736    1.42905  -3.875 0.000116 ***
#   log(BMI)     17.75278    2.84212   6.246 7.12e-10 ***
#   shs           2.36169    0.91379   2.584 0.009945 ** 
#   asset_index  -0.14718    0.17665  -0.833 0.405037    
# gestwks      -0.32215    0.08815  -3.654 0.000276 ***
#   invcoal_sum   1.26368    0.92596   1.365 0.172759    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 10.04 on 733 degrees of freedom
# (131 observations deleted due to missingness)
# Multiple R-squared:  0.07985,  Adjusted R-squared:  0.07107 
# F-statistic: 9.087 on 7 and 733 DF,  p-value: 8.659e-11


lm_dbp_mean <- lm(dbp ~ log(mean_co)+ log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata6)
summary(lm_dbp_mean)

# Call:
#   lm(formula = dbp ~ log(mean_co) + log(age) + log(BMI) + shs + 
#        asset_index + gestwks + invcoal_sum, data = bpdata6)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -23.854  -4.760  -0.507   4.086  35.813 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  47.19669    7.60251   6.208 8.98e-10 ***
#   log(mean_co)  1.03923    0.33408   3.111 0.001939 ** 
#   log(age)     -0.34060    1.13833  -0.299 0.764861    
# log(BMI)      6.77411    2.26393   2.992 0.002863 ** 
#   shs           0.50590    0.72789   0.695 0.487265    
# asset_index  -0.26598    0.14071  -1.890 0.059123 .  
# gestwks      -0.25722    0.07022  -3.663 0.000267 ***
#   invcoal_sum   0.32191    0.73759   0.436 0.662649    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 7.999 on 733 degrees of freedom
# (131 observations deleted due to missingness)
# Multiple R-squared:  0.04115,  Adjusted R-squared:  0.032 
# F-statistic: 4.494 on 7 and 733 DF,  p-value: 6.466e-05

# Sensitivity analysis - different percentiles CO -----
#SBP
#q95
lm_sbp95 <- lm(sbp ~ log(q95) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata6[!bpdata6$q95==0,])
summary(lm_sbp95) 

#q97
lm_sbp97 <- lm(sbp ~ log(q97) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata6[!bpdata6$q97==0,])
summary(lm_sbp97) 

#q99
lm_sbp99 <- lm(sbp ~ log(q99) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata6)
summary(lm_sbp99) 

#DBP
#q95
lm_dbp95 <- lm(dbp ~ log(q95) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata6[!bpdata6$q95==0,])
summary(lm_dbp95) 

#q97
lm_dbp97 <- lm(dbp ~ log(q97) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata6[!bpdata6$q97==0,])
summary(lm_dbp97) 

#q99
lm_dbp99 <- lm(dbp ~ log(q99) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, data = bpdata6)
summary(lm_dbp99) 





### Multiple imputation for missing values --------------
# Multiple imputation ---------

bpdata6$complete <- complete.cases(bpdata6)
# table of t.test p-values
pval <- matrix(nrow = 35, ncol = 2)
# pval[,1] <- names(analysis_data[2:36])
for (i in 8:35) {
  subset <- cbind(bpdata6[,i], bpdata6$complete)
  colnames(subset) <- c("variable", "complete")
  ttest <- t.test(variable~ complete, data = subset)
  pval[i,1] <- colnames(bpdata6[i])
  pval[i,2] <- round(ttest$p.value, digits = 3)
}
pval
# bp, medlev, asset index look significantly different between complete cases and missing? Others look ok

# correlations between CO and other variables
library(Hmisc)

rcorr(as.matrix(analysis_data[c(3:25, 27:30)]))

# q98 is correlated with: married (neg), ownhouse (neg), other CO measures
# q97 is correlated with: married, ownhouse, other CO measures
# q95 is correlated with: married, other CO measures
# q90 is correlated with: age, married, ethnic, comptype, other CO measures

# asset index is correlated with: medlev, ethnic, crops, salary, comptype, wtkg (not with age, married, wownland, farmln, ownhouse) - strange

# Using Amelia --------------------
# See http://www.unt.edu/rss/class/Jon/Benchmarks/MissingValueImputation_JDS_Nov2010.pdf and pdf vignettes from the Amelia package

library(mi)
library(Amelia)

imp_data <- subset(bpdata6, select = -c(smokecur, smokpast, startdate, htcm, wtkg, months, rainy, weekday, min_co, mean_co)) # remove current and past smoking since so few smokers, and other extraneous data


# remove some variables to reduce collinearity and only include the final analysis variables
imp_data98 <- subset(imp_data, select = -c(anthrop_validity, bp_validity, co_validity, q90, q95, q97, q99, max_co, hours, bp,  vname, ethnic, religion, comptype, farmln, salary, medlev, ownhouse, cookingevents, occ_exp, complete))

names(imp_data98)

# [1] "mstudyid"       "sbp"            "dbp"            "shs"            "age"           
# [6] "married"        "wownland"       "crops"          "q98"            "BMI"           
# [11] "gestwks"        "asset_index"    "onelsecok"      "coils"          "invcom_sum"    
# [16] "invcoal_sum"    "oth_smksources"   


# # same for q97
# imp_data97 <- subset(imp_data, select = -c(anthrop_validity, bp_validity, co_validity, q90, q95, q98, bp, crops, vname, ethnic, religion, comptype,wownland, farmln, salary, medlev, married, ownhouse, cookingevents))
# names(imp_data97)
# 
# # same for q95
# imp_data95 <- subset(imp_data, select = -c(anthrop_validity, bp_validity, co_validity, q90, q97, q98, bp, crops, vname, ethnic, religion, comptype,wownland, farmln, salary, medlev, married, ownhouse, cookingevents))
# names(imp_data95)
# 
# # same for q90
# imp_data90 <- subset(imp_data, select = -c(anthrop_validity, bp_validity, co_validity, q97, q95, q98, bp, crops, vname, ethnic, religion, comptype,wownland, farmln, salary, medlev, married, ownhouse))
# names(imp_data90)

# getting some info from mi on transformations
info <- mi.info(imp_data98)
processed <- mi.preprocess(imp_data98, info)
processed # look at these transformations from the mi package


set.seed(721)
a.out98 <- amelia(x = imp_data98, idvars = "mstudyid", logs = c("sbp", "dbp", "age", "BMI", "q98", noms = "married"))

# # add 0.1 to all CO values
# imp_data97$q97 <- imp_data97 + 0.1
# set.seed(721)
# a.out97 <- amelia(x = imp_data97, idvars = "mstudyid", logs = c("sbp", "dbp", "age", "BMI", "q97", noms = "married"))
# 
# imp_data95$q95 <- imp_data95 + 0.1
# set.seed(721)
# a.out95 <- amelia(x = imp_data95, idvars = "mstudyid", logs = c("sbp", "dbp", "age", "BMI", "q95", noms = "married"))



# variables treated as continuous or quasi-continuous (including binary variables): shs,   gestwks, asset_index
summary(a.out98)
plot(a.out98)

summary(a.out97)
plot(a.out97)

summary(a.out95)
plot(a.out95)

summary(a.out90)
plot(a.out90)



# imputes: shs, age, BMI, asset_index, invcoal_sum

save(a.out98, file = paste0("imputations98_", format(Sys.Date(), format = "%b%d"), ".RData"))
# save(a.out97, file = paste0("imputations97_", format(Sys.Date(), format = "%b%d"), ".RData"))
# save(a.out95, file = paste0("imputations95_", format(Sys.Date(), format = "%b%d"), ".RData"))
# save(a.out90, file = paste0("imputations90_", format(Sys.Date(), format = "%b%d"), ".RData"))

# Analysis with imputed data
library(Zelig) 

# checking
z.out1 <- zelig(sbp ~ log(mean_co) + log(age) + log(BMI) + factor(married) + factor(religion) + factor(ethnic) + factor(ownhouse) + factor(salary) + asset_index + gestwks, model = "ls", data = bpdata)
summary(z.out1) # results from zelig with the complete cases are exactly the same as for lm with same model

#using imputed dataset

# SBP
sbp_imp_98 <- zelig(sbp ~ log(q98) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, model = "ls", data = a.out98$imputations)
summary(sbp_imp_98) # not signif, coef = 0.262



# DBP
dbp_imp_98 <- zelig(dbp ~ log(q98) + log(age) + log(BMI) + shs + asset_index + gestwks + invcoal_sum, model = "ls", data = a.out98$imputations)
summary(dbp_imp_98) 




# stepwise selection

library(MASS)
bpdata_complete <- bpdata[complete.cases(bpdata),]

# for sbp
fit <- lm(sbp ~ log(q98) + log(age) + log(BMI) + asset_index + gestwks + shs  + cookingevents + wownland + crops + farmln + medlev + ownhouse + onelsecok + coils + invcom_sum + invcoal_sum + oth_smksources, data = bpdata_complete)
step <- stepAIC(fit, direction="both", na.action = na.omit)
step$anova # display results

# Final Model:
#  sbp ~ log(age) + log(BMI) + gestwks + shs + crops + invcoal_sum
# but keep CO in since it's our predictor of interest


# for dbp
fit <- lm(dbp ~ log(q98) + log(age) + log(BMI)  + asset_index + gestwks + shs  + cookingevents+ onelsecok + coils + invcom_sum + invcoal_sum + oth_smksources, data = bpdata_complete)
step <- stepAIC(fit, direction="both", na.action = na.omit)
step$anova # display results

# Final Model:
#   dbp ~ log(q98) + log(BMI) + asset_index + gestwks






# time trends: 
## apparent temperature? How would I get this info and how much does it vary across time independently of rainy vs dry season?
## day of week (of first sampling day?) - dummies
## rainy vs dry season - dummies

# Using Zelig to combine imputations in later analysis:

# Running the same model with imputed data is almost identical. Simply replace the original data set with the imputations from the amelia output:
# z.out.imp <- zelig(tariff ~ polity + pop + gdp.pc + year +country, data =  a.out$imputations, model = "ls")

# Scenario - what if all missing age toward the top end
quantile(bpdata6$age[!is.na(bpdata6$age)], probs = seq(from = 0, to = 1, by = 0.05)) #90%: 37, 95%: 40
bpdata11 <- bpdata6
bpdata11$age <- ifelse(is.na(bpdata11$age), 19, bpdata11$age)
summary(bpdata11$age)

lm_dbp <-  lm(dbp ~ log(q98) + log(age) + log(BMI) + shs + asset_index  + invcoal_sum + gestwks_ind, data = bpdata12)
summary(lm_dbp) # doesn't change it whether lo or hi

bpdata12 <- bpdata6
bpdata12$gestwks_ind <- ifelse(bpdata12$gestwks <21, 0, 1)
summary(bpdata12$gestwks_ind)
# Table 1 -----

# for continuous variables and NAs
library(pastecs)
options("scipen" = 6, "digits" = 3)
stat.desc(bpdata6)

# for binary variables
for (i in c(5,8,15,42,45)) {
  print(colnames(bpdata6)[i])
  print(nrow(bpdata6[which(bpdata6[,i] ==1),]))
  print(nrow(bpdata6[which(bpdata6[,i] ==1),])/nrow(bpdata6) * 100)
  print(nrow(bpdata6[is.na(bpdata6[,i]),])/nrow(bpdata6) * 100)
}

# for smokecur/smokpast
nrow(bpdata2[which(bpdata2$smokecur ==1 | bpdata2$smokpast ==1),])/nrow(bpdata2) * 100
nrow(bpdata[is.na(bpdata$smokecur),]) #3

# for categorical variables
for (i in c(10, 11,12,13,14,15,16,17,18, 19, 30)) {
  print(colnames(bpdata6)[i])
  print(summary(factor(bpdata6[,i]))/nrow(bpdata6) * 100) # note: NA percent is not valid using this method
}

summary(bpdata$cookingevents)
hist(bpdata$cookingevents)
nrow(bpdata[bpdata$cookingevents ==0 & !is.na(bpdata$cookingevents),]) #10


# for asset index quintiles
quints <- quantile(bpdata$asset_index, probs = seq(0,1,0.2), na.rm = TRUE)
sum(is.na(bpdata$asset_index)) # 32
r <- bpdata$asset_index[!is.na(bpdata$asset_index)]
for (i in 1:5){
  print(i)
  print(length(r[r >= quints[i] & r < quints[i+1]])/872)
}
#? not working

###### FIGURE 1: Example CO Plot
CO_stacked <- CO_stacked_Sep02
CO_stacked$datetime <- ymd_hms(CO_stacked$datetime, tz = "GMT")
directory <- "/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/CO minutewise plots/"
pdf(file = paste(directory, "SAMPLE_CO_minute_avg.pdf", sep = ""))
print(ggplot(data=filter(CO_stacked, mstudyid == "BM0289M"))+ geom_line(aes(datetime,co), color="blue") + labs(title=paste("Minute-Averaged CO \n", "Session 1", data$session[1]))+xlab("Date")+ ylab("CO (ppm)") + scale_x_datetime(breaks = date_breaks("1 day"), labels = date_format("%a %b %d %H:%M")) + expand_limits(y=0) +  theme(plot.title=element_text(size=rel(0.92), color = "darkgrey")))
dev.off()


###### FIGURE 2: Graph of CO Kernel Density ----
# q98
pdf(file = paste0("Fig2_CO_Density", format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 6)
d <- density(bpdata6$q98)
plot(d, ylim = c(0, 0.1), main = "Distribution of 98th-percentile measured 72-hour CO", lwd = 2, xlab = "98th percentile of CO(ppm)", ylab = "Probability density", xaxp = c(0, 150, 10), col = "coral3")
abline(v = mean(bpdata6$q98), lty = "dotted")
text(x = mean(bpdata6$q98), y = 0.09, pos = 4, label = paste("Average 98th percentile CO:\n", round(mean(bpdata6$q98), digits = 2), "\U00b1", round(sd(bpdata6$q98), digits = 2), "ppm"))
dev.off()

# mean_co
pdf(file = paste0("Fig2_CO_Density_meanCO_", format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 6)
d <- density(bpdata6$mean_co)
plot(d, main = "Distribution of Mean 72-hour CO", lwd = 2, xlab = "Mean CO(ppm)", ylab = "Probability density", col = "coral3")
abline(v = mean(bpdata6$mean_co), lty = "dotted")
text(x = mean(bpdata6$mean_co), y = 0.4, pos = 4, label = paste("Average CO:\n", round(mean(bpdata6$mean_co), digits = 2), "\U00b1", round(sd(bpdata6$mean_co), digits = 2), "ppm"))
dev.off()

##### FIGURE 3: Graph of BP Kernel Density ----
pdf(file = paste0("Fig3_BP_Density", format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 6)
e <- density(bpdata6$sbp)
plot(e, ylim = c(0, 0.1), xlim = c(0, 200), main = "Distribution of baseline BP", lwd = 2, xlab = "BP (mmHg)", ylab = "Probability density",  col = "purple")
f <- density(bpdata6$dbp)
lines(f, col = "darkgreen", lwd = 2)
abline(v = mean(bpdata6$sbp), lty = "dotted")
abline(v = mean(bpdata6$dbp), lty= "dotted")

text(x = mean(bpdata6$sbp), y = 0.09, pos = 4, label = paste("SBP:\n", round(mean(bpdata6$sbp), digits = 2), "\U00b1" , round(sd(bpdata6$sbp), digits = 2), "mmHg"))
text(x = mean(bpdata6$dbp), y = 0.09, pos = 2, label = paste("DBP:\n ", round(mean(bpdata6$dbp), digits = 2),  "\U00b1", round(sd(bpdata6$dbp), digits = 2), "mmHg"))
legend ("topright", legend = c("SBP", "DBP"), col = c("purple", "darkgreen"), lwd = 2)
dev.off()