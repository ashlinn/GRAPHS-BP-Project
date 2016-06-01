# Acute ABP modeling

library(nlme)
library(plyr)
library(dplyr)
library(forecast)
library(lubridate)
library(scales)
library(zoo)

cc_peak <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/cc_peak_2016Feb10.rds")

cc_peak$morning2 <- ifelse(cc_peak$hrs_since_waking >= -1 & cc_peak$hrs_since_waking <=1, 1, 0) # use this since many plots look like the rise occurs before 0?
cc_peak$day_measurement2 <- ifelse(cc_peak$hrs_since_waking > 1 | cc_peak$datetime < cc_peak$sleeptime, 1, 0) # use this so no overlap between day and morning

cc_peak$morning <- cc_peak$morning2
cc_peak$day_measurement <- cc_peak$day_measurement2

cc_peak <- subset(cc_peak, select = -c(morning2, day_measurement2, night_measurement))

saveRDS(cc_peak, file = paste0("cc_peak_", format(Sys.Date(), format = "%b%d"), ".rds"))

# plot of time since waking
par(mfrow = c(2,1))
colors <- rainbow(n=20)
plot(cc_peak$hrs_since_waking, cc_peak$SBP_mean, main = "SBP", xlab = "hours since waking", ylab = "mean SBP (mmHg)", type = "n")
for (i in 1:length(unique(cc_peak$unique_visit))) {
  data <- filter(cc_peak, unique_visit == unique(cc_peak$unique_visit)[i])
  lines(data$hrs_since_waking, data$SBP_mean, col = colors[i])
  abline(v = data$hrs_since_waking[which.min(abs(difftime(data$datetime, data$sleeptime)))], col = colors[i], lty = "dotted")
}
points(-23:8, tapply(cc_peak$SBP_mean, cc_peak$hrs_since_waking, mean))
lines(-23:8, tapply(cc_peak$SBP_mean, cc_peak$hrs_since_waking, mean), lwd = 2)

plot(cc_peak$hrs_since_waking, cc_peak$DBP_mean, main = "DBP", xlab = "hours since waking", ylab = "mean DBP (mmHg)", type = "n")
for (i in 1:length(unique(cc_peak$unique_visit))) {
  data <- filter(cc_peak, unique_visit == unique(cc_peak$unique_visit)[i])
  lines(data$hrs_since_waking, data$DBP_mean, col = colors[i])
  abline(v = data$hrs_since_waking[which.min(abs(difftime(data$datetime, data$sleeptime)))], col = colors[i], lty = "dotted")  
}
points(-23:8, tapply(cc_peak$DBP_mean, cc_peak$hrs_since_waking, mean))
lines(-23:8, tapply(cc_peak$DBP_mean, cc_peak$hrs_since_waking, mean), lwd = 2)

# plot of each
par(mfrow = c(2,2))
for (i in 1:length(unique(cc_peak$unique_visit))) {
  data  <- filter(cc_peak, unique_visit == unique(cc_peak$unique_visit)[i])
  plot(data$hrs_since_waking, data$DBP_mean, ylim = c(20, 150), col = "blue", ylab = "BP (mmHg)", xlab = "Hours Since Waking", main = data$unique_visit[1], xlim = c(-23, 8))
  lines(data$hrs_since_waking, data$DBP_mean, col = "blue")
  points(data$hrs_since_waking, data$SBP_mean, col = "red")
  lines(data$hrs_since_waking, data$SBP_mean, col = "red")
  # rect(xleft = as.numeric(difftime(data$sleeptime, data$waketime))[1], ybottom = 20, xright = 0, ytop = 165, density = 80, angle = 135,col = "grey")
  rect(xleft = data$hrs_since_waking[1], ybottom = 20, xright = as.numeric(difftime(data$sleeptime, data$waketime))[1], ytop = 165, density = 80, angle = 135,col = "grey")
  if (data$hrs_since_waking[nrow(data)] > 1) {
  rect(xleft = 2, ybottom = 20, xright = data$hrs_since_waking[nrow(data)], ytop = 165, density = 80, angle = 135,col = "grey")}
  rect(xleft = -1, ybottom = 20, xright = 1, ytop = 165, density = 80, angle = 135,col = "yellow")

  lines(-23:8, tapply(cc_peak$SBP_mean, cc_peak$hrs_since_waking, mean), lwd = 2)

  lines(-23:8, tapply(cc_peak$DBP_mean, cc_peak$hrs_since_waking, mean), lwd = 2)
}


# how to deal with autocorrelation: correlation = corAR1?? 
for (i in 1:20) {
  data <- filter(cc_peak, unique_visit == unique(cc_peak$unique_visit)[i])
  data <- arrange(data, hrs_since_waking)
  print(auto.arima(data$SBP_mean))
}

# for both SBP and DBP, majority ARIMA(1,0,0) but some others. 
# lag 0, co_90

# #  model for SBP ----
fm <- lme(fixed = SBP_mean ~ co_90 + day_measurement + morning, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, correlation = corAR1(form = ~hrs_since_waking|unique_visit)) # pos, not sig, coef = 1.68 [1.23]/ using overall dist: not sig, 1.66 [1.49]
summary(fm)

print(pacf(residuals(fm1, type = "normalized"))) # good 




fm2 <- update(fm, fixed = ~.  + hrs_since_waking) # 1.64, not sig

fm2 <- update(fm, fixed = ~. + gestwks + BMI + age) # 1.68, not sig, gestwks, BMI, nd age not sig

# #  model for DBP -----
fm <- lme(fixed = DBP_mean ~ co_90 + day_measurement + morning, random = ~1|unique_visit, data = cc_peak, na.action = na.omit,correlation = corAR1(form = ~hrs_since_waking|unique_visit)) # pos, SIG, coef = 2.34/ using overall dist: 1.75 [1.27], not sig
summary(fm)

pacf(residuals(fm, type = "normalized")) # must use "normalized" residuals when looking at the resids accounting for the correlation structure - see http://stats.stackexchange.com/questions/80823/do-autocorrelated-residual-patterns-remain-even-in-models-with-appropriate-corre


fm2 <- update(fm, fixed = ~. + hrs_since_waking) # 2.32, sig 

fm2 <- update(fm, fixed = ~.  + gestwks + BMI + age) # 2.34, sig, getswks, BMI, age not sig


# CO_90_lag1
# SBP
fm1 <- lme(fixed = SBP_mean ~ co_90_lag1 + day_measurement + morning, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, correlation = corAR1(form = ~hrs_since_waking|unique_visit)) 
summary(fm1) # 2.19 [1.29], not sig/ using overall: 3.30 [1.49], SIG

print(pacf(residuals(fm1, type = "normalized"))) # good 


# DBP
fm1 <- lme(fixed = DBP_mean ~ co_90_lag1 + day_measurement + morning, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, correlation = corAR1(form = ~hrs_since_waking|unique_visit)) 
summary(fm1) # 1.45, not sig/ using overall: 3.11 [1.27], SIG

print(pacf(residuals(fm1, type = "normalized"))) # good 



# CO_90_lag2
# SBP
fm1 <- lme(fixed = SBP_mean ~ co_90_lag2 + day_measurement + morning, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, correlation = corAR1(form = ~hrs_since_waking|unique_visit)) 

summary(fm1) # neg, not sig
print(pacf(residuals(fm1, type = "normalized"))) # good 

# DBP
fm1 <- lme(fixed = DBP_mean ~ co_90_lag2 + day_measurement + morning, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, correlation = corAR1(form = ~hrs_since_waking|unique_visit)) 
summary(fm1) # neg, not sig

print(pacf(residuals(fm1, type = "normalized"))) # good 

fm2 <- update(fm1, fixed = ~. + morning) # -1.78, 1.10, 0.10
summary(fm2)
fm2 <- update(fm1, fixed = ~. + morning + hrs_since_waking)

# CO_corr_mean
fm2 <- lme(fixed = DBP_mean ~ co_corr_mean + day_measurement + morning, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, correlation = corAR1(form = ~hrs_since_waking|unique_visit)) 

print(pacf(residuals(fm2, type = "normalized"))) # good 
summary(fm2) # neg, not sig

# co_MA


fm2 <- lme(fixed = DBP_mean ~ co_MA + day_measurement + morning, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, correlation = corAR1(form = ~hrs_since_waking|unique_visit)) 

print(pacf(residuals(fm2, type = "normalized"))) # good 
summary(fm2) # small and not sig for both


# distributed lag?
# SBP
fm <-  lme(fixed = SBP_mean ~ co_90 + co_90_lag1 + co_90_lag2 + day_measurement + morning, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, correlation = corAR1(form = ~hrs_since_waking|unique_visit)) 
summary(fm) # only lag 1 is positive

fm <-  lme(fixed = SBP_mean ~ co_90 + co_90_lag1 + day_measurement + morning, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, correlation = corAR1(form = ~hrs_since_waking|unique_visit)) 
summary(fm) # 1.49 + 2.62 = 4.11, lag 1 sig/ using overall: 0.78 + 3.23 = 4.01, lag 1 sig


# DBP
fm <-  lme(fixed = DBP_mean ~ co_90 + co_90_lag1 + co_90_lag2 + day_measurement + morning, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, correlation = corAR1(form = ~hrs_since_waking|unique_visit)) 
summary(fm) #  # 0 and 1 positive, 2 and 3 negative

fm <-  lme(fixed = DBP_mean ~ co_90 + co_90_lag1 + day_measurement + morning, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, correlation = corAR1(form = ~hrs_since_waking|unique_visit)) 
summary(fm) # 2.35 + 1.78 = 4.13, co_90 sig/ using overall: 0.99 + 2. 99, lag1 sig

# moving average (co_MA)

cc_peak2 <- data.frame()
for (i in 1:20) {
  data <- filter(cc_peak, unique_visit == unique(cc_peak$unique_visit)[i])
  data$co_MA <- rollapply(data$co_corr_mean, width = 2, FUN = mean, align = "right", fill = NA) # 2-hour moving average (must align right: last hour and this hour)
  
  # for individual percentiles
#   data$co_70_MA <- ifelse(data$co_MA > quantile(data$co_MA, probs = 0.7, na.rm = TRUE), 1, 0)
#   data$co_75_MA <- ifelse(data$co_MA > quantile(data$co_MA, probs = 0.75, na.rm = TRUE), 1, 0)
#   data$co_80_MA <- ifelse(data$co_MA > quantile(data$co_MA, probs = 0.80, na.rm = TRUE), 1, 0)
#   data$co_85_MA <- ifelse(data$co_MA > quantile(data$co_MA, probs = 0.85, na.rm = TRUE), 1, 0)
#   data$co_90_MA <- ifelse(data$co_MA > quantile(data$co_MA, probs = 0.9, na.rm = TRUE), 1, 0) # # using individual visit's co quantiles
#   data$co_95_MA <- ifelse(data$co_MA > quantile(data$co_MA, probs = 0.95, na.rm = TRUE), 1, 0)
 cc_peak2 <- rbind(cc_peak2, data)
}
quantile(cc_peak2$co_MA, probs = c(0.7, 0.75, 0.8,0.85, 0.9, 0.95), na.rm = TRUE)



head(cc_peak2[, c("unique_visit", "co_corr_mean", "co_corr_mean_lag1", "co_MA")], n = 32)

cc_peak <- cc_peak2

quantile(cc_peak$co_MA, probs = c(0.7, 0.75, 0.8,0.85, 0.9, 0.95), na.rm = TRUE)


par(mfrow= c(1,1))
x <- c(70, 75, 80, 85, 90, 95)
y <- c(0.62, 1.13, 2.08, 1.59, 2.97, 4.51)
plot(x, y, xlab = "percentile of CO", ylab = "Beta Coefficient for increase in BP", ylim = c(0, 5), col = "blue")
summary(lm(y~x))
abline(lm(y~x), col = "blue")

a <- c(70, 75, 80, 85, 90, 95)
b <- c(1.07, 1.87, 1.95, 1.51, 2.59, 3.82)
points(a, b, col = "red")
summary(lm(b~a))
abline(lm(b~a), col = "red")
legend ("topleft", legend = c("SBP: Beta = 0.14**", "DBP: Beta = 0.08*"), col = c("blue", "red"), lwd = 2)

quantile(cc_peak$co_MA, probs = seq(from = 0.7, to = 0.95, by = 0.05), na.rm = TRUE)
for (i in 1:20) {
  data <- filter(cc_peak, unique_visit == unique(cc_peak$unique_visit)[i])
  print(c(unique(data$unique_visit), quantile(data$co_MA, probs = c(0.7, 0.95), na.rm = TRUE)))
}

# using overall percentiles --------
cc_peak$co_75 <- ifelse(cc_peak$co_corr_mean >=quantile(cc_peak$co_corr_mean, probs = 0.75, na.rm = TRUE), 1, 0)
cc_peak$co_90 <- ifelse(cc_peak$co_corr_mean >=quantile(cc_peak$co_corr_mean, probs = 0.9, na.rm = TRUE), 1, 0)

cc_peak2 <- data.frame()
for (i in 1:20) {
  data <- filter(cc_peak, unique_visit == unique(cc_peak$unique_visit)[i])
  data <- arrange(data, datetime)
  data$co_75_lag1 <- lag(data$co_75, 1)
  data$co_75_lag2 <- lag(data$co_75, 2)
  data$co_75_lag3 <- lag(data$co_75, 3)
  data$co_90_lag1 <- lag(data$co_90, 1)
  data$co_90_lag2 <- lag(data$co_90, 2)
  data$co_90_lag3 <- lag(data$co_90, 3)
  
  cc_peak2 <- rbind(cc_peak2, data)
}
cc_peak <- cc_peak2

cc_peak$co_05_MA <- ifelse(cc_peak$co_MA > quantile(cc_peak$co_MA, probs = 0.05, na.rm = TRUE), 1, 0)
cc_peak$co_10_MA <- ifelse(cc_peak$co_MA > quantile(cc_peak$co_MA, probs = 0.1, na.rm = TRUE), 1, 0)
cc_peak$co_15_MA <- ifelse(cc_peak$co_MA > quantile(cc_peak$co_MA, probs = 0.15, na.rm = TRUE), 1, 0)
cc_peak$co_20_MA <- ifelse(cc_peak$co_MA > quantile(cc_peak$co_MA, probs = 0.20, na.rm = TRUE), 1, 0)
cc_peak$co_25_MA <- ifelse(cc_peak$co_MA > quantile(cc_peak$co_MA, probs = 0.25, na.rm = TRUE), 1, 0)
cc_peak$co_30_MA <- ifelse(cc_peak$co_MA > quantile(cc_peak$co_MA, probs = 0.3, na.rm = TRUE), 1, 0)
cc_peak$co_35_MA <- ifelse(cc_peak$co_MA > quantile(cc_peak$co_MA, probs = 0.35, na.rm = TRUE), 1, 0)
cc_peak$co_40_MA <- ifelse(cc_peak$co_MA > quantile(cc_peak$co_MA, probs = 0.4, na.rm = TRUE), 1, 0)
cc_peak$co_45_MA <- ifelse(cc_peak$co_MA > quantile(cc_peak$co_MA, probs = 0.45, na.rm = TRUE), 1, 0)
cc_peak$co_50_MA <- ifelse(cc_peak$co_MA > quantile(cc_peak$co_MA, probs = 0.5, na.rm = TRUE), 1, 0)
cc_peak$co_55_MA <- ifelse(cc_peak$co_MA > quantile(cc_peak$co_MA, probs = 0.55, na.rm = TRUE), 1, 0)
cc_peak$co_60_MA <- ifelse(cc_peak$co_MA > quantile(cc_peak$co_MA, probs = 0.6, na.rm = TRUE), 1, 0)
cc_peak$co_65_MA <- ifelse(cc_peak$co_MA > quantile(cc_peak$co_MA, probs = 0.65, na.rm = TRUE), 1, 0)
cc_peak$co_70_MA <- ifelse(cc_peak$co_MA > quantile(cc_peak$co_MA, probs = 0.7, na.rm = TRUE), 1, 0)
cc_peak$co_75_MA <- ifelse(cc_peak$co_MA > quantile(cc_peak$co_MA, probs = 0.75, na.rm = TRUE), 1, 0)
cc_peak$co_80_MA <- ifelse(cc_peak$co_MA > quantile(cc_peak$co_MA, probs = 0.8, na.rm = TRUE), 1, 0)
cc_peak$co_85_MA <- ifelse(cc_peak$co_MA > quantile(cc_peak$co_MA, probs = 0.85, na.rm = TRUE), 1, 0)
cc_peak$co_90_MA <- ifelse(cc_peak$co_MA > quantile(cc_peak$co_MA, probs = 0.9, na.rm = TRUE), 1, 0)
cc_peak$co_95_MA <- ifelse(cc_peak$co_MA > quantile(cc_peak$co_MA, probs = 0.95, na.rm = TRUE), 1, 0)

saveRDS(cc_peak, file = paste0("cc_peak_", format(Sys.Date(), format = "%Y%b%d"), ".rds"))

# mean moving average
fm <-  lme(fixed = SBP_mean ~ co_MA + day_measurement + morning, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, correlation = corAR1(form = ~hrs_since_waking|unique_visit)) 
summary(fm)
fm <-  lme(fixed = DBP_mean ~ co_MA + day_measurement + morning, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, correlation = corAR1(form = ~hrs_since_waking|unique_visit)) 
summary(fm)


# CO_90_MA
table(cc_peak$unique_visit, cc_peak$co_80_MA) # 15 files contribute to this
table(cc_peak$unique_visit, cc_peak$co_85_MA) # 15 files contribute to this
table(cc_peak$unique_visit, cc_peak$co_90_MA) # 12 files contribute to this
table(cc_peak$unique_visit, cc_peak$co_95_MA) # 7 files contribute to this

# FINAL MODELS-------
library(papeR) # for confint
fm <-  lme(fixed = SBP_mean ~ co_90_MA + day_measurement + morning, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, correlation = corAR1(form = ~hrs_since_waking|unique_visit)) 
summary(fm) # 4.25 [1.59] 0.008
confint(fm)

fm1 <- update(fm, fixed = ~. + age + gestwks + BMI)
summary(fm1) # 4.3, none of addl are sig
fm1 <- update(fm, fixed = ~. + hrs_since_waking)
summary(fm1) # 4.1, hour not sig

fm <-  lme(fixed = DBP_mean ~ co_90_MA + day_measurement + morning, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, correlation = corAR1(form = ~hrs_since_waking|unique_visit)) 
summary(fm) # 4.54, SIG
confint(fm)

fm1 <- update(fm, fixed = ~. + age + gestwks + BMI)
summary(fm1) # 4.5, none of addl are sig
fm1 <- update(fm, fixed = ~. + hrs_since_waking)
summary(fm1) # 4.5, hour not sig

# tabulate and plot info for all percentiles--------
newtable <- data.frame()
for (i in c(42:46)) { # 50:62, 
  variable <- cc_peak[,i]
  print(names(cc_peak)[i])
  fm <-  lme(fixed = SBP_mean ~ variable + day_measurement + morning, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, correlation = corAR1(form = ~hrs_since_waking|unique_visit)) 
  print(summary(fm)$tTable[2,])
  newtable[i-41,1] <-  summary(fm)$tTable[2,1]
  newtable[i-41,2] <- intervals(fm)$fixed[2,1]
  newtable[i-41,3] <- intervals(fm)$fixed[2,3]
  newtable[i-41,4] <- names(cc_peak)[i]
  newtable[i-41,5] <- substr(names(cc_peak)[i], 4, 5)
  fm2 <-  lme(fixed = DBP_mean ~ variable + day_measurement + morning, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, correlation = corAR1(form = ~hrs_since_waking|unique_visit)) 
  newtable[i-41,6] <-  summary(fm2)$tTable[2,1]
  newtable[i-41,7] <- intervals(fm2)$fixed[2,1]
  newtable[i-41,8] <- intervals(fm2)$fixed[2,3]
}

# for (i in c(50:62)) { # 50:62, 
#   variable <- cc_peak[,i]
#   print(names(cc_peak)[i])
#   fm <-  lme(fixed = SBP_mean ~ variable + day_measurement + morning, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, correlation = corAR1(form = ~hrs_since_waking|unique_visit)) 
#   print(summary(fm)$tTable[2,])
#   newtable[i-43,1] <-  summary(fm)$tTable[2,1]
#   newtable[i-43,2] <- intervals(fm)$fixed[2,1]
#   newtable[i-43,3] <- intervals(fm)$fixed[2,3]
#   newtable[i-43,4] <- names(cc_peak)[i]
#   newtable[i-43,5] <- substr(names(cc_peak)[i], 4, 5)
#   fm2 <-  lme(fixed = DBP_mean ~ variable + day_measurement + morning, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, correlation = corAR1(form = ~hrs_since_waking|unique_visit)) 
#   newtable[i-43,6] <-  summary(fm2)$tTable[2,1]
#   newtable[i-43,7] <- intervals(fm2)$fixed[2,1]
#   newtable[i-43,8] <- intervals(fm2)$fixed[2,3]
# }

newtable <- newtable[c(2:4, 1, 5),]
names(newtable) <- c("SBP_coef", "SBP_five", "SBP_ninetyfive", "name", "percentile", "DBP_coef", "DBP_five", "DBP_ninetyfive")

# # Plot of CO Moving Average Percentiles -----
# par(mfrow = c(2,1), mar = c(5,4,1,2))
# plot(seq(from = 5, to = 95, by = 1),quantile(cc_peak$co_MA, probs = seq(from = 0.05, to = .95, by = 0.01), na.rm = TRUE), xlab = "", ylab = "CO (ppm)", type = "l", col = "coral", lwd = 3, xaxt = "n")
# axis(1, at = seq(from = 5, to = 95, by = 5))
# abline(h  = 0, col = "grey")
# 
# legend("topleft", legend = "CO", col = "coral", lwd =2, cex = 0.8)
# 
# # Plot of Beta against Percentile -----------
# pdf(file = "BP_CO_beta_by_percentile.pdf", bg = "white", height = 8, width = 10)
# plot(newtable$percentile, newtable$SBP_coef, col = "red", xlab = "Percentile of CO (2-hour moving average)", ylab = "Beta coefficient for increase in hourly BP (mmHg)", pch = 16, ylim = c(-5, 8), type = "n", xaxt = "n")
# axis(1, at = newtable$percentile)
# polygon(x = c(newtable$percentile, rev(newtable$percentile)), y = c(newtable$SBP_five, rev(newtable$SBP_ninetyfive)), col = alpha('red', alpha = 0.4), border = NA, density = 30)
# polygon(x = c(newtable$percentile, rev(newtable$percentile)), y = c(newtable$DBP_five, rev(newtable$DBP_ninetyfive)), col = alpha('blue', alpha = 0.4), border = NA, density = 30, angle = 135 )
# points(newtable$percentile, newtable$SBP_coef, col = "red3", pch = 16)
# lines(newtable$percentile, newtable$SBP_coef, col = "red3")
# points(newtable$percentile, newtable$DBP_coef, col = "blue2", pch = 16)
# lines(newtable$percentile, newtable$DBP_coef, col = "blue2")
# abline(h = 0, col = "grey")
# legend("topleft", legend = c("SBP", "DBP"), col = c("red3", "blue2"), lwd = 2, cex = 0.8, horiz = TRUE)
# dev.off()

# FIGURE 2 with CO percentiles -------
pdf(file = "CO_percentiles_against_BP.pdf", width = 6, height = 8)
par(mfrow = c(3,1), mar = c(4,4,2,1))
plot(seq(from = 75, to = 95, by = 1),quantile(cc_peak$co_MA, probs = seq(from = 0.75, to = .95, by = 0.01), na.rm = TRUE), xlab = "", ylab = "CO (ppm)", type = "l", col = "green4", lwd = 3, xaxt = "n", ylim = c(0,7))
title("a.", adj = 0, cex.main = 1.2)
axis(1, at = seq(from = 75, to = 95, by = 5))
grid()
legend("topleft", legend = "CO", col = "green4", lwd =3, cex = 0.8)

plot(newtable$percentile, newtable$SBP_coef, col = "red", xlab = "", ylab = "Beta coefficient for increase in BP (mmHg)", pch = 16, ylim = c(-5, 8), type = "n", xaxt = "n")
title("b.", adj = 0, cex.main = 1.2)
axis(1, at = newtable$percentile)
polygon(x = c(newtable$percentile, rev(newtable$percentile)), y = c(newtable$SBP_five, rev(newtable$SBP_ninetyfive)), col = alpha('red', alpha = 0.4), border = NA, density = 30)
points(newtable$percentile, newtable$SBP_coef, col = "red3", pch = 16)
lines(newtable$percentile, newtable$SBP_coef, col = "red3")
abline(h = 0, col = "grey")
grid()
legend("topleft", legend = "SBP", col = "red3", lwd = 2, cex = 0.8)

plot(newtable$percentile, newtable$SBP_coef, col = "red", xlab = "Percentile of CO (2-hour moving average)", ylab = "Beta coefficient for increase in BP (mmHg)", pch = 16, ylim = c(-5, 8), type = "n", xaxt = "n")
title("c.", adj = 0, cex.main = 1.2)
axis(1, at = newtable$percentile)
polygon(x = c(newtable$percentile, rev(newtable$percentile)), y = c(newtable$DBP_five, rev(newtable$DBP_ninetyfive)), col = alpha('blue', alpha = 0.4), border = NA, density = 30)
points(newtable$percentile, newtable$DBP_coef, col = "blue2", pch = 16)
lines(newtable$percentile, newtable$DBP_coef, col = "blue2")
abline(h = 0, col = "grey")
grid()
legend("topleft", legend = "DBP", col = "blue2", lwd = 2, cex = 0.8)

dev.off()



par(mfrow = c(2,2)) 
  hist(cc_peak$co_70_MA, main = "co_70_MA")
  hist(cc_peak$co_75_MA,  main = "co_75_MA")
  hist(cc_peak$co_80_MA,  main = "co_80_MA")
  hist(cc_peak$co_85_MA,  main = "co_85_MA")
  hist(cc_peak$co_90_MA,  main = "co_90_MA")
  hist(cc_peak$co_95_MA,  main = "co_95_MA")
  
hist(table(cc_peak$unique_visit, cc_peak$co_70_MA)[,2], breaks = 20, main = "co_70_MA")
hist(table(cc_peak$unique_visit, cc_peak$co_75_MA)[,2], breaks = 20, main = "co_75_MA")
hist(table(cc_peak$unique_visit, cc_peak$co_80_MA)[,2], breaks = 20, main = "co_80_MA")
hist(table(cc_peak$unique_visit, cc_peak$co_85_MA)[,2], breaks = 20, main = "co_85_MA")
hist(table(cc_peak$unique_visit, cc_peak$co_90_MA)[,2], breaks = 20, main = "co_90_MA")
hist(table(cc_peak$unique_visit, cc_peak$co_95_MA)[,2], breaks = 20, main = "co_95_MA")



par(mfrow= c(1,1))
x <- c(70, 75, 80, 85, 90, 95)
y <- c(1.3, 1.11, 1.5, 2.01, 4.25, 1.04)
plot(x, y, xlab = "percentile of CO", ylab = "Beta Coefficient for increase in BP", ylim = c(0, 5), col = "blue")
lines(x,y, col = "blue")
# summary(lm(y~x))
# abline(lm(y~x), col = "blue")

a <- c(70, 75, 80, 85, 90, 95)
b <- c(1.71, 1.9, 1.23, 2.6, 4.54, 0.98)
points(a, b, col = "red")
lines(a,b, col = "red")
# summary(lm(b~a))
# abline(lm(b~a), col = "red")
legend ("topleft", legend = c("SBP", "DBP"), col = c("blue", "red"), lwd = 2)

# IQR
IQR(cc_peak$co_MA, na.rm = TRUE)
cc_peak$co_IQR <- cc_peak$co_MA/1.762834
fm <-  lme(fixed = SBP_mean ~ co_IQR + day_measurement + morning, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, correlation = corAR1(form = ~hrs_since_waking|unique_visit)) 
summary(fm) # small, not sig

fm <-  lme(fixed = DBP_mean ~ co_IQR + day_measurement + morning, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, correlation = corAR1(form = ~hrs_since_waking|unique_visit)) 
summary(fm) # small, not sig

# Range of hourly CO exposure ------
median(cc_peak$co_corr_mean, na.rm = TRUE)
mean(cc_peak$co_corr_mean, na.rm = TRUE)
quantile(cc_peak$co_MA, na.rm = TRUE, probs = c(0.25, 0.75, 0.8, 0.9, 0.95))
