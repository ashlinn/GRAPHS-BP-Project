# Analysis of acute ABP/HAP relationship
# only 4 Upem plots line up so don't use. 
# Use the 20 CO plots that line up.
# Note that 5 of them have validity other than 0, but for these purposes LOOK ok.
# the 5 previously unvalidated CO plots are NOT synchronous with ABP.


library(foreign)
library(plyr)
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)


# Functions -------
make.n.lag<-function(x,n)
{
  xlag<-rep(NA,length(x)*(n+1))
  dim(xlag)<-c(length(x),(n+1))
  
  xlag[,1]<-x
  
  if(n!=0)
  {
    for(j in 2:(n+1))
    {
      for(i in 1:length(x))
      {
        if(i>(j-1))
        {
          xlag[i,j]<-x[i-(j-1)]
        }
      }
    }
  }
  xlag
}



# Process UPEM data -------
# will not end up using these.
# # function to load upem data from files named after filters
# get_upem <- function(file) {
#   dt <- read.csv(file, stringsAsFactors=F, header=T)[,c(1:5,23)]
#   #dt$datetime <- dmy_hms(dt$datetime, tz="GMT")
#   dt$filter<-strsplit(basename(file), split = "\\.")[[1]][1]
#   dt
# }
# 
# upem_files <- list.files("/Users/ashlinn/Dropbox/Ghana project/BP project/ABP Project/Final Data/ABP_upem/", full.names = T)
# length(upem_files) #25
# UPEM_stacked <- ldply(upem_files, get_upem, .progress = "text")
# UPEM_stacked <- UPEM_stacked[!is.na(UPEM_stacked$AdjConc),] # get rid of two invalid files
# 
# filematch <- read.csv("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/ABP_upem_validfiles.csv", header = TRUE, stringsAsFactors = FALSE)
# names(filematch) <- tolower(names(filematch))
# filematch$unique_visit <- paste(filematch$mstudyid, "V2", sep = "_")
# 
# UPEM_stacked <- merge(UPEM_stacked, filematch, by = "filter", all.x = TRUE)
# UPEM_stacked$mstudyid[UPEM_stacked$filter == "KHC1003"] <- "BM1418M_dup"
# UPEM_stacked$mstudyid[UPEM_stacked$filter == "KHC1006"] <- "BM1416M_dup"
# UPEM_stacked$unique_visit <- paste(UPEM_stacked$mstudyid, "V2", sep = "_")
# UPEM_stacked <- merge(UPEM_stacked, abpdata[,c("unique_visit", "sleeptime", "waketime")], by = "unique_visit", all.x = TRUE)
# UPEM_stacked$day_measurement <- ifelse(UPEM_stacked$unique_min <= UPEM_stacked$sleeptime | UPEM_stacked$unique_min >= UPEM_stacked$waketime, 1, 0)
# UPEM_stacked$night_measurement <- ifelse(UPEM_stacked$unique_min > UPEM_stacked$sleeptime & UPEM_stacked$unique_min < UPEM_stacked$waketime, 1, 0)
# 
# saveRDS(UPEM_stacked, file = paste0("UPEM_stacked_", format(Sys.Date(), format = "%b%d"), ".rds"))

# Load ABP data -------


abpdata <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/abpdata_Nov17.rds") # summary data

ABP_stacked <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/ABP_stacked_Oct13.rds")

ABP_stacked_noerr <- ABP_stacked[ABP_stacked$event_code ==0,]

# Get CO data ------
# See "CO_Stacking_for_ABP.R"

CO_stacked <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/CO_stacked_ABP_Nov28.rds") 

# Hourly and half-hourly CO averages ------
ABP_data <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/ABP_stacked_noerr_matched_Nov27.rds")

ABP_data$unique_hour <- floor_date(ABP_data$date_time, unit = "hour")

ABP_extrainfo <- ABP_data %.% group_by(unique_visit) %.% summarize(sleeptime = sleeptime[1], waketime = waketime[1])
ABP_hourly <- ABP_data[, c("unique_visit", "SBP", "DBP", "file", "unique_hour")] %.% group_by(file, unique_visit, unique_hour) %.% summarise_each(funs(mean, max))
ABP_hourly <- merge(ABP_hourly, ABP_extrainfo, by = "unique_visit")

CO_stacked_hourly <- CO_stacked[, c("file3", "overall_valid", "unique_visit", "unique_hour", "co_corr")] %.% group_by(file3, overall_valid, unique_visit, unique_hour) %.% summarise_each(funs(mean,max)) # use summarise_each


CO_stacked_30min <- CO_stacked[, c("file3", "overall_valid", "unique_visit", "unique_30", "co_corr")] %.% group_by(file3, unique_visit, overall_valid, unique_30) %.% summarise_each(funs(mean, max))


names(CO_stacked_hourly) <- c("file", "overall_valid", "unique_visit", "datetime", "co_corr_mean", "co_corr_max")
names(CO_stacked_30min) <- c("file", "overall_valid",  "unique_visit", "datetime", "co_corr_mean", "co_corr_max")



CO_stacked_hourly <- CO_stacked_hourly[CO_stacked_hourly$unique_visit %in% ABP_hourly$unique_visit,]
CO_stacked_30min <- CO_stacked_30min[CO_stacked_30min$unique_visit %in% ABP_hourly$unique_visit,]

saveRDS(ABP_hourly, file = paste0("ABP_hourly_", format(Sys.Date(), format = "%b%d"), ".rds"))

saveRDS(CO_stacked_hourly, file = paste0("CO_stacked_hourly_ABP_", format(Sys.Date(), format = "%b%d"), ".rds"))

saveRDS(CO_stacked_30min, file = paste0("CO_stacked_30min_ABP_", format(Sys.Date(), format = "%b%d"), ".rds"))

# # figure out overlap for UPEM-----
# UPEM_stacked <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/UPEM_stacked_Oct08.rds")
# UPEM_stacked <- arrange(UPEM_stacked, filter, unique_min)
# abpdata <- merge(abpdata, filematch[,c("filter", "unique_visit")], by = "unique_visit", all.x = TRUE)
# abpdata$UPEMfirstdate <- NA
# abpdata$UPEMlastdate <- NA
# for (i in 1:nrow(abpdata)) {
#   data <- UPEM_stacked[UPEM_stacked$filter == abpdata$filter[i],]
#   if(nrow(data) !=0) {
#     abpdata$UPEMfirstdate[i] <- data$unique_min[1]
#     abpdata$UPEMlastdate[i] <- data$unique_min[nrow(data)]
#   }
# }
# 
# 
# 
# abpdata$UPEMfirstdate <- ymd_hms(abpdata$UPEMfirstdate, tz = "GMT")
# abpdata$UPEMlastdate <- ymd_hms(abpdata$UPEMlastdate, tz = "GMT")
# abpdata$UPEMoverlap <- as.numeric(abpdata$UPEMfirstdate - abpdata$date_time_first, units = "days")
# abpdata$UPEMoverlap2 <- as.numeric(abpdata$UPEMlastdate - abpdata$date_time_first, units = "days")
# abpdata$UPEMoverlap_ok <- ifelse(abpdata$UPEMoverlap < 0 & abpdata$UPEMoverlap2 > 0 &!is.na(abpdata$UPEMoverlap), 1, 0)
# sum(abpdata$UPEMoverlap_ok) # 6 out of 20 



### ABP_CO Plots - only the ones that line up (works) ----
  # raw data

ABP_matched <-  readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/ABP_stacked_noerr_matched_Nov27.rds")


pdf(file = paste0("ABP_CO_plots_overlapok_", format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 10)
par(mfrow = c(3,3), mar = c(5,3,4,3))

for (i in 1:length(unique(ABP_matched$file))) {
  data <- ABP_matched[ABP_matched$file == unique(ABP_matched$file)[i],]
  plot(data$date_time, data$SBP, pch = 16, ylim = c(0, 160), main = paste(data$mstudyid[1], data$visit[1]), xlab = paste("Time (", round(difftime(data$date_time[nrow(data)], data$date_time[1], units = "hours"), digits = 1), "hours,", nrow(data), "readings)"), ylab = "mmHg", xaxt = "n", yaxt = "n", type = "n")

  points(data$date_time, data$SBP, pch = 16, col = "black")
  lines(data$date_time, data$SBP)
  points(data$date_time, data$DBP, pch = 16, col = "blue")
  lines(data$date_time, data$DBP, col = "blue")
  
  data3 <- CO_stacked[CO_stacked$unique_visit == unique(data$unique_visit),]

  lines(data3$datetime, (data3$co_corr), col = "darkgreen", lwd = 2)

  # data4 <- CO_stacked_hourly[CO_stacked_hourly$unique_visit == unique(data$unique_visit),]
  # lines(data4$unique_hour, data4$co, col = "lightgreen", lwd = 2)
  
  # y axes
  axis(side = 2, at = seq(from = 0, to = 160, by = 20), cex.axis = 0.8)
  mtext("BP (mmHg)", side=2, line=2, cex=0.4)
  axis(side = 4, at = seq(from = 0, to = 160, by = 20), labels = seq(from = 0, to = 160, by = 20), col = "darkgreen", col.axis = "darkgreen", cex.axis = 0.8)
  mtext("CO (ppm)", side=4, line=2, cex=0.4, col="darkgreen")
  
  # x axis
  hours <- seq(from = ceiling_date(data$date_time[1], unit = "hour"), to = floor_date(data$date_time[nrow(data)], unit = "hour"), by = "hour")
  axis(1, at = hours, labels = format(hours, format = "%H"))
  
  # hypertension lines
 #  abline(h = 140, lty = "dotted") 
 #  abline(h = 90, col = "blue", lty = "dotted") 
  
  # CO percentile lines (hourly)
  percentiles <- as.data.frame(cbind(percent = c(0.95, 0.98, 0.99), cols = c("yellow", "orange", "red")), stringsAsFactors = FALSE)
  percentiles[,1] <- as.numeric(percentiles[,1])
  for (j in 1:3) {
  #   abline(h= quantile(data4$co, probs = percentiles$percent[j]), lty = "dotted", col = percentiles$cols[j])
  }
  # add points for errors (comment out to turn off error markers)
  #   data3 <- ABP_stacked_errors[ABP_stacked_errors$file == unique(ABP_stacked_noerr$file)[i],]
  #   points(x = data3$date_time, y = rep(25,times = length(data3$date_time)), col = "red")
  
 # add shading for night (by diary)
 night_start <-  unique(data$sleeptime)
 night_end <- unique(data$waketime)
 
 rect(xleft = night_start, ybottom = -5, xright = night_end, ytop = 165, density = 20, angle = 45,col = "grey") 
 
  legend("topright", legend = c("SBP", "DBP", "CO"), lwd = 2, col = c("black", "blue", "darkgreen"), cex = 0.68) # without the errors 
  
}
dev.off()


# plot of Hourly averages---------
ABP_hourly <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/ABP_hourly_Nov28.rds")
CO_stacked_hourly <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/CO_stacked_hourly_ABP_Nov28.rds")
CO_stacked_30min <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/CO_stacked_30min_ABP_Nov28.rds")


pdf(file = paste0("ABP_CO_plots_hourly_", format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 10)
par(mfrow = c(3,3), mar = c(5,3,4,3))

for (i in 1:length(unique(ABP_hourly$file))) {
  data <- ABP_hourly[ABP_hourly$file == unique(ABP_hourly$file)[i],]
  names(data)[3] <- "datetime"
  plot(data$datetime, data$SBP_mean, pch = 16, ylim = c(0, 160), main = paste(data$unique_visit[1]), xlab = paste("Time (", round(difftime(data$datetime[nrow(data)], data$datetime[1], units = "hours"), digits = 1), "hours,", nrow(data), "readings)"), ylab = "mmHg", xaxt = "n", yaxt = "n", type = "n")
  
  points(data$datetime, data$SBP_mean, pch = 16, col = "black")
  lines(data$datetime, data$SBP_mean)
  points(data$datetime, data$DBP_mean, pch = 16, col = "blue")
  lines(data$datetime, data$DBP_mean, col = "blue")
  
  data3 <- CO_stacked_hourly[CO_stacked_hourly$unique_visit == unique(data$unique_visit),]
  
  lines(data3$datetime, (data3$co_corr_mean), col = "darkgreen", lwd = 2)
  lines(data3$datetime, data3$co_corr_max, col = "red", lwd = 2)
  # y axes
  axis(side = 2, at = seq(from = 0, to = 160, by = 20), cex.axis = 0.8)
  mtext("BP (mmHg)", side=2, line=2, cex=0.4)
  axis(side = 4, at = seq(from = 0, to = 160, by = 20), labels = seq(from = 0, to = 160, by = 20), col = "darkgreen", col.axis = "darkgreen", cex.axis = 0.8)
  mtext("CO (ppm)", side=4, line=2, cex=0.4, col="darkgreen")
  
  # x axis
  hours <- seq(from = ceiling_date(data$datetime[1], unit = "hour"), to = floor_date(data$datetime[nrow(data)], unit = "hour"), by = "hour")
  axis(1, at = hours, labels = format(hours, format = "%H"))
  
  # add shading for night (by diary)
  night_start <-  unique(data$sleeptime)
  night_end <- unique(data$waketime)
  
  rect(xleft = night_start, ybottom = -5, xright = night_end, ytop = 165, density = 20, angle = 45,col = "grey") 
  
  legend("topright", legend = c("SBP mean", "DBP mean", "CO mean", "CO max"), lwd = 2, col = c("black", "blue", "darkgreen", "red"), cex = 0.68) 
  
}
dev.off()


# add village randomization
village_randomization <- read.dta("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Final_Data/Ghana_randomization.dta")
names(village_randomization)[1] <- "vcode"
random <- village_randomization[,c("vcode", "arm")] # 1 = control, 2 = biolite, 3 = LPG
abpdata <- merge(abpdata, random, by = "vcode")
abpdata$arm2 <- ifelse(abpdata$arm ==1, "Control", ifelse(abpdata$arm ==3, "LPG", "BioLite"))

# How to analyze:
# night & day, matched to CO
ABP_stacked_noerr_matched <- ABP_stacked[ABP_stacked$event_code ==0 & ABP_stacked$file %in% abpdata$file[abpdata$overlap_ok ==1],]

ABP_stacked_noerr_matched <- ABP_stacked_noerr_matched[!ABP_stacked_noerr_matched$unique_visit == "BM1329M_V2",] # this one overlaps for only 2 hours


# Just daytime, matched to CO
ABP_stacked_day_noerr_matched <- ABP_stacked[ABP_stacked$day_measurement ==1 & ABP_stacked$event_code ==0 & ABP_stacked$file %in% abpdata$file[abpdata$overlap_ok ==1],]

ABP_stacked_day_noerr_matched <- ABP_stacked_day_noerr_matched[!ABP_stacked_day_noerr_matched$unique_visit == "BM1329M_V2",] # this one overlaps for only 2 hours



# just the matched ones, with arm info (this group is only LPG and Control)
par(mfrow = c(2,2))
for (i in 1:length(unique(ABP_stacked_noerr_matched$file))) {
  data <- ABP_stacked_noerr_matched[ABP_stacked_noerr_matched$file == unique(ABP_stacked_noerr_matched$file)[i],]
  plot(data$date_time, data$SBP, pch = 16, ylim = c(20, 160), main = paste(data$unique_visit[1], "\n", abpdata$arm2[abpdata$unique_visit == unique(data$unique_visit)]), xlab = paste("Time (", round(difftime(data$date_time[nrow(data)], data$date_time[1], units = "hours"), digits = 1), "hours,", nrow(data), "readings)"), ylab = "mmHg", xaxt = "n", yaxt = "n", type = "n")
  data2 <- ABP_stacked_noerr_matched[ABP_stacked_noerr_matched$file == unique(ABP_stacked_noerr_matched$file)[i],]
  points(data2$date_time, data2$SBP, pch = 16, col = "black")
  lines(data2$date_time, data2$SBP)
  points(data2$date_time, data2$DBP, pch = 16, col = "blue")
  lines(data2$date_time, data2$DBP, col = "blue")
  
  data3 <- CO_stacked[CO_stacked$mstudyid == unique(data$mstudyid),]
  data3 <- data3[data3$visit == unique(data$visit),]
  lines(data3$datetime, (data3$co+20), col = "darkgreen", lwd = 2)
  
  # y axes
  axis(side = 2, at = seq(from = 20, to = 160, by = 20), cex.axis = 0.8)
  mtext("BP (mmHg)", side=2, line=2, cex=0.4)
  axis(side = 4, at = seq(from = 20, to = 160, by = 20), labels = seq(from = 0, to = 140, by = 20), col = "darkgreen", col.axis = "darkgreen", cex.axis = 0.8)
  mtext("CO (ppm)", side=4, line=2, cex=0.4, col="darkgreen")
  
  # x axis
  hours <- seq(from = ceiling_date(data$date_time[1], unit = "hour"), to = floor_date(data$date_time[nrow(data)], unit = "hour"), by = "hour")
  axis(1, at = hours, labels = format(hours, format = "%H"))
  
  # hypertension lines
  abline(h = 140, lty = "dotted") 
  abline(h = 90, col = "blue", lty = "dotted") 
  
  # add points for errors (comment out to turn off error markers)
  #   data3 <- ABP_stacked_errors[ABP_stacked_errors$file == unique(ABP_stacked_noerr$file)[i],]
  #   points(x = data3$date_time, y = rep(25,times = length(data3$date_time)), col = "red")
  
  # add shading for night (by diary)
  night_start <-  unique(data$sleeptime)
  night_end <- unique(data$waketime)
 
  rect(xleft = night_start, ybottom = 15, xright = night_end, ytop = 165, density = 20, angle = 45,col = "grey") 
  
  
  # legend("topright", legend = c("SBP", "DBP", "err"), pch = c(16, 16, 1), col = c("black", "blue", "red"), cex = 0.68)
  legend("topright", legend = c("SBP", "DBP"), pch = c(16, 16, 1), col = c("black", "blue"), cex = 0.68) # without the errors 
  
}

saveRDS(ABP_stacked_noerr_matched, file = paste0("ABP_stacked_noerr_matched_", format(Sys.Date(), format = "%b%d"), ".rds"))

# ### ABP_UPEM Plots - only the ones that line up  ----
# # 6 files -- but 2 of them are very little overlap so really only 4
# 
# ABP_stacked_day_matchedUPEM <- ABP_stacked[ABP_stacked$day_measurement ==1 & ABP_stacked$event_code ==0 & ABP_stacked$file %in% abpdata$file[abpdata$UPEMoverlap_ok ==1],]
# 
# UPEM_stacked$unique_min <- ymd_hms(UPEM_stacked$unique_min, tz = "GMT")
# 
# # just the matched ones, with arm info (this group is only LPG and Control)
# par(mfrow = c(2,2))
# for (i in 1:length(unique(ABP_stacked_day_matchedUPEM$file))) {
#   data <- ABP_stacked_day_matchedUPEM[ABP_stacked_day_matchedUPEM$file == unique(ABP_stacked_day_matchedUPEM$file)[i],]
#   plot(data$date_time, data$SBP, pch = 16, ylim = c(0, 160), main = paste(data$unique_visit[1], "\n", abpdata$arm2[abpdata$unique_visit == unique(data$unique_visit)]), xlab = paste("Time (", round(difftime(data$date_time[nrow(data)], data$date_time[1], units = "hours"), digits = 1), "hours,", nrow(data), "readings)"), ylab = "mmHg", xaxt = "n", yaxt = "n", type = "n")
# 
#   points(data$date_time, data$SBP, pch = 16, col = "black")
#   lines(data$date_time, data$SBP)
#   points(data$date_time, data$DBP, pch = 16, col = "blue")
#   lines(data$date_time, data$DBP, col = "blue")
#   
#   data3 <- UPEM_stacked[UPEM_stacked$unique_visit == unique(data$unique_visit),]
#   lines(data3$unique_min, log(data3$AdjConc +.1)*10, col = "darkgreen", lwd = 2) # on log scale
#   
#   # y axes
#   axis(side = 2, at = seq(from = 0, to = 160, by = 20), cex.axis = 0.8)
#   mtext("BP (mmHg)", side=2, line=2, cex=0.4)
#   # axis(side = 4, at = seq(from = 20, to = 160, by = 20), labels = seq(from = 0, to = 140, by = 20), col = "darkgreen", col.axis = "darkgreen", cex.axis = 0.8)
#   mtext("log UPEM *10", side=4, line=2, cex=0.4, col="darkgreen")
#   
#   # x axis
#   hours <- seq(from = ceiling_date(data$date_time[1], unit = "hour"), to = floor_date(data$date_time[nrow(data)], unit = "hour"), by = "hour")
#   axis(1, at = hours, labels = format(hours, format = "%H"))
#   
#   # hypertension lines
#   abline(h = 140, lty = "dotted") 
#   abline(h = 90, col = "blue", lty = "dotted") 
#   
#   # add shading for night (by diary)
#   night_start <-  unique(data$sleeptime)
#   night_end <- unique(data$waketime)
#   
#   rect(xleft = night_start, ybottom = -5, xright = night_end, ytop = 165, density = 20, angle = 45,col = "grey") 
#   
# 
#   legend("topright", legend = c("SBP", "DBP"), pch = c(16, 16, 1), col = c("black", "blue"), cex = 0.68) # without the errors 
#   
# }

# START HERE with ABP_stacked_noerr_matched and CO_stacked ----
# Build a lagged dataset
ABP_hourly <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/ABP_hourly_Nov28.rds")
CO_stacked_hourly <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/CO_stacked_hourly_ABP_Nov28.rds")
CO_stacked_30min <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/CO_stacked_30min_ABP_Nov28.rds")



# build a lag 0 dataset ----
names(ABP_hourly)[3] <- "datetime"
names(ABP_hourly)[2] <- "ABPfile"
ABP_CO_hourly_lag0 <- merge(ABP_hourly, CO_stacked_hourly, all.x = TRUE )


cor(ABP_CO_hourly_lagged$co_corr_mean, ABP_CO_hourly_lagged$SBP_lag1, use = "pairwise.complete.obs") # 0.12
cor(ABP_CO_hourly_lagged$co_corr_mean, ABP_CO_hourly_lagged$DBP_lag1, use = "pairwise.complete.obs") # 0.16
cor(ABP_CO_hourly_lagged$co_corr_mean, ABP_CO_hourly_lagged$SBP_lag2, use = "pairwise.complete.obs") # 0.11
cor(ABP_CO_hourly_lagged$co_corr_mean, ABP_CO_hourly_lagged$DBP_lag2, use = "pairwise.complete.obs") # 0.12






ABP_CO_hourly_lagged$mstudyid <- substr(ABP_CO_hourly_lagged$unique_visit, 1, 7) # 15 unique

# add day and night measurement, and unique hours
# for lag 1, this should be lagged

ABP_CO_hourly_lagged$night_measurement <- ifelse(ABP_CO_hourly_lagged$datetime > ABP_CO_hourly_lagged$sleeptime & ABP_CO_hourly_lagged$datetime < ABP_CO_hourly_lagged$waketime, 1, 0)
ABP_CO_hourly_lagged$day_measurement <- ifelse(ABP_CO_hourly_lagged$datetime <= ABP_CO_hourly_lagged$sleeptime | ABP_CO_hourly_lagged$datetime >= ABP_CO_hourly_lagged$waketime, 1, 0)

ABP_CO_hourly_lagged$hour <- as.factor(hour(ABP_CO_hourly_lagged$datetime))
  
ABP_CO_hourly_lagged_all <- data.frame()
for (i in 1:length(unique(ABP_CO_hourly_lagged$unique_visit))) {
  data <- ABP_CO_hourly_lagged[ABP_CO_hourly_lagged$unique_visit == unique(ABP_CO_hourly_lagged$unique_visit)[i],]
  data <- arrange(data, datetime)
  data$co_corr_mean_lag1 <- lag(data$co_corr_mean, 1)
  data$co_corr_mean_lag2 <- lag(data$co_corr_mean, 2)
  data$co_corr_mean_lag3 <- lag(data$co_corr_mean, 3)
  ABP_CO_hourly_lagged_all <- rbind(ABP_CO_hourly_lagged_all, data)
}

ABP_CO_hourly_lagged <- ABP_CO_hourly_lagged_all
cor(ABP_CO_hourly_lagged$SBP_mean, ABP_CO_hourly_lagged$co_corr_mean, use = "pairwise.complete.obs") # 0.09
cor(ABP_CO_hourly_lagged$SBP_mean, ABP_CO_hourly_lagged$co_corr_mean_lag1, use = "pairwise.complete.obs") # 0.14
cor(ABP_CO_hourly_lagged$SBP_mean, ABP_CO_hourly_lagged$co_corr_mean_lag2, use = "pairwise.complete.obs") # 0.08
cor(ABP_CO_hourly_lagged$SBP_mean, ABP_CO_hourly_lagged$co_corr_mean_lag3, use = "pairwise.complete.obs") # 0.07

cor(ABP_CO_hourly_lagged$DBP_mean, ABP_CO_hourly_lagged$co_corr_mean, use = "pairwise.complete.obs") # 0.15
cor(ABP_CO_hourly_lagged$DBP_mean, ABP_CO_hourly_lagged$co_corr_mean_lag1, use = "pairwise.complete.obs") # 0.13
cor(ABP_CO_hourly_lagged$DBP_mean, ABP_CO_hourly_lagged$co_corr_mean_lag2, use = "pairwise.complete.obs") # 0.07
cor(ABP_CO_hourly_lagged$DBP_mean, ABP_CO_hourly_lagged$co_corr_mean_lag3, use = "pairwise.complete.obs") # 0.05

#0-1 hour lag best

saveRDS(ABP_CO_hourly_lagged_all, file = paste0("ABP_CO_hourly_lagged_", format(Sys.Date(), format = "%b%d"), ".rds"))

# multilevel models -------
ABP_CO_hourly_lagged <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/ABP_CO_hourly_lagged_Dec10.rds")


# spline of time?
plot(tapply(abp$SBP_mean, abp$hour, FUN = mean, na.rm = TRUE), type = "l")
plot(tapply(abp$DBP_mean, abp$hour, FUN = mean, na.rm = TRUE), type = "l")

library(nlme)
length(unique(ABP_CO_hourly_lagged$mstudyid)) #15
length(unique(ABP_CO_hourly_lagged$unique_visit)) #20

# SBP
# lag 0
fm <- lme(fixed = SBP_mean ~ co_corr_mean + day_measurement, random = ~1|unique_visit, data = ABP_CO_hourly_lagged, na.action = na.omit, method = "ML") # neg, not sig, coef = -0.14

# plus AR1
fm3 <- update(fm, fixed = ~. + SBP_lag1) # coef -0.13

# lag 1
fm <- lme(fixed = SBP_mean ~ co_corr_mean_lag1 + day_measurement, random = ~1|unique_visit, data = ABP_CO_hourly_lagged, na.action = na.omit, method = "ML") # neg, not sig, coef = -0.0007

# plus AR1
fm3 <- update(fm, fixed = ~. + SBP_lag1) # pos, not sig, coef = 0.04 

# plus hour dummies
fm2 <- lme(fixed = SBP_mean ~ co_corr_mean_lag1 + day_measurement + hour, random = ~1|unique_visit, data = ABP_CO_hourly_lagged, na.action = na.omit, method = "ML") # neg, not sig, coef = -0.03
anova(fm,fm2) # 2 is better

# DBP
# lag 0
fm <- lme(fixed = DBP_mean ~ co_corr_mean + day_measurement, random = ~1|unique_visit, data = ABP_CO_hourly_lagged, na.action = na.omit, method = "ML") # neg, not sig ,coef = -0.04

# plus AR1
fm3 <- update(fm, fixed = ~. + DBP_lag1) # neg, not sig, coef = -0.04
print(pacf(residuals(fm3))) #ok

# lag 1
fm <- lme(fixed = DBP_mean ~ co_corr_mean_lag1 + day_measurement, random = ~1|unique_visit, data = ABP_CO_hourly_lagged, na.action = na.omit, method = "ML") # neg, not sig ,coef = -0.05

# plus AR1
fm3 <- update(fm, fixed = ~. + DBP_lag1) # neg, not sig, coef = -0.04
print(pacf(residuals(fm3))) #ok

# # plus hour dummies
# fm2 <- lme(fixed = DBP_mean ~ co_corr_mean_lag1 + day_measurement + hour, random = ~1|unique_visit, data = ABP_CO_hourly_lagged, na.action = na.omit, method = "ML") # neg, not sig, coef = -0.03






# Try using peak exposure in a case-crossover -----------
cc <- ABP_CO_hourly_lagged
cc_peak <- data.frame()
for (i in 1:length(unique(cc$unique_visit))) {
  data <- cc[cc$unique_visit == unique(cc$unique_visit)[i],]
  data$peak_SBP <- ifelse(data$SBP_mean == max(data$SBP_mean, na.rm = TRUE), 1, 0)
  data$peak_DBP <- ifelse(data$DBP_mean == max(data$DBP_mean, na.rm = TRUE), 1, 0)
  data$peak_co <- ifelse(data$co_corr_mean == max(data$co_corr_mean, na.rm = TRUE), 1, 0)
  data$co_75 <- ifelse(data$co_corr_mean > quantile(data$co_corr_mean, probs = 0.75, na.rm = TRUE), 1, 0) # using individual visit's co quantiles
  data$co_90 <- ifelse(data$co_corr_mean > quantile(data$co_corr_mean, probs = 0.9, na.rm = TRUE), 1, 0) # using individual visit's co quantiles
  cc_peak <- rbind(cc_peak, data)
}

table(cc_peak$unique_visit, cc_peak$peak_SBP) # 1 has 3 values
table(cc_peak$unique_visit, cc_peak$peak_DBP) # 1 has 2 values
table(cc_peak$unique_visit, cc_peak$peak_co)
table(cc_peak$unique_visit, cc_peak$co_75) #3-7 values each
table(cc_peak$unique_visit, cc_peak$co_90) #2-3 values each


# lag the CO
cc_peak_lagged <- data.frame()
for (i in 1:length(unique(cc_peak$unique_visit))) {
  data <- cc_peak[cc_peak$unique_visit == unique(cc_peak$unique_visit)[i],]
  data <- arrange(data, datetime)
  data$co_75_lag1 <- lag(data$co_75, 1)
  data$co_75_lag2 <- lag(data$co_75, 2)
  data$co_75_lag3 <- lag(data$co_75, 3)
  data$co_90_lag1 <- lag(data$co_90, 1)
  data$co_90_lag2 <- lag(data$co_90, 2)
  data$co_90_lag3 <- lag(data$co_90, 3)
  cc_peak_lagged <- rbind(cc_peak_lagged, data)
}

for (i in 1:length(unique(cc_peak$unique_visit))) {
  data <- cc_peak[cc_peak$unique_visit == unique(cc_peak$unique_visit)[i],]
  plot(data$datetime, data$peak_SBP, main = unique(data$unique_visit), ylim = c(0, 1))
  points(data$datetime, data$peak_co, col = "red")
  points(data$datetime, data$co_75, col = "blue")
}
  lines(data$datetime, data$co_corr_mean)
  lines(data$datetime, data$co_corr_max, col = "purple")
}

cc_peak <- cc_peak_lagged

# using own 75th percentile CO
# lag 0
fm <- lme(fixed = SBP_mean ~ co_75 + day_measurement, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, method = "ML") # pos, not sig, coef = 1.39
print(pacf(residuals(fm)))

# plus AR1 term
fm3 <- update(fm, fixed = ~. + SBP_lag1) # pos, not sig, coef = 0.37
print(pacf(residuals(fm3))) # nice

# lag 1
fm <- lme(fixed = SBP_mean ~ co_75_lag1 + day_measurement, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, method = "ML") # pos, not sig, coef = 0.40

# lags 0 & 1- not sure this makes any sense because of correlation
fm <- lme(fixed = SBP_mean ~ co_75 + co_75_lag1 + day_measurement, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, method = "ML") # both pos, not sig, coefs = 0.73, 0.29

# # plus hour
# fm2 <- lme(fixed = SBP_mean ~ co_75 + day_measurement + hour, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, method = "ML") # pos, not sig, coef = 0.51
# 
# fm2 <- lme(fixed = SBP_mean ~ co_75_lag1 + day_measurement + hour, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, method = "ML") # neg, not sig, coef = -0.8
# 
# fm2 <- lme(fixed = SBP_mean ~ co_75 + co_75_lag1 + day_measurement + hour, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, method = "ML") # 0 lag pos, 1 lag neg, not sig.


# DBP
# lag 0
fm <- lme(fixed = DBP_mean ~ co_75 + day_measurement, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, method = "ML") # pos,  SIG, coef = 1.78, p-val = 0.027

# plus AR1
fm3 <- update(fm, fixed = ~.+DBP_lag1) # pos, not sig, coef = 1.23, p-val = 0.13

# add in time since waking -----
temp <- data.frame()
for (i in 1:length(unique(cc_peak$unique_visit))) {
  data <- filter(cc_peak, unique_visit == unique(cc_peak$unique_visit)[i])
  data <- arrange(data, datetime)
 data$hrs_since_waking <- as.numeric(ceiling(difftime(data$datetime, data$waketime, units = "hours")))
temp <- rbind(temp, data)
}

cc_peak <- temp

# add morning (2 hrs since waking)
cc_peak$morning <- ifelse(cc_peak$hrs_since_waking >=0 & cc_peak$hrs_since_waking <=2, 1, 0)

par(mfrow = c(2,1))
colors <- rainbow(n=20)
plot(cc_peak$hrs_since_waking, cc_peak$SBP_mean, main = "SBP", xlab = "hours since waking", ylab = "mean SBP (mmHg)", type = "n")
for (i in 1:length(unique(cc_peak$unique_visit))) {
  data <- filter(cc_peak, unique_visit == unique(cc_peak$unique_visit)[i])
  lines(data$hrs_since_waking, data$SBP_mean, col = colors[i])
 abline(v = data$hrs_since_waking[which.min(abs(difftime(data$datetime, data$sleeptime)))], col = colors[i], lty = "dotted")
}

plot(cc_peak$hrs_since_waking, cc_peak$DBP_mean, main = "DBP", xlab = "hours since waking", ylab = "mean DBP (mmHg)", type = "n")
for (i in 1:length(unique(cc_peak$unique_visit))) {
  data <- filter(cc_peak, unique_visit == unique(cc_peak$unique_visit)[i])
  lines(data$hrs_since_waking, data$DBP_mean, col = colors[i])
  abline(v = data$hrs_since_waking[which.min(abs(difftime(data$datetime, data$sleeptime)))], col = colors[i], lty = "dotted")
  
  
}

# add spline for time

# plus natural spline for time ------------
library(splines)
# plot the spline
# natural spline with random intercept by person
cc_peak <- arrange(cc_peak, unique_visit, hour)

# DBP
fit.3 <- lme(fixed = DBP_mean ~ ns(as.numeric(hour), df = 3), random = ~1|unique_visit, data = cc_peak)
fit.4 <- lme(fixed = DBP_mean ~ ns(as.numeric(hour), df = 4), random = ~1|unique_visit, data = cc_peak)
fit.5 <- lme(fixed = DBP_mean ~ ns(as.numeric(hour), df = 5), random = ~1|unique_visit, data = cc_peak)
fit.6 <- lme(fixed = DBP_mean ~ ns(as.numeric(hour), df = 6), random = ~1|unique_visit, data = cc_peak)

par(mfrow = c(2,2))
plot(as.numeric(cc_peak$hour), cc_peak$DBP_mean, main = "DBP, knots = 3", xlab = "hour", ylab = "mean DBP (mmHg)")
lines(as.numeric(cc_peak$hour), predict(fit.3), col="lightblue") # goes back to the beginning each time but that might be ok

plot(as.numeric(cc_peak$hour), cc_peak$DBP_mean, main = "DBP, knots = 4", xlab = "hour", ylab = "mean DBP (mmHg)")
lines(as.numeric(cc_peak$hour), predict(fit.4), col="coral") # goes back to the beginning each time but that might be ok

plot(as.numeric(cc_peak$hour), cc_peak$DBP_mean, main = "DBP, knots = 5", xlab = "hour", ylab = "mean DBP (mmHg)")
lines(as.numeric(cc_peak$hour), predict(fit.5), col="purple") # goes back to the beginning each time but that might be ok

plot(as.numeric(cc_peak$hour), cc_peak$DBP_mean, main = "DBP, knots = 6", xlab = "hour", ylab = "mean DBP (mmHg)")
lines(as.numeric(cc_peak$hour), predict(fit.6), col="red") # goes back to the beginning each time but that might be ok

# SBP
fit.3 <- lme(fixed = SBP_mean ~ ns(as.numeric(hour), df = 3), random = ~1|unique_visit, data = cc_peak)
fit.4 <- lme(fixed = SBP_mean ~ ns(as.numeric(hour), df = 4), random = ~1|unique_visit, data = cc_peak)
fit.5 <- lme(fixed = SBP_mean ~ ns(as.numeric(hour), df = 5), random = ~1|unique_visit, data = cc_peak)
fit.6 <- lme(fixed = SBP_mean ~ ns(as.numeric(hour), df = 6), random = ~1|unique_visit, data = cc_peak)
plot(as.numeric(cc_peak$hour), cc_peak$SBP_mean, main = "SBP, knots = 3", xlab = "hour", ylab = "mean SBP (mmHg)")
lines(as.numeric(cc_peak$hour), predict(fit.3), col="lightblue") # goes back to the beginning each time but that might be ok

plot(as.numeric(cc_peak$hour), cc_peak$SBP_mean, main = "SBP, knots = 4", xlab = "hour", ylab = "mean SBP (mmHg)")
lines(as.numeric(cc_peak$hour), predict(fit.4), col="coral") # goes back to the beginning each time but that might be ok

plot(as.numeric(cc_peak$hour), cc_peak$SBP_mean, main = "SBP, knots = 5", xlab = "hour", ylab = "mean SBP (mmHg)")
lines(as.numeric(cc_peak$hour), predict(fit.5), col="purple") # goes back to the beginning each time but that might be ok

plot(as.numeric(cc_peak$hour), cc_peak$SBP_mean, main = "SBP, knots = 6", xlab = "hour", ylab = "mean SBP (mmHg)")
lines(as.numeric(cc_peak$hour), predict(fit.6), col="red") # goes back to the beginning each time but that might be ok



fm4 <- update(fm, fixed = ~. + ns(as.numeric(hour), 5)) # coef 1.04, not sig
fm5 <- update(fm3, fixed = ~. + ns(as.numeric(hour), 5)) # coef 0.2, not sig



# lag 1
fm <- lme(fixed = DBP_mean ~ co_75_lag1 + day_measurement, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, method = "ML") # neg, not sig, coef = -0.02

# lags 0 and 1
fm <- lme(fixed = DBP_mean ~ co_75 + co_75_lag1 + day_measurement, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, method = "ML") # co_75 pos and sig, lag1 neg and not sig

# plus AR1
fm3 <- update(fm, fixed = ~. + DBP_lag1) # pos, sig, coef = 1.77, p-val = 0.04
print(pacf(residuals(fm3))) # nice

# plus hour
fm2 <- lme(fixed = DBP_mean ~ co_75 + day_measurement + hour, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, method = "ML") # pos, not sig, coef = 0.73
fm2 <- lme(fixed = DBP_mean ~ co_75_lag1 + day_measurement + hour, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, method = "ML") # neg, not sig, coef = -0.4



# using own 90th percentile CO
# lag 0
fm <- lme(fixed = SBP_mean ~ co_90 + day_measurement, random = ~1|unique_visit, data = cc_peak, na.action = na.omit) # pos, not sig, coef = 2.02
print(pacf(residuals(fm)))

# plus AR1 term
fm3 <- update(fm, fixed = ~. + SBP_lag1) # pos, not sig, coef = 0.53
print(pacf(residuals(fm3))) # nice

# plus spline for time
fm4 <- update(fm, fixed = ~.+ ns(as.numeric(hour), 5)) # pos, not sig, coef = 1.07
fm5 <- update(fm3, fixed = ~.+ ns(as.numeric(hour), 5)) # pos, not sig, coef = 0.18


# lag 1
fm <- lme(fixed = SBP_mean ~ co_90_lag1 + day_measurement, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, method = "ML") # pos, not sig, coef = 1.48

# plus AR1 term
fm3 <- update(fm, fixed = ~. + SBP_lag1) # pos, not sig, coef = 0.39
print(pacf(residuals(fm3))) # nice

# lags 0 & 1- not sure this makes any sense because of correlation
fm <- lme(fixed = SBP_mean ~ co_90 + co_90_lag1 + day_measurement, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, method = "ML") # both pos, not sig, coefs = 0.88, 1.52

# plus AR1 term
fm3 <- update(fm, fixed = ~. + SBP_lag1) # both pos, not sig, coef = 0.41, 0.60
print(pacf(residuals(fm3))) # nice

# DBP
# lag 0
fm <- lme(fixed = DBP_mean ~ co_90 + day_measurement, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, method = "ML") # pos, SIG, coef = 2.69
print(pacf(residuals(fm)))

# plus AR1 term
fm3 <- update(fm, fixed = ~. + DBP_lag1) # pos, not sig, coef = 1.47
print(pacf(residuals(fm3))) 

# plus spline for time
fm4 <- update(fm, fixed = ~. + ns(as.numeric(hour), 5)) # pos, not sig, coef = 1.82
fm5 <- update(fm3, fixed = ~. + ns(as.numeric(hour), 5)) # pos, not sig, coef = 1.20

# lag 1
fm <- lme(fixed = DBP_mean ~ co_90_lag1 + day_measurement, random = ~1|unique_visit, data = cc_peak, na.action = na.omit) # pos, not sig, coef = 0.65

# plus AR1 term
fm3 <- update(fm, fixed = ~. + DBP_lag1) # neg, not sig, coef = -0.64
print(pacf(residuals(fm3))) # nice

# lags 0 & 1- not sure this makes any sense because of correlation
fm <- lme(fixed = DBP_mean ~ co_90 + co_90_lag1 + day_measurement, random = ~1|unique_visit, data = cc_peak, na.action = na.omit, method = "ML") # both pos, lag is SIG, coefs = 2.27, 0.03

# plus AR1 term
fm3 <- update(fm, fixed = ~. + DBP_lag1) # not sig, coefs 1.64 & -0.90
print(pacf(residuals(fm3))) 


# add age, BMI, gestwks
BPchange <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/BPchange_Nov18.rds")

cc_peak <- merge(cc_peak, BPchange[, c("mstudyid", "age", "gestwks", "BMI", "arm")], by = "mstudyid", all.x = TRUE)

saveRDS(cc_peak, file = paste0("cc_peak_", format(Sys.Date(), format = "%b%d"), ".rds"))

# RESULTS ------
# Acute CO exposure was not associated with increased BP at lag 1, although a trend was seen for exposures at hi percentiles?
# Conf ints:
## tcrit: t0.025, 362 = 1.96?
## confint(fm2, "co_90")[[1]]  (string of numbers all the same)

# Autoregressive component: acf(residuals(fm2))
# Dealing with autocorrelation from indoor study:
# The initial model incorporated terms for the best-fitting “immediate” lag of outdoor temp/vpmb (eg lag 3 for temp and lag 1 for vpmb), and terms for the 1-day and 2-day lagged outdoor values to account for building inertia. Because the resulting partial autocorrelation function indicated substantial autocorrelation at one hour, we then took the deviance residuals from the model, lagged them by one hour, and introduced the lagged residuals as a predictor into the model to reduce the autocorrelation (script Multilevel_models_disagg.R, from Gasparrini  http://www.ag-myresearch.com/ije2013.html):


cc_peak <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/cc_peak_Dec14.rds")


# another way of dealing with autoregression (lagged residuals)
data <- ABP_CO_hourly_lagged[!is.na(ABP_CO_hourly_lagged$SBP_lag1) & !is.na(ABP_CO_hourly_lagged$co_corr_mean),]
data$resid <-residuals(fm2) 
res_fm2_lags <- data.frame()
for (i in 1:length(unique(data$unique_visit))){
  tempdata <- data[data$unique_visit == unique(data$unique_visit)[i],]
  lags <- as.data.frame(make.n.lag(tempdata$resid, 1))# creates lags 0, 1
 res_fm2_lags <- rbind(res_fm2_lags, lags)
}
identical(as.numeric(data$resid), res_fm2_lags[,1]) # should be true
data$res_fm2_lag1 <- res_fm2_lags[,2]

fm4 <- update(fm2, data = data)
fm5 <- update(fm2, data = data, fixed = ~.+res_fm2_lag1) #-0.11, p-val 0.12 (more similar to results from fm2)
print(acf(residuals(fm5))) # good
anova(fm4, fm5) # doesn't work




# OLD------

# plot of percentiles
pdf(file = "CO Exposure Percentiles.pdf", height = 10, width = 10)
par(mfrow = c(2,2))
for (i in 1:length(unique(ABP_stacked_noerr_matched$file))) {
  data <- ABP_stacked_noerr_matched[ABP_stacked_noerr_matched$file == unique(ABP_stacked_noerr_matched$file)[i],]
  data3 <- CO_stacked_hourly[CO_stacked_hourly$unique_visit == unique(data$unique_visit),]
  data4 <- CO_stacked[CO_stacked$unique_visit == unique(data$unique_visit),]
  data4 <- data4[data4$datetime >=data$date_time[1] & data4$datetime <=data$date_time[nrow(data)],]
  data4$CO_hourly_rollmean <- rollmean(data4$co_corr, 60, fill = NA)
  
  data5 <- abpdata[abpdata$unique_visit == data$unique_visit[i],]
  
  plot(data4$datetime, data4$co_corr, type = "l", lwd = 1, main = paste(data$unique_visit[1], "\n", abpdata$arm2[abpdata$unique_visit == unique(data$unique_visit)]), xlab = paste("Time (", round(difftime(data$date_time[nrow(data)], data$date_time[1], units = "hours"), digits = 1), "hours,", nrow(data), "readings)"), ylab = "ppm", xaxt = "n", col = "grey", ylim = c(0, 160), sub = paste(nrow(data[data$day_measurement == 1,]), "daytime measurements"))
  
  lines(data4$datetime, data4$CO_hourly_rollmean, col = "orange", lwd = 2)

# x axis
hours <- seq(from = data3$datetime[1], to = data3$datetime[nrow(data3)], by = "2 hours")
axis(1, at = hours, labels = format(hours, format = "%H"), cex.axis = 0.9)

# percentiles <- as.data.frame(cbind(percent = c(0.90, 0.95, 0.98), cols = c("orange", "red", "purple")), stringsAsFactors = FALSE)
# percentiles[,1] <- as.numeric(percentiles[,1])
# for (j in 1:3) {
#   abline(h= quantile(data4$CO_hourly_rollmean, probs = percentiles$percent[j], na.rm = TRUE), lty = "dotted", col = percentiles$cols[j], lwd = 2)
# }

# ABP monitoring time
# rect(xleft = data$date_time[1], ybottom = -5, xright = data$date_time[nrow(data)], ytop =165, density = 20, angle = 45,col = "yellow")

probs = c(0.85)
cols = c("orange", "green")
for (m in 1:length(probs)){
for (k in which(data4$CO_hourly_rollmean > quantile(data4$CO_hourly_rollmean, probs = probs[m], na.rm = TRUE))) {
  text(x = data4$datetime[k], y = max(data$SBP)+(m*-2), labels = "*", col = cols[m])
}
}

# using overall rolling mean
cols = c("red", "brown")
for (m in 1:length(probs)){
for (k in which(data4$CO_hourly_rollmean > quantile(CO_stacked$CO_hourly_rollmean, probs = probs[m], na.rm = TRUE))) {
  text(x = data4$datetime[k], y = max(data$SBP)+(m*2), labels = "*", col = cols[m])
}
}

# BP points
points(data$date_time, data$SBP, col = "black")
lines(data$date_time, data$SBP, col = "black")
points(data$date_time, data$DBP, col = "blue")
lines(data$date_time, data$DBP, col = "blue")

# sleep time
rect(xleft = unique(data5$sleeptime), ybottom = -5, xright = unique(data5$waketime), ytop = 165, density = 80, angle = 135,col = "grey")

legend("topright", legend = paste(c("onefile", "alldata"), probs), lwd = 2,col = c("orange", "red"), cex = 0.8)
}
dev.off()


# Still not sure how to determine exposure periods. Using percentile? Using absolute cutoff? Issue with any interval is the before-after may run into sleep or another exposure period. Or just use cross-correlation coefficients? Kaz and Kate Env Health paper: Spearman's rank correlations of peak values. 

# Is the mean during cooking events higher than the daytime mean (excluding these events)?

# using distribution during visit 1? Using 85th percentile of all round 1 CO (17 files), this lines up with the percentile of single files most of the time, but for some files identifies no exposure events (even when cooking form indicates that cooking occurred), for other files identifies long stretches of time (can't make sense of).


hourly_rolling_means <- data.frame()
for (i in 1:length(unique(CO_stacked$unique_visit))) {
  data <- CO_stacked[CO_stacked$unique_visit == unique(CO_stacked$unique_visit)[i],]
  CO_hourly_rollmean <- as.data.frame(rollmean(data$co_corr, 60, fill = NA))
  hourly_rolling_means <- rbind(hourly_rolling_means, CO_hourly_rollmean)
}
CO_stacked$CO_hourly_rollmean <- hourly_rolling_means[,1]

length(unique(CO_stacked$unique_visit[CO_stacked$visit == "V1"])) #17
quantile(CO_stacked$CO_hourly_rollmean[CO_stacked$visit == "V1"], probs = c(0.8, 0.85, 0.9, 0.95, 0.98), na.rm = TRUE)


# add cooking events to abpdata

# add cooking events
DEM <- read.csv("~/Dropbox/Ghana project/BP project/Baseline BP Paper/Final_Data/Update_Nov20/DEM.csv", stringsAsFactors=FALSE)
DEM <- DEM[DEM$mstudyid %in% abpdata$mstudyid,]

uniqueid <- unique(DEM$mstudyid)


j = 2
cookingevents <- data.frame(mstudyid = uniqueid)
for (i in 1:length(uniqueid)) { 
  DEM_id <- DEM[DEM$mstudyid == uniqueid[i] & DEM$vround ==j,] # visit 1 or 2
  if (nrow(DEM_id) > 0) {
  cookingevents[i, "visit"] <- j
  cookingevents[i,"cookingevents"] <- sum(length(which(DEM_id[, c("mipmeal", "evpmeal", "mopmeal")] !=99)))
  cookingevents[i, "no_forms"] <- nrow(DEM_id)
  # if(cookingevents[i, "no_forms"] > 3) print(DEM_id[, c("mstudyid", "datevisit.x", "lastdate", "mipmeal", "evpmeal", "mopmeal")])
  cookingevents[i, "cookfood_days"] <- nrow(DEM_id[DEM_id$cookfood==1 & !is.na(DEM_id$cookfood),])
  cookingevents[i, "nocookfood_days"] <- nrow(DEM_id[DEM_id$cookfood == 2 & !is.na(DEM_id$cookfood),])
  cookingevents[i, "charcoal"] <- sum(length(which(DEM_id[, c("miusecoal", "evusecoal", "mousecoal")] ==1)))
  cookingevents[i, "otherstoveuse"] <- sum(length(which(DEM_id[, c("heatwater", "medicine", "tea", "fosale", "other")] ==1)))
  # self and household smoke exposures
  cookingevents[i, "mosqcoil"] <- sum(length(which(DEM_id[, c("mosqcoil", "amosqcoil")] ==1)))
  cookingevents[i, "kerosene"] <- sum(length(which(DEM_id[, c("keroselan", "akeroselan")] ==1)))
  cookingevents[i, "candle"] <- sum(length(which(DEM_id[, c("candle", "acandle")] ==1)))
  cookingevents[i, "tobacco"] <- sum(length(which(DEM_id[, c("smoke", "asmoke")] ==1)))
  cookingevents[i, "bark"] <- sum(length(which(DEM_id[, c("bark", "abark")] ==1)))
  cookingevents[i, "burntrash"] <- sum(length(which(DEM_id[, "burntrash"] ==1)))
  cookingevents[i, "burngrass"] <- sum(length(which(DEM_id[, "burngras"] ==1)))
  cookingevents[i, "generator"] <- sum(length(which(DEM_id[, "generator"] ==1)))
  cookingevents[i, "mill"] <- sum(length(which(DEM_id[, "mill"] ==1)))
  cookingevents[i, "threestone"] <- sum(length(which(DEM_id[, "stones3"] ==1)))
  cookingevents[i, "coalpot"] <- sum(length(which(DEM_id[, "coalpot"] ==1)))
  
  # self smoke exposures
  cookingevents[i, "roadsale"] <- sum(length(which(DEM_id[, "roadsale"] ==1)))
  cookingevents[i, "makecharcoal"] <- sum(length(which(DEM_id[, "charcoal"] ==1)))
  
  # compliance
  cookingevents[i, "wealltime"] <- sum(length(which(DEM_id[, "wealltime"] ==2))) # 2= did not remove for other reasons than bathing, napping
  }
}

# do this line after j= 1
cookingevents_1 <- cookingevents
# then change j to 2 and run again
cookingevents <- rbind(cookingevents_1, cookingevents)

# can't figure out what's going on in th eones with more than 3 forms so remove
cookingevents <- cookingevents[order(cookingevents$mstudyid),]
row.names(cookingevents) <- NULL
cookingevents$unique_visit <- paste0(cookingevents$mstudyid, "_V", cookingevents$visit)
saveRDS(cookingevents, file = "cookingevents_ABP.rds")

abpdata <- merge(abpdata, cookingevents[,3:ncol(cookingevents)], by = "unique_visit", all.x = TRUE)

saveRDS(abpdata, file = paste0("abpdata_", format(Sys.Date(), format = "%b%d"), ".rds"))


abpdata_valid <- abpdata[abpdata$is_valid_diary ==1,] #49 obs
saveRDS(abpdata_valid, file = paste0("abpdata_valid_", format(Sys.Date(), format = "%b%d"), ".rds"))


# merge hourly average CO with hourly average BP?
ABP_hourly <- data.frame()
for (i in 1:length(unique(ABP_stacked_noerr_matched$unique_visit))) {
  data <- ABP_stacked_noerr_matched[ABP_stacked_noerr_matched$unique_visit == unique(ABP_stacked_noerr_matched$unique_visit)[i],]
  COdata <- CO_stacked[CO_stacked$unique_visit == unique(data$unique_visit) & CO_stacked$datetime >= min(data$date_time) & CO_stacked$datetime <= max(data$date_time),]
ABP_hourly_i <- group_by(data, "unique_hour") %.% summarize (unique_visit = unique_visit[1], sleeptime = sleeptime[1], waketime = waketime[1], hourlymean_SBP = mean(SBP), hourlymean_DBP = mean(DBP))
ABP_hourly_i <- ABP_hourly_i[ABP_hourly_i$unique_hour <= ABP_hourly_i$sleeptime[1] | ABP_hourly_i$unique_hour >= ABP_hourly_i$waketime[1],]
CO_hourly_i <- group_by(COdata, "unique_hour") %.% summarize(hourlymean_COcorr = mean(co_corr))

CO_hourly_i <-  CO_hourly_i[CO_hourly_i$unique_hour <= ABP_hourly_i$sleeptime[1] | CO_hourly_i$unique_hour >= ABP_hourly_i$waketime[1],]
data_hourly_i <- merge(ABP_hourly_i, CO_hourly_i, by = "unique_hour") # this will remove any rows that exist in one dataset but not the other
ABP_hourly <- rbind(ABP_hourly, data_hourly_i)
}

# # restricted to daytime
cor(ABP_hourly$hourlymean_SBP, ABP_hourly$hourlymean_COcorr, method = "spearman") # 0.22
cor(ABP_hourly$hourlymean_DBP, ABP_hourly$hourlymean_COcorr, method = "spearman") # 0.25

cor.test(ABP_hourly$hourlymean_SBP, ABP_hourly$hourlymean_COcorr, method = "spearman") 
cor.test(ABP_hourly$hourlymean_DBP, ABP_hourly$hourlymean_COcorr, method = "spearman") # both have sig p-values but warning "cannot compute exact p-value with ties"

# lagged?



