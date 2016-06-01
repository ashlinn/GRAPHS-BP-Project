# Function to import data files from 90207 ABP monitor
# created June 20, 2014
# to do:
#   1.  create count of errors
#   2.  summary statistics (mean daytime, mean nighttime, drop, what else?)

require(lubridate)
require(plyr)
require(dplyr)

# IMPORT DATA ------

files <- list.files("~/Dropbox/APB_Pilot_Data_encrypted/ABP_encrypted",pattern = ".abp", recursive=F, full.names=T) 
length(files) #29/ Nov 5: 46/ Nov 13: 54/ Feb 6: 55
  
spacelabs.import <- function(file) {
  ID <- read.csv(file, fileEncoding="UTF-16LE", header=F, nrows=50, sep=" ", stringsAsFactors=F)
  visit <- ID[1,1]      # assumes that visit number is entered for first name
  studyid <- ID[1,2]    # assumes that study id is entered for last name
  session <- ID[2,1]    # assumes that session id is entered for patient id 
  header.end <- max(which(ID$V1 == "Unknown")) + 1
  nobs <- as.numeric(ID[header.end,1]) # this is not always in the same place, sometimes it's row 8 and sometimes row 9
  BP <- read.csv(file, fileEncoding="UTF-16LE", header=F, skip=51, nrows=nobs) # load BP data 
  BP <- BP[,1:7]
    names(BP) <- c("hour", "minute", "SBP", "MAP", "DBP", "HR", "event_code")
  timestart <-51+nobs
  DS <- read.csv(file, fileEncoding="UTF-16LE", header=F, skip=timestart, nrows=nobs) # load date stamps
    names(DS) <- c( "month", "day","year", "code")
    BP$mstudyid <- studyid
    BP$visit <- visit
    BP$session <- session
    BP$nobs <- nobs
  BP <- cbind(BP, DS)
  BP$date_time <- paste(BP$year, BP$month, BP$day, BP$hour, BP$minute, sep="-")
    BP$date_time <- ymd_hm(BP$date_time)
    BP$is_night_asia <- 0 
  BP$is_night_europe <- 0
    BP$is_night_asia[BP$hour >= 20 | BP$hour < 6] <- 1 # after 8 pm & before 6am
    BP$is_night_europe[BP$hour >=22 | BP$hour < 8] <- 1
    BP$night_ten_four <- 0
  BP$night_ten_four[BP$hour >=22] <- 1
  BP$night_ten_four[BP$hour < 4] <- 1
  BP$day_eight_six <- 0
  BP$day_eight_six[BP$hour >=8 & BP$hour <18] <- 1
  BP$night_twelve_six <- 0
  BP$night_twelve_six[BP$hour < 6] <- 1
  BP$day_ten_eight <- 0
  BP$day_ten_eight[BP$hour >=10 & BP$hour <20] <- 1
  BP$file <- basename(file)
  drop <- names(BP) %in% c("day", "month", "year", "hour", "minute", "NA")
  BP <- BP[!drop] 
  BP
}



ABP_stacked <- ldply(files, spacelabs.import, .progress = "text")  

unique(ABP_stacked$event_code) # check nothing other than "", NA, EE

ABP_stacked$event_code <- ifelse((ABP_stacked$event_code %in% c("", NA)), 0, 1) # change error coding to 0 for no error, 1 for error. (Most files will have "" if no error and "EE" if error, but if there was no error in the entire session all event codes will be NA)

# change the variables for the file with info entered backward
ABP_stacked$mstudyid[ABP_stacked$file == "BM1329M,V1ABP020 2014-08-20 07.25.00.abp"] <- "BM1329M"
ABP_stacked$visit[ABP_stacked$file == "BM1329M,V1ABP020 2014-08-20 07.25.00.abp"] <- "V1"

# ABP_stacked$mstudyid[ABP_stacked$mstudyid == "BM1112M_2"] <- "BM1121M" # TESTING if 1112 is a typo
ABP_stacked$mstudyid[ABP_stacked$mstudyid == "BM1415"] <- "BM1415M"

saveRDS(ABP_stacked, file = paste0("ABP_stacked", format(Sys.Date(), format = "%b%d"), ".rds"))

# GENERATE SUMMARY STATISTICS -----

# Error percentages
ABP_errors <- ABP_stacked %.% group_by(file) %.% dplyr:: summarise(sum(event_code), nobs[1], length(SBP)) # compare last 2 columns to see if there are any discrepancies
names(ABP_errors) <- c("file", "errors", "nobs", "nrows")
ABP_errors$percent_errors <- ABP_errors$errors/ABP_errors$nobs * 100


# remove Error rows from the data
ABP_stacked_noerr <- ABP_stacked[ABP_stacked$event_code ==0,]
row.names(ABP_stacked_noerr) <- NULL

# create separate errors dataset
ABP_stacked_errors <- ABP_stacked[ABP_stacked$event_code ==1,]
row.names(ABP_stacked_errors) <- NULL

# Summary stats for the good data
ABP_summary <- ABP_stacked_noerr %.% group_by(file) %.% dplyr::summarise(mstudyid[1], visit[1], date_time[1], session[1], nobs[1], length(SBP), min(SBP), mean(SBP), max(SBP), sd(SBP), min(DBP), mean(DBP), max(DBP), sd(DBP), mean(MAP), mean(HR), sum(is_night_asia), sum(is_night_europe), sum(night_ten_four), sum(day_eight_six), sum(night_twelve_six), sum(day_ten_eight))

names(ABP_summary) <- c("file", "mstudyid", "visit", "date_time", "session", "total_obs", "good_obs", "min_SBP", "mean_SBP", "max_SBP", "sd_SBP", "min_DBP", "mean_DBP", "max_DBP", "sd_DBP", "mean_MAP", "mean_HR", "night_obs_asia", "night_obs_europe", "night_ten_four", "day_eight_six", "night_twelve_six", "day_ten_eight")

# add day_obs  
ABP_summary$day_obs_asia <- ABP_summary$good_obs - ABP_summary$night_obs_asia
ABP_summary$day_obs_europe <- ABP_summary$good_obs - ABP_summary$night_obs_europe

# add validity (according to IDACO criteria for Asia/Europe)
ABP_summary$is_valid_asia <- ifelse(ABP_summary$day_obs_asia >=10 & ABP_summary$night_obs_asia >=5, 1, 0) # 3 out of 29 invalid
ABP_summary$is_valid_europe <- ifelse(ABP_summary$day_obs_europe >=10 & ABP_summary$night_obs_europe >=5, 1, 0) # 3 out of 29 invalid

# validity using truncated Asia ranges (day = 8a-6p; night = 10p-4a)
ABP_summary$is_valid_asia_trunc <- ifelse(ABP_summary$day_eight_six >=10 & ABP_summary$night_ten_four >=5, 1, 0) # 5 out of 29 invalid

# validity using truncated Europe ranges (day = 10a-8p; night = 12a-6a)
ABP_summary$is_valid_europe_trunc <- ifelse(ABP_summary$day_ten_eight >=10 & ABP_summary$night_twelve_six >=5, 1, 0) # 5 out of 29 invalid

#  number obs using truncated Asia ranges
ABP_summary$good_obs_asia_trunc <- ABP_summary$day_eight_six + ABP_summary$night_ten_four


saveRDS(ABP_summary, file = paste0("ABP_summary_", format(Sys.Date(), format = "%b%d"), ".rds"))




# SUMMARY STATS----
ABP_stats_overall <- ABP_summary %.% dplyr::summarise(length(file), mean(total_obs), mean(good_obs), sum(is_valid_asia), sum(is_valid_europe), sum(is_valid_asia_trunc), sum(is_valid_europe_trunc))
ABP_stats_byvisit <- ABP_summary %.% group_by(visit) %.% dplyr::summarise(length(file), mean(total_obs), mean(good_obs), sum(is_valid_asia), sum(is_valid_europe), sum(is_valid_asia_trunc), sum(is_valid_europe_trunc))

# SUMMARY HISTOGRAMS ----
# observations and errors
pdf(file = paste0("ABP_summary_histograms_", format(Sys.Date(), format = "%b%d"), ".pdf"))
par(mfrow = c(2,2))
hist(ABP_summary$total_obs, main = paste(nrow(ABP_summary), "ABP Sessions"), xlab = "Number of Total Observations", col = "grey")
hist(ABP_summary$good_obs, main = paste(nrow(ABP_summary), "ABP Sessions"), xlab = "Number of Good Observations", col = "grey")
hist(ABP_errors$errors, main = paste(nrow(ABP_errors), "ABP Sessions"), xlab = "Total Errors per Session", col = "grey")
hist(ABP_errors$percent_errors, main = paste(nrow(ABP_errors), "ABP Sessions"), xlab = "Error Percentage per Session", col = "grey")


# summary BP measures
# histograms
par(mfrow = c(3,2))
for (i in c(8,9,10, 12,13,14)) {
  hist(ABP_summary[,i], main = paste(nrow(ABP_errors), "ABP Sessions"),xlab = names(ABP_summary[i]), col = "grey")
}
dev.off()

# stats
ABP_stats_BP_asia <- filter(ABP_summary, is_valid_asia_trunc == 1) %.% dplyr::summarise(length(file), mean(total_obs), mean(good_obs), mean(min_SBP), mean(mean_SBP), mean(max_SBP), mean(min_DBP), mean(mean_DBP), mean(max_DBP))

ABP_stats_BP_europe <- filter(ABP_summary, is_valid_europe_trunc == 1) %.% dplyr::summarise(length(file), mean(total_obs), mean(good_obs), mean(min_SBP), mean(mean_SBP), mean(max_SBP), mean(min_DBP), mean(mean_DBP), mean(max_DBP))

# subsetting the valid files
ABP_daytime_asia <- ABP_stacked_noerr[ABP_stacked_noerr$day_eight_six ==1 & ABP_stacked_noerr$file %in% ABP_summary$file[ABP_summary$is_valid_asia_trunc ==1],]
ABP_daytime_europe <- ABP_stacked_noerr[ABP_stacked_noerr$day_ten_eight ==1 & ABP_stacked_noerr$file %in% ABP_summary$file[ABP_summary$is_valid_europe_trunc ==1],]

ABP_nighttime_asia <- ABP_stacked_noerr[ABP_stacked_noerr$night_ten_four ==1 & ABP_stacked_noerr$file %in% ABP_summary$file[ABP_summary$is_valid_asia_trunc ==1],]
ABP_nighttime_europe <- ABP_stacked_noerr[ABP_stacked_noerr$night_twelve_six ==1 & ABP_stacked_noerr$file %in% ABP_summary$file[ABP_summary$is_valid_europe_trunc ==1],]

ABP_stats_BP_asia_daytime <-  ABP_daytime_asia %.% group_by(file) %.% dplyr::summarise(nobs[1], length(SBP), mean(SBP), mean(DBP)) # gives the mean per file
ABP_stats_BP_asia_nighttime <- ABP_nighttime_asia %.% group_by(file) %.% dplyr::summarise(nobs[1], length(SBP), mean(SBP), mean(DBP))

ABP_stats_BP_europe_daytime <-  ABP_daytime_europe %.% group_by(file) %.% dplyr::summarise(nobs[1], length(SBP), mean(SBP), mean(DBP)) # gives the mean per file
ABP_stats_BP_europe_nighttime <- ABP_nighttime_europe %.% group_by(file) %.% dplyr::summarise(nobs[1], length(SBP), mean(SBP), mean(DBP))

sapply(ABP_stats_BP_asia_daytime, mean)
sapply(ABP_stats_BP_europe_daytime, mean)
sapply(ABP_stats_BP_asia_nighttime, mean)
sapply(ABP_stats_BP_europe_nighttime, mean)

## Time series plots -----

pdf(file= paste0("ABP_plots_", format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 10)
par(mfrow = c(3,3))
for (i in 1:length(unique(ABP_stacked$file))) {
  data <- ABP_stacked[ABP_stacked$file == unique(ABP_stacked$file)[i],]
  plot(data$date_time, data$SBP, pch = 16, ylim = c(20, 160), main = paste(data$mstudyid[1], data$visit[1]), xlab = paste("Time (", round(difftime(data$date_time[nrow(data)], data$date_time[1], units = "hours"), digits = 1), "hours,", nrow(data), "readings)"), ylab = "mmHg", xaxt = "n", type = "n")
  data2 <- ABP_stacked_noerr[ABP_stacked_noerr$file == unique(ABP_stacked_noerr$file)[i],]
  points(data2$date_time, data2$SBP, pch = 16, col = "black")
  lines(data2$date_time, data2$SBP)
  points(data2$date_time, data2$DBP, pch = 16, col = "blue")
  lines(data2$date_time, data2$DBP, col = "blue")
  
  # x axis
  hours <- seq(from = ceiling_date(data$date_time[1], unit = "hour"), to = floor_date(data$date_time[nrow(data)], unit = "hour"), by = "hour")
  axis(1, at = hours, labels = format(hours, format = "%H"))
  
  # hypertension lines
  abline(h = 140, lty = "dotted") 
  abline(h = 90, col = "blue", lty = "dotted") 
  
  # add points for errors (comment out to turn off error markers)
  data3 <- ABP_stacked_errors[ABP_stacked_errors$file == unique(ABP_stacked_noerr$file)[i],]
  points(x = data3$date_time, y = rep(25,times = length(data3$date_time)), col = "red")
  
  # add shading for night (21h- 5h)
  for (i in c(5:0, 24:21)) {
    night_start <-  
      ifelse(i %in% hour(hours), i, night_start)
  }
  for (i in c(21:23, 0:5)) {
    night_end <-  
      ifelse(i %in% hour(hours), i, night_end)
  }
  
  rect(xleft = hours[hour(hours) == night_start], ybottom = 15, xright = hours[hour(hours) == night_end], ytop = 165, density = 20, angle = 45,col = "grey") 
  legend("topright", legend = c("SBP", "DBP", "err"), pch = c(16, 16, 1), col = c("black", "blue", "red"), cex = 0.68)
# legend("topright", legend = c("SBP", "DBP"), pch = c(16, 16, 1), col = c("black", "blue"), cex = 0.68) # without the errors 


}
dev.off()


#######################
# Superimpose CO on ABP ------
#######################



# GETTING FILE ALIGNMENT -----
StudyIDs <- unique(ABP_stacked$mstudyid) #29

CO_parameters <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/CO_parameters_7152sessions_Feb03.rds")

alignment <- ABP_stacked %.% group_by(mstudyid, visit) %.% dplyr::summarise(abpfirstdate = min(date_time), abplastdate = max(date_time))

for (i in 1:nrow(alignment)) {
  co <- CO_parameters[CO_parameters$mstudyid == alignment$mstudyid[i],]
  alignment$COfirstdate[i] <- co$firstdate[which.min(abs(alignment$abpfirstdate[i] - co$firstdate))]
  alignment$COlastdate[i] <- co$lastdate[which.min(abs(alignment$abpfirstdate[i] - co$firstdate))]
  alignment$COfile[i] <- co$file[which.min(abs(alignment$abpfirstdate[i] - co$firstdate))]
}

for (i in 3:6) {
alignment[,i] <- as.POSIXct(alignment[,i], origin = "1970-1-1", tz = "UTC")
}

alignment$overlap_full <- ifelse(as.numeric(alignment$abpfirstdate - alignment$COfirstdate, units = "hours") > 0 & as.numeric(alignment$COlastdate - alignment$abplastdate, units = "hours") >0, 1, 0)
alignment$overlap_some <- ifelse(abs(as.numeric(alignment$abpfirstdate - alignment$COfirstdate, units = "hours")) < 24 | abs(as.numeric(alignment$COlastdate - alignment$abplastdate, units = "hours")) <24, 1, 0)
alignment$overlap_none <- ifelse(alignment$overlap_some == 0, 1, 0)

# some CO files are duplicated
dups <- alignment$COfile[which(duplicated(alignment$COfile))]
dupCOfiles <- alignment[alignment$COfile %in% dups,]
dupCOfiles[, c(1:3, 7:ncol(dupCOfiles))]

# bad rows: 11, 15, 34, 40, 42, 44, 50, 52
alignment[row.names(alignment) %in% as.character(c(11, 15, 34, 40, 42, 44, 50, 52)), c(1:3,7:ncol(alignment))]
alignment$COfile[row.names(alignment) %in% as.character(c(11, 15, 34, 40, 42, 44, 50, 52))] <- NA

write.csv(alignment, file = "ABP_CO_alignment.csv", row.names = FALSE)

Lascar_data <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/Lascar_data_cf_Jan29.rds") # 7152

Lascar_data <- Lascar_data[Lascar_data$file2 %in% alignment$COfile,]


# LASCAR FILE IDENTIFICATION ------

# 
# 
# 
# #create vectors of file names  -- look for both "CU_CO" and "CU_C0"
# files<-list.files("~/Dropbox/Ghana_exposure_data_SHARED (1)/Main_study_exposure_assessment",recursive=T,pattern="^(CU_CO|CU_C0|CO_USB|COL_USB|CU-CO|CU-C0|CO-USB|COL-USB)", full.names=T) 
# length(files) 
# 
# s01 <- files[grep("s_01", files)] # separates out session 1
# s02 <- files[grep("s_02", files)] # session 2
# 
# files1 <- files[files %in% s01] 
# files2 <- files[files %in% s02]
# files <- append(files1, files2)
# length(files) #Dec 7 3499/ Dec 8 3564
# 
# 
# # subset the files to coincide with unique StudyIDs from ABP. 
# StudyIDs <- unique(ABP_stacked$mstudyid)
# Lascar_data <- as.data.frame(files[substr(gsub("^.*BM", "BM", files), 1,7) %in% StudyIDs]) # 55 
# 
# 
# Lascar_data[,2] <- substr(gsub("^.*BM", "BM", Lascar_data[,1]), 1, 7)
# 
# Lascar_data <- Lascar_data[,c(2,1)]
# colnames(Lascar_data) <- c("mstudyid", "Lascarfile")
# 
# # get rid of files that are actual duplicates (where 2 monitors were deployed simultaneously)
# dupfiles_Lascar <- Lascar_data[grep("dup", Lascar_data[,2]),2] #4
# Lascar_data <- Lascar_data[!Lascar_data$Lascarfile %in% dupfiles_Lascar,] 
# nrow(Lascar_data) #52
# 
# length(unique(ABP_stacked$mstudyid)) #29
# length(unique(Lascar_data$mstudyid)) #29: compare to above number to see if any duplicates exist 
# 
# 
# # ## sort out the duplicates before proceeding
# 
# 
# 
# # check for child files 
# child <- Lascar_data[grep("BM....C", Lascar_data[,2]),]  # no child files
# 
# # see how many StudyIDs have no Lascar_data
# unmatched_IDs <- StudyIDs[!StudyIDs %in% Lascar_data$mstudyid] #0
# length(unmatched_IDs) #26/ Jul 18: 33 (some are new files)/ Jul 30: 23 / Sep 2: 20 / Nov 3: still 20
# 
# files <- as.character(Lascar_data[,2])

####### LASCAR DATA PROCESSING -----

# # provides the minutewise average CO value over the observations in each file
# lascar.import <- function(x){
#   dt <- read.csv(x, stringsAsFactors=F, header=T)[,c(2,3)]
#   names(dt) <- c('datetime','co')
#   dt$datetime <- dmy_hms(dt$datetime, tz="GMT")
#   dt$rd.datetime <- as.character(round(dt$datetime, 'min'))
#   dt<-dt %.% group_by(rd.datetime) %.% dplyr::summarise(mean(co)) #replaced the plyr approach that ajay provided w/ ddply
#   names(dt) <- c('datetime','co')
#   dt$datetime <- ymd_hms(dt$datetime)
#   dt
# }
# 
# names(files) <- files #for reasons I don't understand, this forces ldply to include a column with the file name, for parsing below
# 
# ptm <- proc.time()
# 
# CO_stacked <- ldply(files, lascar.import, .progress = "text" ) #this creates a single dataframe 
# 
# proc.time() - ptm
# 
# #create study ID variable
# hhid_pattern<-"BM....."
# hhid_match<-regexpr(hhid_pattern, CO_stacked$.id)
# CO_stacked$mstudyid<-regmatches(CO_stacked$.id, hhid_match)
# 
# #create village id variable - inefficient 2 step process, but works
# vill_pattern<-"vil_.."
# vill_match<-regexpr(vill_pattern, CO_stacked$.id,ignore.case=T)
# CO_stacked$vill<-regmatches(CO_stacked$.id, vill_match)
# CO_stacked$village_code<-substr(CO_stacked$vill,5,6)
# 
# # #create session id variable
# session_pattern<-"s_.."
# session_match<-regexpr(session_pattern, CO_stacked$.id)
# CO_stacked$session<-regmatches(CO_stacked$.id, session_match)
# CO_stacked$session<-substr(CO_stacked$session,3,4)
# 
# #convert strings to factors
# CO_stacked$mstudyid<-factor(CO_stacked$mstudyid)
# CO_stacked$village_code<-factor(CO_stacked$village_code)
# CO_stacked$session<-factor(CO_stacked$session)
# 
# 
# CO_stacked <- CO_stacked[!is.na(CO_stacked$co),] # removing NAs
# 
# CO_stacked$datetime <- ymd_hms(CO_stacked$datetime, tz = "GMT")
# 
# CO_stacked$visit <- paste0("V", substr(CO_stacked$session, 2, 2))

# 
# ##################################
# # set a directory for the saved data
# directory <- "~/Dropbox/Ghana_exposure_data_SHARED (1)/CO_files_processed/29Jan2015/CO_stacked files_all/"
# ##################################
# 
# saveRDS(CO_alldata, file = paste0(directory, "CO_stacked_alldata_", format(Sys.Date(), format = "%b%d"), ".rds"))



# Lascar.import function -------
lascar.import <- function(file,cf, cf_conf) { 
  dt <- read.csv(file, stringsAsFactors=F, header=T)[,1:5]
  dt$lascar <- names(dt)[1]
  dt$lascar <- gsub("C0", "CO", dt$lascar)
  dt$lascar <- gsub("-", "_", dt$lascar)
  dt$SN<- dt$Serial.Number[1]
  dt$datetime <- dmy_hms(dt$Time, tz="GMT")
  dt$rd.datetime <- as.character(round(dt$datetime, 'min'))
  dt<-dt %.% group_by(rd.datetime) %.% dplyr::summarise(mean(CO.ppm.), lascar[1], SN[1])
  names(dt) <- c('datetime','co', 'lascar', 'SN')
  dt$datetime <- ymd_hms(dt$datetime)
  dt$cf <- cf
  dt$cf_conf <- ifelse(!is.na(dt$cf[1]), cf_conf, "lo") # if CF is NA, set as low
  cfgood <- !is.na(dt$cf[1]) & dt$cf[1] >=0.2
  if(cfgood == TRUE) dt$co_corr <- dt$co/dt$cf
  if(cfgood == FALSE) dt$co_corr <- dt$co/0.85 # if CF is NA or < 0.2,  adjust with the mean
  dt
}



##################################
# set a directory for the saved data
directory <- "~/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/ABP CO Data/"
##################################


ptm <- proc.time()

  files_bySN <- Lascar_data[, c("file", "cf", "cf_conf")]
  
  CO_stacked <- mdply(files_bySN, lascar.import, .progress = "text") # mdply so can supply multiple arguments to lascar.import. Variable names of files_bySN must match those in lascar.import (file, cf, cf_conf)
  
  CO_stacked <- CO_stacked[!is.na(CO_stacked$co),] # removing NAs
  
  
  # grab mother and child id info
  id_pattern <- "BM....."
  CO_stacked$mstudyid <- regmatches(CO_stacked$file, regexpr(id_pattern, CO_stacked$file))
  
  child_pattern <- "BM....C"
  CO_stacked$cstudyid <- regexpr(child_pattern, CO_stacked$file)
  CO_stacked$cstudyid <- ifelse(CO_stacked$cstudyid == -1, NA, substr(x = CO_stacked$file, start = CO_stacked$cstudyid, stop = CO_stacked$cstudyid + 6))
  
  # grab session info
  # grab session info: do in multipe steps to deal with bad value propagation after NA
  session_pattern <- "(s_[0123456789]{1,2}|s[0123456789]{1,2})"
  CO_stacked$session <- regmatches(CO_stacked$file, regexpr(session_pattern, CO_stacked$file, ignore.case =TRUE))
  CO_stacked$session <- tolower(CO_stacked$session)
  
  # order from most recent to oldest
  CO_stacked <- CO_stacked[order(CO_stacked$datetime),]
  
  # simplify Lascar names
  CO_stacked$lascar <- gsub("\\.", "_", CO_stacked$lascar)
  
  # add visit
  CO_stacked$visit <- paste0("V", substr(CO_stacked$session, 4, 4))

  # save
  saveRDS(CO_stacked, file = paste0(directory, "CO_stacked_", CO_stacked$lascar[1], "_", CO_stacked$SN[1],".rds"))
  
  
### ABP_CO Plots (works, but not all CO data is there and/or lines up) ----

pdf(file = paste0("ABP_CO_plots_", format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 10)
par(mfrow = c(3,3), mar = c(5,3,4,3))
for (i in 1:length(unique(ABP_stacked$file))) {
  data <- ABP_stacked[ABP_stacked$file == unique(ABP_stacked$file)[i],]
  plot(data$date_time, data$SBP, pch = 16, ylim = c(20, 160), main = paste(data$mstudyid[1], data$visit[1]), xlab = paste("Time (", round(difftime(data$date_time[nrow(data)], data$date_time[1], units = "hours"), digits = 1), "hours,", nrow(data), "readings)"), ylab = "mmHg", xaxt = "n", yaxt = "n", type = "n")
  data2 <- ABP_stacked_noerr[ABP_stacked_noerr$file == unique(ABP_stacked_noerr$file)[i],]
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
  data3 <- ABP_stacked_errors[ABP_stacked_errors$file == unique(ABP_stacked_noerr$file)[i],]
  points(x = data3$date_time, y = rep(25,times = length(data3$date_time)), col = "red")
  
  # add shading for night (21h- 5h)
  for (i in c(5:0, 24:21)) {
    night_start <-  
      ifelse(i %in% hour(hours), i, night_start)
  }
  for (i in c(21:23, 0:5)) {
    night_end <-  
      ifelse(i %in% hour(hours), i, night_end)
  }
  
  rect(xleft = hours[hour(hours) == night_start], ybottom = 15, xright = hours[hour(hours) == night_end], ytop = 165, density = 20, angle = 45,col = "grey") 
  legend("topright", legend = c("SBP", "DBP", "err"), pch = c(16, 16, 1), col = c("black", "blue", "red"), cex = 0.68)
  # legend("topright", legend = c("SBP", "DBP"), pch = c(16, 16, 1), col = c("black", "blue"), cex = 0.68) # without the errors 
  
}
dev.off()

## Checking whether the ABP and CO files line up --------

ABP_CO <- data.frame(mstudyid = unique(ABP_stacked$mstudyid), ABP_V1_start = NA, ABP_V1_end = NA, CO_V1_start = NA, CO_V1_end = NA, ABP_V2_start = NA, ABP_V2_end = NA, CO_V2_start = NA, CO_V2_end = NA, V1_timeproblem = NA, V2_timeproblem = NA)

for (i in 1:length(StudyIDs)) {
  
data <- ABP_stacked[ABP_stacked$mstudyid == StudyIDs[i] & ABP_stacked$visit == "V1",]
data3 <- CO_stacked[CO_stacked$mstudyid == StudyIDs[i],]
data3 <- data3[data3$visit == "V1",]
ABP_CO[i, 2] <- data$date_time[1]
ABP_CO[i, 3] <- data$date_time[nrow(data)]
ABP_CO[i, 4] <- ifelse (!is.na(data3$datetime[1]), data3$datetime[1], NA)
ABP_CO[i, 5] <- ifelse (!is.na(data3$datetime[1]), data3$datetime[nrow(data3)], NA)

data4 <- ABP_stacked[ABP_stacked$mstudyid == StudyIDs[i] & ABP_stacked$visit == "V2",]
data5 <-CO_stacked[CO_stacked$mstudyid == StudyIDs[i] & CO_stacked$visit == "V2",]
ABP_CO[i, 6] <- ifelse(!is.na(data4$date_time[1]), data4$date_time[1], NA)
ABP_CO[i, 7] <- ifelse(!is.na(data4$date_time[1]), data4$date_time[nrow(data4)], NA)
ABP_CO[i, 8] <- ifelse (!is.na(data5$datetime[1]), data5$datetime[1], NA)
ABP_CO[i, 9] <- ifelse (!is.na(data5$datetime[1]), data5$datetime[nrow(data5)], NA)
ABP_CO[i,10] <- ifelse(round_date(median(data$date_time), unit = "hour") %in% round_date(data3$datetime, unit = "hour"), "ok", "problem")
ABP_CO[i,11] <- ifelse(round_date(median(data4$date_time), unit = "hour") %in% round_date(data5$datetime, unit = "hour"), "ok", "problem")
}

for (i in 2:9) {
ABP_CO[,i] <- as.POSIXct(ABP_CO[,i], origin = "1970-1-1", tz = "GMT")
}

write.csv(ABP_CO, file = "ABP_CO.csv", row.names = FALSE)
