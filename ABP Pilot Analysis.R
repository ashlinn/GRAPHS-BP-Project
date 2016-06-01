## ABP Pilot

library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(pastecs)


### FINAL DATA
ABP_stacked <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/ABP_stacked_Oct13.rds")

ABP_stacked_noerr_matched <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/ABP_stacked_noerr_matched_Nov27.rds")

CO_stacked <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/CO_stacked_ABP_Nov28.rds")

abpdata <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/abpdata_Nov17.rds") # summary data

abpdata_valid <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/abpdata_valid_Nov17.rds")

### Reading in data from Questionnaires-----
abp <- read.csv("/Users/ashlinn/Dropbox/Ghana project/BP project/ABP Project/Final Data/Update March 29/abp.csv", stringsAsFactors = FALSE) # datevisit is not correct nor is duration
homebp <- read.csv("/Users/ashlinn/Dropbox/Ghana project/BP project/ABP Project/Final Data/Update March 29/homebptest.csv", stringsAsFactors = FALSE)
stress <- read.csv("/Users/ashlinn/Dropbox/Ghana project/BP project/ABP Project/Final Data/Update March 29/stress.csv", stringsAsFactors = FALSE)
lascar <- read.csv("/Users/ashlinn/Dropbox/Ghana project/BP project/ABP Project/Final Data/Update March 29/lascar.csv", stringsAsFactors = FALSE) # this may not be all the variables and may not have all the participants? Check against data
micropem <- read.csv("/Users/ashlinn/Dropbox/Ghana project/BP project/ABP Project/Final Data/Update March 29/micropem.csv", stringsAsFactors = FALSE) # only 29 rows? Check against data 
dem <- read.csv("/Users/ashlinn/Dropbox/Ghana project/BP project/ABP Project/Final Data/Update March 29/exposmon.csv", stringsAsFactors = FALSE)
cookstove <- read.csv("/Users/ashlinn/Dropbox/Ghana project/BP project/ABP Project/Final Data/Update March 29/cookstove.csv", stringsAsFactors = FALSE)

names(abp) <- tolower(names(abp))
names(homebp) <- tolower(names(homebp))
names(stress) <- tolower(names(stress))
names(lascar) <- tolower(names(lascar))
names(micropem) <- tolower(names(lascar))
names(dem) <- tolower(names(dem))
names(cookstove) <- tolower(names(cookstove))

# Remove study id BM1121M (test subject):
abp <- abp[!abp$mstudyid == "BM1121M",]
homebp <- homebp[!homebp$mstudyid == "BM1121M",]
stress <- stress[!stress$mstudyid == "BM1121M",]
lascar <- lascar[!lascar$mstudyid == "BM1121M",]
micropem <- micropem[!micropem$mstudyid == "BM1121M",]
dem <- dem[!dem$mstudyid == "BM1121M",]
cookstove <- cookstove[!cookstove$mstudyid == "BM1121M",]

# how many participants?
length(unique(abp$mstudyid)) #27 for ABP
length(unique(homebp$mstudyid)) #17 for Home BP
allids <- c(unique(abp$mstudyid), unique(homebp$mstudyid))
length(allids) #44
length(unique(allids)) #44

length(unique(stress$mstudyid)) #44 
length(unique(lascar$mstudyid)) #27.... check against data
length(unique(micropem$mstudyid)) #22.... check against data
length(unique(cookstove$mstudyid)) #21

# Get initial hypertension/diabetes prevalence
Enrollment <- read.csv("~/Dropbox/Ghana project/BP project/Baseline BP Paper/Final_Data/Update_Nov20/Enrollment.csv", stringsAsFactors=FALSE)
names(Enrollment) <- tolower(names(Enrollment))
Enrollment <- Enrollment[Enrollment$mstudyid %in% allids,]
Enrollment <- Enrollment[, c("bp", "diabetes")]
table(Enrollment$bp) # none
table(Enrollment$diabetes) # none

ABP_stacked <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/ABP Project/ABP R work/ABP_stackedMar17.rds")
allids <- unique(abpdata$mstudyid)
length(unique(ABP_stacked$mstudyid)) #29
unique(ABP_stacked$mstudyid[!ABP_stacked$mstudyid %in% allids]) # BM1121M, BM1425M
# these two were not in the pilot, so remove
ABP_stacked <- ABP_stacked[!ABP_stacked$mstudyid %in% c("BM1121M", "BM1425M"),]
length(unique(ABP_stacked$mstudyid)) #27

unique(abp$mstudyid[!abp$mstudyid %in% unique(ABP_stacked$mstudyid)]) # none

# Add wake-sleep data from questionnaire
# format field set and field pickup times in "0000" format
abp$unique_visit <- paste0(abp$mstudyid, "_V", abp$vround)
abp$timesleep2 <-  sprintf("%04.0f", abp$timesleep)
abp$sleeptime <- mdy_hm(paste(abp$fieldsetdd, abp$timesleep2, tz = "GMT"))
abp$timewake2 <- sprintf("%04.0f", abp$timewake)
abp$waketime <- mdy_hm(paste(abp$pickupdd, abp$timewake2, tz = "GMT"))
abp[which(abp$waketime-abp$sleeptime > 12),c("unique_visit", "datevisit", "fieldsetdd", "timesleep2", "sleeptime", "waketime", "pickupdd", "timewake2")] # a couple of them skip a day

abp[abp$unique_visit == "BM1267M_V2","sleeptime"] <- as.POSIXct("2014-08-11 19:00:00", origin = "1970-1-1", tz = "UTC")
abp$waketime[which(abp$waketime-abp$sleeptime > 12)] <- abp$waketime[which(abp$waketime-abp$sleeptime > 12)] - days(1)

sleepwake <- abp[,c("unique_visit", "sleeptime", "waketime")]

ABP_stacked$unique_visit <- paste(ABP_stacked$mstudyid, ABP_stacked$visit, sep = "_")

ABP_stacked <- merge(ABP_stacked, sleepwake, by = "unique_visit", all.x = TRUE)
ABP_stacked$night_measurement <- ifelse(ABP_stacked$date_time > ABP_stacked$sleeptime & ABP_stacked$date_time < ABP_stacked$waketime, 1, 0)
ABP_stacked$day_measurement <- ifelse(ABP_stacked$date_time <= ABP_stacked$sleeptime | ABP_stacked$date_time >= ABP_stacked$waketime, 1, 0)

# there are some misaligned "test" rows?
ABP_stacked$difftime <- as.numeric(difftime(ABP_stacked$sleeptime, ABP_stacked$date_time, units = "hours"))
mismatch <- unique(ABP_stacked$unique_visit[abs(ABP_stacked$difftime) > 20]) #8
for (i in 1: length(mismatch)) {
  data <- ABP_stacked[ABP_stacked$unique_visit == mismatch[i],]
  print(data[1, c("unique_visit", "date_time", "sleeptime", "waketime", "difftime", "day_measurement", "night_measurement")])
}

# BM1252M_V1: 1st 4 rows mismatched
# BM1335M_V1 total date mismatch??
# BM1346M_V1 total date mismatch??
# BM1252M_V2: Add 1 day to sleep/wake
# BM1255M_V2: Add 1 day to sleep/wake
# BM1263M_V2: Add 1 day to sleep/wake

# BM1427M_V1: Add 1 day to sleep/wake
ABP_stacked$sleeptime[ABP_stacked$unique_visit %in% c("BM1252M_V2", "BM1255M_V2", "BM1263M_V2", "BM1427M_V1")] <- ABP_stacked$sleeptime[ABP_stacked$unique_visit %in% c("BM1252M_V2", "BM1255M_V2", "BM1263M_V2", "BM1427M_V1")] + days(1)
ABP_stacked$waketime[ABP_stacked$unique_visit %in% c("BM1252M_V2", "BM1255M_V2", "BM1263M_V2", "BM1267M_V2", "BM1427M_V1")] <- ABP_stacked$waketime[ABP_stacked$unique_visit %in% c("BM1252M_V2", "BM1255M_V2", "BM1263M_V2", "BM1267M_V2", "BM1427M_V1")] + days(1)

# BM1267M_V2: Subtract 1 day from sleep/wake
ABP_stacked$sleeptime[ABP_stacked$unique_visit== "BM1267M_V2"] <- ABP_stacked$sleeptime[ABP_stacked$unique_visit =="BM1267M_V2"] - days(1)
ABP_stacked$waketime[ABP_stacked$unique_visit== "BM1267M_V2"] <- ABP_stacked$waketime[ABP_stacked$unique_visit =="BM1267M_V2"] - days(1)

# BM1278M_V1 fix sleeptime
ABP_stacked$sleeptime[ABP_stacked$unique_visit== "BM1278M_V1"] <- as.POSIXct("2014-07-24 19:00:00", origin = "1970-1-1", tz = "UTC")

ABP_stacked$night_measurement <- ifelse(ABP_stacked$date_time > ABP_stacked$sleeptime & ABP_stacked$date_time < ABP_stacked$waketime, 1, 0)
ABP_stacked$day_measurement <- ifelse(ABP_stacked$date_time <= ABP_stacked$sleeptime | ABP_stacked$date_time >= ABP_stacked$waketime, 1, 0)
ABP_stacked$difftime <- as.numeric(difftime(ABP_stacked$sleeptime, ABP_stacked$date_time, units = "hours"))
mismatch <- unique(ABP_stacked$unique_visit[abs(ABP_stacked$difftime) > 20]) #0


# BM1263M_V2: 1st 2 rows mismatched
row.names(ABP_stacked[ABP_stacked$unique_visit == "BM1263M_V2",]) # 403, 404
row.names(ABP_stacked[ABP_stacked$unique_visit == "BM1252M_V1",]) # 1:4
ABP_stacked <- ABP_stacked[!row.names(ABP_stacked) %in% c("1","2", "3", "4", "403", "404"),]


# BM1335M_V1 total date mismatch?? Need to add 5 days to sleep/wake
# BM1346M_V1 total date mismatch?? Need to subtract 6 days from sleep/wake
# create a flag for these two - ABP time doesn't line up with questionnaire time
ABP_stacked$date_mismatch <- ifelse(ABP_stacked$unique_visit %in% c("BM1335M_V1", "BM1346M_V1"), 1, 0)

# then change the date of the sleep/wake times to line up with the ABP measurements
ABP_stacked$sleeptime[ABP_stacked$unique_visit == "BM1335M_V1"] <- ABP_stacked$sleeptime[ABP_stacked$unique_visit  == "BM1335M_V1"] + days(5)
ABP_stacked$waketime[ABP_stacked$unique_visit == "BM1335M_V1"] <- ABP_stacked$waketime[ABP_stacked$unique_visit  == "BM1335M_V1"] + days(5)

ABP_stacked$sleeptime[ABP_stacked$unique_visit == "BM1346M_V1"] <- ABP_stacked$sleeptime[ABP_stacked$unique_visit  == "BM1346M_V1"] - days(6)
ABP_stacked$waketime[ABP_stacked$unique_visit == "BM1346M_V1"] <- ABP_stacked$waketime[ABP_stacked$unique_visit  == "BM1346M_V1"] - days(6)

ABP_stacked$night_measurement <- ifelse(ABP_stacked$date_time > ABP_stacked$sleeptime & ABP_stacked$date_time < ABP_stacked$waketime, 1, 0)
ABP_stacked$day_measurement <- ifelse(ABP_stacked$date_time <= ABP_stacked$sleeptime | ABP_stacked$date_time >= ABP_stacked$waketime, 1, 0)
ABP_stacked$difftime <- as.numeric(difftime(ABP_stacked$sleeptime, ABP_stacked$date_time, units = "hours"))
mismatch <- unique(ABP_stacked$unique_visit[abs(ABP_stacked$difftime) > 20])

# for (i in 1:length(unique(ABP_stacked$unique_visit))) {
#   data <- ABP_stacked[ABP_stacked$unique_visit == unique(ABP_stacked$unique_visit)[i],]
#   print(unique(data$unique_visit))
#   print(table(data$night_measurement, data$day_measurement))
# }

# check: "BM1287M_V1", "BM1369M_V1", "BM1424M_V2"
# BM1287M_V1 and BM1369M_V1 didn't wear overnight but put back on in the morning
# BM1424M_V2 all rows are duplicated
ABP_stacked <- ABP_stacked[!duplicated(ABP_stacked),]

# add Morning SBP and nighttime trough SBP
ABP_stacked$waketimeplus2 <- ABP_stacked$waketime + hours(2)
ABP_stacked$morning_measurement <- ifelse(ABP_stacked$date_time > ABP_stacked$waketime & ABP_stacked$date_time <= ABP_stacked$waketimeplus2, 1, 0)



saveRDS(ABP_stacked, file = paste0("ABP_stacked_", format(Sys.Date(), format = "%b%d"), ".rds"))

# GENERATE SUMMARY STATISTICS for ABP-----

# Error percentages
ABP_errors <- ABP_stacked %.% group_by(file) %.% dplyr:: summarise(sum(event_code), nobs[1], length(SBP)) # compare last 2 columns to see if there are any discrepancies
names(ABP_errors) <- c("file", "errors", "nobs", "nrows")
ABP_errors$percent_errors <- ABP_errors$errors/ABP_errors$nobs * 100


# remove Error rows from the data
ABP_stacked_noerr <- ABP_stacked[ABP_stacked$event_code ==0,]
row.names(ABP_stacked_noerr) <- NULL

# add nighttime trough: the lowest nighttime reading plus the readings immediately before and after
ABP_stacked_noerr$nighttime_trough <- 0
unique_visits <- unique(ABP_stacked_noerr$unique_visit)
for (i in 1:length(unique_visits)) {
  data <- ABP_stacked_noerr[ABP_stacked_noerr$unique_visit == unique_visits[i],]
  if(sum(data$night_measurement >0)) {
  data$nighttime_trough[which(data$night_measurement ==1 & data$SBP == min(data$SBP[data$night_measurement ==1]))[1]] <- 1
lowest <- which(data$nighttime_trough ==1) # just use the first one
  data$nighttime_trough[(lowest-1):(lowest +1)] <- 1
}
  ABP_stacked_noerr2 <- rbind(ABP_stacked_noerr2, data)
}
 
ABP_stacked_noerr <- ABP_stacked_noerr2

table(ABP_stacked_noerr$unique_visit, ABP_stacked_noerr$nighttime_trough) 



# create separate errors dataset
ABP_stacked_errors <- ABP_stacked[ABP_stacked$event_code ==1,]
row.names(ABP_stacked_errors) <- NULL

# Summary stats for the good data
ABP_summary <- ABP_stacked_noerr %.% group_by(file) %.% dplyr::summarise(mstudyid[1], unique_visit[1], date_time[1], last(date_time), session[1], nobs[1], length(SBP), min(SBP), mean(SBP), max(SBP), sd(SBP), min(DBP), mean(DBP), max(DBP), sd(DBP), mean(MAP), mean(HR), sleeptime[1], waketime[1], sum(day_measurement), sum(night_measurement))

names(ABP_summary) <- c("file", "mstudyid", "unique_visit", "date_time_first", "date_time_last", "session", "total_obs", "good_obs", "min_SBP", "mean_SBP", "max_SBP", "sd_SBP", "min_DBP", "mean_DBP", "max_DBP", "sd_DBP", "mean_MAP", "mean_HR", "sleeptime", "waketime", "day_obs_diary", "night_obs_diary")

# # add day_obs 
# ABP_summary$day_obs_asia <- ABP_summary$total_obs - ABP_summary$night_obs_asia
# ABP_summary$day_obs_europe <- ABP_summary$total_obs - ABP_summary$night_obs_europe
# 
# # add validity (according to IDACO criteria for Asia/Europe)
# ABP_summary$is_valid_asia <- ifelse(ABP_summary$day_obs_asia >=10 & ABP_summary$night_obs_asia >=5, 1, 0) # 3 out of 29 invalid
# ABP_summary$is_valid_europe <- ifelse(ABP_summary$day_obs_europe >=10 & ABP_summary$night_obs_europe >=5, 1, 0) # 3 out of 29 invalid
# 
# # validity using truncated Asia ranges (day = 8a-6p; night = 10p-4a)
# ABP_summary$is_valid_asia_trunc <- ifelse(ABP_summary$day_eight_six >=10 & ABP_summary$night_ten_four >=5, 1, 0) # 5 out of 29 invalid
# 
# # validity using truncated Europe ranges (day = 10a-8p; night = 12a-6a)
# ABP_summary$is_valid_europe_trunc <- ifelse(ABP_summary$day_ten_eight >=10 & ABP_summary$night_twelve_six >=5, 1, 0) # 5 out of 29 invalid

# validity using diary information
ABP_summary$is_valid_diary <- ifelse(ABP_summary$day_obs_diary >=10 & ABP_summary$night_obs_diary>=5, 1,0)
# add duration
ABP_summary$date_time_last <- as.POSIXct(ABP_summary$date_time_last, origin = "1970-1-1", tz = "GMT")
ABP_summary$hours <- as.numeric(difftime(ABP_summary$date_time_last, ABP_summary$date_time_first, units = "hours"))


pdf(file = "all_ABP_plots.pdf")
unique_visits <- unique(ABP_stacked_noerr$unique_visit)
par(mfrow = c(2,2))
for (i in 1:length(unique_visits)) {
  data <- ABP_stacked_noerr[ABP_stacked_noerr$unique_visit == unique_visits[i],]
  data3 <- ABP_summary[ABP_summary$unique_visit == unique_visits[i],]
  plot(data$date_time, data$SBP, main = paste(unique_visits[i], "\n Validity", data3$is_valid_diary))
  lines(data$date_time, data$SBP)
  abline(v = data3$sleeptime, col = "blue")
  abline(v = data3$waketime, col = "red")
}
dev.off()


# mean daytime and nighttime SBP and DBP (using truncated Asia ranges)
# ABP_stats <- group_by(ABP_stacked_noerr, file) %.% summarise(mstudyid[1], unique_visit[1], mean(SBP), mean(DBP), sd(SBP), sd(DBP))
# 
# ABP_stats_day <- group_by(ABP_stacked_noerr[ABP_stacked_noerr$day_eight_six == 1,], file) %.% summarise(mstudyid[1], unique_visit[1], mean(SBP), mean(DBP), sd(SBP), sd(DBP))
# 
# ABP_stats_night <- group_by(ABP_stacked_noerr[ABP_stacked_noerr$night_ten_four == 1,], file) %.% summarise(mstudyid[1], unique_visit[1], mean(SBP), mean(DBP), sd(SBP), sd(DBP))
# 
# ABP_stats_day <- ABP_stats_day[ABP_stats_day$file %in% ABP_summary_valid$file,]
# ABP_stats_night <- ABP_stats_night[ABP_stats_night$file %in% ABP_summary_valid$file,]
# 
# stat.desc(ABP_stats) # SBP: 107.0 (6.4); DBP: 62.5 (5.0)
# stat.desc(ABP_stats_day) # SBP: 110.4 (6.4); DBP: 66.6 (5.0)
# stat.desc(ABP_stats_night) # SBP: 98.4 (7.6); DBP: 54.2 (6.5)

# Daytime/Nighttime stats using diary values

ABP_stats <- group_by(ABP_stacked_noerr, file) %.% summarise(mstudyid[1], unique_visit[1], mean(SBP), mean(DBP), sd(SBP), sd(DBP))
# ABP_stats <- ABP_stats[ABP_stats$file %in% ABP_summary_valid$file,]
stat.desc(ABP_stats) # SBP: 106.2 (5.6); DBP: 62.2 (4.6)
names(ABP_stats) <- c("file", "mstudyid", "unique_visit", "mean_24h_SBP", "mean_24h_DBP", "sd_24h_SBP", "sd_24h_DBP")

ABP_stats_day <- group_by(ABP_stacked_noerr[ABP_stacked_noerr$day_measurement == 1,], file) %.% summarise(mstudyid[1], unique_visit[1], mean(SBP), mean(DBP), sd(SBP), sd(DBP))
# ABP_stats_day <- ABP_stats_day[ABP_stats_day$file %in% ABP_summary_valid$file,]
# stat.desc(ABP_stats_day) # SBP: 110.4 (5.7); DBP: 66.8 (4.9)
names(ABP_stats_day) <- c("file", "mstudyid", "unique_visit", "mean_SBP_awake", "mean_DBP_awake", "sd_SBP_awake", "sd_DBP_awake")

ABP_stats_night <- group_by(ABP_stacked_noerr[ABP_stacked_noerr$night_measurement == 1,], file) %.% summarise(mstudyid[1], unique_visit[1], mean(SBP), mean(DBP), sd(SBP), sd(DBP))
# ABP_stats_night <- ABP_stats_night[ABP_stats_night$file %in% ABP_summary_valid$file,]
# stat.desc(ABP_stats_night) # SBP: 99.9 (7.5); DBP: 55.6 (6.0)
names(ABP_stats_night) <- c("file", "mstudyid", "unique_visit", "mean_SBP_asleep", "mean_DBP_asleep", "sd_SBP_asleep", "sd_DBP_asleep")

# Morning SBP and Nighttime Trough SBP
ABP_stats_morning <- group_by(ABP_stacked_noerr[ABP_stacked_noerr$morning_measurement == 1,], file) %.% summarise(unique_visit = unique_visit[1], mean_SBP_morning = mean(SBP))

ABP_stats_nighttime_trough <- group_by(ABP_stacked_noerr[ABP_stacked_noerr$nighttime_trough == 1,], file) %.% summarise(unique_visit = unique_visit[1], nighttime_trough_SBP = mean(SBP)) # 50 records 

ABP_stats_all <- merge(ABP_stats, ABP_stats_day[,3:7], by = "unique_visit", all= TRUE)
ABP_stats_all <- merge(ABP_stats_all, ABP_stats_night[,3:7], by = "unique_visit", all = TRUE)
ABP_stats_all <- merge(ABP_stats_all, ABP_stats_morning[,2:3], by = "unique_visit", all = TRUE)
ABP_stats_all <- merge(ABP_stats_all, ABP_stats_nighttime_trough[,2:3], by = "unique_visit", all = TRUE)

ABP_summary <- merge(ABP_summary, ABP_stats_all[, c(1, 4:ncol(ABP_stats_all))], by = "unique_visit", all.x = TRUE)


# Morning Surge (morning SBP - nighttime trough SBP); Non-dipping (nocturnal decline <=10% - IS THIS OVERALL OR TROUGH?)
ABP_summary$morning_surge <- ABP_summary$mean_SBP_morning - ABP_summary$nighttime_trough_SBP
ABP_summary$non_dip <- ifelse(((ABP_summary$mean_SBP_awake - ABP_summary$mean_SBP_asleep)/ABP_summary$mean_SBP_awake) * 100 <= 10, 1, 0)



  
saveRDS(ABP_summary, file = paste0("ABP_summary_", format(Sys.Date(), format = "%b%d"), ".rds"))

ABP_summary_valid <- ABP_summary[ABP_summary$is_valid_diary == 1,] # 48 obs

saveRDS(ABP_summary_valid, file = paste0("ABP_summary_valid", format(Sys.Date(), format = "%b%d"), ".rds"))




## Adding village codes ---- 
# NOTE these are for convenience. Don't save "village_codes" file after as some things are not quite accurate.
library(foreign)
village_randomization <- read.dta("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Final_Data/Ghana_randomization.dta")
village_codes <- read.dta("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Final_Data/village info/clusters_for_CEM.dta")
village_codes <- village_codes[order(village_codes$village),]
abp$vname <- gsub(" ", "", abp$vname)
length(unique(abp$vname)) #19
unique(abp$vname[!abp$vname %in% village_codes$village])

abp$vname[abp$vname == "AMPOMAH"] <- "AMPOMA"
abp$vname[abp$vname == "ATTAAKURA"] <- "ATTA AKURA"
abp$vname[abp$vname == "AJENA"] <- "AJINA"
abp$vname[abp$vname == "DUMSO" |abp$vname == "DUMSO2" ] <- "DUMSO 2"
abp$vname[abp$vname == "JATOAKURA"] <- "JATO-AKURA"

village_codes$village[village_codes$code == "JA"] <- "JATO-AKURA"
village_codes$village[village_codes$code == "AB+NT"] <- "ABUDWOM"
village_codes$village[village_codes$code == "LH+UR"] <- "ALHASSANAKURA"


for (i in 1:nrow(abp)) {
 abp$vcode[i] <- ifelse(abp$vname[i] == "KURAWURAAKURA", "LH+UR", village_codes$code[village_codes$village == abp$vname[i]])
}


homebp$vname <- gsub(" ", "", homebp$vname)
unique(homebp$vname[!homebp$vname %in% village_codes$village])
homebp$vname[homebp$vname == "AJENA"] <- "AJINA"
homebp$vname[homebp$vname == "BADUAKURA"] <- "BADU AKURA"
homebp$vname[homebp$vname == "NANTEZONGO"] <- "NANTE ZONGO"
village_codes$village[village_codes$code == "AB+NT"] <- "NTANKRO"

for (i in 1:nrow(homebp)) {
  homebp$vcode[i] <- village_codes$code[village_codes$village == homebp$vname[i]]
}

# # merging ABP data -----
abp <- abp[order(abp$unique_visit),]
ABP_summary <- ABP_summary[order(ABP_summary$unique_visit),]

abpdata <- merge(abp, ABP_summary[,c(1:2, 4:5,7:ncol(ABP_summary))], by = "unique_visit", all = TRUE)



### Calculate home bp parameters ------


homebp[,c("sbp1", "sbp2", "sbp3", "sbp4", "sbp5", "sbp6", "sbp7", "sbp8", "sbp9", "sbp10")]
for (i in c("sbp1", "sbp2", "sbp3", "sbp4", "sbp5", "sbp6", "sbp7", "sbp8", "sbp9", "sbp10")) {
homebp[, i] <- mapvalues(homebp[,i], from = c(999, 0), to = c(NA, NA))
}
summary(homebp[,c("sbp1", "sbp2", "sbp3", "sbp4", "sbp5", "sbp6", "sbp7", "sbp8", "sbp9", "sbp10")])

homebp[,c("bdp1", "bdp2", "bdp3", "bdp4", "bdp5", "bdp6", "bdp7", "bdp8", "bdp9", "bdp10")]
for (i in c("bdp1", "bdp2", "bdp3", "bdp4", "bdp5", "bdp6", "bdp7", "bdp8", "bdp9", "bdp10")) {
  homebp[, i] <- mapvalues(homebp[,i], from = c(999, 0), to = c(NA, NA))
}
summary(homebp[,c("bdp1", "bdp2", "bdp3", "bdp4", "bdp5", "bdp6", "bdp7", "bdp8", "bdp9", "bdp10")])

homebp$mean_sbp <- rowMeans(homebp[,c("sbp1", "sbp2", "sbp3", "sbp4", "sbp5", "sbp6", "sbp7", "sbp8", "sbp9", "sbp10")], na.rm = TRUE)
homebp$mean_dbp <- rowMeans(homebp[,c("bdp1", "bdp2", "bdp3", "bdp4", "bdp5", "bdp6", "bdp7", "bdp8", "bdp9", "bdp10")], na.rm = TRUE)


saveRDS(homebp, file = paste0("homebp_", format(Sys.Date(), format = "%b%d"), ".rds"))

homebp_short <- homebp[,c(1:4, 131:ncol(homebp))]
saveRDS(homebp_short, file= paste0("homebp_short_", format(Sys.Date(), format = "%b%d"), ".rds"))

homebp <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/homebp_Sep09.rds")

# home bp success rate
homebp[,c("success1", "success2", "success3", "success4", "success5", "success6", "success7", "success8", "success9", "success10")]
for (i in c("success1", "success2", "success3", "success4", "success5", "success6", "success7", "success8", "success9", "success10")) {
  homebp[,i] <- mapvalues(homebp[,i], from = c (1,2), to = c(1,0))
}
homebp[,c("success1", "success2", "success3", "success4", "success5", "success6", "success7", "success8", "success9", "success10")]
which(homebp[,c("success1", "success2", "success3", "success4", "success5", "success6", "success7", "success8", "success9", "success10")] !=1) # 4 of these
homebp[,c("ncomment1", "ncomment2", "ncomment3", "ncomment4", "ncomment5", "ncomment6", "ncomment7", "ncomment8", "ncomment9", "ncomment10")]

homebp$total_success <- homebp$success1 + homebp$success2 + homebp$success3 + homebp$success4 + homebp$success5 + homebp$success6 + homebp$success7 + homebp$success8 + homebp$success9 + homebp$success10
homebp$morning_success <- homebp$success1 + homebp$success3 + homebp$success5 + homebp$success7 + homebp$success9
homebp$evening_success <- homebp$success2 + homebp$success4 + homebp$success6 + homebp$success8 + homebp$success10
stat.desc(homebp[,c("total_success", "morning_success", "evening_success")])

# # Looking at results between visits 1 and 2
# library(lattice)
# 
# # home bp
# histogram(~mean_sbp | as.factor(vround), data = homebp, layout = c(1,2))
# densityplot(~mean_sbp | as.factor(vround), data = homebp, layout = c(1,2))
# histogram(~mean_dbp | as.factor(vround), data = homebp, layout = c(1,2))
# densityplot(~mean_dbp | as.factor(vround), data = homebp, layout = c(1,2))
# 
# # abp  - BUT should check these values against the actual data
# histogram(~asv | as.factor(vround), data = abp, layout = c(1,2))
# densityplot(~asv | as.factor(vround), data = abp, layout = c(1,2))
# histogram(~adv | as.factor(vround), data = abp, layout = c(1,2))
# densityplot(~adv | as.factor(vround), data = abp, layout = c(1,2))
# 
# mean(homebp$mean_sbp[homebp$vround == 1]) #100.6033
# mean(homebp$mean_sbp[homebp$vround == 2]) #99.48
# t.test(mean_sbp~vround, data = homebp) # p = 0.7  BUT THESE SHOULD BE DONE AS MATCHED PAIRS
# 
# mean(abp$asv[abp$vround == 1]) #107.46
# mean(abp$asv[abp$vround == 2]) #106.88
# t.test (asv ~ vround, data = abp) #p = 0.7
# 
# mean(homebp$mean_dbp[homebp$vround == 1]) #65.78
# mean(homebp$mean_dbp[homebp$vround == 2]) #65.05
# t.test(mean_dbp~vround, data = homebp) # p = 0.7
# 
# mean(abp$adv[abp$vround == 1]) #63.07
# mean(abp$adv[abp$vround == 2]) #62.19
# t.test (adv ~ vround, data = abp) #p = 0.5

# For demographic info - this also includes all the lascar CO info for the first session
baselinebpdata <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/baselinedata_Feb12.rds")
allids[!allids %in% baselinebpdata$mstudyid] # all are in baselinebpdata

demo_info <- baselinebpdata[baselinebpdata$mstudyid %in% allids,]


# MERGE WITH CO DATA ----------
abpdata <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/ABPdata_Nov17.rds")

homebp <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/homebp_Mar31.rds")

CO_parameters <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/CO_parameters_7152sessions_Feb03.rds")

# GETTING FILE ALIGNMENT -----
# ABP

StudyIDs <- unique(abpdata$mstudyid) #27


for (i in 1:nrow(abpdata)) {
  co <- CO_parameters[CO_parameters$mstudyid == abpdata$mstudyid[i],]
  abpdata$COfirstdate[i] <- co$firstdate[which.min(abs(abpdata$date_time_first[i] - co$firstdate))]
  abpdata$COlastdate[i] <- co$lastdate[which.min(abs(abpdata$date_time_first[i]- co$firstdate))]
  abpdata$COfile[i] <- co$file[which.min(abs(abpdata$date_time_first[i] - co$firstdate))]
}

# two were not picked up: BM1269 vround2, BM1277 vround2
co <- CO_parameters[CO_parameters$mstudyid == "BM1269M",]
abpdata$COfirstdate[abpdata$mstudyid == "BM1269M" & abpdata$vround == 2] <- co$firstdate[co$file == "CU_CO_049_BM1269M_25Sep14_s_02_pri.txt"]
abpdata$COlastdate[abpdata$mstudyid == "BM1269M" & abpdata$vround == 2] <- co$lastdate[co$file == "CU_CO_049_BM1269M_25Sep14_s_02_pri.txt"]
abpdata$COfile[abpdata$mstudyid == "BM1269M" & abpdata$vround == 2] <- "CU_CO_049_BM1269M_25Sep14_s_02_pri.txt"

co <- CO_parameters[CO_parameters$mstudyid == "BM1277M",]
abpdata$COfirstdate[abpdata$mstudyid == "BM1277M" & abpdata$vround == 2] <- co$firstdate[co$file == "CU_CO_134_BM1277M_03Oct14_s_02_pri.txt"]
abpdata$COlastdate[abpdata$mstudyid == "BM1277M" & abpdata$vround == 2] <- co$lastdate[co$file == "CU_CO_134_BM1277M_03Oct14_s_02_pri.txt"]
abpdata$COfile[abpdata$mstudyid == "BM1277M" & abpdata$vround == 2] <- "CU_CO_134_BM1277M_03Oct14_s_02_pri.txt"


for (i in c("COfirstdate", "COlastdate")) {
  abpdata[,i] <- as.POSIXct(abpdata[,i], origin = "1970-1-1", tz = "UTC")
}

abpdata$overlap <- as.numeric(abpdata$COfirstdate - abpdata$date_time_first, units = "days")
abpdata$overlap2 <- as.numeric(abpdata$COlastdate - abpdata$date_time_first, units = "days")
abpdata$overlap_ok <- ifelse(abpdata$overlap < 0 & abpdata$overlap2 > 0, 1, 0)

table(abpdata$overlap_ok) # 0: 32; 1: 21

# some don't overlap but would still be useful for the pre- or post-intervention mean - let's say if within 1 week on either side
abpdata$permissible_co <- ifelse(abpdata$overlap2 > -8 & abpdata$overlap2 < 8, 1, 0)
table(abpdata$permissible_co) # 0: 14, 1: 39


# allow CO data even if it doesn't overlap within the week, as long as it is pre- or post-intervention, and on correct side of stove delivery 
round1ok <- c("BM1369M", "BM1421M")
round2ok <- c("BM1271M", "BM1278M", "BM1419M", "BM1420M", "BM1269M", "BM1277M")

abpdata$permissible_co[abpdata$mstudyid %in% round1ok & abpdata$vround ==1] <- 1
abpdata$permissible_co[abpdata$mstudyid %in% round2ok & abpdata$vround ==2] <- 1
table(abpdata$permissible_co) # 0: 6, 1: 47. 

# five were not yet validated: BM1413, BM1416, BM1417, BM1418, BM1424
# one is missing: BM 1421


abpdata$COfile[abpdata$permissible_co == 0] <- NA
COdata <- CO_parameters[CO_parameters$file %in% abpdata$COfile,]

names(COdata)[21]  <- "CO_hours"

abpdata <- merge(abpdata, COdata[, c(1,3:ncol(COdata))], by.x = "COfile", by.y = "file", all = TRUE)
abpdata <- abpdata[order(abpdata$unique_visit),]

# Validating the unvalidated ones -------
files <- list.files("/Users/ashlinn/Dropbox/Ghana project/BP project/ABP Project/CO_data_to_validate_ABP/", full.names = T)
length(files) #5

# then go through Lascar Processing by SN script through genereation of CO_parameters
# visually, all look good

CO_parameters_missingfiles <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/ABP Project/CO_parameters_ABP_5sessions_Nov13.rds")

CO_parameters_missingfiles$unique_visit <- paste(CO_parameters_missingfiles$mstudyid, "V2", sep = "_")

CO_parameters_missingfiles <- subset(CO_parameters_missingfiles, select = -c(mstudyid, SN, hours, session))

names(CO_parameters_missingfiles)[1] <- "COfile"
names(CO_parameters_missingfiles)[4:5] <- c("COfirstdate", "COlastdate")


for (i in 1:nrow(CO_parameters_missingfiles)) { 
  for (j in colnames(CO_parameters_missingfiles[1:16])) {
  abpdata[abpdata$unique_visit == CO_parameters_missingfiles$unique_visit[i], j] <- CO_parameters_missingfiles[i,j]
}
}
abpdata[abpdata$unique_visit %in% c("BM1413M_V2", "BM1416M_V2", "BM1417M_V2", "BM1418M_V2", "BM1424M_V2"), c("unique_visit", "COfirstdate", "COlastdate", "date_time_first", "overlap", "overlap2", "overlap_ok", "stove_delivery", "permissible_co")]

# all these 5 files are non-syncrhonous with ABP but CO is permissible (happened after ABP and after stove intervention)
abpdata[abpdata$unique_visit %in% c("BM1413M_V2", "BM1416M_V2", "BM1417M_V2", "BM1418M_V2", "BM1424M_V2"), "permissible_co"] <- 1

table(abpdata$permissible_co) # 0: 1 (file missing), 1: 52

abpdata[abpdata$unique_visit %in% c("BM1413M_V2", "BM1416M_V2", "BM1417M_V2", "BM1418M_V2", "BM1424M_V2"), "overall_valid"] <- 1

#homebp -----
StudyIDs <- unique(homebp$mstudyid) #17
CO_parameters <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/CO_parameters_7152sessions_Feb03.rds")

homebp$date_time <- mdy(homebp$dvisit1)

for (i in 1:nrow(homebp)) {
  co <- CO_parameters[CO_parameters$mstudyid == homebp$mstudyid[i],]
  homebp$COfirstdate[i] <- co$firstdate[which.min(abs(homebp$date_time[i] - co$firstdate))]
  homebp$COlastdate[i] <- co$lastdate[which.min(abs(homebp$date_time[i]- co$firstdate))]
  homebp$COfile[i] <- co$file[which.min(abs(homebp$date_time[i] - co$firstdate))]
}

for (i in c("COfirstdate", "COlastdate")) {
  homebp[,i] <- as.POSIXct(homebp[,i], origin = "1970-1-1", tz = "UTC")
}

homebp$overlap <- as.numeric(homebp$COfirstdate - homebp$date_time, units = "days")
homebp$overlap2 <- as.numeric(homebp$COlastdate - homebp$date_time, units = "days")
homebp$overlap_ok <- ifelse(homebp$overlap < 0 & homebp$overlap2 > 0, 1, 0)

table(homebp$overlap_ok) # 0: 13; 1: 20

# some don't overlap but would still be useful for the pre- or post-intervention mean - let's say if within 1 week on either side
homebp$permissible_co <- ifelse(homebp$overlap2 > -8 & homebp$overlap2 < 8, 1, 0)
table(homebp$permissible_co) # 0: 7, 1: 26

# allow others if they're appropriately pre- or post- intervention
ok_co <- c("BM1289M", "BM1295M", "BM1302M", "BM1303M", "BM1336M", "BM1337M")
homebp$permissible_co[homebp$mstudyid %in% ok_co & homebp$vround ==2]  <- 1
table(homebp$permissible_co) # 0: 1, 1: 32

homebp$COfile[homebp$permissible_co == 0] <- NA
COdata <- CO_parameters[CO_parameters$file %in% homebp$COfile,]

homebp <- merge(homebp, COdata, by.x = "COfile", by.y = "file", all = TRUE)

homebp <- homebp[order(homebp$mstudyid, homebp$vround),]




# ADD COOKSTOVE DATA -----
names(cookstove)[3] <- "stove_delivery"
names(homebp)[2] <- "mstudyid"
abpdata <- merge(abpdata, cookstove[,c("mstudyid", "stove_delivery")], by = "mstudyid", all.x = TRUE)
homebp <- merge(homebp, cookstove[,c("mstudyid", "stove_delivery")], by = "mstudyid", all.x = TRUE)

homebp_short <- homebp[,c(1:4, 132:ncol(homebp))]

# all the stoves delivered after the 1st visit = good (except for questionable BM1295M - assume 4/6 is a typo for 8/6?)

saveRDS(homebp, file = paste0("homebp_", format(Sys.Date(), format = "%b%d"), ".rds"))
saveRDS(homebp_short, file= paste0("homebp_short_", format(Sys.Date(), format = "%b%d"), ".rds"))




# add village randomization
village_randomization <- read.dta("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Final_Data/Ghana_randomization.dta")
names(village_randomization)[1] <- "vcode"
random <- village_randomization[,c("vcode", "arm")] # 1 = control, 2 = biolite, 3 = LPG
abpdata <- merge(abpdata, random, by = "vcode")
abpdata$arm2 <- ifelse(abpdata$arm ==1, "Control", ifelse(abpdata$arm ==3, "LPG", "BioLite"))


# add UPEM alignment
UPEM_stacked <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/UPEM_stacked_Oct08.rds")


# figure out overlap for UPEM-----
UPEM_stacked <- arrange(UPEM_stacked, filter, unique_min)
abpdata <- merge(abpdata, filematch[,c("filter", "unique_visit")], by = "unique_visit", all.x = TRUE)
abpdata$UPEMfirstdate <- NA
abpdata$UPEMlastdate <- NA
for (i in 1:nrow(abpdata)) {
  data <- UPEM_stacked[UPEM_stacked$filter == abpdata$filter[i],]
  if(nrow(data) !=0) {
    abpdata$UPEMfirstdate[i] <- data$unique_min[1]
    abpdata$UPEMlastdate[i] <- data$unique_min[nrow(data)]
  }
}



abpdata$UPEMfirstdate <- ymd_hms(abpdata$UPEMfirstdate, tz = "GMT")
abpdata$UPEMlastdate <- ymd_hms(abpdata$UPEMlastdate, tz = "GMT")
abpdata$UPEMoverlap <- as.numeric(abpdata$UPEMfirstdate - abpdata$date_time_first, units = "days")
abpdata$UPEMoverlap2 <- as.numeric(abpdata$UPEMlastdate - abpdata$date_time_first, units = "days")
abpdata$UPEMoverlap_ok <- ifelse(abpdata$UPEMoverlap < 0 & abpdata$UPEMoverlap2 > 0 &!is.na(abpdata$UPEMoverlap), 1, 0)
sum(abpdata$UPEMoverlap_ok) # 6 out of 20 


# time-weighted 24h BP average---------
# method from Joe Schwartz (email 11/14)
# What I do for almost all of our studies is:
# 1)  compute the average of all awake readings
# 2)  compute the average of all sleep readings
# 3)  compute the duration of sleep (based on self-report or actigraphy), measured in hours
# 4)  compute 24-hr average = [(sleep duration * sleep average BP) + {(24 - sleep duration) * awake average BP}] / 24

abpdata$sleep_duration <- round(as.numeric(abpdata$waketime - abpdata$sleeptime), digits = 1)

abpdata$mean_24h_SBP_tw <- ((abpdata$mean_SBP_asleep * abpdata$sleep_duration) + ((24 - abpdata$sleep_duration)*abpdata$mean_SBP_awake))/24
abpdata$mean_24h_DBP_tw <- ((abpdata$mean_DBP_asleep * abpdata$sleep_duration) + ((24 - abpdata$sleep_duration)*abpdata$mean_DBP_awake))/24

summary(abpdata$mean_24h_SBP - abpdata$mean_24h_SBP_tw) # mean = 0.26, man = 6.7
summary(abpdata$mean_24h_DBP - abpdata$mean_24h_DBP_tw) # mean = 0.14, max = 4.8

# daytime validity using NICE criteria
# at least 14 daytime measurements as per NICE guidelines Note - no cutoff criteria for duration (eg we have one file with duration 11 hours, one with 17.3, the rest are all >20)
abpdata$is_valid_daytime <- ifelse(abpdata$day_obs_diary >=14, 1, 0)

saveRDS(abpdata, file = paste0("abpdata_", format(Sys.Date(), format = "%b%d"), ".rds"))


abpdata_valid <- abpdata[abpdata$is_valid_diary ==1,] #49 obs
saveRDS(abpdata_valid, file = paste0("abpdata_valid_", format(Sys.Date(), format = "%b%d"), ".rds"))

# NOTE for daytime only, all 53 files have at least 10 daytime measurement (52 at least 14 daytime measurements as per NICE guidelines, the one with 11 measurements looks wonky). So use 52 for daytime analyses. Note - no cutoff criteria for duration (eg we have one file with duration 11 hours, one with 17.3, the rest are all >20)
abpdata_valid_daytime <- abpdata[abpdata$is_valid_daytime ==1,] # 52
saveRDS(abpdata_valid_daytime, file = paste0("abpdata_valid_daytime_", format(Sys.Date(), format = "%b%d"), ".rds"))

# redo with new time-weighting?
#### ABP STATS ---------
abpdata_valid <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/abpdata_valid_Nov17.rds")

# how many sessions
nrow(abpdata_valid) # 49 valid/53 = 92.4%

# how many women
length(unique(abpdata_valid$mstudyid)) # 27

# length of session
mean(abpdata_valid$hours) #23.1
sd(abpdata_valid$hours) #1.6

# BP 
# overall
sapply(abpdata_valid[, c("mean_24h_SBP", "mean_24h_SBP_tw", "mean_24h_DBP", "mean_24h_DBP_tw", "mean_SBP_awake", "mean_DBP_awake", "mean_SBP_asleep", "mean_DBP_asleep", "nighttime_trough_SBP")], FUN = mean)
sapply(abpdata_valid[, c("mean_24h_SBP", "mean_24h_SBP_tw", "mean_24h_DBP", "mean_24h_DBP_tw", "mean_SBP_awake", "mean_DBP_awake", "mean_SBP_asleep", "mean_DBP_asleep", "nighttime_trough_SBP")], FUN = sd)

# session1
sapply(abpdata_valid[abpdata_valid$vround ==1, c("mean_24h_SBP", "mean_24h_SBP_tw", "mean_24h_DBP", "mean_24h_DBP_tw", "mean_SBP_awake", "mean_DBP_awake", "mean_SBP_asleep", "mean_DBP_asleep", "nighttime_trough_SBP")], FUN = mean)
sapply(abpdata_valid[abpdata_valid$vround ==1, c("mean_24h_SBP", "mean_24h_SBP_tw", "mean_24h_DBP", "mean_24h_DBP_tw", "mean_SBP_awake", "mean_DBP_awake", "mean_SBP_asleep", "mean_DBP_asleep", "nighttime_trough_SBP")], FUN = sd)

# session2
nrow(abpdata_valid[abpdata_valid$vround ==2,])
sapply(abpdata_valid[abpdata_valid$vround ==2, c("mean_24h_SBP", "mean_24h_SBP_tw", "mean_24h_DBP", "mean_24h_DBP_tw", "mean_SBP_awake", "mean_DBP_awake", "mean_SBP_asleep", "mean_DBP_asleep", "nighttime_trough_SBP")], FUN = mean)
sapply(abpdata_valid[abpdata_valid$vround ==2, c("mean_24h_SBP", "mean_24h_SBP_tw", "mean_24h_DBP", "mean_24h_DBP_tw", "mean_SBP_awake", "mean_DBP_awake", "mean_SBP_asleep", "mean_DBP_asleep", "nighttime_trough_SBP")], FUN = sd)

# how many measurements
mean(abpdata_valid$good_obs) #49.9
sd(abpdata_valid$good_obs) #10.9
mean(abpdata_valid$day_obs_diary) #30.7
sd(abpdata_valid$day_obs_diary) #9.0
mean(abpdata_valid$night_obs_diary) #19.2
sd(abpdata_valid$night_obs_diary) #4.0

# duration
mean(abpdata_valid$hours) #23.1
sd(abpdata_valid$hours) #1.6

# how many with: 
# non-dipping pattern (nocturnal decline <= 10% ) 
sum(abpdata_valid$non_dip) # 28
table(abpdata_valid$non_dip, abpdata_valid$vround)
length(unique(abpdata_valid$mstudyid[abpdata_valid$non_dip ==1])) # 18
table(abpdata_valid$mstudyid[abpdata_valid$non_dip ==1], abpdata_valid$vround[abpdata_valid$non_dip ==1])

# nocturnal hypertension (mean SBP/DBP during nighttime >= 120/70)
nrow(abpdata_valid[abpdata_valid$mean_SBP_asleep >=120,]) # 0
nrow(abpdata_valid[abpdata_valid$mean_DBP_asleep >=70,]) # 2
abpdata_valid[abpdata_valid$mean_DBP_asleep >=70, "unique_visit"] # different women, both on V2

# awake ambulatory hypertension (mean daytime SBP/DBP >= 135/85)
nrow(abpdata_valid[abpdata_valid$mean_SBP_awake >=135,]) # 0
nrow(abpdata_valid[abpdata_valid$mean_DBP_awake >=85,]) # 0

# normotension
nrow(abpdata_valid[abpdata_valid$mean_SBP_awake < 135 & abpdata_valid$mean_DBP_awake < 85 & abpdata_valid$mean_SBP_asleep < 120 & abpdata_valid$mean_DBP_asleep < 70,]) #47
nrow(abpdata_valid[abpdata_valid$mean_SBP_awake < 135 & abpdata_valid$mean_DBP_awake < 85 & abpdata_valid$mean_SBP_asleep < 120 & abpdata_valid$mean_DBP_asleep < 70 & abpdata_valid$vround ==1,]) #23
nrow(abpdata_valid[abpdata_valid$mean_SBP_awake < 135 & abpdata_valid$mean_DBP_awake < 85 & abpdata_valid$mean_SBP_asleep < 120 & abpdata_valid$mean_DBP_asleep < 70 & abpdata_valid$vround ==2,]) #24

# hypertension using 24h means
nrow(abpdata_valid[abpdata_valid$mean_SBP >=130 | abpdata_valid$mean_DBP >=80,]) #0

# are any associated with exposure/with intervention status?
abpdata <- abpdata_valid
abpdata$intervention <- ifelse(abpdata$arm2 %in% c("LPG", "BioLite"), 1, 0)
# mean 24h SBP/DBP
fit <- lme(fixed = mean_24h_SBP ~ intervention, random = ~1|mstudyid, data = abpdata)
summary(fit) # not sig
fit <- lme(fixed = mean_24h_DBP ~ intervention, random = ~1|mstudyid, data = abpdata)
summary(fit) # not sig
# awake SBP/DBP
fit <- lme(fixed = mean_SBP_awake ~ intervention, random = ~1|mstudyid, data = abpdata)
summary(fit) # not sig
fit <- lme(fixed = mean_DBP_awake ~ intervention, random = ~1|mstudyid, data = abpdata)
summary(fit) # not sig
# asleep SBP/DBP
fit <- lme(fixed = mean_SBP_asleep ~ intervention, random = ~1|mstudyid, data = abpdata)
summary(fit) # not sig
fit <- lme(fixed = mean_DBP_asleep ~ intervention, random = ~1|mstudyid, data = abpdata)
summary(fit) # not sig

# non-dipping
library(lme4)
fit <- glmer(non_dip ~ intervention+(1|mstudyid), data=abpdata, family=binomial)
summary(fit)

# nocturnal hypertension
abpdata$noct_hyp <- ifelse(abpdata$mean_SBP_asleep >= 120 | abpdata$mean_DBP_asleep >=70, 1, 0)
fit <- glmer(noct_hyp ~ intervention+(1|mstudyid), data=abpdata, family=binomial)
summary(fit)

# by exposure mean #########
abpdata <- abpdata[!is.na(abpdata$mean_corr),] # 48
abpdata <- abpdata[abpdata$visually_valid == 1 & !is.na(abpdata$visually_valid),] # 36
fit <- lme(fixed = mean_24h_SBP ~ mean_corr, random = ~1|mstudyid, data = abpdata)
summary(fit) # not sig
fit <- lme(fixed = mean_24h_DBP ~ mean_corr, random = ~1|mstudyid, data = abpdata)
summary(fit) # not sig
# awake SBP/DBP
fit <- lme(fixed = mean_SBP_awake ~ mean_corr, random = ~1|mstudyid, data = abpdata)
summary(fit) # not sig
fit <- lme(fixed = mean_DBP_awake ~ mean_corr, random = ~1|mstudyid, data = abpdata)
summary(fit) # not sig
# asleep SBP/DBP
fit <- lme(fixed = mean_SBP_asleep ~ mean_corr, random = ~1|mstudyid, data = abpdata)
summary(fit) # not sig
fit <- lme(fixed = mean_DBP_asleep ~ mean_corr, random = ~1|mstudyid, data = abpdata)
summary(fit) # not sig

# non-dipping
library(lme4)
fit <- glmer(non_dip ~ mean_corr+(1|mstudyid), data=abpdata, family=binomial)
summary(fit)

# nocturnal hypertension
abpdata$noct_hyp <- ifelse(abpdata$mean_SBP_asleep >= 120 | abpdata$mean_DBP_asleep >=70, 1, 0)
fit <- glmer(noct_hyp ~ mean_corr+(1|mstudyid), data=abpdata, family=binomial)
summary(fit) # not sig



# Summary Statistics for home BP--------


homebp_short <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/homebp_short_Mar31.rds")

# time between ABP readings: 30.1 [+/- 12.3] days

days <- vector(mode = "numeric")
for (i in 1:length(unique(abpdata$mstudyid))) {
  data <- abpdata[abpdata$mstudyid == unique(abpdata$mstudyid)[i],]
  duration <- as.numeric(data$date_time[nrow(data)] - data$date_time[1], units = "days")
  days <- append(days, duration)
}
length(days) #27
days <- days[!days == 0]
length(days) #26
mean(days) # 30.1
sd(days) # 12.3



# Get minutewise CO data --------
Lascar_data <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/Lascar_data_cf_Jan29.rds") # 7152

Lascar_data <- Lascar_data[Lascar_data$file2 %in% abpdata$COfile,]

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
directory <- "/Users/ashlinn/Dropbox/Ghana project/BP project/ABP Project/Final Data/ABP_CO data/"
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
saveRDS(CO_stacked, file = paste0(directory, "CO_stacked_ABP",".rds"))

### ABP_CO Plots (works, but not all CO data is there and/or lines up) ----

ABP_matched <- ABP_stacked[ABP_stacked$file %in% abpdata$file[!is.na(abpdata$COfile)],]
ABP_matched_noerr <- ABP_stacked_noerr[ABP_stacked_noerr$file %in% abpdata$file[!is.na(abpdata$COfile)],]



pdf(file = paste0("ABP_CO_plots_good_", format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 10)
par(mfrow = c(3,3), mar = c(5,3,4,3))
# for (i in 1:length(unique(ABP_stacked$file))) {
#   data <- ABP_stacked[ABP_stacked$file == unique(ABP_stacked$file)[i],]

# just the matched ones
for (i in 1:length(unique(ABP_matched$file))) {
  data <- ABP_matched[ABP_matched$file == unique(ABP_matched$file)[i],]
  plot(data$date_time, data$SBP, pch = 16, ylim = c(20, 160), main = paste(data$mstudyid[1], data$visit[1]), xlab = paste("Time (", round(difftime(data$date_time[nrow(data)], data$date_time[1], units = "hours"), digits = 1), "hours,", nrow(data), "readings)"), ylab = "mmHg", xaxt = "n", yaxt = "n", type = "n")
  #data2 <- ABP_stacked_noerr[ABP_stacked_noerr$file == unique(ABP_stacked_noerr$file)[i],]
  data2 <- ABP_matched_noerr[ABP_matched_noerr$file == unique(ABP_matched_noerr$file)[i],]
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

names(abpdata)[1] <- "mstudyid"


## FIGURE FOR PAPER : BM1329 V1-------------
ABP_matched <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/ABP_stacked_noerr_matched_Nov27.rds")
CO_stacked <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/CO_stacked_ABP_Nov28.rds")

data <- ABP_matched[ABP_matched$mstudyid == "BM1329M" & ABP_matched$visit == "V1", ]
data2 <- CO_stacked[CO_stacked$mstudyid == "BM1329M" & CO_stacked$session == "s_01",]


pdf(file = paste0("ABP_CO_plot_example_", format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 10)
par(mfrow = c(1,1), mar = c(5,4,4,4))

  plot(data$date_time, data$SBP, pch = 16, ylim = c(20, 160), main = "24-hour session of ABP and CO Monitoring", xlab = "Time", ylab = "", xaxt = "n", yaxt = "n", type = "n")

  points(data$date_time, data$SBP, pch = 16, col = "black")
  lines(data$date_time, data$SBP)
  points(data$date_time, data$DBP, pch = 16, col = "blue")
  lines(data$date_time, data$DBP, col = "blue")
  
  lines(data2$datetime, (data2$co+20), col = "darkgreen", lwd = 2)
  
  # y axes
  axis(side = 2, at = seq(from = 20, to = 160, by = 20), cex.axis = 0.8)
  mtext("BP (mmHg)", side=2, line=2, cex=1)
  axis(side = 4, at = seq(from = 20, to = 160, by = 20), labels = seq(from = 0, to = 140, by = 20), col = "darkgreen", col.axis = "darkgreen", cex.axis = 0.8)
  mtext("Minute-Averaged CO (ppm)", side=4, line=2, cex=1, col="darkgreen")
  
  # x axis
  hours <- seq(from = ceiling_date(data$date_time[1], unit = "hour") -hours(1), to = floor_date(data$date_time[nrow(data)], unit = "hour") +hours(1), by = "hour")
  axis(1, at = hours, labels = format(hours, format = "%H:%M"))
  
  # add shading for night (21h- 5h)
  for (i in c(5:0, 24:21)) {
    night_start <-  
      ifelse(i %in% hour(hours), i, night_start)
  }
  for (i in c(21:23, 0:5)) {
    night_end <-  
      ifelse(i %in% hour(hours), i, night_end)
  }
  grid()
  rect(xleft = hours[hour(hours) == night_start], ybottom = 15, xright = hours[hour(hours) == night_end], ytop = 165, density = 20, angle = 45,col = "grey") 
  #legend("topright", legend = c("SBP", "DBP", "err"), pch = c(16, 16, 1), col = c("black", "blue", "red"), cex = 0.68)
   legend("topleft", legend = c("SBP", "DBP"), pch = c(16, 16, 1), col = c("black", "blue"), cex = 1) # without the errors 
   legend("topright", legend = "CO", lwd = 2, col = "darkgreen", cex = 1)

dev.off()


# Analysis

abpdata <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/abpdata_Nov17.rds")

homebp <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/homebp_Mar31.rds")

homebp_short <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/homebp_short_Sep09.rds")


# need to find upem data for all

COdata <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/ABP Project/Final Data/ABP_CO data/CO_stacked_ABP.rds")


# CHANGE IN BP --------

homebp_short$is_valid_diary <- NA
names(homebp_short)[8] <- "date_time_first"

abpchange <- reshape(abpdata[,c("mstudyid", "vcode", "stove_delivery", "vround", "date_time_first", "COfirstdate", "mean_24h_SBP", "mean_24h_DBP", "mean_24h_SBP_tw", "mean_24h_DBP_tw", "mean_SBP_awake", "mean_DBP_awake", "cf", "cf_conf", "mean_corr", "q90_corr", "q98_corr", "mean_first24_corr", "mean_first48_corr", "hours", "visually_valid", "duration_valid", "overall_valid", "permissible_co", "is_valid_diary", "is_valid_daytime")], idvar = "mstudyid", timevar = "vround", v.names = c("date_time_first", "COfirstdate", "mean_24h_SBP", "mean_24h_DBP", "mean_24h_SBP_tw", "mean_24h_DBP_tw", "mean_SBP_awake", "mean_DBP_awake", "cf", "cf_conf", "mean_corr", "q90_corr", "q98_corr", "mean_first24_corr", "mean_first48_corr", "hours", "visually_valid", "duration_valid", "overall_valid", "permissible_co", "is_valid_diary", "is_valid_daytime"), direction = "wide")
abpchange$SBP_change <- abpchange$mean_24h_SBP.2 - abpchange$mean_24h_SBP.1
abpchange$DBP_change <- abpchange$mean_24h_DBP.2 - abpchange$mean_24h_DBP.1

abpchange$SBP_change_tw <- abpchange$mean_24h_SBP_tw.2 - abpchange$mean_24h_SBP_tw.1
abpchange$DBP_change_tw <- abpchange$mean_24h_DBP_tw.2 - abpchange$mean_24h_DBP_tw.1

abpchange$SBP_change_awake <- abpchange$mean_SBP_awake.2 - abpchange$mean_SBP_awake.1
abpchange$DBP_change_awake <- abpchange$mean_DBP_awake.2 - abpchange$mean_DBP_awake.1
abpchange$mean_CO_change <- abpchange$mean_corr.2 - abpchange$mean_corr.1
abpchange$bp_type <- "ABP"
names(abpchange) <- tolower(names(abpchange))

homebpchange <- reshape(homebp_short[,c("mstudyid", "vcode", "stove_delivery", "vround", "date_time_first", "COfirstdate", "mean_sbp", "mean_dbp", "cf", "cf_conf", "mean_corr", "q90_corr", "q98_corr", "mean_first24_corr", "mean_first48_corr", "hours", "visually_valid", "duration_valid", "overall_valid", "permissible_co", "is_valid_diary")],  idvar = "mstudyid", timevar = "vround",  v.names = c("date_time_first", "COfirstdate", "mean_sbp", "mean_dbp", "cf", "cf_conf", "mean_corr", "q90_corr", "q98_corr", "mean_first24_corr", "mean_first48_corr", "hours", "visually_valid", "duration_valid", "overall_valid", "permissible_co", "is_valid_diary"), direction = "wide")
homebpchange$SBP_change <- homebpchange$mean_sbp.2 - homebpchange$mean_sbp.1
homebpchange$DBP_change <- homebpchange$mean_dbp.2 - homebpchange$mean_dbp.1
homebpchange$mean_CO_change <- homebpchange$mean_corr.2 - homebpchange$mean_corr.1
homebpchange$bp_type <- "homebp"

names(homebpchange)[6:7] <- c("mean_sbp_awake.1", "mean_dbp_awake.1")
names(homebpchange)[23:24] <- c("mean_sbp_awake.2", "mean_dbp_awake.2")
names(homebpchange)[38:39] <- c("sbp_change_awake", "dbp_change_awake")

homebpchange[, c("mean_24h_sbp.1","mean_24h_dbp.1", "mean_24h_sbp.2", "mean_24h_dbp.2", "sbp_change", "dbp_change",  "mean_24h_SBP_tw.1", "mean_24h_SBP_tw.2", "mean_24h_DBP_tw.1", "mean_24h_DBP_tw.2", "SBP_change_tw", "DBP_change_tw", "is_valid_daytime.1", "is_valid_daytime.2")] <- NA

names(homebpchange) <- tolower(names(homebpchange))

homebpchange <- homebpchange[,order(names(abpchange))]

BPchange <- rbind(abpchange, homebpchange)
BPchange$intervention <- ifelse(!is.na(BPchange$stove_delivery), 1, 0)

BPchange <- BPchange[order(BPchange$intervention, BPchange$mstudyid),]






# add village randomization
village_randomization <- read.dta("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Final_Data/Ghana_randomization.dta")
names(village_randomization)[1] <- "vcode"
random <- village_randomization[,c("vcode", "arm")]
BPchange <- merge(BPchange, random, by = "vcode")

# how long between pre- and post- readings?
BPchange$BP_interval_days <- as.numeric(round(BPchange$date_time_first.2 - BPchange$date_time_first.1, digits = 1))
BPchange$CO_interval_days <- as.numeric(round(difftime(BPchange$cofirstdate.2,BPchange$cofirstdate.1, unit = "days"),digits = 1))

saveRDS(BPchange, file = paste0("BPchange_", format(Sys.Date(), format = "%b%d"), ".rds"))


# Within-subject analysis ------
BPchange <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/BPchange_Nov17.rds")

# how many had valid sessions?
abpdata[, c("day_obs_diary", "is_valid_daytime")] # 52 out of 53

# using awake BP
BPchange$daytime_valid <- ifelse(BPchange$is_valid_daytime.1 ==1 & BPchange$is_valid_daytime.2 ==1, 1, 0)
BPchange$daytime_valid <- ifelse(BPchange$bp_type == "homebp" & !is.na(BPchange$sbp_change_awake), 1, BPchange$daytime_valid)

testdata <- BPchange[BPchange$daytime_valid ==1 & !is.na(BPchange$daytime_valid),] # 41 obs

table(testdata$bp_type)
table(testdata$intervention)

# is type of BP assessment associatd with BP change?
t.test(sbp_change_awake ~ as.factor(bp_type), data = testdata)
t.test(dbp_change_awake ~ as.factor(bp_type), data = testdata)




# # Time spans -------
# # using valid data only
# 
# 
# testdata <- filter(BPchange, (bp_type == "ABP" & is_valid_diary.1 ==1 &is_valid_diary.2 == 1) | (bp_type == "homebp" &!is.na(mean_sbp.2))) # 37 rows
# mean(testdata$BP_interval_days, na.rm = TRUE) #36
# t.test(BP_interval_days ~ bp_type, data = testdata) # sig different, p-val = 0.0004
# t.test(BP_interval_days ~ intervention, data = testdata) # p-val = 0.81
# 
# testdata <- filter(BPchange, (permissible_co.1 ==1 & permissible_co.2 == 1 & overall_valid.1 == 1 & overall_valid.2 ==1)) # 17 rows
# mean(testdata$CO_interval_days) #41.6
# t.test(CO_interval_days ~ bp_type, data = testdata) # p-val = 0.63
# t.test(CO_interval_days ~ intervention, data = testdata) # p-val = 0.99
# 
# 
# # time between BP and CO
# testdata <- filter(BPchange, (permissible_co.1 ==1 & overall_valid.1 ==1 & (is_valid_diary.1 ==1 | is.na(is_valid_diary.1)))) # 27 obs
# mean(testdata$date_time_first.1 - testdata$cofirstdate.1) #3549 mins/3600 = 0.96 days
# testdata <- filter(BPchange, (permissible_co.2 ==1 & overall_valid.2 ==1 & (is_valid_diary.2 ==1 | is.na(is_valid_diary.2)))) # 21 obs
# mean(testdata$date_time_first.2 - testdata$cofirstdate.2, na.rm = TRUE) # 3065/3600 = 0.85 days
# 
# 

# CO means by intervention arm --  with validity ---------
# 
# BPchange <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/BPchange_Nov17.rds")
# testdata <- BPchange[BPchange$overall_valid.1 == 1 &!is.na(BPchange$overall_valid.1) & BPchange$permissible_co.1 ==1,]
# nrow(testdata) #31
# table(testdata$intervention) # 0: 17, 1:14
# t.test(mean_corr.1 ~ intervention, data = testdata) # p-val = 0.105
# # mean in group 0 mean in group 1 
# # 1.01            1.48 
# 
# 
# fit <- lm(mean_corr.1 ~ intervention, data = testdata)
# summary(fit) # p-val = 0.09, coef = 0.46
# 
# testdata <- BPchange[BPchange$overall_valid.2 ==1 & !is.na(BPchange$overall_valid.2) & BPchange$permissible_co.2 ==1,]
# nrow(testdata) # 27
# table(testdata$intervention) #0: 13, 1:14
# t.test(mean_corr.2 ~ intervention, data = testdata) # p-val = 0.88 
# # mean in group 0 mean in group 1 
# # 0.92          0.87 
# fit <- aov(mean_corr.2 ~ arm, data = testdata) # anova across arms
# summary(fit) # p-val = 0.49
# 
# fit <- lm(mean_corr.2 ~ intervention, data = testdata)
# summary(fit) # p-val = 0.88, coef = -0.04

# is the CO change statistically different between groups? Yes
co_testdata <- BPchange[BPchange$overall_valid.1 == 1 & BPchange$overall_valid.2 == 1 &BPchange$permissible_co.1 ==1 & BPchange$permissible_co.2 ==1&!is.na(BPchange$overall_valid.1) & !is.na(BPchange$overall_valid.2),] # only uses 20 out of 44 observations

pdf(file = "CO_change_by_intervention.pdf", width = 6, height = 6, bg = "white")
boxplot((mean_corr.2 - mean_corr.1) ~ intervention, data = co_testdata, ylab = "Mean change in  72-hour CO (ppm)", names = c(paste0("Control (n= ", nrow(co_testdata[co_testdata$intervention ==0,]), ")"), paste0("Intervention (n= ", nrow(co_testdata[co_testdata$intervention ==1,]), ")")), main = "Change in mean 72-hour CO exposure \n by intervention status", col = "orange")
dev.off()

table(co_testdata$arm) # only 1 biolite
table(co_testdata$intervention)  # 10 and 10

table(testdata$bp_type)
t.test(mean_co_change ~ intervention, data = testdata) # p-val = 0.01
# mean in group 0 mean in group 1 
# -0.05283424          -1.14264466 
fit <- aov(mean_co_change ~ arm, data = testdata) 
summary(fit) # p-val = 0.028




# add covariates for adjusted analysis ---------
studyids <- unique(BPchange$mstudyid)
ANC <- read.csv("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Final_Data/Update_Nov20/ANC.csv", stringsAsFactors = FALSE)
ANC <- ANC[ANC$mstudyid %in% studyids,] # 45
ANC <- ANC[!duplicated(ANC),] # 44
for (i in 5:10) {
  ANC[,i] <- as.numeric(ANC[,i])
}
ANC$gestwks <- round((ANC$usgaw + ANC$usgad/7), digits = 2)
hist(ANC$wtkg)
hist(ANC$htcm)

ANC$BMI <- round(ANC$wtkg/(.01*ANC$htcm)^2, digits = 2)
boxplot(ANC$BMI) # one outlier, exactly 3.5 times the IQR away from the median. Keep?

drops <- c("Formtype", "vname")
ANC <- ANC[,!names(ANC) %in% drops]

BPchange <- merge(BPchange, ANC, by = "mstudyid", all.x = TRUE)

# add age
Econ <- read.csv("~/Dropbox/Ghana project/BP project/Baseline BP Paper/Final_Data/Update_Nov20/Econ.csv", stringsAsFactors=FALSE)
Econ <- Econ[Econ$mstudyid %in% studyids,] # 43
studyids[!studyids %in% Econ$mstudyid] # "BM1420M"

Econ <- Econ[,c("mstudyid", "age")]
BPchange <- merge(BPchange, Econ, by = "mstudyid", all.x = TRUE)
BPchange$age[BPchange$mstudyid == "BM1420M"] <- 37 # from Ken email 9/11/15

# add stress
# 82 rows in stress data? was administered twice.
# what to do with discordant rows?
# important re: INTERHEART: 
stress <- read.csv("/Users/ashlinn/Dropbox/Ghana project/BP project/ABP Project/Final Data/Update March 29/stress.csv", stringsAsFactors = FALSE)
stress <- stress[,c(1, 3:ncol(stress))]
names(stress)[1] <- "mstudyid"
for (i in c(4:12, 14)) {
  stress[,i] <- mapvalues(stress[,i], from = c(1,2), to = c(1,0))
}

stress <- stress[!duplicated(stress),] # 58 obs
dups <- stress$mstudyid[which(duplicated(stress$mstudyid))]
length(dups) # 14 who responded differently twice
for (i in 1:length(dups)){
  print(dups[i])
  print(row.names(stress[stress$mstudyid == dups[i], ]))
  print(stress[stress$mstudyid == dups[i], ])
}

# choose the most stress reported?
# remove row.names: 2, 3,6,10,16,21,33,37,43,45,47,57,68,80

stress <- stress[!row.names(stress) %in% c(2, 3,6,10,16,21,33,37,43,45,47,57,68,80),]
length(unique(stress$mstudyid)) #44

stress$stress_events <- rowSums(stress[,4:11])
stress <- stress[, c(1:3, 15, 12:14)]

BPchange <- merge(BPchange, stress, by = "mstudyid", all.x = TRUE)
saveRDS(BPchange, file = paste0("BPchange_", format(Sys.Date(), format = "%b%d"), ".rds"))


# WITHIN-SUBJECT FINAL Analysis -------
# This hypothesis will be analyzed via mixed methods linear regression, comparing the pre-intervention to post-intervention change in within-subject BP between the two intervention arms (LPG and control). Using this method takes advantage of the repeated measures on each subject, and the randomization of the study (ideally) controls for other individual-level factors such as BMI, maternal age, and SES. The known downward trend in pregnancy BP during the second trimester is controlled for by the fact that the outcome of interest is the difference in the groupwise average change from BP reading 1 to BP reading 2 between the two arms of the study.
BPchange <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/BPchange_Nov18.rds")

testdata <- BPchange[BPchange$daytime_valid ==1 & !is.na(BPchange$daytime_valid),] # 41 obs


# boxplot of BP

par(mfrow = c(2,2))
boxplot(testdata$mean_sbp_awake.1, col = "lightblue", main = "SBP 1")
boxplot(testdata$mean_sbp_awake.2, col = "lightblue", main = "SBP 2")
boxplot(testdata$mean_dbp_awake.1, col = "coral", main = "DBP 1")
boxplot(testdata$mean_dbp_awake.2, col = "coral", main = "DBP 2")

par(mfrow = c(1,2))
boxplot(sbp_change_awake ~ intervention, data = testdata, col = "grey", main = "SBP change")
boxplot(dbp_change_awake ~ intervention, data = testdata, col = "grey", main = "DBP change")


# Histogram of gestational age --------
par(mfrow = c(1,1))
pdf(file = "ABP_Pilot_Gestage.pdf", width = 10, height = 8, bg = "white")
hist(BPchange$gestwks, breaks = 10, xlim = c(0,40), main = paste0("Gestational Age at Enrollment (n = ", nrow(testdata), ")"), xlab = "Weeks of Gestation", col = "aquamarine")
abline(v = mean(BPchange$gestwks), col = "red", lwd = 3)
legend("topright", legend = paste0("mean (", round(mean(BPchange$gestwks), digits = 1), " weeks)"), col = "red", lwd = 3)
dev.off()

# Spaghetti plot of BP----------
pdf("BP_spaghetti_plot.pdf")
par(mfrow = c(1,2), mar = c(1,4,4,3), oma = c(4,0,0,0))
# ABP
abp_testdata <- abpdata[abpdata$is_valid_daytime == 1 &!abpdata$mstudyid %in% c("BM1269M", "BM1335M"),] # the two with missing v2
nrow(abp_testdata) # 50
interaction.plot(abp_testdata$vround,abp_testdata$mstudyid, abp_testdata$mean_SBP_awake,xlab="time", ylab="SBP", col=rainbow(n = 27), legend=F, lwd = 2, ylim = c(80, 130), main = "ABPM")

interaction.plot(abp_testdata$vround,abp_testdata$mstudyid, abp_testdata$mean_DBP_awake,xlab="time", ylab="DBP", col=rainbow(n=27), legend=F, lwd = 2, ylim = c(50, 90), main = "ABPM") 
mtext("Session", side = 1, line = 1.5, outer = TRUE)

# home BP
interaction.plot(homebp_short$vround,homebp_short$mstudyid, homebp_short$mean_sbp,xlab="time", ylab="SBP", col=rainbow(n = 17), legend=F, lwd = 2, ylim = c(80, 130), main = "HBPM") 
interaction.plot(homebp_short$vround,homebp_short$mstudyid, homebp_short$mean_dbp,xlab="time", ylab="DBP", col=rainbow(n=17), legend=F, lwd = 2, ylim = c(50,90), main = "HBPM") 

mtext("Session", side = 1, line = 1.5, outer = TRUE)
dev.off()

# Spaghetti plot of CO ------
abp_testdata2 <- abp_testdata[, c( "mstudyid", "vcode", "vround", "mean_corr", "arm")]
homebp_testdata2 <- homebp_short[, c("mstudyid", "vcode", "vround", "mean_corr")]
homebp_testdata2 <- merge(homebp_testdata2, random, by = "vcode")
abp_testdata2 <- abp_testdata2[ ,order(names(homebp_testdata2))]

co_testdata2 <- rbind(abp_testdata2, homebp_testdata2)
co_testdata2$intervention <- ifelse(co_testdata2$arm > 1, 1, 0)
co_testdata2 <- co_testdata2[co_testdata2$mstudyid %in% co_testdata$mstudyid,]

# Figure 2 Change in CO by intervention status --------
pdf("CO_change_by_intervention_spaghetti.pdf")
par(mfrow = c(1,2), oma = c(2,0,2,0), mar = c(4,4,4,1))
co_testdata3 <- co_testdata2[co_testdata2$intervention == 0,]
interaction.plot(co_testdata3$vround,co_testdata3$mstudyid, co_testdata3$mean_corr, ylab="Mean 72-hr CO (ppm)", col=rainbow(n = nrow(co_testdata3)/2), legend=F, lwd = 2, main = paste0("Control \n (n = ", nrow(co_testdata3)/2, ")"), xlab = "", ylim = c(0,4))

co_testdata4 <- co_testdata2[co_testdata2$intervention == 1,]
interaction.plot(co_testdata4$vround,co_testdata4$mstudyid, co_testdata4$mean_corr, ylab="", col=rainbow(n = nrow(co_testdata4)/2), legend=F, lwd = 2, main = paste0("Intervention \n (n = ", nrow(co_testdata4)/2, ")"), xlab = "", ylim = c(0,4))

title(main = "Change in CO by Intervention Status", outer = TRUE)
mtext(side = 1, line = 0, text = "Session", outer = TRUE)
dev.off()



# Boxplot of change ---------

par(oma = c(0,0,3,0))
# by bp type
par(mfrow = c(1,2))
boxplot(sbp_change_awake ~ as.factor(bp_type), data = testdata, ylab = "SBP Change", col = "orange", main = "SBP")
boxplot(dbp_change_awake ~ as.factor(bp_type), data = testdata, ylab = "DBP Change", col = "purple2", main = "DBP")

## FIGURE 3 BP CHANGE BY INTERVENTION STATUS -------------
# by intervention
pdf(file = "BP_change_by_intervention.pdf" )
par(oma = c(0,0,3,0))
par(mfrow = c(1,2))
boxplot(sbp_change_awake ~ intervention, data = testdata, ylab = "SBP Change", col = "orange", main = "SBP", ylim = c(-20,20), xaxt = "n")
axis(1, at = 1:2, labels = c(paste0("Control \n (n= ", nrow(testdata[testdata$intervention ==0,]), ")"), paste0("Intervention \n (n = ", nrow(testdata[testdata$intervention ==1,]), ")")), tick = FALSE)
boxplot(dbp_change_awake ~ intervention, data = testdata, ylab = "DBP Change",  names = c("Control", "Intervention"), col = "purple2", main = "DBP", ylim = c(-15, 15), xaxt = "n")
axis(1, at = 1:2, labels = c(paste0("Control \n (n= ", nrow(testdata[testdata$intervention ==0,]), ")"), paste0("Intervention \n (n = ", nrow(testdata[testdata$intervention ==1,]), ")")), tick = FALSE)
# title("Intervention includes LPG and BioLite", outer = TRUE)

#only LPG versus control
testdata2 <- testdata[testdata$arm ==1 |testdata$arm ==3,] # 36 obs
boxplot(sbp_change_awake ~ intervention, data = testdata2, ylab = "SBP Change",col = "orange", main = "SBP", ylim = c(-20,20), xaxt = "n")
axis(1, at = 1:2, labels = c(paste0("Control \n (n= ", nrow(testdata2[testdata2$intervention ==0,]), ")"), paste0("Intervention \n (n = ", nrow(testdata2[testdata2$intervention ==1,]), ")")), tick = FALSE)
boxplot(dbp_change_awake ~ intervention, data = testdata2, ylab = "DBP Change",  col = "purple2", main = "DBP", ylim = c(-15,15), xaxt = "n")
axis(1, at = 1:2, labels = c(paste0("Control \n (n= ", nrow(testdata2[testdata2$intervention ==0,]), ")"), paste0("Intervention \n (n = ", nrow(testdata2[testdata2$intervention ==1,]), ")")), tick = FALSE)

title("Intervention Restricted to LPG", outer = TRUE)

dev.off()

# BPchange: crude results --------
# using t-test
table(testdata$bp_type) # ABP: 25, homebp: 16
table(testdata$intervention) # 0: 20, 1:21
# crude results (not adjusted for anything)
# ddply(testdata[,c("intervention", "sbp_change_awake", "dbp_change_awake")], "intervention", colwise(mean))
t.test(sbp_change_awake ~ intervention, data = testdata)
# mean in group 0 mean in group 1 
# 2.5          -0.98
# p-val = 0.109
t.test(dbp_change_awake ~ intervention, data = testdata)
# mean in group 0 mean in group 1 
# 1.45          -0.09
# p-val = 0.32

t.test(sbp_change_awake ~ bp_type, data = testdata) # not sig 
t.test(dbp_change_awake ~ bp_type, data = testdata) # not sig 
 



# adjusted -------
# Balance between control and intervention
nrow(testdata) #41

library(psych)
table(testdata[testdata$bp_type == "ABP", "arm"]) # 12 control, 5 Biolite, 8 LPG
table(testdata[testdata$bp_type == "homebp", "arm"]) # 8 control, 8 LPG
describeBy(testdata[,c("bp_type2", "age", "BMI", "gestwks", "stresslevel", "stress_events", "depressed2")], group = testdata$intervention)

testdata$bp_type2 <- mapvalues(testdata$bp_type, from = c("homebp", "ABP"), to = c(0,1))
testdata$bp_type2 <- as.numeric(testdata$bp_type2)

# Table S1 ----------
balance <- data.frame()
for (i in c("age", "gestwks", "BMI", "stresslevel", "stress_events", "depressed2", "bp_type2")) {
  print(i)
  p <- t.test(testdata[testdata$intervention == 0, i], testdata[testdata$intervention == 1,i])
  balance[i, 1] <- i
  balance[i, 2] <- round(p$estimate[1], digits = 1)
  balance[i,3] <- round(p$estimate[2], digits = 1)
  balance[i,4] <- round(p$p.value, digits = 2)
}
colnames(balance) <- c("variable", "ctrl_group_mean", "interv_group_mean", "t.test_pval")
balance
# ones with p-val < 0.2: age (0.06), gestwks (0.02).
write.csv(balance, file = "ABP_variable_balance.csv", row.names = FALSE)

# plot of balance of variables with intervention status
library(sm)
library(gridExtra)
testdata$intervention.f <- factor(testdata$intervention, levels= c(0,1), labels = c("Control", "Intervention"))
pdf(file = "Variable_Balance_by_intervention.pdf")
par(mfrow = c(2,2), mar = c(4,4,4,2))
for (i in c("age", "gestwks", "BMI", "stresslevel", "stress_events", "depressed2", "bp_type2")) {
  p <- t.test(testdata[testdata$intervention == 0, i], testdata[testdata$intervention == 1,i])
assign(paste("plot", i), {sm.density.compare(testdata[,i], testdata$intervention.f, col = c("green", "red"), xlab = "")
                          title(main = paste(i, "\n p-value = ", round(p$p.value, digits = 2)))
                          legend("topleft", levels(testdata$intervention.f), col = c("green", "red"), lwd = 2, cex = 0.5, bty = "n")
})
  
}
dev.off()

# which are associated with outcome?
for (i in c("age", "gestwks", "BMI", "stresslevel", "stress_events", "depressed2", "bp_type2")) {
  model <- lm(testdata$sbp_change_awake ~ testdata[,i])
  print(i)
  print(summary(model)$coefficients[2,4])
} # only gestational age is associated with the outcome at p < 0.2

# which are associated with exposure?
for (i in c("age", "gestwks", "BMI", "stresslevel", "stress_events", "depressed2", "bp_type2")) {
  print(i)
  model <- lm(testdata$mean_corr.1[!is.na(testdata$mean_corr.2)] ~ testdata[!is.na(testdata$mean_corr.2),i])
  print(summary(model))
}
# none at 0.2 for mean_corr.1 or mean_corr.2
# should combine them

# loess plots of predictors vs outcome
plot(testdata$gestwks, testdata$sbp_change_awake)
abline(lm(sbp_change_awake ~gestwks, data = testdata), col="red") # regression line (y~x) 
lines(lowess(testdata$gestwks, testdata$sbp_change_awake), col="blue") # lowess line (x,y)

plot(testdata$gestwks, testdata$dbp_change_awake)
abline(lm(dbp_change_awake ~gestwks, data = testdata), col="red") # regression line (y~x) 
lines(lowess(testdata$gestwks, testdata$dbp_change_awake), col="blue") # lowess line (x,y)


# unadjusted models ---------
testdata_complete <- testdata[!is.na(testdata$sbp_change_awake),]# 41 obs
length(unique(testdata_complete$mstudyid))

fm <- lm(sbp_change_awake ~ intervention, data = testdata_complete)
summary(fm) # intervention coef -3.5, not sig [-7.7, 0.75]
confint(fm)


fm <- lm(dbp_change_awake ~ intervention, data = testdata)
summary(fm) # intervention coef -1.5, not sig [-4.6, 1.5]
confint(fm)

# Final adjusted models -----------
# adjust for gestwks as it could cause difference in outcome and is associated with intervention condition. Don't adjust for stress, age, or BMI as they wouldn't be expected to cause difference in change in BP (and they don't). Adjust for BP type as precision variable even though it is not associated with intervention condition.

## CONFIDENCE INTERVALS-----
# 95% CI: coef +/- SE*t^alpha/2,n-k-1
# unadjusted: n = 41, k = 1
# t^0.025,39 = 2.02
# adjusted: n = 41, k = 3
# t^0.025,37 = 2.03 (same)

fm <- lm(sbp_change_awake ~ intervention + gestwks + bp_type, data = testdata)
summary(fm)
# coef = -2.1, p-val = 0.35
# 95% CI: -2.1 +/- 2.23*2.02 [-6.6, 2.4]

fm <- lm(dbp_change_awake ~ intervention + gestwks + bp_type, data = testdata)
summary(fm)
# coef = -0.1, p=val = 0.95
# 95% CI: -0.1 +/- 1.55 *2.02 [-3.23, 3.03]





# Sensistivity analysis: only LPG and control ---------
testdata2 <- testdata[testdata$arm == 1 | testdata$arm ==3,] # 36 obs
table(testdata2$intervention, testdata2$bp_type) # slightly unbalanced
# change in CO

codata <- filter(BPchange,(arm ==1 | arm ==3) & (permissible_co.1 == 1 & permissible_co.2 ==1) & (overall_valid.1 ==1 & overall_valid.2 ==1)) # 17 obs, same as above
t.test(mean_co_change ~ intervention, data = codata)
table(codata$intervention)

# crude - using t-test
t.test(sbp_change_awake ~ intervention, data = testdata2)
# mean in group 0 mean in group 1 
# 2.503935       -1.283022 
# p-val = 0.08

t.test(dbp_change_awake ~ intervention, data = testdata2)
# mean in group 0 mean in group 1 
# 1.4549333      -0.6712369 
# p-val = 0.18

# adjusted
fm <- lm(sbp_change_awake ~ intervention + gestwks + bp_type, data = testdata2)
summary(fm) 
# coef = -2.5, p-val = 0.33
 #95% CI: -2.5 +/- 2.5*2.03 [-7.58, 2.58]

fm <- lm(dbp_change_awake ~ intervention + gestwks + bp_type, data = testdata2)
summary(fm)
# coef = -0.8, p-val = 0.65
#95% CI: -0.8 +/- 1.79*2.03 [-4.43, 2.83]


# # using multilevel controlling for community (very similar results)
# library(nlme)
# fm <- lme(fixed = sbp_change_awake ~ intervention  + age + gestwks , random = ~ intervention | vcode, data = testdata2)
# summary(fm) # 
# fm <- lme(fixed = dbp_change_awake ~ intervention  + age + gestwks , random = ~ intervention | vcode, data = testdata2)
# summary(fm) # 


# Association with CO?---
fit <- lm(mean_dbp.2 ~ mean_corr.1 + mean_corr.2, data = BPchange)
testdata <- filter(BPchange,overall_valid.1 == 1 & overall_valid.2 == 1 &!is.na(overall_valid.1) &!is.na(overall_valid.2)) # 17 obs
testdata <- filter(testdata, (is_valid_diary.1 ==1 & is_valid_diary.2 ==1)| is.na(is_valid_diary.1))  # 15 obs
fit <- lm(mean_sbp.1 ~ intervention + bp_type + age + BMI + gestwks + stresslevel + stress_events + mean_corr.1, data = testdata)
fit <- lm(mean_sbp.2 ~ intervention + bp_type + age + BMI + gestwks + stresslevel + stress_events + mean_corr.1 + mean_corr.2 + mean_sbp.1, data = testdata)
fit <- lm(mean_dbp.1 ~ intervention + bp_type + age + BMI + gestwks + stresslevel + stress_events + mean_corr.1, data = testdata)
fit <- lm(mean_dbp.2 ~ intervention + bp_type + age + BMI + gestwks + stresslevel + stress_events + mean_corr.1 + mean_corr.2 + mean_dbp.1, data = testdata)
summary(fit)

# Table 1 ------

stat.desc(BPchange[BPchange$bp_type  == "ABP", c("intervention", "age", "BMI", "gestwks", "stresslevel", "stress_events", "depressed2")])
table(BPchange[BPchange$bp_type == "ABP", "arm"])
stat.desc(BPchange[BPchange$bp_type  == "homebp", c("intervention", "age", "BMI", "gestwks", "stresslevel", "stress_events", "depressed2")])
table(BPchange[BPchange$bp_type == "homebp", "arm"])

for (i in c("intervention", "age", "gestwks", "BMI", "stresslevel", "stress_events", "depressed2")) {
  print(i)
  print(t.test(BPchange[BPchange$bp_type == "ABP", i], BPchange[BPchange$bp_type == "homebp",i]))
}


# Table 2 -------
for (i in c("mean_SBP_awake", "mean_DBP_awake", "mean_SBP_asleep", "mean_DBP_asleep", "mean_SBP_morning", "nighttime_trough_SBP")){
  print(t.test(abpdata_valid[,"vround"], abpdata_valid[,i]))
}

df <- abpdata_valid[, c("vround", "mean_24h_SBP", "mean_24h_DBP", "mean_SBP_awake", "mean_DBP_awake", "mean_SBP_asleep", "mean_DBP_asleep", "mean_SBP_morning", "nighttime_trough_SBP")]
ddply(df, .(vround), colwise(mean))
ddply(df, .(vround), colwise(sd))
apply(df,2, mean)
apply(df, 2,sd)

# Are means diff by bp type? ----------
abpdata_valid <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/abpdata_valid_Nov17.rds")
abpdata <- abpdata_valid

homebp_short <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/homebp_short_Mar31.rds")
home <- homebp_short[, c("mstudyid", "mean_sbp", "mean_dbp")]
home$bp_type <- "homebp"
abp <- abpdata[, c("mstudyid", "mean_24h_SBP", "mean_24h_DBP")]
abp$bp_type <- "abp"
names(home) <- c("mstudyid", "SBP", "DBP", "bp_type")
names(abp) <- c("mstudyid", "SBP", "DBP", "bp_type")
all <- rbind(home, abp)

all <- merge(all, BPchange[, c("mstudyid", "BMI", "gestwks", "age")], by = "mstudyid", all.x = TRUE)
fit <- lme(fixed = SBP ~ bp_type, random = ~1|mstudyid, data = all)
summary(fit) # sig, p-val = 0.006
fit <- lme(fixed = SBP ~ bp_type + BMI + gestwks + age, random = ~1|mstudyid, data = all)
summary(fit) # sig, p-val = 0.006
fit <- lme(fixed = DBP ~ bp_type, random = ~1|mstudyid, data = all)
summary(fit) # sig p-val = 0.02
fit <- lme(fixed = DBP ~ bp_type + BMI + gestwks + age, random = ~1|mstudyid, data = all)
summary(fit) # not sig p-val = 0.06 
