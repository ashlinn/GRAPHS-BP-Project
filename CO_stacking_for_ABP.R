# Getting all ABP CO data including non-validated--------

# I DO NOT NEED TO DO THIS SINCE THESE NON-VALIDATED ONES ARE NON-SYNCHRONOUS WITH ABP

CO_stacked <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/CO_all_Nov27.rds")
CO_stacked$file2 <- basename(CO_stacked$file)

CO_params <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/CO_parameters_7152sessions_Feb03.rds")

CO_params <- CO_params[CO_params$file %in% CO_stacked$file2,]
CO_params <- CO_params[,c("file", "visually_valid", "visual_notes", "duration_valid", "overall_valid")]

CO_stacked$file3 <- CO_stacked$file
CO_stacked$file <- CO_stacked$file2
CO_stacked <- merge(CO_stacked, CO_params, by = "file", all.x = TRUE)

CO_stacked$unique_hour <- floor_date(CO_stacked$datetime, unit = "hour")

# make a vector of unique 30 minute periods (using floor to determine period)
CO_stacked$unique_30 <- format(strptime(CO_stacked$datetime, "%Y-%m-%d", tz="UTC") + floor(as.numeric(CO_stacked$datetime)/1800)*1800,"%H:%M")

CO_stacked$unique_30 <- ymd_hm(paste(format(strptime(CO_stacked$unique_hour, "%Y-%m-%d", tz="UTC")), CO_stacked$unique_30))

CO_stacked <- arrange(CO_stacked, unique_visit, datetime)

# some have validity not 1
length(unique(CO_stacked$unique_visit[CO_stacked$overall_valid > 1])) # 5
bad_data <- CO_stacked[CO_stacked$overall_valid > 1,]
for (i in 1:length(unique(bad_data$unique_visit))) {
 
  data <- bad_data[bad_data$unique_visit == unique(bad_data$unique_visit)[i],]
  print(paste ("######################", data$unique_visit[1], i))
  print(data[1,])
  print(plot(data$datetime, data$co_corr, type = "l", main = data$unique_visit[1]))
}
# BM1255M_V1: lo conf
# BM1263M_V1: duration 2 but this section is ok
# BM_1287M_V2: lo conf
# BM1300M_V1: lo conf
# BM1413_V1: "a lot of flatline begins" but looks ok on this section

# these plots LOOK ok so maybe for the purposes of analysis keep them in? Can do sensitivity later without them

saveRDS(CO_stacked, file = paste0("CO_stacked_ABP_", format(Sys.Date(), format = "%b%d"), ".rds"))




# # five were not yet validated: BM1413, BM1416, BM1417, BM1418, BM1424
# CO_parameters_missingfiles <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/ABP Project/CO_parameters_ABP_5sessions_Nov13.rds")
# 
# # process these
# files <- CO_parameters_missingfiles$file
# # make a data frame of the files
# 
# #### OK TO HERE #####
# Lascar_data <- CO_parameters_missingfiles[, c("file", "cf", "cf_conf")]
# 
# # Lascar.import function -------
# lascar.import <- function(file,cf, cf_conf) { 
#   dt <- read.csv(file, stringsAsFactors=F, header=T)[,1:5]
#   dt$lascar <- names(dt)[1]
#   dt$lascar <- gsub("C0", "CO", dt$lascar)
#   dt$lascar <- gsub("-", "_", dt$lascar)
#   dt$SN<- dt$Serial.Number[1]
#   dt$datetime <- dmy_hms(dt$Time, tz="GMT")
#   dt$rd.datetime <- as.character(round(dt$datetime, 'min'))
#   dt<-dt %.% group_by(rd.datetime) %.% dplyr::summarise(mean(CO.ppm.), lascar[1], SN[1])
#   names(dt) <- c('datetime','co', 'lascar', 'SN')
#   dt$datetime <- ymd_hms(dt$datetime)
#   dt$cf <- cf
#   dt$cf_conf <- ifelse(!is.na(dt$cf[1]), cf_conf, "lo") # if CF is NA, set as low
#   cfgood <- !is.na(dt$cf[1]) & dt$cf[1] >=0.2
#   if(cfgood == TRUE) dt$co_corr <- dt$co/dt$cf
#   if(cfgood == FALSE) dt$co_corr <- dt$co/0.85 # if CF is NA or < 0.2,  adjust with the mean
#   dt
# }
# 
# 
# #################### START LOOP HERE if working from raw Lascar files ###################
# # If working from raw files be sure to comment out section at "START HERE IF WORKING FROM SAVED .RDS DATA
# ## NOTE: THE FOLLOWING STEPS FROM THE RAW DATA TAKE A LONG TIME!
# 
# 
# ##################################
# # set a directory for the saved data
# directory <- "/Users/ashlinn/Dropbox/Ghana project/BP project/ABP Project/"
# ##################################
# 
# 
# ptm <- proc.time()
# 
# # for (i in 1:length(unique(Lascar_data$SN))) { #1:length(unique(Lascar_data$SN)))
# #  files_bySN <- Lascar_data[Lascar_data$SN == unique(Lascar_data$SN)[i], c("file", "cf", "cf_conf")]
#   
#   CO_stacked <- mdply(Lascar_data, lascar.import, .progress = "text") # mdply so can supply multiple arguments to lascar.import. Variable names of files_bySN must match those in lascar.import (file, cf, cf_conf)
#   
#   CO_stacked <- CO_stacked[!is.na(CO_stacked$co),] # removing NAs
#   
#   
#   # grab mother and child id info
#   id_pattern <- "BM....."
#   CO_stacked$mstudyid <- regmatches(CO_stacked$file, regexpr(id_pattern, CO_stacked$file))
#   
#   child_pattern <- "BM....C"
#   CO_stacked$cstudyid <- regexpr(child_pattern, CO_stacked$file)
#   CO_stacked$cstudyid <- ifelse(CO_stacked$cstudyid == -1, NA, substr(x = CO_stacked$file, start = CO_stacked$cstudyid, stop = CO_stacked$cstudyid + 6))
#   
#   # grab session info
#   # grab session info: do in multipe steps to deal with bad value propagation after NA
#   session_pattern <- "(s_[0123456789]{1,2}|s[0123456789]{1,2})"
#   CO_stacked$session <- regmatches(CO_stacked$file, regexpr(session_pattern, CO_stacked$file, ignore.case =TRUE))
#   CO_stacked$session <- tolower(CO_stacked$session)
#   
#   # order from most recent to oldest
#   CO_stacked <- CO_stacked[order(CO_stacked$datetime),]
#   
#   # simplify Lascar names
#   CO_stacked$lascar <- gsub("\\.", "_", CO_stacked$lascar)
#   
#   
#   # separate and save the data 
#   saveRDS(CO_stacked, file = "CO_stacked_5missing.rds")
#   
#   
# # Merge with previous
# CO_stacked <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/CO_stacked_ABP_Nov27.rds") 
# missing <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/CO_stacked_5missing.rds")
# 
# 
# missing$unique_hour <- floor_date(missing$datetime, unit = "hour")
# missing <- arrange(missing, mstudyid, datetime) 
# missing$unique_visit <- paste(missing$mstudyid, "V2", sep = "_")
# missing$unique_30 <- format(strptime(missing$datetime, "%Y-%m-%d", tz="UTC") + floor(as.numeric(missing$datetime)/1800)*1800,"%H:%M")
# missing$overall_valid <- 1
# 
# CO_stacked <- subset(CO_stacked, select = -c(visit, visually_valid, visual_notes, duration_valid))
# 
# CO_stacked <- rbind(CO_stacked, missing)
# saveRDS(CO_stacked, file = paste0("CO_stacked_ABP_all_", format(Sys.Date(), format = "%b%d"), ".rds"))

# 
########################## START HERE IF WORKING FROM SAVED .RDS DATA ######################
#     #  If working from saved .rds data START HERE and run this section thru first print[i], otherwise comment it out along with the print[i] before the loop closure------
#     
#
abpdata <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/abpdata_Nov17.rds") # summary data

abpdata <- abpdata[abpdata$overlap_ok ==1,]
unique(abpdata$sn)[order(unique(abpdata$sn))]

# copy these processed files into local, then proceed
savedfiles <- list.files("/Users/ashlinn/Dropbox/Ghana project/BP project/ABP Project/CO data/", full.names = TRUE)
 length(savedfiles) #21
CO_all <- data.frame()
  for (i in 1:length(savedfiles)) {
CO_bySN <- readRDS(savedfiles[i])
CO_bySN$unique_visit <- paste0(CO_bySN$mstudyid, "_V", substr(CO_bySN$session, 4,4))
CO_all <- rbind(CO_all, CO_bySN)
}

CO_all <- arrange(CO_all, unique_visit, datetime) # ascending by date
CO_all <- CO_all[CO_all$unique_visit %in% abpdata$unique_visit,]
saveRDS(CO_all, file = paste0("CO_all_", format(Sys.Date(), format = "%b%d"), ".rds"))

#########################