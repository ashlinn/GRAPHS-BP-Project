require(plyr)
require(dplyr)
require(reshape2)
require(lubridate)
require(foreach)

# functions ---------------------

NAPerVariable <- function(X1) {
  D1 <- is.na(X1)
  colSums(D1)
}

lm_result <- function(model) {
  result <- as.data.frame(round(summary(model)$coefficients[,1], digits = 3))
  colnames(result)[1] <- "coefficient"
  result$SE <- round(summary(model)$coefficients[,2], digits = 3)
  result$p_value <- round(summary(model)$coefficients[,4], digits = 3)
  result$rsq[1] <- round(summary(model)$adj.r.squared, digits = 3)
  result
}

##########################################################
############## DATA GATHERING AND CLEANING ---------------
##########################################################


# Loading Spreadsheet Data ------------------------------------------------


ANC <- read.csv("~/Dropbox/Ghana project/BP project/Baseline BP Paper/Final_Data/Update_Nov20/ANC.csv", stringsAsFactors=FALSE)

Enrollment <- read.csv("~/Dropbox/Ghana project/BP project/Baseline BP Paper/Final_Data/Update_Nov20/Enrollment.csv", stringsAsFactors=FALSE) # this has the medical history variables
Enrollment2 <- read.csv("~/Dropbox/Ghana project/BP project/Baseline BP Paper/Final_Data/Update_Nov18/Enrollment_Nov18.csv", stringsAsFactors = FALSE) # this has the smoking variables
names(Enrollment) <- tolower(names(Enrollment))
Enrollment <- merge(Enrollment2[,c(1:2, 4:13)], Enrollment [, c(2, 5:33)], by = "mstudyid", all.x = TRUE)

Econ <- read.csv("~/Dropbox/Ghana project/BP project/Baseline BP Paper/Final_Data/Update_Nov20/Econ.csv", stringsAsFactors=FALSE) ## July 10 version for studyid
Cooking <- read.csv("~/Dropbox/Ghana project/BP project/Baseline BP Paper/Final_Data/Update_Nov20/Cooking.csv", stringsAsFactors=FALSE)
DEM <- read.csv("~/Dropbox/Ghana project/BP project/Baseline BP Paper/Final_Data/Update_Nov20/DEM.csv", stringsAsFactors=FALSE)
Lascar <- read.csv("~/Dropbox/Ghana project/BP project/Baseline BP Paper/Final_Data/Update_Nov20/Lascar.csv", stringsAsFactors=FALSE)

# subsetting only the baseline observations
DEM_baseline <- DEM[DEM$vround ==1,]
# Lascar_baseline <- Lascar[Lascar$viround ==1,]

# getting rid of duplicate rows
Econ <- Econ[!duplicated(Econ),] #1239 rows
Cooking <- Cooking[!duplicated(Cooking),] #1217
ANC <- ANC[!duplicated(ANC),] #1219
Enrollment <- Enrollment[!duplicated(Enrollment),] #1260
DEM_baseline <- DEM_baseline[!duplicated(DEM_baseline),]
Lascar_baseline <- Lascar_baseline[!duplicated(Lascar_baseline),] # 1236

DEM <- DEM_baseline
Lascar <- Lascar_baseline
rm("DEM_baseline", "Lascar_baseline")

# there are still some duplicated rows in ANC: 2
duplicatedid <- which(duplicated(ANC$mstudyid))
duplicatednames <- ANC$mstudyid[duplicatedid]
duplicatednames #  BM0556M, BM0698M
ANCduplicates <- ANC[ANC$mstudyid %in% duplicatednames,]
ANC <- ANC[!ANC$mstudyid %in% duplicatednames,] #1215
ANC <- ANC[-1,] #1214

# matching all to the study IDs in ANC
StudyIDs <- unique(ANC$mstudyid) #929/ Jul 18: 996/ Dec 30: 1214
Enrollment <- Enrollment[Enrollment$mstudyid %in% StudyIDs,] #929 rows/ Jul 18: 993/ Dec 30: 1214
Econ <- Econ[Econ$mstudyid %in% StudyIDs,] #922 rows/ Jul 18: 984 / Dec. 30: 1209
Cooking <- Cooking[Cooking$mstudyid %in% StudyIDs,] # 913 rows/ Jul 18: 963/ Dec 30: 1197
DEM <- DEM[DEM$mstudyid %in% StudyIDs,]
Lascar <- Lascar[Lascar$mstudyid %in% StudyIDs,] #928/ Dec 30: 1211

# Looking for duplicates
# Enrollment: 0 duplicates
Enrollmentdups <- Enrollment$mstudyid[which(duplicated(Enrollment$mstudyid))]

# Econ: 9 duplicates
Econdups <- Econ$mstudyid[which(duplicated(Econ$mstudyid))]
Econduplicates <- Econ[Econ$mstudyid %in% Econdups,]
Econduplicates <- Econduplicates[order(Econduplicates$mstudyid),] # sorted
Econ <- Econ[!Econ$mstudyid %in% Econdups,] #896 rows/ Jul 18: 958/ Dec 30: 1191


# Cooking: 7 duplicates
Cookingdups <- Cooking$mstudyid[which(duplicated(Cooking$mstudyid))]
Cookingduplicates <- Cooking[Cooking$mstudyid %in% Cookingdups,]
Cookingduplicates <- Cookingduplicates[order(Cookingduplicates$mstudyid),] # sorted
Cooking <- Cooking[!Cooking$mstudyid %in% Cookingdups,] #887 rows/ Jul 18: 937/ Dec 30: 1183


######

colnames(ANC)[1] <- "Formtype_ANC"
colnames(Econ)[1] <- "Formtype_Econ"
colnames(Cooking)[1] <- "Formtype_Cooking"
colnames(Enrollment)[2] <- "Formtype_Enroll"
colnames(Lascar)[1] <- "Formtype_Lascar"


# Merging spreadsheet data (not including Lascar which will be used with the CO data) --------
spreadsheetdata <- merge(ANC, Enrollment, by = "mstudyid", all.x = TRUE)
spreadsheetdata <- merge(spreadsheetdata, Cooking, by = "mstudyid", all.x = TRUE)
spreadsheetdata <- merge(spreadsheetdata, Econ, by = "mstudyid", all.x = TRUE)

nrow(spreadsheetdata) # 1214
names(spreadsheetdata)[169] <- tolower(names(spreadsheetdata[169]))
write.csv(spreadsheetdata, file = paste0("spreadsheetdata_", format(Sys.Date(), format = "%b%d"), ".csv"), row.names = FALSE)

# ####### COMPILE LASCAR CO DATA -------- 
# ######## Identify Lascar files: creating "Lascar_data" file and the "files" (copied from "Lascar_file_ID_and_cleaning") --------
# 
# lascars <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/Lascar_data_cf_Dec21.rds") # this is the file of basic info on all the lascar files processed through Dec 21 including SN and CF, from Lascar_batch_processing_bySN.R
# 
# # grab session info: do in multipe steps to deal with bad value propagation after NA
# session_pattern <- "(s_[0123456789]{1,2}|s[0123456789]{1,2})"
# 
# r <- regexpr(session_pattern, lascars$file2, ignore.case = TRUE)
# out <- rep(NA,length(lascars$file2))
# out[r!=-1] <- regmatches(lascars$file2, r)
# lascars$session <- out
# 
# lascars$session <- toupper(lascars$session)
# 
# # fix weird ones
# unique(lascars$session) # NA, S_15, S_0
# lascars[is.na(lascars$session),] # check for session 1 & fix
# lascars[is.na(lascars$session),][1:3,] <- "S_01"
# lascars[which(lascars$session == "S_15"),]
# lascars$session[which(lascars$session == "S_15")] <- "S_01"
# lascars[which(lascars$session == "S_0"),] #s6
# 
# unique(lascars$session)
# 
# # add duplicate info
# lascars$dup <- ifelse(grepl("dup", lascars$file2) == TRUE, 1, 0)
# sum(lascars$dup) #63
# 
# # SAVE
# saveRDS(lascars, file = paste0("Lascar_data_cf_session", format(Sys.Date(), format = "%b%d"), ".rds"))
# 
# 
# 
# # subset the session 1 files
# Lascar_data <- lascars[lascars$session == "S_01"| lascars$session == "S01",] #1924
# 
# # add mstudyid
# Lascar_data$mstudyid <- substr(gsub("^.*BM", "BM", Lascar_data$file2), 1, 7)
# 
# # choose those files that coincide with unique StudyIDs from the Ultrasound spreadsheet. 
# Lascar_data <- Lascar_data[Lascar_data$mstudyid %in% StudyIDs,] # 1196
# 
# 
# length(unique(Lascar_data$mstudyid)) #1185: compare to above number to see how many duplicates exist (July 14 just BM0328M and BM0583M which each have their own folders)/ Jul 18 BM0328M, BM1132M, BM1168M / July 30 BM0310, BM0336 / Aug 26 + BM0349M (these three are an issue with child files, brought to MM's attention 8/26) / Sep 2 BM0493M
# 
# ### SORT OUT DUPLICATES -------
# # check for child files 
# child <- Lascar_data[grep("BM....C", Lascar_data[,2]),]  # 1 child file
# Lascar_data <- Lascar_data[!Lascar_data$file %in% child$file,]
# 
# ## sort out the duplicates before proceeding
# Lascar_data_nodup <- Lascar_data[Lascar_data$dup == 0,]
# dups <- Lascar_data_nodup$mstudyid[which(duplicated(Lascar_data_nodup$mstudyid))]
# duplicates <- Lascar_data_nodup[Lascar_data_nodup$mstudyid %in% dups,]
# duplicates <- duplicates[order(duplicates$mstudyid),] #sorted
# 
# othersession <- regexpr("s_04|s_05", Lascar_data$file, ignore.case = TRUE)
# Lascar_data <- Lascar_data[othersession == -1,]
# 
# 
# 
# ## run "dups" again to make sure there are no remaining duplicates
# Lascar_data_nodup <- Lascar_data[Lascar_data$dup == 0,]
# dups <- Lascar_data_nodup$mstudyid[which(duplicated(Lascar_data_nodup$mstudyid))]
# duplicates <- Lascar_data_nodup[Lascar_data_nodup$mstudyid %in% dups,] # 0
# 
# 
# nrow(Lascar_data) # 1193 unique observations
# 
# 
# # see how many StudyIDs have no Lascar_data
# unmatched_IDs <- StudyIDs[!StudyIDs %in% Lascar_data$mstudyid] #29 of these
# length(unmatched_IDs) 
# 
# files <- as.character(Lascar_data$file)
# 
# # Save the info on the relevant files
# saveRDS(Lascar_data, file = paste0("Lascar_data_baseline", format(Sys.Date(), format = "%b%d"), ".rds"))
# 
# ##### LOAD PROCESSED CO DATA--------
# # Load the most recent CO_parameters file (the processed CO data), and subset only those files in Lascar_data
# 
# CO_parameters <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/CO_parameters_6623sessions_Jan08.rds")
# CO_parameters_baseline <- CO_parameters[CO_parameters$file %in% Lascar_data$file,] # check that file lengths are the same
# 
# CO_parameters_baseline$dup <- ifelse(grepl("dup", CO_parameters_baseline$file) == TRUE, 1, 0)
# sum(CO_parameters_baseline$dup) #8
#     
# 
# 
CO_parameters <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/CO_parameters_7152sessions_Feb03.rds")
Lascar <- read.csv("~/Dropbox/Ghana project/BP project/Baseline BP Paper/Final_Data/Update_Nov20/Lascar.csv", stringsAsFactors=FALSE)
Lascar <- Lascar[!duplicated(Lascar),] # 5059

CO_parameters_baseline <- CO_parameters[CO_parameters$session == "s_01",] # 1256
Lascar_baseline <- Lascar[Lascar$viround == 1,] #1236


# Checking against "Lascar" file for logsheet data times ------- 
all_lascar <- merge(CO_parameters_baseline, Lascar_baseline, by = "mstudyid", all.x = TRUE) #1280


# format field set and field pickup times in "0000" format
all_lascar$fieldsetdt <- sprintf("%04.0f", all_lascar$fieldsetdt)
all_lascar$fpdtt <- sprintf("%04.0f", all_lascar$fpdtt)

# calculate differences in time between lascar datafile and lascar logsheet
all_lascar$fieldset <- mdy_hm(paste(all_lascar$fieldsetd, all_lascar$fieldsetdt), tz = "GMT")
all_lascar$pickup <- mdy_hm(paste(all_lascar$fpdtd, all_lascar$fpdtt), tz = "GMT")


all_lascar$fieldsetdiff <- difftime(all_lascar$fieldset, all_lascar$firstdate, tz = "GMT", units = "mins")
all_lascar$pickupdiff <- difftime(all_lascar$pickup, all_lascar$lastdate, tz = "GMT", units = "mins")

# the ones where the fieldsetdiff >1000 are the most likely to be files that are matched incorrectly (rather than having a timing typo or the lascar running low)
mismatch <- all_lascar$mstudyid[!is.na(all_lascar$fieldsetdiff) & (abs(all_lascar$fieldsetdiff) > 1000)]
which(all_lascar$mstudyid %in% mismatch)
all_lascar[all_lascar$mstudyid %in% mismatch,c("mstudyid", "firstdate",  "fieldset", "lastdate", "pickup", "fieldsetdiff", "pickupdiff")]
# Apparent typos (so keep): BM0558M, BM0609M, BM1285M, BM1300M, BM1345M
# Unable to reconcile: BM0234M, BM0265M, BM0327M, BM1030M, BM1236M
all_lascar$badmatch <- 0
all_lascar$badmatch[c(39,69, 128, 827,1029)] <- 1
all_lascar[all_lascar$badmatch ==1,c("mstudyid", "firstdate",  "fieldset", "lastdate", "pickup", "fieldsetdiff", "pickupdiff")]
all_lascar[!is.na(all_lascar$fieldsetdiff) & (abs(all_lascar$fieldsetdiff) > 1000) , c("mstudyid", "firstdate",  "fieldset", "lastdate", "pickup", "fieldsetdiff", "pickupdiff")] # look at the problems. 10 of these.  

all_lascar <- all_lascar[all_lascar$badmatch ==0,] #1275

# flag the one file where the comments indicate the woman wore it only 1 day
all_lascar$field_data_invalid <- ifelse(all_lascar$mstudyid == "BM0259M", 1, 0)



# ### SORT OUT DUPLICATES -------
# check for child files 
unique(all_lascar$cstudyid) # NA - ok

unique(all_lascar$mstudyid[duplicated(all_lascar$mstudyid)])

# go through one by one and try to sort out
# all the below have mismatch between lascar and lasid
all_lascar[all_lascar$mstudyid == "BM0856M",] #  bad row names:642, 645
all_lascar[all_lascar$mstudyid == "BM0857M",] # 647, 648
all_lascar[all_lascar$mstudyid == "BM0859M",] # 652, 653
all_lascar[all_lascar$mstudyid == "BM0860M",] # 656, 657
all_lascar[all_lascar$mstudyid == "BM1338M",] # 1132, 1133
all_lascar[all_lascar$mstudyid == "BM1341M",] # 1137,1140
all_lascar[all_lascar$mstudyid == "BM1360M",] #1158, 1161
all_lascar[all_lascar$mstudyid == "BM1361M",] #1163, 1164
all_lascar[all_lascar$mstudyid == "BM1363M",] #1168, 1169
all_lascar[all_lascar$mstudyid == "BM1364M",] #1171, 1174

# all the below are legitimate dups
all_lascar[all_lascar$mstudyid == "BM1439M",]
all_lascar[all_lascar$mstudyid == "BM1469M",]
all_lascar[all_lascar$mstudyid == "BM1470M",]
all_lascar[all_lascar$mstudyid == "BM1471M",]
all_lascar[all_lascar$mstudyid == "BM1474M",]
all_lascar[all_lascar$mstudyid == "BM1475M",]

# remove the mismatches
all_lascar[row.names(all_lascar) %in% as.character(c(642, 645,647, 648, 652, 653, 656, 657, 1132, 1133, 1137,1140, 1158, 1161, 1163, 1164,1168, 1169, 1171, 1174)), c("mstudyid", "dup", "lascar", "lasid")]
all_lascar <- all_lascar[!row.names(all_lascar) %in% as.character(c(642, 645,647, 648, 652, 653, 656, 657, 1132, 1133, 1137,1140, 1158, 1161, 1163, 1164,1168, 1169, 1171, 1174)),]

# for the dups, choose the better ones based on validation data
dupids <- all_lascar$mstudyid[which(duplicated(all_lascar$mstudyid))]
all_lascar[all_lascar$mstudyid %in% dupids,c("mstudyid", "cf_conf", "visually_valid", "visual_notes", "duration_valid", "overall_valid", "field_data_invalid", "comments")]
 # bad rows: 644, 651, 658, 1131, 1139, 1246, 1269, 1271, 1279

all_lascar[row.names(all_lascar) %in% as.character(c(644, 651, 658, 1131, 1139, 1246, 1269, 1271, 1279)), c("mstudyid", "cf_conf", "visually_valid", "visual_notes", "duration_valid", "overall_valid")]
all_lascar <- all_lascar[!row.names(all_lascar) %in% as.character(c(644, 651, 658, 1131, 1139, 1246, 1269, 1271, 1279)), ]

# for the others, choose the first of the two
dupids <- all_lascar$mstudyid[which(duplicated(all_lascar$mstudyid))]
all_lascar <- all_lascar[!row.names(all_lascar) %in% as.character(c(649, 1160, 1165, 1170, 1173, 1274, 1278)), ]


# Check no more duplicates
dupids <- all_lascar$mstudyid[which(duplicated(all_lascar$mstudyid))]
dupids # 0


# if field data invalid, set overall validity to 4
all_lascar$overall_valid <- ifelse(all_lascar$field_data_invalid == 1, 4, all_lascar$overall_valid)

# remove "badmatch" variable
all_lascar <- subset(all_lascar, select = -badmatch)

# see how many StudyIDs have no Lascar_data
unmatched_IDs <- StudyIDs[!StudyIDs %in% all_lascar$mstudyid]
length(unmatched_IDs)  #31


saveRDS(all_lascar, file = paste0("CO_parameters_baseline", format(Sys.Date(), format = "%b%d"), ".rds"))


############### DATA CLEANING AND PROCESSING --------#############


# requires: 
## spreadsheetdata
## CO_parameters_baseline

data <- read.csv("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/spreadsheetdata_Jan09.csv", stringsAsFactors = FALSE) #1214


# convert "88" and "99" to NA

# age
unique(data$age) # no 88
which(data$age == 88)

# "chicken" through "aircon"
names(data[,126:153])

# to check:
which(data[,"sheep"] == 88) # sheep: 562
which(data[,"cattle"] == 88) # cattle: 425
which(data[,"pig"] == 99) # pig: 136

for (i in 126:153) { #"chicken" through "aircon"
  data[,i] <- ifelse (data[,i] == 88 | data[,i] == 99, NA, data[,i])
}
# to check: 
data[562,] # sheep
data[425,] # cattle
data[136,] # cattle, pig, cart



# Change all 1/2 binaries to 0/1: bp, smokhh, smokcc, wownland, electric, kitchen, domestic  (0 = no, 1 = yes)
library(plyr)
head(data[, c("bp", "smokecur", "smokpast", "smokhh", "smokcc", "wownland", "electric", "kitchen", "domestic")])

head(data$bp)
tail(data$bp)

head(data$smokcc)
tail(data$smokcc)

head(data$wownland)
tail(data$wownland)

head(data$kitchen)
tail(data$kitchen)

for (i in c("bp", "smokecur", "smokpast", "smokhh", "smokcc", "wownland", "electric", "kitchen", "domestic")) {
  data[,i] <- mapvalues(data[,i], from = c(1,2), to = c(1,0))
}

# make binaries for: cart, tabled, mattres, divider, itn, radio, tv, cooker, fridge, bicycle, motorc, tract, phone, iron, fan, tvdish, comput, semach, internet, aircon, car

# make new variables "_bin"
for ( i in c("cart", "tabled", "mattres", "divider", "itn", "radio", "tv", "cooker", "fridge", "bicycle", "motorc", "tract", "phone", "iron", "fan", "tvdish", "comput", "semach", "internet", "aircon", "car")) {
  data[,paste0(i, "_bin")] <- data[,i]
}

# transform into binaries
for ( i in c("cart_bin", "tabled_bin", "mattres_bin", "divider_bin", "itn_bin", "radio_bin", "tv_bin", "cooker_bin", "fridge_bin", "bicycle_bin", "motorc_bin", "tract_bin", "phone_bin", "iron_bin", "fan_bin", "tvdish_bin", "comput_bin", "semach_bin", "internet_bin", "aircon_bin", "car_bin")) {
  data[,i] <- ifelse(data[,i] >=1, 1, 0)
  print(unique(data[,i]))
}
  
# normalize to household size for: chicken, sheep, cattle, pig
# variable "people2" is total household members
hist(data$people2) # but some are 0, not sure what to do with
length(which(data$people2 == 0)) #8

# change 0s to 1?
data$people_all <- ifelse(data$people2 == 0, 1, data$people2) # 31 NAs

# note all these now have 45 NAs (instead of 25 for the original variables)
data$chicken_hh <- data$chicken/data$people_all
data$sheep_hh <- data$sheep/data$people_all
data$cattle_hh <- data$cattle/data$people_all
data$pig_hh <- data$pig/data$people_all



# check and change order of "ordinal" variables: medlev, farmln, crops, salary, comptype, ownhouse

# medlev: 1 = no education, 7 = university (ordinal w/no recode needed)
# farmln: "do you have land on which you farm" ordinal 1-5, levels should be reversed
# crops: 9 = no farm, 1= mainly for own food, 2 = for market sale, 3 = cash crops (ordinal, recode 9 to 0)
# salary: 7 categories, should be categorical?
# comptype: categorical (do I want to include this variable?)
# ownhouse: categorical
# married, religion, ethnic: categorical (no recode)

summary(data$farmln)
head(data$farmln)
data$farmln <- as.factor(data$farmln)
data$farmln <- as.numeric(rev(levels(data$farmln)) [data$farmln])


summary(data$crops)
head(data$crops)
tail(data$crops)
data$crops <- mapvalues(data$crops, from = 9, to = 0)

# create binary variables for different categories of toilet, floor, wall, roof, comptype, water, ownhouse
unique(data$toilet) #1,2,3,5, 6, NA
hist(data$toilet)
# 1 = flush WC, 2 = Vent improved pit (VIP), 3 = Pit, 5 = bucket/fields, 6 = other
data$toilet_WC <- ifelse(data$toilet == 1, 1, 0)
data$toilet_VIP <- ifelse(data$toilet == 2, 1, 0)
data$toilet_pit <- ifelse(data$toilet == 3, 1, 0)
data$toilet_field <- ifelse(data$toilet == 5, 1, 0)
data$toilet_other <- ifelse(data$toilet == 6, 1, 0)

unique(data$floor) #1 2 NA
#1 = cement, 2= mud
data$floor_cement <- ifelse(data$floor == 1, 1, 0)
data$floor_mud <- ifelse(data$floor == 2, 1, 0)

unique(data$wall) #1 2 3 NA
# 1= cement, 2 = mud, 3 = other
data$wall_cement <- ifelse(data$wall ==1, 1, 0)
data$wall_mud <- ifelse(data$wall == 2, 1, 0)
data$wall_other <- ifelse(data$wall == 3, 1, 0)


unique(data$roof) # 1 2 NA
# 1= metal/asbestos, 2 = thatch/mud/wood
data$roof_metal <- ifelse(data$roof == 1, 1, 0)
data$roof_thatch <- ifelse(data$roof == 2, 1, 0)

unique(data$comptype) # 1 2 3 4 5 NA
# 1 - none (freestanding), 2 = Common wall, 3 = courtyard with more than 1 entrance, 4 = courtyard with only 1 entrance (there is no 5)
data$comptype <- ifelse(data$comptype == 5, NA, data$comptype)
data$comptype_none <- ifelse(data$comptype == 1, 1, 0)
data$comptype_commonwall <- ifelse(data$comptype == 2, 1, 0)
data$comptype_ctydmany <- ifelse(data$comptype == 3, 1, 0)
data$comptype_ctydone <- ifelse(data$comptype == 4, 1, 0)

unique(data$water) # 11 12 13 14 15 16 17 18 19 20 22 NA
#11 = piped, 12 = public tap, 13 = hand pump, 14 = closed well, 15 = open well, 16 = stream/river, 17 = lake/pond, 18 = water truck, 19 = rainwater, 20 = sachets, 21 = bottled water, 22 = other
data$water_piped <- ifelse(data$water == 11, 1, 0)
data$water_pubtap <- ifelse(data$water == 12, 1, 0)
data$water_pump <- ifelse(data$water == 13, 1, 0)
data$water_closedwell <- ifelse(data$water == 14, 1, 0)
data$water_openwell <- ifelse(data$water == 15, 1, 0)
data$water_stream <- ifelse(data$water == 16, 1, 0)
data$water_lake <- ifelse(data$water == 17, 1, 0)
data$water_truck <- ifelse(data$water == 18, 1, 0)
data$water_rain <- ifelse(data$water == 19, 1, 0)
data$water_sachet <- ifelse(data$water == 20, 1, 0)
data$water_bottled <- ifelse(data$water == 21, 1, 0)
data$water_other <- ifelse(data$water == 22, 1, 0)


unique(data$ownhouse) # 1 2 3 4 5 6 7 NA
# 1 = sole ownership, 2 = joint ownership, 3 = family/relation's house, 4 = house provided rent free, 5 = renting, 6 = perching, 7 = other
data$ownhouse_sole <- ifelse(data$ownhouse == 1, 1, 0)
data$ownhouse_joint <- ifelse(data$ownhouse == 2, 1, 0)
data$ownhouse_family <- ifelse(data$ownhouse == 3, 1, 0)
data$ownhouse_rentfree <- ifelse(data$ownhouse == 4, 1, 0)
data$ownhouse_rent <- ifelse(data$ownhouse == 5, 1, 0)
data$ownhouse_perch <- ifelse(data$ownhouse == 6, 1, 0)
data$ownhouse_other <- ifelse(data$ownhouse == 7, 1, 0)




# remove extraneous variables
# data <- subset(data, select = -c(usdatevisi, usgaw, usgad, numsmcur, smduratd, smduratm, smduratpd, smduratpm, numsmhx, dob, numyrs, religioth, ethnicoth, salaryoth, wateroth, toilet, toiletoth, ownhousoth, floor, flooroth, roof, roofoth, wall, walloth, kitchen))

### Asset index -------


# variables to include in the asset index: 
# kitchen through ownhouse_other, excluding "people_all"

# type of: toilet, floor, wall, roof, compound, water source, home ownership; house has: kitchen, domestic worker; household owns: cart, table, mattres, room divider, insecticide treated net, radio, tv, cooker, ridge, bicycle, motorcycle, tractor, phone, iron, fan, tvdish, computer, sewing machine, internet, aircon, car; number of: chickens, sheep, cattle, pigs  (# animals adjusted for household size) 

# chicken_hh"          "sheep_hh"            "cattle_hh"           "pig_hh" 

# "cart_bin"            "tabled_bin"          "mattres_bin"         "divider_bin"         "itn_bin"             "radio_bin"           "tv_bin"              "cooker_bin"         "fridge_bin"          "bicycle_bin"         "motorc_bin"          "tract_bin"           "phone_bin"          "iron_bin"            "fan_bin"             "tvdish_bin"          "comput_bin"          "semach_bin"         "internet_bin"        "aircon_bin"          "car_bin"


# exclude: other animals (othani), store, commercial vehicle (comveh)
names(data[,c(168:202, 204:230)]) # 62 variables. exclude people_all 
   


# PCA for Asset Index --------
# As per O'Donnell 2007, "Typically, the asset index is assumed to be the first principal component—that is, the first linear combination"
# to get individual asset index, must adjust for household size and composition. 
# per Gunnsteinsson 2010: " most previous studies using asset indices did not adjust for household size, arguing that household characteristics and many durable assets benefit the whole household, irrespective of the number of household members "

# getting the data
PCAdata <-data[,c(1,168:202, 204:230)]
PCAdata <- subset(PCAdata, select = -water_bottled) # since this is constant at 0
PCAdata <- PCAdata[order(PCAdata[,1]),]
missing <- PCAdata$mstudyid[which(is.na(PCAdata$cattle_hh))] # 23 rows with no econ data, 44-45 with no animal data
missingcomptype <- PCAdata$mstudyid[which(is.na(PCAdata$comptype_none))]
missingchicken <- PCAdata$mstudyid[which(is.na(PCAdata$chicken_hh))]
missing <- unique(c(missing, missingcomptype, missingchicken))
nonmissing <- PCAdata$mstudyid[!PCAdata$mstudyid %in% missing]
PCAdata <- PCAdata[!PCAdata$mstudyid %in% missing,] #1167

# performing the pca on centered, scaled data
pca<- prcomp(PCAdata[,2:62], center = TRUE, scale = TRUE) # the dimensions account for more of the inertia than if I scale the units. Without scaling, Dim 1 accounts for 41.3% of the inertia. WIth scaling, Dim 1 is 7.8%.
summary(pca) # PC1: 7.8% of variance (this is not very much compared with 18.45 before all the binarization)
pca.result <- as.data.frame(pca$x) # getting the principal components
pca.result <- subset(pca.result,select = 1) # keeping only PC1
head(pca.result)
range(pca.result) #-3.2 to 11.6/ Jan 9: -5 to 7.9

# merge back in the missing rows
pca.result$mstudyid <- nonmissing
head(pca.result)
colnames(pca.result)[1] <- "asset_index"
missing2 <- as.data.frame(missing)
missing2$asset_index <- NA
colnames(missing2)[1] <- "mstudyid"
missing2 <- missing2[,2:1]
head(missing2)
pca.result <- rbind(pca.result, missing2)

# merge into a new dataset
data2 <- merge(data, pca.result, by = "mstudyid")

# Gunnsteinsson: Before estimating the principal components, all the variables were centered at zero and scaled to have a unit variance. This way the principal component has a mean of zero, and all the variables have an effect on the principal components in proportion to the weight they are assigned by the analytical procedure.

# Vyas 2006: Using the factor scores from the first principal component as weights, a dependent variable can then be constructed for each household (Y1) which has a mean equal to zero, and a standard deviation equal to one. This dependent variable can be regarded as the households ‘socio-economic’ score, and the higher the household socio-economic score, the higher the implied SES of that household.

# note: using command "princomp" results in the opposite results (signs are reversed). prcomp provides results in the expected order.

#the sum of the asset variables, weighted by the elements of the first eigenvector.

# add dichotomous household/compound smoking variable
data2$shs <- ifelse(data2$smokcc == 1 , 1, 0)
data2$shs <- ifelse(data2$smokhh ==1, 1, data2$shs)
data2$shs[head(which(data2$smokhh ==1 & data2$smokcc ==0))]
data2$shs[head(which(data2$smokhh==0 & data2$smokcc ==1))]
data2$shs[head(which(data2$smokhh==1 & data2$smokcc ==1))]
head(data2$shs)

# add Gestational age:
data2$gestwks <- round((data2$usgaw + data2$usgad/7), digits = 2)
hist(data2$gestwks, breaks = 50)



# save spreadsheet data
write.csv(data2, file = paste0("cleaned_spreadsheet_data", format(Sys.Date(), format = "%b%d"), ".csv"), row.names = FALSE)

# stopped here Jan 9

### merge with CO_parameters_baseline --------
data <- read.csv("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/cleaned_spreadsheet_dataJan09.csv", stringsAsFactors = FALSE) 
nrow(data) #1214
CO_parameters_baseline <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/CO_parameters_baselineFeb03.rds")
nrow(CO_parameters_baseline) #1239

data <- merge(data, CO_parameters_baseline, by = "mstudyid")
nrow(data) #1183

# save
saveRDS(data, file = paste0("baselinebpdata", format(Sys.Date(), format = "%b%d"), ".rds"))


# add info from DEM forms (3 per lascar session) -----
baselinebpdata <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/baselinebpdataFeb03.rds")
DEM <- read.csv("~/Dropbox/Ghana project/BP project/Baseline BP Paper/Final_Data/Update_Nov20/DEM.csv", stringsAsFactors=FALSE)

# subsetting only the baseline observations
DEM <- DEM[DEM$vround ==1,]

# getting rid of duplicate rows
DEM <- DEM[!duplicated(DEM),]

StudyIDs <- unique(ANC$mstudyid) #929/ Jul 18: 996/ Dec 30: 1214


DEM <- DEM[DEM$mstudyid %in% StudyIDs,] #3621
uniqueid <- unique(DEM$mstudyid) #1199

# merge Lascar and DEM data to check for weird files
all_lascar <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/CO_parameters_baselineFeb03.rds")
lascar_dem <- merge(DEM, all_lascar, all.x = TRUE, by = "mstudyid")

for (i in 1:length(uniqueid)) { #1:length(uniqueid)
  DEM_id <- lascar_dem[lascar_dem$mstudyid == uniqueid[i],]
  if(nrow(DEM_id)> 3) print(DEM_id[, c("mstudyid", "datevisit.x", "pickup", "firstdate", "lastdate")])
}

# bad rows (dates don't align between lascar info and DEM info: 374, 890, 893, 895, 807, 788, 1010, 995, 997, 999, 1167, 1169, 1172, 1369, 1372, 1607, 1608, 1956, 2272, 2543, 2544, 2546, 3030, 3031, 3224, 3226, 3227, 3373, 3375, 3378

lascar_dem[row.names(lascar_dem) %in% as.character(c(374, 890, 893, 895, 807, 788, 1010, 995, 997, 999, 1167, 1169, 1172, 1369, 1372, 1607, 1608, 1956, 2272, 2543, 2544, 2546, 3030, 3031, 3224, 3226, 3227, 3373, 3375, 3378)), c("mstudyid", "datevisit.x", "pickup", "firstdate", "lastdate")]

lascar_dem <- lascar_dem[!row.names(lascar_dem) %in% as.character(c(374, 890, 893, 895, 807, 788, 1010, 995, 997, 999, 1167, 1169, 1172, 1369, 1372, 1607, 1608, 1956, 2272, 2543, 2544, 2546, 3030, 3031, 3224, 3226, 3227, 3373, 3375, 3378)),]

# check for mismatched dates
for (i in 1:length(uniqueid)) { #1:length(uniqueid)
  DEM_id <- lascar_dem[lascar_dem$mstudyid == uniqueid[i],]
  if(abs(as.numeric(difftime(mdy(DEM_id$datevisit.x), DEM_id$lastdate, units = "days"))) > 4 &!is.na(DEM_id$pickup)) print(DEM_id[, c("mstudyid", "datevisit.x", "pickup", "firstdate", "lastdate")])
}

# more bad rows (dates don't align between lascar info and DEM info & not an apparent typo: 1469, 1470, 1471, 2243, 2244, 2245, 2364, 2365, 2366, 2720, 2721, 2722, 2289, 2290, 2291)

lascar_dem[row.names(lascar_dem) %in% as.character(c(1469, 1470, 1471, 2243, 2244, 2245, 2364, 2365, 2366, 2720, 2721, 2722, 2289, 2290, 2291)), c("mstudyid", "datevisit.x", "pickup", "firstdate", "lastdate")]

lascar_dem <- lascar_dem[!row.names(lascar_dem) %in% as.character(c(1469, 1470, 1471, 2243, 2244, 2245, 2364, 2365, 2366, 2720, 2721, 2722, 2289, 2290, 2291)), ]


# add cooking events
cookingevents <- data.frame(mstudyid = uniqueid)
uniqueid <- unique(lascar_dem$mstudyid)
for (i in 1:length(uniqueid)) { #1:length(uniqueid)
  DEM_id <- lascar_dem[lascar_dem$mstudyid == uniqueid[i],]
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

# can't figure out what's going on in th eones with more than 3 forms so remove
cookingevents <- cookingevents[cookingevents[,"no_forms"] < 4,] # 1185
cookingevents <- cookingevents[order(cookingevents$mstudyid),]

cookingevents$Formtype_DEM <- "DEM"
cookingevents <- cookingevents[,c(1, 22, 2:21)]
cookingevents$mstudyid <- as.character(cookingevents$mstudyid)


baselinebpdata <- merge(baselinebpdata, cookingevents, by = "mstudyid", all.x = TRUE)

sum(is.na(baselinebpdata$cookingevents)) # 19 missing






# add other general smoke exposures from Cooking form

summary(baselinebpdata[, c("invcomm","incoalsel1","cinvcom","incoalsel2")]) #  invcomm      incoalsel1     cinvcom      incoalsel2 (involved in commercial food prod or charcoal prod)
for (i in c("invcomm","incoalsel1","cinvcom","incoalsel2")) {
  baselinebpdata[,i] <- mapvalues(baselinebpdata[,i], from = c(1,2,9), to = c(1,0,0))
}
baselinebpdata$invcom_sum <- ifelse(baselinebpdata$invcomm == 1 | baselinebpdata$cinvcom ==1, 1 ,0) # involved in commercial food prod
baselinebpdata$invcoal_sum <- ifelse(baselinebpdata$incoalsel1 ==1 | baselinebpdata$incoalsel2 ==1, 1, 0) #involved in charcoal production

summary(baselinebpdata$coils)
baselinebpdata$coils <- mapvalues(baselinebpdata$coils, from = c(1,2), to = c(1,0))

summary(baselinebpdata[,97:102])  # trashsmk through othsmk, all 1/2 binaries, change to 0,1
for (i in 97:102) {
  baselinebpdata[,i] <- mapvalues(baselinebpdata[,i], from = c(1,2), to = c(1,0))
}
baselinebpdata$oth_smksources <- ifelse(rowSums(baselinebpdata[,97:102]) >0, 1, 0) # binary: if any of the smoke sources present, 1

summary(baselinebpdata$onelsecok)
baselinebpdata$onelsecok <- mapvalues(baselinebpdata$onelsecok, from = c(1,2,9), to = c(1,0,0)) # 1 = there is someone else who regularly participates in cooking in this compound

summary(baselinebpdata[,c("onelsecok", "coils", "invcom_sum", "invcoal_sum", "oth_smksources")])


# add BMI
baselinebpdata$BMI <- round(baselinebpdata$wtkg/(.01*baselinebpdata$htcm)^2, digits = 2)
hist(baselinebpdata$BMI, breaks = 50)



### DEALING WITH OUTLIERS --------------


# see http://www.unige.ch/ses/sococ/cl/r/tasks/outliers.e.html

# outliers defined as values more than 3.5 times the interquartile range from the edges of the interquartile range (the box). E.g. above the 3rd quartile or below the 1st quartile.

# plot of the outliers
data <- baselinebpdata
par(mfrow = c(2,2))
pdf(file = "Outliers.pdf", width = 8, height = 6)
boxplot(data$htcm, range = 3.5, main = "height in cm")
boxplot(data$wtkg, range = 3.5, main = "weight in kg")
boxplot(data$sbp, range = 3.5, main = "sbp in mmHg")
boxplot(data$dbp, range = 3.5, main = "dbp in mmHg")
boxplot(data$mean_corr, range = 3.5, main = "mean corrected CO")

dev.off()

# create "validity" variables: 0 = fine, 1 & 2 = questionable, 3 = clearly bogus


names(baselinebpdata[,c("wtkg", "htcm", "sbp", "dbp")]) # wtkg, htcm, sbp, dbp

# sets validity variable to 2, 3, or 3.5 if ANY of wtkg, htcm, sbp, dbp are outliers of that magnitude
# htcm, wtkg
baselinebpdata$anthrop_validity <- 0
for (i in c("wtkg", "htcm")) {
  for (j in c(2,3,3.5)) {
    id <- boxplot.stats(baselinebpdata[,i], coef = j)
    print(id)
    baselinebpdata$anthrop_validity <- ifelse(baselinebpdata[,i] %in% id$out, j, baselinebpdata$anthrop_validity)
  }
}

# check! 
baselinebpdata[baselinebpdata$anthrop_validity > 1, c("wtkg", "htcm", "BMI", "anthrop_validity")]

#sbp, dbp 
baselinebpdata$bp_validity <- 0
for (i in c("sbp", "dbp")) {
  for (j in c(2,3,3.5)) {
    id <- boxplot.stats(baselinebpdata[,i], coef = j)
    baselinebpdata$bp_validity <- ifelse(baselinebpdata[,i] %in% id$out, j, baselinebpdata$bp_validity)
  }
}
# if dbp > sbp, set bp_validity to 3.5
baselinebpdata$bp_validity <- ifelse(baselinebpdata$dbp > baselinebpdata$sbp, 3.5, baselinebpdata$bp_validity)

# check! 
baselinebpdata[baselinebpdata$bp_validity > 1,c("sbp", "dbp", "anthrop_validity", "bp_validity")]

# how many are outliers?
id <- boxplot.stats(baselinebpdata$sbp, coef = 3.5)
which(baselinebpdata$sbp %in% id$out) #0 for sbp

id <- boxplot.stats(baselinebpdata$dbp, coef = 3.5)
which(baselinebpdata$dbp %in% id$out) #2 for dbp

which(baselinebpdata$dbp > baselinebpdata$sbp) #1 (included in above)

### Add village coding -------

baselinebpdata <- baselinebpdata <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/baselinedata_Feb04.rds")

# villages <- as.data.frame(unique(baselinebpdata$vname))
# villages <- villages[order(villages[,1]),]
# write.csv(villages, file = "villages.csv", row.names = FALSE)
library(foreign)
village_codes <- read.dta("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Final_Data/village info/clusters_for_CEM.dta")
village_codes <- village_codes[order(village_codes$village),]

baselinebpdata$vil_code <- NA
baselinebpdata$vil_code <- ifelse(grepl("(ABUDWOM|NTANKRO|NTANKORO)", baselinebpdata$vname) == TRUE, "AB_NT", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("AJENA|AJENE", baselinebpdata$vname) == TRUE, "AJ", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("AKORA", baselinebpdata$vname) == TRUE, "AK", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("(ALHASSAN|KROWURA|KURAWURA|KUROWURA|KURUWURA)", baselinebpdata$vname) == TRUE, "LH_UR", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("(AMPOMA|ANPOMA)", baselinebpdata$vname) == TRUE, "NP", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("ANOKYE", baselinebpdata$vname) == TRUE, "KY", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("APESIKA", baselinebpdata$vname) == TRUE, "AP", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("ASANTE", baselinebpdata$vname) == TRUE, "AS", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("(ATAA AKURA|ATAKURA|ATTA AKURA)", baselinebpdata$vname) == TRUE, "AA", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("BADU AKURA", baselinebpdata$vname) == TRUE, "BD", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("BEPOSO", baselinebpdata$vname) == TRUE, "BE", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("BUSUAMA", baselinebpdata$vname) == TRUE, "BS", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("(CHERIHI|CHIRIHI)", baselinebpdata$vname) == TRUE, "CH", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("DAWADAWA", baselinebpdata$vname) == TRUE, "DD", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("(DUMSO|DUMSU)", baselinebpdata$vname) == TRUE, "DM", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("DWENEWOHO", baselinebpdata$vname) == TRUE, "DN", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("(HIRESO|HYERISO|HYIRESO)", baselinebpdata$vname) == TRUE, "HS", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("JATO", baselinebpdata$vname) == TRUE, "JA", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("KADELSO", baselinebpdata$vname) == TRUE, "KL", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("KANDIGE", baselinebpdata$vname) == TRUE, "KA", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("(KAWAMPE|KAWANPE)", baselinebpdata$vname) == TRUE, "KP", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("(KOBEDA|KOBEDU)", baselinebpdata$vname) == TRUE, "KD", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("KOKUMA", baselinebpdata$vname) == TRUE, "KK", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("KRABONSO|KRABONSU", baselinebpdata$vname) == TRUE, "KR", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("(KRUTAKYI|PAMDU)", baselinebpdata$vname) == TRUE, "KT_PU", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("KUNSU", baselinebpdata$vname) == TRUE, "KU", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("(KWABEA|KWABIA)", baselinebpdata$vname) == TRUE, "KB", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("MANSIE", baselinebpdata$vname) == TRUE, "MA", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("NANTE", baselinebpdata$vname) == TRUE, "NZ", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("(NEW L|NEW-L)", baselinebpdata$vname) == TRUE, "LL", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("(PANIA|PANINA|PANINI|PENIA)", baselinebpdata$vname) == TRUE, "PS", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("PRAMPOSO", baselinebpdata$vname) == TRUE, "PR", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("SABULE", baselinebpdata$vname) == TRUE, "LE", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("(SORO|SURO)", baselinebpdata$vname) == TRUE, "SU", baselinebpdata$vil_code)
baselinebpdata$vil_code <- ifelse(grepl("WEILA", baselinebpdata$vname) == TRUE, "WE", baselinebpdata$vil_code)


length(unique(baselinebpdata$vil_code)) #35
table(baselinebpdata$vname, baselinebpdata$vil_code) # to check


village_codes$code <- gsub("\\+", "_", village_codes$code)
colnames(village_codes)[1] <- "vil_code"
colnames(village_codes)[c(4:5,7:8,19)] <- paste0("vil_", colnames(village_codes)[c(4:5,7:8,19)])
village_variables <- village_codes[,c("vil_code", "vil_pop", "vil_some_edu", "vil_modal_rel", "vil_modal_ethn", "village_ses2", "vil_hhsize", "vil_on_road")]
baselinebpdata <- merge(baselinebpdata, village_variables, by = "vil_code", all.x = TRUE)


## check variables
summary(baselinebpdata[, c("age", "BMI", "gestwks", "asset_index", "sbp", "dbp")])

# Fix BM1315M age
baselinebpdata$age[baselinebpdata$mstudyid == "BM1315M"] <- 20

# save data
saveRDS(baselinebpdata, file = paste0("baselinedata_", format(Sys.Date(), format = "%b%d"), ".rds"))


write.csv(baselinebpdata, file = paste0("baselinedata_", format(Sys.Date(), format = "%b%d"), ".csv"), row.names = FALSE)


# for variable codebook (then fill in by hand)
variables <- as.data.frame(names(baselinebpdata))
write.csv(variables, file = paste0("baselinedata_codebook_", format(Sys.Date(), format = "%b%d"), ".csv"), row.names = FALSE)

# ####################### DATA ANALYSIS ------#####################
# 
# # requires: lastest baselinebpdata file
# 
# # # remove extraneous variables
# analysis_data <- subset(baselinebpdata, select = c("mstudyid", "vname", "sbp", "dbp", "bp", "smokecur", "smokpast", "shs", "age", "medlev", "married", "religion", "ethnic", "wownland", "farmln", "crops", "salary", "comptype", "ownhouse", "min_co", "mean_co", "q90", "q95", "q97", "q98", "q99", "max_co", "hours", "startdate", "htcm", "wtkg", "BMI", "gestwks", "asset_index", "cookingevents", "anthrop_validity", "bp_validity", "co_validity", "onelsecok", "coils", "invcom_sum", "invcoal_sum", "oth_smksources"))
# 
# names(analysis_data)
# # [1] "mstudyid"         "vname"            "sbp"              "dbp"              "bp"              
# # [6] "smokecur"         "smokpast"         "shs"              "age"              "medlev"          
# # [11] "married"          "religion"         "ethnic"           "wownland"         "farmln"          
# # [16] "crops"            "salary"           "comptype"         "ownhouse"         "min_co"          
# # [21] "mean_co"          "q90"              "q95"              "q97"              "q98"             
# # [26] "q99"              "max_co"           "hours"            "startdate"        "htcm"            
# # [31] "wtkg"             "BMI"              "gestwks"          "asset_index"      "cookingevents"   
# # [36] "anthrop_validity" "bp_validity"      "co_validity"      "onelsecok"        "coils"           
# # [41] "invcom_sum"       "invcoal_sum"      "oth_smksources"  
# 
# 
# NAPerVariable(analysis_data) # 95 missing age, 3 each missing: bp, smokecur, smokpast, shs,  31 missing all stuff on econ form, and asset index, 46 missing other smoke sources
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
# 
# 
# 
# 
# 
# 
# write.csv(analysis_data, file = paste0("data_bpanalysis_", format(Sys.Date(), format = "%b%d"), ".csv"), row.names = FALSE)
# 
# 
# 
# 
# 
# ### ANALYSIS -----------
# ## TRY Q90, Q95, Q97 ----
# 
# 
# 
# # sbp and dbp data bell-shaped but with some outliers. CO data extremely right-skewed (some extreme outliers)
# 
# # log-transform all CO data. Don't log-transform the sbp and dbp data? (still think about outliers)
# 
# # robust regression? see http://www.ats.ucla.edu/stat/r/dae/rreg.htm
# 
# 
# # Unadjusted analysis
# 
# analysis_data <- data_bpanalysis_Sep11
# 
# 
# # set BMI outliers to NA
# bpdata <- analysis_data
# summary(bpdata$BMI)
# bpdata$BMI <- ifelse(bpdata$anthrop_validity > 0, NA, bpdata$BMI)
# 
# # remove BP outliers from analysis
# bpdata <- bpdata[bpdata$bp_validity < 3.5,] #883
# row.names(bpdata) <- NULL
# 
# bpdata$occ_exp <- ifelse(bpdata$invcom_sum ==1 | bpdata$invcoal_sum ==1, 1, 0)
# summary(bpdata$occ_exp)
# sum(bpdata$occ_exp[!is.na(bpdata$occ_exp)]) #271
# 
# # exclude those who reported no cooking events
# nocooking <- bpdata$mstudyid[bpdata$cookingevents ==0 &!is.na(bpdata$cookingevents)] #10
# bpdata2 <- bpdata[!bpdata$mstudyid %in% nocooking,] #873
# row.names(bpdata2) <- NULL
# 
# 
# write.csv(bpdata2, file = paste0("final_analysis_data_", format(Sys.Date(), format = "%b%d"), ".csv"), row.names = FALSE) # this dataset excludes BP outliers and those with no cooking events
# 
# 
# 
# # Adjusted analyses -------
# bpdata2 <- final_analysis_data_Oct04 # these values are not lining up with the ones from the analysis!
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
# # DBP
# lm_dbp <- lm(dbp ~ log(q98), data = bpdata2)
# cooksD <- cooks.distance(lm_dbp) # using adjusted model with bpdata2 
# max(cooksD) #.0923
# length(cooksD) #744/ # Oct 4 742
# tail(cooksD) # but the numbers go up to 870 (so these are row numbers in bpdata2 - the last 5 were excluded because of NAs)
# 
# 
# length(cooksD[cooksD > 0.04]) #1
# length(cooksD[cooksD > 0.03]) #3: 269, 663, 680
# cooksD[cooksD > 0.03] # 269, 663, 680
# bpdata6 <- bpdata2[-c(269),]
# bpdata7 <- bpdata2[-c(269, 663),]
# bpdata8 <- bpdata2[-c(269,663, 680),]
# 
# 
# # Mccracken: age & BMI (linear); binary indicators for: smoking, SHS exposure, temescal use, household electricity. The asset index is the sum of binary indicators for having a bicycle, a radio, and a television, and was entered as categorical variable. To increase precision, we also considered time-varying covariates, such as apparent temperature, sea- son, day of the week, and time of day. We used linear terms to control for daily average apparent temperature and time of day and dummy variables for each day of the week and for rainy (1 May–31 October) versus dry season (1 November –30 April).
# 
# # complete case analysis
# 
# # linear variables: sbp, dbp, asset_index, gestwks, crops, farmln
# # variables to log transform:
# ## age, mean_co, q90, max_co, BMI 
# # factor variables: married, religion, ethnic, comptype, ownhouse, salary, weekday, [comptype]
# # binary variables: bp, smokhh, smokcc, wownland, rainy
# 
# 
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
# bpdata6 <- bpdata2[-c(269),]
# 
# # Unadjusted model
# #SBP
# # q98
# lm1 <- lm(sbp ~ log(q98), data = bpdata6)
# summary(lm1) # not signif, coef = 0.1226
# 
# # mean_co
# lm2 <- lm(sbp ~ log(mean_co), data = bpdata6)
# summary(lm2) # not signif, coef = 0.1658
# 
# #DBP
# # q98 - *
# lm4 <- lm(dbp ~ log(q98), data = bpdata6)
# summary(lm4) # marginally signif, coef = 0.8133
# 
# # mean_co - **
# lm5 <- lm(dbp ~ log(mean_co), data = bpdata6)
# summary(lm5) # signif, coef = 0.8217
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
# # stepwise selection
# 
# library(MASS)
# bpdata_complete <- bpdata[complete.cases(bpdata),]
# 
# # for sbp
# fit <- lm(sbp ~ log(q98) + log(age) + log(BMI) + asset_index + gestwks + shs  + cookingevents + wownland + crops + farmln + medlev + ownhouse + onelsecok + coils + invcom_sum + invcoal_sum + oth_smksources, data = bpdata_complete)
# step <- stepAIC(fit, direction="both", na.action = na.omit)
# step$anova # display results
# 
# # Final Model:
# #  sbp ~ log(age) + log(BMI) + gestwks + shs + crops + invcoal_sum
# # but keep CO in since it's our predictor of interest
# 
# 
# # for dbp
# fit <- lm(dbp ~ log(q98) + log(age) + log(BMI)  + asset_index + gestwks + shs  + cookingevents+ onelsecok + coils + invcom_sum + invcoal_sum + oth_smksources, data = bpdata_complete)
# step <- stepAIC(fit, direction="both", na.action = na.omit)
# step$anova # display results
# 
# # Final Model:
# #   dbp ~ log(q98) + log(BMI) + asset_index + gestwks
# 
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
# # Scenario - what if all missing age toward the top end
# quantile(bpdata6$age[!is.na(bpdata6$age)], probs = seq(from = 0, to = 1, by = 0.05)) #90%: 37, 95%: 40
# bpdata11 <- bpdata6
# bpdata11$age <- ifelse(is.na(bpdata11$age), 19, bpdata11$age)
# summary(bpdata11$age)
# 
# lm_dbp <-  lm(dbp ~ log(q98) + log(age) + log(BMI) + shs + asset_index  + invcoal_sum + gestwks_ind, data = bpdata12)
# summary(lm_dbp) # doesn't change it whether lo or hi
# 
# bpdata12 <- bpdata6
# bpdata12$gestwks_ind <- ifelse(bpdata12$gestwks <21, 0, 1)
# summary(bpdata12$gestwks_ind)
# # Table 1 -----
# 
# # for continuous variables and NAs
# library(pastecs)
# options("scipen" = 6, "digits" = 3)
# stat.desc(bpdata6)
# 
# # for binary variables
# for (i in c(5,8,15,42,45)) {
#   print(colnames(bpdata6)[i])
#   print(nrow(bpdata6[which(bpdata6[,i] ==1),]))
#   print(nrow(bpdata6[which(bpdata6[,i] ==1),])/nrow(bpdata6) * 100)
#   print(nrow(bpdata6[is.na(bpdata6[,i]),])/nrow(bpdata6) * 100)
# }
# 
# # for smokecur/smokpast
# nrow(bpdata2[which(bpdata2$smokecur ==1 | bpdata2$smokpast ==1),])/nrow(bpdata2) * 100
# nrow(bpdata[is.na(bpdata$smokecur),]) #3
# 
# # for categorical variables
# for (i in c(10, 11,12,13,14,15,16,17,18, 19, 30)) {
#   print(colnames(bpdata6)[i])
#   print(summary(factor(bpdata6[,i]))/nrow(bpdata6) * 100) # note: NA percent is not valid using this method
# }
# 
# summary(bpdata$cookingevents)
# hist(bpdata$cookingevents)
# nrow(bpdata[bpdata$cookingevents ==0 & !is.na(bpdata$cookingevents),]) #10
# 
# 
# # for asset index quintiles
# quints <- quantile(bpdata$asset_index, probs = seq(0,1,0.2), na.rm = TRUE)
# sum(is.na(bpdata$asset_index)) # 32
# r <- bpdata$asset_index[!is.na(bpdata$asset_index)]
# for (i in 1:5){
#   print(i)
#   print(length(r[r >= quints[i] & r < quints[i+1]])/872)
# }
# #? not working
# 
# ###### FIGURE 1: Example CO Plot
# CO_stacked <- CO_stacked_Sep02
# CO_stacked$datetime <- ymd_hms(CO_stacked$datetime, tz = "GMT")
# directory <- "/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/CO minutewise plots/"
# pdf(file = paste(directory, "SAMPLE_CO_minute_avg.pdf", sep = ""))
# print(ggplot(data=filter(CO_stacked, mstudyid == "BM0289M"))+ geom_line(aes(datetime,co), color="blue") + labs(title=paste("Minute-Averaged CO \n", "Session 1", data$session[1]))+xlab("Date")+ ylab("CO (ppm)") + scale_x_datetime(breaks = date_breaks("1 day"), labels = date_format("%a %b %d %H:%M")) + expand_limits(y=0) +  theme(plot.title=element_text(size=rel(0.92), color = "darkgrey")))
# dev.off()
# 
# 
# ###### FIGURE 2: Graph of CO Kernel Density ----
# # q98
# pdf(file = paste0("Fig2_CO_Density", format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 6)
# d <- density(bpdata6$q98)
# plot(d, ylim = c(0, 0.1), main = "Distribution of 98th-percentile measured 72-hour CO", lwd = 2, xlab = "98th percentile of CO(ppm)", ylab = "Probability density", xaxp = c(0, 150, 10), col = "coral3")
# abline(v = mean(bpdata6$q98), lty = "dotted")
# text(x = mean(bpdata6$q98), y = 0.09, pos = 4, label = paste("Average 98th percentile CO:\n", round(mean(bpdata6$q98), digits = 2), "\U00b1", round(sd(bpdata6$q98), digits = 2), "ppm"))
# dev.off()
# 
# # mean_co
# pdf(file = paste0("Fig2_CO_Density_meanCO_", format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 6)
# d <- density(bpdata6$mean_co)
# plot(d, main = "Distribution of Mean 72-hour CO", lwd = 2, xlab = "Mean CO(ppm)", ylab = "Probability density", col = "coral3")
# abline(v = mean(bpdata6$mean_co), lty = "dotted")
# text(x = mean(bpdata6$mean_co), y = 0.4, pos = 4, label = paste("Average CO:\n", round(mean(bpdata6$mean_co), digits = 2), "\U00b1", round(sd(bpdata6$mean_co), digits = 2), "ppm"))
# dev.off()
# 
# ##### FIGURE 3: Graph of BP Kernel Density ----
# pdf(file = paste0("Fig3_BP_Density", format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 6)
# e <- density(bpdata6$sbp)
# plot(e, ylim = c(0, 0.1), xlim = c(0, 200), main = "Distribution of baseline BP", lwd = 2, xlab = "BP (mmHg)", ylab = "Probability density",  col = "purple")
# f <- density(bpdata6$dbp)
# lines(f, col = "darkgreen", lwd = 2)
# abline(v = mean(bpdata6$sbp), lty = "dotted")
# abline(v = mean(bpdata6$dbp), lty= "dotted")
# 
# text(x = mean(bpdata6$sbp), y = 0.09, pos = 4, label = paste("SBP:\n", round(mean(bpdata6$sbp), digits = 2), "\U00b1" , round(sd(bpdata6$sbp), digits = 2), "mmHg"))
# text(x = mean(bpdata6$dbp), y = 0.09, pos = 2, label = paste("DBP:\n ", round(mean(bpdata6$dbp), digits = 2),  "\U00b1", round(sd(bpdata6$dbp), digits = 2), "mmHg"))
# legend ("topright", legend = c("SBP", "DBP"), col = c("purple", "darkgreen"), lwd = 2)
# dev.off()