##################################################
# Project:  MPC-693 UDOT-22.601 Transit peds
# Authors:  Aleks Paskett (aleks.paskett@usu.edu)
#           Fariba Soltani (fariba.soltani@usu.edu)
#           Patrick Singleton (patrick.singleton@usu.edu)
# File:     combine_data.R
# Date:     2025 Spring
# About:    Combine cleaned data for transit peds project
##################################################

##################################################
# Notes

# 2024-05-28 created FS
# 2024-06-12 updated FS
# 2024-06-21 updated PS
# 2024-06-26 updated FS/PS
# 2025-02-26 updated AP
# 2025-03-12 updated AP
# 2025-04-01 updated PS
# 2025-04-14 updated PS

# Load packages
library(tidyr)
library(readxl)
library(dplyr)
library(stringr)

##################################################
# Load and merge data

# Define file path
myfpath2 <- file.path("Data", "Videos ped transit", "2_Checked")
myfpath3 <- file.path("Data", "Videos ped transit", "3_Combined")

# Define info about files
myfiles <- data.frame(FULL=list.files(file.path(myfpath2), pattern = ".xlsx"))
myfiles$SHORT <- gsub(".xlsx", "", myfiles$FULL)

# Increase max.print to help use tables for data cleaning
options(max.print = 2000)

# Load info about data columns
# columns <- read.csv(file.path(myfpath3, "columns.csv"))
columns <- read_excel(file.path(myfpath3, "columns.xlsx")) 

# Check that column names are identical before merging
# - get column names from each file
all_cols <- lapply(myfiles$FULL, function(f) {
  temp <- read_excel(file.path(myfpath2, f))
  names(temp)
})
# - identify common columns
common_cols <- Reduce(intersect, all_cols)
# - identify non-mutual columns
non_mutual_cols <- unique(unlist(lapply(all_cols, function(cols) {
  setdiff(cols, common_cols)
})))
# - print non-mutual columns
print(non_mutual_cols)

# Loop through source files to get data and merge
# - import first file and check for import errors
i <- 1
print(myfiles$FULL[i])
temp <- read_excel(file.path(myfpath2, myfiles$FULL[i]))
temp$File <- myfiles$SHORT[i]
temp <- temp[, !(names(temp) %in% non_mutual_cols)]
# temp <- temp[-1,]
dat0 <- temp
rm(temp)
# - loop through the rest of the files
for (i in 2:nrow(myfiles)) {
  print(i)
  print(myfiles$FULL[i])
  temp <- read_excel(file.path(myfpath2, myfiles$FULL[i]))
  temp$File <- myfiles$SHORT[i]
  temp <- temp[, !(names(temp) %in% non_mutual_cols)]
  # temp <- temp[-1,]
  dat0 <- rbind(dat0, temp)
  rm(temp)
}; rm(i)
# - move file name to first column
dat0 <- dat0[,c("File", names(dat0)[1:(ncol(dat0)-1)])]

# Inspect
names(dat0)
str(dat0)
summary(dat0)

##################################################
# Inspect data

# Visual check of dates and timestamps
# - looking for not sequential, out-of-order
sort(unique(dat0$SIGNAL))
# View(dat0[dat0$SIGNAL==1021,])
# View(dat0[dat0$SIGNAL==1094,])
# View(dat0[dat0$SIGNAL==1221,])
# View(dat0[dat0$SIGNAL==1225,])
# View(dat0[dat0$SIGNAL==1229,])
# View(dat0[dat0$SIGNAL==4522,])
# View(dat0[dat0$SIGNAL==5030,])
# View(dat0[dat0$SIGNAL==5056,])
# View(dat0[dat0$SIGNAL==5093,])
# View(dat0[dat0$SIGNAL==5108,])
# View(dat0[dat0$SIGNAL==5139,])
# View(dat0[dat0$SIGNAL==5300,])
# View(dat0[dat0$SIGNAL==5324,])
# View(dat0[dat0$SIGNAL==6025,])
# View(dat0[dat0$SIGNAL==6311,])
# View(dat0[dat0$SIGNAL=="7041A",])
# View(dat0[dat0$SIGNAL=="7041B",])
# View(dat0[dat0$SIGNAL==7084,])
# View(dat0[dat0$SIGNAL==7104,])
# View(dat0[dat0$SIGNAL==7110,])
# View(dat0[dat0$SIGNAL==7160,])
# View(dat0[dat0$SIGNAL==7234,])
# View(dat0[dat0$SIGNAL=="7290a",])
# View(dat0[dat0$SIGNAL=="7290b",])
# View(dat0[dat0$SIGNAL==7301,])
# View(dat0[dat0$SIGNAL==7381,])
# View(dat0[dat0$SIGNAL==7487,])
# View(dat0[dat0$SIGNAL=="7719A",])
# View(dat0[dat0$SIGNAL %in% c("7719b", "7719B"),])
# View(dat0[dat0$SIGNAL==8102,])

# Summarize signals by stop type
summary_sigs <- dat0 |>
  mutate(
    NEAR_FAR = str_trim(NEAR_FAR),                        # trim spaces/tabs
    SIGNAL_GROUP = str_replace(SIGNAL, "[A-Za-z]$", "")   # remove trailing letters
  ) |>
  group_by(SIGNAL_GROUP) |>
  summarize(
    has_near_side = any(NEAR_FAR=="Near-side (transit vehicles stop BEFORE they reach the intersection)", na.rm=T),
    has_far_side = any(NEAR_FAR=="Far-side (transit vehicles stop AFTER they pass through the intersection)", na.rm=T),
    .groups = "drop"
  ) |>
  mutate(
    summary = case_when(
      has_near_side & has_far_side ~ "Both",
      has_near_side ~ "Near-Side",
      has_far_side ~ "Far-Side",
      TRUE ~ "Unknown"
    )
  )

# Create a count summary
count_sigs <- summary_sigs |>
  count(summary)

# Inspect
# View(summary_sigs)
# View(count_sigs)

# Remove
rm(summary_sigs, count_sigs)
rm(common_cols, non_mutual_cols, all_cols)

##################################################
# Remove unnecessary rows

# Remove test entries by researchers
unique(dat0$PERSON)
table(dat0$PERSON)
values_to_remove <- c("PS", "AS", "FS", "")
# View(dat0[dat0$PERSON %in% values_to_remove,])
dat0 <- dat0[!dat0$PERSON %in% values_to_remove, ]
rm(values_to_remove)

# Remove training entries by students
trem <- c()
# 1 -- 2023-06-12 7041 08:11:03-08:14:00
# View(dat0[dat0$File=="2023-06-12 7041A",])
trem <- c(trem)
# 2 -- 2019-08-05 5093 11:18:11-11:24:11
# View(dat0[dat0$File=="2019-08-05 5093",])
trem <- c(trem)
# 3 -- 2023-06-20 1221 08:45:08-09:10:00
# View(dat0[dat0$File=="2023-06-20 1221",])
trem <- c(trem)
# 4 -- 2023-06-20 7110 08:15:29-08:20:00
# View(dat0[dat0$File=="2023-06-20 7110",])
trem <- c(trem)
# remove rows
dat0 <- dat0[!(dat0$ResponseId %in% trem),]
rm(trem)

# Remove if entry is incomplete (not 100% progress)
table(dat0$Progress)
dat0$ResponseId[which(dat0$Progress!=100)]
# "R_3f5nBtKV4IBhsKB"
ids_to_remove <- dat0$ResponseId[which(dat0$Progress!=100)]
# View(dat0[dat0$ResponseId %in% ids_to_remove,])
dat0 <- dat0[!dat0$ResponseId %in% ids_to_remove, ]
rm(ids_to_remove)

# Remove if entry is a duplicate
table(duplicated(dat0))
dat0$ResponseId[which(duplicated(dat0))]
# none
ids_to_remove <- dat0$ResponseId[which(duplicated(dat0))]
# View(dat0[dat0$ResponseId %in% ids_to_remove,])
dat0 <- dat0[!duplicated(dat0),]
rm(ids_to_remove)

# Inspect
names(dat0)
str(dat0)
summary(dat0)

# Save
saveRDS(dat0, file=file.path(myfpath3, "dat0.rds"))

##################################################
# Format wide: metadata, general information, transit stop event information

# Rename
dat1 <- dat0

# Inspect
tcols <- names(dat1)[!grepl("X", names(dat1))]
tcols
# View(columns[columns$NAME %in% tcols,])
# View(dat1[,tcols])
# str(dat1[,tcols])
rm(tcols)

### Process columns - Metadata
# ResponseId
head(dat1$ResponseId)
# File
table(dat1$File)
dat1$File <- factor(dat1$File)
summary(dat1$File)
# StartDate, EndDate, RecordedDate
dat1$StartDate <- as.POSIXct(dat1$StartDate, tz="America/Denver", format="%Y-%m-%d %H:%M:%S")
dat1$EndDate <- as.POSIXct(dat1$EndDate, tz="America/Denver", format="%Y-%m-%d %H:%M:%S")
dat1$RecordedDate <- as.POSIXct(dat1$RecordedDate, tz="America/Denver", format="%Y-%m-%d %H:%M:%S")
summary(dat1[,c("StartDate", "EndDate", "RecordedDate")])
# Status, IPAddress
table(dat1$Status)
table(dat1$IPAddress)
dat1[,c("Status", "IPAddress")] <- NULL
# Progress, Duration, Finished
summary(dat1$Progress)
summary(dat1$`Duration (in seconds)`)
table(dat1$Finished)
dat1[,c("Progress", "Finished")] <- NULL
names(dat1)[names(dat1)=="Duration (in seconds)"] <- "DurationSec"
# RecipientLastName, RecipeintFirstName, RecipientEmail
table(dat1$RecipientLastName)
table(dat1$RecipientFirstName)
table(dat1$RecipientEmail)
dat1[,c("RecipientLastName", "RecipientFirstName", "RecipientEmail")] <- NULL
# LocationLatitude, LocationLongitude
summary(dat1$LocationLatitude)
summary(dat1$LocationLongitude)
dat1[,c("LocationLatitude", "LocationLongitude")] <- NULL
# ExternalReference, DistributionChannel, UserLanguage
table(dat1$ExternalReference)
table(dat1$DistributionChannel)
table(dat1$UserLanguage)
dat1[,c("ExternalReference", "DistributionChannel", "UserLanguage")] <- NULL

### Process columns - General information
# PERSON
table(dat1$PERSON); table(is.na(dat1$PERSON))
dat1$PERSON <- trimws(dat1$PERSON)
dat1$PERSON <- toupper(dat1$PERSON)
dat1$PERSON <- factor(dat1$PERSON)
summary(dat1$PERSON)
# SIGNAL
table(dat1$SIGNAL); table(is.na(dat1$SIGNAL))
dat1$SIGNAL <- trimws(dat1$SIGNAL)
dat1$SIGNAL <- toupper(dat1$SIGNAL)
dat1$SIGNAL <- factor(dat1$SIGNAL)
summary(dat1$SIGNAL)
# NEAR_FAR
table(dat1$NEAR_FAR); table(is.na(dat1$NEAR_FAR))
dat1$NEAR_FAR <- factor(dat1$NEAR_FAR, 
                        levels=c("Far-side (transit vehicles stop AFTER they pass through the intersection)", 
                                 "Near-side (transit vehicles stop BEFORE they reach the intersection)", 
                                 "Median (transit vehicles from both directions stop in the center of the street)"), 
                        labels=c("FarSide", "NearSide", "Median"))
summary(dat1$NEAR_FAR)
# DATE
table(dat1$DATE); table(is.na(dat1$DATE))
dat1$DATE <- as.Date(dat1$DATE, tz="America/Denver", format="%Y-%m-%d")
summary(dat1$DATE)

# Process columns - Transit stop event information
# T_ARR, T_DEP
table(dat1$T_ARR); table(is.na(dat1$T_ARR)); tail(table(dat1$T_ARR),25)
table(dat1$T_DEP); table(is.na(dat1$T_DEP)); tail(table(dat1$T_DEP),25)
dat1$T_ARR <- as.POSIXct(paste(dat1$DATE, dat1$T_ARR), tz="America/Denver", format="%Y-%m-%d %H:%M:%S")
dat1$T_DEP <- as.POSIXct(paste(dat1$DATE, dat1$T_DEP), tz="America/Denver", format="%Y-%m-%d %H:%M:%S")
summary(dat1$T_ARR)
summary(dat1$T_DEP)
dat1[is.na(dat1$T_DEP),c("File", "ResponseId")]
dat1[is.na(dat1$T_ARR),c("File", "ResponseId")]
# - okay: 2023-06-07 7719A, R_OGOIlFmGXwFG3Hb, end of video
# - okay: 2024-08-27 1094, R_396SdSZ3j93bWvL, start of video
# N_BOARD, N_ALIGHT
table(dat1$N_BOARD); table(is.na(dat1$N_BOARD))
table(dat1$N_ALIGHT); table(is.na(dat1$N_ALIGHT))
dat1$N_BOARD <- ifelse(dat1$N_BOARD=="5+", 5L, as.integer(dat1$N_BOARD))
dat1$N_ALIGHT <- ifelse(dat1$N_ALIGHT=="5+", 5L, as.integer(dat1$N_ALIGHT))
table(dat1$N_BOARD); summary(dat1$N_BOARD)
table(dat1$N_ALIGHT); summary(dat1$N_ALIGHT)
# STOP_EVENT
table(dat1$STOP_EVENT); table(is.na(dat1$STOP_EVENT))
dat1a <- dat1[,1:which(names(dat1)=="STOP_EVENT")]
dat1b <- dat1[,(which(names(dat1)=="STOP_EVENT")+1):ncol(dat1)]
dat1a$STOP_E1 <- grepl("The transit vehicle was delayed by traffic when arriving at the stop", dat1$STOP_EVENT)
dat1a$STOP_E2 <- grepl("The transit vehicle was delayed by traffic when leaving the stop", dat1$STOP_EVENT)
dat1a$STOP_E3 <- grepl("Other traffic was delayed by the transit vehicle while it was stopped", dat1$STOP_EVENT)
dat1a$STOP_E4 <- grepl("Some other vehicles changed lanes in order to pass the stopped transit vehicle", dat1$STOP_EVENT)
dat1a$STOP_E5 <- grepl("The transit vehicle stopped to pick-up/drop-off passengers, but not at or near the stop location", dat1$STOP_EVENT)
dat1a$STOP_E6 <- grepl("The transit vehicle was blocking a driveway or intersection while it was stopped", dat1$STOP_EVENT)
summary(dat1a[,c("STOP_E1", "STOP_E2", "STOP_E3", "STOP_E4", "STOP_E5", "STOP_E6")])
dat1 <- cbind(dat1a, dat1b); rm(dat1a, dat1b)
# STOP_E1_N_VEH, STOP_E2_N_VEH, STOP_E3_N_VEH, STOP_E4_N_VEH
table(dat1$STOP_E1_N_VEH); table(is.na(dat1$STOP_E1_N_VEH))
table(dat1$STOP_E2_N_VEH); table(is.na(dat1$STOP_E2_N_VEH))
table(dat1$STOP_E3_N_VEH); table(is.na(dat1$STOP_E3_N_VEH))
table(dat1$STOP_E4_N_VEH); table(is.na(dat1$STOP_E4_N_VEH))
dat1$STOP_E1_N_VEH <- ifelse(is.na(dat1$STOP_E1_N_VEH), 0L, 
                      ifelse(dat1$STOP_E1_N_VEH=="8+", 8L, 
                      ifelse(dat1$STOP_E1_N_VEH=="", 0L, 
                      as.integer(dat1$STOP_E1_N_VEH))))
dat1$STOP_E2_N_VEH <- ifelse(is.na(dat1$STOP_E2_N_VEH), 0L, 
                      ifelse(dat1$STOP_E2_N_VEH=="8+", 8L, 
                      ifelse(dat1$STOP_E2_N_VEH=="", 0L, 
                      as.integer(dat1$STOP_E2_N_VEH))))
dat1$STOP_E3_N_VEH <- ifelse(is.na(dat1$STOP_E3_N_VEH), 0L, 
                      ifelse(dat1$STOP_E3_N_VEH=="8+", 8L, 
                      ifelse(dat1$STOP_E3_N_VEH=="", 0L, 
                      as.integer(dat1$STOP_E3_N_VEH))))
dat1$STOP_E4_N_VEH <- ifelse(is.na(dat1$STOP_E4_N_VEH), 0L, 
                      ifelse(dat1$STOP_E4_N_VEH=="8+", 8L, 
                      ifelse(dat1$STOP_E4_N_VEH=="", 0L, 
                      as.integer(dat1$STOP_E4_N_VEH))))
table(dat1$STOP_E1_N_VEH); summary(dat1$STOP_E1_N_VEH)
table(dat1$STOP_E2_N_VEH); summary(dat1$STOP_E2_N_VEH)
table(dat1$STOP_E3_N_VEH); summary(dat1$STOP_E3_N_VEH)
table(dat1$STOP_E4_N_VEH); summary(dat1$STOP_E4_N_VEH)
# NOTES_2
unique(dat1$NOTES_2)
# NOTES_END
unique(dat1$NOTES_END)

##################################################
# Check wide for errors or missing data

# Compare File with SIGNAL
# table(dat1$File, dat1$SIGNAL)
# convert factors to character
File_char <- as.character(dat1$File)
Signal_char <- as.character(dat1$SIGNAL)
# extract the last 4 digits from the File column
File_last <- sub(".*\\s(\\d{4,5}[A-Z]?)$", "\\1", File_char)
# identify mismatches
mismatches <- File_last != Signal_char
table(mismatches)
# display just the mismatched rows with File and SIGNAL columns
print(dat1[mismatches, c("File", "SIGNAL")])
# -> none, looks okay
# clean up
rm(File_char, Signal_char, File_last, mismatches)

# Check person by signal 
table(dat1$SIGNAL, dat1$PERSON)
# - if still have training entries for 1221, 5093, 7041, 7110 --> delete rows
# -> looks okay

# Check for duplicate ResponseId
table(dat1$File, duplicated(dat1$ResponseId))
# -> looks okay

# Check for missing near/far
table(is.na(dat1$NEAR_FAR))
# -> looks okay

# Check for correct near/far
# - in most cases, should be just one type for each file, notes: 
#   1094 two different bus stops, one near-side, one far-side
#   5108 two different bus stops, one near-side, one far-side
#   7041A/B two different bus stops, one near-side, one far-side
#   7104 two different bus stops, one near-side, one far-side
#   7290A/B two different bus stops, both near-side
#   7487 two different bus stops, one near-side, one far-side
#   7719A/B two different bus stops, both far-side
#   8102 two different bus stops, one near-side, one far-side
table(dat1$File, dat1$NEAR_FAR)
table(dat1$SIGNAL, dat1$NEAR_FAR)
# -> looks okay

# Check for correct dates
# - usually should be consecutive dates (max of three dates per file)
table(dat1$DATE, dat1$SIGNAL)
# -> looks okay

### Check for time errors

# 1. Transit vehicle arrival vs. departure
dat1$tdiff <- as.numeric(difftime(dat1$T_DEP, dat1$T_ARR, units="mins"))
summary(dat1$tdiff)
sort(dat1$tdiff[which(dat1$tdiff<0.10)]) # less than 0.10 min = 6 sec (or)
sort(dat1$tdiff[which(dat1$tdiff>2.00)]) # more than 2.00 min = 120 sec
tdiff_low <- dat1[which(dat1$tdiff<0.10),c("File", "ResponseId", "tdiff")]
tdiff_high <- dat1[which(dat1$tdiff>2.00),c("File", "ResponseId", "tdiff")]
# - low: these entries are verified as correct: 
ok_low <- c("R_3koJsjkYUNjVZkj", "R_6OVt7FfP68gEAZS", "R_7mRbgVvQkDcMZuE", "R_31vDH3hsxmSa4ON", "R_vBvPyOs3k2yVtD3", "R_1JEqSOS6NmtW2IZ", 
            "R_3GwyQm4zS8G1OIY", "R_1o6Zgr7025qbZiT", "R_1NmOw20L1GGYNNb", "R_1mgVJtO1qlARC5z", "R_sYGvXAbYTZvlx6x", "R_1IgvF278guR5UNu", 
            "R_7qfPqWb2wmpgUE9", "R_7aJjxWtkKpdAE1A", "R_9WGvJKcGXsHhEit", "R_2zOSyEdBtZKbBzH", "R_ZlDQKYecWmZLxER", "R_1hPU0oSbsIPEu4d", 
            "R_7JyFlKdtSnae6s3", "R_7VnK2eyDPf4bxId", "R_2gbdSu3XsOOdzIE", "R_3H1OIiIHZnZMtFS", "R_6h7uD4t3ZJ8ePv3", "R_7OOmSb7fHHrEfOf", 
            "R_7w4hy1wbOyeDafX", "R_1QyGd5hewR93Oth", "R_7BapFv56y4fOreR", "R_3r36TZCJbCaMW0m", "R_76bD1FQLpDj8iCR", "R_7fBtCMjeooKdx9q", 
            "R_7i8L886VK3IohnV", "R_6O2rt4v2Dt9IWWZ", "R_747WIex5cTBJgRh", "R_31aYyKomFsUmiDT", "R_7uKsd1ExmLdVWet", "R_7DMsuZ3QGVFnyIi", 
            "R_6hmTYjFxcojQeN1", "R_671EM87wYnRkpIG", "R_1ViMkOkfSrc48WB", "R_5DyIzZXIRuYxNeN", "R_57JpkYTq3D1hPRW", "R_3GD1afMvKWvBcqe",
            "R_3x7CMxrxJdKSrfP", "R_7C2pQXAIg5vJDcB", "R_7fdtfMFRS3ADIPP", "R_6bKxYYXprIxZc1X", "R_6zSk9a9edmucBt9", "R_7i1PbxYU5wyZxqV", 
            "R_3gXPaLot3BzRmVP", "R_3Ok7BoKwZsKMzD5", "R_1sO7pgmCkYajHqh", "R_1QyIZXBQ8LHpzzp", "R_2ya32KHigMxFIPD", "R_5z789NL629huIh6")
tdiff_low$Okay <- ifelse(tdiff_low$ResponseId %in% ok_low, T, F)
# - high: these entries are verified as correct:
ok_high <- c("R_3QJZRZEO6sTUAb3", "R_3Jk3LgcpNUPDts9", "R_27ND9MSyTMpgbbl", "R_3d048hD8N7VSfKn", "R_1FG94TRAKRRFm5N", "R_1lzxYsPOq59mdnx", 
             "R_11GLosdFreAQBvC", "R_24jV3bj44VyltLy", "R_1FsKJdDApqdPSg5", "R_3CJFsJ3qAfUg4xP", "R_25MJqe4WwbCS48h", "R_3O19uWobcwdCkMn", 
             "R_2zOsAz7lquuPKkE", "R_2dARcb2KmSab6ji", "R_yF0M6aXGFaft6ut", "R_2RVSVmYQ9KuiiIB", "R_10PApnT9MTDoiTZ", "R_2PgIxkGEkgE1veh", 
             "R_2R47QE2RY8wmRwK", "R_3QL591Wqkx3cTcu", "R_3kdj1fuKU2Apo7L", "R_3mlP2qh950UONcS", "R_27EGfUbvn7sry3N", "R_3m1ZswNEKvtuYg4", 
             "R_3RebC0mQqIfTcVm", "R_tQVBrRFqbK0zF85", "R_3D6iqPS0jeUhzjK", "R_3nvDFEDUILNHwqs", "R_3lz4MogcGeXbxUE", "R_tYvtEgixNlIZXvr", 
             "R_Rt5kg8xhdLQ4OQx", "R_1GkRDgWc8JXTgkh", "R_12ElS2EHTvqS0Az", "R_3HFYaSY6fnAMQYH", "R_RQaQF5oyRRKS8mZ", "R_cBkHTr8ZMjr4ASJ", 
             "R_XgDBjDnDGVXIyZ3", "R_1esZGF4JmOU9jX8", "R_2uCKhRc8zYAgTxf", "R_BRkBIql8GVg42lz", "R_3J4xuFnWZY2DCo2", "R_2P0PjEMUZZeC5Av", 
             "R_6PPKvXqo8jN6aEF", "R_1BmQLMtqsTQyg72", "R_1h6Dtlq4RjvAgqo", "R_70IBaR3R6LeLIgQ", "R_7Ip9CbRzgsCK8Zr", "R_7JoOHPKicSWZu8x", 
             "R_62Drxe9OFGQk7I9", "R_6wyV5KyJnguQJSV", "R_6OdnBcWhR7WyHxd", "R_7MJRAZvALuyzt7H", "R_1iUObLGjitz676C", "R_3QYjgjH6e2TjmWl", 
             "R_6DMGWZsnpht1pe8", "R_33fpA3JS2wrr7Xm", "R_5ottwnG8P1tvvDH", "R_6dElD8uRuuYNiEq", "R_3WPAE5VRZamypff", "R_1cUOARKUvmmsOCl", 
             "R_5SSwxPCNRMzCCC5", "R_6A2d85L8FclAjXb", "R_3rGLLYcl9qjNx8M", "R_5GNBVV00KbJkC2J", "R_7KGVqAtIW2js4xu", "R_125feM0xZbUHZYD")
tdiff_high$Okay <- ifelse(tdiff_high$ResponseId %in% ok_high, T, F)
# check
tdiff_low[tdiff_low$Okay==F,]
tdiff_high[tdiff_high$Okay==F,]
# -> none, looks okay
# remove
rm(tdiff_low, ok_low, tdiff_high, ok_high)
dat1$tdiff <- NULL

# Check for timestamps out-of-order
# View(dat1[order(dat1$RecordedDate),])
for (i in unique(dat1$File)) {
  temp <- dat1[dat1$File==i,]
  temp$tdiff <- 0L
  if (nrow(temp)>1) {
    temp$tdiff[2:nrow(temp)] <- temp$T_ARR[2:nrow(temp)] - temp$T_ARR[1:(nrow(temp)-1)]
    if (any(temp$tdiff<0)) {
      print(as.character(i))
      print(temp$ResponseId[which(temp$tdiff<0)])
    }
  }
  rm(temp)
}; rm(i)
# - these ones are okay: 
#   2019-08-05 5093: R_1GC9p20Ptlobekp, R_21iHCJWBNKQKXOB
#   2019-08-05 7381: R_2eQK0RTsuKpnZ32, R_2WYgP0d8FZkoY72, R_1gp7IhipM5JbC8C, R_06dsbt4ozBsKO1b, R_2Sk8y2AeteF31Ra
#   2021-11-16 7234: R_3fw55wZFAXwFcWr
#   2023-06-07 5056: R_7V6nESFdTeI7kpr
#   2023-06-07 7719A: R_25MJqe4WwbCS48h, R_1feoCJIiifEao9A
#   2023-06-07 7719B: R_6wVUtkpOxcoSBP3
#   2023-06-12 7041B: R_1ln0YbJdzSJ1vWx
#   2023-06-20 1221: R_1O3ucF2VewoOilP, R_5DnMuIf9RbDKqf3, R_72iRdwunjDV7nyN, R_31aYyKomFsUmiDT, R_6e96RCHW5G8xI9U, R_6wyV5KyJnguQJSV
#   2024-06-28 5030: R_6COcsPohHxkA96S
#   2024-08-27 1094: R_73BXE0bwZPQxMjJ
#   2024-10-18 7104: R_1QofHR74OITlnJB, R_57vZNtvqepYsoXk

# Re-order columns
which(names(dat1) %in% c("ResponseId", "File", "StartDate", "EndDate", "RecordedDate", "DurationSec"))
dat1 <- dat1[c("ResponseId", "File", "StartDate", "EndDate", "RecordedDate", "DurationSec", names(dat1)[7:ncol(dat1)])]
# - column name starts with(out) a number
tcols1 <- names(dat1)[!grepl("^[[:digit:]]+", names(dat1))]
tcols2 <- names(dat1)[grepl("^[[:digit:]]+", names(dat1))]
dat1 <- dat1[,c(tcols1, tcols2)]
rm(tcols1, tcols2)

# Inspect
names(dat1)
str(dat1)
summary(dat1)

# Save
saveRDS(dat1, file=file.path(myfpath3, "dat1.rds"))

##################################################
# Reshape wide to long

# Inspect
tcols <- names(dat1)[!grepl("^[[:digit:]]+", names(dat1))]
tcols
str(dat1[,tcols])
summary(dat1[,tcols])
rm(tcols)

# Create long
# initialize columns
tcol1 <- names(dat1)[!grepl("^[[:digit:]]+", names(dat1))]
tcol2 <- unique(gsub("^\\d+_", "", names(dat1)[grepl("^\\d+_", names(dat1))]))

# Initialize first person (e.g., "1_")
dat2 <- dat1[, tcol1]
dat2$PER <- 1
dat2 <- cbind(dat2, dat1[, paste0("1_", tcol2), drop = FALSE])
names(dat2) <- c(tcol1, "PER", tcol2)

# Loop for other persons (e.g., "2_", "3_", ...)
for (i in 2:10) {
  tdat <- dat1[, tcol1]
  tdat$PER <- i
  tdat <- cbind(tdat, dat1[, paste0(i, "_", tcol2), drop = FALSE])
  names(tdat) <- c(tcol1, "PER", tcol2)
  dat2 <- rbind(dat2, tdat)
  rm(tdat)
}; rm(i)

# Adjust rows
# inspect
table(rowSums(is.na(dat2[,tcol2])))
table(rowSums(dat2[,tcol2]=="", na.rm=T))
table(rowSums(is.na(dat2[,tcol2])) + rowSums(dat2[,tcol2]=="", na.rm=T))
# replace "" with NA
dat2[,tcol2][dat2[,tcol2]==""] <- NA
table(rowSums(is.na(dat2[,tcol2])))
# keep rows with not all NA
dat2 <- dat2[rowSums(is.na(dat2[,tcol2]))!=length(tcol2),]

# Inspect
names(dat2)
str(dat2)
summary(dat2)

# Remove
rm(tcol1, tcol2)

# Save
saveRDS(dat2, file=file.path(myfpath3, "dat2.rds"))

##################################################
# Format long: pedestrian information, crossing behaviors, vehicle conflicts

# Rename
dat3 <- dat2

# Inspect
tcols <- names(dat3)[which(names(dat3)=="PER"):ncol(dat3)]
tcols
# View(columns[columns$NAME %in% paste("1", tcols, sep="_"),])
# View(dat3[,tcols])
str(dat3[,tcols])
rm(tcols)

# Process columns - Pedestrian information
# PER
table(dat3$PER)
# ALIGHT_BOARD
table(dat3$ALIGHT_BOARD); table(is.na(dat3$ALIGHT_BOARD))
dat3$ALIGHT_BOARD <- factor(dat3$ALIGHT_BOARD, levels=c("Boarding (getting ON)", "Alighting (getting OFF)"), labels=c("Boarding", "Alighting"))
dat3$ALIGHT_BOARD <- relevel(dat3$ALIGHT_BOARD, ref="Alighting")
summary(dat3$ALIGHT_BOARD)
# AGE
table(dat3$AGE); table(is.na(dat3$AGE))
dat3$AGE <- factor(dat3$AGE, levels=c("Child", "Teenager", "Young adult", "Middle-aged adult", "Older adult", "Adult of unknown age"))
dat3$AGE <- relevel(dat3$AGE, ref="Adult of unknown age")
summary(dat3$AGE)
# GENDER
table(dat3$GENDER); table(is.na(dat3$GENDER))
dat3$GENDER <- factor(dat3$GENDER, levels=c("Female", "Male", "Unknown gender"))
dat3$GENDER <- relevel(dat3$GENDER, ref="Unknown gender")
summary(dat3$GENDER)
# PER_OTHER
table(dat3$PER_OTHER); table(is.na(dat3$PER_OTHER))
dat3a <- dat3[,1:which(names(dat3)=="PER_OTHER")]
dat3b <- dat3[,(which(names(dat3)=="PER_OTHER")+1):ncol(dat3)]
dat3a$PER_OTHER_PERSON <- grepl("Traveling with", dat3$PER_OTHER)
dat3a$PER_OTHER_CARRYLOAD <- grepl("Bicycle", dat3$PER_OTHER)
dat3a$PER_OTHER_STROLLER <- grepl("Stroller", dat3$PER_OTHER)
dat3a$PER_OTHER_WHEELCHAIR <- grepl("Wheelchair", dat3$PER_OTHER)
dat3a$PER_OTHER_SKATEBOARD <- grepl("Skateboard", dat3$PER_OTHER)
dat3a$PER_OTHER_SCOOTER <- grepl("Scooter", dat3$PER_OTHER)
dat3a$PER_OTHER_BICYCLE <- grepl("Skateboard", dat3$PER_OTHER)
dat3a$PER_OTHER_DISTRACTED <- grepl("Distracted", dat3$PER_OTHER)
dat3a$PER_OTHER_OTHER <- grepl("Other", dat3$PER_OTHER)
summary(dat3a[,c("PER_OTHER_PERSON", "PER_OTHER_CARRYLOAD", "PER_OTHER_STROLLER", 
                 "PER_OTHER_WHEELCHAIR", "PER_OTHER_SKATEBOARD", "PER_OTHER_SCOOTER", 
                 "PER_OTHER_BICYCLE", "PER_OTHER_DISTRACTED", "PER_OTHER_OTHER")])
dat3 <- cbind(dat3a, dat3b); rm(dat3a, dat3b)
# PER_OTHER_17_TEXT, Address responses as needed
other_response <- dat3 %>%
  filter(!is.na(PER_OTHER_17_TEXT)) %>%
  select(File, ResponseId, PER_OTHER_17_TEXT)
rm(other_response)
# TP_ALIGHT, TP_DEP
table(dat3$TP_ALIGHT); table(is.na(dat3$TP_ALIGHT))
table(dat3$TP_DEP); table(is.na(dat3$TP_DEP))
dat3$TP_ALIGHT[dat3$TP_ALIGHT==0] <- NA
dat3$TP_DEP[dat3$TP_DEP==0] <- NA
dat3$TP_ALIGHT <- as.POSIXct(paste(dat3$DATE, dat3$TP_ALIGHT), tz="America/Denver", format="%Y-%m-%d %H:%M:%S")
dat3$TP_DEP <- as.POSIXct(paste(dat3$DATE, dat3$TP_DEP), tz="America/Denver", format="%Y-%m-%d %H:%M:%S")
summary(dat3$TP_ALIGHT)
summary(dat3$TP_DEP)
dat2[which(!is.na(dat2$TP_ALIGHT) & is.na(dat3$TP_ALIGHT)),]
dat2[which(!is.na(dat2$TP_DEP) & is.na(dat3$TP_DEP)),]
# PER_CROSS_A
table(dat3$PER_CROSS_A); table(is.na(dat3$PER_CROSS_A))
dat3$PER_CROSS_A <- factor(dat3$PER_CROSS_A, 
                           levels=c("At an intersection or a marked crossing", 
                                    "Mid-block, away from an intersection or a marked crossing", 
                                    "Did not cross a street; turned a corner instead", 
                                    "Did not cross a street; walked away from the transit stop along the street", 
                                    "Did not cross a street; went to an adjacent land use (business, home, etc.)", 
                                    "Did not leave; stayed at the transit stop to board another transit vehicle", 
                                    "Cannot see from the view of the video", "Other"))
dat3$PER_CROSS_A <- relevel(dat3$PER_CROSS_A, ref="At an intersection or a marked crossing")
summary(dat3$PER_CROSS_A)
# PER_CROSS_A_8_TEXT, address responses as needed
other_response <- dat3 %>%
  distinct(PER_CROSS_A_8_TEXT, .keep_all = TRUE) %>%
  select(File, ResponseId, PER_CROSS_A_8_TEXT)
rm(other_response)
# PER_CROSS_B
table(dat3$PER_CROSS_B); table(is.na(dat3$PER_CROSS_B))
dat3$PER_CROSS_B <- factor(dat3$PER_CROSS_B, 
                           levels=c("At an intersection or a marked crossing", 
                                    "Mid-block, away from an intersection or a marked crossing", 
                                    "Did not cross a street; turned a corner instead", 
                                    "Did not cross a street; walked towards the transit stop along the street", 
                                    "Did not cross a street; came from an adjacent land use (business, home, etc.)", 
                                    "Did not arrive; was already at the transit stop after alighting another transit vehicle", 
                                    "Cannot see from the view of the video", "Other"))
dat3$PER_CROSS_B <- relevel(dat3$PER_CROSS_B, ref="At an intersection or a marked crossing")
summary(dat3$PER_CROSS_B)
# PER_CROSS_B_8_TEXT, address responses as needed
other_response <- dat3 %>%
  distinct(PER_CROSS_B_8_TEXT, .keep_all = TRUE) %>%
  select(File, ResponseId, PER_CROSS_B_8_TEXT)
rm(other_response)
# TP_ARR, TP_BOARD
table(dat3$TP_ARR); table(is.na(dat3$TP_ARR))
table(dat3$TP_BOARD); table(is.na(dat3$TP_BOARD))
dat3$TP_ARR[dat3$TP_ARR==0] <- NA
dat3$TP_BOARD[dat3$TP_BOARD==0] <- NA
dat3$TP_ARR <- as.POSIXct(paste(dat3$DATE, dat3$TP_ARR), tz="America/Denver", format="%Y-%m-%d %H:%M:%S")
dat3$TP_BOARD <- as.POSIXct(paste(dat3$DATE, dat3$TP_BOARD), tz="America/Denver", format="%Y-%m-%d %H:%M:%S")
summary(dat3$TP_ARR)
summary(dat3$TP_BOARD)
dat2[which(!is.na(dat2$TP_ARR) & is.na(dat3$TP_ARR)),]
dat2[which(!is.na(dat2$TP_BOARD) & is.na(dat3$TP_BOARD)),]
# - okay: 2023-06-20 1221, R_71bldPNCPquUIbw, start of video
# Process columns - Crossing behaviors
# PER_STREET
table(dat3$PER_STREET); table(is.na(dat3$PER_STREET))
dat3$PER_STREET <- factor(dat3$PER_STREET, 
                          levels=c("The main street (the same street where the transit stop was located)", 
                                   "A side street (a different street from the one where the transit stop was located)", 
                                   "Other"), labels=c("MainSt", "SideSt", "Other"))
dat3$PER_STREET <- relevel(dat3$PER_STREET, ref="MainSt")
summary(dat3$PER_STREET)
# PER_STREET_5_TEXT
unique(dat3$PER_STREET_5_TEXT)
# TP_CROSS1, TP_CROSS2
table(dat3$TP_CROSS1); table(is.na(dat3$TP_CROSS1))
table(dat3$TP_CROSS2); table(is.na(dat3$TP_CROSS2))
dat3$TP_CROSS1 <- as.POSIXct(paste(dat3$DATE, dat3$TP_CROSS1), tz="America/Denver", format="%Y-%m-%d %H:%M:%S")
dat3$TP_CROSS2 <- as.POSIXct(paste(dat3$DATE, dat3$TP_CROSS2), tz="America/Denver", format="%Y-%m-%d %H:%M:%S")
summary(dat3$TP_CROSS1)
summary(dat3$TP_CROSS2)
dat2[which(!is.na(dat2$TP_CROSS1) & is.na(dat3$TP_CROSS1)),]
dat2[which(!is.na(dat2$TP_CROSS2) & is.na(dat3$TP_CROSS2)),]
# PER_BEH
table(dat3$PER_BEH); table(is.na(dat3$PER_BEH))
dat3a <- dat3[,1:which(names(dat3)=="PER_BEH")]
dat3b <- dat3[,(which(names(dat3)=="PER_BEH")+1):ncol(dat3)]
dat3a$PER_BEH_OUTSIDE <- grepl("Was outside of the crosswalk markings for most if not all of the crossing", dat3$PER_BEH)
dat3a$PER_BEH_CHNGSPD <- grepl("Changed speed", dat3$PER_BEH)
dat3a$PER_BEH_PAUSMID <- grepl("Paused in the middle of the street", dat3$PER_BEH)
dat3a$PER_BEH_DISTRAC <- grepl("Seemed distracted by phone or something else", dat3$PER_BEH)
dat3a$PER_BEH_CRFRONT <- grepl("Crossed just in front of the transit vehicle", dat3$PER_BEH)
dat3a$PER_BEH_CRBEHIN <- grepl("Crossed just behind the transit vehicle", dat3$PER_BEH)
summary(dat3a[,c("PER_BEH_OUTSIDE", "PER_BEH_CHNGSPD", "PER_BEH_PAUSMID", 
                 "PER_BEH_DISTRAC", "PER_BEH_CRFRONT", "PER_BEH_CRBEHIN")])
dat3 <- cbind(dat3a, dat3b); rm(dat3a, dat3b)
# NOTES_3A, address responses as needed
other_response <- dat3 %>%
  distinct(NOTES_3A, .keep_all = TRUE) %>%
  select(File, ResponseId, NOTES_3A)
rm(other_response)
# PER_VEH_CONF
table(dat3$PER_VEH_CONF); table(is.na(dat3$PER_VEH_CONF))
dat3$PER_VEH_CONF <- factor(dat3$PER_VEH_CONF, levels=c("Yes", "No", "Cannot see from the view of the video"))
dat3$PER_VEH_CONF <- relevel(dat3$PER_VEH_CONF, ref="No")
summary(dat3$PER_VEH_CONF)

# Process columns - Vehicle conflicts
# CONF_LOC
table(dat3$CONF_LOC); table(is.na(dat3$CONF_LOC))
dat3$CONF_LOC <- factor(dat3$CONF_LOC, 
                        levels=c("When the person was entering (or starting to cross) the street", 
                                 "When the person was in the middle of the street", 
                                 "When the person was exiting (or finishing crossing) the street", 
                                 "Other"), labels=c("Entering", "Middle", "Exiting", "Other"))
dat3$CONF_LOC <- relevel(dat3$CONF_LOC, ref="Entering")
summary(dat3$CONF_LOC)
# CONF_LOC_4_TEXT
unique(dat3$CONF_LOC_4_TEXT)
# VEH_MOVE
table(dat3$VEH_MOVE); table(is.na(dat3$VEH_MOVE))
dat3$VEH_MOVE <- factor(dat3$VEH_MOVE, levels=c("Turning right", "Turning left", "Driving straight", "Other"))
dat3$VEH_MOVE <- relevel(dat3$VEH_MOVE, ref="Driving straight")
summary(dat3$VEH_MOVE)
# VEH_MOVE_4_TEXT
unique(dat3$VEH_MOVE_4_TEXT)
# VEH_TYPE
table(dat3$VEH_TYPE); table(is.na(dat3$VEH_TYPE))
dat3$VEH_TYPE <- factor(dat3$VEH_TYPE, levels=c("Large truck (semi-truck, delivery truck, etc.)", 
                                                "Van (mini-van, sprinter van, etc.)", "SUV", 
                                                "Sedan", "Pickup truck", "Vehicle pulling a trailer", 
                                                "Motorcycle", "Other"))
dat3$VEH_TYPE <- relevel(dat3$VEH_TYPE, ref="Sedan")
summary(dat3$VEH_TYPE)
# VEH_TYPE_9_TEXT
unique(dat3$VEH_TYPE_9_TEXT)
# TP_CONF, TV_CONF
table(dat3$TP_CONF); table(is.na(dat3$TP_CONF))
table(dat3$TV_CONF); table(is.na(dat3$TV_CONF))
dat3$TP_CONF <- as.POSIXct(paste(dat3$DATE, dat3$TP_CONF), tz="America/Denver", format="%Y-%m-%d %H:%M:%S")
dat3$TV_CONF <- as.POSIXct(paste(dat3$DATE, dat3$TV_CONF), tz="America/Denver", format="%Y-%m-%d %H:%M:%S")
summary(dat3$TP_CONF)
summary(dat3$TV_CONF)
dat2[which(!is.na(dat2$TP_CONF) & is.na(dat3$TP_CONF)),]
dat2[which(!is.na(dat2$TV_CONF) & is.na(dat3$TV_CONF)),]
# VEH_REACT
table(dat3$VEH_REACT); table(is.na(dat3$VEH_REACT))
dat3$VEH_REACT <- factor(dat3$VEH_REACT, levels=c("Driver fully stopped", "Driver slowed down", "Driver sped up", "Driver swerved", 
                                                  "No obvious reaction", "Cannot see from the view of the video"))
dat3$VEH_REACT <- relevel(dat3$VEH_REACT, ref="No obvious reaction")
summary(dat3$VEH_REACT)
# PED_REACT
table(dat3$PED_REACT); table(is.na(dat3$PED_REACT))
dat3$PED_REACT <- factor(dat3$PED_REACT, levels=c("Stopped and waited for the vehicle", "Slowed down to avoid collision", 
                                                  "Sped up or ran to avoid collision", "Changed direction", 
                                                  "No obvious reaction", "Cannot see from the view of the video"))
dat3$PED_REACT <- relevel(dat3$PED_REACT, ref="No obvious reaction")
summary(dat3$PED_REACT)
# NOTES_3B, address responses as needed
other_response <- dat3 %>%
  distinct(NOTES_3B, .keep_all = TRUE) %>%
  select(File, ResponseId, NOTES_3B)
rm(other_response)

# PER_LOOP
table(dat3$PER_LOOP); table(is.na(dat3$PER_LOOP))
dat3$PER_LOOP <- factor(dat3$PER_LOOP, levels=c("Yes", "No"))
dat3$PER_LOOP <- relevel(dat3$PER_LOOP, ref="No")
summary(dat3$PER_LOOP)
dat2[which(is.na(dat2$PER_LOOP)),]

##################################################
# Check long for missing data

# Check # alighting/boarding vs. # people recorded
# - Note: Some stop events were made with no passengers alighting or boarding. 
#   For these entries, a record had to be made to complete the form. 
#   These passenger events were deleted, but the stop event was retained. 
# - 1. Tabulate boarding/alighting events by response ID and file
ttab <- table(dat3$ResponseId, dat3$ALIGHT_BOARD); head(ttab); tail(ttab)
ttab <- as.data.frame(ttab); head(ttab); tail(ttab)
tdat <- dat1[,c("File", "ResponseId", "N_BOARD", "N_ALIGHT")]
tdat <- merge(tdat, ttab[ttab$Var2=="Boarding",], by.x="ResponseId", by.y="Var1", all.x=T, all.y=T)
tdat <- merge(tdat, ttab[ttab$Var2=="Alighting",], by.x="ResponseId", by.y="Var1", all.x=T, all.y=T)
tdat[,c("Var2.x", "Var2.y")] <- NULL
names(tdat)[names(tdat)=="Freq.x"] <- "Rows_Board"
names(tdat)[names(tdat)=="Freq.y"] <- "Rows_Alight"
tdat <- tdat[,c("File", "ResponseId", "N_BOARD", "N_ALIGHT", "Rows_Board", "Rows_Alight")]
tdat <- tdat[match(dat1$ResponseId, tdat$ResponseId),]
# - 2. Check match of B/A count and number of B/A passenger events
table(tdat$N_BOARD, tdat$Rows_Board)
table(tdat$N_ALIGHT, tdat$Rows_Alight)
tboard <- tdat[tdat$N_BOARD != tdat$Rows_Board,]
tboard <- tboard[!(tboard$N_BOARD==5 & tboard$Rows_Board>=5),]
tboard
talight <- tdat[tdat$N_ALIGHT != tdat$Rows_Alight,]
talight <- talight[!(talight$N_ALIGHT==5 & talight$Rows_Alight>=5),]
talight
# -> none, looks okay
table(is.na(dat3$ALIGHT_BOARD), dat3$N_BOARD + dat3$N_ALIGHT)
# -> looks okay
# remove
rm(ttab, tdat, tboard, talight)

# Copy TP_ALIGHT to TP_DEP if missing, for alighting passengers
# - Note: Instructions said to omit TP_DEP if same as TP_ALIGHT
table(dat3$ALIGHT_BOARD, is.na(dat3$TP_ALIGHT))
table(dat3$ALIGHT_BOARD, is.na(dat3$TP_BOARD))
table(dat3$ALIGHT_BOARD, is.na(dat3$TP_DEP))
twhich <- which(dat3$ALIGHT_BOARD=="Alighting" & is.na(dat3$TP_DEP))
dat3[twhich, "TP_DEP"] <- dat3[twhich, "TP_ALIGHT"]
rm(twhich)

# Check for incomplete pedestrian information
# - ALIGHT_BOARD, AGE, GENDER
temp <- dat3[!is.na(dat3$ALIGHT_BOARD),]
summary(temp$ALIGHT_BOARD)
summary(temp$AGE)
summary(temp$GENDER)
t0 <- temp[rowSums(is.na(temp[,c("ALIGHT_BOARD", "AGE", "GENDER")]))>0,]
t0[,c("File", "ResponseId", "PER", "ALIGHT_BOARD", "AGE", "GENDER")]
# -> none, looks okay
# - if alight: TP_ALIGHT, TP_DEP, PER_CROSS_A
#   (okay if TP_DEP is blank, b/c same as TP_ALIGHT)
tempa <- temp[temp$ALIGHT_BOARD=="Alighting",]
summary(tempa$TP_ALIGHT)
summary(tempa$TP_DEP)
summary(tempa$PER_CROSS_A)
tA <- tempa[rowSums(is.na(tempa[,c("TP_ALIGHT", "TP_DEP", "PER_CROSS_A")]))>0,]
tA[,c("File", "ResponseId", "PER", "TP_ALIGHT", "TP_DEP", "PER_CROSS_A")]
# -> none, looks okay
# - if board: PER_CROSS_B, TP_ARR, TP_BOARD
tempb <- temp[temp$ALIGHT_BOARD=="Boarding",]
summary(tempb$PER_CROSS_B)
summary(tempb$TP_ARR)
summary(tempb$TP_BOARD)
tB <- tempb[rowSums(is.na(tempb[,c("PER_CROSS_B", "TP_ARR", "TP_BOARD")]))>0,]
tB[,c("File", "ResponseId", "PER", "PER_CROSS_B", "TP_ARR", "TP_BOARD")]
# okay b/c before video started: 
#   R_30l6Qsxl91CQDSv 1, R_1Q0GHfmRruv3ftl 1, R_71bldPNCPquUIbw 1, R_5oyShXboNaLjxDv 1, 
#   R_1FUMqsNw563D8gu 1, R_1FUMqsNw563D8gu 2, R_1FUMqsNw563D8gu 3
# -> looks okay
rm(temp, tempa, tempb, t0, tA, tB)

# Check for incomplete crossing information
# if PER_CROSSA/B=="At an intersection or a marked crossing" | "Mid-block, away from an intersection or a marked crossing"
tf <- c("At an intersection or a marked crossing", "Mid-block, away from an intersection or a marked crossing")
# - PER_STREET, TP_CROSS1, TP_CROSS2, PER_VEH_CONF
#   (okay if TP_CROSS1/2 are blank, if cannot see, but needs to be consistent)
temp <- dat3[(!is.na(dat3$PER_CROSS_A) & dat3$PER_CROSS_A %in% tf) | 
             (!is.na(dat3$PER_CROSS_B) & dat3$PER_CROSS_B %in% tf),]
# View(temp)
summary(temp$PER_STREET)
summary(temp$TP_CROSS1)
summary(temp$TP_CROSS2)
summary(is.na(temp$TP_CROSS1) & is.na(temp$TP_CROSS2))
summary(temp$PER_VEH_CONF)
t0 <- temp[rowSums(is.na(temp[,c("PER_STREET", "PER_VEH_CONF")]))>0,]
t0[,c("File", "ResponseId", "PER", "PER_STREET", "PER_VEH_CONF")]
# -> none, looks okay
# table(temp$File, is.na(temp$TP_CROSS1))
# table(temp$File, is.na(temp$TP_CROSS2))
# table(temp$File, is.na(temp$TP_CROSS1), temp$PER_STREET)
# table(temp$File, is.na(temp$TP_CROSS2), temp$PER_STREET)
table(temp$File, rowSums(is.na(temp[,c("TP_CROSS1", "TP_CROSS2")])), temp$PER_STREET)
# - the following are okay b/c of the video views don't show full crossing
#   2019-08-05 7381:                    okay for SideSt=1
#   2020-06-22 1021:                    okay for SideSt=1
#   2021-11-16 7234: okay for MainSt=1, okay for SideSt=1
#   2022-05-26 1229: okay for MainSt=1, okay for SideSt=1
#   2023-06-12 7110: okay for MainSt=1
#   2023-06-20 1221: okay for MainSt=1, okay for SideSt=1
#   2023-06-20 4522: okay for MainSt=1
#   2023-06-20 7110: okay for MainSt=1
#   2024-06-26 1229:                    okay for SideSt=1
#   2024-06-28 5030:                    okay for SideSt=1
#   2024-06-28 7084:                    okay for SideSt=1
#   2024-08-27 1225:                    okay for SideSt=1
#   2024-09-03 6311:                    okay for SideSt=1
#   2024-09-05 5056:                    okay for SideSt=1
#   2024-10-18 7104:                    okay for SideSt=1
# -> looks okay
rm(temp, t0, tf)

# Check for incomplete conflict information
# if PER_VEH_CONF=="Yes"
# - CONF_LOC, VEH_MOVE, VEH_TYPE, TP_CONF, TV_CONF, VEH_REACT, PED_REACT
#   (okay if TP/TV_CONF are blank, if cannot see, but needs to be consistent)
temp <- dat3[!is.na(dat3$PER_VEH_CONF) & dat3$PER_VEH_CONF=="Yes",]
# View(temp)
summary(temp$CONF_LOC)
summary(temp$VEH_MOVE)
summary(temp$VEH_TYPE)
summary(temp$TP_CONF)
summary(temp$TV_CONF)
summary(temp$VEH_REACT)
summary(temp$PED_REACT)
t0 <- temp[rowSums(is.na(temp[,c("CONF_LOC", "VEH_MOVE", "VEH_TYPE", "VEH_REACT", "PED_REACT")]))>0,]
t0[,c("File", "ResponseId", "PER", "CONF_LOC", "VEH_MOVE", "VEH_TYPE", "VEH_REACT", "PED_REACT")]
# -> none, looks okay
t1 <- temp[temp$VEH_REACT=="Cannot see from the view of the video",]
t2 <- temp[temp$PED_REACT=="Cannot see from the view of the video",]
t1[,c("File", "ResponseId", "PER", "CONF_LOC", "VEH_MOVE", "VEH_TYPE", "VEH_REACT", "PED_REACT")]
t2[,c("File", "ResponseId", "PER", "CONF_LOC", "VEH_MOVE", "VEH_TYPE", "VEH_REACT", "PED_REACT")]
# okay: R_25NhizlCgx6rVDX 1
table(temp$File, is.na(temp$TP_CONF))
table(temp$File, is.na(temp$TV_CONF))
# -> none, looks okay
rm(temp, t0, t1, t2)

##################################################
# Check long for time errors

# Alighting, assumed sequence: 
# - T_ARR: transit vehicle arrives at transit stop
# - TP_ALIGHT: person stops off of the transit vehicle
# - (T_DEP: transit vehicle departs from transit stop)
# - TP_DEP: person leaves transit stop
# - TP_CROSS1: person starts crossing the street
# - TP_CONF: pedestrian at conflict point
# - (TV_CONF: motor vehicle at conflict point)
# - TP_CROSS2: person finishes crossing the street

# Boarding, assumed sequence: 
# - TP_CROSS1: person starts crossing the street
# - TP_CONF: pedestrian at conflict point
# - (TV_CONF: motor vehicle at conflict point)
# - TP_CROSS2: person finishes crossing the street
# - TP_ARR: person arrives at transit stops
# - (T_ARR: transit vehicle arrives at transit stop)
# - TP_BOARD: person steps on to the transit vehicle
# - T_DEP: transit vehicle departs from transit stop

# A1: Transit vehicle arrives -> Passenger alights (TP_ALIGHT - T_ARR)
# A2: Passenger alights -> Transit vehicle departs (T_DEP - TP_ALIGHT)
# A3: Passenger alights -> Passenger leaves stop (TP_DEP - TP_ALIGHT)
# A4: Passenger leaves stop -> Person starts crossing (TP_CROSS1 - TP_DEP)

# B1: Person finishes crossing -> Passenger arrives stop (TP_ARR - TP_CROSS2)
# B2: Passenger arrives stop -> Passenger boards (TP_BOARD - TP_ARR)
# B3: Transit vehicle arrives -> Passenger boards (TP_BOARD - T_ARR)
# B4: Passenger boards -> Transit vehicle departs (T_DEP - TP_BOARD)

# C1: Person starts crossing -> Person finishes crossing (TP_CROSS2 - TP_CROSS1)
# C2: Person starts crossing -> Pedestrian at conflict point (TP_CONF - TP_CROSS1)
# C3: Pedestrian at conflict point -> Person finishes crossing (TP_CROSS2 - TP_CONF)
# C4: Pedestrian at conflict point <-> Motor vehicle at conflict point (TV_CONF - TP_CONF)

# A1: Transit vehicle arrives -> Passenger alights (TP_ALIGHT - T_ARR)
dat3$tdiff <- as.numeric(difftime(dat3$TP_ALIGHT, dat3$T_ARR, units="mins"))
summary(dat3$tdiff)
sort(dat3$tdiff[which(dat3$tdiff<0.00)]) # less than 0.00 min = 0 sec (or)
sort(dat3$tdiff[which(dat3$tdiff>2.00)]) # more than 2.00 min = 120 sec
tdiff1 <- dat3[which(dat3$tdiff<0.00),c("File", "ResponseId", "PER", "tdiff")]
tdiff2 <- dat3[which(dat3$tdiff>2.00),c("File", "ResponseId", "PER", "tdiff")]
# these entries are verified as correct: 
ok1 <- c()
tdiff1$Okay <- ifelse(paste(tdiff1$ResponseId, tdiff1$PER) %in% ok1, T, F)
ok2 <- c(paste("R_6wyV5KyJnguQJSV",3), paste("R_24jV3bj44VyltLy",4))
tdiff2$Okay <- ifelse(paste(tdiff2$ResponseId, tdiff2$PER) %in% ok2, T, F)
# check
tdiff1[tdiff1$Okay==F,]
tdiff2[tdiff2$Okay==F,]
# -> none, looks okay
# remove
rm(tdiff1, ok1, tdiff2, ok2)
dat3$tdiff <- NULL

# A2: Passenger alights -> Transit vehicle departs (T_DEP - TP_ALIGHT)
dat3$tdiff <- as.numeric(difftime(dat3$T_DEP, dat3$TP_ALIGHT, units="mins"))
summary(dat3$tdiff)
sort(dat3$tdiff[which(dat3$tdiff<0.00)]) # less than 0.00 min = 0 sec (or)
sort(dat3$tdiff[which(dat3$tdiff>5.00)]) # more than 5.00 min = 300 sec
tdiff1 <- dat3[which(dat3$tdiff<0.00),c("File", "ResponseId", "PER", "tdiff")]
tdiff2 <- dat3[which(dat3$tdiff>5.00),c("File", "ResponseId", "PER", "tdiff")]
# these entries are verified as correct: 
ok1 <- c()
tdiff1$Okay <- ifelse(paste(tdiff1$ResponseId, tdiff1$PER) %in% ok1, T, F)
ok2 <- c(paste("R_24jV3bj44VyltLy",1), paste("R_24jV3bj44VyltLy",2), paste("R_24jV3bj44VyltLy",3))
tdiff2$Okay <- ifelse(paste(tdiff2$ResponseId, tdiff2$PER) %in% ok2, T, F)
# check
tdiff1[tdiff1$Okay==F,]
tdiff2[tdiff2$Okay==F,]
# -> none, looks okay
# remove
rm(tdiff1, ok1, tdiff2, ok2)
dat3$tdiff <- NULL

# A3: Passenger alights -> Passenger leaves stop (TP_DEP - TP_ALIGHT)
dat3$tdiff <- as.numeric(difftime(dat3$TP_DEP, dat3$TP_ALIGHT, units="mins"))
summary(dat3$tdiff)
sort(dat3$tdiff[which(dat3$tdiff<0.00)]) # less than 0.00 min = 0 sec (or)
sort(dat3$tdiff[which(dat3$tdiff>10.00)]) # more than 10.00 min = 600 sec
tdiff1 <- dat3[which(dat3$tdiff<0.00),c("File", "ResponseId", "PER", "tdiff")]
tdiff2 <- dat3[which(dat3$tdiff>10.00),c("File", "ResponseId", "PER", "tdiff")]
# these entries are verified as correct: 
ok1 <- c()
tdiff1$Okay <- ifelse(paste(tdiff1$ResponseId, tdiff1$PER) %in% ok1, T, F)
ok2 <- c(paste("R_1M6BExT5xhm3UOG",1), paste("R_7Ib2dZXPgr8a1ED",1), paste("R_7V6nESFdTeI7kpr",1), 
         paste("R_72iRdwunjDV7nyN",1), paste("R_7haMnctfpnOwEZw",2), paste("R_1O3ucF2VewoOilP",2), 
         paste("R_1O3ucF2VewoOilP",3), paste("R_5DnMuIf9RbDKqf3",3), paste("R_6wyV5KyJnguQJSV",3))
tdiff2$Okay <- ifelse(paste(tdiff2$ResponseId, tdiff2$PER) %in% ok2, T, F)
# check
tdiff1[tdiff1$Okay==F,]
tdiff2[tdiff2$Okay==F,]
# -> none, looks okay
# remove
rm(tdiff1, ok1, tdiff2, ok2)
dat3$tdiff <- NULL

# A4: Passenger leaves stop -> Person starts crossing (TP_CROSS1 - TP_DEP)
dat3$tdiff <- as.numeric(difftime(dat3$TP_CROSS1, dat3$TP_DEP, units="mins"))
summary(dat3$tdiff)
sort(dat3$tdiff[which(dat3$tdiff<0.00)]) # less than 0.00 min = 0 sec (or)
sort(dat3$tdiff[which(dat3$tdiff>5.00)]) # more than 5.00 min = 300 sec
tdiff1 <- dat3[which(dat3$tdiff<0.00),c("File", "ResponseId", "PER", "tdiff")]
tdiff2 <- dat3[which(dat3$tdiff>5.00),c("File", "ResponseId", "PER", "tdiff")]
# these entries are verified as correct: 
ok1 <- c()
tdiff1$Okay <- ifelse(paste(tdiff1$ResponseId, tdiff1$PER) %in% ok1, T, F)
ok2 <- c(paste("R_7ZOeweqdDHQ5yNf",1), paste("R_3DKBkKPpj4MUycx",2), paste("R_79iF8MUu3dy4znP",2))
tdiff2$Okay <- ifelse(paste(tdiff2$ResponseId, tdiff2$PER) %in% ok2, T, F)
# check
tdiff1[tdiff1$Okay==F,]
tdiff2[tdiff2$Okay==F,]
# -> none, looks okay
# remove
rm(tdiff1, ok1, tdiff2, ok2)
dat3$tdiff <- NULL

# B1: Person finishes crossing -> Passenger arrives stop (TP_ARR - TP_CROSS2)
dat3$tdiff <- as.numeric(difftime(dat3$TP_ARR, dat3$TP_CROSS2, units="mins"))
summary(dat3$tdiff)
sort(dat3$tdiff[which(dat3$tdiff<0.00)]) # less than 0.00 min = 0 sec (or)
sort(dat3$tdiff[which(dat3$tdiff>5.00)]) # more than 5.00 min = 300 sec
tdiff1 <- dat3[which(dat3$tdiff<0.00),c("File", "ResponseId", "PER", "tdiff")]
tdiff2 <- dat3[which(dat3$tdiff>5.00),c("File", "ResponseId", "PER", "tdiff")]
# these entries are verified as correct: 
ok1 <- c()
tdiff1$Okay <- ifelse(paste(tdiff1$ResponseId, tdiff1$PER) %in% ok1, T, F)
ok2 <- c(paste("R_2ARVor4BGzADvwS",1), paste("R_1onhdbEN2J36kWk",2), paste("R_bqDmZU6Bd5VsfXH",4))
tdiff2$Okay <- ifelse(paste(tdiff2$ResponseId, tdiff2$PER) %in% ok2, T, F)
# check
tdiff1[tdiff1$Okay==F,]
tdiff2[tdiff2$Okay==F,]
# -> none, looks okay
# remove
rm(tdiff1, ok1, tdiff2, ok2)
dat3$tdiff <- NULL

# B2: Passenger arrives stop -> Passenger boards (TP_BOARD - TP_ARR)
dat3$tdiff <- as.numeric(difftime(dat3$TP_BOARD, dat3$TP_ARR, units="mins"))
summary(dat3$tdiff)
sort(dat3$tdiff[which(dat3$tdiff<0.00)]) # less than 0.00 min = 0 sec (or)
sort(dat3$tdiff[which(dat3$tdiff>30.0)]) # more than 30.0 min = 1800 sec
tdiff1 <- dat3[which(dat3$tdiff<0.00),c("File", "ResponseId", "PER", "tdiff")]
tdiff2 <- dat3[which(dat3$tdiff>30.0),c("File", "ResponseId", "PER", "tdiff")]
# these entries are verified as correct: 
ok1 <- c()
tdiff1$Okay <- ifelse(paste(tdiff1$ResponseId, tdiff1$PER) %in% ok1, T, F)
ok2 <- c(paste("R_1lichGzd5CurPiE",1), paste("R_28S23D7CK63MyQn",1), paste("R_2Bn8frQ9m5Mwkdr",1), 
         paste("R_1FG94TRAKRRFm5N",1), paste("R_1QfmSMbtHAnAiCr",1), paste("R_yOpJbVBXWMs8eZ3",1), 
         paste("R_19mDHcLTPe6mTJ7",1), paste("R_77UYWxsgC787dmh",1), paste("R_6ymjgC9bxsz8zAd",1), 
         paste("R_54q6JLynqQVlKvl",1), paste("R_3E4KtRvKgrEvubY",1), paste("R_1qfVFoGvhIdXf69",1), 
         paste("R_9B0FMq0wQVe8c3n",1), paste("R_69WyJFIremYEy1b",1), paste("R_62y8SivhGQq18OO",1), 
         paste("R_3ei4l0R6cCZDhuN",1), paste("R_5ANNjGx6W2DseY1",1), paste("R_62y8SivhGQq18OO",2), 
         paste("R_1QfmSMbtHAnAiCr",2))
tdiff2$Okay <- ifelse(paste(tdiff2$ResponseId, tdiff2$PER) %in% ok2, T, F)
# check
tdiff1[tdiff1$Okay==F,]
tdiff2[tdiff2$Okay==F,]
# -> none, looks okay
# remove
rm(tdiff1, ok1, tdiff2, ok2)
dat3$tdiff <- NULL

# B3: Transit vehicle arrives -> Passenger boards (TP_BOARD - T_ARR)
dat3$tdiff <- as.numeric(difftime(dat3$TP_BOARD, dat3$T_ARR, units="mins"))
summary(dat3$tdiff)
sort(dat3$tdiff[which(dat3$tdiff<0.00)]) # less than 0.00 min = 0 sec (or)
sort(dat3$tdiff[which(dat3$tdiff>2.00)]) # more than 2.00 min = 120 sec
tdiff1 <- dat3[which(dat3$tdiff<0.00),c("File", "ResponseId", "PER", "tdiff")]
tdiff2 <- dat3[which(dat3$tdiff>2.00),c("File", "ResponseId", "PER", "tdiff")]
# these entries are verified as correct: 
ok1 <- c()
tdiff1$Okay <- ifelse(paste(tdiff1$ResponseId, tdiff1$PER) %in% ok1, T, F)
ok2 <- c(paste("R_25MJqe4WwbCS48h",1), paste("R_27EGfUbvn7sry3N",1), paste("R_1BmQLMtqsTQyg72",1), 
         paste("R_5SSwxPCNRMzCCC5",1), paste("R_yF0M6aXGFaft6ut",2), paste("R_27EGfUbvn7sry3N",2), 
         paste("R_3WPAE5VRZamypff",2), paste("R_3CJFsJ3qAfUg4xP",3), paste("R_27EGfUbvn7sry3N",3), 
         paste("R_3Jk3LgcpNUPDts9",8), paste("R_3Jk3LgcpNUPDts9",9))
tdiff2$Okay <- ifelse(paste(tdiff2$ResponseId, tdiff2$PER) %in% ok2, T, F)
# check
tdiff1[tdiff1$Okay==F,]
tdiff2[tdiff2$Okay==F,]
# -> none, looks okay
# remove
rm(tdiff1, ok1, tdiff2, ok2)
dat3$tdiff <- NULL

# B4: Passenger boards -> Transit vehicle departs (T_DEP - TP_BOARD)
dat3$tdiff <- as.numeric(difftime(dat3$T_DEP, dat3$TP_BOARD, units="mins"))
summary(dat3$tdiff)
sort(dat3$tdiff[which(dat3$tdiff<0.00)]) # less than 0.00 min = 0 sec (or)
sort(dat3$tdiff[which(dat3$tdiff>3.00)]) # more than 3.00 min = 180 sec
tdiff1 <- dat3[which(dat3$tdiff<0.00),c("File", "ResponseId", "PER", "tdiff")]
tdiff2 <- dat3[which(dat3$tdiff>3.00),c("File", "ResponseId", "PER", "tdiff")]
# these entries are verified as correct: 
ok1 <- c()
tdiff1$Okay <- ifelse(paste(tdiff1$ResponseId, tdiff1$PER) %in% ok1, T, F)
ok2 <- c(paste("R_62Drxe9OFGQk7I9",1), paste("R_1cUOARKUvmmsOCl",1), paste("R_1cUOARKUvmmsOCl",2), 
         paste("R_1cUOARKUvmmsOCl",3))
tdiff2$Okay <- ifelse(paste(tdiff2$ResponseId, tdiff2$PER) %in% ok2, T, F)
# check
tdiff1[tdiff1$Okay==F,]
tdiff2[tdiff2$Okay==F,]
# -> none, looks okay
# remove
rm(tdiff1, ok1, tdiff2, ok2)
dat3$tdiff <- NULL

# C1: Person starts crossing -> Person finishes crossing (TP_CROSS2 - TP_CROSS1)
dat3$tdiff <- as.numeric(difftime(dat3$TP_CROSS2, dat3$TP_CROSS1, units="mins"))
summary(dat3$tdiff)
sort(dat3$tdiff[which(dat3$tdiff<0.05)]) # less than 0.05 min = 3 sec (or)
sort(dat3$tdiff[which(dat3$tdiff>1.00)]) # more than 1.00 min = 60 sec
tdiff1 <- dat3[which(dat3$tdiff<0.05),c("File", "ResponseId", "PER", "tdiff")]
tdiff2 <- dat3[which(dat3$tdiff>1.00),c("File", "ResponseId", "PER", "tdiff")]
# these entries are verified as correct: 
ok1 <- c()
tdiff1$Okay <- ifelse(paste(tdiff1$ResponseId, tdiff1$PER) %in% ok1, T, F)
ok2 <- c(paste("R_3PUDvmgGXw3sv5H",2))
tdiff2$Okay <- ifelse(paste(tdiff2$ResponseId, tdiff2$PER) %in% ok2, T, F)
# check
tdiff1[tdiff1$Okay==F,]
tdiff2[tdiff2$Okay==F,]
# -> none, looks okay
# remove
rm(tdiff1, ok1, tdiff2, ok2)
dat3$tdiff <- NULL

# C2: Person starts crossing -> Pedestrian at conflict point (TP_CONF - TP_CROSS1)
dat3$tdiff <- as.numeric(difftime(dat3$TP_CONF, dat3$TP_CROSS1, units="mins"))
summary(dat3$tdiff)
sort(dat3$tdiff[which(dat3$tdiff<0.00)]) # less than 0.00 min = 0 sec (or)
sort(dat3$tdiff[which(dat3$tdiff>1.00)]) # more than 1.00 min = 60 sec
tdiff1 <- dat3[which(dat3$tdiff<0.00),c("File", "ResponseId", "PER", "tdiff")]
tdiff2 <- dat3[which(dat3$tdiff>1.00),c("File", "ResponseId", "PER", "tdiff")]
# these entries are verified as correct: 
ok1 <- c()
tdiff1$Okay <- ifelse(paste(tdiff1$ResponseId, tdiff1$PER) %in% ok1, T, F)
ok2 <- c(paste("R_3PUDvmgGXw3sv5H",2))
tdiff2$Okay <- ifelse(paste(tdiff2$ResponseId, tdiff2$PER) %in% ok2, T, F)
# check
tdiff1[tdiff1$Okay==F,]
tdiff2[tdiff2$Okay==F,]
# -> none, looks okay
# remove
rm(tdiff1, ok1, tdiff2, ok2)
dat3$tdiff <- NULL

# C3: Pedestrian at conflict point -> Person finishes crossing (TP_CROSS2 - TP_CONF)
dat3$tdiff <- as.numeric(difftime(dat3$TP_CROSS2, dat3$TP_CONF, units="mins"))
summary(dat3$tdiff)
sort(dat3$tdiff[which(dat3$tdiff<0.00)]) # less than 0.00 min = 0 sec (or)
sort(dat3$tdiff[which(dat3$tdiff>1.00)]) # more than 1.00 min = 60 sec
tdiff1 <- dat3[which(dat3$tdiff<0.00),c("File", "ResponseId", "PER", "tdiff")]
tdiff2 <- dat3[which(dat3$tdiff>1.00),c("File", "ResponseId", "PER", "tdiff")]
# these entries are verified as correct: 
ok1 <- c()
tdiff1$Okay <- ifelse(paste(tdiff1$ResponseId, tdiff1$PER) %in% ok1, T, F)
ok2 <- c()
tdiff2$Okay <- ifelse(paste(tdiff2$ResponseId, tdiff2$PER) %in% ok2, T, F)
# check
tdiff1[tdiff1$Okay==F,]
tdiff2[tdiff2$Okay==F,]
# -> none, looks okay
# remove
rm(tdiff1, ok1, tdiff2, ok2)
dat3$tdiff <- NULL

# C4: Pedestrian at conflict point <-> Motor vehicle at conflict point (TV_CONF - TP_CONF)
dat3$tdiff <- as.numeric(difftime(dat3$TV_CONF, dat3$TP_CONF, units="mins"))
summary(dat3$tdiff)
sort(dat3$tdiff[which(dat3$tdiff<(-0.084))]) # less than -0.084 min = -5 sec (or)
sort(dat3$tdiff[which(dat3$tdiff>(+0.084))]) # more than +0.084 min = +5 sec
tdiff1 <- dat3[which(dat3$tdiff<(-0.084)),c("File", "ResponseId", "PER", "tdiff")]
tdiff2 <- dat3[which(dat3$tdiff>(+0.084)),c("File", "ResponseId", "PER", "tdiff")]
# these entries are verified as correct: 
ok1 <- c(paste("R_3FDpo5ZGhCN9dzR",1), paste("R_1kHA2Mr4PL5lgEq",2))
tdiff1$Okay <- ifelse(paste(tdiff1$ResponseId, tdiff1$PER) %in% ok1, T, F)
ok2 <- c(paste("R_3fjDLYP4Mi7O4A9",3), paste("R_7jPuhkyVagM1uAV",1), paste("R_7GP82nEMqjwWlVv", 2))
tdiff2$Okay <- ifelse(paste(tdiff2$ResponseId, tdiff2$PER) %in% ok2, T, F)
# check
tdiff1[tdiff1$Okay==F,]
tdiff2[tdiff2$Okay==F,]
# -> none, looks okay
# remove
rm(tdiff1, ok1, tdiff2, ok2)
dat3$tdiff <- NULL

# Inspect
str(dat3)
summary(dat3)
table(dat3$REMOVE)

# Save
saveRDS(dat3, file=file.path(myfpath3, "dat3.rds"))

##################################################
# Final format and save

# Create wide
datwide <- dat1
str(datwide)
summary(datwide)

# Create long
datlong <- dat3
str(datlong)
summary(datlong)

# Save files
saveRDS(datwide, file.path(myfpath3, "datwide.rds"))
saveRDS(datlong, file.path(myfpath3, "datlong.rds"))
write.csv(datwide, file.path(myfpath3, "datwide.csv"), row.names=F)
write.csv(datlong, file.path(myfpath3, "datlong.csv"), row.names=F)

##################################################
# Clean up

# Remove
rm(myfpath2, myfpath3, myfiles, columns)
rm(dat0, dat1, dat2, dat3)
rm(datwide, datlong)
gc()

##################################################
# END
##################################################