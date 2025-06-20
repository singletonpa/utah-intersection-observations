#######################################
# Project:  UDOT-20.313 Ped violations
# Authors:  Patrick A. Singleton (patrick.singleton@usu.edu)
#           Sadie Boyer (sadie.boyer@usu.edu)
# File:     03_Add_Time_Weather_Location.R
# Date:     2022 Spring, Summer
# About:    Add temporal, weather, and location information
########################################

########################################
# Notes

# Open R project first, then open this R script

# Major edits
# 2022-02-16 SB  created
# 2022-03-10 SB  updated
# 2022-08-12 PAS updated: added time data from old script, adjusted processing
# 2022-09-05 PAS updated: updated weather dataset
# 2022-11-29 PAS updated: added hourly weather data from IEMRE: https://mesonet.agron.iastate.edu/iemre/
# 2023-01-13 PAS updated: 
# - fixed minor error in missing hourly weather data during switch to/from DST
# - removed daily weather dataset
# - added more data to CrossingInfo spreadsheet
# - created a new sigs dataset (from UGRC, SLD, Census)

# Load libraries
library("readxl")
library("sf")

########################################
# Load data

# Load dat
dat <- readRDS(file.path("Data", "dat02.rds"))
names(dat)
str(dat)
summary(dat)

########################################
# Add temporal variables

# Add day-of-week
dat$Weekday <- format(dat$TimeWaitArr, "%a")
dat$Weekday <- factor(dat$Weekday, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
summary(dat$Weekday)
dat$Weekend <- ifelse(dat$Weekday %in% c("Sat", "Sun"), T, F)
summary(dat$Weekend)

# Add time-of-day
dat$Hour <- format(dat$TimeWaitArr, "%H")
dat$Hour <- factor(dat$Hour)
summary(dat$Hour)
dat$TOD1 <- factor(dat$Hour, labels=c("0002", "0002", "0002", "0305", "0305", "0305", 
                                      "0608", "0608", "0608", "0911", "0911", "0911", 
                                      "1214", "1214", "1214", "1517", "1517", "1517", 
                                      "1820", "1820", "1820", "2123", "2123", "2123"))
summary(dat$TOD1)
dat$TOD2 <- factor(dat$TOD1, labels=c("0005", "0005", "0611", "0611", "1217", "1217", "1823", "1823"))
summary(dat$TOD2)
dat$TOD1 <- relevel(dat$TOD1, ref="1517")
dat$TOD2 <- relevel(dat$TOD2, ref="1217")
dat$AMPeak <- ifelse(dat$Hour %in% c("07", "08"), T, F)
dat$PMPeak <- ifelse(dat$Hour %in% c("16", "17"), T, F)
summary(dat$AMPeak)
summary(dat$PMPeak)

########################################
# Add hourly weather data
# https://mesonet.agron.iastate.edu/iemre/
# example: https://mesonet.agron.iastate.edu/iemre/hourly/2019-01-17/41.73906976832612/-111.83476703255893/json

# Packages
library(httr)
library(jsonlite)

# Example
req <- GET("https://mesonet.agron.iastate.edu/iemre/hourly/2019-01-17/41.73906976832612/-111.83476703255893/json")
wth <- fromJSON(content(req, as="text"))$data
wth$valid_local <- gsub("T", " ", wth$valid_local)
wth$valid_local <- as.POSIXct(wth$valid_local, format="%Y-%m-%d %H:%M", tz="America/Denver")
wth[,c("valid_utc", "valid_local")] <- NULL
addnames <- names(wth)
rm(req, wth)

# Add columns to dat
dat[,addnames] <- NA

# Load locations
locinfo <- read_excel(path=file.path("Data", "CrossingInfo.xlsx"), sheet=1)
locinfo <- locinfo[,c("Folder", "SignalID", "Latitude", "Longitude")]

# Loop through folders
# check
table(unique(dat$Folder) %in% locinfo$Folder)
table(locinfo$Folder %in% unique(dat$Folder))
# for each folder
for (i in 1:nrow(locinfo)) {
  print(paste0("Working on folder ", locinfo$Folder[i]))
  myjs <- which(dat$Folder == locinfo$Folder[i])
  # get hourly weather data for all days
  mylat <- locinfo$Latitude[i]
  mylon <- locinfo$Longitude[i]
  mydates <- sort(unique(as.Date(dat[myjs,"TimeWaitArr"])))
  myreq <- paste0("https://mesonet.agron.iastate.edu/iemre/hourly/", mydates[1], "/", mylat, "/", mylon, "/json")
  temp <- fromJSON(content(GET(myreq), as="text", encoding="UTF-8"))$data
  temp$valid_local <- gsub("T", " ", temp$valid_local)
  temp$valid_local <- as.POSIXct(temp$valid_local, format="%Y-%m-%d %H:%M", tz="America/Denver")
  rm(myreq)
  if (length(mydates)>1) {
    for (d in 2:length(mydates)) {
      myreq <- paste0("https://mesonet.agron.iastate.edu/iemre/hourly/", mydates[d], "/", mylat, "/", mylon, "/json")
      dtemp <- fromJSON(content(GET(myreq), as="text", encoding="UTF-8"))$data
      dtemp$valid_local <- gsub("T", " ", dtemp$valid_local)
      dtemp$valid_local <- as.POSIXct(dtemp$valid_local, format="%Y-%m-%d %H:%M", tz="America/Denver")
      temp <- rbind(temp, dtemp)
      rm(myreq, dtemp)
    }; rm(d)
  }
  rm(mylat, mylon, mydates)
  # add hourly weather data for each event
  for (j in myjs) {
    ttemp <- temp
    ttemp$timediff <- abs(difftime(ttemp$valid_local,dat[j,"TimeWaitArr"], units="mins"))
    thr <- which(ttemp$timediff==min(ttemp$timediff, na.rm=T))[1]
    dat[j,addnames] <- ttemp[thr,addnames]
    rm(ttemp, thr)
  }; rm(j)
  # cleanup
  rm(temp, myjs)
}; rm(i)

# Inspect
summary(dat$air_temp_f)
boxplot(dat$air_temp_f)
summary(dat$hourly_precip_in)
boxplot(dat$hourly_precip_in)
# View(dat[is.na(dat$air_temp_f),])
# - only 4 missing, okay

# Remove
rm(locinfo, addnames)

########################################
# Add crossing and signal location information

# Load crossing and signal location information (manual)
crossing_info <- read_excel(path=file.path("Data", "CrossingInfo.xlsx"), sheet=1)
dat_ci <- crossing_info
# inspect
names(dat_ci)
str(dat_ci)
summary(dat_ci)
# process columns
names(dat_ci)[2] <- "Signal"
dat_ci$NIntLegs <- as.factor(dat_ci$NIntLegs)
dat_ci$CrossType <- factor(dat_ci$CrossType, levels=c("Standard", "Continental"))
dat_ci[,c("MedType", "Notes")] <- NULL

# Load crossing and signal location information (processed)
sigs <- readRDS(file.path("Data", "Location", "sigs.rds"))
dat_s <- st_drop_geometry(sigs)
# inspect
names(dat_s)
str(dat_s)
summary(dat_s)
# process columns
dat_s[,c("Latitude", "Longitude")] <- NULL

# Merge and sort data
dat_cis <- merge(dat_ci, dat_s, by="Signal", all.x=T, all.y=F)
dat_cis <- dat_cis[order(dat_cis$Folder),]
row.names(dat_cis) <- NULL
dat <- merge(dat, dat_cis, by=c("Folder", "Signal"), all.x=T, all.y=F)

# Inspect dat_ci
str(dat[,names(dat_ci)])
summary(dat[,names(dat_ci)])
# - CrossAADT missing 387
table(dat[is.na(dat$CrossAADT),"Signal"])
# - signals: 4130, 5299, 5305, 6146, 7099, 7218, 7332, 8222, 8627
# model it
vmod <- lm(log(I(CrossAADT/1000)) ~ CrossDist + CrossLane + CrossDist*CrossLane, data=crossing_info)
summary(vmod)
temp <- crossing_info[,c("Folder", "CrossAADT")]
temp$PredAADT <- exp(predict(vmod, newdata=crossing_info))*1000
plot(temp$PredAADT, temp$CrossAADT)
data.frame(temp)
# - most predicted to be 2000-3000, so assume 2000 for missing
dat$CrossAADT[is.na(dat$CrossAADT)] <- 2000
table(dat[is.na(dat$CrossAADT),"Signal"])
rm(vmod, temp)

# Inspect
str(dat[,names(dat_s)])
summary(dat[,names(dat_s)])
# no variables with missing data, good

# Remove
rm(crossing_info, dat_ci, sigs, dat_s, dat_cis)

########################################
# Save files

# Inspect
names(dat)
str(dat)
summary(dat)

# Save as dat03
saveRDS(dat, file.path("Data", "dat03.rds"))
write.csv(dat, file.path("Data", "dat03.csv"), row.names=F)

# Remove
rm(dat)
gc()

########################################
# END
########################################