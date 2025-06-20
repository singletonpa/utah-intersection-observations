#######################################
# Project:  UDOT-19.312 Right turn safety
# Authors:  Alyssa Gaither (alyssa.gaither@usu.edu)
#           Atul Subedi (atul.subedi@usu.edu)
#           Patrick Singleton (patrick.singleton@usu.edu)
# File:     Conflict-Location Combination.R
# Date:     2022 Summer, Fall, 2023 Spring
# About:    Combine Conflict Data with Location data
########################################

########################################
# Notes:

# Major Edits
# 2022-07-25 created AG
# 2022-10-10 updated PAS
# - updated to simplify and work with new location data
# - added signal status, with link to other scripts with functions
# 2023-02-03 updated PAS
# - added process to get ped/veh signal status for each event
#   based on first/second crosswalk and pre/post encroachment
# 2023-04-02 updated PAS: fixed encroachment time calculations in prior dataset
# 2023-04-08 updated AS: added weather data from IEMRE, script from ped violations project

# Load packages
#library()

########################################
# Load Data

# Corner-Data Combination file
location_info <- readRDS(file.path("Analysis", "Conflict Analysis", "Location Data Assembly", "datlocation.rds"))
str(location_info)

# Wide and long conflict data
datwide <- readRDS(file.path("Analysis", "Conflict Analysis", "Data Cleaning", "datwide.rds"))
datlong <- readRDS(file.path("Analysis", "Conflict Analysis", "Data Cleaning", "datlong.rds"))
str(datwide)
str(datlong)

########################################
# Merge data

# Create folder for location_info
location_info$Folder <- paste(as.Date(location_info$Start, tz="America/Denver"), location_info$Signal, sep=" ")

# Check correspondance of folders
location_info$Folder[!(location_info$Folder %in% datwide$Folder)]
unique(datwide$Folder)[!(unique(datwide$Folder) %in% location_info$Folder)]

# Combine with wide data
combinedWide <- merge(datwide, location_info, by="Folder", all.x=T, all.y=F)
# View(combinedWide)

# Combine with long data
combinedLong <- merge(datlong, location_info, by="Folder", all.x=T, all.y=F)
# View(combinedLong)

########################################
# Get signal status information

# Load general signal data functions
source(file.path("Analysis", "Conflict Analysis", "Combine All Data", "funs.R"))

# Load special signal status functions
source(file.path("Analysis", "Conflict Analysis", "Combine All Data", "signal_status_funs.R"))
# - sigstatus: info about ped or veh signal status
#   * inputs: signal data, signal id, phase, mode, timestamp
#   * outputs: ped or veh signal status

# Rename
cwide <- combinedWide
clong <- combinedLong

# Initialize
tnm1 <- c("PEDCP1", "PEDCP2", "VEHCP")
tnm2 <- c("ped1", "ped2", "veh")
tnames <- paste(rep(tnm1, each=length(tnm2)), tnm2, sep="_")
clong[,tnames] <- NA

# Start
myfpath <- file.path("Data", "UDOT Right Turn Videos", "2_Checked")
myfile <- "ControllerEventLogs.csv"
myfolders <- unique(clong$Folder, na.rm=T)
# myevents <- c(0,21,22,23,45,89,90)

# Iterate over folders
# this takes some time (several minutes)
for(i in 1:length(myfolders)) {
  # Initialize
  # i <- 26
  # i <- 1
  # i <- 2
  myfold <- as.character(myfolders[i])
  print(myfold)
  
  # Get signal data
  print("...loading signal data...")
  sigdata <- getsig(file.path(myfpath, myfold, myfile))
  # sigdata <- subsig(sigdata, events=myevents) # removed b/c need other events for violation
  
  # Iterate over observations
  print("...getting signal status info...")
  for(j in which(clong$Folder==myfold)) {
    # j <- 2
    tp_p1 <- clong$P_P1[j]
    tp_p2 <- clong$P_P2[j]
    tp_rt <- clong$P_RT[j]
    if (myfold=="2021-11-18 7122") { tp_rt <- c(tp_rt,5) } # RT overlap phase 5
    # Get signal status
    clong$PEDCP1_ped1[j] <- sigstatus(sigdata, sigid=clong$Signal.ID[j], phase=tp_p1, mode="ped", dtime=clong$Ped.TS.at.CP.1[j])$sig_status
    clong$PEDCP1_ped2[j] <- sigstatus(sigdata, sigid=clong$Signal.ID[j], phase=tp_p2, mode="ped", dtime=clong$Ped.TS.at.CP.1[j])$sig_status
    clong$PEDCP1_veh[j]  <- sigstatus(sigdata, sigid=clong$Signal.ID[j], phase=tp_rt, mode="veh", dtime=clong$Ped.TS.at.CP.1[j])$sig_status
    clong$PEDCP2_ped1[j] <- sigstatus(sigdata, sigid=clong$Signal.ID[j], phase=tp_p1, mode="ped", dtime=clong$Ped.TS.at.CP.2[j])$sig_status
    clong$PEDCP2_ped2[j] <- sigstatus(sigdata, sigid=clong$Signal.ID[j], phase=tp_p2, mode="ped", dtime=clong$Ped.TS.at.CP.2[j])$sig_status
    clong$PEDCP2_veh[j]  <- sigstatus(sigdata, sigid=clong$Signal.ID[j], phase=tp_rt, mode="veh", dtime=clong$Ped.TS.at.CP.2[j])$sig_status
    clong$VEHCP_ped1[j]  <- sigstatus(sigdata, sigid=clong$Signal.ID[j], phase=tp_p1, mode="ped", dtime=clong$TS.CP[j])$sig_status
    clong$VEHCP_ped2[j]  <- sigstatus(sigdata, sigid=clong$Signal.ID[j], phase=tp_p2, mode="ped", dtime=clong$TS.CP[j])$sig_status
    clong$VEHCP_veh[j]   <- sigstatus(sigdata, sigid=clong$Signal.ID[j], phase=tp_rt, mode="veh", dtime=clong$TS.CP[j])$sig_status
    rm(tp_p1, tp_p2, tp_rt)
  }; rm(j)
  
  # Cleanup
  rm(myfold, sigdata)
}; rm(i)

# Process signal status information
apply(clong[,grep("_ped1", names(clong), value=T)], 2, table, simplify=F)
apply(clong[,grep("_ped2", names(clong), value=T)], 2, table, simplify=F)
apply(clong[,grep("_veh", names(clong), value=T)], 2, table, simplify=F)
clong$PEDCP_veh <- NA
clong$PEDCP_ped <- NA
clong$VEHCP_ped <- NA
# clong$VEHCP_veh <- clong$VEHCP_veh
for (i in 1:nrow(clong)) {
  if (!is.na(clong$Encroachment_Time[i])) {
    if (clong$Encroachment.Car.Before.Ped[i]) {
      clong$PEDCP_veh[i] <- clong$PEDCP1_veh[i]
      if (clong$Crosswalk.ID[i] == "First crosswalk") {
        clong$PEDCP_ped[i] <- clong$PEDCP1_ped1[i]
        clong$VEHCP_ped[i] <- clong$VEHCP_ped1[i]
      } else if (clong$Crosswalk.ID[i] == "Second crosswalk") {
        clong$PEDCP_ped[i] <- clong$PEDCP1_ped2[i]
        clong$VEHCP_ped[i] <- clong$VEHCP_ped2[i]
      }
    } else if (clong$Encroachment.Car.Same.Ped[i] | clong$Encroachment.Car.After.Ped[i]) {
      clong$PEDCP_veh[i] <- clong$PEDCP2_veh[i]
      if (clong$Crosswalk.ID[i] == "First crosswalk") {
        clong$PEDCP_ped[i] <- clong$PEDCP2_ped1[i]
        clong$VEHCP_ped[i] <- clong$VEHCP_ped1[i]
      } else if (clong$Crosswalk.ID[i] == "Second crosswalk") {
        clong$PEDCP_ped[i] <- clong$PEDCP2_ped2[i]
        clong$VEHCP_ped[i] <- clong$VEHCP_ped2[i]
      }
    }
  }
}; rm(i)
apply(clong[,c("PEDCP_ped", "PEDCP_veh", "VEHCP_ped", "VEHCP_veh")], 2, table, simplify=F)
clong$PEDCP_ped <- factor(clong$PEDCP_ped, levels=c("Walk", "Flashing Don't Walk", "Solid Don't Walk"), labels=c("Walk", "FDW", "SDW"))
clong$PEDCP_veh <- factor(clong$PEDCP_veh, levels=c("Green", "Yellow", "Red"))
clong$VEHCP_ped <- factor(clong$VEHCP_ped, levels=c("Walk", "Flashing Don't Walk", "Solid Don't Walk"), labels=c("Walk", "FDW", "SDW"))
clong$VEHCP_veh <- factor(clong$VEHCP_veh, levels=c("Green", "Yellow", "Red"))
summary(clong[,c("PEDCP_ped", "PEDCP_veh", "VEHCP_ped", "VEHCP_veh")])

# Inspect/check
for (i in 1:length(myfolders)) {
  print(myfolders[i])
  print(summary(clong[clong$Folder==myfolders[i],c("PEDCP_ped", "PEDCP_veh", "VEHCP_ped", "VEHCP_veh")]))
}; rm(i)

########################################
# Get weather data

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

# Rename, add columns
dat <- clong
dat[,addnames] <- NA

# Create temporary data frame
df <- location_info[,c("Signal", "Start", "End", "LAT", "LONG")]
df$StartDate <- as.Date(df$Start, tz="America/Denver")
df$EndDate <- as.Date(df$End, tz="America/Denver")
# check
table(unique(dat$Signal) %in% df$Signal)
table(df$Signal %in% unique(dat$Signal))

# for each Signal
for (i in 1:nrow(df)) {
  print(paste0("Working on signal ", df$Signal[i]))
  myjs <- which(dat$Signal == df$Signal[i])
  if (length(myjs)>0) {
    # get hourly weather data for all days
    mylat <- df$LAT[i]
    mylon <- df$LONG[i]
    mydates <- sort(unique(as.Date(dat[myjs,"Ped.TS.at.CP.1"])))
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
      ttemp$timediff <- abs(difftime(ttemp$valid_local,dat[j,"Ped.TS.at.CP.1"], units="mins"))
      thr <- which(ttemp$timediff==min(ttemp$timediff, na.rm=T))[1]
      dat[j,addnames] <- ttemp[thr,addnames]
      rm(ttemp, thr)
    }; rm(j)
    # cleanup
    rm(temp)
  }
  # cleanup
  rm(myjs)
}; rm(i)

# Inspect
summary(dat$air_temp_f)
boxplot(dat$air_temp_f)
summary(dat$hourly_precip_in)
boxplot(dat$hourly_precip_in)
summary(dat$hourly_precip_in>0)

########################################
# Add temporal variables

# Add day-of-week
dat$Weekday <- format(dat$Ped.TS.at.CP.1, "%a")
dat$Weekday <- factor(dat$Weekday, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
summary(dat$Weekday)
dat$Weekend <- ifelse(dat$Weekday %in% c("Sat", "Sun"), T, F)
summary(dat$Weekend)

# Add time-of-day
dat$Hour <- format(dat$Ped.TS.at.CP.1, "%H")
dat$Hour <- factor(dat$Hour)
summary(dat$Hour)
dat$TOD1 <- factor(dat$Hour, labels=c("0002", "0002", "0305", "0305", 
                                      "0608", "0608", "0608", "0911", "0911", "0911", 
                                      "1214", "1214", "1214", "1517", "1517", "1517", 
                                      "1820", "1820", "1820", "2123", "2123", "2123"))
summary(dat$TOD1)
dat$TOD2 <- factor(dat$TOD1, labels=c("1805", "1805", "0611", "0611", "1217", "1217", "1805", "1805"))
summary(dat$TOD2)
dat$TOD1 <- relevel(dat$TOD1, ref="1517")
dat$TOD2 <- relevel(dat$TOD2, ref="1217")
dat$AMPeak <- ifelse(dat$Hour %in% c("07", "08"), T, F)
dat$PMPeak <- ifelse(dat$Hour %in% c("16", "17"), T, F)
summary(dat$AMPeak)
summary(dat$PMPeak)

########################################
# Save and finish

# Inspect
summary(dat)
summary(cwide)

# Save Combinations
write.csv(dat, file.path("Analysis", "Conflict Analysis", "datlongcombo.csv"), row.names=F)
saveRDS(dat, file.path("Analysis", "Conflict Analysis", "datlongcombo.rds"))
write.csv(cwide, file.path("Analysis", "Conflict Analysis", "datwidecombo.csv"), row.names=F)
saveRDS(cwide, file.path("Analysis", "Conflict Analysis", "datwidecombo.rds"))

# Cleanup
rm(datlong, datwide, location_info)
rm(combinedLong, combinedWide, clong, cwide, dat, df)
rm(myfile, myfolders, myfpath, tnm1, tnm2, tnames, addnames)
rm(avgsig, getsig, seqsig, subsig, tabsig, tabsigseq, tabsigtdif, timesigmiss)
rm(sigstatus, pedsigstatus, vehsigstatus)
gc()

########################################
# END
########################################