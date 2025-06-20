#######################################
# Project:  UDOT-20.313 Ped violations
# Authors:  Patrick A. Singleton (patrick.singleton@usu.edu)
# File:     add_ped_signal_status.R
# Date:     2021 Fall, 2022 Spring, Summer
# About:    Add pedestrian signal status to pedestrian behavior data
########################################

########################################
# Notes

# Open R project first, then open this R script

# Major edits
# 2021-09-03 PAS created
# 2021-09-12 PAS updated version for UDOT Meeting
# 2022-01-31 PAS updated added vehicle signal status function
# 2022-05-13 SB  updated
# 2022-08-12 PAS updated
# 2022-11-27 PAS updated to shift times

# Load libraries
# library(readxl)
# library("")

# Load functions
source(file.path("Analysis", "funs.R"))

########################################
# Load data

# Load dat
dat <- readRDS(file.path("Data", "dat03.rds"))

# Inspect
names(dat)
str(dat)
summary(dat)

# Load conflicting phases
# conpha <- read_excel(file.path("Data", "CrossingInfo.xlsx"))
# str(conpha)

########################################
# Shift times
# account for difference between video time and signal time

# TimeShift
summary(dat$TimeShift)
# subtract TimeShift from every timestamp
# - time means video is before signal
# + time means video is after signal

# Round TimeShift
dat$TimeShiftI <- as.integer(round(dat$TimeShift))
summary(dat$TimeShiftI)

# Subtract TimeShiftI
mycols <- c("TimeWaitArr", "TimeWaitDep", "TimeCurbDep", "TimeCurbArr")
summary(dat[,mycols])
dat[,mycols] <- dat[,mycols] - dat$TimeShiftI
summary(dat[,mycols])
rm(mycols)

########################################
# Checks (don't run)

# # Initialize
# myfpath <- file.path("Data", "UDOT Ped Videos", "2_Checked")
# myfile <- "ControllerEventLogs.csv"
# myfile2 <- "sigpha.txt"
# myfolders <- unique(dat$Folder)
# checkdf <- data.frame(Folder=myfolders, Events1810=NA, PedPhase=NA)
# 
# # Check for same ped phase
# for (i in 1:length(myfolders)) {
#   myfold <- myfolders[i]
#   tfile2 <- read.table(file.path(myfpath, myfold, myfile2), sep=",")
#   sigid <- tfile2$V1[1]
#   phase <- tfile2$V2[1]
#   pphase <- dat[dat$Folder==myfold & dat$Signal==sigid,"PedPh"]
#   pphase <- as.integer(as.vector(unique(pphase)))
#   if (!is.na(phase) & !is.na(pphase)) {
#     if (phase==pphase) {
#       checkdf$PedPhase[i] <- "Same"
#     } else {
#       checkdf$PedPhase[i] <- "Different"
#     }
#   }
#   rm(myfold, tfile2, sigid, phase, pphase)
# }; rm(i)
# checkdf[checkdf$PedPhase==F,]
# 
# # Check for event codes 1,8,10
# # note: this takes a few minutes
# for (i in 1:length(myfolders)) {
#   myfold <- myfolders[i]
#   print(myfold)
#   sigdata <- getsig(file.path(myfpath, myfold, myfile))
#   phases <- sort(unique(sigdata$EVENT))
#   if (sum(c(1,8,10) %in% phases)==3) {
#     checkdf$Events1810[i] <- "Yes"
#   } else {
#     checkdf$Events1810[i] <- "No"
#   }
#   rm(myfold, sigdata, phases)
# }; rm(i)
# checkdf[checkdf$Events1810=="No",]
# 
# # Save checks
# write.csv(checkdf, file.path("Data", "checkphases.csv"), row.names=F)
# 
# # Remove
# rm(myfpath, myfile, myfile2, myfolders, checkdf)

########################################
# Function

# Function to get pedestrian signal status info, at one time
pedsigstatus <- function(sigdata=NA, sigid=NA, phase=NA, dtime=NA) {
  # initialize outputs
  status <- list("ped_status"=NA, 
                 "prev_event"=NA, 
                 "sec_prev_event"=NA, 
                 "next_event"=NA, 
                 "sec_next_event"=NA, 
                 "sec_prev_walk"=NA, 
                 "sec_next_walk"=NA)
  # check inputs
  # ...
  if(is.na(dtime)) { return(status) }
  # initialize temp
  temp <- sigdata
  # subset for signal id
  temp <- temp[temp$SIGNAL==sigid,]
  # subset for phase number
  temp <- temp[temp$PARAM==phase,]
  # subset for ped phase events
  temp <- temp[temp$EVENT %in% c(21,22,23),]
  # find nearby events
  temp$TDIFF <- difftime(dtime, temp$TIME, units="secs")
  tpos <- temp$TDIFF; tpos[tpos<0] <- NA
  if (sum(!is.na(tpos))==0) { } else {
    twhich <- which(temp$TDIFF==min(tpos, na.rm=T))
    status$prev_event <- temp$EVENT[twhich]
    status$sec_prev_event <- as.numeric(temp$TDIFF[twhich])
    status$next_event <- temp$EVENT[twhich+1]
    status$sec_next_event <- abs(as.numeric(temp$TDIFF[twhich+1]))
    rm(twhich)
  }
  # find current status
  status$ped_status <- ifelse(is.na(status$prev_event), NA, 
                       ifelse(status$prev_event==21, "Walk", 
                       ifelse(status$prev_event==22, "Flashing Don't Walk", 
                       ifelse(status$prev_event==23, "Solid Don't Walk", NA))))
  if(!is.na(status$ped_status) & status$ped_status=="Walk") {
    status$sec_prev_walk <- 0.0
    status$sec_next_walk <- 0.0
  } else {
    # find nearby walk
    temp22 <- temp[temp$EVENT==22,]
    tpos22 <- temp22$TDIFF; tpos22[tpos22<0] <- NA
    if (sum(!is.na(tpos22))==0) { } else {
      twhich22 <- which(temp22$TDIFF==min(tpos22, na.rm=T))
      status$sec_prev_walk <- as.numeric(temp22$TDIFF[twhich22])
      rm(twhich22)
    }
    temp21 <- temp[temp$EVENT==21,]
    tneg21 <- temp21$TDIFF; tneg21[tneg21>=0] <- NA
    if (sum(!is.na(tneg21))==0) { } else {
      twhich21 <- which(temp21$TDIFF==max(tneg21, na.rm=T))
      status$sec_next_walk <- abs(as.numeric(temp21$TDIFF[twhich21]))
      rm(twhich21)
    }
    rm(temp22, tpos22, temp21, tneg21)
  }
  rm(temp, tpos)
  # return
  return(status)
}

# Function to get conflicting vehicle signal status info, for two times
vehsigstatus <- function(sigdata=NA, sigid=NA, phase=NA, cphase=NA, cdtime=NA, catime=NA) {
  # initialize outputs
  status <- list("cross_sec"=NA, 
                 "green_sec"=NA, 
                 "yellow_sec"=NA, 
                 "red_sec"=NA)
  # check inputs
  # ...
  if (is.na(cdtime) | is.na(catime) | cdtime >= catime) {
    warning(paste("Time error: Depart", cdtime, "but arrive", catime))
    return(status)
  }
  # find crossing time
  if(is.na(cdtime) | is.na(catime)) { return(status) }
  status$cross_sec <- as.numeric(difftime(catime, cdtime))
  # initialize temp
  temp <- sigdata
  # subset for signal id
  temp <- temp[temp$SIGNAL==sigid,]
  # subset for vehicle phase events
  # temp <- temp[temp$EVENT %in% c(1,7,8,9,10,11),]
  temp <- temp[temp$EVENT %in% c(1,8,10),]
  # table(temp$EVENT, temp$PARAM)
  # find nearby events
  temp$TDIFF1 <- difftime(cdtime, temp$TIME, units="secs")
  temp$TDIFF2 <- difftime(catime, temp$TIME, units="secs")
  # find unique vehicle phases
  vpha <- sort(unique(temp$PARAM))
  # subset by vehicle phase
  vtemp <- list()
  for (v in vpha) {
    vt <- temp[temp$PARAM==v,]
    tpos <- vt$TDIFF1; tpos[tpos<0] <- NA
    twhichpos <- which(vt$TDIFF1==min(tpos, na.rm=T))
    if(length(twhichpos)==0) { twhichpos <- 1L } # if only negative
    tneg <- vt$TDIFF2; tneg[tneg>=0] <- NA
    twhichneg <- which(vt$TDIFF2==max(tneg, na.rm=T))
    if(length(twhichneg)==0) { twhichneg <- nrow(vt) } # if only positive
    vt <- vt[twhichpos[length(twhichpos)]:twhichneg[1],]
    vtemp[[as.character(v)]] <- vt
    # print(vt)
    rm(vt, tpos, twhichpos, tneg, twhichneg)
  }; rm(v)
  # find conflicting vehicle phases
  vcpha <- vpha[vpha %in% cphase]
  # calc time with different veh signal indications
  ttimes <- c(cdtime, catime)
  for (v in vcpha) {
    vt <- vtemp[[as.character(v)]]
    ttimes <- c(ttimes, vt$TIME)
    rm(vt)
  }; rm(v)
  ttimes <- sort(unique(ttimes))
  ttimes <- ttimes[ttimes>=cdtime & ttimes<=catime]
  # initialize time calc data frame
  tt <- data.frame(TIME=ttimes)
  tt[,paste0("P", vcpha)] <- NA
  # loop to get veh signal status 
  for (vj in vcpha) {
    vt <- vtemp[[as.character(vj)]]
    for (vi in 1:nrow(tt)) {
      vt$TDIFF <- difftime(tt$TIME[vi], vt$TIME)
      vt$TDIFF[vt$TDIFF<0] <- NA
      tw <- which(vt$TDIFF==min(vt$TDIFF, na.rm=T))
      if(length(tw)==0) { # if only negative, assume red
        tt[vi,paste0("P",vj)] <- 10L
      } else {
        tt[vi,paste0("P",vj)] <- vt[tw,"EVENT"]
      }
      vt$TDIFF <- NULL
      rm(tw)
    }; rm(vi)
    rm(vt)
  }; rm(vj)
  # get G/Y/R times
  tt$TDIFF <- difftime(c(tt$TIME[2:nrow(tt)],NA), tt$TIME)
  tt$GREEN <- as.difftime("0", "%S")
  tt$YELLOW <- as.difftime("0", "%S")
  tt$RED <- as.difftime("0", "%S")
  for (vi in 1:(nrow(tt)-1)) {
    if(any(tt[vi,paste0("P",vcpha)]==1)) { tt$GREEN[vi] <- tt$TDIFF[vi] }
    else if(any(tt[vi,paste0("P",vcpha)]==8)) { tt$YELLOW[vi] <- tt$TDIFF[vi] }
    else if(any(tt[vi,paste0("P",vcpha)]==10)) { tt$RED[vi] <- tt$TDIFF[vi] }
  }; rm(vi)
  status$green_sec <- as.numeric(sum(tt$GREEN))
  status$yellow_sec <- as.numeric(sum(tt$YELLOW))
  status$red_sec <- as.numeric(sum(tt$RED))
  # cleanup
  rm(temp, vpha, vtemp, vcpha, ttimes, tt)
  # return
  return(status)
}

########################################
# Add ped signal status and conflicting vehicle phase info

# Initialize
# dat$TimeWaitArr <- NA
# dat$TimeCurbDep <- NA
# dat$TimeCurbArr <- NA
tnm1 <- c("TimeWaitArr", "TimeCurbDep", "TimeCurbArr")
tnm2 <- c("ped_status", "prev_event", "sec_prev_event", 
          "next_event", "sec_next_event", 
          "sec_prev_walk", "sec_next_walk")
tnm3 <- c("cross_sec", "green_sec", "yellow_sec", "red_sec")
tnames <- paste(rep(tnm1, each=length(tnm2)), tnm2, sep="_")
tnames <- c(tnames, tnm3)
dat[,tnames] <- NA

# Start
myfpath <- file.path("Data", "UDOT Ped Videos", "2_Checked")
myfile <- "ControllerEventLogs.csv"
myfile2 <- "sigpha.txt"
myfolders <- unique(dat$Folder)
myevents <- c(0,21,22,23,45,89,90)

# Iterate over folders
# this takes some time (less than an hour)
for(i in 1:length(myfolders)) {
  # Initialize
  # i <- 26
  # i <- 1
  # i <- 2
  myfold <- myfolders[i]
  print(myfold)
  
  # Get signal data
  print("...loading signal data...")
  sigdata <- getsig(file.path(myfpath, myfold, myfile))
  # sigdata <- subsig(sigdata, events=myevents) # removed b/c need other events for violation
  
  # Get signal and phase
  tfile2 <- read.table(file.path(myfpath, myfold, myfile2), sep=",")
  sigid <- tfile2$V1[1]
  phase <- tfile2$V2[1]
  # dtime <- dat$TimeCurbDep[j]
  # pedsigstatus(sigdata, 4130, 2, as.POSIXct("2019-02-04 10:41:30", tz="America/Denver"))
  # find conflicting vehicle phases
  cphase <- dat[dat$Folder==myfold & dat$Signal==sigid,grep("ConPh",names(dat),value=T)]
  cphase <- sort(as.integer(as.vector(unique(cphase))))
  pphase <- dat[dat$Folder==myfold & dat$Signal==sigid,"PedPh"]
  pphase <- as.integer(as.vector(unique(pphase)))
  if(phase!=pphase) { warning(paste("Ped phases don't match:", phase, pphase)) }
  
  # Iterate over observations
  print("...getting signal status info...")
  for(j in which(dat$Folder==myfold)) {
    # j <- 2
    # Get ped signal status
    # dat$TimeWaitArr[j] <- pedsigstatus(sigdata, sigid, phase, dtime=dat$TimeWaitArr[j])$ped_status
    # dat$TimeCurbDep[j] <- pedsigstatus(sigdata, sigid, phase, dtime=dat$TimeCurbDep[j])$ped_status
    # dat$TimeCurbArr[j] <- pedsigstatus(sigdata, sigid, phase, dtime=dat$TimeCurbArr[j])$ped_status
    dat[j,paste("TimeWaitArr", tnm2, sep="_")] <- pedsigstatus(sigdata, sigid, phase, dtime=dat$TimeWaitArr[j])
    dat[j,paste("TimeCurbDep", tnm2, sep="_")] <- pedsigstatus(sigdata, sigid, phase, dtime=dat$TimeCurbDep[j])
    dat[j,paste("TimeCurbArr", tnm2, sep="_")] <- pedsigstatus(sigdata, sigid, phase, dtime=dat$TimeCurbArr[j])
    # Get veh signal status
    dat[j,paste(tnm3)] <- vehsigstatus(sigdata, sigid, phase, cphase, cdtime=dat$TimeCurbDep[j], catime=dat$TimeCurbArr[j])
  }; rm(j)
  
  # Cleanup
  rm(myfold, sigdata, tfile2, sigid, phase, cphase, pphase)
}; rm(i)

# Adjust factors
dat$TimeWaitArr_ped_status <- factor(dat$TimeWaitArr_ped_status, levels=c("Walk", "Flashing Don't Walk", "Solid Don't Walk"))
dat$TimeCurbDep_ped_status <- factor(dat$TimeCurbDep_ped_status, levels=c("Walk", "Flashing Don't Walk", "Solid Don't Walk"))
dat$TimeCurbArr_ped_status <- factor(dat$TimeCurbArr_ped_status, levels=c("Walk", "Flashing Don't Walk", "Solid Don't Walk"))
table(dat$TimeWaitArr_ped_status)
table(dat$TimeCurbDep_ped_status)
table(dat$TimeCurbArr_ped_status)

########################################
# Check signal information

# Inspect ped signal status
# check time since prev / to next
summary(dat$TimeCurbDep_sec_prev_event)
summary(dat$TimeCurbDep_sec_next_event)
table(dat$Folder, dat$TimeCurbDep_sec_prev_event>120)
table(dat$Folder, dat$TimeCurbDep_sec_next_event>120)
# most are actually okay, b/c time to next ped phase
# (so many not on recall, or overnight)
# check ped signal status
table(dat$TimeCurbDep_ped_status)
table(dat$Folder, dat$TimeCurbDep_ped_status)
# 2019-02-28 8302b - maybe okay, most Dep SWD on 02/28 are just a few seconds before W
#                    suspect b/c TimeShift is very different (-2 on 02/28, +4 on 03/01)
#                    but maybe okay b/c most are green_sec==0
# 2019-03-11 5702 -- fixed, wrong ped phase number
# 2019-03-13 4301 -- fixed, wrong ped phase number
# 2019-04-03 4511 -- okay, many cross outside of crosswalk (mid-block)
# 2019-07-29 7475 -- okay, many cross without pressing button (HAWK)
# 2021-09-16 7086 -- maybe okay, many Dep FWD are just a few seconds after W
#                    suspect b/c TimeShift is off slightly or people Dep on FWD
#                    but maybe okay b/c most are green_sec==0
table(dat$TimeCurbDep_ped_status, dat$TimeCurbArr_ped_status)
summary(dat$green_sec[dat$TimeCurbDep_ped_status=="Walk" & dat$TimeCurbArr_ped_status=="Walk"])
summary(dat$green_sec[dat$TimeCurbDep_ped_status=="Walk" & dat$TimeCurbArr_ped_status=="Flashing Don't Walk"])
summary(dat$green_sec[dat$TimeCurbDep_ped_status=="Walk" & dat$TimeCurbArr_ped_status=="Solid Don't Walk"])
summary(dat$green_sec[dat$TimeCurbDep_ped_status=="Flashing Don't Walk" & dat$TimeCurbArr_ped_status=="Walk"])
summary(dat$green_sec[dat$TimeCurbDep_ped_status=="Flashing Don't Walk" & dat$TimeCurbArr_ped_status=="Flashing Don't Walk"])
summary(dat$green_sec[dat$TimeCurbDep_ped_status=="Flashing Don't Walk" & dat$TimeCurbArr_ped_status=="Solid Don't Walk"])
summary(dat$green_sec[dat$TimeCurbDep_ped_status=="Solid Don't Walk" & dat$TimeCurbArr_ped_status=="Walk"])
summary(dat$green_sec[dat$TimeCurbDep_ped_status=="Solid Don't Walk" & dat$TimeCurbArr_ped_status=="Flashing Don't Walk"])
summary(dat$green_sec[dat$TimeCurbDep_ped_status=="Solid Don't Walk" & dat$TimeCurbArr_ped_status=="Solid Don't Walk"])
# check missing
table(is.na(dat$TimeCurbDep_ped_status))
table(dat$Folder, is.na(dat$TimeCurbDep_ped_status))
table(dat$WaitBehLeft, is.na(dat$TimeCurbDep_ped_status))
# many missing are because pedestrians left waiting area without crossing
temp <- dat[is.na(dat$green_sec),]
table(temp$Folder, temp$WaitBehLeft)
temp <- dat[is.na(dat$TimeCurbDep_ped_status),]
table(temp$Folder, temp$WaitBehLeft)
temp <- dat[is.na(dat$TimeCurbArr_ped_status),]
table(temp$Folder, temp$WaitBehLeft)
# 2019-03-04 7086 -- okay, b/c many events
# 2019-03-29 6407 -- okay, b/c walking to middle parking area
# 2019-04-01 4511 -- okay, b/c many events
# 2019-08-08 5093 -- okay, b/c erroneously recorded many people as "left waiting area" if crossed using different crosswalk
# 2021-09-14 8302 -- okay, b/c erroneously recorded many people as "left waiting area" if crossed using different crosswalk
temp <- dat[!is.na(dat$green_sec) & dat$green_sec>5,]
table(temp$Folder)
rm(temp)

# Inspect veh signal status
summary(dat$cross_sec)
summary(dat$green_sec)
summary(dat$yellow_sec)
summary(dat$red_sec)
ttt <- data.frame(Folder=myfolders)
ttt[,tnm3] <- 0
for (f in 1:nrow(ttt)) {
  tttf <- ttt$Folder[f]
  tttdat <- dat[dat$Folder==tttf,]
  ttt$cross_sec[f] <- mean(tttdat$cross_sec, na.rm=T)
  ttt$green_sec[f] <- mean(tttdat$green_sec, na.rm=T)
  ttt$yellow_sec[f] <- mean(tttdat$yellow_sec, na.rm=T)
  ttt$red_sec[f] <- mean(tttdat$red_sec, na.rm=T)
  rm(tttf, tttdat)
}; rm(f)
ttt
rm(ttt)
# 2019-03-04 5702 -- fixed, had the wrong phase numbers
# 2019-03-04 8634 -- okay, small sample size
# 2019-07-29 7475 -- okay, HAWK signal, many cross without protection
# 2019-11-23 7126 -- can't use, because missing signal data then

# Initialize
checkdat <- data.frame(j=0, Folder=NA, WaitBehLeft=NA, Timestamp=Sys.time(), TimeCurbDep=Sys.time(), 
                       TimeCurbDep_sec_prev_event=0, TimeCurbDep_sec_next_event=0)

# Check for consistent pedestrian/vehicle signal status
for (j in 1:nrow(dat)) {
  Folder <- dat$Folder[j]
  WaitBehleft <- dat$WaitBehLeft[j]
  Timestamp <- dat$Timestamp[j]
  TimeCurbDep <- dat$TimeCurbDep[j]
  TimeCurbDep_sec_prev_event <- dat$TimeCurbDep_sec_prev_event[j]
  TimeCurbDep_sec_next_event <- dat$TimeCurbDep_sec_next_event[j]
  if (!is.na(TimeCurbDep) & !is.na(TimeCurbDep_sec_prev_event) & !is.na(TimeCurbDep_sec_next_event) & (TimeCurbDep_sec_prev_event + TimeCurbDep_sec_next_event < 30*60)) {
  } else {
    checkdat <- rbind(checkdat, list(j, Folder, WaitBehleft, Timestamp, TimeCurbDep, TimeCurbDep_sec_prev_event, TimeCurbDep_sec_next_event))
  }
  rm(Folder, WaitBehleft, Timestamp, TimeCurbDep, TimeCurbDep_sec_prev_event, TimeCurbDep_sec_next_event)
}; rm(j)
checkdat <- checkdat[-1,]

# Save
# write.csv(checkdat, file.path("Data", "checksignalstatus.csv"), row.names=F)

# Remove
rm(checkdat)

# Cleanup
rm(tnm1, tnm2, tnm3, tnames)
rm(myfpath, myfile, myfile2, myfolders, myevents)

########################################
# Remove data can't use

# Initialize
dat2 <- dat

# 2019-11-23 7126: missing signal data for time period
dat2 <- dat[dat$Folder!="2019-11-23 7126",]

########################################
# Save files

# Inspect
names(dat2)
str(dat2)
summary(dat2)

# Save dat
saveRDS(dat2, file.path("Data", "dat04.rds"))
write.csv(dat2, file.path("Data", "dat04.csv"), row.names=F)

########################################
# Cleanup

# Remove
rm(avgsig, getsig, seqsig, subsig, tabsig, tabsigseq, tabsigtdif, timesigmiss)
rm(pedsigstatus, vehsigstatus)
rm(dat, dat2)
gc()

########################################
# END
########################################