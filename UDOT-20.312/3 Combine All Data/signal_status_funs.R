########################################
# Function

# Function to get signal status info, at one time
sigstatus <- function(sigdata=NA, sigid=NA, phase=NA, mode=NA, dtime=NA) {
  # initialize outputs
  status <- list("sig_status"=NA, 
                 "prev_event"=NA, 
                 "sec_prev_event"=NA, 
                 "next_event"=NA, 
                 "sec_next_event"=NA)
  # check inputs
  # ...
  if(is.na(dtime)) { return(status) }
  # initialize temp
  temp <- sigdata
  # subset for signal id
  temp <- temp[temp$SIGNAL %in% sigid,]
  # subset for phase number
  # temp <- temp[temp$PARAM==phase,]
  temp <- temp[temp$PARAM %in% phase,] # allows multiple (e.g.: overlap)
  # subset for mode phase events
  if (mode=="ped") {
    temp <- temp[temp$EVENT %in% c(21,22,23),]
  } else if (mode=="veh") {
    temp <- temp[temp$EVENT %in% c(1,8,10),]
    
  } else { return(status) }
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
  if (mode=="ped") {
    status$sig_status <- ifelse(is.na(status$prev_event), NA, 
                         ifelse(status$prev_event==21, "Walk", 
                         ifelse(status$prev_event==22, "Flashing Don't Walk", 
                         ifelse(status$prev_event==23, "Solid Don't Walk", NA))))
  } else if (mode=="veh") {
    status$sig_status <- ifelse(is.na(status$prev_event), NA, 
                         ifelse(status$prev_event==1, "Green", 
                         ifelse(status$prev_event==8, "Yellow", 
                         ifelse(status$prev_event==10, "Red", NA))))
  } else { return(status) }
  # return
  return(status)
}

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

# END