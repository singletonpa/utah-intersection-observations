#######################################
# Project:  UDOT-20.313 Ped violations
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     05_Describe_Analyze.R
# Date:     2022 Spring, Summer, Fall, 2023 Spring
# About:    Descriptive statistics & models of spatial/temporal violations & pedestrian behaviors
########################################

########################################
# Initialize

# Load R-project first, then open this file. 

# Packages
library(nlme)
library(lme4)
library(lmerTest)
library(multilevelTools)
library(mgcv)

# 2022-07-13 PAS created: only SDV1MB (logistic) and SDV1 (MNL)
# 2022-09-06 PAS updated: new data, add desc, update SDV1MB and SDV1, add SDV1MI (logistic)
# 2022-09-07 PAS updated: add desc, add TDV100, TDV105, TDV110, TDV115, TDV120 (logistic)
# 2022-09-08 PAS updated: revised models using new data, for Ped Summit presentation
# 2022-11-29 PAS updated: revised dataset, added new DVs
# 2023-01-05 PAS updated: refined DVs, updated calcs/figures for TRB presentation
# 2023-01-12 PAS updated: updated descriptive statistics, selected new DVs
# 2023-05-06 PAS updated: added calcs for walking speed

########################################
# Prepare data

# Load data
dat <- readRDS(file.path("Data", "dat04.rds"))
names(dat)
head(dat)

# Save backup
xdat <- dat

# Create DVs for spatial violations
# 1. CrossLoc
summary(dat$CrossLoc)
table(dat$CrossLoc, dat$WaitBehLeft)
# - all "Other" are okay, b/c WaitBehLeft==T
# drop Other
dat$SDV1 <- dat$CrossLoc
dat$SDV1[dat$SDV1=="Other"] <- NA
dat$SDV1 <- droplevels(dat$SDV1)
# create dummy DVs
dat$SDV1XW <- ifelse(dat$SDV1=="In the crosswalk or the crosswalk area", T, F)
dat$SDV1MB <- ifelse(dat$SDV1=="Mid-block, away from the crosswalk", T, F)
dat$SDV1MI <- ifelse(dat$SDV1=="In the middle of the intersection", T, F)
dat$SDV1NXW <- (dat$SDV1MB + dat$SDV1MI)>0
dat$SDV1MB2 <- ifelse(dat$SDV1=="In the middle of the intersection", NA, dat$SDV1MB)
# 2. CrossMark
summary(dat$CrossMarkInAll)
summary(dat$CrossMarkInOut)
summary(dat$CrossMarkOutAll)
summary(dat$CrossMarkOther)
table(dat$CrossMarkOtherText)
table(rowSums(dat[,c("CrossMarkInAll", "CrossMarkInOut", "CrossMarkOutAll", "CrossMarkOther")]))
table(rowSums(dat[,c("CrossMarkInAll", "CrossMarkInOut", "CrossMarkOutAll")]))
# - need to convert to NA if no crossing
# - should convert to NA if crossed not in crosswalk area
# create dummy DVs
dat$SDV2IA <- dat$CrossMarkInAll
dat$SDV2IO <- dat$CrossMarkInOut
dat$SDV2OA <- dat$CrossMarkOutAll
# drop not {InAll, InOut, OutAll}
dat$SDV2IA[rowSums(dat[,c("CrossMarkInAll", "CrossMarkInOut", "CrossMarkOutAll")])==0] <- NA
dat$SDV2IO[rowSums(dat[,c("CrossMarkInAll", "CrossMarkInOut", "CrossMarkOutAll")])==0] <- NA
dat$SDV2OA[rowSums(dat[,c("CrossMarkInAll", "CrossMarkInOut", "CrossMarkOutAll")])==0] <- NA
# drop not "In the crosswalk or the crosswalk area"
dat$SDV2IA2 <- ifelse(dat$SDV1XW!=T, NA, dat$SDV2IA)
dat$SDV2IO2 <- ifelse(dat$SDV1XW!=T, NA, dat$SDV2IO)
dat$SDV2OA2 <- ifelse(dat$SDV1XW!=T, NA, dat$SDV2OA)
# inspect DVs
summary(dat$SDV1)    # categorical
summary(dat$SDV1XW)  # binary
summary(dat$SDV1MB)  # binary
summary(dat$SDV1MI)  # binary
summary(dat$SDV1NXW) # binary
summary(dat$SDV1MB2) # binary <- logistic regression
summary(dat$SDV2IA)  # binary
summary(dat$SDV2IO)  # binary
summary(dat$SDV2OA)  # binary
summary(dat$SDV2IA2) # binary
summary(dat$SDV2IO2) # binary
summary(dat$SDV2OA2) # binary <- logistic regression

# Create DVs for temporal violations
# 1. green_sec
summary(dat$green_sec)
table(is.na(dat$green_sec), dat$WaitBehLeft)
sort(dat$green_sec, decreasing=T)[1:50]
# - most NA are okay, b/c WaitBehLeft==T or didn't reach one curb
# - max green_sec is 61 sec, okay
# drop large values > 75 sec
dat$TDV1 <- dat$green_sec
dat$TDV1[dat$TDV1>=75] <- NA
# - should convert to NA if crossed not in crosswalk area
# drop not "In the crosswalk or the crosswalk area"
dat$TDV1 <- ifelse(dat$SDV1XW!=T, NA, dat$TDV1)
# create dummy DVs
dat$TDV100 <- ifelse(dat$TDV1 > 00, T, F)
dat$TDV105 <- ifelse(dat$TDV1 > 05, T, F)
dat$TDV110 <- ifelse(dat$TDV1 > 10, T, F)
dat$TDV115 <- ifelse(dat$TDV1 > 15, T, F)
dat$TDV120 <- ifelse(dat$TDV1 > 20, T, F)
# 2/3/4. TimeCurbDep_ped_status, TimeCurbArr_ped_status
summary(dat$TimeCurbDep_ped_status)
summary(dat$TimeCurbArr_ped_status)
table(dat$TimeCurbDep_ped_status, dat$TimeCurbArr_ped_status)
table(is.na(dat$TimeCurbDep_ped_status), dat$WaitBehLeft)
table(is.na(dat$TimeCurbArr_ped_status), dat$WaitBehLeft)
# - most NA are okay, b/c WaitBehLeft==T or didn't reach one curb
# - should convert to NA if crossed not in crosswalk area
dat$TDV2 <- dat$TimeCurbDep_ped_status
dat$TDV3 <- dat$TimeCurbArr_ped_status
levels(dat$TDV2) <- c("W", "FDW", "SDW")
levels(dat$TDV3) <- c("W", "FDW", "SDW")
dat$TDV2[dat$SDV1XW!=T] <- NA
dat$TDV3[dat$SDV1XW!=T] <- NA
dat$TDV4 <- paste(dat$TDV2, dat$TDV3)
dat$TDV4[grepl("NA", dat$TDV4)] <- NA
# create dummy DVs
dat$TDV2WK <- ifelse(dat$TDV2=="W", T, F)
dat$TDV2FD <- ifelse(dat$TDV2=="FDW", T, F)
dat$TDV2SD <- ifelse(dat$TDV2=="SDW", T, F)
dat$TDV3WK <- ifelse(dat$TDV3=="W", T, F)
dat$TDV3FD <- ifelse(dat$TDV3=="FDW", T, F)
dat$TDV3SD <- ifelse(dat$TDV3=="SDW", T, F)
dat$TDV4WK_WK <- ifelse(dat$TDV4=="W W", T, F)
dat$TDV4WK_FD <- ifelse(dat$TDV4=="W FDW", T, F)
dat$TDV4WK_SD <- ifelse(dat$TDV4=="W SDW", T, F)
dat$TDV4FD_WK <- ifelse(dat$TDV4=="FDW W", T, F)
dat$TDV4FD_FD <- ifelse(dat$TDV4=="FDW FDW", T, F)
dat$TDV4FD_SD <- ifelse(dat$TDV4=="FDW SDW", T, F)
dat$TDV4SD_WK <- ifelse(dat$TDV4=="SDW W", T, F)
dat$TDV4SD_FD <- ifelse(dat$TDV4=="SDW FDW", T, F)
dat$TDV4SD_SD <- ifelse(dat$TDV4=="SDW SDW", T, F)
# inspect DVs
summary(dat$TDV1)   # continuous
summary(dat$TDV100) # binary <- maybe
summary(dat$TDV105) # binary <- logistic regression
summary(dat$TDV110) # binary <- maybe
summary(dat$TDV115) # binary <- maybe
summary(dat$TDV120) # binary <- maybe
table(dat$TDV2); table(is.na(dat$TDV2)) # categorical
summary(dat$TDV2WK) # binary
summary(dat$TDV2FD) # binary <- maybe
summary(dat$TDV2SD) # binary <- logistic regression
table(dat$TDV3); table(is.na(dat$TDV3)) # categorical
summary(dat$TDV3WK) # binary
summary(dat$TDV3FD) # binary
summary(dat$TDV3SD) # binary <- maybe
table(dat$TDV4); table(is.na(dat$TDV4)) # categorical
summary(dat$TDV4WK_WK) # binary
summary(dat$TDV4WK_FD) # binary
summary(dat$TDV4WK_SD) # binary <- maybe
summary(dat$TDV4FD_WK) # binary
summary(dat$TDV4FD_FD) # binary
summary(dat$TDV4FD_SD) # binary <- maybe
summary(dat$TDV4SD_WK) # binary
summary(dat$TDV4SD_FD) # binary
summary(dat$TDV4SD_SD) # binary <- maybe

# Create new IVs
# Other_
dat$OtherStrollerLoad <- (dat$OtherStroller + dat$OtherLoad) > 0
table(dat$OtherStroller, dat$OtherLoad); table(dat$OtherStrollerLoad)
dat$OtherMode <- (dat$OtherWheelchair + dat$OtherSkateboard + dat$OtherScooter + dat$OtherBicycle) > 0
table(dat$OtherWheelchair, dat$OtherSkateboard, dat$OtherScooter, dat$OtherBicycle); table(dat$OtherMode)
# CrossOtherPeople_
dat$CrossOtherPeople <- dat$CrossOtherPeopleSame + dat$CrossOtherPeopleOppo
summary(dat$CrossOtherPeople)
# CrossObs
dat$CrossObs <- (dat$CrossObsCar + dat$CrossObsSWD) > 0
table(dat$CrossObsCar, dat$CrossObsSWD); table(dat$CrossObs)
# air_temp_f
summary(dat$air_temp_f)
hist(dat$air_temp_f)
dat$TEMP <- dat$air_temp_f
dat$TEMPCAT9 <- cut(dat$TEMP, breaks=c(-10,20,30,40,50,60,70,80,90,110), right=F)
dat$TEMPCAT5 <- cut(dat$TEMP, breaks=c(-10,32,50,65,80,110), right=F)
summary(dat$TEMPCAT9); summary(dat$TEMPCAT5)
dat$TEMPCAT9 <- relevel(dat$TEMPCAT9, ref="[40,50)")
dat$TEMPCAT5 <- relevel(dat$TEMPCAT5, ref="[32,50)")
dat$TEMPL32 <- dat$TEMPCAT5=="[-10,32)"
dat$TEMP80P <- dat$TEMPCAT5=="[80,110)"
summary(dat$TEMPL32); summary(dat$TEMP80P)
# hourly_precip_in
table(dat$hourly_precip_in)
summary(dat$hourly_precip_in)
hist(dat$hourly_precip_in)
dat$PRCP <- dat$hourly_precip_in
dat$PRCP_01 <- dat$PRCP >= 0.01
dat$PRCP_05 <- dat$PRCP >= 0.05
summary(dat$PRCP_01); summary(dat$PRCP_05)

########################################
# Descriptive analysis

# Dependent variable: Spatial violations
summary(dat$SDV1)
summary(dat$SDV1XW)
summary(dat$SDV1MB)
summary(dat$SDV1MI)
summary(dat$SDV1NXW)
summary(dat$SDV1MB2)
summary(dat$SDV2IA)
summary(dat$SDV2IO)
summary(dat$SDV2OA)
summary(dat$SDV2IA2)
summary(dat$SDV2IO2)
summary(dat$SDV2OA2)
# overall
table(dat$SDV1MB2)
table(dat$SDV2OA2)
# by location
table(paste(dat$Signal, dat$PedLeg), dat$SDV1MB2)
table(paste(dat$Signal, dat$PedLeg), dat$SDV2OA2)
# chi-square tests
for (i in sort(unique(paste(dat$Signal, dat$PedLeg)))) {
  mytest <- chisq.test(dat$SDV1MB2, paste(dat$Signal, dat$PedLeg)==i)
  if(mytest$p.value < 0.05) { print(i) }
  rm(mytest)
}; rm(i)
for (i in sort(unique(paste(dat$Signal, dat$PedLeg)))) {
  mytest <- chisq.test(dat$SDV2OA2, paste(dat$Signal, dat$PedLeg)==i)
  if(mytest$p.value < 0.05) { print(i) }
  rm(mytest)
}; rm(i)

# Dependent variable: Temporal violations
summary(dat$TDV1)
summary(dat$TDV100)
summary(dat$TDV105)
summary(dat$TDV110)
summary(dat$TDV115)
summary(dat$TDV120)
table(dat$TDV2); table(is.na(dat$TDV2))
summary(dat$TDV2WK)
summary(dat$TDV2FD)
summary(dat$TDV2SD)
table(dat$TDV3); table(is.na(dat$TDV3))
summary(dat$TDV3WK)
summary(dat$TDV3FD)
summary(dat$TDV3SD)
table(dat$TDV4); table(is.na(dat$TDV4))
summary(dat$TDV4WK_WK)
summary(dat$TDV4WK_FD)
summary(dat$TDV4WK_SD)
summary(dat$TDV4FD_WK)
summary(dat$TDV4FD_FD)
summary(dat$TDV4FD_SD)
summary(dat$TDV4SD_WK)
summary(dat$TDV4SD_FD)
summary(dat$TDV4SD_SD)
# overall
table(dat$TDV105)
table(dat$TDV2SD)
# by location
table(paste(dat$Signal, dat$PedLeg), dat$TDV105)
table(paste(dat$Signal, dat$PedLeg), dat$TDV2SD)
# chi-square tests
for (i in sort(unique(paste(dat$Signal, dat$PedLeg)))) {
  mytest <- chisq.test(dat$TDV105, paste(dat$Signal, dat$PedLeg)==i)
  if(mytest$p.value < 0.05) { print(i) }
  rm(mytest)
}; rm(i)
for (i in sort(unique(paste(dat$Signal, dat$PedLeg)))) {
  mytest <- chisq.test(dat$TDV2SD, paste(dat$Signal, dat$PedLeg)==i)
  if(mytest$p.value < 0.05) { print(i) }
  rm(mytest)
}; rm(i)

# Other descriptive statistics about crossings or behaviors
# time with opposing green/yellow/red
summary(dat$green_sec[dat$SDV1XW==T])
summary(dat$yellow_sec[dat$SDV1XW==T])
summary(dat$red_sec[dat$SDV1XW==T])
ttt <- data.frame(Folder=unique(dat$Folder))
ttt[,c("cross_sec", "green_sec", "yellow_sec", "red_sec")] <- 0
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
ttt <- data.frame(Signal=sort(unique(dat$Signal)))
ttt[,c("cross_sec", "green_sec", "yellow_sec", "red_sec")] <- 0
for (f in 1:nrow(ttt)) {
  tttf <- ttt$Signal[f]
  tttdat <- dat[dat$Signal==tttf,]
  ttt$cross_sec[f] <- mean(tttdat$cross_sec, na.rm=T)
  ttt$green_sec[f] <- mean(tttdat$green_sec, na.rm=T)
  ttt$yellow_sec[f] <- mean(tttdat$yellow_sec, na.rm=T)
  ttt$red_sec[f] <- mean(tttdat$red_sec, na.rm=T)
  rm(tttf, tttdat)
}; rm(f)
ttt
rm(ttt)
# other temporal violations
table(dat$TDV2)
table(dat$TDV3)
table(dat$TDV2, dat$TDV3)
# waiting information
summary(dat$WaitOtherPeople)
summary(I(dat$WaitOtherPeople>0))
summary(dat$VehiclesPast10)
summary(dat$VehiclesNext10)
summary(dat$WaitBehPressed)
summary(dat$WaitBehPaced)
summary(dat$WaitBehLeft)
summary(dat$WaitBehOther)
summary(dat$TimeWaitClean)
hist(dat$TimeWaitClean, breaks=c(-1,seq(0,120,5),180))
hist(dat$TimeWaitClean, breaks=c(-1,seq(0,120,10),180))
summary(cut(dat$TimeWaitClean, breaks=c(-1,seq(0,120,10),180)))
# crossing information
summary(dat$CrossOtherPeopleSame)
summary(dat$CrossOtherPeopleOppo)
summary(I(dat$CrossOtherPeopleSame>0))
summary(I(dat$CrossOtherPeopleOppo>0))
summary(dat$CrossMarkInAll)
summary(dat$CrossMarkInOut)
summary(dat$CrossMarkOutAll)
summary(dat$CrossMarkOther)
summary(dat$CrossBehSpeed)
summary(dat$CrossBehPaused)
summary(dat$CrossBehDistracted)
summary(dat$CrossBehOther)
summary(dat$CrossObsCar)
summary(dat$CrossObsSWD)
summary(dat$CrossObsOther)
summary(dat$TimeCurbClean)
hist(dat$TimeCurbClean, breaks=seq(0,60,5))
hist(dat$TimeCurbClean, breaks=seq(0,60,2))
summary(cut(dat$TimeCurbClean, breaks=seq(0,60,5)))
# crossing time vs. distance
boxplot(TimeCurbClean ~ CrossDist, data=dat, las=1, 
        xlab="Crossing distance (ft)", 
        ylab="Crossing time (sec)", 
        main="Pedestrian crossing distance vs. time")
plot(dat$CrossDist, dat$TimeCurbClean, las=1, 
     pch=16, col=rgb(0,0,0, alpha=0.05), 
     xlab="Crossing distance (ft)", 
     ylab="Crossing time (s)", 
     main="Pedestrian crossing distance vs. time")
tdatplot <- data.frame(x=seq(min(dat$CrossDist)-1,max(dat$CrossDist)+1))
tdatplot$y1 <- tdatplot$x / 3.5
tdatplot$y2 <- tdatplot$x / 4.0
lines(y1 ~ x, data=tdatplot, type="l", col="red", lwd=2)
lines(y2 ~ x, data=tdatplot, type="l", col="orange", lwd=2)
legend("topleft", legend=c("Pedestrian observation", "3.5 ft/s walking speed", "4.0 ft/s walking speed"), 
       col=c(rgb(0,0,0, alpha=0.10), "red", "orange"), pch=c(16,NA,NA), lty=c(NA,1,1), lwd=c(NA,2,2))
rm(tdatplot)
summary(dat$TimeCurbClean > dat$CrossDist/3.5)
summary(dat$TimeCurbClean > dat$CrossDist/4.0)
# - save plot as cross_dist_time.png at 900 x 600

# Descriptive statistics for IVs
# initialize
myds <- function(x, xname) {
  tdfds <- list(xname, NA, NA, NA, NA)
  if (class(x) %in% c("logical")) {
    tdfds[4] <- sum(x==T, na.rm=T)
    tdfds[5] <- sum(x==T, na.rm=T)/sum(sum(x==T, na.rm=T), sum(x==F, na.rm=T))
  } else if (class(x) %in% c("integer", "numeric")) {
    tdfds[2] <- mean(x, na.rm=T)
    tdfds[3] <- sd(x, na.rm=T)
  } else {
    # NA
  }
  return(tdfds)
}
dfds <- data.frame(Var="", Mean=0.00, SD=0.00, Freq=0L, Prop=0.00)
datl2 <- dat[!duplicated(dat[,c("Signal", "PedLeg")]),]
# micro-level-one (pedestrian or event info) IVs
dfds <- rbind(dfds, myds(dat$GroupSize, "GroupSize"))
dfds <- rbind(dfds, myds(dat$AgeChild, "AgeChild"))
dfds <- rbind(dfds, myds(dat$AgeTeen, "AgeTeen"))
dfds <- rbind(dfds, myds(dat$AgeAdultYoung, "AgeAdultYoung"))
dfds <- rbind(dfds, myds(dat$AgeAdultMiddle, "AgeAdultMiddle"))
dfds <- rbind(dfds, myds(dat$AgeAdultOlder, "AgeAdultOlder"))
dfds <- rbind(dfds, myds(dat$AgeAdultUnknown, "AgeAdultUnknown"))
dfds <- rbind(dfds, myds(dat$GenderMale, "GenderMale"))
dfds <- rbind(dfds, myds(dat$GenderFemale, "GenderFemale"))
dfds <- rbind(dfds, myds(dat$GenderUnknown, "GenderUnknown"))
dfds <- rbind(dfds, myds(dat$OtherStroller, "OtherStroller"))
dfds <- rbind(dfds, myds(dat$OtherLoad, "OtherLoad"))
dfds <- rbind(dfds, myds(dat$OtherWheelchair, "OtherWheelchair"))
dfds <- rbind(dfds, myds(dat$OtherSkateboard, "OtherSkateboard"))
dfds <- rbind(dfds, myds(dat$OtherScooter, "OtherScooter"))
dfds <- rbind(dfds, myds(dat$OtherBicycle, "OtherBicycle"))
dfds <- rbind(dfds, myds(dat$OtherOther, "OtherOther"))
dfds <- rbind(dfds, myds(dat$OtherStrollerLoad, "OtherStrollerLoad"))
dfds <- rbind(dfds, myds(dat$OtherMode, "OtherMode"))
dfds <- rbind(dfds, myds(dat$WaitOtherPeople, "WaitOtherPeople"))
dfds <- rbind(dfds, myds(dat$VehiclesPast10, "VehiclesPast10"))
dfds <- rbind(dfds, myds(dat$VehiclesNext10, "VehiclesNext10"))
dfds <- rbind(dfds, myds(dat$WaitBehPressed, "WaitBehPressed"))
dfds <- rbind(dfds, myds(dat$WaitBehPaced, "WaitBehPaced"))
dfds <- rbind(dfds, myds(dat$WaitBehLeft, "WaitBehLeft"))
dfds <- rbind(dfds, myds(dat$WaitBehOther, "WaitBehOther"))
dfds <- rbind(dfds, myds(dat$TimeWaitClean, "TimeWaitClean"))
dfds <- rbind(dfds, myds((dat$TimeWaitClean/60), "I(TimeWaitClean/60)"))
dfds <- rbind(dfds, myds(dat$SDV1XW, "SDV1XW"))
dfds <- rbind(dfds, myds(dat$SDV1MB, "SDV1MB"))
dfds <- rbind(dfds, myds(dat$SDV1MI, "SDV1MI"))
dfds <- rbind(dfds, myds(dat$CrossOtherPeopleSame, "CrossOtherPeopleSame"))
dfds <- rbind(dfds, myds(dat$CrossOtherPeopleOppo, "CrossOtherPeopleOppo"))
dfds <- rbind(dfds, myds((dat$CrossOtherPeople>0), "I(CrossOtherPeople>0)"))
dfds <- rbind(dfds, myds(dat$CrossMarkInAll, "CrossMarkInAll"))
dfds <- rbind(dfds, myds(dat$CrossMarkInOut, "CrossMarkInOut"))
dfds <- rbind(dfds, myds(dat$CrossMarkOutAll, "CrossMarkOutAll"))
dfds <- rbind(dfds, myds(dat$CrossMarkOther, "CrossMarkOther"))
dfds <- rbind(dfds, myds(dat$CrossBehSpeed, "CrossBehSpeed"))
dfds <- rbind(dfds, myds(dat$CrossBehPaused, "CrossBehPaused"))
dfds <- rbind(dfds, myds(dat$CrossBehDistracted, "CrossBehDistracted"))
dfds <- rbind(dfds, myds(dat$CrossObsCar, "CrossObsCar"))
dfds <- rbind(dfds, myds(dat$CrossObsSWD, "CrossObsSWD"))
dfds <- rbind(dfds, myds(dat$CrossObsOther, "CrossObsOther"))
dfds <- rbind(dfds, myds(dat$CrossObs, "CrossObs"))
dfds <- rbind(dfds, myds(dat$TimeCurbClean, "TimeCurbClean"))
dfds <- rbind(dfds, myds((dat$TimeCurbClean/60), "I(TimeCurbClean/60)"))
dfds <- rbind(dfds, myds((dat$Weekend==F), "I(dat$Weekend==F)"))
dfds <- rbind(dfds, myds(dat$Weekend, "Weekend"))
dfds <- rbind(dfds, myds((dat$TOD2=="0005"), "I(TOD2=='0005')"))
dfds <- rbind(dfds, myds((dat$TOD2=="0611"), "I(TOD2=='0611')"))
dfds <- rbind(dfds, myds((dat$TOD2=="1217"), "I(TOD2=='1217')"))
dfds <- rbind(dfds, myds((dat$TOD2=="1823"), "I(TOD2=='1823')"))
dfds <- rbind(dfds, myds(dat$AMPeak, "AMPeak"))
dfds <- rbind(dfds, myds(dat$PMPeak, "PMPeak"))
dfds <- rbind(dfds, myds(dat$TEMP, "TEMP"))
dfds <- rbind(dfds, myds((dat$TEMPCAT5=="[-10,32)"), "I(TEMPCAT5=='[-10,32)')"))
dfds <- rbind(dfds, myds((dat$TEMPCAT5=="[32,50)"), "I(TEMPCAT5=='[32,50)')"))
dfds <- rbind(dfds, myds((dat$TEMPCAT5=="[50,65)"), "I(TEMPCAT5=='[50,65)')"))
dfds <- rbind(dfds, myds((dat$TEMPCAT5=="[65,80)"), "I(TEMPCAT5=='[65,80)')"))
dfds <- rbind(dfds, myds((dat$TEMPCAT5=="[80,110)"), "I(TEMPCAT5=='[80,110)')"))
dfds <- rbind(dfds, myds(dat$PRCP, "PRCP"))
dfds <- rbind(dfds, myds(dat$PRCP_01, "PRCP_01"))
dfds <- rbind(dfds, myds(dat$PRCP_05, "PRCP_05"))
# macro-level-two (crossing or signal info) IVs
dfds <- rbind(dfds, myds((datl2$NIntLegs==2), "I(NIntLegs==2)"))
dfds <- rbind(dfds, myds((datl2$NIntLegs==3), "I(NIntLegs==3)"))
dfds <- rbind(dfds, myds((datl2$NIntLegs==4), "I(NIntLegs==4)"))
dfds <- rbind(dfds, myds((datl2$CrossType=="Standard"), "I(CrossType=='Standard')"))
dfds <- rbind(dfds, myds((datl2$CrossType=="Continental"), "I(CrossType=='Continental')"))
dfds <- rbind(dfds, myds(datl2$CrossDist, "CrossDist"))
dfds <- rbind(dfds, myds((datl2$CrossDist-80), "I(CrossDist-80)"))
dfds <- rbind(dfds, myds(datl2$CrossLane, "CrossLane"))
dfds <- rbind(dfds, myds((datl2$CrossAADT/1000), "I(CrossAADT/1000)"))
dfds <- rbind(dfds, myds(datl2$SpeedLim, "SpeedLim"))
dfds <- rbind(dfds, myds((datl2$Median==T), "I(Median==T)"))
dfds <- rbind(dfds, myds(datl2$MedWidth, "MedWidth"))
dfds <- rbind(dfds, myds((datl2$DistNearCross/100), "I(DistNearCross/100)"))
dfds <- rbind(dfds, myds((datl2$StreetLight==T), "I(StreetLight==T)"))
dfds <- rbind(dfds, myds((datl2$BikeLane==T), "I(BikeLane==T)"))
dfds <- rbind(dfds, myds((datl2$TranStop==T), "I(TranStop==T)"))
dfds <- rbind(dfds, myds((datl2$GasConv==T), "I(GasConv==T)"))
dfds <- rbind(dfds, myds(datl2$D1A, "D1A"))
dfds <- rbind(dfds, myds(datl2$D1B, "D1B"))
dfds <- rbind(dfds, myds(datl2$D1C, "D1C"))
dfds <- rbind(dfds, myds(datl2$D2A_JPHH, "D2A_JPHH"))
dfds <- rbind(dfds, myds(datl2$D3B, "D3B"))
dfds <- rbind(dfds, myds(datl2$TranStops, "TranStops"))
dfds <- rbind(dfds, myds(datl2$Liqours, "Liqours"))
dfds <- rbind(dfds, myds(datl2$Schools, "Schools"))
dfds <- rbind(dfds, myds(datl2$Worships, "Worships"))
dfds <- rbind(dfds, myds(datl2$ParkAcres, "ParkAcres"))
dfds <- rbind(dfds, myds(datl2$HHSize, "HHSize"))
dfds <- rbind(dfds, myds((datl2$IncMed/1000), "I(IncMed/1000)"))
dfds <- rbind(dfds, myds(datl2$VehOwn, "VehOwn"))
dfds <- rbind(dfds, myds(datl2$PercDisab, "PercDisab"))
dfds <- rbind(dfds, myds(datl2$PercNonWhite, "PercNonWhite"))
# summarize
dfds <- dfds[2:nrow(dfds),]
dfds
rm(dfds, myds, datl2)
# more descriptive stats for figures
summary(dat$AgeChild)
summary(dat$AgeTeen)
summary(dat$AgeAdultYoung)
summary(dat$AgeAdultMiddle)
summary(dat$AgeAdultOlder)
summary(dat$AgeAdultUnknown)
summary(dat$GenderMale)
summary(dat$GenderFemale)
summary(dat$GenderUnknown)
summary(dat$OtherStroller)
summary(dat$OtherLoad)
summary(dat$OtherWheelchair)
summary(dat$OtherSkateboard)
summary(dat$OtherScooter)
summary(dat$OtherBicycle)
summary(dat$OtherOther)
summary(dat$VehiclesPast10>0)
summary(dat$VehiclesNext10>0)
summary(dat$GroupSize>1)
summary(dat$WaitOtherPeople>0)
summary(dat$CrossOtherPeople>0)
summary(dat$Hour)
summary(dat$TOD1)
summary(dat$TOD2)
summary(dat$Weekday)
summary(dat$Weekend)
summary(dat$TEMPCAT5)
summary(dat$PRCP_01)
table(format(dat$Date, format="%m"))

# Descriptive information about study locations
datl2 <- dat[!duplicated(dat$Folder),c("Folder", "Signal", "PedLeg")]
datl1 <- datl2[!duplicated(datl2[,c("Signal", "PedLeg")]),c("Signal", "PedLeg")]
datl1 <- datl1[order(datl1$Signal, datl1$PedLeg),]
signals <- sort(unique(dat$Signal))
datl0 <- dat[!duplicated(dat[c("City", "Signal")]),c("City", "Signal")]
datl0 <- datl0[order(datl0$City, datl0$Signal),]
# inspect
signals
datl0
datl1
datl2
# cleanup
rm(signals, datl0, datl1, datl2)

########################################
# Select independent variables

# Inspect
names(dat)

# DVs
dvs <- c("SDV1MB2", "SDV2OA2", "TDV105", "TDV2SD")

# Level 2 variable for random intercept/slope
dat$Crossing <- paste(dat$Signal, dat$PedLeg)

# Micro-level-one (pedestrian or event info) IVs
# construct new IVs
dat$Vehicles10Avg <- (dat$VehiclesPast10 + dat$VehiclesNext10)/2
dat$TimeWaitClean60 <- dat$TimeWaitClean/60
dat$CrossOtherPeopleT <- dat$CrossOtherPeople>0
dat$WeekendF <- dat$Weekend==F
dat$TOD0005 <- dat$TOD2=="0005"
dat$TOD0611 <- dat$TOD2=="0611"
dat$TOD1217 <- dat$TOD2=="1217"
dat$TOD1823 <- dat$TOD2=="1823"
dat$TEMP0031 <- dat$TEMPCAT5=="[-10,32)"
dat$TEMP3249 <- dat$TEMPCAT5=="[32,50)"
dat$TEMP5064 <- dat$TEMPCAT5=="[50,65)"
dat$TEMP6579 <- dat$TEMPCAT5=="[65,80)"
dat$TEMP8099 <- dat$TEMPCAT5=="[80,110)"
# select variables
ivs1 <- c("GroupSize", "AgeChild", "AgeTeen", "AgeAdultYoung", 
          "AgeAdultMiddle", "AgeAdultOlder", "AgeAdultUnknown", 
          "GenderMale", "GenderFemale", "GenderUnknown", 
          "OtherStroller", "OtherLoad", "OtherWheelchair", "OtherSkateboard", 
          "OtherScooter", "OtherBicycle", "OtherOther", "OtherStrollerLoad", "OtherMode", 
          "WaitOtherPeople", "VehiclesPast10", "VehiclesNext10", "Vehicles10Avg", 
          "WaitBehPressed", "WaitBehPaced", "WaitBehLeft", "WaitBehOther", "TimeWaitClean60", 
          "CrossOtherPeopleSame", "CrossOtherPeopleOppo", "CrossOtherPeople", "CrossOtherPeopleT", 
          "CrossBehSpeed", "CrossBehPaused", "CrossBehDistracted", "CrossBehOther", 
          "CrossObsCar", "CrossObsSWD", "CrossObsOther", "CrossObs", "TimeCurbClean", 
          "WeekendF", "Weekend", 
          "TOD0005", "TOD0611", "TOD1217", "TOD1823", "AMPeak", "PMPeak", 
          "TEMP", "TEMP0031", "TEMP3249", "TEMP5064", "TEMP6579", "TEMP8099", 
          "PRCP", "PRCP_01", "PRCP_05")

# Macro-level-two (crossing or signal info) IVs
# construct new IVs
dat$NIntLegs2 <- dat$NIntLegs==2
dat$NIntLegs3 <- dat$NIntLegs==3
dat$NIntLegs4 <- dat$NIntLegs==4
dat$CrossTypeStan <- dat$CrossType=="Standard"
dat$CrossTypeCont <- dat$CrossType=="Continental"
dat$CrossDist80 <- dat$CrossDist-80
dat$CrossAADT1000 <- dat$CrossAADT/1000
dat$MedianT <- dat$Median==T
dat$DistNearCross100 <- dat$DistNearCross/100
dat$StreetLightT <- dat$StreetLight==T
dat$BikeLaneT <- dat$BikeLane==T
dat$TranStopT <- dat$TranStop==T
dat$GasConvT <- dat$GasConv==T
dat$IncMed1000 <- dat$IncMed/1000
# select variables
ivs2 <- c("NIntLegs2", "NIntLegs3", "NIntLegs4", "CrossTypeStan", "CrossTypeCont", 
          "CrossDist80", "CrossLane", "CrossAADT1000", 
          "SpeedLim", "MedianT", "MedWidth", "DistNearCross100", 
          "StreetLightT", "BikeLaneT", "TranStopT", "GasConvT", 
          "D1A", "D1B", "D1C", "D2A_JPHH", "D3B", 
          "TranStops", "Liqours", "Schools", "Worships", "ParkAcres", 
          "HHSize", "IncMed1000", "VehOwn", "PercDisab", "PercNonWhite")

# Correlations
# cor_dvs <- cor(dat[,dvs])
# cor_ivs1 <- cor(dat[,ivs1])
# cor_ivs2 <- cor(dat[,ivs2])
mycor <- cor(dat[c(dvs, ivs1, ivs2)], use="pairwise.complete.obs")
# write.csv(mycor, file.path("Analysis", "mycor.csv"))

# Exclude some IVs because
# - AgeAdultUnknown: base category
# - GenderUnknown: base category
# - OtherStroller + OtherLoad: combined into OtherStrollerLoad
# - OtherWheelchair + OtherSkateboard + OtherScooter + OtherBicycle: combined into OtherMode
# - OtherOther: lack of relevance
# - VehiclesPast10 + VehiclesNext10: combined into Vehicles10Avg
# - WaitBehPressed + WaitBehPaced: potential indicators of tendency to (not) violate signal
# - WaitBehLeft: NA correlation with DVs
# - WaitBehOther: lack of relevance
# - CrossOtherPeopleSame + CrossOtherPeopleOppo: combined into CrossOtherPeople
# - CrossBehSpeed + CrossBehPaused + CrossBehDistracted: also crossing outcomes
# - CrossBehOther: lack of relevance
# - CrossObsCar + CrossObsSWD: combined into CrossObs
# - CrossObsOther: lack of relevance
# - TimeCurbClean: also a crossing outcome, correlated with CrossDist
# - WeekendF: base category
# - TOD1217: base category
# - AMPeak + PMPeak: correlated with TOD
# - TEMP3249: base category
# - NIntLegs4: base category
# - CrossTypeStan: base category
# - CrossLane: correlated with CrossDist80
# - SpeedLim: correlated with CrossAADT1000
# - MedianT: correlated with MedWidth
# - StreetLightT: NA correlation with all DVs & IVs
# - D1B: correlated with D1A
# - Worships: correlated with TranStops
# - VehOwn: correlated with IncMed1000

# Final list of potential level 1 IVs
# - GroupSize
# - AgeChild + AgeTeen + AgeAdultYoung + AgeAdultMiddle + AgeAdultOlder [or] AgeChild + AgeTeen + AgeAdultOlder
# - GenderMale + GenderFemale [or] GenderFemale
# - OtherStrollerLoad + OtherMode
# - WaitOtherPeople
# - Vehicles10Avg
# - TimeWaitClean60
# - CrossOtherPeople [or] CrossOtherPeopleT
# - CrossObs
# - Weekend
# - TOD0005 + TOD0611 + TOD1823
# - TEMP [or] TEMP0031 + TEMP5064 + TEMP6579 + TEMP8099
# - PRCP [or] PRCP_01 + PRCP_05

# Final list of potential level 2 IVs
# - NIntLegs2 + NIntLegs3
# - CrossTypeCont
# - CrossDist80
# - CrossAADT1000
# - MedWidth
# - DistNearCross100
# - BikeLaneT
# - TranStopT
# - GasConvT
# - D1A
# - D1C
# - D2A_JPHH
# - D3B
# - TranStops
# - Liqours
# - Schools
# - ParkAcres
# - HHSize
# - IncMed1000
# - PercDisab
# - PercNonWhite

# Remove
rm(mycor, dvs, ivs1, ivs2)

########################################
# Correlation with crashes

# Assemble pedestrian crossing behaviors
t1 <- aggregate(SDV1MB2 ~ Signal, data=dat, FUN=sum)
t2 <- aggregate(SDV2OA2 ~ Signal, data=dat, FUN=sum)
t3 <- aggregate(TDV105  ~ Signal, data=dat, FUN=sum)
t4 <- aggregate(TDV2SD  ~ Signal, data=dat, FUN=sum)
temp <- merge(t1, t2, by="Signal", all.x=T, all.y=T)
temp <- merge(temp, t3, by="Signal", all.x=T, all.y=T)
temp <- merge(temp, t4, by="Signal", all.x=T, all.y=T)
rm(t1, t2, t3, t4)

# Load video data
vids <- read.csv(file=file.path("Data", "VideoInfo.csv"))
# process
vids$Time1 <- as.POSIXct(vids$TimeBeg, format="%m/%d/%Y %H:%M", tz="America/Denver")
vids$Time2 <- as.POSIXct(vids$TimeEnd, format="%m/%d/%Y %H:%M", tz="America/Denver")
vids$TDiff <- as.numeric(difftime(vids$Time2, vids$Time1, units="hours"))
# aggregate
vtime <- aggregate(TDiff ~ Signal, data=vids, FUN=sum)
summary(vtime$TDiff)

# Load crash data
sig <- readRDS(file.path("Data", "from UDOT-19.318", "sig.rds"))
sig_pred <- readRDS(file.path("Data", "from UDOT-19.318", "sig_pred.rds"))
crash <- sig_pred[,c("SIGNALID", "CRASH_WALK", "P-Sig-All-A", "P-Sig-All-A-EB")]
rm(sig, sig_pred)

# Merge and calculate
temp <- merge(temp, vtime, by="Signal", all.x=T, all.y=F)
temp$SDV1MB2hr <- temp$SDV1MB2 / temp$TDiff
temp$SDV2OA2hr <- temp$SDV2OA2 / temp$TDiff
temp$TDV105hr  <- temp$TDV105  / temp$TDiff
temp$TDV2SDhr  <- temp$TDV2SD  / temp$TDiff
temp <- merge(temp, crash, by.x="Signal", by.y="SIGNALID", all.x=T, all.y=F)
temp$CRASHyr   <- temp$CRASH_WALK / 10
temp$CRMOD1yr  <- temp$'P-Sig-All-A' / 10
temp$CRMOD2yr  <- temp$'P-Sig-All-A-EB' / 10

# Correlation
cor(temp[,c("CRASHyr", "CRMOD1yr", "CRMOD2yr", "SDV1MB2hr", "SDV2OA2hr", "TDV105hr", "TDV2SDhr")], use="pairwise.complete.obs")
cor.test(temp$CRASHyr, temp$SDV1MB2hr)
cor.test(temp$CRASHyr, temp$SDV2OA2hr)
cor.test(temp$CRASHyr, temp$TDV105hr)
cor.test(temp$CRASHyr, temp$TDV2SDhr)
cor.test(temp$CRMOD1yr, temp$SDV1MB2hr)
cor.test(temp$CRMOD1yr, temp$SDV2OA2hr)
cor.test(temp$CRMOD1yr, temp$TDV105hr)
cor.test(temp$CRMOD1yr, temp$TDV2SDhr)
cor.test(temp$CRMOD2yr, temp$SDV1MB2hr)
cor.test(temp$CRMOD2yr, temp$SDV2OA2hr)
cor.test(temp$CRMOD2yr, temp$TDV105hr)
cor.test(temp$CRMOD2yr, temp$TDV2SDhr)

# Remove
rm(temp, vids, vtime, crash)

########################################
# Walking speed

# Initialize
temp <- dat
myps <- c(0.00,0.05,0.10,0.15,0.25,0.50,0.75,0.85,0.90,0.95,1.00)

# Calculate walking speed
temp$WS <- temp$CrossDist / temp$TimeCurbClean
summary(temp$WS)

# Filter
temp <- temp[!is.na(temp$WS),]

# Overall
quantile(temp$WS, probs=myps, na.rm=T); prop.table(table(temp$WS<4.00))

# By mode
# other characteristics: 
summary(temp[,c("OtherStroller", "OtherLoad", "OtherWheelchair", "OtherSkateboard", "OtherScooter", "OtherBicycle", "OtherOther")])
quantile(temp$WS[temp$OtherStroller==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$OtherStroller==T]<4.00))
quantile(temp$WS[temp$OtherLoad==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$OtherLoad==T]<4.00))
quantile(temp$WS[temp$OtherWheelchair==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$OtherWheelchair==T]<4.00))
quantile(temp$WS[temp$OtherSkateboard==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$OtherSkateboard==T]<4.00))
quantile(temp$WS[temp$OtherScooter==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$OtherScooter==T]<4.00))
quantile(temp$WS[temp$OtherBicycle==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$OtherBicycle==T]<4.00))
quantile(temp$WS[temp$OtherOther==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$OtherOther==T]<4.00))
# other modes: Wheelchair OR Skateboard OR Scooter OR Bicycle
summary(temp$OtherMode)
quantile(temp$WS[temp$OtherMode==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$OtherMode==T]<4.00))
quantile(temp$WS[temp$OtherMode==F], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$OtherMode==F]<4.00))
# just pedestrians: NOT Wheelchair NOR Skateboard NOR Scooter NOR Bicycle NOR Other
temp$OnlyPeds <- !(temp$OtherMode | temp$OtherOther)
summary(temp$OnlyPeds)
quantile(temp$WS[temp$OnlyPeds==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$OnlyPeds==T]<4.00))

# By age
summary(temp[,c("AgeChild", "AgeTeen", "AgeAdultYoung", "AgeAdultMiddle", "AgeAdultOlder", "AgeAdultUnknown")])
quantile(temp$WS[temp$AgeChild==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$AgeChild==T]<4.00))
quantile(temp$WS[temp$AgeTeen==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$AgeTeen==T]<4.00))
quantile(temp$WS[temp$AgeAdultYoung==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$AgeAdultYoung==T]<4.00))
quantile(temp$WS[temp$AgeAdultMiddle==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$AgeAdultMiddle==T]<4.00))
quantile(temp$WS[temp$AgeAdultOlder==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$AgeAdultOlder==T]<4.00))
quantile(temp$WS[temp$AgeAdultUnknown==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$AgeAdultUnknown==T]<4.00))

# By gender
summary(temp[,c("GenderMale", "GenderFemale", "GenderUnknown")])
quantile(temp$WS[temp$GenderMale==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$GenderMale==T]<4.00))
quantile(temp$WS[temp$GenderFemale==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$GenderFemale==T]<4.00))
quantile(temp$WS[temp$GenderUnknown==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$GenderUnknown==T]<4.00))

# By group size
summary(temp$GroupSize)
table(temp$GroupSize)
temp$GS1 <- temp$GroupSize==1
temp$GS2 <- temp$GroupSize==2
temp$GS3 <- temp$GroupSize>=3
summary(temp[,c("GS1", "GS2", "GS3")])
quantile(temp$WS[temp$GS1==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$GS1==T]<4.00))
quantile(temp$WS[temp$GS2==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$GS2==T]<4.00))
quantile(temp$WS[temp$GS3==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$GS3==T]<4.00))

# Filter to try to match BYU study
# - only in the crosswalk or the crosswalk area
# - not skateboard, scooter, bicycle, other
# - didn't pause or change speed
# - started or ended on W or FDW (not SDW SDW)
temp <- temp[!is.na(temp$CrossLoc) & temp$CrossLoc=="In the crosswalk or the crosswalk area",]
temp <- temp[temp$OtherSkateboard==F & temp$OtherScooter==F & temp$OtherBicycle==F & temp$OtherOther==F,]
temp <- temp[temp$CrossBehPaused==F & temp$CrossBehSpeed==F,]
temp <- temp[!is.na(temp$TDV4) & temp$TDV4!="SDW SDW",]

# Overall
quantile(temp$WS, probs=myps, na.rm=T); prop.table(table(temp$WS<4.00))

# By mode
# other characteristics: 
summary(temp[,c("OtherStroller", "OtherLoad", "OtherWheelchair")])
quantile(temp$WS[temp$OtherStroller==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$OtherStroller==T]<4.00))
quantile(temp$WS[temp$OtherLoad==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$OtherLoad==T]<4.00))
quantile(temp$WS[temp$OtherWheelchair==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$OtherWheelchair==T]<4.00))
# just pedestrians: NOT Wheelchair
summary(temp$OnlyPeds)
quantile(temp$WS[temp$OnlyPeds==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$OnlyPeds==T]<4.00))

# By age
summary(temp[,c("AgeChild", "AgeTeen", "AgeAdultYoung", "AgeAdultMiddle", "AgeAdultOlder", "AgeAdultUnknown")])
quantile(temp$WS[temp$AgeChild==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$AgeChild==T]<4.00))
quantile(temp$WS[temp$AgeTeen==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$AgeTeen==T]<4.00))
quantile(temp$WS[temp$AgeAdultYoung==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$AgeAdultYoung==T]<4.00))
quantile(temp$WS[temp$AgeAdultMiddle==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$AgeAdultMiddle==T]<4.00))
quantile(temp$WS[temp$AgeAdultOlder==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$AgeAdultOlder==T]<4.00))
quantile(temp$WS[temp$AgeAdultUnknown==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$AgeAdultUnknown==T]<4.00))

# By gender
summary(temp[,c("GenderMale", "GenderFemale", "GenderUnknown")])
quantile(temp$WS[temp$GenderMale==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$GenderMale==T]<4.00))
quantile(temp$WS[temp$GenderFemale==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$GenderFemale==T]<4.00))
quantile(temp$WS[temp$GenderUnknown==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$GenderUnknown==T]<4.00))

# By group size
summary(temp[,c("GS1", "GS2", "GS3")])
quantile(temp$WS[temp$GS1==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$GS1==T]<4.00))
quantile(temp$WS[temp$GS2==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$GS2==T]<4.00))
quantile(temp$WS[temp$GS3==T], probs=myps, na.rm=T); prop.table(table(temp$WS[temp$GS3==T]<4.00))

# Remove
rm(temp, myps)

########################################
# Multilevel multivariate analysis
# logistic regression on SDV1MB2

# Create dataset
dats1 <- dat[!is.na(dat$SDV1MB2) & !is.na(dat$TimeWaitClean60) & !is.na(dat$Vehicles10Avg),]

# One-level intercept-only model
mod01 <- glm(SDV1MB2 ~ 1, data=dats1, family=binomial)
summary(mod01)

# Multilevel model (empty) with random intercept
mod02 <- glmer(SDV1MB2 ~ (1 | Crossing), data=dats1, family=binomial)
summary(mod02)
anova(mod02, mod01) # better with random intercept

# Try different micro-level-one (pedestrian or event info) IVs as fixed effects
# test variables one-at-a-time, pick between specifications using BIC
mod <- update(mod02, formula= . ~ . + GroupSize)
# mod <- update(mod02, formula= . ~ . + AgeChild + AgeTeen + AgeAdultYoung + AgeAdultMiddle + AgeAdultOlder)
mod <- update(mod02, formula= . ~ . + AgeChild + AgeTeen + AgeAdultOlder)
# mod <- update(mod02, formula= . ~ . + GenderMale + GenderFemale)
mod <- update(mod02, formula= . ~ . + GenderFemale)
mod <- update(mod02, formula= . ~ . + OtherStrollerLoad + OtherMode)
mod <- update(mod02, formula= . ~ . + WaitOtherPeople)
mod <- update(mod02, formula= . ~ . + Vehicles10Avg)
mod <- update(mod02, formula= . ~ . + TimeWaitClean60)
# mod <- update(mod02, formula= . ~ . + CrossOtherPeople)
mod <- update(mod02, formula= . ~ . + CrossOtherPeopleT)
mod <- update(mod02, formula= . ~ . + CrossObs)
mod <- update(mod02, formula= . ~ . + Weekend)
mod <- update(mod02, formula= . ~ . + TOD0005 + TOD0611 + TOD1823)
mod <- update(mod02, formula= . ~ . + TEMP)
# mod <- update(mod02, formula= . ~ . + TEMP0031 + TEMP5064 + TEMP6579 + TEMP8099)
mod <- update(mod02, formula= . ~ . + PRCP)
# mod <- update(mod02, formula= . ~ . + PRCP_01 + PRCP_05)
summary(mod)
# sig: GroupSize + AgeTeen + GenderFemale + OtherMode + TimeWaitClean60 + CrossOtherPeopleT + TOD0005 + TEMP
rm(mod)

# Multilevel model with random intercept and micro-level-one IVs
# test specifications, backwards removal
mod03a <- update(mod02, formula= . ~ . + GroupSize + AgeTeen + GenderFemale + OtherMode + TimeWaitClean60 + CrossOtherPeopleT + TOD0005 + TEMP)
summary(mod03a)
mod03b <- update(mod03a, formula= . ~ . - AgeTeen)
summary(mod03b)
anova(mod03b, mod03a) # okay to remove AgeTeen
mod03c <- update(mod03b, formula= . ~ . - GroupSize)
summary(mod03c)
anova(mod03c, mod03b) # okay to remove GroupSize
rm(mod03a, mod03b, mod03c)
# final specification
mod03 <- update(mod02, formula= . ~ . + GenderFemale + OtherMode + TimeWaitClean60 + CrossOtherPeopleT + TOD0005 + TEMP)
summary(mod03)
anova(mod03, mod02) # better with new IVs

# Try different macro-level-two (crossing or signal info) IVs as fixed effects
# test variables one-at-a-time, pick between specifications using BIC
mod <- update(mod02, formula= . ~ . + NIntLegs2 + NIntLegs3)
mod <- update(mod02, formula= . ~ . + CrossTypeCont)
mod <- update(mod02, formula= . ~ . + CrossDist80)
mod <- update(mod02, formula= . ~ . + CrossAADT1000)
mod <- update(mod02, formula= . ~ . + MedWidth)
mod <- update(mod02, formula= . ~ . + DistNearCross100)
mod <- update(mod02, formula= . ~ . + BikeLaneT)
mod <- update(mod02, formula= . ~ . + TranStopT)
mod <- update(mod02, formula= . ~ . + GasConvT)
mod <- update(mod02, formula= . ~ . + D1A)
mod <- update(mod02, formula= . ~ . + D1C)
mod <- update(mod02, formula= . ~ . + D2A_JPHH)
mod <- update(mod02, formula= . ~ . + D3B)
mod <- update(mod02, formula= . ~ . + TranStops)
mod <- update(mod02, formula= . ~ . + Liqours)
mod <- update(mod02, formula= . ~ . + Schools)
mod <- update(mod02, formula= . ~ . + ParkAcres)
mod <- update(mod02, formula= . ~ . + HHSize)
mod <- update(mod02, formula= . ~ . + IncMed1000)
mod <- update(mod02, formula= . ~ . + PercDisab)
mod <- update(mod02, formula= . ~ . + PercNonWhite)
summary(mod)
# sig: CrossDist80 + CrossAADT1000 + Liqours + PercNonWhite
rm(mod)

# Multilevel model with random intercept and macro-level-two IVs
# test specifications, backwards removal
mod04a <- update(mod02, formula= . ~ . + CrossDist80 + CrossAADT1000 + Liqours + PercNonWhite)
summary(mod04a)
mod04b <- update(mod04a, formula= . ~ . - CrossDist80)
summary(mod04b)
anova(mod04b, mod04a) # okay to remove CrossDist80
rm(mod04a, mod04b)
# final specification
mod04 <- update(mod02, formula= . ~ . + CrossAADT1000 + Liqours + PercNonWhite)
summary(mod04)
anova(mod04, mod02) # better with new IVs

# Multilevel model with random intercept and both micro-level-one IVs and macro-level-two IVs
# test specifications, backwards removal
mod05a <- update(mod03, formula= . ~ . + CrossAADT1000 + Liqours + PercNonWhite)
summary(mod05a)
mod05b <- update(mod05a, formula= . ~ . - Liqours)
summary(mod05b)
anova(mod05b, mod05a) # okay to remove Liqours
rm(mod05a, mod05b)
# final specification
mod05 <- update(mod03, formula= . ~ . + CrossAADT1000 + PercNonWhite)
summary(mod05)
anova(mod05, mod03) # better with new IVs
anova(mod05, mod04) # better with new IVs

# Multilevel model with random slope
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(as.integer(GenderFemale)) | Crossing))
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(as.integer(OtherMode)) | Crossing))
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(TimeWaitClean60) | Crossing))
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(as.integer(CrossOtherPeopleT)) | Crossing))
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(as.integer(TOD0005)) | Crossing))
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(TEMP) | Crossing))
summary(mod06)
anova(mod06, mod05)
# sig: TimeWaitClean60
rm(mod06)

# Summarize, combine, and save all models
summary(mod01)
summary(mod02)
summary(mod03)
summary(mod04)
summary(mod05)
modsSDV1MB2 <- list(dats1, mod01, mod02, mod03, mod04, mod05)
names(modsSDV1MB2) <- c("dats1", "mod01", "mod02", "mod03", "mod04", "mod05")
saveRDS(modsSDV1MB2, file=file.path("Analysis", "modsSDV1MB2.rds"))

# Cleanup
rm(mod01, mod02, mod03, mod04, mod05)
rm(modsSDV1MB2, dats1)
gc()

########################################
# Multilevel multivariate analysis
# logistic regression on SDV2OA2

# Create dataset
dats2 <- dat[!is.na(dat$SDV2OA2) & !is.na(dat$TimeWaitClean60) & !is.na(dat$Vehicles10Avg),]

# One-level intercept-only model
mod01 <- glm(SDV2OA2 ~ 1, data=dats2, family=binomial)
summary(mod01)

# Multilevel model (empty) with random intercept
mod02 <- glmer(SDV2OA2 ~ (1 | Crossing), data=dats2, family=binomial)
summary(mod02)
anova(mod02, mod01) # better with random intercept

# Try different micro-level-one (pedestrian or event info) IVs as fixed effects
# test variables one-at-a-time, pick between specifications using BIC
mod <- update(mod02, formula= . ~ . + GroupSize)
# mod <- update(mod02, formula= . ~ . + AgeChild + AgeTeen + AgeAdultYoung + AgeAdultMiddle + AgeAdultOlder)
mod <- update(mod02, formula= . ~ . + AgeChild + AgeTeen + AgeAdultOlder)
# mod <- update(mod02, formula= . ~ . + GenderMale + GenderFemale)
mod <- update(mod02, formula= . ~ . + GenderFemale)
mod <- update(mod02, formula= . ~ . + OtherStrollerLoad + OtherMode)
mod <- update(mod02, formula= . ~ . + WaitOtherPeople)
mod <- update(mod02, formula= . ~ . + Vehicles10Avg)
mod <- update(mod02, formula= . ~ . + TimeWaitClean60)
mod <- update(mod02, formula= . ~ . + CrossOtherPeople)
# mod <- update(mod02, formula= . ~ . + CrossOtherPeopleT)
mod <- update(mod02, formula= . ~ . + CrossObs)
mod <- update(mod02, formula= . ~ . + Weekend)
mod <- update(mod02, formula= . ~ . + TOD0005 + TOD0611 + TOD1823)
mod <- update(mod02, formula= . ~ . + TEMP)
# mod <- update(mod02, formula= . ~ . + TEMP0031 + TEMP5064 + TEMP6579 + TEMP8099)
mod <- update(mod02, formula= . ~ . + PRCP)
# mod <- update(mod02, formula= . ~ . + PRCP_01 + PRCP_05)
summary(mod)
# sig: OtherMode + WaitOtherPeople + Vehicles10Avg + TimeWaitClean60 + CrossOtherPeople + TEMP
rm(mod)

# Multilevel model with random intercept and micro-level-one IVs
# test specifications, backwards removal
mod03a <- update(mod02, formula= . ~ . + OtherMode + WaitOtherPeople + Vehicles10Avg + TimeWaitClean60 + CrossOtherPeople + TEMP)
summary(mod03a)
mod03b <- update(mod03a, formula= . ~ . - WaitOtherPeople)
summary(mod03b)
anova(mod03b, mod03a) # okay to remove WaitOtherPeople
rm(mod03a, mod03b)
# final specification
mod03 <- update(mod02, formula= . ~ . + OtherMode + Vehicles10Avg + TimeWaitClean60 + CrossOtherPeople + TEMP)
summary(mod03)
anova(mod03, mod02) # better with new IVs

# Try different macro-level-two (crossing or signal info) IVs as fixed effects
# test variables one-at-a-time, pick between specifications using BIC
mod <- update(mod02, formula= . ~ . + NIntLegs2 + NIntLegs3)
mod <- update(mod02, formula= . ~ . + CrossTypeCont)
mod <- update(mod02, formula= . ~ . + CrossDist80)
mod <- update(mod02, formula= . ~ . + CrossAADT1000)
mod <- update(mod02, formula= . ~ . + MedWidth)
mod <- update(mod02, formula= . ~ . + DistNearCross100)
mod <- update(mod02, formula= . ~ . + BikeLaneT)
mod <- update(mod02, formula= . ~ . + TranStopT)
mod <- update(mod02, formula= . ~ . + GasConvT)
mod <- update(mod02, formula= . ~ . + D1A)
mod <- update(mod02, formula= . ~ . + D1C)
mod <- update(mod02, formula= . ~ . + D2A_JPHH)
mod <- update(mod02, formula= . ~ . + D3B)
mod <- update(mod02, formula= . ~ . + TranStops)
mod <- update(mod02, formula= . ~ . + Liqours)
mod <- update(mod02, formula= . ~ . + Schools)
mod <- update(mod02, formula= . ~ . + ParkAcres)
mod <- update(mod02, formula= . ~ . + HHSize)
mod <- update(mod02, formula= . ~ . + IncMed1000)
mod <- update(mod02, formula= . ~ . + PercDisab)
mod <- update(mod02, formula= . ~ . + PercNonWhite)
summary(mod)
# sig: CrossDist80 + CrossAADT1000 + DistNearCross100 + GasConvT + Schools
rm(mod)

# Multilevel model with random intercept and macro-level-two IVs
# test specifications, backwards removal
mod04a <- update(mod02, formula= . ~ . + CrossDist80 + CrossAADT1000 + DistNearCross100 + GasConvT + Schools)
summary(mod04a)
mod04b <- update(mod04a, formula= . ~ . - GasConvT)
summary(mod04b)
anova(mod04b, mod04a) # okay to remove GasConvT
mod04c <- update(mod04b, formula= . ~ . - CrossAADT1000)
summary(mod04c)
anova(mod04c, mod04b) # okay to remove CrossAADT1000
mod04d <- update(mod04c, formula= . ~ . - DistNearCross100)
summary(mod04d)
anova(mod04d, mod04c) # okay to remove DistNearCross100
rm(mod04a, mod04b, mod04c, mod04d)
# final specification
mod04 <- update(mod02, formula= . ~ . + CrossDist80 + Schools)
summary(mod04)
anova(mod04, mod02) # better with new IVs

# Multilevel model with random intercept and both micro-level-one IVs and macro-level-two IVs
# test specifications, backwards removal
mod05a <- update(mod03, formula= . ~ . + CrossDist80 + Schools)
summary(mod05a)
mod05b <- update(mod05a, formula= . ~ . - Schools)
summary(mod05b)
anova(mod05b, mod05a) # okay to remove Schools
rm(mod05a, mod05b)
# final specification
mod05 <- update(mod03, formula= . ~ . + CrossDist80)
summary(mod05)
anova(mod05, mod03) # better with new IVs
anova(mod05, mod04) # better with new IVs

# Multilevel model with random slope
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(as.integer(OtherMode)) | Crossing))
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(Vehicles10Avg) | Crossing))
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(TimeWaitClean60) | Crossing))
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(as.integer(CrossOtherPeopleT)) | Crossing))
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(TEMP) | Crossing))
summary(mod06)
anova(mod06, mod05)
# sig: OtherMode
rm(mod06)

# Summarize, combine, and save all models
summary(mod01)
summary(mod02)
summary(mod03)
summary(mod04)
summary(mod05)
modsSDV2OA2 <- list(dats2, mod01, mod02, mod03, mod04, mod05)
names(modsSDV2OA2) <- c("dats2", "mod01", "mod02", "mod03", "mod04", "mod05")
saveRDS(modsSDV2OA2, file=file.path("Analysis", "modsSDV2OA2.rds"))

# Cleanup
rm(mod01, mod02, mod03, mod04, mod05)
rm(modsSDV2OA2, dats2)
gc()

########################################
# Multilevel multivariate analysis
# logistic regression on TDV105

# Create dataset
datt1 <- dat[!is.na(dat$TDV105) & !is.na(dat$TimeWaitClean60) & !is.na(dat$Vehicles10Avg),]

# One-level intercept-only model
mod01 <- glm(TDV105 ~ 1, data=datt1, family=binomial)
summary(mod01)

# Multilevel model (empty) with random intercept
mod02 <- glmer(TDV105 ~ (1 | Crossing), data=datt1, family=binomial)
summary(mod02)
anova(mod02, mod01) # better with random intercept

# Try different micro-level-one (pedestrian or event info) IVs as fixed effects
# test variables one-at-a-time, pick between specifications using BIC
mod <- update(mod02, formula= . ~ . + GroupSize)
# mod <- update(mod02, formula= . ~ . + AgeChild + AgeTeen + AgeAdultYoung + AgeAdultMiddle + AgeAdultOlder)
mod <- update(mod02, formula= . ~ . + AgeChild + AgeTeen + AgeAdultOlder)
# mod <- update(mod02, formula= . ~ . + GenderMale + GenderFemale)
mod <- update(mod02, formula= . ~ . + GenderFemale)
mod <- update(mod02, formula= . ~ . + OtherStrollerLoad + OtherMode)
mod <- update(mod02, formula= . ~ . + WaitOtherPeople)
mod <- update(mod02, formula= . ~ . + Vehicles10Avg)
mod <- update(mod02, formula= . ~ . + TimeWaitClean60)
mod <- update(mod02, formula= . ~ . + CrossOtherPeople)
# mod <- update(mod02, formula= . ~ . + CrossOtherPeopleT)
mod <- update(mod02, formula= . ~ . + CrossObs)
mod <- update(mod02, formula= . ~ . + Weekend)
mod <- update(mod02, formula= . ~ . + TOD0005 + TOD0611 + TOD1823)
mod <- update(mod02, formula= . ~ . + TEMP)
# mod <- update(mod02, formula= . ~ . + TEMP0031 + TEMP5064 + TEMP6579 + TEMP8099)
mod <- update(mod02, formula= . ~ . + PRCP)
# mod <- update(mod02, formula= . ~ . + PRCP_01 + PRCP_05)
summary(mod)
# sig: AgeChild + Vehicles10Avg + TimeWaitClean60 + CrossOtherPeople + TOD0005 + TOD0611 + TOD1823 + TEMP
rm(mod)

# Multilevel model with random intercept and micro-level-one IVs
# test specifications, backwards removal
mod03a <- update(mod02, formula= . ~ . + AgeChild + Vehicles10Avg + TimeWaitClean60 + CrossOtherPeople + TOD0005 + TOD0611 + TOD1823 + TEMP)
summary(mod03a)
mod03b <- update(mod03a, formula= . ~ . - Vehicles10Avg)
summary(mod03b)
anova(mod03b, mod03a) # okay to remove Vehicles10Avg
mod03c <- update(mod03b, formula= . ~ . - AgeChild)
summary(mod03c)
anova(mod03c, mod03b) # okay to remove AgeChild
mod03d <- update(mod03c, formula= . ~ . - TOD1823)
summary(mod03d)
anova(mod03d, mod03c) # okay to remove TOD1823
rm(mod03a, mod03b, mod03c, mod03d)
# final specification
mod03 <- update(mod02, formula= . ~ . + TimeWaitClean60 + CrossOtherPeople + TOD0005 + TOD0611 + TEMP)
summary(mod03)
anova(mod03, mod02) # better with new IVs

# Try different macro-level-two (crossing or signal info) IVs as fixed effects
# test variables one-at-a-time, pick between specifications using BIC
mod <- update(mod02, formula= . ~ . + NIntLegs2 + NIntLegs3)
mod <- update(mod02, formula= . ~ . + CrossTypeCont)
mod <- update(mod02, formula= . ~ . + CrossDist80)
mod <- update(mod02, formula= . ~ . + CrossAADT1000)
mod <- update(mod02, formula= . ~ . + MedWidth)
mod <- update(mod02, formula= . ~ . + DistNearCross100)
mod <- update(mod02, formula= . ~ . + BikeLaneT)
mod <- update(mod02, formula= . ~ . + TranStopT)
mod <- update(mod02, formula= . ~ . + GasConvT)
mod <- update(mod02, formula= . ~ . + D1A)
mod <- update(mod02, formula= . ~ . + D1C)
mod <- update(mod02, formula= . ~ . + D2A_JPHH)
mod <- update(mod02, formula= . ~ . + D3B)
mod <- update(mod02, formula= . ~ . + TranStops)
mod <- update(mod02, formula= . ~ . + Liqours)
mod <- update(mod02, formula= . ~ . + Schools)
mod <- update(mod02, formula= . ~ . + ParkAcres)
mod <- update(mod02, formula= . ~ . + HHSize)
mod <- update(mod02, formula= . ~ . + IncMed1000)
mod <- update(mod02, formula= . ~ . + PercDisab)
mod <- update(mod02, formula= . ~ . + PercNonWhite)
summary(mod)
# sig: NIntLegs2 + CrossAADT1000 + MedWidth + DistNearCross100 + PercNonWhite
rm(mod)

# Multilevel model with random intercept and macro-level-two IVs
# test specifications, backwards removal
mod04a <- update(mod02, formula= . ~ . + NIntLegs2 + CrossAADT1000 + MedWidth + DistNearCross100 + PercNonWhite)
summary(mod04a)
mod04b <- update(mod04a, formula= . ~ . - MedWidth)
summary(mod04b)
anova(mod04b, mod04a) # okay to remove MedWidth
mod04c <- update(mod04b, formula= . ~ . - DistNearCross100)
summary(mod04c)
anova(mod04c, mod04b) # okay to remove DistNearCross100
rm(mod04a, mod04b, mod04c)
# final specification
mod04 <- update(mod02, formula= . ~ . + NIntLegs2 + CrossAADT1000 + PercNonWhite)
summary(mod04)
anova(mod04, mod02) # better with new IVs

# Multilevel model with random intercept and both micro-level-one IVs and macro-level-two IVs
# test specifications, backwards removal
mod05a <- update(mod03, formula= . ~ . + NIntLegs2 + CrossAADT1000 + PercNonWhite)
summary(mod05a)
mod05b <- update(mod05a, formula= . ~ . - CrossAADT1000)
summary(mod05b)
anova(mod05b, mod05a) # okay to remove CrossAADT1000
rm(mod05a, mod05b)
# final specification
mod05 <- update(mod03, formula= . ~ . + NIntLegs2 + PercNonWhite)
summary(mod05)
anova(mod05, mod03) # better with new IVs
anova(mod05, mod04) # better with new IVs

# Multilevel model with random slope
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(TimeWaitClean60) | Crossing))
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(CrossOtherPeople) | Crossing))
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(as.integer(TOD0005)) | Crossing))
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(as.integer(TOD0611)) | Crossing))
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(TEMP) | Crossing))
summary(mod06)
anova(mod06, mod05)
# sig: TimeWaitClean60, CrossOtherPeople
rm(mod06)

# Summarize, combine, and save all models
summary(mod01)
summary(mod02)
summary(mod03)
summary(mod04)
summary(mod05)
modsTDV105 <- list(datt1, mod01, mod02, mod03, mod04, mod05)
names(modsTDV105) <- c("datt1", "mod01", "mod02", "mod03", "mod04", "mod05")
saveRDS(modsTDV105, file=file.path("Analysis", "modsTDV105.rds"))

# Cleanup
rm(mod01, mod02, mod03, mod04, mod05)
rm(modsTDV105, datt1)
gc()

########################################
# Multilevel multivariate analysis
# logistic regression on TDV2SD

# Create dataset
datt2 <- dat[!is.na(dat$TDV2SD) & !is.na(dat$TimeWaitClean60) & !is.na(dat$Vehicles10Avg),]

# One-level intercept-only model
mod01 <- glm(TDV2SD ~ 1, data=datt2, family=binomial)
summary(mod01)

# Multilevel model (empty) with random intercept
mod02 <- glmer(TDV2SD ~ (1 | Crossing), data=datt2, family=binomial)
summary(mod02)
anova(mod02, mod01) # better with random intercept

# Try different micro-level-one (pedestrian or event info) IVs as fixed effects
# test variables one-at-a-time, pick between specifications using BIC
mod <- update(mod02, formula= . ~ . + GroupSize)
# mod <- update(mod02, formula= . ~ . + AgeChild + AgeTeen + AgeAdultYoung + AgeAdultMiddle + AgeAdultOlder)
mod <- update(mod02, formula= . ~ . + AgeChild + AgeTeen + AgeAdultOlder)
# mod <- update(mod02, formula= . ~ . + GenderMale + GenderFemale)
mod <- update(mod02, formula= . ~ . + GenderFemale)
mod <- update(mod02, formula= . ~ . + OtherStrollerLoad + OtherMode)
mod <- update(mod02, formula= . ~ . + WaitOtherPeople)
mod <- update(mod02, formula= . ~ . + Vehicles10Avg)
mod <- update(mod02, formula= . ~ . + TimeWaitClean60)
# mod <- update(mod02, formula= . ~ . + CrossOtherPeople)
mod <- update(mod02, formula= . ~ . + CrossOtherPeopleT)
mod <- update(mod02, formula= . ~ . + CrossObs)
mod <- update(mod02, formula= . ~ . + Weekend)
mod <- update(mod02, formula= . ~ . + TOD0005 + TOD0611 + TOD1823)
mod <- update(mod02, formula= . ~ . + TEMP)
# mod <- update(mod02, formula= . ~ . + TEMP0031 + TEMP5064 + TEMP6579 + TEMP8099)
mod <- update(mod02, formula= . ~ . + PRCP)
# mod <- update(mod02, formula= . ~ . + PRCP_01 + PRCP_05)
summary(mod)
# sig: GroupSize + AgeChild + AgeTeen + GenderFemale + OtherMode + WaitOtherPeople + Vehicles10Avg + TimeWaitClean60 + CrossOtherPeopleT + CrossObs + Weekend + TOD0005 + TOD0611 + PRCP
rm(mod)

# Multilevel model with random intercept and micro-level-one IVs
# test specifications, backwards removal
mod03a <- update(mod02, formula= . ~ . + GroupSize + AgeChild + AgeTeen + GenderFemale + OtherMode + WaitOtherPeople + Vehicles10Avg + TimeWaitClean60 + CrossOtherPeopleT + CrossObs + Weekend + TOD0005 + TOD0611 + PRCP)
summary(mod03a)
mod03b <- update(mod03a, formula= . ~ . - WaitOtherPeople)
summary(mod03b)
anova(mod03b, mod03a) # okay to remove WaitOtherPeople
mod03c <- update(mod03b, formula= . ~ . - TOD0611)
summary(mod03c)
anova(mod03c, mod03b) # okay to remove TOD0611
mod03d <- update(mod03c, formula= . ~ . - GenderFemale)
summary(mod03d)
anova(mod03d, mod03c) # okay to remove GenderFemale
mod03e <- update(mod03d, formula= . ~ . - Vehicles10Avg)
summary(mod03e)
anova(mod03e, mod03d) # okay to remove Vehicles10Avg
rm(mod03a, mod03b, mod03c, mod03d, mod03e)
# final specification
mod03 <- update(mod02, formula= . ~ . + GroupSize + AgeChild + AgeTeen + OtherMode + TimeWaitClean60 + CrossOtherPeopleT + CrossObs + Weekend + TOD0005 + PRCP)
summary(mod03)
anova(mod03, mod02) # better with new IVs

# Try different macro-level-two (crossing or signal info) IVs as fixed effects
# test variables one-at-a-time, pick between specifications using BIC
mod <- update(mod02, formula= . ~ . + NIntLegs2 + NIntLegs3)
mod <- update(mod02, formula= . ~ . + CrossTypeCont)
mod <- update(mod02, formula= . ~ . + CrossDist80)
mod <- update(mod02, formula= . ~ . + CrossAADT1000)
mod <- update(mod02, formula= . ~ . + MedWidth)
mod <- update(mod02, formula= . ~ . + DistNearCross100)
mod <- update(mod02, formula= . ~ . + BikeLaneT)
mod <- update(mod02, formula= . ~ . + TranStopT)
mod <- update(mod02, formula= . ~ . + GasConvT)
mod <- update(mod02, formula= . ~ . + D1A)
mod <- update(mod02, formula= . ~ . + D1C)
mod <- update(mod02, formula= . ~ . + D2A_JPHH)
mod <- update(mod02, formula= . ~ . + D3B)
mod <- update(mod02, formula= . ~ . + TranStops)
mod <- update(mod02, formula= . ~ . + Liqours)
mod <- update(mod02, formula= . ~ . + Schools)
mod <- update(mod02, formula= . ~ . + ParkAcres)
mod <- update(mod02, formula= . ~ . + HHSize)
mod <- update(mod02, formula= . ~ . + IncMed1000)
mod <- update(mod02, formula= . ~ . + PercDisab)
mod <- update(mod02, formula= . ~ . + PercNonWhite)
summary(mod)
# sig: NIntLegs2 + CrossAADT1000 + PercNonWhite
rm(mod)

# Multilevel model with random intercept and macro-level-two IVs
# test specifications, backwards removal
mod04a <- update(mod02, formula= . ~ . + NIntLegs2 + CrossAADT1000 + PercNonWhite)
summary(mod04a)
mod04b <- update(mod04a, formula= . ~ . - PercNonWhite)
summary(mod04b)
anova(mod04b, mod04a) # okay to remove PercNonWhite
rm(mod04a, mod04b)
# final specification
mod04 <- update(mod02, formula= . ~ . + NIntLegs2 + CrossAADT1000)
summary(mod04)
anova(mod04, mod02) # better with new IVs

# Multilevel model with random intercept and both micro-level-one IVs and macro-level-two IVs
# test specifications, backwards removal
mod05a <- update(mod03, formula= . ~ . + NIntLegs2 + CrossAADT1000)
summary(mod05a)
mod05b <- update(mod05a, formula= . ~ . - CrossAADT1000)
summary(mod05b)
anova(mod05b, mod05a) # okay to remove CrossAADT1000
rm(mod05a, mod05b)
# final specification
mod05 <- update(mod03, formula= . ~ . + NIntLegs2)
summary(mod05)
anova(mod05, mod03) # better with new IVs
anova(mod05, mod04) # better with new IVs

# Multilevel model with random slope
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(GroupSize) | Crossing))
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(as.integer(AgeChild)) | Crossing))
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(as.integer(AgeTeen)) | Crossing))
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(as.integer(OtherMode)) | Crossing))
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(TimeWaitClean60) | Crossing))
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(as.integer(CrossOtherPeopleT)) | Crossing))
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(as.integer(CrossObs)) | Crossing))
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(as.integer(Weekend)) | Crossing))
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(as.integer(TOD0005)) | Crossing))
mod06 <- update(mod05, formula= . ~ . - (1 | Crossing) + (1 + I(PRCP) | Crossing))
summary(mod06)
anova(mod06, mod05)
# sig: GroupSize, TimeWaitClean60, CrossOtherPeopleT
rm(mod06)

# Summarize, combine, and save all models
summary(mod01)
summary(mod02)
summary(mod03)
summary(mod04)
summary(mod05)
modsTDV2SD <- list(datt2, mod01, mod02, mod03, mod04, mod05)
names(modsTDV2SD) <- c("datt2", "mod01", "mod02", "mod03", "mod04", "mod05")
saveRDS(modsTDV2SD, file=file.path("Analysis", "modsTDV2SD.rds"))

# Cleanup
rm(mod01, mod02, mod03, mod04, mod05)
rm(modsTDV2SD, datt2)
gc()

########################################
# Cleanup

# Remove
rm(dat, xdat)
gc()

########################################
# END
########################################