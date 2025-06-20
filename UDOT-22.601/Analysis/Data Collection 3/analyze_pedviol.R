##################################################
# Project:  MPC-693 Transit peds
# Authors:  Atul Subedi (atul.subedi@usu.edu)
#           Patrick Singleton (patrick.singleton@usu.edu)
# File:     anaylze_pedviol.R
# Date:     2024 Summer
# About:    Pedestrian violations data
#           - Descriptive statistics
#           - Bivariate analysis
##################################################

# Load packages
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(chron)
# library(mlogit)
# library(readxl)
# library(psych)
library(fastDummies)

##################################################
# Load data

# Initialize file path
myfpath <- file.path("Data", "Data Collection 3")

# Load pedestrian violations dataset
dat04 <- readRDS(file.path(myfpath, "dat04.rds"))

# Load list of signals with near/far stops
studylocs <- read.csv(file.path(myfpath, "studylocs_PV.csv"))

##################################################
# Prepare outcomes for analysis

# Inspect IVs
table(dat04$Signal)
table(dat04$PedLeg)
studylocs

# Create new dataset with near-far information
# datF <- dat04[dat04$Signal %in% studylocs$SIGNAL[studylocs$FAR1==T],]  # 1452
# datN <- dat04[dat04$Signal %in% studylocs$SIGNAL[studylocs$NEAR1==T],] #  290
# datF <- dat04[dat04$Signal %in% studylocs$SIGNAL[studylocs$FAR2==T],]  # 2172
# datN <- dat04[dat04$Signal %in% studylocs$SIGNAL[studylocs$NEAR2==T],] #  598
datF <- dat04[paste(dat04$Signal, dat04$PedLeg) %in% paste(studylocs$SIGNAL[studylocs$FAR2==T], studylocs$CROSSWALK[studylocs$FAR2==T]),] # 2172
datN <- dat04[paste(dat04$Signal, dat04$PedLeg) %in% paste(studylocs$SIGNAL[studylocs$NEAR2==T], studylocs$CROSSWALK[studylocs$NEAR2==T]),] # 439
table(datF$Signal)
table(datN$Signal)
datF$NEAR_FAR <- "FarSide"
datN$NEAR_FAR <- "NearSide"
dat <- rbind(datF, datN)
rm(datF, datN)
dat$NEAR_FAR <- factor(dat$NEAR_FAR)
summary(dat$NEAR_FAR)
table(dat$Signal, dat$NEAR_FAR)

# Inspect and adjust DVs
# - Crossing location
summary(dat$CrossLoc)
dat$CROSS_LOC <- dat$CrossLoc
levels(dat$CROSS_LOC) <- c("Crosswalk", "Away", "Away", NA)
table(dat$CrossLoc, dat$CROSS_LOC)
# - Crossing behaviors
summary(dat[,c("CrossMarkInAll", "CrossMarkInOut", "CrossMarkOutAll", "CrossMarkOther")])
summary(dat[,c("CrossBehSpeed", "CrossBehPaused", "CrossBehDistracted", "CrossBehOther")])
summary(dat[,c("CrossObsCar", "CrossObsSWD", "CrossObsOther")])

##################################################
# Prepare for descriptive statistics

# Function to calculate descriptive statistics
myds <- function(myname, myvar) {
  if (class(myvar) %in% c("integer", "numeric")) {
    td <- list(myname, length(na.omit(myvar)), mean(myvar, na.rm=T), sd(myvar, na.rm=T), NA, NA)
    td <- as.data.frame(td)
    names(td) <- c("Var", "N", "Mean", "SD", "Freq", "Perc")
  } else if (class(myvar) %in% c("logical")) {
    td <- list(paste(myname, "TRUE", sep="_"), length(na.omit(myvar)), NA, NA, sum(myvar==T, na.rm=T), 100 * sum(myvar==T, na.rm=T) / length(na.omit(myvar)))
    td <- as.data.frame(td)
    names(td) <- c("Var", "N", "Mean", "SD", "Freq", "Perc")
  } else if (class(myvar) %in% c("factor")) {
    tvar <- data.frame(myvar); names(tvar) <- myname
    temp <- dummy_cols(tvar, ignore_na=T, remove_selected_columns=T)
    td <- list(names(temp)[1], length(na.omit(myvar)), NA, NA, sum(temp[,1], na.rm=T), 100 * sum(temp[,1], na.rm=T) / length(na.omit(myvar)))
    td <- as.data.frame(td)
    names(td) <- c("Var", "N", "Mean", "SD", "Freq", "Perc")
    for (j in 2:ncol(temp)) {
      xtd <- list(names(temp)[j], length(na.omit(myvar)), NA, NA, sum(temp[,j], na.rm=T), 100 * sum(temp[,j], na.rm=T) / length(na.omit(myvar)))
      xtd <- as.data.frame(xtd)
      names(xtd) <- c("Var", "N", "Mean", "SD", "Freq", "Perc")
      td <- rbind(td, xtd)
      rm(xtd)
    }; rm(j)
    rm(tvar, temp)
  } else {
    td <- list(myname, NA, NA, NA, NA, NA)
    td <- as.data.frame(td)
    names(td) <- c("Var", "N", "Mean", "SD", "Freq", "Perc")
  }
  row.names(td) <- NULL
  return(td)
}

##################################################
# Calculate descriptive stats

# Initialize
ds <- data.frame(Var=character(), N=integer(), Mean=numeric(), SD=numeric(), Freq=integer(), Perc=numeric())

# Select variables
ivs <- c("NEAR_FAR", "GroupSize", 
         "AgeChild", "AgeTeen", "AgeAdultYoung", "AgeAdultMiddle", "AgeAdultOlder", "AgeAdultUnknown", 
         "GenderMale", "GenderFemale", "GenderUnknown", 
         "OtherStroller", "OtherLoad", "OtherWheelchair", "OtherSkateboard", "OtherScooter", "OtherBicycle", "OtherOther", 
         "WaitOtherPeople", "VehiclesPast10", "VehiclesNext10", 
         "WaitBehPressed", "WaitBehPaced", "WaitBehLeft", "WaitBehOther", "TimeWaitClean", 
         "CrossLoc", "CrossDir", "CrossOtherPeopleSame", "CrossOtherPeopleOppo", 
         "CrossMarkInAll", "CrossMarkInOut", "CrossMarkOutAll", "CrossMarkOther", 
         "CrossBehSpeed", "CrossBehPaused", "CrossBehDistracted", "CrossBehOther", 
         "CrossObsCar", "CrossObsSWD", "CrossObsOther", "TimeCurbClean")
# run
for (i in c(ivs)) {
  ds <- rbind(ds, myds(i, dat[[i]]))
}; rm(i)
rm(ivs)

# Inspect
ds

##################################################
# Prepare for bivariate analyses

# Function to get information from bivariate statistical tests
mystats <- function(dv, iv, mydat) {
  if (class(mydat[,dv]) %in% c("factor", "logical")) {
    mytest <- fisher.test(mydat[,dv], mydat[,iv])
    mytab <- table(mydat[,dv], mydat[,iv])
    myptab <- prop.table(mytab, 2)
    mylist <- list(dv, iv, "Fisher", 
                   mytest$estimate, NA, mytest$p.value, 
                   colSums(mytab)[1], colSums(mytab)[2], 
                   row.names(mytab)[1], myptab[1,1], myptab[1,2], 
                   row.names(mytab)[2], myptab[2,1], myptab[2,2])
    rm(mytest, mytab, myptab)
  } else if (class(mydat[,dv]) %in% c("numeric", "integer")) {
    mytest <- t.test(formula(paste(dv, "~", iv)), data=mydat)
    mytab <- table(is.na(mydat[,dv]), mydat[,iv])
    mylist <- list(dv, iv, "t", 
                   mytest$statistic, mytest$parameter, mytest$p.value, 
                   mytab[1,1], mytab[1,2],
                   NA, NA, NA, 
                   NA, mytest$estimate[1], mytest$estimate[2])
  } else {
    mylist <- list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  }
  names(mylist) <- c("DV", "IV", "Test", "Stat", "Param", "pval", "NF", "NN", 
                     "Level1", "Stat1F", "Stat1N", "Level2", "Stat2F", "Stat2N")
  return(mylist)
}

##################################################
# Conduct bivariate analyses

# Initialize dataframe
datstat <- data.frame(DV=character(), IV=character(), Test=character(), 
                      Stat=numeric(), Param=numeric(), pval=numeric(), 
                      NF=integer(), NN=integer(0), 
                      Level1=character(), Stat1F=numeric(), Stat1N=numeric(), 
                      Level2=character(), Stat2F=numeric(), Stat2N=numeric())

# Pedestrian crossings
dvs <- c("CROSS_LOC", "CrossMarkInAll", "CrossMarkInOut", "CrossMarkOutAll", 
         "CrossBehSpeed", "CrossBehPaused", "CrossBehDistracted", "CrossObsCar")
for (i in 1:length(dvs)) {
  datstat <- rbind(datstat, mystats(dvs[i], "NEAR_FAR", dat))
}; rm(i)
rm(dvs)

# Inspect
datstat

##################################################
# Save

# Save results
# saveRDS(ds, file.path("Analysis", "Data Collection 3", "desc_stat_PV.rds"))
write.csv(ds, file.path("Analysis", "Data Collection 3", "desc_stat_PV.csv"), row.names=F)
# saveRDS(datstat, file.path("Analysis", "Data Collection 3", "bivariate_results_PV.rds"))
write.csv(datstat, file.path("Analysis", "Data Collection 3", "bivariate_results_PV.csv"), row.names=F)

# Remove
rm(mystats, datstat, myfpath)
rm(dat, dat04, studylocs, ds, myds)
gc()

##################################################
# END
##################################################