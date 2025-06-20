##################################################
# Project:  MPC-693 Transit peds
# Authors:  Atul Subedi (atul.subedi@usu.edu)
#           Patrick Singleton (patrick.singleton@usu.edu)
# File:     analyze_rightturn.R
# Date:     2024 Summer
# About:    Right-turn safety data
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
myfpath <- file.path("Data", "Data Collection 2")

# Load right-turn datasets
df_RT <- readRDS(file.path(myfpath, "df_RT.rds"))

# Load list of signals with near/far stops
studylocs <- read.csv(file.path(myfpath, "studylocs_RT.csv"))

##################################################
# Prepare outcomes for analysis

# Inspect IVs
# table(df_RT_FS$Signal)
# table(df_RT_NS$Signal)
table(df_RT$Signal)
studylocs

# Create new dataset with near-far information
# datF <- df_RT_FS
# datN <- df_RT_NS
datF <- df_RT[df_RT$Signal %in% studylocs$SIGNAL[studylocs$FAR==T],]
datN <- df_RT[df_RT$Signal %in% studylocs$SIGNAL[studylocs$NEAR==T],]
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
summary(dat$Crossing.location)
summary(dat$LocCross)
table(dat$Crossing.location, dat$LocCross)
dat$CROSS_LOC <- dat$LocCross
table(dat$CROSS_LOC, dat$LocCross)
# - Driver reaction
summary(dat$Reaction.to.conflict)
summary(dat$ReactDrv)
table(dat$Reaction.to.conflict, dat$ReactDrv)
dat$DRV_REACT <- dat$ReactDrv
levels(dat$DRV_REACT) <- c("None", "Other", "Other")
table(dat$DRV_REACT, dat$ReactDrv)
# - Pedestrian reaction
summary(dat$Ped.Reaction)
summary(dat$ReactPed)
table(dat$Ped.Reaction, dat$ReactPed)
dat$PED_REACT <- dat$ReactPed
levels(dat$PED_REACT) <- c("None", "Other", "Other")
table(dat$PED_REACT, dat$ReactPed)
# - Conflict severity
summary(abs(dat$Encroachment_Time))
summary(dat$ConflSev)
table(abs(dat$Encroachment_Time), dat$ConflSev)
dat$CONF_SEV <- dat$ConflSev
dat$CONF_SEV_H <- dat$CONF_SEV=="High"
dat$CONF_SEV_M <- dat$CONF_SEV=="Mild"
dat$CONF_SEV_L <- dat$CONF_SEV=="Low"
dat$CONF_SEV_ET10 <- abs(dat$Encroachment_Time)
dat$CONF_SEV_ET03 <- ifelse(dat$CONF_SEV_ET10>3, NA, dat$CONF_SEV_ET10)
table(abs(dat$Encroachment_Time), dat$ConflSev)
table(abs(dat$Encroachment_Time), dat$CONF_SEV_ET10)
table(abs(dat$Encroachment_Time), dat$CONF_SEV_ET03)

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
# a. Pedestrian characteristics
ivsa <- c("NEAR_FAR", "GroupSize", 
          "Age_Child", "Age_Teenager", "Age_Young_adult", "Age_Middle_aged_adult", "Age_Older", "Age_Adult_of_unknown_age", 
          "Gender_Male", "Gender_Female", "Gender_Unknown", 
          "OC_Load", "OC_Stroller", "OC_Wheelchair", "OC_Skateboard", "OC_Scooter", "OC_Bicycle", "OC_Distracted", 
          "CWNum1", "CWNum2", 
          "LocCross_Crosswalk", "LocCross_Midblock", "LocCross_Middle", 
          "CDirLeav", "CDirAppr", 
          "ReactPed_None", "ReactPed_Stop", "ReactPed_Slow", "ReactPed_Spedup", "ReactPed_Ran", "ReactPed_Cdir")
# b. Driver & vehicle characteristics
ivsb <- c("RTQueue", 
          "Loc_Stop_None", "Loc_Stop_Before", "Loc_Stop_Inside1", "Loc_Stop_Between", "Loc_Stop_Inside2", 
          "ReactDrv_None", "ReactDrv_Stop", "ReactDrv_Slow", "ReactDrv_Spedup", "ReactDrv_Swerve", 
          "VehType2_Small", "VehType2_Medium", "VehType2_Large")
# c. Conflict information
ivsc <- c("EncrTime", "ConflSev_Low", "ConflSev_Mild", "ConflSev_High")
# run
for (i in c(ivsa, ivsb, ivsc)) {
  ds <- rbind(ds, myds(i, dat[[i]]))
}; rm(i)
rm(ivsa, ivsb, ivsc)

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

# Pedestrian-right-turning vehicle conflicts
dvs <- c("CROSS_LOC", "DRV_REACT", "PED_REACT", 
         "CONF_SEV_H", "CONF_SEV_M", "CONF_SEV_L", "CONF_SEV_ET10")
for (i in 1:length(dvs)) {
  datstat <- rbind(datstat, mystats(dvs[i], "NEAR_FAR", dat))
}; rm(i)
rm(dvs)

# Filter for conflicts with 3 seconds or less
dat03 <- dat[dat$CONF_SEV_ET10<=3,]
dvs <- c("CROSS_LOC", "DRV_REACT", "PED_REACT", "CONF_SEV_ET03")
for (i in 1:length(dvs)) {
  datstat <- rbind(datstat, mystats(dvs[i], "NEAR_FAR", dat03))
}; rm(i)
rm(dvs)
rm(dat03)

# Inspect
datstat

##################################################
# Save

# Save results
# saveRDS(ds, file.path("Analysis", "Data Collection 2", "desc_stat_RT.rds"))
write.csv(ds, file.path("Analysis", "Data Collection 2", "desc_stat_RT.csv"), row.names=F)
# saveRDS(datstat, file.path("Analysis", "Data Collection 2", "bivariate_results_RT.rds"))
write.csv(datstat, file.path("Analysis", "Data Collection 2", "bivariate_results_RT.csv"), row.names=F)

# Remove
rm(mystats, datstat, myfpath)
# rm(dat, df_RT_FS, df_RT_NS, df_RT_TS)
rm(dat, df_RT, studylocs, ds, myds)
gc()

##################################################
# END
##################################################