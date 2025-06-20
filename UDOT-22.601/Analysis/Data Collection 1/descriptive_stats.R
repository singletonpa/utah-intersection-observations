##################################################
# Project:  MPC-693 Transit peds
# Authors:  Aleks Paskett (aleks.paskett@usu.edu)
#           Patrick Singleton (patrick.singleton@usu.edu)
# File:     descriptive_stats.R
# Date:     2025 Spring
# About:    Calculate descriptive statistics
##################################################

##################################################
# Notes

# 2024-06-26 created FS
# 2024-06-28 updated FS
# 2024-07-01 updated PS
# 2025-03-05 updated AP
# 2025-04-14 updated PS

# Load packages
library(readxl)
library(fastDummies)

##################################################
# Load data

# Initialize file path
myfpath <- file.path("Data", "Data Collection 1")

# Load columns
# columns <- read.csv(file.path(myfpath, "columns.csv")) 
columns <- read_excel(file.path(myfpath, "columns.xlsx")) 

# Load datwide
datwide <- readRDS(file.path(myfpath, "datwide.rds"))
datwide <- datwide[datwide$NEAR_FAR != "Median", ]
datwide$NEAR_FAR <- droplevels(datwide$NEAR_FAR)

# Load datlong
datlong <- readRDS(file.path(myfpath, "datlong.rds"))
datlong <- datlong[datlong$NEAR_FAR != "Median", ]
datlong$NEAR_FAR <- droplevels(datlong$NEAR_FAR)

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
# Descriptive statistics for datwide

# Create new time variables
# - bus arrival and departure "T_ARR" "T_DEP" (Dwell time)
datwide$DwellTime <- as.numeric(difftime(datwide$T_DEP, datwide$T_ARR, units="secs"))
summary(datwide$DwellTime)

# Initialize
dswide <- data.frame(Var=character(), N=integer(), Mean=numeric(), SD=numeric(), Freq=integer(), Perc=numeric())

# All wide
variables <- c("NEAR_FAR","N_BOARD", "N_ALIGHT", "STOP_E1", "STOP_E2", "STOP_E3", "STOP_E4", "STOP_E5", "STOP_E6", 
               "STOP_E1_N_VEH", "STOP_E3_N_VEH", "STOP_E4_N_VEH", "STOP_E2_N_VEH", "DwellTime")
for (i in variables) {
  dswide <- rbind(dswide, myds(i, datwide[[i]]))
}; rm(i)
rm(variables)

# Inspect
dswide

##################################################
# Descriptive statistics for datlong

# Create new other variables
# - Crossing location
summary(datlong[,c("PER_CROSS_A", "PER_CROSS_B")])
levels(datlong$PER_CROSS_A); levels(datlong$PER_CROSS_B)
datlong$PER_CROSS_A2 <- datlong$PER_CROSS_A
datlong$PER_CROSS_B2 <- datlong$PER_CROSS_B
levels(datlong$PER_CROSS_A2)<- c("Intersection", "Mid-block", NA, NA, NA, NA, NA, NA)
levels(datlong$PER_CROSS_B2)<- c("Intersection", "Mid-block", NA, NA, NA, NA, NA, NA)
summary(datlong[,c("PER_CROSS_A2", "PER_CROSS_B2")])
summary(datlong$PER_CROSS_A)
summary(datlong$PER_CROSS_B)
datlong$PER_CROSS <- ifelse(!is.na(datlong$PER_CROSS_A2), as.character(datlong$PER_CROSS_A2), 
                            ifelse(!is.na(datlong$PER_CROSS_B2), as.character(datlong$PER_CROSS_B2), NA))
datlong$PER_CROSS <- as.factor(datlong$PER_CROSS)
summary(datlong$PER_CROSS)
datlong[,c("PER_CROSS_A2", "PER_CROSS_B2")] <- NULL
# - Crossing behaviors
summary(datlong[,c("PER_BEH_OUTSIDE", "PER_BEH_CHNGSPD", "PER_BEH_PAUSMID", "PER_BEH_DISTRAC", "PER_BEH_CRFRONT", "PER_BEH_CRBEHIN")])
for (i in c("PER_BEH_OUTSIDE", "PER_BEH_CHNGSPD", "PER_BEH_PAUSMID", "PER_BEH_DISTRAC", "PER_BEH_CRFRONT", "PER_BEH_CRBEHIN")) {
  datlong[,i] <- ifelse(!is.na(datlong$PER_CROSS), datlong[,i], NA)
}; rm(i)
summary(datlong[,c("PER_BEH_OUTSIDE", "PER_BEH_CHNGSPD", "PER_BEH_PAUSMID", "PER_BEH_DISTRAC", "PER_BEH_CRFRONT", "PER_BEH_CRBEHIN")])
# - Conflict severity
datlong$CONF_SEV <- abs(as.numeric(difftime(datlong$TP_CONF, datlong$TV_CONF, units="sec")))
summary(datlong$CONF_SEV); table(datlong$CONF_SEV) # remove longer than 3 sec (limit in instructions)
datlong$CONF_SEV[!is.na(datlong$CONF_SEV) & datlong$CONF_SEV>3] <- NA
summary(datlong$CONF_SEV)

### Create new time variables
# A1: Transit vehicle arrives -> Passenger alights (TP_ALIGHT - T_ARR)
datlong$TimeA1 <- as.numeric(difftime(datlong$TP_ALIGHT, datlong$T_ARR, units="secs"))
summary(datlong$TimeA1)
# A2: Passenger alights -> Transit vehicle departs (T_DEP - TP_ALIGHT)
datlong$TimeA2 <- as.numeric(difftime(datlong$T_DEP, datlong$TP_ALIGHT, units="secs"))
summary(datlong$TimeA2)
# A3: Passenger alights -> Passenger leaves stop (TP_DEP - TP_ALIGHT)
datlong$TimeA3 <- as.numeric(difftime(datlong$TP_DEP, datlong$TP_ALIGHT, units="secs"))
summary(datlong$TimeA3)
# A4: Passenger leaves stop -> Person starts crossing (TP_CROSS1 - TP_DEP)
datlong$TimeA4 <- as.numeric(difftime(datlong$TP_CROSS1, datlong$TP_DEP, units="secs"))
summary(datlong$TimeA4)
# B1: Person finishes crossing -> Passenger arrives stop (TP_ARR - TP_CROSS2)
datlong$TimeB1 <- as.numeric(difftime(datlong$TP_ARR, datlong$TP_CROSS2, units="secs"))
summary(datlong$TimeB1)
# B2: Passenger arrives stop -> Passenger boards (TP_BOARD - TP_ARR)
datlong$TimeB2 <- as.numeric(difftime(datlong$TP_BOARD, datlong$TP_ARR, units="secs"))
summary(datlong$TimeB2)
# B3: Transit vehicle arrives -> Passenger boards (TP_BOARD - T_ARR)
datlong$TimeB3 <- as.numeric(difftime(datlong$TP_BOARD, datlong$T_ARR, units="secs"))
summary(datlong$TimeB3)
# B4: Passenger boards -> Transit vehicle departs (T_DEP - TP_BOARD)
datlong$TimeB4 <- as.numeric(difftime(datlong$T_DEP, datlong$TP_BOARD, units="secs"))
summary(datlong$TimeB4)
# C1: Person starts crossing -> Person finishes crossing (TP_CROSS2 - TP_CROSS1)
datlong$TimeC1 <- as.numeric(difftime(datlong$TP_CROSS2, datlong$TP_CROSS1, units="secs"))
summary(datlong$TimeC1)
# C2: Person starts crossing -> Pedestrian at conflict point (TP_CONF - TP_CROSS1)
datlong$TimeC2 <- as.numeric(difftime(datlong$TP_CONF, datlong$TP_CROSS1, units="secs"))
summary(datlong$TimeC2)
# C3: Pedestrian at conflict point -> Person finishes crossing (TP_CROSS2 - TP_CONF)
datlong$TimeC3 <- as.numeric(difftime(datlong$TP_CROSS2, datlong$TP_CONF, units="secs"))
summary(datlong$TimeC3)
# C4: Pedestrian at conflict point <-> Motor vehicle at conflict point (TV_CONF - TP_CONF)
datlong$TimeC4 <- as.numeric(difftime(datlong$TV_CONF, datlong$TP_CONF, units="secs"))
summary(datlong$TimeC4)

# Initialize
dslong <- data.frame(Var=character(), N=integer(), Mean=numeric(), SD=numeric(), Freq=integer(), Perc=numeric())

# All wide
variables <- c("NEAR_FAR", "ALIGHT_BOARD", "AGE", "GENDER",  
               "PER_OTHER_PERSON", "PER_OTHER_CARRYLOAD", "PER_OTHER_STROLLER", "PER_OTHER_WHEELCHAIR", 
               "PER_OTHER_SKATEBOARD", "PER_OTHER_SCOOTER", "PER_OTHER_BICYCLE", "PER_OTHER_DISTRACTED", "PER_OTHER_OTHER", 
               "PER_CROSS_A", "PER_CROSS_B", "PER_STREET", 
               "PER_BEH_OUTSIDE", "PER_BEH_CHNGSPD", "PER_BEH_PAUSMID", "PER_BEH_DISTRAC", "PER_BEH_CRFRONT", "PER_BEH_CRBEHIN", 
               "CONF_LOC", "VEH_MOVE", "VEH_REACT", "PED_REACT", "CONF_SEV", 
               "TimeA1", "TimeA2", "TimeA3", "TimeA4", "TimeB1", "TimeB2", "TimeB3", "TimeB4", "TimeC1", "TimeC2", "TimeC3", "TimeC4")
for (i in variables) {
  dslong <- rbind(dslong, myds(i, datlong[[i]]))
}; rm(i)
rm(variables)

# Inspect
dslong

##################################################
# Save

# Save results
# saveRDS(dswide, file.path("Analysis","Data Collection 1", "dswide.rds"))
write.csv(dswide, file.path("Analysis","Data Collection 1", "dswide.csv"), row.names=F)
# saveRDS(dslong, file.path("Analysis","Data Collection 1", "dslong.rds"))
write.csv(dslong, file.path("Analysis","Data Collection 1", "dslong.csv"), row.names=F)

# Remove
rm(datwide, datlong, myfpath, columns)
rm(myds, dswide, dslong)
gc()

##################################################
# END
##################################################