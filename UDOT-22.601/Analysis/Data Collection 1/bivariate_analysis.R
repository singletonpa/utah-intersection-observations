##################################################
# Project:  MPC-693 Transit peds
# Authors:  Aleks Paskett (aleks.paskett@usu.edu)
#           Fariba Soltani (fariba.soltani@usu.edu)
#           Patrick Singleton (patrick.singleton@usu.edu)
# File:     bivariate_analysis.R
# Date:     2025 Spring
# About:    Perform bivariate analyses
##################################################

##################################################
# Notes

# 2024-06-28 created PS
# 2024-07-01 updated PS
# 2025-03-05 updated AP
# 2025-04-14 updated PS

# Load packages
# library()

##################################################
# Load data

# Initialize file path
myfpath <- file.path("Data", "Data Collection 1")

# Load datwide
datwide <- readRDS(file.path(myfpath, "datwide.rds"))
datwide <- datwide[datwide$NEAR_FAR != "Median", ]
datwide$NEAR_FAR <- droplevels(datwide$NEAR_FAR)

# Load datlong
datlong <- readRDS(file.path(myfpath, "datlong.rds"))
datlong <- datlong[datlong$NEAR_FAR != "Median", ]
datlong$NEAR_FAR <- droplevels(datlong$NEAR_FAR)

##################################################
# Prepare outcomes for analysis

# Inspect IVs
summary(datwide$NEAR_FAR)
summary(datlong$NEAR_FAR)

# Traffic operations
summary(datwide[,c("STOP_E1", "STOP_E2", "STOP_E3", "STOP_E4", "STOP_E5", "STOP_E6")])
summary(datwide[,c("STOP_E1_N_VEH", "STOP_E2_N_VEH", "STOP_E3_N_VEH", "STOP_E4_N_VEH")])

# Pedestrian crossing behaviors
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

# Pedestrian-vehicle conflicts
# - Conflict severity
datlong$CONF_SEV <- abs(as.numeric(difftime(datlong$TP_CONF, datlong$TV_CONF, units="sec")))
summary(datlong$CONF_SEV); table(datlong$CONF_SEV) # remove longer than 3 sec (limit in instructions)
datlong$CONF_SEV[!is.na(datlong$CONF_SEV) & datlong$CONF_SEV>3] <- NA
summary(datlong$CONF_SEV)
# - Driver reaction
summary(datlong$VEH_REACT)
levels(datlong$VEH_REACT)
datlong$VEH_REACT3 <- datlong$VEH_REACT
levels(datlong$VEH_REACT3) <- c("None", "StopSlow", "StopSlow", "Other", "Other", NA)
datlong$VEH_REACT2 <- datlong$VEH_REACT
levels(datlong$VEH_REACT2) <- c("None", "Other", "Other", "Other", "Other", NA)
summary(datlong[,c("VEH_REACT3", "VEH_REACT2")])
# - Pedestrian reaction
summary(datlong$PED_REACT)
levels(datlong$PED_REACT)
datlong$PED_REACT3 <- datlong$PED_REACT
levels(datlong$PED_REACT3) <- c("None", "StopSlow", "StopSlow", "Other", "Other", NA)
datlong$PED_REACT2 <- datlong$PED_REACT
levels(datlong$PED_REACT2) <- c("None", "Other", "Other", "Other", "Other", NA)
summary(datlong[,c("PED_REACT3", "PED_REACT2")])

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

# Traffic operations
dvs <- c("STOP_E1", "STOP_E2", "STOP_E3", "STOP_E4", "STOP_E5", "STOP_E6", 
         "STOP_E1_N_VEH", "STOP_E2_N_VEH", "STOP_E3_N_VEH", "STOP_E4_N_VEH")
for (i in 1:length(dvs)) {
  datstat <- rbind(datstat, mystats(dvs[i], "NEAR_FAR", datwide))
}; rm(i)
rm(dvs)

# Pedestrian crossing behaviors
dvs <- c("PER_CROSS", 
         "PER_BEH_OUTSIDE", "PER_BEH_CHNGSPD", "PER_BEH_PAUSMID", 
         "PER_BEH_DISTRAC", "PER_BEH_CRFRONT", "PER_BEH_CRBEHIN")
for (i in 1:length(dvs)) {
  datstat <- rbind(datstat, mystats(dvs[i], "NEAR_FAR", datlong))
}; rm(i)
rm(dvs)

# Pedestrian-vehicle conflicts
dvs <- c("CONF_SEV", "VEH_REACT2", "PED_REACT2")
for (i in 1:length(dvs)) {
  datstat <- rbind(datstat, mystats(dvs[i], "NEAR_FAR", datlong))
}; rm(i)
rm(dvs)

# Inspect
datstat

##################################################
# Save

# Save results
# saveRDS(datstat, file.path("Analysis", "Data Collection 1", "bivariate_results.rds"))
write.csv(datstat, file.path("Analysis", "Data Collection 1", "bivariate_results.csv"), row.names=F)

# Remove
rm(datwide, datlong, myfpath)
rm(mystats, datstat)
gc()

##################################################
# END
##################################################