#######################################
# Project:  UDOT-19.312 Right turn safety
# Authors:  Alyssa Gaither (alyssa.gaither@usu.edu)
#           Patrick Singleton (patrick.singleton@usu.edu)
# File:     table_desc_corr.R
# Date:     2023 Spring
# About:    Creating tables for descriptive statistics and bivariate analysis
########################################

# Notes
# created from Correlations Table.R
# 2023-02-13 AG updated 
# 2023-03-13 PS updated
# 2023-03-15 PS updated, moved code to add_vars.R
# 2023-04-15 PS added variables: weather, time, log(exposure)

#####################################################
# Load data

# Load data
dat <- readRDS(file.path("Analysis", "Conflict Analysis", "Combine All Data", "datlongcombo.rds"))

# Backup
xdat <- dat
# dat <- xdat

# Inspect
names(dat)

# Filter data
dat <- dat[abs(dat$Encroachment_Time)<=10,]
dat <- dat[!is.na(dat$Encroachment_Time),]

########################################
# Process variables

# Run script to process variables
source(file.path("Analysis", "Conflict Analysis", "Bivariate Analysis", "add_vars.R"))

# Inspect
names(dat)

########################################
# Select level 1 variables

# a. Pedestrian characteristics
ivsa <- c("GroupSize", "logGroupSize", 
          "Age_ChildTeen", "Age_Child", "Age_Teenager", "Age_Adult", "Age_Young_adult", "Age_Middle_aged_adult", "Age_Older", "Age_Adult_of_unknown_age", 
          "Gender_Male", "Gender_Female", "Gender_Unknown", 
          "OC_Load", "OC_StrWhe", "OC_Stroller", "OC_Wheelchair", "OC_SkaSco", "OC_Skateboard", "OC_Scooter", "OC_Bicycle", "OC_Distracted", 
          "CWNum1", "CWNum2", 
          "LocCross_Crosswalk", "LocCross_Away", "LocCross_Midblock", "LocCross_Middle", 
          "CDirLeav", "CDirAppr", 
          "ReactPed_None", "ReactPed_StopSlow", "ReactPed_Stop", "ReactPed_Slow", "ReactPed_Other", "ReactPed_Spedup", "ReactPed_Ran", "ReactPed_Cdir")
str(dat[,ivsa])

# b. Driver & vehicle characteristics
ivsb <- c("RTQueue", 
          "Loc_Stop_None", "Loc_Stop_Before", "Loc_Stop_InBtwn", "Loc_Stop_Inside1", "Loc_Stop_Between", "Loc_Stop_Inside2", 
          "ReactDrv_None", "ReactDrv_StopSlow", "ReactDrv_Stop", "ReactDrv_Slow", "ReactDrv_Other", "ReactDrv_Spedup", "ReactDrv_Swerve", 
          "VehType2_Small", "VehType1_Sedan", "VehType1_Mcycle", "VehType2_Medium", "VehType1_SUV", "VehType1_Truck", "VehType1_Van", 
          "VehType2_Large", "VehType0_Largetruck", "VehType0_Trailer", "VehType0_Bus")
str(dat[,ivsb])

# c. Conflict information
ivsc <- c("EncrTime", "PreEncrTime", "PostEncrTime", 
          "ConflSev_Low", "ConflSev_Mild", "ConflSev_High")
str(dat[,ivsc])

# d. Weather and time information
ivsd <- c("WeathCle", "WeathRai", "WeathSno", 
          "PRCP", "PRCP_01", 
          "TEMP", "TEMP_2050", "TEMP_5065", "TEMP_6580", "TEMP_8095", # "TEMPL32", "TEMP80P", 
          "Weekday_M_F", "Weekday_TWH", "Weekend", 
          "TOD2_0611", "TOD2_1217", "TOD2_1805", "AMPeak", "PMPeak")
str(dat[,ivsd])

# e. Traffic signal statuses
ivse <- c("PEDCP_ped_Walk", "PEDCP_ped_FDW", "PEDCP_ped_SDW", "RTNotSig", 
          "VEHCP_veh_G", "VEHCP_veh_Y", "VEHCP_veh_R", "RTNotSig")
str(dat[,ivse])

########################################
# Select level 2 variables

# f. Corner and intersection attributes - descriptives
ivsf_d <- c("CURB_RAD", 
            "CW_DIST", "CHANNEL", 
            "SB_DIST", "CHANNEL", 
            "RAMPS", "RAMPS_1", "RAMPS_2", 
            "CRType_Diag", "CRType_Dire", "CRType_Blen", 
            "CWType1_Stan", "CWType1_Cont", "CWType1_None", 
            "CWType2_Stan", "CWType2_Cont", "CWType2_None", 
            "RT_LNS", "RT_LNS_05", "RT_LNS_1", "RT_LNS_2", 
            "REC_LNS", "REC_LNS_0", "REC_LNS_1", 
            "CHANNEL", 
            "Skewed", 
            "EXTENSION",
            "RTOR",
            "Street.Lt", 
            "BIKE_LN", 
            # "Bike_None", "Bike_Right", "Bike_Other", "Bike_Other_Left", "Bike_Other_Other", 
            "AADP0100", "logAADP", 
            "AADT1000", "logAADT", 
            # Highway", "MajorRoad", "LocalRoad", 
            "RTNotSig", 
            "OnRamp", "OffRamp")
str(dat[,ivsf_d])

# f. Corner and intersection attributes - correlation
ivsf_c <- c("CURB_RAD", 
            "CW_DIST", "CHANNEL", 
            "SB_DIST", "CHANNEL", 
            "RAMPS", "RAMPS_1", "RAMPS_2", 
            "CRType_Diag", "CRType_Dire", "CRType_Blen", 
            "CWType_Stan", "CWType_Cont", "CWType_None", 
            "RT_LNS", "RT_LNS_05", "RT_LNS_1", "RT_LNS_2", 
            "REC_LNS", "REC_LNS_0", "REC_LNS_1", 
            "CHANNEL", 
            "Skewed", 
            "EXTENSION",
            "RTOR",
            "Street.Lt", 
            "BIKE_LN", 
            # "Bike_None", "Bike_Right", "Bike_Other", "Bike_Other_Left", "Bike_Other_Other", 
            "AADP0100", "logAADP", 
            "AADT1000", "logAADT", 
            # "Highway", "MajorRoad", "LocalRoad", 
            "RTNotSig", 
            "OnRamp", "OffRamp")
str(dat[,ivsf_c])

# g. Neighborhood attributes
ivsg <- c("popden_000_qtmi", "empden_000_qtmi", 
          "per_res_qtmi", "per_com_qtmi", "per_ind_qtmi", "per_vac_qtmi", "per_other_qtmi", 
          "intden_qtmi", "per4wy_qtmi", "stops_qtmi", 
          "worship_qtmi", "schools_qtmi", "park_acre_qtmi", 
          "income_000_qtmi", "avgveh_qtmi", "hhsize_qtmi")
str(dat[,ivsg])

########################################
# Calculate descriptive statistics

# Create level-2 variable dataset
dat2 <- dat[,c("L2ID", ivsf_d, ivsg)]
dat2 <- dat2[!duplicated(dat2),]
str(dat2[,ivsf_d])
str(dat2[,ivsg])

# Initialize table of descriptive statistics
mydf <- data.frame(Variable=character(), Num=integer(), Perc=numeric(), Mean=numeric(), SD=numeric(), N=integer())

# Create desc stats for Level-1 variables
# initialize
mydf1 <- mydf
myvs1 <- c(ivsa, ivsb, ivsc, ivsd, ivse)
# calc desc stats
for (i in myvs1) {
  Variable <- i
  if (class(dat[,i]) %in% c("numeric", "integer")) {
    Num <- NA
    Perc <- NA
    Mean <- mean(dat[,i], na.rm=T)
    SD <- sd(dat[,i], na.rm=T)
    N <- sum(!is.na(dat[,i])==T, na.rm=T)
  } else if (class(dat[,i]) %in% c("logical")) {
    Num <- sum(dat[,i]==T, na.rm=T)
    Perc <- (sum(dat[,i]==T, na.rm=T) / sum(dat[,i] %in% c(T,F), na.rm=T)) * 100
    Mean <- NA
    SD <- NA
    N <- sum(dat[,i] %in% c(T,F), na.rm=T)
  } else {
    Num <- NA
    Perc <- NA
    Mean <- NA
    SD <- NA
    N <- NA
  }
  res <- data.frame(Variable, Num, Perc, Mean, SD, N)
  mydf1 <- rbind(mydf1, res)
  rm(Variable, Num, Perc, Mean, SD, N, res)
}; rm(i)
# inspect
mydf1

# Create desc stats for Level-2 variables
# initialize
mydf2 <- mydf
myvs2 <- c(ivsf_d, ivsg)
# calc desc stats
for (i in myvs2) {
  Variable <- i
  if (class(dat2[,i]) %in% c("numeric", "integer")) {
    Num <- NA
    Perc <- NA
    Mean <- mean(dat2[,i], na.rm=T)
    SD <- sd(dat2[,i], na.rm=T)
    N <- sum(!is.na(dat2[,i])==T, na.rm=T)
  } else if (class(dat2[,i]) %in% c("logical")) {
    Num <- sum(dat2[,i]==T, na.rm=T)
    Perc <- (sum(dat2[,i]==T, na.rm=T) / sum(dat2[,i] %in% c(T,F), na.rm=T)) * 100
    Mean <- NA
    SD <- NA
    N <- sum(dat2[,i] %in% c(T,F), na.rm=T)
  } else {
    Num <- NA
    Perc <- NA
    Mean <- NA
    SD <- NA
    N <- NA
  }
  res <- data.frame(Variable, Num, Perc, Mean, SD, N)
  mydf2 <- rbind(mydf2, res)
  rm(Variable, Num, Perc, Mean, SD, N, res)
}; rm(i)
# inspect
mydf2

########################################
# Prepare for correlations

# Combine independent variables
myivs <- c(ivsa, ivsb, ivsc, ivsd, ivse, ivsf_c, ivsg)

# Initialize results table
res <- data.frame(DV=character(), IV=character(), test=character(), 
                  cor=numeric(), cil=numeric(), ciu=numeric(), 
                  t=numeric(), df=integer(), p=numeric())

# Function to get correlation test results
mycorrres <- function(dat, dvs, ivs, res) {
  # Perform correlation tests
  for (y in dvs) {
    # print(paste0("DV: ", y))
    # get type of DV
    if (class(dat[,y]) %in% c("numeric", "integer")) {
      dvtype <- "continuous"
    } else if (class(dat[,y]) %in% c("logical")) {
      dvtype <- "dichotomous"
    }
    # for each IV
    for (x in ivs) {
      # print(x)
      # initialize row
      info <- res[0,]
      # get type of IV
      if (class(dat[,x]) %in% c("numeric", "integer")) {
        ivtype <- "continuous"
      } else if (class(dat[,x]) %in% c("logical")) {
        ivtype <- "dichotomous"
      }
      # get type of correlation test
      if (dvtype=="continuous" & ivtype=="continuous") {
        testtype <- "Pearson"
      } else if (dvtype=="continuous" & ivtype=="dichotomous") {
        testtype <- "Point-Biserial"
      } else if (dvtype=="dichotomous" & ivtype=="continuous") {
        testtype <- "Point-Biserial"
      } else if (dvtype=="dichotomous" & ivtype=="dichotomous") {
        testtype <- "Phi"
      }
      # perform correlation test
      mytest <- cor.test(as.numeric(dat[,y]), as.numeric(dat[,x]), correct=F)
      # assign to empty row
      info[1,c("DV","IV","test")] <- list(y, x, testtype)
      info[1,c("cor")] <- list(mytest$estimate)
      info[1,c("cil","ciu")] <- list(mytest$conf.int[1], mytest$conf.int[2])
      info[1,c("t","df","p")] <- list(mytest$statistic, mytest$parameter, mytest$p.value)
      # row bind to results
      res <- rbind(res, info)
      # remove
      rm(info, ivtype, testtype, mytest)
    }; rm(x)
    rm(dvtype)
  }; rm(y)
  # Return
  return(res)
}

########################################
# 1. Encroachment time

# Select dependent variables
dv1 <- c("EncrTime", "PreEncrTime", "PostEncrTime")

# Select independent variables
iv1 <- myivs[-which(myivs %in% dv1)]

# Calculate correlations
res1 <- mycorrres(dat, dv1, iv1, res)

# Inspect
View(res1)

########################################
# 2. Conflict severity

# Select dependent variables
dv2 <- c("ConflSev_Low", "ConflSev_Mild", "ConflSev_High")

# Select independent variables
iv2 <- myivs[-which(myivs %in% dv2)]

# Calculate correlations
res2 <- mycorrres(dat, dv2, iv2, res)

# Inspect
View(res2)

########################################
# 3. Pedestrian reaction

# Select dependent variables
dv3 <- c("ReactPed_None", "ReactPed_StopSlow", "ReactPed_Other")

# Select independent variables
iv3 <- myivs[-which(myivs %in% dv3)]

# Calculate correlations
res3 <- mycorrres(dat, dv3, iv3, res)

# Inspect
View(res3)

########################################
# 4. Pedestrian crossing location

# Select dependent variables
dv4 <- c("LocCross_Crosswalk", "LocCross_Away")

# Select independent variables
iv4 <- myivs[-which(myivs %in% dv4)]

# Calculate correlations
res4 <- mycorrres(dat, dv4, iv4, res)

# Inspect
View(res4)

########################################
# 5. Vehicle driver reaction

# Select dependent variables
dv5 <- c("ReactDrv_None", "ReactDrv_StopSlow", "ReactDrv_Other")

# Select independent variables
iv5 <- myivs[-which(myivs %in% dv5)]

# Calculate correlations
res5 <- mycorrres(dat, dv5, iv5, res)

# Inspect
View(res5)

########################################
# 6. Vehicle driver stopping location

# Select dependent variables
dv6 <- c("Loc_Stop_None", "Loc_Stop_Before", "Loc_Stop_InBtwn")

# Select independent variables
iv6 <- myivs[-which(myivs %in% dv6)]

# Calculate correlations
res6 <- mycorrres(dat, dv6, iv6, res)

# Inspect
View(res6)

########################################
# Save results

# Save
write.csv(mydf1, file.path("Analysis", "Conflict Analysis", "table_desc_L1.csv"), row.names=F)
write.csv(mydf2, file.path("Analysis", "Conflict Analysis", "table_desc_L2.csv"), row.names=F)
write.csv(res1, file.path("Analysis", "Conflict Analysis", "table_corr_1.csv"), row.names=F)
write.csv(res2, file.path("Analysis", "Conflict Analysis", "table_corr_2.csv"), row.names=F)
write.csv(res3, file.path("Analysis", "Conflict Analysis", "table_corr_3.csv"), row.names=F)
write.csv(res4, file.path("Analysis", "Conflict Analysis", "table_corr_4.csv"), row.names=F)
write.csv(res5, file.path("Analysis", "Conflict Analysis", "table_corr_5.csv"), row.names=F)
write.csv(res6, file.path("Analysis", "Conflict Analysis", "table_corr_6.csv"), row.names=F)

########################################
# Cleanup

# Remove
rm(dv1, dv2, dv3, dv4, dv5, dv6)
rm(iv1, iv2, iv3, iv4, iv5, iv6)
rm(res1, res2, res3, res4, res5, res6)
rm(ivsa, ivsb, ivsc, ivsd, ivse, ivsf_d, ivsf_c, ivsg)
rm(myvs1, myvs2, myivs, res, mycorrres)
rm(dat, dat2, xdat)
rm(mydf, mydf1, mydf2)
gc()

########################################
# END
########################################