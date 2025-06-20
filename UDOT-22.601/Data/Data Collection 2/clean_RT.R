#######################################
# Project:  MPC-693 Transit ped safety
# Authors:  Atul Subedi (atul.subedi@usu.edu)
# File:     Cleaning Right turn dataset.R
# Date:     2024 Fall
# About:    Cleaning to merge 
########################################

# Install Packages

# Load Libraries
library("lmtest")
library("lme4")
library("lmerTest")
library("ordinal")
library("mgcv")
library("rcompanion")
library("gratia")

# Loading
dat <- readRDS(file=file.path("Data", "Data Collection 2", "datlongcombo.rds"))

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
source(file.path("Data", "Data Collection 2", "add_vars.R"))
# Inspect
names(dat)

########################################
# Select independent variables

# a. Pedestrian characteristics
ivsa <- c("GroupSize", "logGroupSize", "Age_ChildTeen", "Gender_Female", "Gender_Unknown", 
          "OC_Load", "OC_StrWhe", "OC_SkaSco", "OC_Bicycle", "OC_Distracted", 
          "CWNum1", "CDirAppr")

# b. Driver & vehicle characteristics
ivsb <- c("RTQueue", "VehType2_Small", "VehType2_Large")

# c. Conflict information
ivsc <- c()

# d. Weather and time information
ivsd <- c("PRCP_01", "TEMP", "TEMP_5065", "TEMP_6580", "TEMP_8095", 
          "Weekday_M_F", "Weekend", 
          "TOD2_0611", "TOD2_1805", "AMPeak", "PMPeak")

# e. Traffic signal statuses
ivse <- c("PEDCP_ped_FDW", "PEDCP_ped_SDW", "VEHCP_veh_Y", "VEHCP_veh_R") 

# f. Corner & intersection attributes
ivsf <- c("CURB_RAD", "CW_DIST", "SB_DIST", "RAMPS_2", "CRType_Dire", "CRType_Blen", 
          "CWType_Cont", "CWType_None", "RT_LNS_05", "RT_LNS_2", "REC_LNS_1", 
          "CHANNEL", "Skewed", "EXTENSION", "RTOR", "Street.Lt", "BIKE_LN", 
          "AADP0100", "logAADP", "AADT1000", "logAADT", 
          "RTNotSig", "OnRamp", "OffRamp")

# g. Neighborhood attributes
ivsg <- c("popden_000_qtmi", "empden_000_qtmi", 
          "per_res_qtmi", "per_com_qtmi", "per_ind_qtmi", "per_vac_qtmi", "per_other_qtmi", 
          "intden_qtmi", "per4wy_qtmi", "stops_qtmi", 
          "worship_qtmi", "schools_qtmi", "park_acre_qtmi", 
          "income_000_qtmi", "avgveh_qtmi", "hhsize_qtmi")

# Inspect distributions
summary(dat[,c(ivsa, ivsb, ivsc, ivsd, ivse, ivsf, ivsg)])

# Inspect correlations
# level 1 units
mycor1 <- cor(dat[,c(ivsa, ivsb, ivsc, ivsd, ivse, ivsf, ivsg)], use="pairwise.complete.obs")
# write.csv(mycor1, file=file.path("Analysis", "Conflict Analysis", "Multilevel Models", "mycor1.csv"))
# level 2 units
l2dat <- dat[!duplicated(dat$L2ID),]
mycor2 <- cor(l2dat[,c(ivsf, ivsg)], use="pairwise.complete.obs")
# write.csv(mycor2, file=file.path("Analysis", "Conflict Analysis", "Multilevel Models", "mycor2.csv"))
# cleanup
rm(mycor1, mycor2, l2dat)

# Refine independent variables
# based on correlation inspection (> 0.60)
# Level 1: a,b,c,d,e: ivsa, ivsb, ivsc, ivsd, ivse
# NAs for Weekend                     --> remove
# +0.63 CWNum1 VEHCP_veh_R            --> okay
# others okay                         --> pick {TEMP} or {TEMP_5065, TEMP_6580, TEMP_8095}
# others okay                         --> pick {TOD2_0611, TOD2_1805} or {AMPeak, PMPeak}
ivsd <- ivsd[-grep("Weekend", ivsd)]
# Level 2: f,g: ivsf & ivsg
# NAs for CWType_None                 --> remove
# NAs for EXTENSION, RTOR, Street.Lt  --> remove
# +0.89 AADT1000 logAADT              --> pick {AADT1000} or {logAADT}
# +0.85 OffRamp RT_LNS_2              --> okay
# +0.80 AADP0100 logAADP              --> pick {AADP0100} or {logAADP}
# +0.75 RTNotSig CHANNEL              --> okay
# +0.71 AADT1000 RT_LNS_2             --> okay
# +0.70 empden_000_qtmi AADP0100      --> okay
# +0.70 stops_qtmi logAADP            --> okay
# +0.67 OnRamp REC_LNS1               --> okay
# +0.66 OffRamp CHANNEL               --> okay
# -0.66 per_com_qtmi per_res_qtmi    --> okay
# +0.64 SB_DIST CW_DIST               --> okay
# +0.64 popden_000_qtmi logAADP       --> okay
# -0.64 hhsize_qtmi AADP0100          --> okay
# -0.62 hhsize_qtmi logAADP           --> okay
# +0.61 CHANNEL CURB_RAD              --> okay
# +0.61 AADP0100 RAMPS_2              --> okay
ivsf <- ivsf[-grep("CWType_None", ivsf)]
ivsf <- ivsf[-grep("EXTENSION", ivsf)]
ivsf <- ivsf[-grep("RTOR", ivsf)]
ivsf <- ivsf[-grep("Street.Lt", ivsf)]

# Inspect
summary(dat[,c(ivsa, ivsb, ivsc, ivsd, ivse, ivsf, ivsg)])

# Rename
df_RT <- dat

# Save
write.csv(df_RT, file.path("Data", "Data Collection 2", "df_RT.csv"), row.names=F)
saveRDS(df_RT, file.path("Data", "Data Collection 2", "df_RT.rds"))

# Remove
rm(dat, xdat, ivsa, ivsb, ivsc, ivsd, ivse, ivsf, ivsg)
rm(df_RT)
gc()

# END