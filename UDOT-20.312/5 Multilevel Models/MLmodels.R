#######################################
# Project:  UDOT-19.312 Right turn safety
# Authors:  Atul Subedi (atul.subedi@usu.edu)
#           Patrick Singleton (patrick.singleton@usu.edu)
# File:     MLmodels.R
# Date:     2023 Spring
# About:    Estimate multilevel regression models
########################################

########################################
# Notes:

# Major Edits
# 2023-01-30 PAS created from 1-6 MLM scripts
# - simplified code to construct DVs and IVs
# 2023-02-04 PAS updated with new source data
# 2023-02-07 AS added code for other DVs
# 2023-02-13 AS added code for new DVs
# 2023-02-14 PAS minor code updates
# 2023-02-25 AS updated
# 2023-03-15 PAS updated
# 2023-04-15 AS/PAS updated with some new IVs

# Load packages
library("lmtest")
library("lme4")
library("lmerTest")
library("ordinal")
library("mgcv")
library("rcompanion")
library("gratia")

# Function to calculate log-likelihood from an gam MNL model
getLLmnl <- function(tmod) {
  tp <- predict(tmod, type="response")
  ty <- as.numeric(model.response(model.frame(tmod))) + 1
  tt <- as.matrix(cbind(1:length(ty), ty))
  p <- tp[tt]
  l <- log(p)
  return(sum(l))
}

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

########## Encroachment time ##########
# Multilevel linear regression model
# 1. Encroachment time

# Create dataset
dat1 <- dat[!is.na(dat$EncrTime) & dat$CWType_None==F & !is.na(dat$AADT1000),]

# Inspect DV
summary(dat1$EncrTime)

# One-level intercept-only model
mod01 <- lm(EncrTime ~ 1, data=dat1)
summary(mod01)

# Multilevel model (empty) with random intercept
mod02 <- lmer(EncrTime ~ (1 | L2ID), data=dat1)
summary(mod02)
anova(mod02, mod01) # better with random intercept

# Try independent variables, one at a time
# a/b. Pedestrian, driver, vehicle characteristics (ivsa, ivsb)
# sig: logGroupSize + Gender_Female + OC_Bicycle + CWNum1 + CDirAppr + RTQueue + VehType2_Large
mod <- update(mod02, formula= . ~ . + GroupSize)
mod <- update(mod02, formula= . ~ . + logGroupSize)
mod <- update(mod02, formula= . ~ . + Age_ChildTeen)
mod <- update(mod02, formula= . ~ . + Gender_Female)
mod <- update(mod02, formula= . ~ . + OC_Load + OC_StrWhe + OC_SkaSco + OC_Bicycle + OC_Distracted)
mod <- update(mod02, formula= . ~ . + CWNum1)
mod <- update(mod02, formula= . ~ . + CDirAppr)
mod <- update(mod02, formula= . ~ . + RTQueue)
mod <- update(mod02, formula= . ~ . + VehType2_Small + VehType2_Large)
summary(mod); rm(mod)
# c/d/e Conflict, weather, time information, traffic signal statuses (ivsc, ivsd, ivse)
# sig: TEMP_5065 + PMPeak + PEDCP_ped_SDW + VEHCP_veh_R
mod <- update(mod02, formula= . ~ . + TEMP); BIC(mod)
mod <- update(mod02, formula= . ~ . + TEMP_5065 + TEMP_6580 + TEMP_8095); BIC(mod)
mod <- update(mod02, formula= . ~ . + PRCP_01)
mod <- update(mod02, formula= . ~ . + Weekday_M_F)
mod <- update(mod02, formula= . ~ . + TOD2_0611 + TOD2_1805); BIC(mod)
mod <- update(mod02, formula= . ~ . + AMPeak + PMPeak); BIC(mod)
mod <- update(mod02, formula= . ~ . + PEDCP_ped_FDW + PEDCP_ped_SDW + RTNotSig)
mod <- update(mod02, formula= . ~ . + VEHCP_veh_Y + VEHCP_veh_R + RTNotSig)
summary(mod); rm(mod)
# f. Corner & intersection attributes (ivsf)
# sig: CW_DIST + BIKE_LN
mod <- update(mod02, formula= . ~ . + CURB_RAD)
mod <- update(mod02, formula= . ~ . + CW_DIST + CHANNEL)
mod <- update(mod02, formula= . ~ . + SB_DIST + CHANNEL)
mod <- update(mod02, formula= . ~ . + RAMPS_2)
mod <- update(mod02, formula= . ~ . + CRType_Blen + CRType_Dire)
mod <- update(mod02, formula= . ~ . + CWType_Cont)
mod <- update(mod02, formula= . ~ . + RT_LNS_05 + RT_LNS_2)
mod <- update(mod02, formula= . ~ . + REC_LNS_1)
mod <- update(mod02, formula= . ~ . + CHANNEL)
mod <- update(mod02, formula= . ~ . + Skewed)
mod <- update(mod02, formula= . ~ . + BIKE_LN)
mod <- update(mod02, formula= . ~ . + AADP0100); BIC(mod)
mod <- update(mod02, formula= . ~ . + logAADP); BIC(mod)
mod <- update(mod02, formula= . ~ . + AADT1000); BIC(mod)
mod <- update(mod02, formula= . ~ . + logAADT); BIC(mod)
mod <- update(mod02, formula= . ~ . + RTNotSig)
mod <- update(mod02, formula= . ~ . + OnRamp + OffRamp)
summary(mod); rm(mod)
# g. Neighborhood attributes (ivsg)
# sig: none
mod <- update(mod02, formula= . ~ . + popden_000_qtmi)
mod <- update(mod02, formula= . ~ . + empden_000_qtmi)
mod <- update(mod02, formula= . ~ . + per_res_qtmi)
mod <- update(mod02, formula= . ~ . + per_com_qtmi)
mod <- update(mod02, formula= . ~ . + per_ind_qtmi)
mod <- update(mod02, formula= . ~ . + per_vac_qtmi)
mod <- update(mod02, formula= . ~ . + per_other_qtmi)
mod <- update(mod02, formula= . ~ . + intden_qtmi)
mod <- update(mod02, formula= . ~ . + per4wy_qtmi)
mod <- update(mod02, formula= . ~ . + stops_qtmi)
mod <- update(mod02, formula= . ~ . + worship_qtmi)
mod <- update(mod02, formula= . ~ . + schools_qtmi)
mod <- update(mod02, formula= . ~ . + park_acre_qtmi)
mod <- update(mod02, formula= . ~ . + income_000_qtmi)
mod <- update(mod02, formula= . ~ . + avgveh_qtmi)
mod <- update(mod02, formula= . ~ . + hhsize_qtmi)
summary(mod); rm(mod)

# Try all significant IVs altogether, backwards removal
mod03a <- update(mod02, formula= . ~ . + logGroupSize + Gender_Female + OC_Bicycle + CWNum1 + CDirAppr + RTQueue + VehType2_Large + TEMP_5065 + PMPeak + PEDCP_ped_SDW + VEHCP_veh_R + CW_DIST + BIKE_LN)
summary(mod03a)
mod03b <- update(mod03a, formula= . ~ . - Gender_Female)
summary(mod03b)
anova(mod03b, mod03a) # okay to remove Gender_Female
mod03c <- update(mod03b, formula= . ~ . - CW_DIST)
summary(mod03c)
anova(mod03c, mod03b) # okay to remove CW_DIST
mod03d <- update(mod03c, formula= . ~ . - BIKE_LN)
summary(mod03d)
anova(mod03d, mod03c) # okay to remove BIKE_LN
mod03e <- update(mod03d, formula= . ~ . - TEMP_5065)
summary(mod03e)
anova(mod03e, mod03d) # okay to remove TEMP_5065
rm(mod03a, mod03b, mod03c, mod03d, mod03e)
# final specification
mod03 <- update(mod02, formula= . ~ . + logGroupSize + OC_Bicycle + CWNum1 + CDirAppr + RTQueue + VehType2_Large + PMPeak + PEDCP_ped_SDW + VEHCP_veh_R)
summary(mod03)
anova(mod03, mod02) # better with new IVs

# Summarize, combine, and save all models
summary(mod01)
summary(mod02)
summary(mod03)
logLik(mod01); logLik(mod02); logLik(mod03)
mods1 <- list(dat1, mod01, mod02, mod03)
names(mods1) <- c("dat1", "mod01", "mod02", "mod03")
saveRDS(mods1, file=file.path("Analysis", "Conflict Analysis", "mods1.rds"))

# Cleanup
rm(mod01, mod02, mod03)
rm(mods1, dat1)
gc()

########## Pre-encroachment time ##########
# Multilevel linear regression models
# 1a. Pre-encroachment time

# Create dataset
dat1a <- dat[!is.na(dat$Encroachment_Time) & dat$CWType_None==F & !is.na(dat$AADT1000),]
dat1a <- dat1a[dat1a$Encroachment_Time<0,]

# Inspect DV
dat1a$EncrTimePre <- abs(dat1a$Encroachment_Time)
summary(dat1a$EncrTimePre)

# One-level intercept-only model
mod01 <- lm(EncrTimePre ~ 1, data=dat1a)
summary(mod01)

# Multilevel model (empty) with random intercept
mod02 <- lmer(EncrTimePre ~ 1 + (1 | L2ID), data=dat1a)
summary(mod02)
anova(mod02, mod01) # better with random intercept

# Try independent variables, one at a time
# a/b. Pedestrian, driver, vehicle characteristics (ivsa, ivsb)
# sig: logGroupSize + Gender_Female + OC_Bicycle + CWNum1 + CDirAppr + RTQueue + VehType2_Large
mod <- update(mod02, formula= . ~ . + GroupSize)
mod <- update(mod02, formula= . ~ . + logGroupSize)
mod <- update(mod02, formula= . ~ . + Age_ChildTeen)
mod <- update(mod02, formula= . ~ . + Gender_Female)
mod <- update(mod02, formula= . ~ . + OC_Load + OC_StrWhe + OC_SkaSco + OC_Bicycle + OC_Distracted)
mod <- update(mod02, formula= . ~ . + CWNum1)
mod <- update(mod02, formula= . ~ . + CDirAppr)
mod <- update(mod02, formula= . ~ . + RTQueue)
mod <- update(mod02, formula= . ~ . + VehType2_Small + VehType2_Large)
summary(mod); rm(mod)
# c/d/e Conflict, weather, time information, traffic signal statuses (ivsc, ivsd, ivse)
# sig: TEMP_5065 + VEHCP_veh_R
mod <- update(mod02, formula= . ~ . + TEMP); BIC(mod)
mod <- update(mod02, formula= . ~ . + TEMP_5065 + TEMP_6580 + TEMP_8095); BIC(mod)
mod <- update(mod02, formula= . ~ . + PRCP_01)
mod <- update(mod02, formula= . ~ . + Weekday_M_F)
mod <- update(mod02, formula= . ~ . + TOD2_0611 + TOD2_1805); BIC(mod)
mod <- update(mod02, formula= . ~ . + AMPeak + PMPeak); BIC(mod)
mod <- update(mod02, formula= . ~ . + PEDCP_ped_FDW + PEDCP_ped_SDW + RTNotSig)
mod <- update(mod02, formula= . ~ . + VEHCP_veh_Y + VEHCP_veh_R + RTNotSig)
summary(mod); rm(mod)
# f. Corner & intersection attributes (ivsf)
# sig: CRType_Dire
mod <- update(mod02, formula= . ~ . + CURB_RAD)
mod <- update(mod02, formula= . ~ . + CW_DIST + CHANNEL)
mod <- update(mod02, formula= . ~ . + SB_DIST + CHANNEL)
mod <- update(mod02, formula= . ~ . + RAMPS_2)
mod <- update(mod02, formula= . ~ . + CRType_Blen + CRType_Dire)
mod <- update(mod02, formula= . ~ . + CWType_Cont)
mod <- update(mod02, formula= . ~ . + RT_LNS_05 + RT_LNS_2)
mod <- update(mod02, formula= . ~ . + REC_LNS_1)
mod <- update(mod02, formula= . ~ . + CHANNEL)
mod <- update(mod02, formula= . ~ . + Skewed)
mod <- update(mod02, formula= . ~ . + BIKE_LN)
mod <- update(mod02, formula= . ~ . + AADP0100); BIC(mod)
mod <- update(mod02, formula= . ~ . + logAADP); BIC(mod)
mod <- update(mod02, formula= . ~ . + AADT1000); BIC(mod)
mod <- update(mod02, formula= . ~ . + logAADT); BIC(mod)
mod <- update(mod02, formula= . ~ . + RTNotSig)
mod <- update(mod02, formula= . ~ . + OnRamp + OffRamp)
summary(mod); rm(mod)
# g. Neighborhood attributes (ivsg)
# sig: (none)
mod <- update(mod02, formula= . ~ . + popden_000_qtmi)
mod <- update(mod02, formula= . ~ . + empden_000_qtmi)
mod <- update(mod02, formula= . ~ . + per_res_qtmi)
mod <- update(mod02, formula= . ~ . + per_com_qtmi)
mod <- update(mod02, formula= . ~ . + per_ind_qtmi)
mod <- update(mod02, formula= . ~ . + per_vac_qtmi)
mod <- update(mod02, formula= . ~ . + per_other_qtmi)
mod <- update(mod02, formula= . ~ . + intden_qtmi)
mod <- update(mod02, formula= . ~ . + per4wy_qtmi)
mod <- update(mod02, formula= . ~ . + stops_qtmi)
mod <- update(mod02, formula= . ~ . + worship_qtmi)
mod <- update(mod02, formula= . ~ . + schools_qtmi)
mod <- update(mod02, formula= . ~ . + park_acre_qtmi)
mod <- update(mod02, formula= . ~ . + income_000_qtmi)
mod <- update(mod02, formula= . ~ . + avgveh_qtmi)
mod <- update(mod02, formula= . ~ . + hhsize_qtmi)
summary(mod); rm(mod)

# Try all significant IVs altogether, backwards removal
mod03a <- update(mod02, formula= . ~ . + logGroupSize + Gender_Female + OC_Bicycle + CWNum1 + CDirAppr + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CRType_Dire)
summary(mod03a)
mod03b <- update(mod03a, formula= . ~ . - CRType_Dire)
summary(mod03b)
anova(mod03b, mod03a) # okay to remove CRType_Dire
mod03c <- update(mod03b, formula= . ~ . - logGroupSize)
summary(mod03c)
anova(mod03c, mod03b) # okay to remove logGroupSize
mod03d <- update(mod03c, formula= . ~ . - RTQueue)
summary(mod03d)
anova(mod03d, mod03c) # okay to remove RTQueue
rm(mod03a, mod03b, mod03c, mod03d)
# final specification
mod03 <- update(mod02, formula= . ~ . + Gender_Female + OC_Bicycle + CWNum1 + CDirAppr + VehType2_Large + TEMP_5065 + VEHCP_veh_R)
summary(mod03)
anova(mod03, mod02) # better with new IVs

# Summarize, combine, and save all models
summary(mod01)
summary(mod02)
summary(mod03)
logLik(mod01); logLik(mod02); logLik(mod03)
mods1a <- list(dat1a, mod01, mod02, mod03)
names(mods1a) <- c("dat1a", "mod01", "mod02", "mod03")
saveRDS(mods1a, file=file.path("Analysis", "Conflict Analysis", "mods1a.rds"))
# Cleanup
rm(mod01, mod02, mod03)
rm(mods1a, dat1a)
gc()

########## Post-encroachment time ##########
# Multilevel linear regression models
# 1b. Post-encroachment time

# Create dataset
dat1b <- dat[!is.na(dat$Encroachment_Time) & dat$CWType_None==F & !is.na(dat$AADT1000),]
dat1b <- dat1b[dat1b$Encroachment_Time>0,]

# Inspect DV
dat1b$EncrTimePost <- abs(dat1b$Encroachment_Time)
summary(dat1b$EncrTimePost)

# One-level intercept-only model
mod01 <- lm(EncrTimePost ~ 1, data=dat1b)
summary(mod01)

# Multilevel model (empty) with random intercept
mod02 <- lmer(EncrTimePost ~ 1 + (1 | L2ID), data=dat1b)
summary(mod02)
anova(mod02, mod01) # better with random intercept

# Try independent variables, one at a time
# a/b. Pedestrian, driver, vehicle characteristics (ivsa, ivsb)
# sig: logGroupSize + OC_Load + OC_SkaSco + OC_Bicycle + CWNum1 + CDirAppr + VehType2_Large
mod <- update(mod02, formula= . ~ . + GroupSize)
mod <- update(mod02, formula= . ~ . + logGroupSize)
mod <- update(mod02, formula= . ~ . + Age_ChildTeen)
mod <- update(mod02, formula= . ~ . + Gender_Female)
mod <- update(mod02, formula= . ~ . + OC_Load + OC_StrWhe + OC_SkaSco + OC_Bicycle + OC_Distracted)
mod <- update(mod02, formula= . ~ . + CWNum1)
mod <- update(mod02, formula= . ~ . + CDirAppr)
mod <- update(mod02, formula= . ~ . + RTQueue)
mod <- update(mod02, formula= . ~ . + VehType2_Small + VehType2_Large)
summary(mod); rm(mod)
# c/d/e Conflict, weather, time information, traffic signal statuses (ivsc, ivsd, ivse)
# sig: PEDCP_ped_FDW + PEDCP_ped_SDW + VEHCP_veh_R
mod <- update(mod02, formula= . ~ . + TEMP); BIC(mod)
mod <- update(mod02, formula= . ~ . + TEMP_5065 + TEMP_6580 + TEMP_8095); BIC(mod)
mod <- update(mod02, formula= . ~ . + PRCP_01)
mod <- update(mod02, formula= . ~ . + Weekday_M_F)
mod <- update(mod02, formula= . ~ . + TOD2_0611 + TOD2_1805); BIC(mod)
mod <- update(mod02, formula= . ~ . + AMPeak + PMPeak); BIC(mod)
mod <- update(mod02, formula= . ~ . + PEDCP_ped_FDW + PEDCP_ped_SDW + RTNotSig)
mod <- update(mod02, formula= . ~ . + VEHCP_veh_Y + VEHCP_veh_R + RTNotSig)
summary(mod); rm(mod)
# f. Corner & intersection attributes (ivsf)
# sig: CW_DIST + SB_DIST + BIKE_LN
mod <- update(mod02, formula= . ~ . + CURB_RAD)
mod <- update(mod02, formula= . ~ . + CW_DIST + CHANNEL)
mod <- update(mod02, formula= . ~ . + SB_DIST + CHANNEL)
mod <- update(mod02, formula= . ~ . + RAMPS_2)
mod <- update(mod02, formula= . ~ . + CRType_Blen + CRType_Dire)
mod <- update(mod02, formula= . ~ . + CWType_Cont)
mod <- update(mod02, formula= . ~ . + RT_LNS_05 + RT_LNS_2)
mod <- update(mod02, formula= . ~ . + REC_LNS_1)
mod <- update(mod02, formula= . ~ . + CHANNEL)
mod <- update(mod02, formula= . ~ . + Skewed)
mod <- update(mod02, formula= . ~ . + BIKE_LN)
mod <- update(mod02, formula= . ~ . + AADP0100); BIC(mod)
mod <- update(mod02, formula= . ~ . + logAADP); BIC(mod)
mod <- update(mod02, formula= . ~ . + AADT1000); BIC(mod)
mod <- update(mod02, formula= . ~ . + logAADT); BIC(mod)
mod <- update(mod02, formula= . ~ . + RTNotSig)
mod <- update(mod02, formula= . ~ . + OnRamp + OffRamp)
summary(mod); rm(mod)
# g. Neighborhood attributes (ivsg)
# sig: none
mod <- update(mod02, formula= . ~ . + popden_000_qtmi)
mod <- update(mod02, formula= . ~ . + empden_000_qtmi)
mod <- update(mod02, formula= . ~ . + per_res_qtmi)
mod <- update(mod02, formula= . ~ . + per_com_qtmi)
mod <- update(mod02, formula= . ~ . + per_ind_qtmi)
mod <- update(mod02, formula= . ~ . + per_vac_qtmi)
mod <- update(mod02, formula= . ~ . + per_other_qtmi)
mod <- update(mod02, formula= . ~ . + intden_qtmi)
mod <- update(mod02, formula= . ~ . + per4wy_qtmi)
mod <- update(mod02, formula= . ~ . + stops_qtmi)
mod <- update(mod02, formula= . ~ . + worship_qtmi)
mod <- update(mod02, formula= . ~ . + schools_qtmi)
mod <- update(mod02, formula= . ~ . + park_acre_qtmi)
mod <- update(mod02, formula= . ~ . + income_000_qtmi)
mod <- update(mod02, formula= . ~ . + avgveh_qtmi)
mod <- update(mod02, formula= . ~ . + hhsize_qtmi)
summary(mod); rm(mod)

# Try all significant IVs altogether, backwards removal
mod03a <- update(mod02, formula= . ~ . + logGroupSize + OC_Load + OC_SkaSco + OC_Bicycle + CWNum1 + CDirAppr + VehType2_Large + PEDCP_ped_FDW + PEDCP_ped_SDW + VEHCP_veh_R + CW_DIST + SB_DIST + BIKE_LN)
summary(mod03a)
mod03b <- update(mod03a, formula= . ~ . - SB_DIST)
summary(mod03b)
anova(mod03b, mod03a) # ok remove SB_DIST 
mod03c <- update(mod03b, formula= . ~ . - CWNum1)
summary(mod03c)
anova(mod03c, mod03b) # okay to remove CWNum1
mod03d <- update(mod03c, formula= . ~ . - BIKE_LN)
summary(mod03d)
anova(mod03d, mod03c) # okay to remove BIKE_LN
mod03e <- update(mod03d, formula= . ~ . - VehType2_Large)
summary(mod03e)
anova(mod03e, mod03d) # okay to remove VehType2_Large
mod03f <- update(mod03e, formula= . ~ . - OC_Load)
summary(mod03f)
anova(mod03f, mod03e) # okay to remove OC_Load
rm(mod03a, mod03b, mod03c, mod03d, mod03e, mod03f)
# final specification
mod03 <- update(mod02, formula= . ~ . + logGroupSize + OC_SkaSco + OC_Bicycle + CDirAppr + PEDCP_ped_FDW + PEDCP_ped_SDW + VEHCP_veh_R + CW_DIST)
summary(mod03)
anova(mod03, mod02) # better with new IVs

# Summarize, combine, and save all models
summary(mod01)
summary(mod02)
summary(mod03)
logLik(mod01); logLik(mod02); logLik(mod03)
mods1b <- list(dat1b, mod01, mod02, mod03)
names(mods1b) <- c("dat1b", "mod01", "mod02", "mod03")
saveRDS(mods1b, file=file.path("Analysis", "Conflict Analysis", "mods1b.rds"))
# Cleanup
rm(mod01, mod02, mod03)
rm(mods1b, dat1b)
gc()

########## Conflict severity ##########
# Multilevel ordered logit model
# 2. Conflict severity

# Create dataset
dat2 <- dat[!is.na(dat$ConflSev) & dat$CWType_None==F & !is.na(dat$AADT1000),]

# Inspect DV
summary(dat2$ConflSev)

# One-level intercept-only model
mod01 <- clm(ConflSev ~ 1, data=dat2, link="logit")
summary(mod01); coeftest(mod01)

# Multilevel model (empty) with random intercept
mod02 <- clmm(ConflSev ~ 1 + (1 | L2ID), data=dat2, link="logit")
summary(mod02); coeftest(mod01)
anova(mod02, mod01) # better with random intercept

# Try independent variables, one at a time
# a/b. Pedestrian, driver, vehicle characteristics (ivsa, ivsb)
# sig: logGroupSize + OC_StrWhe + OC_Bicycle + CWNum1 + CDirAppr + RTQueue + VehType2_Large
mod <- update(mod02, formula= . ~ . + GroupSize)
mod <- update(mod02, formula= . ~ . + logGroupSize)
mod <- update(mod02, formula= . ~ . + Age_ChildTeen)
mod <- update(mod02, formula= . ~ . + Gender_Female)
mod <- update(mod02, formula= . ~ . + OC_Load + OC_StrWhe + OC_SkaSco + OC_Bicycle + OC_Distracted)
mod <- update(mod02, formula= . ~ . + CWNum1)
mod <- update(mod02, formula= . ~ . + CDirAppr)
mod <- update(mod02, formula= . ~ . + RTQueue)
mod <- update(mod02, formula= . ~ . + VehType2_Small + VehType2_Large)
summary(mod); rm(mod)
# c/d/e Conflict, weather, time information, traffic signal statuses (ivsc, ivsd, ivse)
# sig: TEMP_5065 + PRCP_01 + PMPeak + PEDCP_ped_SDW + VEHCP_veh_R
mod <- update(mod02, formula= . ~ . + TEMP); BIC(mod)
mod <- update(mod02, formula= . ~ . + TEMP_5065 + TEMP_6580 + TEMP_8095); BIC(mod)
mod <- update(mod02, formula= . ~ . + PRCP_01)
mod <- update(mod02, formula= . ~ . + Weekday_M_F)
mod <- update(mod02, formula= . ~ . + TOD2_0611 + TOD2_1805); BIC(mod)
mod <- update(mod02, formula= . ~ . + AMPeak + PMPeak); BIC(mod)
mod <- update(mod02, formula= . ~ . + PEDCP_ped_FDW + PEDCP_ped_SDW + RTNotSig)
mod <- update(mod02, formula= . ~ . + VEHCP_veh_Y + VEHCP_veh_R + RTNotSig)
summary(mod); rm(mod)
# f. Corner & intersection attributes (ivsf)
# sig: CW_DIST + BIKE_LN
mod <- update(mod02, formula= . ~ . + CURB_RAD)
mod <- update(mod02, formula= . ~ . + CW_DIST + CHANNEL)
mod <- update(mod02, formula= . ~ . + SB_DIST + CHANNEL)
mod <- update(mod02, formula= . ~ . + RAMPS_2)
mod <- update(mod02, formula= . ~ . + CRType_Blen + CRType_Dire)
mod <- update(mod02, formula= . ~ . + CWType_Cont)
mod <- update(mod02, formula= . ~ . + RT_LNS_05 + RT_LNS_2)
mod <- update(mod02, formula= . ~ . + REC_LNS_1)
mod <- update(mod02, formula= . ~ . + CHANNEL)
mod <- update(mod02, formula= . ~ . + Skewed)
mod <- update(mod02, formula= . ~ . + BIKE_LN)
mod <- update(mod02, formula= . ~ . + AADP0100); BIC(mod)
mod <- update(mod02, formula= . ~ . + logAADP); BIC(mod)
mod <- update(mod02, formula= . ~ . + AADT1000); BIC(mod)
mod <- update(mod02, formula= . ~ . + logAADT); BIC(mod)
mod <- update(mod02, formula= . ~ . + RTNotSig)
mod <- update(mod02, formula= . ~ . + OnRamp + OffRamp)
summary(mod); rm(mod)
# g. Neighborhood attributes (ivsg)
# sig: hhsize_qtmi
mod <- update(mod02, formula= . ~ . + popden_000_qtmi)
mod <- update(mod02, formula= . ~ . + empden_000_qtmi)
mod <- update(mod02, formula= . ~ . + per_res_qtmi)
mod <- update(mod02, formula= . ~ . + per_com_qtmi)
mod <- update(mod02, formula= . ~ . + per_ind_qtmi)
mod <- update(mod02, formula= . ~ . + per_vac_qtmi)
mod <- update(mod02, formula= . ~ . + per_other_qtmi)
mod <- update(mod02, formula= . ~ . + intden_qtmi)
mod <- update(mod02, formula= . ~ . + per4wy_qtmi)
mod <- update(mod02, formula= . ~ . + stops_qtmi)
mod <- update(mod02, formula= . ~ . + worship_qtmi)
mod <- update(mod02, formula= . ~ . + schools_qtmi)
mod <- update(mod02, formula= . ~ . + park_acre_qtmi)
mod <- update(mod02, formula= . ~ . + income_000_qtmi)
mod <- update(mod02, formula= . ~ . + avgveh_qtmi)
mod <- update(mod02, formula= . ~ . + hhsize_qtmi)
summary(mod); rm(mod)

# Try all significant IVs altogether, backwards removal
mod03a <- update(mod02, formula= . ~ . + logGroupSize + OC_StrWhe + OC_Bicycle + CWNum1 + CDirAppr + RTQueue + VehType2_Large + TEMP_5065 + PRCP_01 + PMPeak + PEDCP_ped_SDW + VEHCP_veh_R + CW_DIST + BIKE_LN + hhsize_qtmi)
summary(mod03a)
mod03b <- update(mod03a, formula= . ~ . - BIKE_LN)
summary(mod03b)
anova(mod03b, mod03a) # Ok remove BIKE_LN
mod03c <- update(mod03b, formula= . ~ . - TEMP_5065)
summary(mod03c)
anova(mod03c, mod03b) # Ok remove TEMP_5065 
mod03d <- update(mod03c, formula= . ~ . - CWNum1)
summary(mod03d)
anova(mod03d, mod03c) # Ok remove CWNum1 
mod03e <- update(mod03d, formula= . ~ . - CDirAppr)
summary(mod03e)
anova(mod03e, mod03d) # ok remove CDirAppr
rm(mod03a, mod03b, mod03c, mod03d, mod03e)
# final specification
mod03 <- update(mod02, formula= . ~ . + logGroupSize + OC_StrWhe + OC_Bicycle + RTQueue + VehType2_Large + PRCP_01 + PMPeak + PEDCP_ped_SDW + VEHCP_veh_R + CW_DIST + hhsize_qtmi)
summary(mod03)
anova(mod03, mod02) # better with new IVs

# Summarize, combine, and save all models
summary(mod01); coeftest(mod01)
summary(mod02); coeftest(mod02)
summary(mod03); coeftest(mod03)
logLik(mod01); logLik(mod02); logLik(mod03)
mods2 <- list(dat2, mod01, mod02, mod03)
names(mods2) <- c("dat2", "mod01", "mod02", "mod03")
saveRDS(mods2, file=file.path("Analysis", "Conflict Analysis", "mods2.rds"))
# Cleanup
rm(mod01, mod02, mod03)
rm(mods2, dat2)
gc()

########## Pedestrian reaction ##########
# Multilevel Multinomial Logit model
# 3. Pedestrian reaction

# Create dataset
dat3 <- dat[!is.na(dat$ReactPed) & dat$CWType_None==F & !is.na(dat$AADT1000),]
summary(dat3)

# Inspect DV
summary(dat3$ReactPed)
dat3$ReactPed0 <- as.integer(dat3$ReactPed) - 1
table(dat3$ReactPed0)

# One-level intercept-only model
mod01 <- gam(list(ReactPed0 
                  ~ 1, 
                  ~ 1), 
             data=dat3, family=multinom(K=2))
summary(mod01)

# Multilevel model (empty) with random intercept
mod02 <- gam(list(ReactPed0 
                  ~ 1 + s(as.factor(dat3$L2ID), bs="re"), 
                  ~ 1 + s(as.factor(dat3$L2ID), bs="re")), 
             data=dat3, family=multinom(K=2))
summary(mod02)
lrtest(mod02, mod01) # better with random intercept

# Try independent variables, one at a time
# a/b. Pedestrian, driver, vehicle characteristics (ivsa, ivsb)
# sig: Gender_Female + OC_Bicycle + CWNum1 + CDirAppr + RTQueue
# sig.1: Age_ChildTeen + RTQueue
mod <- gam(list(ReactPed0 
                  ~  s(as.factor(dat3$L2ID), bs="re") + GroupSize, 
                  ~  s(as.factor(dat3$L2ID), bs="re") + GroupSize), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~  s(as.factor(dat3$L2ID), bs="re") + logGroupSize, 
                ~  s(as.factor(dat3$L2ID), bs="re") + logGroupSize), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + Age_ChildTeen, 
                ~ s(as.factor(dat3$L2ID), bs="re") + Age_ChildTeen), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + Gender_Female , 
                ~ s(as.factor(dat3$L2ID), bs="re") + Gender_Female), 
           data=dat3, family=multinom(K=2))
# mod <- gam(list(ReactPed0 
#                 ~ s(as.factor(dat3$L2ID), bs="re") + OC_Load + OC_StrWhe + OC_SkaSco + OC_Bicycle + OC_Distracted, 
#                 ~ s(as.factor(dat3$L2ID), bs="re") + OC_Load + OC_StrWhe + OC_SkaSco + OC_Bicycle + OC_Distracted), 
#            data=dat3, family=multinom(K=2)) # didn't converge
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + OC_Load + OC_StrWhe + OC_SkaSco + OC_Bicycle + OC_Distracted, 
                ~ s(as.factor(dat3$L2ID), bs="re") + OC_Load + OC_SkaSco + OC_Bicycle + OC_Distracted), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + CWNum1, 
                ~ s(as.factor(dat3$L2ID), bs="re") + CWNum1), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + CDirAppr, 
                ~ s(as.factor(dat3$L2ID), bs="re") + CDirAppr), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + RTQueue, 
                ~ s(as.factor(dat3$L2ID), bs="re") + RTQueue), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + VehType2_Small + VehType2_Large, 
                ~ s(as.factor(dat3$L2ID), bs="re") + VehType2_Small + VehType2_Large), 
           data=dat3, family=multinom(K=2))
summary(mod); rm(mod)
# c/d/e Conflict, weather, time information, traffic signal statuses (ivsc, ivsd, ivse)
# sig: PEDCP_ped_FDW + PEDCP_ped_SDW + VEHCP_veh_R
# sig.1: TEMP_8095 + PEDCP_ped_FDW + PEDCP_ped_SDW 
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + TEMP, 
                ~ s(as.factor(dat3$L2ID), bs="re") + TEMP), 
           data=dat3, family=multinom(K=2)); BIC(mod)
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + TEMP_5065 + TEMP_6580 + TEMP_8095, 
                ~ s(as.factor(dat3$L2ID), bs="re") + TEMP_5065 + TEMP_6580 + TEMP_8095), 
           data=dat3, family=multinom(K=2)); BIC(mod)
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + PRCP_01, 
                ~ s(as.factor(dat3$L2ID), bs="re") + PRCP_01), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + Weekday_M_F, 
                ~ s(as.factor(dat3$L2ID), bs="re") + Weekday_M_F), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + TOD2_0611 + TOD2_1805, 
                ~ s(as.factor(dat3$L2ID), bs="re") + TOD2_0611 + TOD2_1805), 
           data=dat3, family=multinom(K=2)); BIC(mod)
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + AMPeak + PMPeak, 
                ~ s(as.factor(dat3$L2ID), bs="re") + AMPeak + PMPeak), 
           data=dat3, family=multinom(K=2)); BIC(mod)
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + PEDCP_ped_FDW + PEDCP_ped_SDW + RTNotSig, 
                ~ s(as.factor(dat3$L2ID), bs="re") + PEDCP_ped_FDW + PEDCP_ped_SDW + RTNotSig), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + VEHCP_veh_Y + VEHCP_veh_R + RTNotSig, 
                ~ s(as.factor(dat3$L2ID), bs="re") + VEHCP_veh_Y + VEHCP_veh_R + RTNotSig), 
           data=dat3, family=multinom(K=2))
summary(mod); rm(mod)
# f. Corner & intersection attributes (ivsf)
# sig: RT_LNS_2 + CHANNEL + Skewed + AADT1000 + OffRamp
# sig.1: CWType_Cont + RT_LNS_2 + CHANNEL
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + CURB_RAD, 
                ~ s(as.factor(dat3$L2ID), bs="re") + CURB_RAD), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + CW_DIST + CHANNEL, 
                ~ s(as.factor(dat3$L2ID), bs="re") + CW_DIST + CHANNEL), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + SB_DIST + CHANNEL, 
                ~ s(as.factor(dat3$L2ID), bs="re") + SB_DIST + CHANNEL), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + RAMPS_2, 
                ~ s(as.factor(dat3$L2ID), bs="re") + RAMPS_2), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + CRType_Blen + CRType_Dire, 
                ~ s(as.factor(dat3$L2ID), bs="re") + CRType_Blen + CRType_Dire), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + CWType_Cont, 
                ~ s(as.factor(dat3$L2ID), bs="re") + CWType_Cont), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + RT_LNS_05 + RT_LNS_2, 
                ~ s(as.factor(dat3$L2ID), bs="re") + RT_LNS_05 + RT_LNS_2), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + REC_LNS_1, 
                ~ s(as.factor(dat3$L2ID), bs="re") + REC_LNS_1), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + CHANNEL, 
                ~ s(as.factor(dat3$L2ID), bs="re") + CHANNEL), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + Skewed, 
                ~ s(as.factor(dat3$L2ID), bs="re") + Skewed), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + BIKE_LN, 
                ~ s(as.factor(dat3$L2ID), bs="re") + BIKE_LN), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + AADP0100, 
                ~ s(as.factor(dat3$L2ID), bs="re") + AADP0100), 
           data=dat3, family=multinom(K=2)); BIC(mod)
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + logAADP, 
                ~ s(as.factor(dat3$L2ID), bs="re") + logAADP), 
           data=dat3, family=multinom(K=2)); BIC(mod)
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + AADT1000, 
                ~ s(as.factor(dat3$L2ID), bs="re") + AADT1000), 
           data=dat3, family=multinom(K=2)); BIC(mod)
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + logAADT, 
                ~ s(as.factor(dat3$L2ID), bs="re") + logAADT), 
           data=dat3, family=multinom(K=2)); BIC(mod)
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + RTNotSig, 
                ~ s(as.factor(dat3$L2ID), bs="re") + RTNotSig), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + OnRamp + OffRamp, 
                ~ s(as.factor(dat3$L2ID), bs="re") + OnRamp + OffRamp), 
           data=dat3, family=multinom(K=2))
summary(mod); rm(mod)
# g. Neighborhood attributes (ivsg)
# sig: stops_qtmi + income_000_qtmi
# sig.1: income_000_qtmi
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + popden_000_qtmi, 
                ~ s(as.factor(dat3$L2ID), bs="re") + popden_000_qtmi), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + empden_000_qtmi, 
                ~ s(as.factor(dat3$L2ID), bs="re") + empden_000_qtmi), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + per_res_qtmi, 
                ~ s(as.factor(dat3$L2ID), bs="re") + per_res_qtmi), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + per_com_qtmi, 
                ~ s(as.factor(dat3$L2ID), bs="re") + per_com_qtmi), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + per_ind_qtmi, 
                ~ s(as.factor(dat3$L2ID), bs="re") + per_ind_qtmi), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + per_vac_qtmi, 
                ~ s(as.factor(dat3$L2ID), bs="re") + per_vac_qtmi), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + per_other_qtmi, 
                ~ s(as.factor(dat3$L2ID), bs="re") + per_other_qtmi), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + intden_qtmi, 
                ~ s(as.factor(dat3$L2ID), bs="re") + intden_qtmi), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + per4wy_qtmi, 
                ~ s(as.factor(dat3$L2ID), bs="re") + per4wy_qtmi), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + stops_qtmi, 
                ~ s(as.factor(dat3$L2ID), bs="re") + stops_qtmi), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + worship_qtmi, 
                ~ s(as.factor(dat3$L2ID), bs="re") + worship_qtmi), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + schools_qtmi, 
                ~ s(as.factor(dat3$L2ID), bs="re") + schools_qtmi), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + park_acre_qtmi, 
                ~ s(as.factor(dat3$L2ID), bs="re") + park_acre_qtmi), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + income_000_qtmi, 
                ~ s(as.factor(dat3$L2ID), bs="re") + income_000_qtmi), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + avgveh_qtmi, 
                ~ s(as.factor(dat3$L2ID), bs="re") + avgveh_qtmi), 
           data=dat3, family=multinom(K=2))
mod <- gam(list(ReactPed0 
                ~ s(as.factor(dat3$L2ID), bs="re") + hhsize_qtmi, 
                ~ s(as.factor(dat3$L2ID), bs="re") + hhsize_qtmi), 
           data=dat3, family=multinom(K=2))
summary(mod); rm(mod)

# Try all significant IVs altogether, backwards removal
mod03a <- gam(list(ReactPed0 
                   ~ s(as.factor(dat3$L2ID), bs="re") + Gender_Female + OC_Bicycle + CWNum1 + CDirAppr + RTQueue + PEDCP_ped_FDW + PEDCP_ped_SDW + VEHCP_veh_R + RT_LNS_2 + CHANNEL + Skewed + AADT1000 + OffRamp + stops_qtmi + income_000_qtmi, 
                   ~ s(as.factor(dat3$L2ID), bs="re") + Age_ChildTeen + RTQueue + TEMP_8095 + PEDCP_ped_FDW + PEDCP_ped_SDW + CWType_Cont + RT_LNS_2 + CHANNEL + income_000_qtmi), 
              data=dat3, family=multinom(K=2))
summary(mod03a)
mod03b <- gam(list(ReactPed0 
                   ~ s(as.factor(dat3$L2ID), bs="re") + Gender_Female + OC_Bicycle + CWNum1 + CDirAppr + RTQueue + PEDCP_ped_FDW + PEDCP_ped_SDW + VEHCP_veh_R + RT_LNS_2 + Skewed + AADT1000 + OffRamp + stops_qtmi + income_000_qtmi, 
                   ~ s(as.factor(dat3$L2ID), bs="re") + Age_ChildTeen + RTQueue + TEMP_8095 + PEDCP_ped_FDW + PEDCP_ped_SDW + CWType_Cont + RT_LNS_2 + CHANNEL + income_000_qtmi), 
              data=dat3, family=multinom(K=2))
summary(mod03b)
lrtest(mod03b, mod03a) # okay to remove CHANNEL
mod03c <- gam(list(ReactPed0 
                   ~ s(as.factor(dat3$L2ID), bs="re") + Gender_Female + OC_Bicycle + CWNum1 + CDirAppr + RTQueue + PEDCP_ped_SDW + VEHCP_veh_R + RT_LNS_2 + Skewed + AADT1000 + OffRamp + stops_qtmi + income_000_qtmi, 
                   ~ s(as.factor(dat3$L2ID), bs="re") + Age_ChildTeen + RTQueue + TEMP_8095 + PEDCP_ped_FDW + PEDCP_ped_SDW + CWType_Cont + RT_LNS_2 + CHANNEL + income_000_qtmi), 
              data=dat3, family=multinom(K=2))
summary(mod03c)
lrtest(mod03c, mod03b) # okay to remove PEDCP_ped_FDW
mod03d <- gam(list(ReactPed0 
                   ~ s(as.factor(dat3$L2ID), bs="re") + Gender_Female + OC_Bicycle + CWNum1 + CDirAppr + RTQueue + PEDCP_ped_SDW + VEHCP_veh_R + RT_LNS_2 + Skewed + AADT1000 + stops_qtmi + income_000_qtmi, 
                   ~ s(as.factor(dat3$L2ID), bs="re") + Age_ChildTeen + RTQueue + TEMP_8095 + PEDCP_ped_FDW + PEDCP_ped_SDW + CWType_Cont + RT_LNS_2 + CHANNEL + income_000_qtmi), 
              data=dat3, family=multinom(K=2))
summary(mod03d)
lrtest(mod03d, mod03c) # okay to remove OffRamp
mod03e <-  gam(list(ReactPed0 
                    ~ s(as.factor(dat3$L2ID), bs="re") + OC_Bicycle + CWNum1 + CDirAppr + RTQueue + PEDCP_ped_SDW + VEHCP_veh_R + RT_LNS_2 + Skewed + AADT1000 + stops_qtmi + income_000_qtmi, 
                    ~ s(as.factor(dat3$L2ID), bs="re") + Age_ChildTeen + RTQueue + TEMP_8095 + PEDCP_ped_FDW + PEDCP_ped_SDW + CWType_Cont + RT_LNS_2 + CHANNEL + income_000_qtmi), 
               data=dat3, family=multinom(K=2))
summary(mod03e)
lrtest(mod03e, mod03d) # but okay to remove Gender_Female
mod03f <- gam(list(ReactPed0 
                   ~ s(as.factor(dat3$L2ID), bs="re") + OC_Bicycle + CWNum1 + CDirAppr + RTQueue + PEDCP_ped_SDW + VEHCP_veh_R + RT_LNS_2 + Skewed + AADT1000 + income_000_qtmi, 
                   ~ s(as.factor(dat3$L2ID), bs="re") + Age_ChildTeen + RTQueue + TEMP_8095 + PEDCP_ped_FDW + PEDCP_ped_SDW + CWType_Cont + RT_LNS_2 + CHANNEL + income_000_qtmi), 
              data=dat3, family=multinom(K=2))
summary(mod03f)
lrtest(mod03f, mod03e) # okay to remove stops_qtmi
mod03g <- gam(list(ReactPed0 
                   ~ s(as.factor(dat3$L2ID), bs="re") + OC_Bicycle + CWNum1 + CDirAppr + RTQueue + PEDCP_ped_SDW + VEHCP_veh_R + RT_LNS_2 + Skewed + income_000_qtmi, 
                   ~ s(as.factor(dat3$L2ID), bs="re") + Age_ChildTeen + RTQueue + TEMP_8095 + PEDCP_ped_FDW + PEDCP_ped_SDW + CWType_Cont + RT_LNS_2 + CHANNEL + income_000_qtmi), 
              data=dat3, family=multinom(K=2))
summary(mod03g)
lrtest(mod03g, mod03f) # not okay to remove AADT1000
mod03h <- gam(list(ReactPed0 
                   ~ s(as.factor(dat3$L2ID), bs="re") + OC_Bicycle + CWNum1 + CDirAppr + RTQueue + PEDCP_ped_SDW + VEHCP_veh_R + RT_LNS_2 + Skewed + income_000_qtmi, 
                   ~ s(as.factor(dat3$L2ID), bs="re") + Age_ChildTeen + RTQueue + TEMP_8095 + PEDCP_ped_FDW + PEDCP_ped_SDW + CWType_Cont + RT_LNS_2 + CHANNEL), 
              data=dat3, family=multinom(K=2))
summary(mod03h)
lrtest(mod03h, mod03g) # okay to remove income_000_qtmi
mod03i <- gam(list(ReactPed0 
                   ~ s(as.factor(dat3$L2ID), bs="re") + OC_Bicycle + CWNum1 + CDirAppr + RTQueue + PEDCP_ped_SDW + VEHCP_veh_R + RT_LNS_2 + Skewed + income_000_qtmi, 
                   ~ s(as.factor(dat3$L2ID), bs="re") + Age_ChildTeen + RTQueue + TEMP_8095 + PEDCP_ped_FDW + PEDCP_ped_SDW + CWType_Cont + CHANNEL), 
              data=dat3, family=multinom(K=2))
summary(mod03i)
lrtest(mod03i, mod03h) # okay to remove RT_LNS_2
mod03j <- gam(list(ReactPed0 
                   ~ s(as.factor(dat3$L2ID), bs="re") + OC_Bicycle + CDirAppr + RTQueue + PEDCP_ped_SDW + VEHCP_veh_R + RT_LNS_2 + Skewed + income_000_qtmi, 
                   ~ s(as.factor(dat3$L2ID), bs="re") + Age_ChildTeen + RTQueue + TEMP_8095 + PEDCP_ped_FDW + PEDCP_ped_SDW + CWType_Cont + CHANNEL), 
              data=dat3, family=multinom(K=2))
summary(mod03j)
lrtest(mod03j, mod03i) # okay to remove CWNum1
mod03k <- gam(list(ReactPed0 
                   ~ s(as.factor(dat3$L2ID), bs="re") + CDirAppr + RTQueue + PEDCP_ped_SDW + VEHCP_veh_R + RT_LNS_2 + Skewed + income_000_qtmi, 
                   ~ s(as.factor(dat3$L2ID), bs="re") + Age_ChildTeen + RTQueue + TEMP_8095 + PEDCP_ped_FDW + PEDCP_ped_SDW + CWType_Cont + CHANNEL), 
              data=dat3, family=multinom(K=2))
summary(mod03k)
lrtest(mod03k, mod03j) # okay to remove OC_Bicycle
mod03l <- gam(list(ReactPed0 
                   ~ s(as.factor(dat3$L2ID), bs="re") + CDirAppr + RTQueue + PEDCP_ped_SDW + VEHCP_veh_R + RT_LNS_2 + income_000_qtmi, 
                   ~ s(as.factor(dat3$L2ID), bs="re") + Age_ChildTeen + RTQueue + TEMP_8095 + PEDCP_ped_FDW + PEDCP_ped_SDW + CWType_Cont + CHANNEL), 
              data=dat3, family=multinom(K=2))
summary(mod03l)
lrtest(mod03l, mod03k) # not okay to remove Skewed
mod03m <- gam(list(ReactPed0 
                   ~ s(as.factor(dat3$L2ID), bs="re") + CDirAppr + RTQueue + PEDCP_ped_SDW + VEHCP_veh_R + RT_LNS_2 + Skewed + income_000_qtmi, 
                   ~ s(as.factor(dat3$L2ID), bs="re") + Age_ChildTeen + RTQueue + TEMP_8095 + PEDCP_ped_FDW + PEDCP_ped_SDW + CHANNEL), 
              data=dat3, family=multinom(K=2))
summary(mod03m)
lrtest(mod03m, mod03k) # okay to remove CWType_Cont
mod03n <- gam(list(ReactPed0 
                   ~ s(as.factor(dat3$L2ID), bs="re") + CDirAppr + RTQueue + PEDCP_ped_SDW + VEHCP_veh_R + RT_LNS_2 + income_000_qtmi, 
                   ~ s(as.factor(dat3$L2ID), bs="re") + Age_ChildTeen + RTQueue + TEMP_8095 + PEDCP_ped_FDW + PEDCP_ped_SDW + CHANNEL), 
              data=dat3, family=multinom(K=2))
summary(mod03n)
lrtest(mod03n, mod03m) # not okay to remove Skewed
mod03o <- gam(list(ReactPed0 
                   ~ s(as.factor(dat3$L2ID), bs="re") + CDirAppr + RTQueue + PEDCP_ped_SDW + RT_LNS_2 + Skewed + income_000_qtmi, 
                   ~ s(as.factor(dat3$L2ID), bs="re") + Age_ChildTeen + RTQueue + TEMP_8095 + PEDCP_ped_FDW + PEDCP_ped_SDW + CHANNEL), 
              data=dat3, family=multinom(K=2))
summary(mod03o)
lrtest(mod03o, mod03m) # okay to remove VEHCP_veh_R
mod03p <- gam(list(ReactPed0 
                   ~ s(as.factor(dat3$L2ID), bs="re") + CDirAppr + RTQueue + PEDCP_ped_SDW + RT_LNS_2 + income_000_qtmi, 
                   ~ s(as.factor(dat3$L2ID), bs="re") + Age_ChildTeen + RTQueue + TEMP_8095 + PEDCP_ped_FDW + PEDCP_ped_SDW + CHANNEL), 
              data=dat3, family=multinom(K=2))
summary(mod03p)
lrtest(mod03p, mod03o) # not okay to remove Skewed --> remove anyway b/c not significant
rm(mod03a, mod03b, mod03c, mod03d, mod03e, mod03f, mod03g, mod03h, mod03i, mod03j, mod03k, mod03l, mod03m, mod03n, mod03o, mod03p)
# final specification
mod03 <- gam(list(ReactPed0 
                  ~ s(as.factor(dat3$L2ID), bs="re") + CDirAppr + RTQueue + PEDCP_ped_SDW + RT_LNS_2 + income_000_qtmi, 
                  ~ s(as.factor(dat3$L2ID), bs="re") + Age_ChildTeen + RTQueue + TEMP_8095 + PEDCP_ped_FDW + PEDCP_ped_SDW + CHANNEL), 
             data=dat3, family=multinom(K=2))
summary(mod03)
lrtest(mod03, mod02) # better with new IVs

# Summarize, combine, and save all models
summary(mod01)
summary(mod02)
summary(mod03)
variance_comp(mod03)
getLLmnl(mod01); getLLmnl(mod02); getLLmnl(mod03);
mods3 <- list(dat3, mod01, mod02, mod03)
names(mods3) <- c("dat3", "mod01", "mod02", "mod03")
saveRDS(mods3, file=file.path("Analysis", "Conflict Analysis", "mods3.rds"))
# Cleanup
rm(mod01, mod02, mod03)
rm(mods3, dat3)
gc()

########## Driver reaction ##########
# Multilevel Multinomial logit model
# 4. Driver reaction

# Create dataset
dat4 <- dat[!is.na(dat$ReactDrv) & dat$CWType_None==F & !is.na(dat$AADT1000),]
summary(dat4)

# Inspect DV
summary(dat4$ReactDrv)
dat4$ReactDrv0 <- as.integer(dat4$ReactDrv) - 1
table(dat4$ReactDrv0)

# One-level intercept-only model
mod01 <- gam(list(ReactDrv0 
                  ~ 1, 
                  ~ 1), 
             data=dat4, family=multinom(K=2))
summary(mod01)

# Multilevel model (empty) with random intercept
mod02 <- gam(list(ReactDrv0 
                  ~ 1 + s(as.factor(dat4$L2ID), bs="re"), 
                  ~ 1 + s(as.factor(dat4$L2ID), bs="re")), 
             data=dat4, family=multinom(K=2))
summary(mod02)
lrtest(mod02, mod01) # better with random intercept

# Try independent variables, one at a time
# a/b. Pedestrian, driver, vehicle characteristics (ivsa, ivsb)
# sig: Gender_Female + OC_StrWhe + CDirAppr + RTQueue
# sig1: Gender_Female + OC_StrWhe + CWNum1 + CDirAppr
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + GroupSize, 
                ~ s(as.factor(dat4$L2ID), bs="re") + GroupSize), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + logGroupSize, 
                ~ s(as.factor(dat4$L2ID), bs="re") + logGroupSize), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + Age_ChildTeen, 
                ~ s(as.factor(dat4$L2ID), bs="re") + Age_ChildTeen), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + Gender_Female, 
                ~ s(as.factor(dat4$L2ID), bs="re") + Gender_Female), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + OC_Load + OC_StrWhe + OC_SkaSco + OC_Bicycle + OC_Distracted, 
                ~ s(as.factor(dat4$L2ID), bs="re") + OC_Load + OC_StrWhe + OC_SkaSco + OC_Bicycle + OC_Distracted), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + CWNum1, 
                ~ s(as.factor(dat4$L2ID), bs="re") + CWNum1), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + CDirAppr, 
                ~ s(as.factor(dat4$L2ID), bs="re") + CDirAppr), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + RTQueue, 
                ~ s(as.factor(dat4$L2ID), bs="re") + RTQueue), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + VehType2_Small + VehType2_Large, 
                ~ s(as.factor(dat4$L2ID), bs="re") + VehType2_Small + VehType2_Large), 
           data=dat4, family=multinom(K=2))
summary(mod); rm(mod)
# c/d/e Conflict, weather, time information, traffic signal statuses (ivsc, ivsd, ivse)
# sig: VEHCP_veh_R
# sig.1: TEMP_6580 + TOD2_1805 + PEDCP_ped_SDW
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + TEMP, 
                ~ s(as.factor(dat4$L2ID), bs="re") + TEMP), 
           data=dat4, family=multinom(K=2)); BIC(mod)
# mod <- gam(list(ReactDrv0 
#                 ~ s(as.factor(dat4$L2ID), bs="re") + TEMP_5065 + TEMP_6580 + TEMP_8095, 
#                 ~ s(as.factor(dat4$L2ID), bs="re") + TEMP_5065 + TEMP_6580 + TEMP_8095), 
#            data=dat4, family=multinom(K=2)); BIC(mod) # did not converge
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + TEMP_5065 + TEMP_6580 + TEMP_8095, 
                ~ s(as.factor(dat4$L2ID), bs="re") + TEMP_5065 + TEMP_6580), 
           data=dat4, family=multinom(K=2)); BIC(mod) # did not converge
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + PRCP_01, 
                ~ s(as.factor(dat4$L2ID), bs="re") + PRCP_01), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + Weekday_M_F, 
                ~ s(as.factor(dat4$L2ID), bs="re") + Weekday_M_F), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + TOD2_0611 + TOD2_1805, 
                ~ s(as.factor(dat4$L2ID), bs="re") + TOD2_0611 + TOD2_1805), 
           data=dat4, family=multinom(K=2)); BIC(mod)
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + AMPeak + PMPeak, 
                ~ s(as.factor(dat4$L2ID), bs="re") + AMPeak + PMPeak), 
           data=dat4, family=multinom(K=2)); BIC(mod)
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + PEDCP_ped_FDW + PEDCP_ped_SDW + RTNotSig, 
                ~ s(as.factor(dat4$L2ID), bs="re") + PEDCP_ped_FDW + PEDCP_ped_SDW + RTNotSig), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + VEHCP_veh_Y + VEHCP_veh_R + RTNotSig, 
                ~ s(as.factor(dat4$L2ID), bs="re") + VEHCP_veh_Y + VEHCP_veh_R + RTNotSig), 
           data=dat4, family=multinom(K=2))
summary(mod); rm(mod)
# f. Corner & intersection attributes (ivsf)
# sig: REC_LNS_1 + AADT1000
# sig.1: (none)
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + CURB_RAD, 
                ~ s(as.factor(dat4$L2ID), bs="re") + CURB_RAD), 
           data=dat4, family=multinom(K=2)) # Did not Converge
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + CW_DIST + CHANNEL, 
                ~ s(as.factor(dat4$L2ID), bs="re") + CW_DIST + CHANNEL), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + SB_DIST + CHANNEL, 
                ~ s(as.factor(dat4$L2ID), bs="re") + SB_DIST + CHANNEL), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + RAMPS_2, 
                ~ s(as.factor(dat4$L2ID), bs="re") + RAMPS_2), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + CRType_Blen + CRType_Dire, 
                ~ s(as.factor(dat4$L2ID), bs="re") + CRType_Blen + CRType_Dire), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + CWType_Cont, 
                ~ s(as.factor(dat4$L2ID), bs="re") + CWType_Cont), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + RT_LNS_05 + RT_LNS_2, 
                ~ s(as.factor(dat4$L2ID), bs="re") + RT_LNS_05 + RT_LNS_2), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + REC_LNS_1, 
                ~ s(as.factor(dat4$L2ID), bs="re") + REC_LNS_1), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + CHANNEL, 
                ~ s(as.factor(dat4$L2ID), bs="re") + CHANNEL), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + Skewed, 
                ~ s(as.factor(dat4$L2ID), bs="re") + Skewed), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + BIKE_LN, 
                ~ s(as.factor(dat4$L2ID), bs="re") + BIKE_LN), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + AADP0100, 
                ~ s(as.factor(dat4$L2ID), bs="re") + AADP0100), 
           data=dat4, family=multinom(K=2)); BIC(mod)
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + logAADP, 
                ~ s(as.factor(dat4$L2ID), bs="re") + logAADP), 
           data=dat4, family=multinom(K=2)); BIC(mod)
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + AADT1000, 
                ~ s(as.factor(dat4$L2ID), bs="re") + AADT1000), 
           data=dat4, family=multinom(K=2)); BIC(mod)
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + logAADT, 
                ~ s(as.factor(dat4$L2ID), bs="re") + logAADT), 
           data=dat4, family=multinom(K=2)); BIC(mod)
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + RTNotSig, 
                ~ s(as.factor(dat4$L2ID), bs="re") + RTNotSig), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + OnRamp + OffRamp, 
                ~ s(as.factor(dat4$L2ID), bs="re") + OnRamp + OffRamp), 
           data=dat4, family=multinom(K=2))
summary(mod); rm(mod)
# g. Neighborhood attributes (ivsg)
# sig: (none)
# sig.1: (none)
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + popden_000_qtmi, 
                ~ s(as.factor(dat4$L2ID), bs="re") + popden_000_qtmi), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + empden_000_qtmi, 
                ~ s(as.factor(dat4$L2ID), bs="re") + empden_000_qtmi), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + per_res_qtmi, 
                ~ s(as.factor(dat4$L2ID), bs="re") + per_res_qtmi), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + per_com_qtmi, 
                ~ s(as.factor(dat4$L2ID), bs="re") + per_com_qtmi), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + per_ind_qtmi, 
                ~ s(as.factor(dat4$L2ID), bs="re") + per_ind_qtmi), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + per_vac_qtmi, 
                ~ s(as.factor(dat4$L2ID), bs="re") + per_vac_qtmi), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + per_other_qtmi, 
                ~ s(as.factor(dat4$L2ID), bs="re") + per_other_qtmi), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + intden_qtmi, 
                ~ s(as.factor(dat4$L2ID), bs="re") + intden_qtmi), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + per4wy_qtmi, 
                ~ s(as.factor(dat4$L2ID), bs="re") + per4wy_qtmi), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + stops_qtmi, 
                ~ s(as.factor(dat4$L2ID), bs="re") + stops_qtmi), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + worship_qtmi, 
                ~ s(as.factor(dat4$L2ID), bs="re") + worship_qtmi), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + schools_qtmi, 
                ~ s(as.factor(dat4$L2ID), bs="re") + schools_qtmi), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + park_acre_qtmi, 
                ~ s(as.factor(dat4$L2ID), bs="re") + park_acre_qtmi), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + income_000_qtmi, 
                ~ s(as.factor(dat4$L2ID), bs="re") + income_000_qtmi), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + avgveh_qtmi, 
                ~ s(as.factor(dat4$L2ID), bs="re") + avgveh_qtmi), 
           data=dat4, family=multinom(K=2))
mod <- gam(list(ReactDrv0 
                ~ s(as.factor(dat4$L2ID), bs="re") + hhsize_qtmi, 
                ~ s(as.factor(dat4$L2ID), bs="re") + hhsize_qtmi), 
           data=dat4, family=multinom(K=2))
summary(mod); rm(mod)

# Try all significant IVs altogether, backwards removal
mod03a <- gam(list(ReactDrv0 
                   ~ s(as.factor(dat4$L2ID), bs="re") + Gender_Female + OC_StrWhe + CDirAppr + RTQueue + VEHCP_veh_R + REC_LNS_1 + AADT1000, 
                   ~ s(as.factor(dat4$L2ID), bs="re") + Gender_Female + OC_StrWhe + CWNum1 + CDirAppr + TEMP_6580 + TOD2_1805 + PEDCP_ped_SDW), 
              data=dat4, family=multinom(K=2))
summary(mod03a)
mod03b <- gam(list(ReactDrv0 
                   ~ s(as.factor(dat4$L2ID), bs="re") + Gender_Female + OC_StrWhe + CDirAppr + RTQueue + VEHCP_veh_R + REC_LNS_1 + AADT1000, 
                   ~ s(as.factor(dat4$L2ID), bs="re") + OC_StrWhe + CWNum1 + CDirAppr + TEMP_6580 + TOD2_1805 + PEDCP_ped_SDW), 
              data=dat4, family=multinom(K=2))
summary(mod03b)
lrtest(mod03b, mod03a) # okay to remove Gender_Female
mod03c <- gam(list(ReactDrv0 
                   ~ s(as.factor(dat4$L2ID), bs="re") + OC_StrWhe + CDirAppr + RTQueue + VEHCP_veh_R + REC_LNS_1 + AADT1000, 
                   ~ s(as.factor(dat4$L2ID), bs="re") + OC_StrWhe + CWNum1 + CDirAppr + TEMP_6580 + TOD2_1805 + PEDCP_ped_SDW), 
              data=dat4, family=multinom(K=2))
summary(mod03c)
lrtest(mod03c, mod03b) # okay to remove Gender_Female
mod03d <- gam(list(ReactDrv0 
                   ~ s(as.factor(dat4$L2ID), bs="re") + OC_StrWhe + CDirAppr + RTQueue + VEHCP_veh_R + REC_LNS_1 + AADT1000, 
                   ~ s(as.factor(dat4$L2ID), bs="re") + OC_StrWhe + CDirAppr + TEMP_6580 + TOD2_1805 + PEDCP_ped_SDW), 
              data=dat4, family=multinom(K=2))
summary(mod03d)
lrtest(mod03c, mod03b) # okay to remove CWNum1
mod03d <- gam(list(ReactDrv0 
                   ~ s(as.factor(dat4$L2ID), bs="re") + OC_StrWhe + CDirAppr + RTQueue + VEHCP_veh_R + REC_LNS_1, 
                   ~ s(as.factor(dat4$L2ID), bs="re") + OC_StrWhe + CDirAppr + TEMP_6580 + TOD2_1805 + PEDCP_ped_SDW), 
              data=dat4, family=multinom(K=2))
summary(mod03d)
lrtest(mod03d, mod03c) # Not okay to remove AADT1000
rm(mod03a, mod03b, mod03c, mod03d)
# final specification
mod03 <- gam(list(ReactDrv0 
                  ~ s(as.factor(dat4$L2ID), bs="re") + OC_StrWhe + CDirAppr + RTQueue + VEHCP_veh_R + REC_LNS_1 + AADT1000, 
                  ~ s(as.factor(dat4$L2ID), bs="re") + OC_StrWhe + CDirAppr + TEMP_6580 + TOD2_1805 + PEDCP_ped_SDW), 
             data=dat4, family=multinom(K=2))
summary(mod03)
lrtest(mod03, mod02) # better with new IVs

# Summarize, combine, and save all models
summary(mod01)
summary(mod02)
summary(mod03)
variance_comp(mod03)
getLLmnl(mod01); getLLmnl(mod02); getLLmnl(mod03)
mods4 <- list(dat4, mod01, mod02, mod03)
names(mods4) <- c("dat4", "mod01", "mod02", "mod03")
saveRDS(mods4, file=file.path("Analysis", "Conflict Analysis", "mods4.rds"))
# Cleanup
rm(mod01, mod02, mod03)
rm(mods4, dat4)
gc()

########## Pedestrian crossing location ##########
# Multilevel binary logit regression model
# 5. Pedestrian crossing location

# Create dataset
dat5 <- dat[!is.na(dat$LocCross) & dat$CWType_None==F & !is.na(dat$AADT1000),]

# Inspect DV
summary(dat5$LocCross)
dat5$CrossAway <- dat5$LocCross=="Away"
table(dat5$CrossAway)

# One-level intercept-only model
mod01 <- glm(CrossAway ~ 1, data=dat5, family=binomial)
summary(mod01)

# Multilevel model (empty) with random intercept
mod02 <- glmer(CrossAway ~ 1 + (1 | L2ID), data=dat5, family=binomial)
summary(mod02)
anova(mod02, mod01) # better with random intercept

# Try independent variables, one at a time
# a/b. Pedestrian, driver, vehicle characteristics (ivsa, ivsb)
# sig: OC_Bicycle
mod <- update(mod02, formula= . ~ . + GroupSize)
mod <- update(mod02, formula= . ~ . + logGroupSize)
mod <- update(mod02, formula= . ~ . + Age_ChildTeen)
mod <- update(mod02, formula= . ~ . + Gender_Female)
# mod <- update(mod02, formula= . ~ . + OC_Load + OC_StrWhe + OC_SkaSco + OC_Bicycle + OC_Distracted) # doesn't converge
mod <- update(mod02, formula= . ~ . + OC_Load + OC_Bicycle)
mod <- update(mod02, formula= . ~ . + CWNum1)
mod <- update(mod02, formula= . ~ . + CDirAppr)
mod <- update(mod02, formula= . ~ . + RTQueue)
mod <- update(mod02, formula= . ~ . + VehType2_Small + VehType2_Large)
summary(mod); rm(mod)
# c/d/e Conflict, weather, time information, traffic signal statuses (ivsc, ivsd, ivse)
# sig: PEDCP_ped_FDW + PEDCP_ped_SDW + VEHCP_veh_R + RTNotSig
mod <- update(mod02, formula= . ~ . + TEMP); BIC(mod)
mod <- update(mod02, formula= . ~ . + TEMP_5065 + TEMP_6580 + TEMP_8095); BIC(mod)
# mod <- update(mod02, formula= . ~ . + PRCP_01) # doesn't converge
mod <- update(mod02, formula= . ~ . + Weekday_M_F)
mod <- update(mod02, formula= . ~ . + TOD2_0611 + TOD2_1805); BIC(mod)
mod <- update(mod02, formula= . ~ . + AMPeak + PMPeak); BIC(mod)
mod <- update(mod02, formula= . ~ . + PEDCP_ped_FDW + PEDCP_ped_SDW + RTNotSig)
mod <- update(mod02, formula= . ~ . + VEHCP_veh_Y + VEHCP_veh_R + RTNotSig)
summary(mod); rm(mod)
# f. Corner & intersection attributes (ivsf)
# sig: CURB_RAD + CRType_Dire + RTNotSig
mod <- update(mod02, formula= . ~ . + CURB_RAD)
mod <- update(mod02, formula= . ~ . + CW_DIST + CHANNEL)
mod <- update(mod02, formula= . ~ . + SB_DIST + CHANNEL)
mod <- update(mod02, formula= . ~ . + RAMPS_2)
mod <- update(mod02, formula= . ~ . + CRType_Blen + CRType_Dire)
mod <- update(mod02, formula= . ~ . + CWType_Cont)
# mod <- update(mod02, formula= . ~ . + RT_LNS_05 + RT_LNS_2) # doesn't converge
mod <- update(mod02, formula= . ~ . + RT_LNS_05)
mod <- update(mod02, formula= . ~ . + REC_LNS_1)
mod <- update(mod02, formula= . ~ . + CHANNEL)
mod <- update(mod02, formula= . ~ . + Skewed)
mod <- update(mod02, formula= . ~ . + BIKE_LN)
mod <- update(mod02, formula= . ~ . + AADP0100); BIC(mod)
mod <- update(mod02, formula= . ~ . + logAADP); BIC(mod)
mod <- update(mod02, formula= . ~ . + AADT1000); BIC(mod)
mod <- update(mod02, formula= . ~ . + logAADT); BIC(mod)
mod <- update(mod02, formula= . ~ . + RTNotSig)
# mod <- update(mod02, formula= . ~ . + OnRamp + OffRamp) # doesn't converge
mod <- update(mod02, formula= . ~ . + OnRamp)
summary(mod); rm(mod)
# g. Neighborhood attributes (ivsg)
# sig: none
mod <- update(mod02, formula= . ~ . + popden_000_qtmi)
mod <- update(mod02, formula= . ~ . + empden_000_qtmi)
mod <- update(mod02, formula= . ~ . + per_res_qtmi)
mod <- update(mod02, formula= . ~ . + per_com_qtmi)
mod <- update(mod02, formula= . ~ . + per_ind_qtmi)
mod <- update(mod02, formula= . ~ . + per_vac_qtmi)
mod <- update(mod02, formula= . ~ . + per_other_qtmi)
mod <- update(mod02, formula= . ~ . + intden_qtmi)
mod <- update(mod02, formula= . ~ . + per4wy_qtmi)
mod <- update(mod02, formula= . ~ . + stops_qtmi)
mod <- update(mod02, formula= . ~ . + worship_qtmi)
mod <- update(mod02, formula= . ~ . + schools_qtmi)
mod <- update(mod02, formula= . ~ . + park_acre_qtmi)
mod <- update(mod02, formula= . ~ . + income_000_qtmi)
mod <- update(mod02, formula= . ~ . + avgveh_qtmi)
mod <- update(mod02, formula= . ~ . + hhsize_qtmi)
summary(mod); rm(mod)

# Try all significant IVs altogether, backwards removal
mod03a <- update(mod02, formula= . ~ . + OC_Bicycle + PEDCP_ped_FDW + PEDCP_ped_SDW + VEHCP_veh_R + CURB_RAD + CRType_Dire + RTNotSig)
summary(mod03a)
mod03b <- update(mod03a, formula= . ~ . - CRType_Dire)
summary(mod03b)
anova(mod03b, mod03a) # okay to remove CRType_Dire
mod03c <- update(mod03b, formula= . ~ . - VEHCP_veh_R)
summary(mod03c)
anova(mod03c, mod03b) # okay to remove VEHCP_veh_R
mod03d <- update(mod03c, formula= . ~ . - CURB_RAD)
summary(mod03d)
anova(mod03d, mod03c) # okay to remove CURB_RAD
rm(mod03a, mod03b, mod03c, mod03d)
# final specification
mod03 <- update(mod02, formula= . ~ . + OC_Bicycle + PEDCP_ped_FDW + PEDCP_ped_SDW + RTNotSig)
summary(mod03)
anova(mod03, mod02) # better with new IVs

# Summarize, combine, and save all models
summary(mod01)
summary(mod02)
summary(mod03)
logLik(mod01); logLik(mod02); logLik(mod03)
mods5 <- list(dat5, mod01, mod02, mod03)
names(mods5) <- c("dat5", "mod01", "mod02", "mod03")
saveRDS(mods5, file=file.path("Analysis", "Conflict Analysis", "mods5.rds"))
# Cleanup
rm(mod01, mod02, mod03)
rm(mods5, dat5)
gc()

########## Driver stopping location ##########
# Multilevel multinomial logit regression model
# 6. Driver stopping location

# Create dataset
dat6 <- dat[!is.na(dat$Loc_Stop) & dat$CWType_None==F & !is.na(dat$AADT1000),]
summary(dat6)

# Inspect DV
summary(dat6$Loc_Stop)
dat6$Loc_Stop0 <- as.integer(dat6$Loc_Stop) - 1
table(dat6$Loc_Stop0)

# One-level intercept-only model
mod01 <- gam(list(Loc_Stop0 
                  ~ 1, 
                  ~ 1), 
             data=dat6, family=multinom(K=2))
summary(mod01)

# Multilevel model (empty) with random intercept
mod02 <- gam(list(Loc_Stop0 
                  ~ 1 + s(as.factor(dat6$L2ID), bs="re"), 
                  ~ 1 + s(as.factor(dat6$L2ID), bs="re")), 
             data=dat6, family=multinom(K=2))
summary(mod02)
lrtest(mod02, mod01) # better with random intercept

# Try independent variables, one at a time
# a/b. Pedestrian, driver, vehicle characteristics (ivsa, ivsb)
# sig: logGroupSize + CWNum1 + RTQueue + VehType2_Large
# sig1: CWNum1 + CDirAppr + RTQueue + VehType2_Large
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + GroupSize, 
                ~ s(as.factor(dat6$L2ID), bs="re") + GroupSize), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize, 
                ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + Age_ChildTeen, 
                ~ s(as.factor(dat6$L2ID), bs="re") + Age_ChildTeen), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + Gender_Female, 
                ~ s(as.factor(dat6$L2ID), bs="re") + Gender_Female), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + OC_Load + OC_StrWhe + OC_SkaSco + OC_Bicycle + OC_Distracted, 
                ~ s(as.factor(dat6$L2ID), bs="re") + OC_Load + OC_StrWhe + OC_SkaSco + OC_Bicycle + OC_Distracted), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + CWNum1, 
                ~ s(as.factor(dat6$L2ID), bs="re") + CWNum1), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + CDirAppr, 
                ~ s(as.factor(dat6$L2ID), bs="re") + CDirAppr), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + RTQueue, 
                ~ s(as.factor(dat6$L2ID), bs="re") + RTQueue), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + VehType2_Small + VehType2_Large, 
                ~ s(as.factor(dat6$L2ID), bs="re") + VehType2_Small + VehType2_Large), 
           data=dat6, family=multinom(K=2))
summary(mod); rm(mod)
# c/d/e Conflict, weather, time information, traffic signal statuses (ivsc, ivsd, ivse)
# sig: TEMP_5065 + VEHCP_veh_R + RTNotSig
# sig.1: Weekday_M_F + TOD2_1805 + AMPeak + VEHCP_veh_R
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + TEMP, 
                ~ s(as.factor(dat6$L2ID), bs="re") + TEMP), 
           data=dat6, family=multinom(K=2)); BIC(mod)
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + TEMP_5065 + TEMP_6580 + TEMP_8095, 
                ~ s(as.factor(dat6$L2ID), bs="re") + TEMP_5065 + TEMP_6580 + TEMP_8095), 
           data=dat6, family=multinom(K=2)); BIC(mod)
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + PRCP_01, 
                ~ s(as.factor(dat6$L2ID), bs="re") + PRCP_01), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + Weekday_M_F, 
                ~ s(as.factor(dat6$L2ID), bs="re") + Weekday_M_F), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + TOD2_0611 + TOD2_1805, 
                ~ s(as.factor(dat6$L2ID), bs="re") + TOD2_0611 + TOD2_1805), 
           data=dat6, family=multinom(K=2)); BIC(mod)
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + AMPeak + PMPeak, 
                ~ s(as.factor(dat6$L2ID), bs="re") + AMPeak + PMPeak), 
           data=dat6, family=multinom(K=2)); BIC(mod)
# mod <- gam(list(Loc_Stop0 
#                 ~ s(as.factor(dat6$L2ID), bs="re") + PEDCP_ped_FDW + PEDCP_ped_SDW + RTNotSig, 
#                 ~ s(as.factor(dat6$L2ID), bs="re") + PEDCP_ped_FDW + PEDCP_ped_SDW + RTNotSig), 
#            data=dat6, family=multinom(K=2)) # did not converge
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + PEDCP_ped_FDW + PEDCP_ped_SDW + RTNotSig, 
                ~ s(as.factor(dat6$L2ID), bs="re") + PEDCP_ped_FDW + PEDCP_ped_SDW), 
           data=dat6, family=multinom(K=2))
# mod <- gam(list(Loc_Stop0 
#                 ~ s(as.factor(dat6$L2ID), bs="re") + VEHCP_veh_Y + VEHCP_veh_R + RTNotSig, 
#                 ~ s(as.factor(dat6$L2ID), bs="re") + VEHCP_veh_Y + VEHCP_veh_R + RTNotSig), 
#            data=dat6, family=multinom(K=2)) # did not converge
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + VEHCP_veh_Y + VEHCP_veh_R + RTNotSig, 
                ~ s(as.factor(dat6$L2ID), bs="re") + VEHCP_veh_Y + VEHCP_veh_R), 
           data=dat6, family=multinom(K=2))
summary(mod); rm(mod)
# f. Corner & intersection attributes (ivsf)
# sig: CURB_RAD + CRType_Dire + RT_LNS_2 + REC_LNS_1 + logAADP + AADT1000 + RTNotSig
# sig.1: CURB_RAD + SB_DIST + CRType_Dire + CRType_Blen + REC_LNS_1 + CHANNEL + Skewed + logAADP + OnRamp
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + CURB_RAD, 
                ~ s(as.factor(dat6$L2ID), bs="re") + CURB_RAD), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + CW_DIST + CHANNEL, 
                ~ s(as.factor(dat6$L2ID), bs="re") + CW_DIST + CHANNEL), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + SB_DIST + CHANNEL, 
                ~ s(as.factor(dat6$L2ID), bs="re") + SB_DIST + CHANNEL), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + RAMPS_2, 
                ~ s(as.factor(dat6$L2ID), bs="re") + RAMPS_2), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + CRType_Blen + CRType_Dire, 
                ~ s(as.factor(dat6$L2ID), bs="re") + CRType_Blen + CRType_Dire), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + CWType_Cont, 
                ~ s(as.factor(dat6$L2ID), bs="re") + CWType_Cont), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + RT_LNS_05 + RT_LNS_2, 
                ~ s(as.factor(dat6$L2ID), bs="re") + RT_LNS_05 + RT_LNS_2), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + REC_LNS_1, 
                ~ s(as.factor(dat6$L2ID), bs="re") + REC_LNS_1), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + CHANNEL, 
                ~ s(as.factor(dat6$L2ID), bs="re") + CHANNEL), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + Skewed, 
                ~ s(as.factor(dat6$L2ID), bs="re") + Skewed), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + BIKE_LN, 
                ~ s(as.factor(dat6$L2ID), bs="re") + BIKE_LN), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + AADP0100, 
                ~ s(as.factor(dat6$L2ID), bs="re") + AADP0100), 
           data=dat6, family=multinom(K=2)); BIC(mod)
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + logAADP, 
                ~ s(as.factor(dat6$L2ID), bs="re") + logAADP), 
           data=dat6, family=multinom(K=2)); BIC(mod)
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + AADT1000, 
                ~ s(as.factor(dat6$L2ID), bs="re") + AADT1000), 
           data=dat6, family=multinom(K=2)); BIC(mod)
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + logAADT, 
                ~ s(as.factor(dat6$L2ID), bs="re") + logAADT), 
           data=dat6, family=multinom(K=2)); BIC(mod)
# mod <- gam(list(Loc_Stop0 
#                 ~ s(as.factor(dat6$L2ID), bs="re") + RTNotSig, 
#                 ~ s(as.factor(dat6$L2ID), bs="re") + RTNotSig), 
#            data=dat6, family=multinom(K=2)) # does not converge
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + RTNotSig, 
                ~ s(as.factor(dat6$L2ID), bs="re")), 
          data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + OnRamp + OffRamp, 
                ~ s(as.factor(dat6$L2ID), bs="re") + OnRamp + OffRamp), 
           data=dat6, family=multinom(K=2))
summary(mod); rm(mod)
# g. Neighborhood attributes (ivsg)
# sig: per_com_qtmi + worship_qtmi + avgveh_qtmi
# sig.1: popden_000_qtmi + per_vac_qtmi + stops_qtmi + worship_qtmi + income_000_qtmi
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + popden_000_qtmi, 
                ~ s(as.factor(dat6$L2ID), bs="re") + popden_000_qtmi), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + empden_000_qtmi, 
                ~ s(as.factor(dat6$L2ID), bs="re") + empden_000_qtmi), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + per_res_qtmi, 
                ~ s(as.factor(dat6$L2ID), bs="re") + per_res_qtmi), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + per_com_qtmi, 
                ~ s(as.factor(dat6$L2ID), bs="re") + per_com_qtmi), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + per_ind_qtmi, 
                ~ s(as.factor(dat6$L2ID), bs="re") + per_ind_qtmi), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + per_vac_qtmi, 
                ~ s(as.factor(dat6$L2ID), bs="re") + per_vac_qtmi), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + per_other_qtmi, 
                ~ s(as.factor(dat6$L2ID), bs="re") + per_other_qtmi), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + intden_qtmi, 
                ~ s(as.factor(dat6$L2ID), bs="re") + intden_qtmi), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + per4wy_qtmi, 
                ~ s(as.factor(dat6$L2ID), bs="re") + per4wy_qtmi), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + stops_qtmi, 
                ~ s(as.factor(dat6$L2ID), bs="re") + stops_qtmi), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + worship_qtmi, 
                ~ s(as.factor(dat6$L2ID), bs="re") + worship_qtmi), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + schools_qtmi, 
                ~ s(as.factor(dat6$L2ID), bs="re") + schools_qtmi), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + park_acre_qtmi, 
                ~ s(as.factor(dat6$L2ID), bs="re") + park_acre_qtmi), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + income_000_qtmi, 
                ~ s(as.factor(dat6$L2ID), bs="re") + income_000_qtmi), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + avgveh_qtmi, 
                ~ s(as.factor(dat6$L2ID), bs="re") + avgveh_qtmi), 
           data=dat6, family=multinom(K=2))
mod <- gam(list(Loc_Stop0 
                ~ s(as.factor(dat6$L2ID), bs="re") + hhsize_qtmi, 
                ~ s(as.factor(dat6$L2ID), bs="re") + hhsize_qtmi), 
           data=dat6, family=multinom(K=2))
summary(mod); rm(mod)

# Try all significant IVs altogether, backwards removal
mod03a <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + CRType_Dire + RT_LNS_2 + REC_LNS_1 + logAADP + AADT1000 + RTNotSig + per_com_qtmi + worship_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CWNum1 + CDirAppr + RTQueue + VehType2_Large + Weekday_M_F + TOD2_1805 + AMPeak + VEHCP_veh_R + CURB_RAD + SB_DIST + CRType_Dire + CRType_Blen + REC_LNS_1 + CHANNEL + Skewed + logAADP + OnRamp + popden_000_qtmi + per_vac_qtmi + stops_qtmi + worship_qtmi + income_000_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03a)
mod03b <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + CRType_Dire + RT_LNS_2 + REC_LNS_1 + logAADP + AADT1000 + RTNotSig + per_com_qtmi + worship_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CWNum1 + CDirAppr + RTQueue + VehType2_Large + Weekday_M_F + TOD2_1805 + AMPeak + VEHCP_veh_R + CURB_RAD + SB_DIST + CRType_Dire + CRType_Blen + REC_LNS_1 + Skewed + logAADP + OnRamp + popden_000_qtmi + per_vac_qtmi + stops_qtmi + worship_qtmi + income_000_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03b)
lrtest(mod03b, mod03a) # okay to remove CHANNEL
mod03c <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + CRType_Dire + RT_LNS_2 + REC_LNS_1 + logAADP + AADT1000 + RTNotSig + per_com_qtmi + worship_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CWNum1 + CDirAppr + RTQueue + VehType2_Large + Weekday_M_F + TOD2_1805 + AMPeak + VEHCP_veh_R + CURB_RAD + SB_DIST + CRType_Dire + CRType_Blen + REC_LNS_1 + Skewed + logAADP + OnRamp + popden_000_qtmi + per_vac_qtmi + worship_qtmi + income_000_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03c)
lrtest(mod03c, mod03b) # okay to remove stops_qtmi
mod03d <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + CRType_Dire + RT_LNS_2 + REC_LNS_1 + logAADP + AADT1000 + RTNotSig + per_com_qtmi + worship_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CWNum1 + CDirAppr + RTQueue + VehType2_Large + Weekday_M_F + TOD2_1805 + AMPeak + VEHCP_veh_R + CURB_RAD + CRType_Dire + CRType_Blen + REC_LNS_1 + Skewed + logAADP + OnRamp + popden_000_qtmi + per_vac_qtmi + worship_qtmi + income_000_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03d)
lrtest(mod03d, mod03c) # okay to remove SB_DIST
mod03e <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + CRType_Dire + RT_LNS_2 + REC_LNS_1 + logAADP + AADT1000 + per_com_qtmi + worship_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CWNum1 + CDirAppr + RTQueue + VehType2_Large + Weekday_M_F + TOD2_1805 + AMPeak + VEHCP_veh_R + CURB_RAD + CRType_Dire + CRType_Blen + REC_LNS_1 + Skewed + logAADP + OnRamp + popden_000_qtmi + per_vac_qtmi + worship_qtmi + income_000_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03e)
lrtest(mod03e, mod03d) # okay to remove RTNotSig
mod03f <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + CRType_Dire + RT_LNS_2 + REC_LNS_1 + logAADP + AADT1000 + per_com_qtmi + worship_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CWNum1 + CDirAppr + RTQueue + VehType2_Large + Weekday_M_F + TOD2_1805 + AMPeak + VEHCP_veh_R + CURB_RAD + CRType_Dire + CRType_Blen + REC_LNS_1 + Skewed + OnRamp + popden_000_qtmi + per_vac_qtmi + worship_qtmi + income_000_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03f)
lrtest(mod03f, mod03e) # okay to remove logAADP
mod03g <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + CRType_Dire + RT_LNS_2 + REC_LNS_1 + logAADP + AADT1000 + per_com_qtmi + worship_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CWNum1 + CDirAppr + RTQueue + VehType2_Large + Weekday_M_F + TOD2_1805 + AMPeak + VEHCP_veh_R + CURB_RAD + CRType_Blen + REC_LNS_1 + Skewed + OnRamp + popden_000_qtmi + per_vac_qtmi + worship_qtmi + income_000_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03g)
lrtest(mod03g, mod03f) # okay to remove CRType_Dire
mod03h <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + RT_LNS_2 + REC_LNS_1 + logAADP + AADT1000 + per_com_qtmi + worship_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CWNum1 + CDirAppr + RTQueue + VehType2_Large + Weekday_M_F + TOD2_1805 + AMPeak + VEHCP_veh_R + CURB_RAD + CRType_Blen + REC_LNS_1 + Skewed + OnRamp + popden_000_qtmi + per_vac_qtmi + worship_qtmi + income_000_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03h)
lrtest(mod03h, mod03g) # okay to remove CRType_Dire
mod03i <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + RT_LNS_2 + REC_LNS_1 + logAADP + AADT1000 + per_com_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CWNum1 + CDirAppr + RTQueue + VehType2_Large + Weekday_M_F + TOD2_1805 + AMPeak + VEHCP_veh_R + CURB_RAD + CRType_Blen + REC_LNS_1 + Skewed + OnRamp + popden_000_qtmi + per_vac_qtmi + worship_qtmi + income_000_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03i)
lrtest(mod03i, mod03h) # okay to remove worship_qtmi
mod03j <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + RT_LNS_2 + REC_LNS_1 + logAADP + AADT1000 + per_com_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CWNum1 + CDirAppr + RTQueue + VehType2_Large + Weekday_M_F + TOD2_1805 + AMPeak + VEHCP_veh_R + CURB_RAD + CRType_Blen + REC_LNS_1 + Skewed + OnRamp + popden_000_qtmi + per_vac_qtmi + income_000_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03j)
lrtest(mod03j, mod03i) # okay to remove worship_qtmi
mod03k <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + RT_LNS_2 + REC_LNS_1 + logAADP + per_com_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CWNum1 + CDirAppr + RTQueue + VehType2_Large + Weekday_M_F + TOD2_1805 + AMPeak + VEHCP_veh_R + CURB_RAD + CRType_Blen + REC_LNS_1 + Skewed + OnRamp + popden_000_qtmi + per_vac_qtmi + income_000_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03k)
lrtest(mod03k, mod03j) # okay to remove AADT1000
mod03l <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + RT_LNS_2 + REC_LNS_1 + logAADP + per_com_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CWNum1 + CDirAppr + RTQueue + VehType2_Large + Weekday_M_F + TOD2_1805 + AMPeak + VEHCP_veh_R + CURB_RAD + REC_LNS_1 + Skewed + OnRamp + popden_000_qtmi + per_vac_qtmi + income_000_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03l)
lrtest(mod03l, mod03k) # okay to remove CRType_Blen
mod03m <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + RT_LNS_2 + REC_LNS_1 + per_com_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CWNum1 + CDirAppr + RTQueue + VehType2_Large + Weekday_M_F + TOD2_1805 + AMPeak + VEHCP_veh_R + CURB_RAD + REC_LNS_1 + Skewed + OnRamp + popden_000_qtmi + per_vac_qtmi + income_000_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03m)
lrtest(mod03m, mod03l) # okay to remove logAADP
mod03n <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + RT_LNS_2 + REC_LNS_1 + per_com_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CWNum1 + CDirAppr + RTQueue + VehType2_Large + Weekday_M_F + TOD2_1805 + AMPeak + VEHCP_veh_R + CURB_RAD + REC_LNS_1 + Skewed + OnRamp + popden_000_qtmi + per_vac_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03n)
lrtest(mod03n, mod03m) # okay to remove income_000_qtmi
mod03o <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + RT_LNS_2 + REC_LNS_1 + per_com_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CWNum1 + CDirAppr + RTQueue + VehType2_Large + Weekday_M_F + TOD2_1805 + AMPeak + VEHCP_veh_R + REC_LNS_1 + Skewed + OnRamp + popden_000_qtmi + per_vac_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03o)
lrtest(mod03o, mod03n) # okay to remove CURB_RAD
mod03p <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + RT_LNS_2 + REC_LNS_1 + per_com_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CWNum1 + CDirAppr + RTQueue + VehType2_Large + Weekday_M_F + TOD2_1805 + AMPeak + VEHCP_veh_R + Skewed + OnRamp + popden_000_qtmi + per_vac_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03p)
lrtest(mod03p, mod03o) # okay to remove REC_LNS_1
mod03q <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + RT_LNS_2 + REC_LNS_1 + per_com_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CDirAppr + RTQueue + VehType2_Large + Weekday_M_F + TOD2_1805 + AMPeak + VEHCP_veh_R + Skewed + OnRamp + popden_000_qtmi + per_vac_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03q)
lrtest(mod03q, mod03p) # okay to remove CWNum1
mod03r <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + RT_LNS_2 + REC_LNS_1 + per_com_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CDirAppr + RTQueue + VehType2_Large + Weekday_M_F + TOD2_1805 + VEHCP_veh_R + Skewed + OnRamp + popden_000_qtmi + per_vac_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03r)
lrtest(mod03r, mod03q) # okay to remove AMPeak
mod03s <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + RT_LNS_2 + per_com_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CDirAppr + RTQueue + VehType2_Large + Weekday_M_F + TOD2_1805 + VEHCP_veh_R + Skewed + OnRamp + popden_000_qtmi + per_vac_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03s)
lrtest(mod03s, mod03q) # okay to remove REC_LNS_1
mod03t <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + per_com_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CDirAppr + RTQueue + VehType2_Large + Weekday_M_F + TOD2_1805 + VEHCP_veh_R + Skewed + OnRamp + popden_000_qtmi + per_vac_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03t)
lrtest(mod03t, mod03s) # okay to remove RT_LNS_2
mod03u <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + per_com_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CDirAppr + RTQueue + VehType2_Large + Weekday_M_F + TOD2_1805 + VEHCP_veh_R + Skewed + OnRamp + popden_000_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03u)
lrtest(mod03u, mod03t) # okay to remove per_vac_qtmi
mod03v <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + per_com_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CDirAppr + RTQueue + VehType2_Large + TOD2_1805 + VEHCP_veh_R + Skewed + OnRamp + popden_000_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03v)
lrtest(mod03v, mod03u) # okay to remove Weekday_M_F
mod03w <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + per_com_qtmi + avgveh_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CDirAppr + RTQueue + VehType2_Large + TOD2_1805 + VEHCP_veh_R + OnRamp + popden_000_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03w)
lrtest(mod03w, mod03v) # not okay to remove Skewed --> remove anyway b/c not significant
mod03x <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + per_com_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CDirAppr + RTQueue + VehType2_Large + TOD2_1805 + VEHCP_veh_R + OnRamp + popden_000_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03x)
lrtest(mod03x, mod03w) # not okay to remove avgveh_qtmi --> remove anyway b/c not significant
mod03y <- gam(list(Loc_Stop0 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + per_com_qtmi, 
                   ~ s(as.factor(dat6$L2ID), bs="re") + CDirAppr + RTQueue + VehType2_Large + TOD2_1805 + VEHCP_veh_R + OnRamp + popden_000_qtmi), 
              data=dat6, family=multinom(K=2))
summary(mod03y)
lrtest(mod03y, mod03x) # okay to remove logGroupSize --> don't remove
rm(mod03a, mod03b, mod03c, mod03d, mod03e, mod03f, mod03g, mod03h, mod03i, mod03j, mod03k, mod03l, mod03m, mod03n, mod03o, mod03p, mod03q, mod03r, mod03s, mod03t, mod03u, mod03v, mod03w, mod03x, mod03y)
# final specification
mod03 <- gam(list(Loc_Stop0 
                  ~ s(as.factor(dat6$L2ID), bs="re") + logGroupSize + CWNum1 + RTQueue + VehType2_Large + TEMP_5065 + VEHCP_veh_R + CURB_RAD + per_com_qtmi, 
                  ~ s(as.factor(dat6$L2ID), bs="re") + CDirAppr + RTQueue + VehType2_Large + TOD2_1805 + VEHCP_veh_R + OnRamp + popden_000_qtmi), 
             data=dat6, family=multinom(K=2))
summary(mod03)
lrtest(mod03, mod02) # better with new IVs

# Summarize, combine, and save all models
summary(mod01)
summary(mod02)
summary(mod03)
variance_comp(mod03)
getLLmnl(mod01); getLLmnl(mod02); getLLmnl(mod03)
mods6 <- list(dat6, mod01, mod02, mod03)
names(mods6) <- c("dat6", "mod01", "mod02", "mod03")
saveRDS(mods6, file=file.path("Analysis", "Conflict Analysis", "mods6.rds"))
# Cleanup
rm(mod01, mod02, mod03)
rm(mods6, dat6)
gc()

###########################################
# Cleanup

# Remove
rm(ivsa, ivsb, ivsc, ivsd, ivse, ivsf, ivsg)
rm(dat, xdat)
rm(getLLmnl)
gc()

########################################
# END
########################################