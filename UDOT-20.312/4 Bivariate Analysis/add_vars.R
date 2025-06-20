#######################################
# Project:  UDOT-19.312 Right turn safety
# Authors:  Alyssa Gaither (alyssa.gaither@usu.edu)
#           Patrick Singleton (patrick.singleton@usu.edu)
# File:     add_vars.R
# Date:     2023 Spring
# About:    Create variables for bivariate analysis
########################################

# Notes
# - Do not run this script on its own. 
# - It relies upon other data being loaded. 
#   - dat = "datlongcombo.rds"
# - Only run it by calling it from another script. 
#   - table_desc.R
#   - table_corr.R
#   - MLmodels.R

# History
# created from Correlations Table.R
# 2023-02-13 AG updated 
# 2023-03-13 PS updated
# 2023-03-15 PS moved from table_desc.R / MLmodels.R
# 2023-04-10 AS/PS added variables: weather, time, log(exposure)

########################################
# Process level 1 variables

# Inspect
names(dat)

# a. Pedestrian characteristics
summary(dat$Number.of.people.in.group)
dat$GroupSize <- dat$Number.of.people.in.group
dat$logGroupSize <- log(dat$GroupSize)
summary(dat[,grep("Age_", names(dat), value=T)])
dat$Age_Older <- dat$Age_Older_adult_over_65
summary(dat[,grep("Gender_", names(dat), value=T)])
dat$Gender_Unknown <- dat$Gender_Unknown_gender
summary(dat[,grep("OC_", names(dat), value=T)])
dat$OC_Load <- dat$OC_Carrying_load
dat$OC_StrWhe <- ifelse(dat$OC_Stroller | dat$OC_Wheelchair, T, F)
dat$OC_SkaSco <- ifelse(dat$OC_Skateboard | dat$OC_Scooter, T, F)
summary(dat$Crosswalk.ID)
dat$CWNum1 <- dat$Crosswalk.ID=="First crosswalk"
dat$CWNum2 <- dat$Crosswalk.ID=="Second crosswalk"
summary(dat$Crossing.location)
dat$LocCross <- ifelse(dat$Crossing.location %in% c("In the crosswalk or the crosswalk area"), "Crosswalk", 
                ifelse(dat$Crossing.location %in% c("Mid-block, away from the crosswalk", "In the middle of the intersection"), "Away", NA))
dat$LocCross <- factor(dat$LocCross, levels=c("Crosswalk", "Away"))
summary(dat$LocCross)
dat$LocCross_Crosswalk <- dat$LocCross=="Crosswalk"
dat$LocCross_Midblock <- dat$Crossing.location=="Mid-block, away from the crosswalk"
dat$LocCross_Middle <- dat$Crossing.location=="In the middle of the intersection"
dat$LocCross_Away <- dat$LocCross=="Away"
summary(dat$Crossing.direction)
# dat$CDir <- factor(dat$Crossing.direction, levels=c("Leaving Curb", "Approaching Curb"))
dat$CDirLeav <- dat$Crossing.direction=="Leaving Curb"
dat$CDirAppr <- dat$Crossing.direction=="Approaching Curb"
summary(dat$Ped.Reaction)
dat$ReactPed <- ifelse(dat$Ped.Reaction %in% c("No obvious reaction"), "None", 
                ifelse(dat$Ped.Reaction %in% c("Stopped and waited for the vehicle", "Slowed down to avoid collision"), "StopSlow", 
                ifelse(dat$Ped.Reaction %in% c("Sped up to avoid collision", "Ran to avoid collision", "Changed direction"), "Other", NA)))
dat$ReactPed <- factor(dat$ReactPed, levels=c("None", "StopSlow", "Other"))
summary(dat$ReactPed)
dat$ReactPed_None <- dat$ReactPed=="None"
dat$ReactPed_Stop <- dat$Ped.Reaction=="Stopped and waited for the vehicle"
dat$ReactPed_Slow <- dat$Ped.Reaction=="Slowed down to avoid collision"
dat$ReactPed_StopSlow <- dat$ReactPed=="StopSlow"
dat$ReactPed_Spedup <- dat$Ped.Reaction=="Sped up to avoid collision"
dat$ReactPed_Ran <- dat$Ped.Reaction=="Ran to avoid collision"
dat$ReactPed_Cdir <- dat$Ped.Reaction=="Changed direction"
dat$ReactPed_Other <- dat$ReactPed=="Other"

# b. Driver & vehicle characteristics
summary(dat$RT.queue.length)
dat$RTQueue <- as.numeric(dat$RT.queue.length)-1
summary(dat$Stop.location)
dat$Loc_Stop <- ifelse(dat$Stop.location %in% c("Did not stop"), "None", 
                ifelse(dat$Stop.location %in% c("Before the first crosswalk"), "Before", 
                ifelse(dat$Stop.location %in% c("Inside the first crosswalk", "Between the first and second crosswalk"), "InBtwn", NA)))
dat$Loc_Stop <- factor(dat$Loc_Stop, levels=c("None", "Before", "InBtwn"))
summary(dat$Loc_Stop)
dat$Loc_Stop_None <- dat$Loc_Stop=="None"
dat$Loc_Stop_Before <- dat$Loc_Stop=="Before"
dat$Loc_Stop_InBtwn <- dat$Loc_Stop=="InBtwn"
dat$Loc_Stop_Inside1 <- dat$Stop.location=="Inside the first crosswalk"
dat$Loc_Stop_Between <- dat$Stop.location=="Between the first and second crosswalk"
dat$Loc_Stop_Inside2 <- dat$Stop.location=="Inside the second crosswalk"
summary(dat$Reaction.to.conflict)
dat$ReactDrv <- ifelse(dat$Reaction.to.conflict %in% c("No obvious reaction"), "None", 
                ifelse(dat$Reaction.to.conflict %in% c("Driver fully stopped", "Driver slowed down"), "StopSlow", 
                ifelse(dat$Reaction.to.conflict %in% c("Driver sped up", "Driver swerved"), "Other", NA)))
dat$ReactDrv <- factor(dat$ReactDrv, levels=c("None", "StopSlow", "Other"))
summary(dat$ReactDrv)
dat$ReactDrv_None <- dat$ReactDrv=="None"
dat$ReactDrv_StopSlow <- dat$ReactDrv=="StopSlow"
dat$ReactDrv_Stop <- dat$Reaction.to.conflict=="Driver fully stopped"
dat$ReactDrv_Slow <- dat$Reaction.to.conflict=="Driver slowed down"
dat$ReactDrv_Other <- dat$ReactDrv=="Other"
dat$ReactDrv_Spedup <- dat$Reaction.to.conflict=="Driver sped up"
dat$ReactDrv_Swerve <- dat$Reaction.to.conflict=="Driver swerved"
summary(dat$Type)
dat$VehType1 <- ifelse(dat$Type %in% c("Sedan"), "Sedan", 
                ifelse(dat$Type %in% c("SUV"), "SUV", 
                ifelse(dat$Type %in% c("Pickup Truck"), "Truck", 
                ifelse(dat$Type %in% c("Van (mini van, sprinter van, etc.)"), "Van", 
                ifelse(dat$Type %in% c("Large truck (Semi-Truck, Fedex Truck, Uhaul)", "Bus", "Vehicle Pulling a Trailer"), "Large", 
                ifelse(dat$Type %in% c("Motorcycle"), "Motorcycle", NA))))))
dat$VehType1 <- factor(dat$VehType1, levels=c("Sedan", "SUV", "Truck", "Van", "Large", "Motorcycle"))
summary(dat$VehType1)
dat$VehType1_Sedan <- dat$VehType1=="Sedan"
dat$VehType1_SUV <- dat$VehType1=="SUV"
dat$VehType1_Truck <- dat$VehType1=="Truck"
dat$VehType1_Van <- dat$VehType1=="Van"
dat$VehType1_Large <- dat$VehType1=="Large"
dat$VehType0_Largetruck <- dat$Type=="Large truck (Semi-Truck, Fedex Truck, Uhaul)"
dat$VehType0_Bus <- dat$Type=="Bus"
dat$VehType0_Trailer <- dat$Type=="Vehicle Pulling a Trailer"
dat$VehType1_Mcycle <- dat$VehType1=="Motorcycle"
dat$VehType2 <- dat$VehType1
levels(dat$VehType2) <- c("Small", "Medium", "Medium", "Medium", "Large", "Small")
summary(dat$VehType2)
dat$VehType2 <- relevel(dat$VehType2, ref="Medium")
dat$VehType2_Small <- dat$VehType2=="Small"
dat$VehType2_Medium <- dat$VehType2=="Medium"
dat$VehType2_Large <- dat$VehType2=="Large"

# c. Conflict information
summary(dat$Encroachment_Time)
dat$EncrTime <- abs(dat$Encroachment_Time)
summary(dat$EncrTime)
dat$PreEncrTime <- ifelse(dat$Encroachment_Time<0, abs(dat$Encroachment_Time), NA)
dat$PostEncrTime <- ifelse(dat$Encroachment_Time>0, abs(dat$Encroachment_Time), NA)
dat$ConflSev <- ifelse(dat$EncrTime<=3, "High", 
                ifelse(dat$EncrTime>3 & dat$EncrTime<=5, "Mild", 
                ifelse(dat$EncrTime>5, "Low", NA)))
dat$ConflSev <- ordered(dat$ConflSev, levels=c("Low", "Mild", "High"))
summary(dat$ConflSev)
dat$ConflSev_Low <- dat$ConflSev=="Low"
dat$ConflSev_Mild <- dat$ConflSev=="Mild"
dat$ConflSev_High <- dat$ConflSev=="High"

# d. Weather and time information
summary(dat$Weather)
dat$Weath <- dat$Weather
levels(dat$Weath) <- c("Clear", "Rain", "Snow")
# dat$Weath <- droplevels(dat$Weath)
dat$WeathCle <- dat$Weath=="Clear"
dat$WeathRai <- dat$Weath=="Rain"
dat$WeathSno <- dat$Weath=="Snow"
summary(dat$air_temp_f)
# hist(dat$air_temp_f)
dat$TEMP <- dat$air_temp_f
dat$TEMPCAT8 <- cut(dat$TEMP, breaks=c(20,30,40,50,60,70,80,90,100), right=F)
dat$TEMPCAT4 <- cut(dat$TEMP, breaks=c(20,50,65,80,95), right=F)
summary(dat$TEMPCAT8); summary(dat$TEMPCAT4)
dat$TEMPCAT8 <- relevel(dat$TEMPCAT8, ref="[40,50)")
dat$TEMPCAT4 <- relevel(dat$TEMPCAT4, ref="[20,50)")
dat$TEMP_2050 <- dat$TEMPCAT4=="[20,50)"
dat$TEMP_5065 <- dat$TEMPCAT4=="[50,65)"
dat$TEMP_6580 <- dat$TEMPCAT4=="[65,80)"
dat$TEMP_8095 <- dat$TEMPCAT4=="[80,95)"
dat$TEMPL32 <- dat$TEMP<32
dat$TEMP80P <- dat$TEMP>=80
summary(dat$TEMPL32); summary(dat$TEMP80P)
table(dat$hourly_precip_in)
summary(dat$hourly_precip_in)
# hist(dat$hourly_precip_in)
dat$PRCP <- dat$hourly_precip_in
dat$PRCP_01 <- dat$PRCP >= 0.01
summary(dat$PRCP_01)
dat$Weekday_Mon <- dat$Weekday=="Mon"
dat$Weekday_Tue <- dat$Weekday=="Tue"
dat$Weekday_Wed <- dat$Weekday=="Wed"
dat$Weekday_Thu <- dat$Weekday=="Thu"
dat$Weekday_Fri <- dat$Weekday=="Fri"
dat$Weekday_M_F <- dat$Weekday %in% c("Mon", "Fri")
dat$Weekday_TWH <- dat$Weekday %in% c("Tue", "Wed", "Thu")
dat$TOD2_0611 <- dat$TOD2=="0611"
dat$TOD2_1217 <- dat$TOD2=="1217"
dat$TOD2_1805 <- dat$TOD2=="1805"

# e. Traffic signal statuses
# NAs for RTNotSig ~ PEDCP_, VEHCP_       --> edit signal status, include RTNotSig dummy
summary(dat[,c("PEDCP_ped", "PEDCP_veh", "VEHCP_ped", "VEHCP_veh")])
dat$PEDCP_ped_Walk <- dat$PEDCP_ped=="Walk"
dat$PEDCP_ped_FDW <- dat$PEDCP_ped=="FDW"
dat$PEDCP_ped_SDW <- dat$PEDCP_ped=="SDW"
dat$VEHCP_veh_G <- dat$VEHCP_veh=="Green"
dat$VEHCP_veh_Y <- dat$VEHCP_veh=="Yellow"
dat$VEHCP_veh_R <- dat$VEHCP_veh=="Red"
table(dat$RTNotSig, is.na(dat$PEDCP_ped))
dat$PEDCP_ped_Walk[is.na(dat$PEDCP_ped_Walk)] <- F
dat$PEDCP_ped_FDW[is.na(dat$PEDCP_ped_FDW)] <- F
dat$PEDCP_ped_SDW[is.na(dat$PEDCP_ped_SDW)] <- F
table(dat$RTNotSig, is.na(dat$VEHCP_veh))
dat$VEHCP_veh_G[is.na(dat$VEHCP_veh_G)] <- F
dat$VEHCP_veh_Y[is.na(dat$VEHCP_veh_Y)] <- F
dat$VEHCP_veh_R[is.na(dat$VEHCP_veh_R)] <- F

# Inspect
names(dat)

########################################
# Process level 2 variables

# Inspect
names(dat)

# Grouping (level-2) identifier
dat$L2ID <- paste(dat$Signal, dat$Corner)
length(unique(dat$L2ID))
table(dat$L2ID)

# f. Level 2 corner & intersection attributes
# NAs for CHANNEL ~ CW_DIST, SB_DIST     --> edit distances, include CHANNEL dummy
summary(dat$CURB_RAD)
summary(dat$CW_DIST)
table(dat$CHANNEL, is.na(dat$CW_DIST))
dat$CW_DIST[is.na(dat$CW_DIST)] <- 0
summary(dat$SB_DIST)
# -36 value is okay: stop bar is beyond start of crossing
table(dat$CHANNEL, is.na(dat$SB_DIST))
dat$SB_DIST[is.na(dat$SB_DIST)] <- 0
summary(dat$RAMPS); table(dat$RAMPS)
dat$RAMPS_1 <- dat$RAMPS==1
dat$RAMPS_2 <- dat$RAMPS==2
table(dat$CR_TYPE)
dat$CRType <- factor(dat$CR_TYPE, levels=c("Diagonal", "Blended", "Directional"))
dat$CRType_Diag <- dat$CRType=="Diagonal"
dat$CRType_Blen <- dat$CRType=="Blended"
dat$CRType_Dire <- dat$CRType=="Directional"
summary(dat$Crosswalk.ID)
table(dat$CWType1)
table(dat$CWType2)
dat$CWType <- ifelse(dat$Crosswalk.ID=="First crosswalk", dat$CWType1, 
              ifelse(dat$Crosswalk.ID=="Second crosswalk", dat$CWType2, NA))
dat$CWType <- factor(dat$CWType, levels=c("Standard", "Continental", "No Crossing"))
summary(dat$CWType)
dat$CWType_Stan <- dat$CWType=="Standard"
dat$CWType_Cont <- dat$CWType=="Continental"
dat$CWType_None <- dat$CWType=="No Crossing"
dat$CWType1_Stan <- dat$CWType1=="Standard"
dat$CWType1_Cont <- dat$CWType1=="Continental"
dat$CWType1_None <- dat$CWType1=="No Crossing"
dat$CWType2_Stan <- dat$CWType2=="Standard"
dat$CWType2_Cont <- dat$CWType2=="Continental"
dat$CWType2_None <- dat$CWType2=="No Crossing"
summary(dat$RT_LNS); table(dat$RT_LNS)
dat$RT_LNS_05 <- dat$RT_LNS==0.5
dat$RT_LNS_1 <- dat$RT_LNS==1
dat$RT_LNS_2 <- dat$RT_LNS==2
summary(dat$REC_LNS); table(dat$REC_LNS)
dat$REC_LNS_0 <- dat$REC_LNS==0
dat$REC_LNS_1 <- dat$REC_LNS==1
summary(dat$CHANNEL)
table(dat$SKEWED)
dat$Skewed <- as.logical(dat$SKEWED)
summary(dat$EXTENSION)
summary(dat$RTOR)
summary(dat$Street.Lt)
summary(dat$BIKE_LN)
table(dat$BIKE_LOC)
dat$BikeLnLoc <- ifelse(is.na(dat$BIKE_LOC), "None", 
                 ifelse(dat$BIKE_LOC %in% c("None"), "None", 
                 ifelse(dat$BIKE_LOC %in% c("Right"), "Right", 
                 ifelse(dat$BIKE_LOC %in% c("Left", "Other"), "Other", NA))))
dat$BikeLnLoc <- factor(dat$BikeLnLoc, levels=c("None", "Right", "Other"))
summary(dat$BikeLnLoc)
dat$Bike_None <- dat$BikeLnLoc=="None"
dat$Bike_Right <- dat$BikeLnLoc=="Right"
dat$Bike_Other <- dat$BikeLnLoc=="Other"
dat$Bike_Other_Left <- !is.na(dat$BIKE_LOC) & dat$BIKE_LOC=="Left"
dat$Bike_Other_Other <- !is.na(dat$BIKE_LOC) & dat$BIKE_LOC=="Other"
summary(dat$AAD)
dat$AADP0100 <- dat$AAD/100
dat$logAADP <- log(dat$AAD)
summary(dat$AADT_30M_Buffer)
dat$AADT1000 <- dat$AADT_30M_Buffer/1000
dat$logAADT <- log(dat$AADT_30M_Buffer)
summary(dat$highway)
dat$Highway <- dat$highway==1
summary(dat$major_road)
dat$MajorRoad <- dat$major_road==1
summary(dat$local_road)
dat$LocalRoad <- dat$local_road==1
summary(dat$RTNotSig)
summary(dat$OnRamp)
summary(dat$OffRamp)

# g. Level 2 neighborhood attributes
summary(dat$popden_000_qtmi)
summary(dat$empden_000_qtmi)
summary(dat$per_res_qtmi)
summary(dat$per_com_qtmi)
summary(dat$per_ind_qtmi)
summary(dat$per_vac_qtmi)
summary(dat$per_other_qtmi)
summary(dat$intden_qtmi)
summary(dat$per4wy_qtmi)
summary(dat$stops_qtmi)
summary(dat$stopden_qtmi)
summary(dat$worship_qtmi)
summary(dat$schools_qtmi)
summary(dat$park_sqmi_qtmi)
dat$park_acre_qtmi <- dat$park_sqmi_qtmi * 640
summary(dat$income_000_qtmi)
summary(dat$avgveh_qtmi)
summary(dat$hhsize_qtmi)

# Inspect
names(dat)

########################################
# END
########################################