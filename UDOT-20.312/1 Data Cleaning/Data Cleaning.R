##################################################
# Project:  UDOT-20.312 Right turn safety
# Authors:  Atul Subedi (atul.subedi@usu.edu)
#           Patrick Singleton (patrick.singleton@usu.edu)
# File:     Data Cleaning.R
# Date:     2022 Summer, Fall, 2023 Spring
# About:    Data Cleaning for UDOT-20.312 Right turn safety 
##################################################

##################################################
# Notes

# 2022-06-16 created AS
# 2022-07-22 updated PAS
# - updated file loading system
# - fixed wide-to-long process
# - changed column processing
# 2022-10-04 updated AS
# - added Bailey's data
# - added code to process timestamps for when last ped reached conflict point
# 2022-10-10 updated PAS
# - minor code cleanup, especially ped conflict timestamps
# 2023-02-03 updated PAS: minor code cleanup, more data cleaning of CSVs
# 2023-04-02 updated PAS: fixed calculations of pre/post encroachment time

# Note:
# Ped = Pedestrian
# TS = Time Stamp
# CP = Conflict Point
# RT = Right Turn
# V1 = Vehicle 1 
# V2 = Vehicle 2
# V3 = Vehicle 3
# V4 = Vehicle 4
# OC = Other Characteristics

# Installing package
# install.packages("chron")
# Loading Library
library(tidyr)
library(stringr)
library(chron)
library(mlogit)

##################################################
# Loading Data set

# Initialize info about files
myfiles <- data.frame(FULL=list.files(file.path("Data", "Data from Videos", "Checked")))
myfiles$SHORT <- unlist(strsplit(myfiles$FULL, " (", fixed=T))[(1:nrow(myfiles))*2]
myfiles$SHORT <- gsub(").csv", "", myfiles$SHORT)
myfiles$PERSON <- substr(myfiles$SHORT,17,19)
myfiles$FOLDER <- substr(myfiles$SHORT,1,15) 

# Loop to get data
i <- 1
print(myfiles$FULL[i])
temp <- read.csv(file=file.path("Data", "Data from Videos", "Checked", myfiles$FULL[i]), stringsAsFactors=F)
temp <- data.frame(FOLDER=myfiles$FOLDER[i], PERSON=myfiles$PERSON[i], temp)
temp00 <- temp
rm(temp)
for (i in 2:nrow(myfiles)) {
  print(myfiles$FULL[i])
  temp <- read.csv(file=file.path("Data", "Data from Videos", "Checked", myfiles$FULL[i]), stringsAsFactors=F)
  temp <- data.frame(FOLDER=myfiles$FOLDER[i], PERSON=myfiles$PERSON[i], temp)
  temp00 <- rbind(temp00, temp)
  rm(temp)
  }; rm(i)

# Inspect
names(temp00)
str(temp00)
summary(temp00)

##################################################
# Change column names

# Rename
Merged_DF <- temp00

# Creating a temporary data frame and formatting column heading
temp01<-data.frame(ID=1:nrow(temp00),
                   Folder=Merged_DF$FOLDER, 
                   Person=Merged_DF$PERSON, 
                   
                   TS.of.data.collection=Merged_DF$Timestamp,
                   Researcher.Initial=Merged_DF$Enter.your.initials,
                   Signal.ID=Merged_DF$Signal.ID,
                   Date=Merged_DF$Date,
                   Weather=Merged_DF$Weather,
                   
                   Number.of.people.in.group=Merged_DF$Group.size....people.,
                   Age=Merged_DF$Age.category,
                   Gender=Merged_DF$Gender,
                   Other.characterstics=Merged_DF$Other.characteristics,
                   Crosswalk.ID=Merged_DF$Crosswalk,
                   Crossing.location=Merged_DF$Crossing.location,
                   Crossing.direction=Merged_DF$Crossing.direction,
                   Ped.TS.at.CP=Merged_DF$Timestamp.of.pedestrian.at.conflict.point,
                   RT.queue.length=Merged_DF$Right.turn.queue.length,
                   About.Ped=Merged_DF$Anything.else.to.tell.us.about.the.pedestrian.s..themselves.,
                   
                   V1_Stop.location=Merged_DF$Where.did.the.vehicle.stop.,
                   V1_TS.CP=Merged_DF$Timestamp.of.vehicle.at.conflict.point,
                   V1_Reaction.to.conflict=Merged_DF$How.did.the.driver.react.to.the.conflict.,
                   V1_Ped.Reaction=Merged_DF$How.did.the.pedestrian.react.to.the.conflict.,
                   V1_Type=Merged_DF$What.type.of.vehicle.was.turning.right.,
                   V1_About.Conflict=Merged_DF$Anything.else.to.tell.us.about.the.conflict.,
                   
                   V2_Stop.location=Merged_DF$Where.did.the.vehicle.stop..1,
                   V2_TS.CP=Merged_DF$Time.vehicle.reached.the.conflict.point,
                   V2_Reaction.to.conflict=Merged_DF$How.did.the.driver.react.to.the.conflict..1,
                   V2_Ped.Reaction=Merged_DF$How.did.the.pedestrian.react.to.the.conflict..1,
                   V2_Type=Merged_DF$What.type.of.vehicle.was.turning.,
                   V2_About.Conflict=Merged_DF$Anything.else.to.tell.us.about.the.conflict..1,
                   
                   V3_Stop.location=Merged_DF$Where.did.the.vehicle.stop..2,
                   V3_TS.CP=Merged_DF$Time.vehicle.reached.the.conflict.point.1,
                   V3_Reaction.to.conflict=Merged_DF$How.did.the.driver.react.to.the.conflict..2,
                   V3_Ped.Reaction=Merged_DF$How.did.the.pedestrian.react.to.the.conflict..2,
                   V3_Type=Merged_DF$What.type.of.vehicle.was.turning..1,
                   V3_About.Conflict=Merged_DF$Anything.else.to.tell.us.about.the.conflict..2,
                   
                   V4_Stop.location=Merged_DF$Where.did.the.vehicle.stop..3,
                   V4_TS.CP=Merged_DF$Time.vehicle.reached.the.conflict.point.2,
                   V4_Reaction.to.conflict=Merged_DF$How.did.the.driver.react.to.the.conflict..3,
                   V4_Ped.Reaction=Merged_DF$How.did.the.pedestrian.react.to.the.conflict..3,
                   V4_Type=Merged_DF$What.type.of.vehicle.was.turning..2,
                   V4_About.Conflict=Merged_DF$Anything.else.to.tell.us.about.the.conflict..3,
                   
                   About.overall.conflict=Merged_DF$Anything.else.to.tell.us.about.this.pedestrian.vehicle.conflict.event.)

# Cleanup
rm(Merged_DF)

# Add NAs where empty text
temp01[temp01==""] <- NA

# Remove rows with all NAs
temp01[rowSums(is.na(temp01))==40,]
temp01 <- temp01[rowSums(is.na(temp01))<(ncol(temp01)-3),]

# Inspect
names(temp01)
str(temp01)
summary(temp01)

##################################################
# Format columns (general, pedestrian)

# Rename
temp02 <- temp01

# Format/inspect columns: General
# Folder
temp02$Folder <- as.factor(temp02$Folder)
summary(temp02$Folder)
# Person
temp02$Person <- as.factor(temp02$Person)
summary(temp02$Person)
# TS.of.data.collection
timetemp01 <- as.POSIXct(temp02$TS.of.data.collection, tz="America/Denver", 
                         format="%m/%d/%Y %H:%M:%S")
timetemp02 <- as.POSIXct(temp02$TS.of.data.collection, tz="America/Denver", 
                         format="%m/%d/%Y %H:%M")
timetemp <- timetemp01
timetemp[is.na(timetemp)] <- timetemp02[is.na(timetemp)]
temp02$TS.of.data.collection <- timetemp
rm(timetemp01, timetemp02, timetemp)
range(temp02$TS.of.data.collection)
# Researcher.Initial
temp02$Researcher.Initial <- toupper(temp02$Researcher.Initial)
temp02$Researcher.Initial <- gsub(" ", "", temp02$Researcher.Initial, fixed=T)
summary(temp02$Person==temp02$Researcher.Initial)
# Signal.ID
summary(temp02$Signal.ID); table(temp02$Signal.ID)
# Date
temp02$Date <- as.Date(temp02$Date, tz="America/", format="%m/%d/%Y")
for (i in levels(temp02$Folder)) {
  print(i)
  print(table(temp02$Date[temp02$Folder==i]))
}; rm(i)
# Weather
table(temp02$Weather)
temp02$Weather <- ifelse(temp02$Weather %in% c("Overcast", "clear night"), "Clear", temp02$Weather)
temp02$Weather <- factor(temp02$Weather, levels=c("Clear", "Rain (actively raining, or wet roadways)", "Snow (actively snowing, or snow on the roads)"))
summary(temp02$Weather)
# temp02$Weather <- droplevels(temp02$Weather)
# About.overall.conflict
table(!is.na(temp02$About.overall.conflict))
sort(table(temp02$About.overall.conflict))
temp02$About.overall.conflict
trem <- c("no conflict", "No vehicle conflicts", "No vehicle conflicts.", "No vehicles involved in conflict.", 
          "No vehicle conflicts before pedestrian crosses", "No vehicle conflicts after pedestrian crosses", 
          "No conflicts", "no conflict ", "No vehicles in que. No conflict potential.", 
          "No vehicles after pedestrian crosses", "No vehicle in the que. No conflict potential.", 
          "No vehicle conflicts. Streetcar.", "No vehicle conflicts, but the pedestrians still sped up.", 
          "No vehicle conflicts for this pedestrian", "No vehicle conflicts after pedestrian crosses.", 
          "No vehicle conficts", "No right turn vehicle que", "No right-turning vehicles within 10s", 
          "No right-turning vehicles within 10 seconds", "No pedestrian conflicts", 
          "No conflicts after pedestrians crossed", "no conflict, camera fuzzy", 
          "no conflict, but the camera is very difficult to see out of. ", "No conflict", 
          "No cars in the right turning lane.", "No conflicting vehicles")
temp02$About.overall.conflict <- ifelse(temp02$About.overall.conflict %in% trem, NA, temp02$About.overall.conflict)
rm(trem)
table(!is.na(temp02$About.overall.conflict))
sort(table(temp02$About.overall.conflict))
# checked these and probably no more edits required

# Format/inspect columns: General
# Number.of.people.in.group
summary(temp02$Number.of.people.in.group)
temp02$Number.of.people.in.group[temp02$Number.of.people.in.group>=10]
# Age
temp02a <- temp02[,1:(which(names(temp02)=="Age")-1)]
temp02b <- temp02[,(which(names(temp02)=="Age")+1):ncol(temp02)]
temp02a$Age_Child <- grepl("Child", temp02$Age)
temp02a$Age_Teenager <- grepl("Teenager", temp02$Age)
temp02a$Age_Young_adult <- grepl("Young adult", temp02$Age)
temp02a$Age_Middle_aged_adult <- grepl("Middle-aged adult", temp02$Age)
temp02a$Age_Older_adult_over_65 <- grepl("Older adult", temp02$Age)
temp02a$Age_Adult_of_unknown_age <- grepl("Adult of unknown age", temp02$Age)
temp02a$Age_ChildTeen <- (temp02a$Age_Child + temp02a$Age_Teenager)>0
temp02a$Age_Adult <- (temp02a$Age_Young_adult + temp02a$Age_Middle_aged_adult + temp02a$Age_Older_adult_over_65 + temp02a$Age_Adult_of_unknown_age)>0
temp02 <- cbind(temp02a, temp02b)
rm(temp02a, temp02b)
for (i in grep("Age", names(temp02), value=T)) {
  print(i)
  print(table(temp02[,i]))
}; rm(i)
# Gender
temp02a <- temp02[,1:(which(names(temp02)=="Gender")-1)]
temp02b <- temp02[,(which(names(temp02)=="Gender")+1):ncol(temp02)]
temp02a$Gender_Male <- grepl("Male", temp02$Gender)
temp02a$Gender_Female <- grepl("Female", temp02$Gender)
temp02a$Gender_Unknown_gender <- grepl("Unknown gender", temp02$Gender)
temp02 <- cbind(temp02a, temp02b)
rm(temp02a, temp02b)
for (i in grep("Gender", names(temp02), value=T)) {
  print(i)
  print(table(temp02[,i]))
}; rm(i)
# Other characteristics
temp02a <- temp02[,1:(which(names(temp02)=="Other.characterstics")-1)]
temp02b <- temp02[,(which(names(temp02)=="Other.characterstics")+1):ncol(temp02)]
temp02a$OC_Stroller <- grepl("Stroller", temp02$Other.characterstics)
temp02a$OC_Carrying_load <- grepl("Carrying load", temp02$Other.characterstics)
temp02a$OC_Wheelchair <- grepl("Wheelchair", temp02$Other.characterstics)
temp02a$OC_Skateboard <- grepl("Skateboard", temp02$Other.characterstics)
temp02a$OC_Scooter <- grepl("Scooter", temp02$Other.characterstics)
temp02a$OC_Bicycle <- grepl("Bicycle", temp02$Other.characterstics)
temp02a$OC_Distracted <- grepl("Distracted", temp02$Other.characterstics)
temp02a$OC_Others <- temp02$Other.characterstics
temp02a$OC_Others <- gsub("Stroller, ", "", temp02a$OC_Others)
temp02a$OC_Others <- gsub("Stroller", "", temp02a$OC_Others)
temp02a$OC_Others <- gsub("Carrying load (larger than small purse or backpack), ", "", temp02a$OC_Others, fixed=T)
temp02a$OC_Others <- gsub("Carrying load (larger than small purse or backpack)", "", temp02a$OC_Others, fixed=T)
temp02a$OC_Others <- gsub("Carrying load, ", "", temp02a$OC_Others, fixed=T)
temp02a$OC_Others <- gsub("Carrying load", "", temp02a$OC_Others, fixed=T)
temp02a$OC_Others <- gsub("Wheelchair, ", "", temp02a$OC_Others)
temp02a$OC_Others <- gsub("Wheelchair", "", temp02a$OC_Others)
temp02a$OC_Others <- gsub("Skateboard, ", "", temp02a$OC_Others)
temp02a$OC_Others <- gsub("Skateboard", "", temp02a$OC_Others)
temp02a$OC_Others <- gsub("Scooter, ", "", temp02a$OC_Others)
temp02a$OC_Others <- gsub("Scooter", "", temp02a$OC_Others)
temp02a$OC_Others <- gsub("Bicycle, ", "", temp02a$OC_Others)
temp02a$OC_Others <- gsub("Bicycle", "", temp02a$OC_Others)
temp02a$OC_Others <- gsub("Distracted (phone, headphones, conversations, etc), ", "", temp02a$OC_Others, fixed=T)
temp02a$OC_Others <- gsub("Distracted (phone, headphones, conversations, etc)", "", temp02a$OC_Others, fixed=T)
temp02a$OC_Others <- ifelse(temp02a$OC_Others=="", NA, temp02a$OC_Others)
table(!is.na(temp02a$OC_Others))
table(temp02a$OC_Others)
# most others are running/jogging, walking dog, etc. 
# don't process into a new column because not consistently collected
temp02 <- cbind(temp02a, temp02b)
rm(temp02a, temp02b)
for (i in grep("OC_", names(temp02), value=T)) {
  print(i)
  print(table(temp02[,i]))
}; rm(i)
# Crosswalk.ID
temp02$Crosswalk.ID <- as.factor(temp02$Crosswalk.ID)
summary(temp02$Crosswalk.ID)
# didn't fix all missing, only those with potential conflicts
# Crossing.location
table(temp02$Crossing.location)
temp02$Crossing.location <- factor(temp02$Crossing.location, 
  levels=c("In the crosswalk or the crosswalk area", "Mid-block, away from the crosswalk", "In the middle of the intersection"))
summary(temp02$Crossing.location)
# Crossing.direction
temp02$Crossing.direction <- as.factor(temp02$Crossing.direction)
summary(temp02$Crossing.direction)
# Ped.TS.at.CP
temp02a <- temp02[,1:(which(names(temp02)=="Ped.TS.at.CP")-1)]
temp02b <- temp02[,(which(names(temp02)=="Ped.TS.at.CP")+1):ncol(temp02)]
temp02a$Ped.TS.at.CP.1 <- as.POSIXct(paste(temp02$Date, temp02$Ped.TS.at.CP, sep=" "), tz="America/Denver", format="%Y-%m-%d %H:%M:%S")
summary(temp02a$Ped.TS.at.CP.1)
# - extract times for last ped for groups
temp02a$temp1 <- regmatches(temp02b$About.Ped, gregexpr("[[:digit:]]+", temp02b$About.Ped))
temp02a$temp2 <- sapply(temp02a$temp1, paste, collapse=":")
# - inspect
temp02a$temp2[!nchar(temp02a$temp2) %in% c(0,8)]
# View(temp02[!nchar(temp02a$temp2) %in% c(0,8),])
# - fix too long
temp02a$temp2[which(nchar(temp02a$temp2)==17)] <- str_sub(temp02a$temp2[which(nchar(temp02a$temp2)==17)], 1, 8)
temp02a$temp3 <- ifelse(nchar(temp02a$temp2)==8, temp02a$temp2, NA)
temp02a$temp4 <- ifelse(is.na(temp02a$temp3), temp02$Ped.TS.at.CP, temp02a$temp3)
# - inspect
unique(temp02a$temp3)
# - add date
temp02a$Ped.TS.at.CP.2 <- as.POSIXct(paste(temp02a$Date, temp02a$temp4, sep=" "), tz="America/Denver", format="%Y-%m-%d %H:%M:%S")
temp02a$Ped.TS.at.CP.Diff <- difftime(temp02a$Ped.TS.at.CP.2, temp02a$Ped.TS.at.CP.1, units="secs")
table(temp02a$Ped.TS.at.CP.Diff)
# - combine
temp02a[,c("temp1", "temp2", "temp3", "temp4")] <- NULL
temp02 <- cbind(temp02a, temp02b)
rm(temp02a, temp02b)
# RT.queue.length
temp02$RT.queue.length <- as.factor(temp02$RT.queue.length)
summary(temp02$RT.queue.length)
# About.Ped
table(!is.na(temp02$About.Ped))
table(temp02$About.Ped)
# checked these and probably no more edits required

# Inspect
names(temp02)
str(temp02)
summary(temp02)

##################################################
# Convert data from wide to long

# Create long
names(temp02)
veh.nums <- paste0("V", c(1:4))
veh.names <- c("Stop.location", "TS.CP", "Reaction.to.conflict", "Ped.Reaction", "Type", "About.Conflict")
veh.varying <- list(paste(veh.nums, veh.names[1], sep="_"), 
                    paste(veh.nums, veh.names[2], sep="_"), 
                    paste(veh.nums, veh.names[3], sep="_"), 
                    paste(veh.nums, veh.names[4], sep="_"), 
                    paste(veh.nums, veh.names[5], sep="_"), 
                    paste(veh.nums, veh.names[6], sep="_"))
temp03 <- reshape(temp02, direction="long", idvar="ID", timevar="Vehicle", 
                  varying=veh.varying, sep="_", v.names=veh.names)

# Inspect
names(temp03)
str(temp03)
summary(temp03)

##################################################
# Format columns (vehicle)

# Rename
temp04 <- temp03

# Format/inspect columns: Vehicle
# Vehicle
table(temp04$Vehicle)
# Stop.location
table(temp04$Stop.location)
temp04$Stop.location <- factor(temp04$Stop.location, 
  levels=c("Did not stop", "Before the first crosswalk", "Inside the first crosswalk", "Between the first and second crosswalk", "Inside the second crosswalk"))
summary(temp04$Stop.location)
# temp04$Stop.location <- droplevels(temp04$Stop.location)
# TS.CP
temp04$TS.CP <- as.POSIXct(paste(temp04$Date, temp04$TS.CP, sep=" "), tz="America/Denver", format="%Y-%m-%d %H:%M:%S")
summary(temp04$TS.CP)
# Reaction.to.conflict
table(temp04$Reaction.to.conflict)
temp04$Reaction.to.conflict <- factor(temp04$Reaction.to.conflict, 
  levels=c("No obvious reaction","Driver fully stopped", "Driver slowed down", "Driver sped up", "Driver swerved"))
summary(temp04$Reaction.to.conflict)
# Ped.Reaction
table(temp04$Ped.Reaction)
temp04$Ped.Reaction <- factor(temp04$Ped.Reaction, 
  levels=c("No obvious reaction", "Stopped and waited for the vehicle", "Slowed down to avoid collision", 
           "Sped up to avoid collision", "Ran to avoid collision", "Changed direction"))
summary(temp04$Ped.Reaction)
# Type
table(temp04$Type)
temp04$Type <- factor(temp04$Type, 
  levels=c("Sedan", "Large truck (Semi-Truck, Fedex Truck, Uhaul)", "Van (mini van, sprinter van, etc.)", 
           "SUV", "Bus", "Pickup Truck", "Vehicle Pulling a Trailer", "Motorcycle"))
summary(temp04$Type)
# About.Conflict
table(!is.na(temp04$About.Conflict))
table(temp04$About.Conflict)
# checked these and probably no more edits required

# Add new variables (pedestrian, vehicle)
temp04$Encroachment_Time_Pre <- difftime(temp04$Ped.TS.at.CP.1, temp04$TS.CP, units = "secs")
temp04$Encroachment_Time_Post <- difftime(temp04$TS.CP, temp04$Ped.TS.at.CP.2, units = "secs")
table(temp04$Encroachment_Time_Pre)
table(temp04$Encroachment_Time_Post)
table(temp04$Encroachment_Time_Pre>0, temp04$Encroachment_Time_Post>0)
# - calculate overall: + = post (car after ped), - = pre (car before ped), 0 = same
temp04$Encroachment_Time <- ifelse(temp04$Encroachment_Time_Pre==0 & temp04$Encroachment_Time_Post==0, temp04$Encroachment_Time_Post, 
                            ifelse(temp04$Encroachment_Time_Pre>0 & temp04$Encroachment_Time_Post<0, -temp04$Encroachment_Time_Pre, 
                            ifelse(temp04$Encroachment_Time_Pre<0 & temp04$Encroachment_Time_Post>0, temp04$Encroachment_Time_Post, 
                            ifelse(temp04$Encroachment_Time_Pre < temp04$Encroachment_Time_Post, -temp04$Encroachment_Time_Pre, 
                            ifelse(temp04$Encroachment_Time_Pre > temp04$Encroachment_Time_Post, temp04$Encroachment_Time_Post, NA)))))
temp04$Encroachment_Time <- as.numeric(temp04$Encroachment_Time)
table(temp04$Encroachment_Time)
temp04[!is.na(temp04$Encroachment_Time) & abs(temp04$Encroachment_Time)>60,]
temp04[!is.na(temp04$Encroachment_Time) & abs(temp04$Encroachment_Time)>10,]
# assume these are okay but will be removed before analysis
temp04$Encroachment.Car.Before.Ped <- ifelse(temp04$Encroachment_Time < 0, T, F)
temp04$Encroachment.Car.After.Ped <- ifelse(temp04$Encroachment_Time > 0, T, F)
temp04$Encroachment.Car.Same.Ped <- ifelse(temp04$Encroachment_Time == 0, T, F)

# Inspect
names(temp04)
str(temp04)
summary(temp04)

##################################################
# Final datasets

# Wide
datwide <- temp02
# remove vehicle columns
datwide[,grep("V1", names(datwide))] <- NULL
datwide[,grep("V2", names(datwide))] <- NULL
datwide[,grep("V3", names(datwide))] <- NULL
datwide[,grep("V4", names(datwide))] <- NULL
# inspect
names(datwide)
str(datwide)
summary(datwide)

# Long
datlong <- temp04
# remove empty rows
datlong$EMPTY <- ifelse(rowSums(is.na(datlong[,veh.names]))==6, T, F)
table(datlong$EMPTY)
datlong <- datlong[datlong$EMPTY==F,]
rm(veh.nums, veh.names, veh.varying)
datlong$EMPTY <- NULL
datlong <- datlong[order(datlong$ID, datlong$Vehicle),]
# inspect
names(datlong)
str(datlong)
summary(datlong)

# Save files
write.csv(datwide, file.path("Analysis", "Conflict Analysis", "datwide.csv"), row.names=F)
saveRDS(datwide, file.path("Analysis", "Conflict Analysis", "datwide.rds"))
write.csv(datlong, file.path("Analysis", "Conflict Analysis", "datlong.csv"), row.names=F)
saveRDS(datlong, file.path("Analysis", "Conflict Analysis", "datlong.rds"))
# NOTE: manually move files to Analysis/Conflict Analysis/Data Cleaning when ready to overwrite

##################################################
# Cleanup

# Cleanup
rm(temp00, temp01, temp02, temp03, temp04)
rm(myfiles)
rm(datwide, datlong)
gc()

##################################################
# END
##################################################