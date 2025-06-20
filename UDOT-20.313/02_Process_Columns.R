#######################################
# Project:  UDOT-20.313 Ped violations
# Authors:  Patrick A. Singleton (patrick.singleton@usu.edu)
#           Sadie Boyer (sadie.boyer@usu.edu)
# File:     02_Process_Columns.R
# Date:     2021 Summer, 2022 Spring, Summer
# About:    Preliminary analysis of video data
########################################

########################################
# Notes

# Open R project first, then open this R script

# Major edits
# 2021-06-06 PAS created
# 2022-03-04 SB  updated
# 2022-08-11 PAS updated: added column processing code, from script 01 and 03
# 2022-11-19 PAS updated: added some checks of missing/wrong data

# Load libraries
library("lubridate")

########################################
# Load data

# Load dat
dat <- readRDS(file.path("Data", "dat01.rds"))

# Inspect
names(dat)
str(dat)
summary(dat)

########################################
# Adjust columns (names, types)

# Add column
dat$one <- 1L

# Timestamp
# convert to POSIXct type
# time zone assumed to be America/Denver unless otherwise specified
# change CDT and CST 
dat$Timestamp <- gsub("CDT", "-05", dat$Timestamp)
dat$Timestamp <- gsub("CST", "-06", dat$Timestamp)
# comes in different formats, see ?strptime and ?parse_date_time
# row  1 is   Y/m/d I:M:S p z
# row 11 is   m/d/Y H:M
# row 70 is   m/d/Y H:M:S
dat$Timestamp <- parse_date_time(dat$Timestamp, orders=c("Ymd IMS p z", "mdY HM", "mdY HMS"), tz="America/Denver")
summary(dat$Timestamp)

# Signal ID
j <- which(names(dat)=="Date")
dat1 <- dat[,1:(j-1)]
dat2 <- dat[,j:ncol(dat)]
dat1$Signal <- as.integer(substr(dat$Folder,12,15))
dat <- cbind(dat1, dat2)
rm(dat1, dat2, j)
table(dat$Signal)

# Date
# convert to Date type
# comes in different formats, see ?striptime and ?parse_date_time
# row  1 is   Y-m-d
# row 11 is   m/d/Y
dat$Date <- as.Date(parse_date_time(dat$Date, orders=c("Ymd", "mdY"), tz="America/Denver"))
summary(dat$Date)
# check
temp <- as.Date(substr(dat$Folder,0,10))
table(dat$Date-temp)
dat[(dat$Date-temp)<0,1:3]
dat[(dat$Date-temp)>3,1:3]
# errors were fixed manually in original CSV files
rm(temp)

# Origin.location
table(dat$Origin.location)
j <- which(names(dat)=="Origin.location")
dat1 <- dat[,1:(j-1)]
dat2 <- dat[,(j+1):ncol(dat)]
dat1$Origin <- dat$Origin.location
tfac <- c("From approach, left side", "From approach, right side", "From other street, left", "From other street, right", "From different crosswalk, left", "From different crosswalk, right")
dat1$Origin <- ifelse(dat1$Origin %in% tfac, dat1$Origin, "Other")
dat1$Origin <- factor(dat1$Origin, levels=c(tfac, "Other"))
dat1$OriginOtherText <- ifelse(dat1$Origin=="Other", dat$Origin.location, "")
dat <- cbind(dat1, dat2)
rm(dat1, dat2, j, tfac)
summary(dat$Origin)
table(dat$OriginOtherText)

# Group.size.people
names(dat)[names(dat)=="Group.size.people."] <- "GroupSize"
dat$GroupSize <- as.integer(dat$GroupSize)
table(dat$GroupSize)
summary(dat$GroupSize)
# dat[which(dat$GroupSize==0 | is.na(dat$GroupSize)), c("Folder", "Timestamp")]
# missing were fixed manually in original CSV files

# Age.category
table(dat$Age.category)
j <- which(names(dat)=="Age.category")
dat1 <- dat[,1:(j-1)]
dat2 <- dat[,(j+1):ncol(dat)]
dat1$AgeChild <- grepl("Child", dat$Age.category)
dat1$AgeTeen <- grepl("Teenager", dat$Age.category)
dat1$AgeAdultYoung <- grepl("Young adult", dat$Age.category)
dat1$AgeAdultMiddle <- grepl("Middle-aged adult", dat$Age.category)
dat1$AgeAdultOlder <- grepl("Older adult", dat$Age.category)
dat1$AgeAdultUnknown <- grepl("Adult of unknown age", dat$Age.category)
dat <- cbind(dat1, dat2)
rm(dat1, dat2, j)
summary(dat$AgeChild)
summary(dat$AgeTeen)
summary(dat$AgeAdultYoung)
summary(dat$AgeAdultMiddle)
summary(dat$AgeAdultOlder)
summary(dat$AgeAdultUnknown)
# summary(rowSums(dat[,c("AgeChild", "AgeTeen", "AgeAdultYoung", "AgeAdultMiddle", "AgeAdultOlder", "AgeAdultUnknown")]))
# dat[rowSums(dat[,c("AgeChild", "AgeTeen", "AgeAdultYoung", "AgeAdultMiddle", "AgeAdultOlder", "AgeAdultUnknown")])==0, c("Folder", "Timestamp")]
# missing were fixed manually in original CSV files

# Gender
table(dat$Gender)
j <- which(names(dat)=="Gender")
dat1 <- dat[,1:(j-1)]
dat2 <- dat[,(j+1):ncol(dat)]
dat1$GenderMale <- grepl("Male", dat$Gender)
dat1$GenderFemale <- grepl("Female", dat$Gender)
dat1$GenderUnknown <- grepl("Unknown gender", dat$Gender)
dat <- cbind(dat1, dat2)
rm(dat1, dat2, j)
summary(dat$GenderMale)
summary(dat$GenderFemale)
summary(dat$GenderUnknown)
# summary(rowSums(dat[,c("GenderMale", "GenderFemale", "GenderUnknown")]))
# dat[rowSums(dat[,c("GenderMale", "GenderFemale", "GenderUnknown")])==0, c("Folder", "Timestamp")]
# missing were fixed manually in original CSV files

# Other.characteristics
table(dat$Other.characteristics)
dat$Other.characteristics[is.na(dat$Other.characteristics)] <- ""
j <- which(names(dat)=="Other.characteristics")
dat1 <- dat[,1:(j-1)]
dat2 <- dat[,(j+1):ncol(dat)]
dat1$OtherStroller <- grepl("Stroller", dat$Other.characteristics)
dat1$OtherLoad <- grepl("Carrying load", dat$Other.characteristics)
dat1$OtherWheelchair <- grepl("Wheelchair", dat$Other.characteristics)
dat1$OtherSkateboard <- grepl("Skateboard", dat$Other.characteristics)
dat1$OtherScooter <- grepl("Scooter", dat$Other.characteristics)
dat1$OtherBicycle <- grepl("Bicycle", dat$Other.characteristics)
tfac <- c("Stroller", "Carrying load", "Wheelchair", "Skateboard", "Scooter", "Bicycle")
for (k in tfac) {
  dat$Other.characteristics <- gsub(k, "", dat$Other.characteristics)
}; rm(k)
dat$Other.characteristics <- gsub(";", "", dat$Other.characteristics)
dat$Other.characteristics <- gsub(", ", "", dat$Other.characteristics)
dat1$OtherOther <- ifelse(dat$Other.characteristics!="", T, F)
dat1$OtherOtherText <- ifelse(dat1$OtherOther, dat$Other.characteristics, "")
dat <- cbind(dat1, dat2)
rm(dat1, dat2, j, tfac)
summary(dat$OtherStroller)
summary(dat$OtherLoad)
summary(dat$OtherWheelchair)
summary(dat$OtherSkateboard)
summary(dat$OtherScooter)
summary(dat$OtherBicycle)
summary(dat$OtherOther)
table(dat$OtherOtherText)

# Anything.else.to.tell.us.about.the.pedestrian.s.themselves.
names(dat)[names(dat)=="Anything.else.to.tell.us.about.the.pedestrian.s.themselves."] <- "Notes3Pedestrians"
table(dat$Notes3Pedestrians)

# Waiting.area.location
table(dat$Waiting.area.location)
j <- which(names(dat)=="Waiting.area.location")
dat1 <- dat[,1:(j-1)]
dat2 <- dat[,(j+1):ncol(dat)]
dat1$WaitArea <- dat$Waiting.area.location
tfac <- c("Left", "Right")
dat1$WaitArea <- ifelse(dat1$WaitArea %in% tfac, dat1$WaitArea, "Other")
dat1$WaitArea <- factor(dat1$WaitArea, levels=c(tfac, "Other"))
dat1$WaitAreaOtherText <- ifelse(dat1$WaitArea=="Other", dat$Waiting.area.location, "")
dat <- cbind(dat1, dat2)
rm(dat1, dat2, j, tfac)
summary(dat$WaitArea)
table(dat$WaitAreaOtherText)

# Time.arrived.in.waiting.area
names(dat)[names(dat)=="Time.arrived.in.waiting.area"] <- "TimeWaitArr"
dat$temp <- as.POSIXct(dat$Date, tz="America/Denver")
for (i in 1:nrow(dat)) {
  if (is.na(dat$TimeWaitArr[i])) { dat$temp[i] <- NA }
  else if (dat$TimeWaitArr[i]=="") { dat$temp[i] <- NA }
  else { dat$temp[i] <- as.POSIXct(paste(dat$Date[i], dat$TimeWaitArr[i]), tz="America/Denver") }
}; rm(i)
dat$TimeWaitArr <- dat$temp
dat$temp <- NULL
summary(dat$TimeWaitArr)
# dat[which(is.na(dat$TimeWaitArr)), c("Folder", "Timestamp")]
# fixed manually, see "fix2-time" and "checkcurbtimes" later

# Number.of.other.people.waiting.same.waiting.area.
names(dat)[names(dat)=="Number.of.other.people.waiting.same.waiting.area."] <- "WaitOtherPeople"
dat$WaitOtherPeople <- as.integer(dat$WaitOtherPeople)
table(dat$WaitOtherPeople)
summary(dat$WaitOtherPeople)
# dat[which(is.na(dat$WaitOtherPeople)), c("Folder", "Timestamp")]
# missing were fixed manually in original CSV files

# Number.of.vehicles.passing.crossing.location.in.past.10.seconds
names(dat)[names(dat)=="Number.of.vehicles.passing.crossing.location.in.past.10.seconds"] <- "VehiclesPast10"
dat$VehiclesPast10 <- as.integer(dat$VehiclesPast10)
table(dat$VehiclesPast10)
summary(dat$VehiclesPast10)
# dat[which(is.na(dat$VehiclesPast10)), c("Folder", "Timestamp")]
# missing were fixed manually in original CSV files

# Number.of.vehicles.passing.crossing.location.in.next.10.seconds
names(dat)[names(dat)=="Number.of.vehicles.passing.crossing.location.in.next.10.seconds"] <- "VehiclesNext10"
dat$VehiclesNext10 <- as.integer(dat$VehiclesNext10)
table(dat$VehiclesNext10)
summary(dat$VehiclesNext10)
# dat[which(is.na(dat$VehiclesNext10)), c("Folder", "Timestamp")]
# missing were fixed manually in original CSV files

# Waiting.behaviors
table(dat$Waiting.behaviors)
dat$Waiting.behaviors[is.na(dat$Waiting.behaviors)] <- ""
j <- which(names(dat)=="Waiting.behaviors")
dat1 <- dat[,1:(j-1)]
dat2 <- dat[,(j+1):ncol(dat)]
dat1$WaitBehPressed <- grepl("Pressed pedestrian push-button", dat$Waiting.behaviors)
dat1$WaitBehPaced <- grepl("Paced or otherwise seemed impatient", dat$Waiting.behaviors)
dat1$WaitBehLeft <- grepl("Left waiting area without crossing street", dat$Waiting.behaviors)
tfac <- c("Pressed pedestrian push-button", "Paced or otherwise seemed impatient", "Left waiting area without crossing street")
for (k in tfac) {
  dat$Waiting.behaviors <- gsub(k, "", dat$Waiting.behaviors)
}; rm(k)
dat$Waiting.behaviors <- gsub(";", "", dat$Waiting.behaviors)
dat$Waiting.behaviors <- gsub(", ", "", dat$Waiting.behaviors)
dat1$WaitBehOther <- ifelse(dat$Waiting.behaviors!="", T, F)
dat1$WaitBehOtherText <- ifelse(dat1$WaitBehOther, dat$Waiting.behaviors, "")
dat <- cbind(dat1, dat2)
rm(dat1, dat2, j, tfac)
summary(dat$WaitBehPressed)
summary(dat$WaitBehPaced)
summary(dat$WaitBehLeft)
summary(dat$WaitBehOther)
table(dat$WaitBehOtherText)

# X.If.left.without.crossing.Time.left.waiting.area.
names(dat)[names(dat)=="X.If.left.without.crossing.Time.left.waiting.area."] <- "TimeWaitDep"
dat$temp <- as.POSIXct(dat$Date, tz="America/Denver")
for (i in 1:nrow(dat)) {
  if (is.na(dat$TimeWaitDep[i])) { dat$temp[i] <- NA }
  else if (dat$TimeWaitDep[i]=="") { dat$temp[i] <- NA }
  else { dat$temp[i] <- as.POSIXct(paste(dat$Date[i], dat$TimeWaitDep[i]), tz="America/Denver") }
}; rm(i)
dat$TimeWaitDep <- dat$temp
dat$temp <- NULL
summary(dat$TimeWaitDep)

# Anything.else.to.tell.us.about.waiting..
names(dat)[names(dat)=="Anything.else.to.tell.us.about.waiting."] <- "Notes4Waiting"
table(dat$Notes4Waiting)

# Crossing.location
table(dat$Crossing.location)
j <- which(names(dat)=="Crossing.location")
dat1 <- dat[,1:(j-1)]
dat2 <- dat[,(j+1):ncol(dat)]
dat1$CrossLoc <- dat$Crossing.location
tfac <- c("In the crosswalk or the crosswalk area", "Mid-block, away from the crosswalk", "In the middle of the intersection")
dat1$CrossLoc <- ifelse(dat1$CrossLoc %in% tfac, dat1$CrossLoc, "Other")
dat1$CrossLoc <- factor(dat1$CrossLoc, levels=c(tfac, "Other"))
dat1$CrossLocOtherText <- ifelse(dat1$CrossLoc=="Other", dat$Crossing.location, "")
dat <- cbind(dat1, dat2)
rm(dat1, dat2, j, tfac)
summary(dat$CrossLoc)
table(dat$CrossLocOtherText)
table(dat$CrossLoc, dat$WaitBehLeft)
# dat[which(dat$CrossLoc=="Other" & dat$WaitBehLeft!=T), c("Folder", "Timestamp")]
# dat[which(dat$CrossLoc!="Other" & dat$WaitBehLeft==T), c("Folder", "Timestamp")]
# dat[which(dat$CrossLoc=="Mid-block, away from the crosswalk" & dat$WaitBehLeft!=T), c("Folder", "Timestamp")]
# dat[which(dat$CrossLoc=="In the middle of the intersection" & dat$WaitBehLeft!=T), c("Folder", "Timestamp")]
# checked and fixed manually, see "fix3-crossloc"

# Crossing.direction
table(dat$Crossing.direction)
j <- which(names(dat)=="Crossing.direction")
dat1 <- dat[,1:(j-1)]
dat2 <- dat[,(j+1):ncol(dat)]
dat1$CrossDir <- dat$Crossing.direction
tfac <- c("Left to right (-->)", "Right to left (<--)")
dat1$CrossDir <- ifelse(dat1$CrossDir %in% tfac, dat1$CrossDir, "Other")
dat1$CrossDir <- factor(dat1$CrossDir, levels=c(tfac, "Other"))
dat1$CrossDirOtherText <- ifelse(dat1$CrossDir=="Other", dat$Crossing.direction, "")
dat <- cbind(dat1, dat2)
rm(dat1, dat2, j, tfac)
summary(dat$CrossDir)
table(dat$CrossDirOtherText)

# Time.departed.curb
names(dat)[names(dat)=="Time.departed.curb"] <- "TimeCurbDep"
dat$temp <- as.POSIXct(dat$Date, tz="America/Denver")
for (i in 1:nrow(dat)) {
  if (is.na(dat$TimeCurbDep[i])) { dat$temp[i] <- NA }
  else if (dat$TimeCurbDep[i]=="") { dat$temp[i] <- NA }
  else { dat$temp[i] <- as.POSIXct(paste(dat$Date[i], dat$TimeCurbDep[i]), tz="America/Denver") }
}; rm(i)
dat$TimeCurbDep <- dat$temp
dat$temp <- NULL
summary(dat$TimeCurbDep)
# fixed manually, see "fix2-time" and "checkcurbtimes" later

# Number.of.other.people.crossing.same.direction.
names(dat)[names(dat)=="Number.of.other.people.crossing.same.direction."] <- "CrossOtherPeopleSame"
dat$CrossOtherPeopleSame <- as.integer(dat$CrossOtherPeopleSame)
table(dat$CrossOtherPeopleSame)
summary(dat$CrossOtherPeopleSame)
# dat[which(is.na(dat$CrossOtherPeopleSame) & dat$WaitBehLeft!=T), c("Folder", "Timestamp")]
# missing were fixed manually in original CSV files

# Number.of.other.people.crossing.opposite.direction.
names(dat)[names(dat)=="Number.of.other.people.crossing.opposite.direction."] <- "CrossOtherPeopleOppo"
dat$CrossOtherPeopleOppo <- as.integer(dat$CrossOtherPeopleOppo)
table(dat$CrossOtherPeopleOppo)
summary(dat$CrossOtherPeopleOppo)
# dat[which(is.na(dat$CrossOtherPeopleOppo) & dat$WaitBehLeft!=T), c("Folder", "Timestamp")]
# missing were fixed manually in original CSV files

# Crosswalk.markings
table(dat$Crosswalk.markings)
j <- which(names(dat)=="Crosswalk.markings")
dat1 <- dat[,1:(j-1)]
dat2 <- dat[,(j+1):ncol(dat)]
dat1$CrossMarkInAll <- grepl("Stayed within the crosswalk markings for all or almost the whole crossing", dat$Crosswalk.markings)
dat1$CrossMarkInOut <- grepl("Stepped outside of the crosswalk markings for part of the crossing", dat$Crosswalk.markings)
dat1$CrossMarkOutAll <- grepl("Was outside of the crosswalk markings for most if not all of the crossing", dat$Crosswalk.markings)
tfac <- c("Stayed within the crosswalk markings for all or almost the whole crossing", "Stepped outside of the crosswalk markings for part of the crossing", "Was outside of the crosswalk markings for most if not all of the crossing")
for (k in tfac) {
  dat$Crosswalk.markings <- gsub(k, "", dat$Crosswalk.markings)
}; rm(k)
dat$Crosswalk.markings <- gsub(";", "", dat$Crosswalk.markings)
dat$Crosswalk.markings <- gsub(", ", "", dat$Crosswalk.markings)
dat1$CrossMarkOther <- ifelse(dat$Crosswalk.markings!="", T, F)
dat1$CrossMarkOtherText <- ifelse(dat1$WaitBehOther, dat$Crosswalk.markings, "")
dat <- cbind(dat1, dat2)
rm(dat1, dat2, j, tfac)
summary(dat$CrossMarkInAll)
summary(dat$CrossMarkInOut)
summary(dat$CrossMarkOutAll)
summary(dat$CrossMarkOther)
table(dat$CrossMarkOtherText)
table(dat$CrossLoc, dat$CrossMarkOutAll)
# dat[which(dat$CrossLoc=="Mid-block, away from the crosswalk" & dat$CrossMarkOutAll==F), c("Folder", "Timestamp")]
# dat[rowSums(dat[,c("CrossMarkInAll", "CrossMarkInOut", "CrossMarkOutAll", "CrossMarkOther")])==0 & dat$WaitBehLeft!=T, c("Folder", "Timestamp")]
# missing were fixed manually in original CSV files

# Crossing.behaviors
table(dat$Crossing.behaviors)
dat$Crossing.behaviors[is.na(dat$Crossing.behaviors)] <- ""
j <- which(names(dat)=="Crossing.behaviors")
dat1 <- dat[,1:(j-1)]
dat2 <- dat[,(j+1):ncol(dat)]
dat1$CrossBehSpeed <- grepl("Changed speed", dat$Crossing.behaviors)
dat1$CrossBehPaused <- grepl("Paused in the middle of the street", dat$Crossing.behaviors)
dat1$CrossBehDistracted <- grepl("Seemed distracted by phone or something else", dat$Crossing.behaviors)
tfac <- c("Changed speed \\(e.g., walk to run, or run to walk\\)", "Paused in the middle of the street", "Seemed distracted by phone or something else")
for (k in tfac) {
  dat$Crossing.behaviors <- gsub(k, "", dat$Crossing.behaviors)
}; rm(k)
dat$Crossing.behaviors <- gsub(";", "", dat$Crossing.behaviors)
dat$Crossing.behaviors <- gsub(", ", "", dat$Crossing.behaviors)
dat1$CrossBehOther <- ifelse(dat$Crossing.behaviors!="", T, F)
dat1$CrossBehOtherText <- ifelse(dat1$CrossBehOther, dat$Crossing.behaviors, "")
dat <- cbind(dat1, dat2)
rm(dat1, dat2, j, tfac)
summary(dat$CrossBehSpeed)
summary(dat$CrossBehPaused)
summary(dat$CrossBehDistracted)
summary(dat$CrossBehOther)
table(dat$CrossBehOtherText)

# Crossing.obstacles
table(dat$Crossing.obstacles)
dat$Crossing.obstacles[is.na(dat$Crossing.obstacles)] <- ""
j <- which(names(dat)=="Crossing.obstacles")
dat1 <- dat[,1:(j-1)]
dat2 <- dat[,(j+1):ncol(dat)]
dat1$CrossObsCar <- grepl("Car blocking the crosswalk", dat$Crossing.obstacles)
dat1$CrossObsSWD <- grepl("Snow pile, water puddle, or debris", dat$Crossing.obstacles)
tfac <- c("Car blocking the crosswalk", "Snow pile, water puddle, or debris")
for (k in tfac) {
  dat$Crossing.obstacles <- gsub(k, "", dat$Crossing.obstacles)
}; rm(k)
dat$Crossing.obstacles <- gsub(";", "", dat$Crossing.obstacles)
dat$Crossing.obstacles <- gsub(", ", "", dat$Crossing.obstacles)
dat1$CrossObsOther <- ifelse(dat$Crossing.obstacles!="", T, F)
dat1$CrossObsOtherText <- ifelse(dat1$CrossObsOther, dat$Crossing.obstacles, "")
dat <- cbind(dat1, dat2)
rm(dat1, dat2, j, tfac)
summary(dat$CrossObsCar)
summary(dat$CrossObsSWD)
summary(dat$CrossObsOther)
table(dat$CrossObsOtherText)

# Time.arrived.curb
names(dat)[names(dat)=="Time.arrived.curb"] <- "TimeCurbArr"
dat$temp <- as.POSIXct(dat$Date, tz="America/Denver")
for (i in 1:nrow(dat)) {
  if (is.na(dat$TimeCurbArr[i])) { dat$temp[i] <- NA }
  else if (dat$TimeCurbArr[i]=="") { dat$temp[i] <- NA }
  else { dat$temp[i] <- as.POSIXct(paste(dat$Date[i], dat$TimeCurbArr[i]), tz="America/Denver") }
}; rm(i)
dat$TimeCurbArr <- dat$temp
dat$temp <- NULL
summary(dat$TimeCurbArr)
# fixed manually, see "fix2-time" and "checkcurbtimes" later

# Anything.else.to.tell.us.about.the.crossing.
names(dat)[names(dat)=="Anything.else.to.tell.us.about.the.crossing."] <- "Notes5Crossing"
table(dat$Notes5Crossing)

# Destination.location
table(dat$Destination.location)
j <- which(names(dat)=="Destination.location")
dat1 <- dat[,1:(j-1)]
dat2 <- dat[,(j+1):ncol(dat)]
dat1$Destination <- dat$Destination.location
tfac <- c("To approach, left side", "To approach, right side", "To other street, left", "To other street, right", "To different crosswalk, left", "To different crosswalk, right")
dat1$Destination <- ifelse(dat1$Destination %in% tfac, dat1$Destination, "Other")
dat1$Destination <- factor(dat1$Destination, levels=c(tfac, "Other"))
dat1$DestinationOtherText <- ifelse(dat1$Destination=="Other", dat$Destination.location, "")
dat <- cbind(dat1, dat2)
rm(dat1, dat2, j, tfac)
summary(dat$Destination)
table(dat$DestinationOtherText)

# Anything.else.to.tell.us.about.this.pedestrian.crossing.event.
names(dat)[names(dat)=="Anything.else.to.tell.us.about.this.pedestrian.crossing.event."] <- "Notes7Final"
table(dat$Notes7Final)

# Remove extra column
dat$one <- NULL

########################################
# Add time difference variables

# Wait time
dat$TimeWait <- difftime(dat$TimeCurbDep, dat$TimeWaitArr, units="secs")
summary(as.numeric(dat$TimeWait))
# dat[which(is.na(dat$TimeWait) & dat$WaitBehLeft!=T), c("Folder", "Timestamp")]
# dat[which((dat$TimeWait <0 | dat$TimeWait >180) & dat$WaitBehLeft!=T), c("Folder", "Timestamp")]
# fixed manually, see "fix2-time" and "checkcurbtimes" later
# clean version
dat$TimeWaitClean <- ifelse(dat$TimeWait >=0 & dat$TimeWait <= 180, dat$TimeWait, NA)
summary(as.numeric(dat$TimeWaitClean))

# Crossing time
dat$TimeCurb <- difftime(dat$TimeCurbArr, dat$TimeCurbDep, units="secs")
summary(as.numeric(dat$TimeCurb))
# dat[which(is.na(dat$TimeCurb) & dat$WaitBehLeft!=T), c("Folder", "Timestamp")]
# dat[which((dat$TimeCurb <0 | dat$TimeCurb >60) & dat$WaitBehLeft!=T), c("Folder", "Timestamp")]
# fixed manually, see "fix2-time" and "checkcurbtimes" later
# clean version
dat$TimeCurbClean <- ifelse(dat$TimeCurb >=0 & dat$TimeCurb <= 60, dat$TimeCurb, NA)
summary(as.numeric(dat$TimeCurbClean))

########################################
# Inspect key variables

# CrossLoc
summary(dat$CrossLoc)
table(dat$CrossLocOtherText)
mytab <- table(dat$CrossLoc)
myptab <- prop.table(mytab)
print(t(t(mytab)))
pie(mytab, labels=paste(mytab, " (", round(100*myptab), "%), ", names(mytab), sep=""), 
    clockwise=T, col=c("#003366", "#a4aeb5", "#ffffff"), 
    main="Crossing location")
barplot(mytab, horiz=T, las=1, col=c("#003366", "#a4aeb5", "#ffffff"), 
        xlab="Frequency", ylab="Crossing location", main="Crossing location", 
        legend.text = row.names(mytab), args.legend = list(title="# cylinders", horiz=T))
rm(mytab, myptab)

# CrossMark
summary(dat$CrossMarkInAll)
summary(dat$CrossMarkInOut)
summary(dat$CrossMarkOutAll)
summary(dat$CrossMarkOther)
table(dat$CrossMarkOtherText)

# CrossBeh
summary(dat$CrossBehSpeed)
summary(dat$CrossBehPaused)
summary(dat$CrossBehDistracted)
summary(dat$CrossBehOther)
table(dat$CrossBehOtherText)

# CrossObs
summary(dat$CrossObsCar)
summary(dat$CrossObsSWD)
summary(dat$CrossObsOther)

# TimeWait
summary(as.numeric(dat$TimeWait))
hist(as.numeric(dat$TimeWait), breaks=c(-99999,0,10,20,30,40,50,60,99999))
head(sort(dat$TimeWait),50)
tail(sort(dat$TimeWait),50)
temp <- as.integer(dat$TimeWait)
temp <- temp[which(temp>=0)]
temp <- temp[which(temp<180)]
hist(temp, breaks=c(seq(0,180,10)), freq=F)
myhist <- hist(temp, breaks=c(seq(0,180,10)))
# write.csv(temp, file="TimeWait.csv")
rm(myhist, temp)

# TimeCurb
summary(as.numeric(dat$TimeCurb))
hist(as.numeric(dat$TimeCurb), breaks=c(-99999,0,10,20,30,40,50,60,99999))
head(sort(dat$TimeCurb),50)
tail(sort(dat$TimeCurb),50)
temp <- as.integer(dat$TimeCurb)
temp <- temp[which(temp>=0)]
temp <- temp[which(temp<90)]
hist(temp, breaks=c(seq(0,90,5)), freq=F)
myhist <- hist(temp, breaks=c(seq(0,90,5)))
# write.csv(temp, file="TimeCurb.csv")
rm(myhist, temp)

# Initialize
checkdat <- data.frame(j=0, Folder=NA, WaitBehLeft=NA, Timestamp=Sys.time(), TimeWaitArr=Sys.time(), TimeCurbDep=Sys.time(), TimeCurbArr=Sys.time(), TimeWait=0, TimeCurb=0)

# Check for consistent wait/departure/arrival times
for (j in 1:nrow(dat)) {
  Folder <- dat$Folder[j]
  WaitBehleft <- dat$WaitBehLeft[j]
  Timestamp <- dat$Timestamp[j]
  TimeWaitArr <- dat$TimeWaitArr[j]
  TimeCurbDep <- dat$TimeCurbDep[j]
  TimeCurbArr <- dat$TimeCurbArr[j]
  TimeWait <- dat$TimeWait[j]
  TimeCurb <- dat$TimeCurb[j]
  if (!is.na(TimeWaitArr) & !is.na(TimeCurbDep) & !is.na(TimeCurbArr) & (TimeCurbDep < TimeCurbArr) & (TimeCurb < 60) & (TimeWait < 180)) {
  } else {
    checkdat <- rbind(checkdat, list(j, Folder, WaitBehleft, Timestamp, TimeWaitArr, TimeCurbDep, TimeCurbArr, TimeWait, TimeCurb))
  }
  rm(Folder, WaitBehleft, Timestamp, TimeWaitArr, TimeCurbDep, TimeCurbArr, TimeWait, TimeCurb)
}; rm(j)
checkdat <- checkdat[-1,]

# Save
# write.csv(checkdat, file.path("Data", "checkcurbtimes.csv"), row.names=F)

# Remove
rm(checkdat)

########################################
# Save files

# Inspect
names(dat)
str(dat)
summary(dat)

# Save as dat02
saveRDS(dat, file.path("Data", "dat02.rds"))
write.csv(dat, file.path("Data", "dat02.csv"), row.names=F)

########################################
# Cleanup

# Remove
rm(dat)
gc()

########################################
# END
########################################