#######################################
# Project:  UDOT-19.312 Right turn safety
# Author:   Alyssa Gaither (alyssa.gaither@usu.edu)
#           Patrick Singleton (patrick.singleton@usu.edu)
# File:     Location-Data Combination.R
# Date:     2022 Summer, Fall, 2023 Spring
# About:    Combine Location Data
########################################

########################################
# Notes

# Open R Project file, then open this script

# Major Edits
# 2022-06-23 created AG
# 2022-07-22 updated PAS changed file paths, added RDS output, removed unnecessary columns
# 2022-10-10 updated PAS combined with other corner-location script
# 2022-02-03 updated PAS greatly simplified script, 
# - because using revised source data files (especially New_Videos.xlsx)
#   based on manual edits to datlocation_PAS.xlsx

# Load packages
library(readxl)

########################################
# Load data

# Data from videos
dat <- read_excel(file.path("Data", "Data from Videos", "New_Videos.xlsx"), 1)
dat <- as.data.frame(dat)
str(dat)

# Corner information for intersections with cameras
# NOTE: Relevant data have been transferred and checked/edited in New_videos.xlsx
corners <- read_excel(file.path("Data", "Locations", "Signal Corner Info_21.xlsx"), 1)
corners <- as.data.frame(corners)
str(corners)

# Intersection data about built environment
intdata <- readRDS(file.path("Data", "Ints", "intdata.rds"))
str(intdata)

# Signal data about crossings and legs
# NOTE: Relevant data have been transferred and checked/edited in New_videos.xlsx
sigdata <- read.csv(file.path("Data", "Locations", "Signal Database_Trial.csv"))
str(sigdata)

########################################
# Process data

# dat
temp1 <- dat[,!(names(dat) %in% c("Source", "Done"))]

# corners
# nothing to do

# intdata
cols_notneeded <- c("TYPE", "COMM", "ST_EW", "ST_NS", "CITY", "REGION", "OWNER", "XC", "YC", 
                    grep("AAD_", names(intdata), value=T), grep("AAH", names(intdata), value=T), 
                    grep("_hami", names(intdata), value=T))
temp3 <- intdata[,!(names(intdata) %in% cols_notneeded)]
rm(cols_notneeded)

# sigdata
# nothing to do

########################################
# Merge data

# Merge
temp <- merge(temp1, temp3, by.x="Signal", by.y="SIGNAL")

# Inspect
str(temp)
summary(temp)
View(temp)

# Manually edit for missing built environment information
# - NOTE: These were manually calculated. 
# library(mapview)
# library(sf)
# sftemp <- st_as_sf(temp, coords=c("LONG", "LAT"), crs=4326)
# mapview(sftemp)
# sftemp_qtmi <- st_buffer(sftemp, 400)
# mapview(sftemp_qtmi, label="Signal", popup=F)
# rm(sftemp, sftemp_qtmi)
# many missing for 8304
# - okay, will be removed in modeling, only 6 conflict observations
# avgveh_qtmi
temp$Signal[is.na(temp$avgveh_qtmi)]
temp$avgveh_qtmi[temp$Signal==5139] <- 4367/1721
# per4wy_qtmi
temp$Signal[is.na(temp$per4wy_qtmi)]
temp$per4wy_qtmi[temp$Signal==6093] <- 100*(4/(4+8))
temp$per4wy_qtmi[temp$Signal==7355] <- 100*(3/(3+11))
# stops_qtmi & stopden_qtmi
temp$Signal[is.na(temp$stops_qtmi) & is.na(temp$stopden_qtmi)]
temp$stops_qtmi[temp$Signal==5205] <- 0
temp$stops_qtmi[temp$Signal==6046] <- 0
temp$stops_qtmi[temp$Signal==6093] <- 0
temp$stops_qtmi[temp$Signal==6190] <- 0
temp$stops_qtmi[temp$Signal==6390] <- 0
temp$stops_qtmi[temp$Signal==7070] <- 0
temp$stops_qtmi[temp$Signal==7355] <- 0
temp$stops_qtmi[temp$Signal==7391] <- 0
tempsigs <- c(5205,6046,6093,6190,6390,7070,7355,7391)
temp$stopden_qtmi[temp$Signal %in% tempsigs] <- temp$stops_qtmi[temp$Signal %in% tempsigs] / temp$area_sqmi_qtmi[temp$Signal %in% tempsigs]
rm(tempsigs)

# Inspect
str(temp)
summary(temp)
View(temp)

########################################
# Save data

# Save
write.csv(temp, file.path("Analysis", "Conflict Analysis", "datlocation.csv"), row.names=F)
saveRDS(temp, file.path("Analysis", "Conflict Analysis", "datlocation.rds"))
# NOTE: manually copy files to Analysis/Conflict Analysis/Location Data Assembly when ready to overwrite

########################################
# Cleanup

# Remove
rm(temp1, temp3, temp)
rm(dat, corners, intdata, sigdata)
gc()

########################################
# END
########################################