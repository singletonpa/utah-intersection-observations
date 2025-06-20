#######################################
# Project:  UDOT-20.313 Ped violations
# Authors:  Patrick A. Singleton (patrick.singleton@usu.edu)
#           Sadie Boyer (sadie.boyer@usu.edu)
# File:     01_Merge_Raw_Data.R
# Date:     2021 Summer, Fall, 2022 Spring, Summer
# About:    Assemble data from videos
########################################

########################################
# Notes

# Open R project first, then open this R script

# Major edits
# 2021-06-05 PAS created
# 2021-09-03 PAS updated: more folders, new date-time processing
# 2022-03-31 SB  updated
# 2022-08-11 PAS updated: removed column processing code, to script 02

# Load libraries
# library("")

########################################
# Load and merge data

# Get folders
mypath <- file.path("Data", "UDOT Ped Videos", "2_Checked")
myfolders <- list.dirs(file.path(mypath), full.names=F, recursive=F)
myfolders

# Initialize
i <- 1L
print(myfolders[i])
myfiles <- list.files(file.path(mypath, myfolders[i]), pattern=".csv")
myfiles
if (paste0(myfolders[i], ".csv") %in% myfiles) {
  tdat <- read.csv(file.path(mypath, myfolders[i], paste0(myfolders[i], ".csv")), stringsAsFactors=F)
  tdat <- data.frame(Folder=myfolders[i], tdat, stringsAsFactors=F)
  names(tdat) <- gsub("[[:punct:]]+", ".", names(tdat))
  dat <- tdat
  rm(tdat)
} else { print("Missing") }
rm(myfiles)
rm(i)

# For loop over folders
for (i in 2:length(myfolders)) {
  print(myfolders[i])
  myfiles <- list.files(file.path(mypath, myfolders[i]), pattern=".csv")
  myfiles
  if (paste0(myfolders[i], ".csv") %in% myfiles) {
    tdat <- read.csv(file.path(mypath, myfolders[i], paste0(myfolders[i], ".csv")), stringsAsFactors=F)
    tdat <- data.frame(Folder=myfolders[i], tdat, stringsAsFactors=F)
    names(tdat) <- gsub("[[:punct:]]+", ".", names(tdat))
    tdat[,is.na(names(tdat))] <- NULL
    tdat[,"X"] <- NULL
    dat <- rbind(dat, tdat)
    rm(tdat)
  } else { print("Missing") }
  rm(myfiles)
}; rm(i)

########################################
# Save files

# Inspect
names(dat)
str(dat)
summary(dat)

# Save as dat01
saveRDS(dat, file.path("Data", "dat01.rds"))
write.csv(dat, file.path("Data", "dat01.csv"), row.names=F)

########################################
# Cleanup

# Remove
rm(myfolders, mypath)
rm(dat)
gc()

########################################
# END
########################################