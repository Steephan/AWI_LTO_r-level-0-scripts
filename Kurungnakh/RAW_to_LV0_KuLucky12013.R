##################################################################
##
##  KuLucky12013      RAW to level 0
##
##
##
##
##
##  last modified: 26-03-2019
##
##################################################################


##################################################################
## step 1.01
## set path settings for different systems linux vs. windoof
##################################################################
# to run this script seperate, you have to uncomment the next 10 lines!
rm(list = ls())
# if (.Platform$OS.type == "windows") {
#  path<-read.table("N:/geo5/SoilData/doc/scripts/database_R/settings/sa_path_windoof.txt",sep="\t",header=T)
#  maint<-read.table("N:/geo5/SoilData/doc/scripts/database_R/settings/sa_maintance.txt",sep="\t",header=T)
#  source("N:/geo5/SoilData/doc/scripts/database_R/settings/db_func.R")
# }else{
#  path<-read.table("/geo5/SoilData/doc/scripts/database_R/settings/path_linux.txt",sep="\t",header=T,
#                   fileEncoding="UTF-8")
#  maint<-read.table("/geo5/SoilData/doc/scripts/database_R/settings/sa_maintance.txt",sep="\t",header=T)
#  source("/geo5/SoilData/doc/scripts/database_R/settings/db_func.R")
# }

library(tidyverse)
library(tidyr)

##################################################################
## step 1.02
## set running options years, ...
##################################################################
options(scipen = 100) # for non exonential display of numeric values
options(digits = 17)
origin <- "1970-01-01"
aktuell <- as.numeric(format(Sys.Date(), "%Y"))

##################################################################
## step 1.03
## loop 1 over years
##################################################################
for (year in 2013:2014) { # 2014:aktuell

  ##################################################################
  ## step 1.04
  ## set 2 empty tables with length of year
  ## columns: 2 (date table) and number of input table
  ##################################################################
  cat("\nProcessing year", year, "\n====================\n\n")
  start.date <- as.POSIXct(paste(year, "-01-01 00:00:00", sep = ""), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  end.date <- as.POSIXct(paste(year, "-12-31 23:50:00", sep = ""), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  # create empty data frame with UTC time stamp every 10 min
  db.kull2 <- matrix(ncol = 1, nrow = length(seq(start.date, end.date, by = "1 h")))
  # db.kull2[,c(2:3)]<-NA
  compl.q <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "1 h")))
  db.kull2[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "1 h"), format = "%Y-%m-%d %H:%M:%S"))
  compl.q[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "1 h"), format = "%Y-%m-%d %H:%M:%S"))
  colnames(db.kull2) <- c("UTC")
  colnames(compl.q) <- c("UTC", "erste")


  ##################################################################
  ## step 1.05
  ## set input.path and list all files
  ##################################################################
  # inz.path<-paste0(path$w[path$n=="RAW.p"],"KuQ1/")
  # files2read<-list.files(inz.path,pattern="*.dat")
  path <- "N:/sparc/data/LTO/"
  files2read <- list.files(paste(path, "raw/KuLucky12013/", sep = ""), pattern = "*.csv")

  m <- 0
  ##################################################################
  ## step 1.06
  ## loop 2 over years
  ##################################################################
  for (i in 1:length(files2read)) {

    ##################################################################
    ## step 1.07
    ## read one file (skip headers, set NA-values)
    ## set temporal colnames
    ##################################################################
    cat("\nprocessing ", files2read[i], "\n====================\n\n")
    dada <- read.table(paste0(path, "raw/KuLucky12013/", files2read[i]), sep = ",", dec = ".", header = F, fill = T, na.strings = "n/a", skip = 3)
    colnames(dada) <- paste0("V", seq_len(ncol(dada)))

    ##################################################################
    ## step 1.08
    ##
    ##################################################################
    dada <- select(dada, c("V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11"))
    dada[, 1] <- as.POSIXct(dada[, 1], format = "%d-%m-%Y %H:%M", tz = "UTC")
    dada[, 1] <- format(dada[, 1], formate = "%Y-%m-%d %H:%M")

    ##################################################################
    ## step 1.09
    ## check file if dates are in running years of loop 1
    ##################################################################
    # skip file if wrong year
    if (as.numeric(substr(lapply(dada[1, 1], as.character), 1, 4)) > year || as.numeric(substr(lapply(
      dada[length(dada[, 1]), 1],
      as.character
    ), 1, 4)) < year) {
      next
    }
    cat(paste(dada[1, 1], "to", dada[length(dada[, 1]), 1], "", files2read[i]))

    m <- m + 1

    ##################################################################
    ## step 1.10
    ## check file for double entries
    ##################################################################
    if ("TRUE" %in% duplicated(dada) == TRUE) { # check for fully double entries
      double <- duplicated(dada)
      cat(paste(
        length(which(double == "TRUE")), "duplicated records found in file", files2read[i], "\n",
        "first entry:", dada(which(double == "TRUE")[1], 1), "\n last entry:", dada(which(double == "TRUE")
        [length(which(double == "TRUE"))]), "\n\n"
      ))
      dada <- unique(dada) # remove double entries
    } else if (("TRUE" %in% duplicated(dada[, 1])) == TRUE) { # chech for multiple entries with the same timestamp
      double <- duplicated(dada[, 1])
      cat(paste(
        length(which(double == "TRUE")), "multiple records found in file", files2read[i], "\n",
        "first entry:", dada[which(double == "TRUE")[1], 1], "\n last entry:", dada[which(double == "TRUE")
        [length(which(double == "TRUE"))], 1], "\n\n"
      ))
      dd <- which(dada[, 1] %in% dada[which(double == "TRUE"), 1])
      dada <- dada[-dd, ] # remove multiple records
    } else {
      cat(paste("\n", "no double entries found in file", files2read[i], "\n\n"))
    }

    ##################################################################
    ## step 1.11
    ## convert date to numeric vector
    ##################################################################
    dada[, 1] <- as.numeric(as.POSIXct(dada[, 1], format = "%Y-%m-%d %H:%M:%S", origin = origin, tz = "UTC"))

    ##################################################################
    ## step 1.12
    ## set original colnames
    ##################################################################
    colnames(dada) <- c("UTC", "Temp_0m_surface", "Temp_0.5m", "Temp_1.0m", "Temp_1.5m", "Temp_2.0m", "Temp_2.5m", "Temp_x.xm_Ground", "STABW")

    ##################################################################
    ## step 1.13
    ## new arrangement/ order of columns (all temperatures together, ...)
    ##################################################################

    ##################################################################
    ## step 1.14
    ## merge input table with date table
    ##################################################################
    newdf.a <- merge(compl.q, dada, all.x = T, by = "UTC")

    ##################################################################
    ## step 1.15
    ## merge date table with storing table
    ##################################################################
    db.kull2 <- merge(newdf.a, db.kull2, all.x = T, by = "UTC")

    # for(k in 2:(length(db.kull2[1,]))){
    #  db.kull2[,k]<-newdf.a[,k+1]
    # }
  }

  # write values into first columns and remove merging columns
  if (m > 1) {
    for (k in 1:(m - 1)) {
      for (l in 2:(length(newdf.a[1, ]))) {
        for (s in 1:length(db.kull2[, 1])) {
          if (is.na(db.kull2[s, l])) {
            db.kull2[s, l] <- db.kull2[s, (l + length(newdf.a[1, ]) - 1)]
          }
        }
      }
      db.kull2 <- db.kull2[, -c((length(newdf.a) + 1):(2 * length(newdf.a) - 1))]
    }
  }
  db.kull2 <- db.kull2[, -2]

  db.kull2 <- as.data.frame(db.kull2)
  ##################################################################
  ## step 1.16
  ## convert numeric dates back to date format, name columns
  ##################################################################
  db.kull2[, 1] <- format(as.POSIXct(db.kull2[, 1], origin = origin, tz = "UTC"), format = "%Y-%m-%d %H:%M:%S")

  ##################################################################
  ## step 1.17
  ## set "sparc" colnames
  ##################################################################
  colnames(db.kull2) <- c("UTC", "Tw_0", "Tw_0.5", "Tw_1", "Tw_1.5", "Tw_2", "Tw_2.5", "Tw_unknown", "SQw")

  ##################################################################
  ## step 1.18
  ## save data to txt-file
  ##################################################################
  write.table(db.kull2, # [as.numeric(format(as.POSIXct(db.kull2[,1],format="%Y-%m-%d %H:%M:%D",origin=origin,tz="UTC"),
    # format="%Y"))==year,-2],
    paste0(path, "level0/KuLucky12013/00_full_dataset/KuLucky12013_", year, "_lv0.dat"),
    quote = F, dec = ".", sep = ",", row.names = F
  )
}
cat("\n#\n# KuLucky12013 without problems! \n#\n")


# control plot
# ggplot(db.kull2)+
#  geom_point(mapping = aes(UTC,P_raw),size=0.1)+
#  geom_point(mapping = aes(UTC,P),size=0.1,col="blue")
# ggplot(db.kull2)+
#  geom_point(mapping = aes(UTC,WT),size=0.1,col="red")
###################################################################
#
# Erklärung Tabellenkopf:
# UTC - time (format: YY-MM-DD HH:MM:SS)
# P - hydrostatic pressure, measured by U20-Onset Sensor
# Tw_unknown - water temperature in unknown depth, measured by U20-Onset sensor
# roh_unknown - water density in unknown water depth
# PA - atmospheric pressure at Samoylov Meteorological Sataion (SaMet2002)
# WL - water level
