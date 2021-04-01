#############################################################################
##
##   BaSnow2019         RAW to Level0
##
##   equal time steps, no gaps
##
#############################################################################
##
## open issues:
##
##
##
#############################################################################
##
##   written by: stephan.lange@awi.de
##               christian.lehr@awi.de
##
##   last check: 2020-01-22
##   checked by: christian.lehr@awi.de
##
#############################################################################
##
## last modifications:
##  2020-08-18 CL include calculation of "rho_K" (snow density calculated from "SWE_K" and "Dsn")
##  2020-08-18 CL "TL" and "KTL" changed to "Tl" and "KTl" (Tl: Thallium)
##  2020-04-14 CL change loop to (year in run.year) to allow the selection of the processed year in Bayelva_MAIN.R
##
#############################################################################
##
## Comments:
##
## There are two cases:
## CASE 1 - BaSnow2019_SR50.dat
## CASE 2 - BaSnow2019_CS725.dat
##
## please update BaSnow2019sr before BaSnow2019cs because BaSnow2019cs
## inherits "Dsn" from BaSnow2019sr
## consequently, the Dsn_flag is likewise inherited (see LV0_to_LV1_BaAll.R)
##
## also, "Dsn" in BaSnow2019cs is used together with "SWE_K" to calculate snow density ("rho_K")
##
## For the calculation of snowheight from distcorr (CASE 1 - BaSnow2019_SR50.dat),
## the height of the sensor has to be checked / defined every year
## ==> step 1.19 calculate Snowheight from distcorr
##
##
#############################################################################
## step 1.01
## set path settings for different systems linux vs. windoof
#############################################################################
# to run this script separately, you have to uncomment the next 10 lines!
# rm(list = ls())
# if (.Platform$OS.type == "windows") {
#   p.1 <- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/path_win.txt", sep = "\t", header = T)
#   p.1maint <- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)
# 
#   source("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_func.R")
# } else {
#   p.1 <- read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
#   p.1maint <- read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)
# 
#   source("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_func.R")
# }
#############################################################################
## step 1.02
## set running options years, ...
#############################################################################
# options(scipen = 100) # for non-exponential display of numeric values
# origin <- "1970-01-01"
# recent.year <- as.numeric(format(Sys.Date(), "%Y"))
# 
########
# to run this script separately, you have to set run.year:
#
# run.year <- recent.year
# run.year <- 2020
#######

#############################################################################
## CASE 1 - BaSnow2019_SR50.dat
#############################################################################
## step 1.03
## loop 1 over years
#############################################################################

origin <- "1970-01-01"

for (year in run.year) {
  #############################################################################
  ## step 1.04
  ## set 2 empty tables with length of year
  ## columns: 2 (date table) and number of input table (storing table)
  #############################################################################
  #cat("\nProcessing year",year,"\n====================\n\n")
  start.date <- as.POSIXct(paste(year, "-01-01 00:00:00", sep = ""), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date <- as.POSIXct(paste(year, "-", 12,"-", 31," 23:00:00", sep = ""), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  # create empty data frame with UTC time stamp every 60 min
  db.basnow.sr <- matrix(ncol = 7, nrow = length(seq(start.date, end.date, by = "60 min")), -999)
  db.basnow.sr[, c(2:7)] <- NA
  compl.temp <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "60 min")))
  db.basnow.sr[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "60 min"), format = '%Y-%m-%d %H:%M:%S'))
  compl.temp[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date,by = "60 min"), format = '%Y-%m-%d %H:%M:%S'))
  colnames(compl.temp) <- c("UTC","erste")
  #############################################################################
  ## step 1.05
  ## set input.path and list all files
  #############################################################################
  inz.path <- paste0(p.1$w[p.1$n == "ONL.p"], "BaSnow2019sr/")
  files2read <- list.files(inz.path, pattern = "*.dat")
  #############################################################################
  ## step 1.06
  ## loop 2 over all files
  #############################################################################

  for (i in 1:length(files2read)) {
    #############################################################################
    ## step 1.07
    ## read one file (skip headers, set NA-values)
    ## set temporal colnames
    #############################################################################
    #cat("\nprocessing ",files2read[i],"\n====================\n\n")
    dada <- read.table(paste(inz.path, files2read[i], sep = ""), sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")

    colnames(dada) = paste0("V", seq_len(ncol(dada)))
    #############################################################################
    ## step 1.09
    ## check file if dates are in running year of loop 1
    #############################################################################

    if (as.numeric(substr(lapply(dada[1, 1], as.character), 1, 4)) > year || as.numeric(substr(lapply(dada[length(dada[, 1]), 1], as.character), 1, 4)) < year) {next} # skip file if wrong year
    # print to check whether date of the read data is assigned to the correct year
    # cat(paste(dada[1,1],"     to     ",dada[length(dada[,1]),1],"    ",files2read[i], "\n"))
    #############################################################################
    ## step 1.10
    ## check file for double entries
    #############################################################################
    if (("TRUE" %in% duplicated(dada)) == TRUE) { # check for fully double entries
      doouble <- duplicated(dada)
      cat(paste(length(which(doouble == "TRUE")), "duplicated records found in file", files2read[i], "\n",
                "first entry:", dada[which(doouble == "TRUE")[1], 1], "\n  last entry:", dada[which(doouble == "TRUE")[length(which(doouble == "TRUE"))], 1], "\n\n"))
      dada <- unique(dada)  # remove double entries
    } else if (("TRUE" %in% duplicated(dada[, 1])) == TRUE) {  # check for multiple different data records for same! timestamp
      doouble <- duplicated(dada[, 1])
      cat(paste(length(which(doouble == "TRUE")),"multiple records found in file", files2read[i], "\n",
                "first entry:", dada[which(doouble == "TRUE")[1], 1], "\n  last entry:", dada[which(doouble == "TRUE")[length(which(doouble == "TRUE"))], 1], "\n\n"))
      dd <- which(dada[, 1] %in% dada[which(doouble == "TRUE"), 1])
      dada <- dada[-dd, ]  # remove double entries
    } else {
      cat("No double data entries found in", files2read[i], "\n\n")
    }

    #############################################################################
    ## step 1.11
    ## convert date to numeric value
    #############################################################################
    dada[, 1] <- as.numeric(as.POSIXct(dada[, 1], format = '%Y-%m-%d %H:%M:%S', origin = origin, tz = "UTC"))
    #############################################################################
    ## step 1.12a
    ## special case: former files with different columns
    ## set colnames
    #############################################################################
    if (length(dada[1, ]) == 7) {
      colnames(dada) <- c("UTC","RECORD","AirTempC","TCDT","Q","DT","RawQ")
      #############################################################################
      ## step 1.12b
      ## add additional columns to former dataset
      #############################################################################
      # dada$Dsn  <-NA  # column for calculated snowdepth

    } else {
    #############################################################################
    ## step 1.12
    ## standard case
    ## set original colnames
    #############################################################################
    colnames(dada) <- c("UTC", "RECORD", "AirTempC", "TCDT", "Q", "DT", "RawQ")
    }

    #############################################################################
    ## step 1.13
    ## new arrangement / order of columns (all Temperatures together, ascending, ... )
    ## exclude column "RECORD"
    #############################################################################
    dada <- dada[, c("UTC", "AirTempC", "TCDT", "Q", "DT", "RawQ")]
    #############################################################################
    ## step 1.14
    ## merge input data with date table
    #############################################################################

    newdf.a <- merge(compl.temp, dada, all.x = T, by = "UTC")

    #############################################################################
    ## step 1.15
    ## merge date table with storing table
    #############################################################################

    for (k in 2:(length(dada[1, ]))) {
      db.basnow.sr[, k] <- rowMeans(cbind(db.basnow.sr[, k], newdf.a[, k + 1]), na.rm = T)#
    }
  }

  #############################################################################
  ## step 1.16
  ## convert numeric dates back to date format
  #############################################################################

  db.basnow.sr[, 1] <- format(as.POSIXct(db.basnow.sr[, 1], origin = origin, tz = "UTC"), format = '%Y-%m-%d %H:%M')

  #############################################################################
  ## step 1.17
  ## set "sparc" colnames
  #############################################################################

  colnames(db.basnow.sr) <- c("UTC", "Tair_200", "distcor", "QA", "distraw", "QA_raw", "Dsn")

  #############################################################################
  ## step 1.18
  ## Set NAN to NA
  #############################################################################

  for (val in 2:7) {
    db.basnow.sr[which(is.nan(as.numeric(db.basnow.sr[, (val)])) == TRUE), val] <- NA   # set NAN to NA
  }

  #############################################################################
  ## step 1.19
  ## calculate Snowheight from distcorr
  #############################################################################

  if (year == 2019) {###  special calculation of snowdepth
    ####
    snow.free <- c(which(db.basnow.sr[, 1] == "2019-07-27 08:00"),
                   which(db.basnow.sr[, 1] == "2019-11-14 12:00")) # vector of two timesteps
    # two vectors for snowdepth calculation
    #  spring.corr <- 3.08 # the old from last year ####### Dsn correction #########
    autum.corr1 <- 3.08 # the new one based on maximum dist in august ######## distcor correction #########
    autum.corr2 <- 2.33 # since  "2019-11-14 12:00"   check again in summer 2020, old 2.42 height of gamma sonde
    ind.snow1 <- snow.free[1]:snow.free[2]
    ind.snow2 <- snow.free[2]:length(db.basnow.sr[, 1])
    #  db.basnow.sr[1:snow.free[1],10] <- round(spring.corr[1]-as.numeric(db.basnow.sr[1:snow.free[1],9]),3)
    #db.basnow.sr[snow.free[1]:snow.free[2],9*i]<-0 # snowfree
    db.basnow.sr[ind.snow1, "Dsn"] <- round(autum.corr1[1] - as.numeric(db.basnow.sr[ind.snow1, "distcor"]), 3)
    db.basnow.sr[ind.snow2, "Dsn"] <- round(autum.corr2[1] - as.numeric(db.basnow.sr[ind.snow2, "distcor"]), 3)
  }

  if (year == 2020) {###  special calculation of snowdepth
  ####
    snow.free <- c(which(db.basnow.sr[, 1] == "2020-06-26 14:00"),  # OLD from previous year "2020-07-27 08:00"),
                   which(db.basnow.sr[, 1] == "2020-11-14 12:00")) # vector of two timesteps
    # two vectors for snowdepth calculation
    spring.corr <- 2.33 # the old from last year ####### Dsn correction #########
    autum.corr <- 2.31 # the new one
    winter.corr <- 2.31

    ind.snow1 <-            1:snow.free[1]
    ind.snow2 <- snow.free[1]:snow.free[2]
    ind.snow3 <- snow.free[2]:length(db.basnow.sr[, 1])
    #  db.basnow.sr[1:snow.free[1],10] <- round(spring.corr[1]-as.numeric(db.basnow.sr[1:snow.free[1],9]),3)
    #db.basnow.sr[snow.free[1]:snow.free[2],9*i]<-0 # snowfree
    db.basnow.sr[ind.snow1, "Dsn"] <- round(spring.corr[1] - as.numeric(db.basnow.sr[ind.snow1, "distcor"]), 3)
    db.basnow.sr[ind.snow2, "Dsn"] <- round(autum.corr[1] - as.numeric(db.basnow.sr[ind.snow2, "distcor"]), 3)
    db.basnow.sr[ind.snow3, "Dsn"] <- round(winter.corr[1] - as.numeric(db.basnow.sr[ind.snow3, "distcor"]), 3)
  }

  #############################################################################
  ## step 1.20
  ## safe data to txt-file
  #############################################################################

  write.table(db.basnow.sr[as.numeric(format(as.POSIXct(db.basnow.sr[, 1], format = '%Y-%m-%d %H:%M', origin = origin, tz = "UTC"), format = '%Y')) == year, ],
              paste0(p.1$w[p.1$n == "LV0.p"], "BaSnow2019sr/00_full_dataset/BaSnow2019sr_", year, "_lv0.dat"),
              quote = F, dec = ".", sep = ",", row.names = F)

  cat("\n#\n# BaSnow2019sr ", year," without problems!\n#\n")
} # end loop over years





#############################################################################
## CASE 2 - BaSnow2019_CS725.dat
#############################################################################
## step 1.03
## loop 1 over years
#############################################################################

for (year in run.year) {#2013:2016 2012:aktuell
  #############################################################################
  ## step 1.04
  ## set 2 empty tables with length of year
  ## columns: 2 (date table) and number of input table (storing table)
  #############################################################################
  #cat("\nProcessing year",year,"\n====================\n\n")
  start.date <- as.POSIXct(paste(year, "-01-01 00:30:00", sep = ""), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date   <- as.POSIXct(paste(year, "-", 12, "-", 31," 18:30:00", sep = ""), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  # set number of columns in db.basnow.cs ==> check step 1.17 length(c("UTC","K_Counts","Tl_Counts","SWE_K","K_Tl_Ratio","SWE_Tl","Tcryst_min","Tcryst_max","batt_U"))
  ncol.db.basnow.cs <- 9
  # create empty data frame with UTC time stamp every 6 hours
  db.basnow.cs <- matrix(ncol = ncol.db.basnow.cs, nrow = length(seq(start.date, end.date, by = "6 hours")), -999)
  db.basnow.cs[, c(2:ncol.db.basnow.cs)] <- NA
  compl.temp <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "6 hours")))
  db.basnow.cs[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "6 hours"), format = '%Y-%m-%d %H:%M:%S'))
  compl.temp[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "6 hours"), format = '%Y-%m-%d %H:%M:%S'))
  colnames(compl.temp) <- c("UTC", "erste")
  #############################################################################
  ## step 1.05
  ## set input.path and list all files
  #############################################################################
  inz.path <- paste0(p.1$w[p.1$n == "ONL.p"], "BaSnow2019cs/")
  files2read <- list.files(inz.path, pattern = "*.dat")
  #############################################################################
  ## step 1.06
  ## loop 2 over all files
  #############################################################################

  for (i in 1:length(files2read)) {
    #############################################################################
    ## step 1.07
    ## read one file (skip headers, set NA-values)
    ## set temporal colnames
    #############################################################################
    #cat("\nprocessing ",files2read[i],"\n====================\n\n")
    dada <- read.table(paste(inz.path, files2read[i], sep = ""), sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")

    colnames(dada) = paste0("V", seq_len(ncol(dada)))
    #############################################################################
    ## step 1.09
    ## check file if dates are in running year of loop 1
    #############################################################################

    if (as.numeric(substr(lapply(dada[1, 1], as.character), 1, 4)) > year || as.numeric(substr(lapply(dada[length(dada[, 1]), 1], as.character), 1, 4)) < year) {next} # skip file if wrong year
    # print to check whether date of the read data is assigned to the correct year
    # cat(paste(dada[1,1],"     to     ",dada[length(dada[,1]),1],"    ",files2read[i], "\n"))
    #############################################################################
    ## step 1.10
    ## check file for double entries
    #############################################################################
    if (("TRUE" %in% duplicated(dada)) == TRUE) { # check for fully double entries
      doouble <- duplicated(dada)
      cat(paste(length(which(doouble == "TRUE")), "duplicated records found in file", files2read[i], "\n",
                "first entry:", dada[which(doouble == "TRUE")[1], 1], "\n  last entry:", dada[which(doouble == "TRUE")[length(which(doouble == "TRUE"))], 1], "\n\n"))
      dada <- unique(dada)  # remove double entries

      } else if (("TRUE" %in% duplicated(dada[, 1])) == TRUE) {  # check for multiple different data records for same! timestamp
      doouble <- duplicated(dada[, 1])
      cat(paste(length(which(doouble == "TRUE")), "multiple records found in file", files2read[i], "\n",
                "first entry:", dada[which(doouble == "TRUE")[1], 1], "\n  last entry:",dada[which(doouble == "TRUE")[length(which(doouble == "TRUE"))], 1], "\n\n"))
      dd <- which(dada[, 1] %in% dada[which(doouble == "TRUE"), 1])
      dada <- dada[-dd, ]  # remove double entries

      } else { cat("No double data entries found in", files2read[i], "\n\n") }
    #############################################################################
    ## step 1.11
    ## convert date to numeric value
    #############################################################################

    dada[, 1] <- as.numeric(as.POSIXct(dada[, 1], format = '%Y-%m-%d %H:%M:%S', origin = origin, tz = "UTC"))
    #############################################################################
    ## step 1.12a
    ## special case: former files with different columns
    ## set colnames
    #############################################################################
    if (length(dada[1, ]) == 15) {
    colnames(dada) <- c("UTC","RECORD","CS725_TIME_String","CS725_StationID","CS725_SerialNum","CS725_K_Counts","CS725_Tl_Counts","CS725_SWE_K","CS725_K_Tl_Ratio","CS725_SWE_Tl","CS725_Crystal_TEMP_MIN","CS725_Crystal_TEMP_MAX","CS725_Hist_Blocks","CS725_K_Disp","CS725_PWR_Volt")
      #############################################################################
      ## step 1.12b
      ## add additional columns to former dataset
      #############################################################################

      # dada$Dsn  <-NA  # column for calculated snowdepth

      } else {
      #############################################################################
      ## step 1.12
      ## standard case
      ## set original colnames
      #############################################################################

      colnames(dada) <- c("UTC","RECORD","CS725_TIME_String","CS725_StationID","CS725_SerialNum","CS725_K_Counts","CS725_Tl_Counts","CS725_SWE_K","CS725_K_Tl_Ratio","CS725_SWE_Tl","CS725_Crystal_TEMP_MIN","CS725_Crystal_TEMP_MAX","CS725_Hist_Blocks","CS725_K_Disp","CS725_PWR_Volt")
      }
      #############################################################################
      ## step 1.13
      ## new arrangement / order of columns (all Temperatures together, ascending, ... )
      ######
      ## ==> page 30 in campbell_scientific:cs725_cs_manual_v20120801.pdf "The CS725 uses a thallium doped sodium iodide crystal NaI(Tl) for detecting the gamma radiation"
      ##
      ## check page 11 in campbell_scientific:cs725_cs_manual_v20120801.pdf for names of the records in the columns ==> http://sparcwiki.awi-potsdam.de/lib/exe/fetch.php?media=public:sensors:campbell_scientific:cs725_cs_manual_v20120801.pdf
      ## "CS725_TIME_String": Date and Time Stamp
      ## "CS725_StationID": Station ID
      ## "CS725_SerialNum": Sensor Serial number
      ## "CS725_K_Counts": K counts total corrected (this value is used in the actual SWE calculations)
      ## "CS725_Tl_Counts": Tl counts total corrected (this value is used in the actual SWE calculations)
      ## "CS725_SWE_K": SWE value generated from K
      ## "CS725_K_Tl_Ratio": Ratio generated from K and Tl
      ## "CS725_SWE_Tl": SWE value generated from Tl
      ## "CS725_Crystal_TEMP_MIN": Crystal temperature min (during the measurement interval)
      ## "CS725_Crystal_TEMP_MAX": Crystal temperature max (during the measurement interval)
      ## "CS725_Hist_Blocks": Total number of histogram blocks used for the analysis
      ## "CS725_K_Disp":  Displacement of the K peak from its nominal position (in bins)
      ## "CS725_PWR_Volt": Power input voltage at the CS725 (after protection diode drop)
      ######
      ## exclude columns "RECORD","CS725_TIME_String","CS725_StationID","CS725_SerialNum","CS725_Hist_Blocks","CS725_K_Disp"
      #############################################################################
      dada <- dada[, c("UTC","CS725_K_Counts","CS725_Tl_Counts","CS725_SWE_K","CS725_K_Tl_Ratio","CS725_SWE_Tl","CS725_Crystal_TEMP_MIN","CS725_Crystal_TEMP_MAX","CS725_PWR_Volt")]
      #############################################################################
      ## step 1.14
      ## merge input data with date table
      #############################################################################

      newdf.a <- merge(compl.temp, dada, all.x = T, by = "UTC")

      #############################################################################
      ## step 1.15
      ## merge date table with storing table
      #############################################################################

      for (k in 2:(length(dada[1, ]))) {
        db.basnow.cs[, k] <- rowMeans(cbind(db.basnow.cs[, k], newdf.a[, k + 1]), na.rm = T)#
        }
  }

  #############################################################################
  ## step 1.16
  ## convert numeric dates back to date format
  #############################################################################
  # the sensor CS725 measures every 6 hours, the logger logs the data at 00:30, 06:30, 12:30 and 18:30 of every day.
  # substract 30 minutes to get the hours 00:00, 06:00, 12:00, 18:00 of every day.
  db.basnow.cs[, 1] <- db.basnow.cs[, 1] - (60 * 30)
  db.basnow.cs[, 1] <- format( as.POSIXct(db.basnow.cs[, 1], origin = origin, tz = "UTC"), format = '%Y-%m-%d %H:%M')

  #############################################################################
  ## step 1.17
  ## set "sparc" colnames
  #############################################################################

  colnames(db.basnow.cs) <- c("UTC","CountsK","CountsTl","SWE_K","KTl_Ratio","SWE_Tl","Tcryst_min","Tcryst_max","batt_U")

  #############################################################################
  ## step 1.18
  ## Set NAN to NA
  #############################################################################

  for (val in 2:ncol.db.basnow.cs) {
    db.basnow.cs[which(is.nan(as.numeric(db.basnow.cs[, (val)])) == TRUE), val] <- NA   # set NAN to NA
    }


  #############################################################################
  ## step 1.19
  ## new variables:
  ## a) Dsn: snow depth is inherited from BaSnow2019sr
  ## b) rho_K: snow density is calculated from snow depth ("Dsn") and snow water equivalent determined with K ("SWE_K")
  #############################################################################

  ba.snow.sr <- read.table(file = paste0(p.1$w[p.1$n == "LV1.p"], "BaSnow2019sr/00_full_dataset/BaSnow2019sr_", year, "_lv1.dat"),
                           sep = ",", dec = ".", header = T)

  db.basnow.cs <- data.frame(db.basnow.cs, Dsn = NA, rho_K = NA)
  #db.basnow.cs <- cbind(db.basnow.cs, Dsn = NA, SWE_K = NA)
  # a) Dsn
  # ind.6hours.ba.snow.sr: index of the hours "00:00, 06:00, 12:00, 18:00" (6 hours temporal resolution) in the ba.snow.sr data set
  # ==> this is the same temporal resolution like "BaSnow2019cs"
  ind.6hours.ba.snow.sr <- seq(1, nrow(ba.snow.sr), by = 6)
  # ind.basnow.cs_6hours.ba.snow.sr: index of the same time stamp in the db.basnow.cs like ind.6hours.ba.snow.sr
  ind.basnow.cs_6hours.ba.snow.sr <- which(db.basnow.cs[, 1] %in% ba.snow.sr[ind.6hours.ba.snow.sr, "UTC"])
  # add column "Dsn" to ba.snow.cs
  # assign the "Dsn" and "Dsn_fl" values from the ba.snow.sr data set to db.basnow.cs

  #db.basnow.cs[ind.basnow.cs_6hours.ba.snow.sr, which(colnames(db.basnow.cs) == "Dsn")] <- ba.snow.sr[ind.6hours.ba.snow.sr, "Dsn"]
  db.basnow.cs[ind.basnow.cs_6hours.ba.snow.sr, "Dsn"] <- ba.snow.sr[ind.6hours.ba.snow.sr, "Dsn"]

  # b) rho_K
  ###########
  # b) snow density ("rho_K") is calculated from snow depth ("Dsn") and snow water equivalent determined with K ("SWE_K")
  # "SWE_K" in mm ==> page 10, manual cs725_cs_manual_v20120801.pdf
  # "Dsn" in m
  db.basnow.cs[, "rho_K"] <- round(as.numeric(db.basnow.cs$SWE_K) / (db.basnow.cs$Dsn * 1000), 3)

  #############################################################################
  ## step 1.20
  ## safe data to txt-file
  #############################################################################

  write.table(db.basnow.cs[as.numeric(format(as.POSIXct(db.basnow.cs[, 1], format = '%Y-%m-%d %H:%M', origin = origin, tz = "UTC"), format = '%Y')) == year, ],
              paste0(p.1$w[p.1$n == "LV0.p"], "BaSnow2019cs/00_full_dataset/BaSnow2019cs_", year, "_lv0.dat"), quote = F, dec = ".", sep = ",", row.names = F)

  cat("\n#\n# BaSnow2019cs ", year," without problems!\n#\n")
} # end loop over years



