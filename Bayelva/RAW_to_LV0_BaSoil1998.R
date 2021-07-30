###.........................................................................
##
##   BaSoil1998         RAW to Level0 ----
##
###.........................................................................
## ATTENTION! File name of raw data is BaSoil1998 xxxx  == > step 1.05. ----
## But new file name is BaSoil2017, because only the heatflux measurements are used and heatflux measurements started in 2017.
###.........................................................................
##
##   equal time steps, no gaps
##
##   by: Stephan.Lange@awi.de
##		   christian.lehr@awi.de
##   last modified: 2020-04-14
##
###.........................................................................
##
##    modifications: ----
##    2021-05-12 SL adapted to runner app and content management
##    2020-04-14 CL change loop to (year in run.year) to allow the selection of the processed year in Bayelva_MAIN.R
##    2020-03-25 CL change of variable names
##
###.........................................................................
##
##
##   19 steps to get wonderful data
##
##
##
###.........................................................................
## step 1.01 set path settings for different systems linux vs. windows ----
##
###.........................................................................
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
###.........................................................................
## step 1.02 set options ----
##
###.........................................................................

###.........................................................................
# for non-exponential display of numeric values
# options(scipen = 100)
# origin <- "1970-01-01"
# recent.year <- as.numeric(format(Sys.Date(), "%Y"))

###.........................................................................
# to run this script separately, you have to set run.year:
#
# recent.year <- as.numeric(format(Sys.Date(), "%Y"))
# run.year <- recent.year
# run.year <- 2020
###.........................................................................

###.........................................................................
## step 1.03 loop 1 over years ----
##
###.........................................................................

for (year in run.year) {

  ###.........................................................................
  ## step 1.04 define empty table for the processed year ----
  ##
  ## storing table: storing.table with date column + columns for all the variables of the desired end product
  ##
  ##
  ## define two empty tables for the processed year
  ## date table: date.table with date column + one dummy column
  ## storing table: storing.table with date column + columns for all the variables of the desired end product
  ###.........................................................................

  # cat("\nProcessing year", year, "\n====================\n\n")
  start.date <- as.POSIXct(paste(year, "-01-01 00:00:00", sep = ""), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date <- as.POSIXct(paste(year, "-", 12, "-", 31, " 23:30:00", sep = ""), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  # set number of columns of storing table according to the number of variables of the end product + 1 for the date column
  # ==> see also check step 1.17: length(c("UTC", "shf_Avg(1)", "shf_Avg(2)"))
  ncol.storing.table <- 3
  # create empty tables with UTC time stamp every 30 min
  # create empty table with UTC time stamp every 30 min
  # date table
  #date.table <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "30 min")))
  #date.table[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = '%Y-%m-%d %H:%M:%S'))
  #colnames(date.table) <- c("UTC", "dummy")
  # storing table
  storing.table <- matrix(ncol = ncol.storing.table, nrow = length(seq(start.date, end.date, by = "30 min")), -999)
  storing.table[, c(2:ncol.storing.table)] <- NA
  storing.table[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = '%Y-%m-%d %H:%M:%S'))
  colnames(storing.table) <- c("UTC", "V1", "V2")

  ###.........................................................................
  ## step 1.05 set input path and list all files ----
  ##
  ###.........................................................................

  input.path <- paste0(p.1$w[p.1$n == "ONL.p"], "BaSoil1998/")
  files2read <- list.files(input.path, pattern = "*.dat")

  ###.........................................................................
  ## step 1.06 loop 2 over all files ----
  ##
  ###.........................................................................

  for (i in 1:length(files2read)) {

    ###.........................................................................
    ## step 1.07 read one file (skip headers, set NA-values) ----
    ##
    ## set temporal column names
    ###.........................................................................

    #cat("\nprocessing ", files2read[i], "\n ==================== \n\n")
    file.i <- read.table(paste(input.path, files2read[i], sep = ""),
                       sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")

    colnames(file.i) <- paste0("V", seq_len(ncol(file.i)))

    ###.........................................................................
    ## step 1.09 check file if dates are in running year of loop 1 ----
    ##
    ###.........................................................................

    # skip file if wrong year
    if (as.numeric(substr(lapply(file.i[1, 1], as.character), 1, 4)) > year || as.numeric(substr(lapply(file.i[length(file.i[, 1]), 1], as.character), 1, 4)) < year) {
      next
    }
    #cat(paste(file.i[1, 1], " to ", file.i[length(file.i[, 1]), 1], " ", files2read[i]))

    ###.........................................................................
    ## step 1.10 check file for duplicated records ----
    ##
    ###.........................................................................

    # Case 1: check for records with same date AND same data
    # Case 2: check for multiple different data records with the same timestamp
    if (("TRUE" %in% duplicated(file.i)) == TRUE) {
      doouble <- duplicated(file.i)
      # cat(paste(length(which(doouble == "TRUE")), "duplicated records found in file", files2read[i], "\n",
      #           "first entry:", file.i[which(doouble == "TRUE")[1], 1], "\n  last entry:", file.i[which(doouble == "TRUE")[length(which(doouble == "TRUE"))], 1], "\n\n"))
      # remove duplications
      file.i <- unique(file.i)
    } else if (("TRUE" %in% duplicated(file.i[, 1])) == TRUE) {
      doouble <- duplicated(file.i[, 1])
      # cat(paste(length(which(doouble == "TRUE")), "multiple records found in file", files2read[i], "\n",
      #           "first entry:", file.i[which(doouble == "TRUE")[1], 1], "\n  last entry:", file.i[which(doouble == "TRUE")[length(which(doouble == "TRUE"))], 1], "\n\n"))
      dd <- which(file.i[, 1] %in% file.i[which(doouble == "TRUE"), 1])
      # remove duplications
      file.i <- file.i[-dd, ]

    } else {
      # cat("No duplicated records found in", files2read[i], "\n\n")
    }

    ###.........................................................................
    ## step 1.11 convert date to numeric value ----
    ##
    ###.........................................................................

    file.i[, 1] <- as.numeric(as.POSIXct(file.i[, 1], format = '%Y-%m-%d %H:%M:%S', origin = origin, tz = "UTC"))

    ###.........................................................................
    ## step 1.12 Naming of the columns ----
    ##
    ## ==> this has to be set manually, because experience shows that
    ## a) there are often special cases, and
    ## b) that it is good to manually define which variables are actually selected and processed
    ##
    ## Case 1:
    ## standard case: set original colnames
    ## Case 2:
    ## special case: former files with different columns ==> add additional columns
    ## here: no special cases....
    ###.........................................................................

    if (length(file.i[1, ]) == 38) {
      colnames(file.i) <-   c("UTC", "RECORD", "batt_volt_Min", "PTemp", "T107_C_Avg(1)", "T107_C_Avg(2)",
                            "T107_C_Avg(3)", "T107_C_Avg(4)", "T107_C_Avg(5)", "T107_C_Avg(6)", "T107_C_Avg(7)",
                            "T107_C_Avg(8)", "T107_C_Avg(9)", "T107_C_Avg(10)", "T107_C_Avg(11)", "T107_C_Avg(12)",
                            "T107_C_Avg(13)", "T107_C_Avg(14)", "T107_C_Avg(15)", "T107_C_Avg(16)", "T107_C_Avg(17)",
                            "T107_C_Avg(18)", "T107_C_Avg(19)", "T107_C_Avg(20)", "T107_C_Avg(21)", "T107_C_Avg(22)",
                            "T107_C_Avg(23)", "T107_C_Avg(24)", "T107_C_Avg(25)", "T107_C_Avg(26)", "T107_C_Avg(27)",
                            "T107_C_Avg(28)", "T107_C_Avg(29)", "T107_C_Avg(30)", "T107_C_Avg(31)", "T107_C_Avg(32)",
                            "shf_Avg(1)", "shf_Avg(2)")
    } else {
      colnames(file.i) <-  c("UTC", "RECORD", "batt_volt_Min", "PTemp", "T107_C_Avg(1)", "T107_C_Avg(2)",
                           "T107_C_Avg(3)", "T107_C_Avg(4)", "T107_C_Avg(5)", "T107_C_Avg(6)", "T107_C_Avg(7)",
                           "T107_C_Avg(8)", "T107_C_Avg(9)", "T107_C_Avg(10)", "T107_C_Avg(11)", "T107_C_Avg(12)",
                           "T107_C_Avg(13)", "T107_C_Avg(14)", "T107_C_Avg(15)", "T107_C_Avg(16)", "T107_C_Avg(17)",
                           "T107_C_Avg(18)", "T107_C_Avg(19)", "T107_C_Avg(20)", "T107_C_Avg(21)", "T107_C_Avg(22)",
                           "T107_C_Avg(23)", "T107_C_Avg(24)", "T107_C_Avg(25)", "T107_C_Avg(26)", "T107_C_Avg(27)",
                           "T107_C_Avg(28)", "T107_C_Avg(29)", "T107_C_Avg(30)", "T107_C_Avg(31)", "T107_C_Avg(32)",
                           "shf_Avg(1)", "shf_Avg(2)")
    }

    ###.........................................................................
    ## step 1.13 new selection and arrangement / order of columns ----
    ## (for example all Temperatures grouped together and in ascending order, ... )
	  ## here: exclude all columns except "UTC" and the heatflux columns "shf_Avg(1)" and "shf_Avg(2)"
    ###.........................................................................

    file.i <- file.i[, c("UTC", "shf_Avg(1)", "shf_Avg(2)")]

    ###.........................................................................
    ## step 1.14 merge input data with date table ----
    ##
    ###.........................................................................

    # newdf.a <- merge(date.table, file.i, all.x = T, by = "UTC")
    newdf.a <- merge(storing.table[, 1:2], file.i, all.x = T, by = "UTC")

    ###.........................................................................
    ## step 1.15 merge date table with storing table ----
    ##
    ###.........................................................................

    for (j in 2:(length(file.i[1, ]))) {
      storing.table[, j] <- rowMeans(cbind(storing.table[, j], newdf.a[, j + 1]), na.rm = T)
      #storing.table2[, j] <- rowMeans(cbind(storing.table2[, j], newdf.b[, j + 1]), na.rm = T)
    }
  }

  ###.........................................................................
  ## step 1.16 convert numeric dates back to date format ----
  ##
  ###.........................................................................

  storing.table[, 1] <- format(as.POSIXct(storing.table[, 1], origin = origin, tz = "UTC"), format = '%Y-%m-%d %H:%M')

  ###.........................................................................
  ## step 1.17 set "sparc" colnames ----
  ##
  ###.........................................................................

  colnames(storing.table) <- c("UTC", "G_1", "G_2")

  ###.........................................................................
  ## step 1.18 Set NAN to NA ----
  ##
  ###.........................................................................

  for (k in 2:ncol.storing.table) {
      storing.table[which(is.nan(as.numeric(storing.table[, k])) == TRUE), k] <- NA
  }

  ###.........................................................................
  ## step 1.19 save data to txt-file ----
  ##
  ###.........................................................................

  write.table(storing.table[as.numeric(format(as.POSIXct(storing.table[, 1], format = '%Y-%m-%d %H:%M', origin = origin, tz = "UTC"), format = '%Y')) == year, ],
              paste0(p.1$w[p.1$n == "LV0.p"], "BaSoil2017/00_full_dataset/BaSoil2017_", year, "_lv0.dat"),
              quote = F, dec = ".", sep = ",", row.names = F)

}

# end loop over years
cat("\n#\n# BaSoil2017 without problems!\n#\n")
