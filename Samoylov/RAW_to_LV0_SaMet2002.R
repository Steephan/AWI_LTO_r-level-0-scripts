###...........................................................................
##
#            RAW to Level0           --------------------
##   (SaMet2010)
##   equal time steps,  no gaps
##
##   by: Stephan.Lange@awi.de & Peter.Schreiber@awi.de
##   modified: 2018-10-08
##
##   changes:
##   2021-05-06 SL adapted to refresh app
##   2021-04-27 SL comments german to english translation, new pathes
##   2020-03-09: NetRad replaced with RadNet
##   2018-10-08: correction of: 2002-2009 warong correction of NetRad,  wrong read in of CG1 (LwIn instead of LwOut) and CG1Temp was set as PanelTemp!
##
##   error,  not changed yet: partly wrong read in of AirTemp + RH 50 vs 200 cm it was ever 50cm as first sensor,  than 200 cm!!!
##
##   2019-05-21: changed paths
##
##   2019-08-08: new rain calibration factor for 2010 -- now
##
###...........................................................................
##
###...........................................................................
##  all in all -very ugly input dataset !!!! 
##  about 10 different input formats, sometimes 2 changes in each year
##  since 2015 no changes in the format of input!! (Thanks Niko)
##
##  
##
##  special setup of this level0 script ()
##    1) define pathes; different format in different folders for different years
##    2) loop over all relevant files for the specific year
##    3) read, check, some corrections, merge datasets
##
###...........................................................................
## step 1.01 set path settings for different systems linux vs. windoof ----
##
###...........................................................................
# to run this script separately,  you have to uncomment the next 10 lines!
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
###...........................................................................
## step 1.02 set running options years ----
##
###...........................................................................
options(scipen = 100) # for non-exponential display of numeric values
origin <- "1970-01-01"
aktuell <- as.numeric(format(Sys.Date(), "%Y"))
# run.year <- 2021
prec.fac.1 <- 0.2 # before  2010           --- ARG100 Tipping Bucket Raingauge
prec.fac.2 <- 0.1 # since 2010/07/26 01:30 --- Young Raingauge 52203 - SN TB07216

###...........................................................................
## step 1.03 loop 1 over years ----
##
###...........................................................................
for (year in run.year) {#2002:aktuell
  
  ###...........................................................................
  ## step 1.04 set 2 empty tables with length of year ----
  ## 
  ## columns: 2 (date table) and number of input table (storing table)
  ###...........................................................................
  ##cat("\nProcessing year", year, "\n ==================== \n\n")
  start.date <- as.POSIXct(paste0(year, "-01-01 00:00:00"), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date <- as.POSIXct(paste0(year, "-", 12, "-", 31, " 23:30:00"), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  ## create empty data frame with UTC time stamp every 30 min
  db.samet.meteo <- as.data.frame(matrix(ncol = 18, nrow = length(seq(start.date, end.date, by = "30 min")), -999))
  db.samet.meteo[, c(2:18)] <- NA
  db.samet.rad <- as.data.frame(matrix(ncol = 13, nrow = length(seq(start.date, end.date, by = "30 min")), -999))
  db.samet.rad[, c(2:13)] <- NA
  db.samet.soil <- as.data.frame(matrix(ncol = 6, nrow = length(seq(start.date, end.date, by = "30 min")), -999))
  db.samet.soil[, c(2:7)] <- NA
  compl.meteo <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "30 min")), -999)
  compl.rad <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "30 min")), -999)
  compl.soil  <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "30 min")), -999)
  db.samet.meteo[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = '%Y-%m-%d %H:%M:%S'))
  db.samet.rad[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = '%Y-%m-%d %H:%M:%S'))
  db.samet.soil[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = '%Y-%m-%d %H:%M:%S'))
  
  compl.meteo[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = '%Y-%m-%d %H:%M:%S'))
  compl.rad[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = '%Y-%m-%d %H:%M:%S'))
  compl.soil[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = '%Y-%m-%d %H:%M:%S'))
  colnames(compl.meteo) <- c("UTC", "erste");
  colnames(compl.rad) <- c("UTC", "erste");
  colnames(compl.soil) <- c("UTC", "erste")
  ###...........................................................................
  ## step 1.05 set input.path and list all files
  ##
  ## (special conditions at this station!!!)
  ###...........................................................................
  if (year < 2007) {
    inz.01.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaMet2002/06_SAK_2002-2009/")
    files.01 <- list.files(inz.01.path, pattern = "*.dat")
  } else if (year %in% c(2007, 2008)) {
    inz.01.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaMet2002/06_SAK_2002-2009/")
    files.01 <- list.files(inz.01.path, pattern = "*.dat")
    inz.02.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaMet2002/07_SaMet2007/")
    files.02 <- list.files(inz.02.path, pattern = "*.dat")
    files.01 <- c(files.01, files.02)
  } else if (year == 2009) {
    inz.01.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaMet2002/06_SAK_2002-2009/")
    files.01 <- list.files(inz.01.path, pattern = "*.dat")
    inz.02.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaMet2002/07_SaMet2007/")
    files.02 <- list.files(inz.02.path, pattern = "*.dat")
    inz.03.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaMet2002/06_SAK_2009-2014/")
    files.03 <- list.files(inz.03.path, pattern = "*.dat")
    files.01 <- c("sak09_001-039.dat", files.02, files.03[1:3])
  } else if (year %in% c(2010)) {
    inz.01.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaMet2002/07_SaMet2007/")
    files.01 <- list.files(inz.01.path, pattern = "*.dat")
    inz.02.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaMet2002/06_SAK_2009-2014/")
    files.02 <- list.files(inz.02.path, pattern = "*.dat")
    files.01 <- c(files.01, files.02)
  } else if (year %in% c(2011, 2012, 2013)) {
    inz.01.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaMet2002/06_SAK_2009-2014/")
    files.01 <- list.files(inz.01.path, pattern = "*.dat")
  } else if (year == 2014) {
    inz.2014.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaMet2002/06_SAK_2009-2014/")
    files.2014 <- list.files(inz.2014.path, pattern = "*.dat")[19]   # "201408180400_201308221200_SaMet2002_SAK_CR1000_13760.dat"
    inz.01.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaMet2002/01_SaMeteo/")
    files.01 <- list.files(inz.01.path, pattern = "*.dat")
    inz.02.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaMet2002/02_SaRad/")
    files.02 <- list.files(inz.02.path, pattern = "*.dat")
    inz.03.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaMet2002/03_SaSoil/")
    files.03 <- list.files(inz.03.path, pattern = "*.dat")
    m <- 0
  } else if (year > 2014) {
    inz.01.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaMet2002/01_SaMeteo/")
    files.01 <- list.files(inz.01.path, pattern = "*.dat")
    inz.02.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaMet2002/02_SaRad/")
    files.02 <- list.files(inz.02.path, pattern = "*.dat")
    inz.03.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaMet2002/03_SaSoil/")
    files.03 <- list.files(inz.03.path, pattern = "*.dat")
    m <- 1
  }
  
  #  main input tasks     
  
  ###...........................................................................
  ## step 1.06 loop 2 over all SaMeteo files -----
  ## 
  ###...........................................................................
  
  for (i in (1:length(files.01))) {# SaMet2010 SaMeteo 1:length(files.01)
    ###...........................................................................
    ## step 1.07 read one file (skip headers,  set NA-values)-----
    ## 
    ## set temporal colnames;
    ###...........................................................................
    # #cat("\nprocessing ", files.01[i], "\n ======== =", year, " ========== =\n\n")
    if (year < 2007) { # 06_SAK_2002-2009
      dada.meteo <- read.table(paste0(inz.01.path, files.01[i]), sep = ",", dec = ".", header = F, skip = 0, fill = TRUE, na = "NAN")
      colnames(dada.meteo) <- paste0("V", seq_len(ncol(dada.meteo)))
      dada.meteo[, 1] <- as.numeric(as.POSIXct(paste(dada.meteo[, 2], dada.meteo[, 3], dada.meteo[, 4]/100, sep = ":"), format = '%Y:%j:%H', origin = origin, tz = "UTC"))
      dada.meteo <- dada.meteo[, -c(2, 3, 4)]
      if (length(which(duplicated(dada.meteo[, 1]) == TRUE)) > 0) dada.meteo <- dada.meteo[-which(duplicated(dada.meteo[, 1]) == T), ]
    } else if (year %in% c(2007, 2008)) {
      if (i <= 45) { # 06_SAK_2002-2009
        dada.meteo <- read.table(paste0(inz.01.path, files.01[i]), sep = ",", dec = ".", header = F, skip = 0, fill = TRUE, na = "NAN")
        colnames(dada.meteo) <- paste0("V", seq_len(ncol(dada.meteo)))
        dada.meteo[, 1] <- as.numeric(as.POSIXct(paste(dada.meteo[, 2], dada.meteo[, 3], dada.meteo[, 4]/100, sep = ":"), format = '%Y:%j:%H', origin = origin, tz = "UTC"))
        dada.meteo <- dada.meteo[, -c(2, 3, 4)]
        if (length(which(duplicated(dada.meteo[, 1]) == TRUE)) > 0) dada.meteo <- dada.meteo[-which(duplicated(dada.meteo[, 1]) == T), ]
      } else { # 07_SaMet2007
        dada.meteo <- read.table(paste0(inz.02.path, files.01[i]), sep = ",", dec = ".", header = F, skip = 1, fill = TRUE, na = "NA")
        colnames(dada.meteo) <- paste0("V", seq_len(ncol(dada.meteo)))
        dada.meteo[, 1] <- as.numeric(as.POSIXct(dada.meteo[, 1], format = '%d.%m.%Y %H:%M', origin = origin, tz = "UTC"))
        if (length(which(duplicated(dada.meteo[, 1]) == TRUE)) > 0) dada.meteo <- dada.meteo[-which(duplicated(dada.meteo[, 1]) == T), ]
      }
    } else if (year == 2009) {
      if (i == 1) {# 06_SAK_2002-2009
        dada.meteo <- read.table(paste0(inz.01.path, files.01[i]), sep = ",", dec = ".", header = F, skip = 0, fill = TRUE, na = "NAN")
        colnames(dada.meteo) <- paste0("V", seq_len(ncol(dada.meteo)))
        dada.meteo[, 1] <- as.numeric(as.POSIXct(paste(dada.meteo[, 2], dada.meteo[, 3], dada.meteo[, 4] / 100, sep = ":"), format = '%Y:%j:%H', origin = origin, tz = "UTC"))
        dada.meteo <- dada.meteo[, -c(2, 3, 4)]
        if (length(which(duplicated(dada.meteo[, 1]) == TRUE)) > 0) dada.meteo <- dada.meteo[-which(duplicated(dada.meteo[, 1]) == T), ]
      } else if (i == 2) {# 07_SaMet2007
        dada.meteo <- read.table(paste0(inz.02.path, files.01[i]), sep = ",", dec = ".", header = F, skip = 1, fill = TRUE, na = "NA")
        colnames(dada.meteo) <- paste0("V", seq_len(ncol(dada.meteo)))
        dada.meteo[, 1] <- as.numeric(as.POSIXct(dada.meteo[, 1], format = '%d.%m.%Y %H:%M', origin = origin, tz = "UTC"))
        if (length(which(duplicated(dada.meteo[, 1]) == TRUE)) > 0) dada.meteo <- dada.meteo[-which(duplicated(dada.meteo[, 1]) == T), ]
      } else {#06_SAK_2009-2014??
        dada.meteo <- read.table(paste0(inz.03.path, files.01[i]), sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")
        colnames(dada.meteo) <- paste0("V", seq_len(ncol(dada.meteo)))
        dada.meteo[, 1] <- as.numeric(as.POSIXct(dada.meteo[, 1], format = '%Y-%m-%d %H:%M:%S', origin = origin, tz = "UTC"))
        if (length(which(duplicated(dada.meteo[, 1]) == TRUE)) > 0) dada.meteo <- dada.meteo[-which(duplicated(dada.meteo[, 1]) == T), ]
      }
    } else if (year %in% c(2010)) {
      if (i == 1) {# 07_SaMet2007
        dada.meteo <- read.table(paste0(inz.01.path, files.01[i]), sep = ",", dec = ".", header = F, skip = 1, fill = TRUE, na = "NA")
        colnames(dada.meteo) <- paste0("V", seq_len(ncol(dada.meteo)))
        dada.meteo[, 1] <- as.numeric(as.POSIXct(dada.meteo[, 1], format = '%d.%m.%Y %H:%M', origin = origin, tz = "UTC"))
        if (length(which(duplicated(dada.meteo[, 1]) == TRUE)) > 0) dada.meteo <- dada.meteo[-which(duplicated(dada.meteo[, 1]) == T), ]
      } else {
        dada.meteo <- read.table(paste0(inz.02.path, files.01[i]), sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")
        colnames(dada.meteo) <- paste0("V", seq_len(ncol(dada.meteo)))
        dada.meteo[, 1] <- as.numeric(as.POSIXct(dada.meteo[, 1], format = '%Y-%m-%d %H:%M:%S', origin = origin, tz = "UTC"))
        if (length(which(duplicated(dada.meteo[, 1]) == TRUE)) > 0) dada.meteo <- dada.meteo[-which(duplicated(dada.meteo[, 1]) == T), ]
      }
    } else if (year %in% c(2011, 2012, 2013)) {
      dada.meteo <- read.table(paste0(inz.01.path, files.01[i]), sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")
      colnames(dada.meteo) <- paste0("V", seq_len(ncol(dada.meteo)))
      dada.meteo[, 1] <- as.numeric(as.POSIXct(dada.meteo[, 1], format = '%Y-%m-%d %H:%M:%S', origin = origin, tz = "UTC"))
      if (length(which(duplicated(dada.meteo[, 1]) == TRUE)) > 0) dada.meteo <- dada.meteo[-which(duplicated(dada.meteo[, 1]) == T), ]
    } else if (year == 2014 & i == 1 & m == 0) {
      dada.meteo <- read.table(paste0(inz.2014.path, files.2014[i]), sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")
      colnames(dada.meteo) <- paste0("V", seq_len(ncol(dada.meteo)))
      dada.meteo[, 1] <- as.numeric(as.POSIXct(dada.meteo[, 1], format = '%Y-%m-%d %H:%M:%S', origin = origin, tz = "UTC"))
      if (length(which(duplicated(dada.meteo[, 1]) == TRUE)) > 0) dada.meteo <- dada.meteo[-which(duplicated(dada.meteo[, 1]) == T), ]
      m <- 1
      i <- 1
    } else if (year >= 2014 & m == 1) {
      if (length(files.01) != length(files.02)) print("Warning: Possibly missing Data Table in RAW Folder")
      if (length(files.01) != length(files.03)) print("Warning: Possibly missing Data Table in RAW Folder")
      dada.meteo <- read.table(paste0(inz.01.path, files.01[i]), sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")
      colnames(dada.meteo) <- paste0("V", seq_len(ncol(dada.meteo)))
      dada.rad <- read.table(paste0(inz.02.path, files.02[i]), sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")
      colnames(dada.rad) <- paste0("V", seq_len(ncol(dada.rad)))
      dada.soil <- read.table(paste0(inz.03.path, files.03[i]), sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")
      colnames(dada.soil) <- paste0("V", seq_len(ncol(dada.soil)))
      
      dada.meteo[, 1] <- as.numeric(as.POSIXct(dada.meteo[, 1], format = '%Y-%m-%d %H:%M:%S', origin = origin, tz = "UTC"))
      dada.rad[, 1] <- as.numeric(as.POSIXct(dada.rad[, 1],   format = '%Y-%m-%d %H:%M:%S', origin = origin, tz = "UTC"))
      dada.soil[, 1] <- as.numeric(as.POSIXct(dada.soil[, 1],  format = '%Y-%m-%d %H:%M:%S', origin = origin, tz = "UTC"))
      if (length(which(duplicated(dada.meteo[, 1]) == TRUE)) > 0) dada.meteo <- dada.meteo[-which(duplicated(dada.meteo[, 1]) == T), ]
      if (length(which(duplicated(dada.rad[, 1]) == TRUE)) > 0) dada.rad <- dada.rad[-which(duplicated(dada.rad[, 1]) == T), ]
      if (length(which(duplicated(dada.soil[, 1]) == TRUE)) > 0) dada.soil <- dada.soil[-which(duplicated(dada.soil[, 1]) == T), ]
    }
    
    
    ###...........................................................................
    ## step 1.12 a) special case 2010-2013: former files with different columns -----
    ## 
    ## set colnames
    ###...........................................................................
    spalten.met <- length(dada.meteo[1, ])
    
    if (spalten.met == 27) {# 2010-2013
      #cat("zick 27", year, files.01[i], "\n")
      colnames(dada.meteo) <- c("UTC", "rec", "batt_U_min", "Tair_a_50", "Tair_a_200", ###### im jahr 2013 scheint noch ein wechsel zu sein ## PSc: reihenfolge Tair und RH richtig
                                "RH_200", "RH_50",
                                "wind_v_300", "wind_deg_300", "wind_sddeg_300",
                                "prec_tot_count",
                                "SwIn", "SwOut", "LwIn_rawcor", "LwOut_rawcor",
                                "SwIn_raw", #"CM3Up_raw_Avg",
                                "SwOut_raw", #"CM3Dn_raw_Avg",
                                "LwIn_raw", #"CG3Up_raw_Avg",
                                "LwOut_raw", #"CG3Dn_raw_Avg",
                                "Tsen_cnr4_C", "LwIn", "LwOut",
                                "Ts_1", "Ts_6", "Ts_3", "WT_raw", "CS")
      #"Ts_1", "Ts_6", "Ts_3", "PA_mbar", "WT_raw")
      
      dada.meteo$Tair_b_200 <- NA
      dada.meteo$Tair_b_50 <- NA
      dada.meteo$Tpan <- NA
      dada.meteo$Tsen_cnr4_K <- dada.meteo$Tsen_cnr4_C + 273.15
      dada.meteo$WT <- NA
      dada.meteo$WL <- NA
      dada.meteo$SwNet <- dada.meteo$SwIn - dada.meteo$SwOut
      dada.meteo$LwNet <- dada.meteo$LwIn - dada.meteo$LwOut
      
      dada.meteo$Albedo <- dada.meteo$SwOut / dada.meteo$SwIn
      dada.meteo$RadNet <- dada.meteo$SwNet + dada.meteo$LwNet
      dada.meteo$PA <- NA
      dada.meteo$PA_mbar <- NA
      dada.meteo$prec <- dada.meteo$prec_tot_count * prec.fac.2
      dada.meteo$wind_vmax_300 <- NA
      dada.meteo$wind_vmin_300 <- NA
      
      dada.soil <- dada.meteo[, c("UTC", "Ts_1", "Ts_3", "Ts_6", "WT_raw", "WT", "WL")]
      dada.rad <- dada.meteo[, c("UTC", "SwIn", "SwOut", "LwIn", "LwOut",
                                 "Tsen_cnr4_C", "Tsen_cnr4_K", "LwIn_rawcor", "LwOut_rawcor",
                                 "SwNet", "LwNet", "Albedo", "RadNet")]
      dada.meteo <- dada.meteo[, c("UTC", "batt_U_min", "Tpan", "Tair_a_200", "Tair_a_50", "Tair_b_200", "Tair_b_50",
                                   "RH_200", "RH_50", "wind_v_300", "wind_deg_300", "wind_sddeg_300",
                                   "wind_vmax_300", "wind_vmin_300", "prec", "prec_tot_count", "PA", "PA_mbar")]
      ###...........................................................................
      ## step 1.12 a) special case 2013-2014: former files with different columns -----
      ## 
      ## set colnames
      ###...........................................................................
      
      
    } else if (spalten.met == 30) {# 2013-2014
      #cat("zack 30", year, files.01[i],  "\n")
      colnames(dada.meteo) <- c("UTC", "rec", "batt_U_min", "Tpan", "Tair_a_50", "Tair_a_200", "Tair_b_50", "Tair_b_200",  ### PSc: here was a error in read in: Tair and RH not in right way
                                "RH_50", "RH_200", "wind_v_300", "wind_deg_300", "wind_sddeg_300",
                                "prec_tot_count",
                                "SwIn", "SwOut", "LwIn_rawcor", "LwOut_rawcor",
                                "SwIn_raw", #"CM3Up_raw_Avg",
                                "SwOut_raw", #"CM3Dn_raw_Avg",
                                "LwIn_raw", #"CG3Up_raw_Avg",
                                "LwOut_raw", #"CG3Dn_raw_Avg",
                                "Tsen_cnr4_C", "LwIn", "LwOut",
                                "Ts_1", "Ts_6", "Ts_3", "WT_raw", "vwc_WT")
      dada.meteo$Tsen_cnr4_K <- dada.meteo$Tsen_cnr4_C + 273.15
      dada.meteo$PA_mbar <- NA
      dada.meteo$SwNet <- dada.meteo$SwIn - dada.meteo$SwOut
      dada.meteo$LwNet <- dada.meteo$LwIn - dada.meteo$LwOut
      dada.meteo$WT <- NA
      dada.meteo$WL <- NA
      
      dada.meteo$Albedo <- dada.meteo$SwOut / dada.meteo$SwIn
      dada.meteo$RadNet <- dada.meteo$SwNet + dada.meteo$LwNet
      dada.meteo$PA <- NA # dada.meteo$PA_mbar/10
      dada.meteo$prec <- dada.meteo$prec_tot_count * prec.fac.2
      dada.meteo$wind_vmax_300 <- NA
      dada.meteo$wind_vmin_300 <- NA
      
      dada.soil <- dada.meteo[, c("UTC", "Ts_1", "Ts_3", "Ts_6", "WT_raw", "WT", "WL")]
      dada.rad <- dada.meteo[, c("UTC", "SwIn", "SwOut", "LwIn", "LwOut",
                                 "Tsen_cnr4_C", "Tsen_cnr4_K", "LwIn_rawcor", "LwOut_rawcor",
                                 "SwNet", "LwNet", "Albedo", "RadNet")]
      dada.meteo <- dada.meteo[, c("UTC", "batt_U_min", "Tpan", "Tair_a_200", "Tair_a_50", "Tair_b_200", "Tair_b_50",
                                   "RH_200", "RH_50", "wind_v_300", "wind_deg_300", "wind_sddeg_300",
                                   "wind_vmax_300", "wind_vmin_300", "prec", "prec_tot_count", "PA", "PA_mbar")]
      ###...........................................................................
      ## step 1.12 a) special case 2014-2017: former files with different columns -----
      ## 
      ## set colnames
      ###...........................................................................
      
      
    } else if (spalten.met == 15 & year >= 2014) {# 2014-2017
      #cat("zock 15", year, files.01[i], "\n")
      
      colnames(dada.meteo) <- c("UTC", "rec", "batt_U_min", "Tpan", "Tair_a_50", "Tair_a_200", "Tair_b_50", "Tair_b_200",  ### PSc: here was a error in read in: Tair and RH not in right way
                                "RH_50", "RH_200", "wind_v_300", "wind_deg_300", "wind_sddeg_300",
                                "prec_tot_count", "PA_mbar")
      dada.meteo$wind_vmax_300 <- NA
      dada.meteo$wind_vmin_300 <- NA
      dada.meteo$prec <- dada.meteo$prec_tot_count * prec.fac.2
      dada.meteo$PA <- dada.meteo$PA_mbar / 10
      dada.meteo <- dada.meteo[, c("UTC", "batt_U_min", "Tpan", "Tair_a_200", "Tair_a_50", "Tair_b_200", "Tair_b_50",
                                   "RH_200", "RH_50", "wind_v_300", "wind_deg_300", "wind_sddeg_300",
                                   "wind_vmax_300", "wind_vmin_300", "prec", "prec_tot_count", "PA", "PA_mbar")]
      ###...........................................................................
      colnames(dada.rad) <- c("UTC", "rec", "SwIn", "SwOut", "LwIn_rawcor", "LwOut_rawcor",
                              "Tsen_cnr4_C", "Tsen_cnr4_K", "LwIn", "LwOut",
                              "SwNet", "LwNet", "Albedo", "RadNet")
      dada.rad <- dada.rad[, c("UTC", "SwIn", "SwOut", "LwIn", "LwOut",
                               "Tsen_cnr4_C", "Tsen_cnr4_K", "LwIn_rawcor", "LwOut_rawcor",
                               "SwNet", "LwNet", "Albedo", "RadNet")]
      ###...........................................................................
      colnames(dada.soil) <- c("UTC", "rec", "Ts_1", "Ts_6", "Ts_3", "WT_raw", "vwc_WT")
      dada.soil$WT <- NA
      dada.soil$WL <- NA
      dada.soil <- dada.soil[, c("UTC", "Ts_1", "Ts_3", "Ts_6", "WT_raw", "WT", "WL")]
      ###...........................................................................
      ## step 1.12 a) special case 2002-2009: former files with different columns -----
      ## 
      ## set colnames
      ###...........................................................................
      
    } else if (spalten.met == 15 & year <= 2009) {# 2002-2009
      #cat("zock 15", year, files.01[i], "\n")
      colnames(dada.meteo) <- c("UTC", "Tair_a_50", "RH_50", "Tair_a_200", "RH_200", # evtl. anders herum --> PSc: NEIN,  Tair_50 IMMER vor 200 bis 2009
                                "RadNet", "LwOut_raw", "Tsen_cnr4_C", "prec",   #Pete: geaendert "Tpan"-->"Tsen_cnr4_C" @ Stephan
                                "wind_v_300", "wind_deg_300", "wind_sddeg_300",
                                "batt_U_min", "batt_U", "batt_U_max")
      # data soil
      dada.meteo$vwc_WT <- NA
      dada.meteo$WT_raw <- NA
      dada.meteo$WT <- NA
      dada.meteo$WL <- NA
      dada.meteo$Ts_1 <- NA
      dada.meteo$Ts_3 <- NA
      dada.meteo$Ts_6 <- NA
      # data rad
      dada.meteo$RadNet <- dada.meteo$RadNet * 1000 / 15.4     # Pete: Sensitivity: 15.4 ?V/(Wm2) @ Stephan: alt: dada.meteo$RadNet*100 das scheint auch nicht richtig zu sein!!!
      dada.meteo$SwIn <- NA
      dada.meteo$SwOut <- NA
      dada.meteo$LwIn_rawcor <- NA
      dada.meteo$LwOut_rawcor <- dada.meteo$LwOut_raw * 1000 / 15.07 # Pete: Sensitivity: 15.07 ?V/(Wm2) // should be Bolzmann canstant following manual
      dada.meteo$Tsen_cnr4_C <- dada.meteo$Tsen_cnr4_C - 1
      dada.meteo$Tsen_cnr4_C <- dada.meteo$Tsen_cnr4_C / 0.003914 # Pete: Sensitivity taken from Konni
      dada.meteo$Tsen_cnr4_K <- dada.meteo$Tsen_cnr4_C + 273.15
      dada.meteo$LwIn <- NA
      dada.meteo$LwOut <- dada.meteo$LwOut_rawcor + 5.67e-8 * dada.meteo$Tsen_cnr4_K^4
      dada.meteo$SwNet <- NA
      dada.meteo$LwNet <- NA
      dada.meteo$Albedo <- NA
      # data met
      dada.meteo$prec_tot_count <- dada.meteo$prec * 5
      dada.meteo$PA <- NA
      dada.meteo$PA_mbar <- NA
      dada.meteo$Tair_b_50 <- NA
      dada.meteo$Tair_b_200 <- NA
      dada.meteo$wind_vmax_300 <- NA
      dada.meteo$wind_vmin_300 <- NA
      dada.meteo$Tpan <- NA
      
      
      dada.soil <- dada.meteo[, c("UTC", "Ts_1", "Ts_3", "Ts_6", "WT_raw", "WT", "WL")]
      dada.rad <- dada.meteo[, c("UTC", "SwIn", "SwOut", "LwIn", "LwOut",
                                 "Tsen_cnr4_C", "Tsen_cnr4_K", "LwIn_rawcor", "LwOut_rawcor",
                                 "SwNet", "LwNet", "Albedo", "RadNet")]
      dada.meteo <- dada.meteo[, c("UTC", "batt_U_min", "Tpan", "Tair_a_200", "Tair_a_50", "Tair_b_200", "Tair_b_50",
                                   "RH_200", "RH_50", "wind_v_300", "wind_deg_300", "wind_sddeg_300",
                                   "wind_vmax_300", "wind_vmin_300", "prec", "prec_tot_count", "PA", "PA_mbar")]
      ###...........................................................................
      ## step 1.12 a) special case 2011:2013: former files with different columns -----
      ## 
      ## set colnames
      ###...........................................................................
      
    } else if (spalten.met == 17 & year %in% c(2011, 2012, 2013)) {#
      #cat("zock 17", year, files.01[i], "\n")
      colnames(dada.meteo) <- c("UTC", "rec", "batt_U", "Tair_a_200", "RH_200",
                                "wind_v_300", "wind_deg_300",
                                "prec_tot_count",
                                "SwIn", "SwOut", "LwIn", "LwOut", ##### noch fraglich ob hier schon temperaturkorrigiert wurde
                                "Tsen_cnr4_C", "Tsen_cnr4_K",
                                "SwNet", "LwNet", "Albedo")
      # data soil
      dada.meteo$vwc_WT <- NA
      dada.meteo$WT_raw <- NA
      dada.meteo$WT <- NA
      dada.meteo$WL <- NA
      dada.meteo$Ts_1 <- NA
      dada.meteo$Ts_3 <- NA
      dada.meteo$Ts_6 <- NA
      # data rad
      dada.meteo$LwIn_rawcor <- NA
      dada.meteo$LwOut_rawcor <- NA
      # data met
      dada.meteo$prec <- dada.meteo$prec_tot_count * prec.fac.2
      dada.meteo$PA <- NA
      dada.meteo$PA_mbar <- NA
      dada.meteo$batt_U_min <- dada.meteo$batt_U
      dada.meteo$Tair_a_50 <- NA
      dada.meteo$Tair_b_50 <- NA
      dada.meteo$Tair_b_200 <- NA
      dada.meteo$RadNet <- NA
      dada.meteo$Tpan <- NA
      dada.meteo$RH_50 <- NA
      dada.meteo$wind_sddeg_300 <- NA
      dada.meteo$wind_vmax_300 <- NA
      dada.meteo$wind_vmin_300 <- NA
      
      
      
      dada.soil <- dada.meteo[, c("UTC", "Ts_1", "Ts_3", "Ts_6", "WT_raw", "WT", "WL")]
      dada.rad <- dada.meteo[, c("UTC", "SwIn", "SwOut", "LwIn", "LwOut",
                                 "Tsen_cnr4_C", "Tsen_cnr4_K", "LwIn_rawcor", "LwOut_rawcor",
                                 "SwNet", "LwNet", "Albedo", "RadNet")]
      dada.meteo <- dada.meteo[, c("UTC", "batt_U_min", "Tpan", "Tair_a_200", "Tair_a_50", "Tair_b_200", "Tair_b_50",
                                   "RH_200", "RH_50", "wind_v_300", "wind_deg_300", "wind_sddeg_300",
                                   "wind_vmax_300", "wind_vmin_300", "prec", "prec_tot_count", "PA", "PA_mbar")]
      ###...........................................................................
      ## step 1.12 a) special case 2016: now : former files with different columns -----
      ## 
      ## set colnames
      ###...........................................................................
      
    } else if (spalten.met == 17 & year >= 2015) {# since 2016
      #cat("zock 17", year, files.01[i],  "\n")
      colnames(dada.meteo) <- c("UTC", "rec", "batt_U_min", "Tpan", "Tair_a_50", "Tair_a_200", "Tair_b_50", "Tair_b_200",
                                "RH_50", "RH_200", "wind_v_300", "wind_deg_300", "wind_sddeg_300",  ### PSc: here was a error in read in: Tair and RH not in right way
                                "wind_vmax_300", "wind_vmin_300", "prec_tot_count", "PA_mbar")
      
      dada.meteo$prec <- dada.meteo$prec_tot_count * prec.fac.2
      dada.meteo$PA <- dada.meteo$PA_mbar / 10
      dada.meteo <- dada.meteo[, c("UTC", "batt_U_min", "Tpan", "Tair_a_200", "Tair_a_50", "Tair_b_200", "Tair_b_50",
                                   "RH_200", "RH_50", "wind_v_300", "wind_deg_300", "wind_sddeg_300",
                                   "wind_vmax_300", "wind_vmin_300", "prec", "prec_tot_count", "PA", "PA_mbar")]
      ###...........................................................................
      colnames(dada.rad) <- c("UTC", "rec", "SwIn", "SwOut", "LwIn_rawcor", "LwOut_rawcor",
                              "Tsen_cnr4_C", "Tsen_cnr4_K", "LwIn", "LwOut",
                              "SwNet", "LwNet", "Albedo", "RadNet")
      dada.rad <- dada.rad[, c("UTC", "SwIn", "SwOut", "LwIn", "LwOut",
                               "Tsen_cnr4_C", "Tsen_cnr4_K", "LwIn_rawcor", "LwOut_rawcor",
                               "SwNet", "LwNet", "Albedo", "RadNet")]
      ###...........................................................................
      if (length(dada.soil[1, ] == 4)) {
        dada.soil$Ts_3 <- NA
        dada.soil$Ts_6 <- NA
      }
      colnames(dada.soil) <- c("UTC", "rec", "Ts_1", "Ts_6", "Ts_3", "WT_raw", "vwc_WT")
      dada.soil$WT <- NA
      dada.soil$WL <- NA
      dada.soil <- dada.soil[, c("UTC", "Ts_1", "Ts_3", "Ts_6", "WT_raw", "WT", "WL")]
      
      ###...........................................................................
      ## step 1.12 a) special case 2009-2010: former files with different columns -----
      ## 
      ## set colnames
      ###...........................................................................
      
    } else if (spalten.met == 17 & year %in% c(2009, 2010)) {# 2009-2010
      #cat("zock 17", year, files.01[i], "\n")
      colnames(dada.meteo) <- c("UTC", "rec", "batt_U", "Tair_a_200", "RH_200",
                                "wind_v_300", "wind_deg_300", #"wind_sddeg_300",
                                "prec_tot_count",
                                "SwIn", "SwOut", "LwIn_rawcor", "LwOut_rawcor",  # noch nicht temperatur korrigiert
                                #"SwIn_raw", #"CM3Up_raw_Avg",
                                #"SwOut_raw", #"CM3Dn_raw_Avg",
                                #"LwIn_raw", #"CG3Up_raw_Avg",
                                #"LwOut_raw", #"CG3Dn_raw_Avg",
                                "Tsen_cnr4_C",
                                "Tsen_cnr4_K",
                                "SwNet", "LwNet", "Albedo")
      
      
      dada.meteo$batt_U_min <- dada.meteo$batt_U
      dada.meteo$Tair_a_50 <- NA
      dada.meteo$RH_50 <- NA
      dada.meteo$Tair_b_200 <- NA
      dada.meteo$Tair_b_50 <- NA
      dada.meteo$wind_vmin_300 <- NA
      dada.meteo$wind_sddeg_300 <- NA #no wind_sddeg_300 in raw data
      dada.meteo$prec <- dada.meteo$prec_tot_count * prec.fac.1
      dada.meteo$PA_mbar <- NA
      dada.meteo$PA <- NA
      dada.meteo$Tpan <- NA
      dada.meteo$wind_vmax_300 <- NA
      #      dada.meteo$Tsen_cnr4_K      <- dada.meteo$Tsen_cnr4_C+273.15
      dada.rad <- dada.meteo[, c("UTC", "SwIn", "SwOut", "LwIn_rawcor", "LwOut_rawcor", "Tsen_cnr4_C", "Tsen_cnr4_K", "SwNet", "LwNet", "Albedo")]
      
      dada.rad$LwIn <- dada.rad$LwIn_rawcor + 5.67e-8 * dada.rad$Tsen_cnr4_K^4
      dada.rad$LwOut <- dada.rad$LwOut_rawcor + 5.67e-8 * dada.rad$Tsen_cnr4_K^4
      #      dada.rad$SwNet              <- dada.rad$SwIn-dada.rad$SwOut
      #      dada.rad$LwNet              <- dada.rad$LwIn-dada.rad$LwOut
      #      dada.rad$Albedo             <- dada.rad$SwOut/dada.rad$SwIn
      dada.rad$RadNet <- dada.rad$SwNet + dada.rad$LwNet
      
      dada.rad <- dada.rad[, c("UTC", "SwIn", "SwOut", "LwIn", "LwOut", "Tsen_cnr4_C", "Tsen_cnr4_K",
                               "LwIn_rawcor", "LwOut_rawcor", "SwNet", "LwNet", "Albedo", "RadNet")]
      dada.soil <- dada.meteo[, c("UTC", "batt_U")]
      dada.soil$Ts_1 <- NA
      dada.soil$Ts_3 <- NA
      dada.soil$Ts_6 <- NA
      dada.soil$WT_raw <- NA
      dada.soil$WT <- NA
      dada.soil$WL <- NA
      dada.soil <- dada.soil[, -2]
      dada.meteo <- dada.meteo[, c("UTC", "batt_U_min", "Tpan",
                                   "Tair_a_200", "Tair_a_50", "Tair_b_200", "Tair_b_50",
                                   "RH_200", "RH_50",
                                   "wind_v_300", "wind_deg_300", "wind_sddeg_300", "wind_vmax_300", "wind_vmin_300",
                                   "prec", "prec_tot_count", "PA_mbar", "PA")]
      
      
      ###...........................................................................
      ## step 1.12 c) standard case  2009-2010 -----
      ## 
      ## set original colnames
      ###...........................................................................
      
    } else if (spalten.met %in% c(18, 19)) {# 2009-2010
      if (spalten.met == 19) {
        #cat("zock 19", year, files.01[i], "\n")
        colnames(dada.meteo) <- c("UTC", "rec", "batt_U", "Tair_a_200", "RH_200",
                                  "wind_v_300_2", "wind_v_300", "wind_deg_300", "wind_sddeg_300", ### ### no windvmax!!! 2 x wind_v_300 evtl. "wind_vmax_300" & "wind_v_300" anders herum?
                                  "prec_tot_count",
                                  "SwIn", "SwOut", "LwIn_rawcor", "LwOut_rawcor",
                                  "Tsen_cnr4_C", "Tsen_cnr4_K", #"RadNet", "LwIn", "Albedo"
                                  "SwNet", "LwNet", "Albedo")
        
        
      } else {
        #cat("zock 18", year, files.01[i], "\n")
        colnames(dada.meteo) <- c("UTC", "rec", "batt_U", "Tair_a_200", "RH_200",
                                  "wind_v_300", "wind_deg_300", "wind_sddeg_300",
                                  "prec_tot_count",
                                  "SwIn", "SwOut", "LwIn_rawcor", "LwOut_rawcor",
                                  "SwIn_raw", #"CM3Up_raw_Avg",
                                  "SwOut_raw", #"CM3Dn_raw_Avg",
                                  "LwIn_raw", #"CG3Up_raw_Avg",
                                  "LwOut_raw", #"CG3Dn_raw_Avg",
                                  "Tsen_cnr4_C"#, "Tsen_cnr4_K", "RadNet", "LwIn", "Albedo"
        )
        dada.meteo$Tsen_cnr4_K <- dada.meteo$Tsen_cnr4_C + 273.15
      }
      
      dada.meteo$batt_U_min <- dada.meteo$batt_U
      dada.meteo$Tair_a_50 <- NA
      dada.meteo$RH_50 <- NA
      dada.meteo$Tair_b_200 <- NA
      dada.meteo$Tair_b_50 <- NA
      dada.meteo$wind_vmin_300 <- NA
      dada.meteo$prec <- dada.meteo$prec_tot_count * prec.fac.2
      dada.meteo$PA_mbar <- NA
      dada.meteo$PA <- NA
      dada.meteo$Tpan <- NA
      dada.meteo$wind_vmax_300 <- NA
      ##dada.meteo$Tsen_cnr4_K      <- dada.meteo$Tsen_cnr4_C+273.15
      
      dada.rad <- dada.meteo[, c("UTC", "SwIn", "SwOut", "LwIn_rawcor", "LwOut_rawcor", "Tsen_cnr4_C", "Tsen_cnr4_K")]
      
      dada.rad$LwIn <- dada.rad$LwIn_rawcor + 5.67e-8 * dada.rad$Tsen_cnr4_K ^ 4
      dada.rad$LwOut <- dada.rad$LwOut_rawcor + 5.67e-8 * dada.rad$Tsen_cnr4_K ^ 4
      dada.rad$SwNet <- dada.rad$SwIn - dada.rad$SwOut
      dada.rad$LwNet <- dada.rad$LwIn - dada.rad$LwOut
      dada.rad$Albedo <- dada.rad$SwOut / dada.rad$SwIn
      dada.rad$RadNet <- dada.rad$SwNet + dada.rad$LwNet
      
      dada.rad <- dada.rad[, c("UTC", "SwIn", "SwOut", "LwIn", "LwOut", "Tsen_cnr4_C", "Tsen_cnr4_K",
                               "LwIn_rawcor", "LwOut_rawcor", "SwNet", "LwNet", "Albedo", "RadNet")]
      
      
      
      dada.soil <- dada.meteo[, c("UTC", "batt_U")]
      dada.soil$Ts_1 <- NA
      dada.soil$Ts_3 <- NA
      dada.soil$Ts_6 <- NA
      dada.soil$WT_raw <- NA
      dada.soil$WT <- NA
      dada.soil$WL  <- NA#; dada.soil$vwc_WT <- NA
      dada.soil <- dada.soil[, -2]
      dada.meteo <- dada.meteo[, c("UTC", "batt_U_min", "Tpan",
                                   "Tair_a_200", "Tair_a_50", "Tair_b_200", "Tair_b_50",
                                   "RH_200", "RH_50",
                                   "wind_v_300", "wind_deg_300", "wind_sddeg_300", "wind_vmax_300", "wind_vmin_300",
                                   "prec", "prec_tot_count", "PA_mbar", "PA")]
      
      
      ###...........................................................................
      ## step 1.12 c) standard case 2007-2010 ----
      ## 
      ## set original colnames
      ###...........................................................................
      
    } else if (spalten.met == 12 & year %in% c(2007, 2008, 2009, 2010)) {# 2007-2010
      #cat("zock 12", year, files.01[i], "\n")
      colnames(dada.meteo) <- c("UTC", "Ts_1", "Ts_6", "Ts_3", "WT_raw",
                                "vwc_WT", "Vm_1_Avg", "Vm_2_Avg", "R_1_Avg", "R_2_Avg", # not used
                                "Tair_a_50", "RH_50")
      
      dada.meteo$batt_U_min       <- NA
      dada.meteo$Tair_a_200       <- NA
      dada.meteo$RH_200           <- NA
      dada.meteo$Tair_b_200       <- NA
      dada.meteo$Tair_b_50        <- NA
      dada.meteo$wind_vmin_300    <- NA
      dada.meteo$prec             <- NA
      dada.meteo$prec_tot_count   <- NA
      dada.meteo$PA_mbar          <- NA
      dada.meteo$PA               <- NA
      dada.meteo$Tpan             <- NA
      dada.meteo$wind_v_300       <- NA
      dada.meteo$wind_deg_300     <- NA
      dada.meteo$wind_sddeg_300   <- NA
      dada.meteo$wind_vmax_300    <- NA
      dada.meteo$wind_vmin_300    <- NA
      
      dada.rad                   <- dada.meteo[, c("UTC", "Ts_1")]#  Ts_1 as dummy column (is not used)
      
      dada.rad$LwIn               <- NA
      dada.rad$LwOut              <- NA
      dada.rad$SwNet              <- NA
      dada.rad$LwNet              <- NA
      dada.rad$Albedo             <- NA
      dada.rad$RadNet             <- NA
      dada.rad$SwIn               <- NA
      dada.rad$SwOut              <- NA
      dada.rad$LwIn_rawcor        <- NA
      dada.rad$LwOut_rawcor       <- NA
      dada.rad$Tsen_cnr4_C        <- NA
      dada.rad$Tsen_cnr4_K        <- NA
      
      
      dada.rad <- dada.rad[, c("UTC", "SwIn", "SwOut", "LwIn", "LwOut", "Tsen_cnr4_C", "Tsen_cnr4_K",
                               "LwIn_rawcor", "LwOut_rawcor", "SwNet", "LwNet", "Albedo", "RadNet")]
      
      
      dada.soil      <- dada.meteo[, c("UTC", "Ts_1", "Ts_3", "Ts_6", "WT_raw")]
      
      
      dada.soil$WT            <- NA
      dada.soil$WL            <- NA
      
      dada.meteo    <- dada.meteo[, c("UTC", "batt_U_min", "Tpan",
                                      "Tair_a_200", "Tair_a_50", "Tair_b_200", "Tair_b_50",
                                      "RH_200", "RH_50",
                                      "wind_v_300", "wind_deg_300", "wind_sddeg_300", "wind_vmax_300", "wind_vmin_300",
                                      "prec", "prec_tot_count", "PA_mbar", "PA")]
      
      
      ###...........................................................................
      ## step 1.12 c) standard case 2014 ----
      ## 
      ## set original colnames
      ###...........................................................................
      
    } else { # 2014
      #cat("----", spalten.met, year, files.01[i], "\n")
      colnames(dada.meteo)    <- c("UTC", "batt_U_min", "Tpan", "Tair_a_50", "Tair_a_200", "Tair_b_50", "Tair_b_200",
                                   "RH_50", "RH_200", "wind_v_300", "wind_deg_300", "wind_sddeg_300", ### PSc: here was a error in read in: Tair and RH not in right way
                                   "wind_vmax_300", "wind_vmin_300", "prec_tot_count", "PA_mbar")
      colnames(dada.rad)      <- c("UTC", "SwIn", "SwOut", "LwIn_rawcor", "LwOut_rawcor", "Tsen_cnr4_C", "Tsen_cnr4_K",
                                   "LwIn", "LwOut", "SwNet", "LwNet", "Albedo", "RadNet")
      colnames(dada.soil)     <- c("UTC", "Ts_1", "Ts_6", "Ts_3", "WT_raw", "vwc_WT")
      dada.soil$WT            <- NA
      dada.soil$WL            <- NA
      dada.soil <- dada.soil[, c("UTC", "Ts_1", "Ts_3", "Ts_6", "WT_raw", "WT", "WL")]
      
      dada.meteo$prec           <- dada.meteo$prec_tot_count * prec.fac.2
      dada.meteo$PA             <- dada.meteo$PA_mbar / 10
      dada.meteo <- dada.meteo[, c("UTC", "batt_U_min", "Tpan", "Tair_a_200", "Tair_a_50", "Tair_b_200", "Tair_b_50",
                                   "RH_200", "RH_50", "wind_v_300", "wind_deg_300", "wind_sddeg_300",
                                   "wind_vmax_300", "wind_vmin_300", "prec", "prec_tot_count", "PA", "PA_mbar")]
    }
    
    ###...........................................................................
    ## step 1.14 b) merge input data with date table ----
    ## 
    ###...........................................................................
    # print(head(dada.meteo))
    #dada.meteo <-  unique(dada.meteo)
    newdf.meteo <- merge(compl.meteo, dada.meteo, all.x = T,  by = "UTC")
    newdf.rad   <- merge(compl.rad,   dada.rad,   all.x = T,  by = "UTC")
    newdf.soil  <- merge(compl.soil,  dada.soil,  all.x = T,  by = "UTC")
    #newdf.meteo[which(duplicated(format( as.POSIXct(newdf.meteo[1:17521, 1], origin = origin, tz = "UTC"), format = '%Y-%m-%d %H:%M')) == T), 1]
    #format( as.POSIXct(1121220000, origin = origin, tz = "UTC"), format = '%Y-%m-%d %H:%M %j')
    ###...........................................................................
    ## step 1.15 merge date table with storing table ----
    ## 
    ###...........................................................................
    
    for (k in 2:18) {
      db.samet.meteo[, k] <- rowMeans(cbind(db.samet.meteo[, k], newdf.meteo[, k + 1]), na.rm = T)# here check if all not needed colums are removed
    }
    for (k in 2:13) {
      db.samet.rad[, k] <- rowMeans(cbind(db.samet.rad[, k], newdf.rad[, k + 1]), na.rm = T)# here check if all not needed colums are removed
    }
    for (k in 2:7) {
      db.samet.soil[, k] <- rowMeans(cbind(db.samet.soil[, k], newdf.soil[, k + 1]), na.rm = T)# here check if all not needed colums are removed
    }
    
    
  } # end SaMeteo
  
  
  
  ###...........................................................................
  ## step 1.16  convert numeric dates back to date format -----
  ##
  ###...........................................................................
  
  
  db.samet.meteo[, 1]   <- format( as.POSIXct(db.samet.meteo[, 1], origin = origin, tz = "UTC"), format = '%Y-%m-%d %H:%M')
  db.samet.rad[, 1]     <- format( as.POSIXct(db.samet.rad[, 1], origin = origin, tz = "UTC"), format = '%Y-%m-%d %H:%M')
  db.samet.soil[, 1]    <- format( as.POSIXct(db.samet.soil[, 1], origin = origin, tz = "UTC"), format = '%Y-%m-%d %H:%M')
  
  ###...........................................................................
  ## step 1.17 set "sparc" colnames -----
  ## 
  ###...........................................................................
  #
  colnames(db.samet.meteo) <-  c("UTC", "Ubat", "Tpan", "Tair_a_200", "Tair_a_50", "Tair_b_200", "Tair_b_50",
                                 "RH_200", "RH_50", "wind_v_300", "wind_deg_300", "wind_sddeg_300",
                                 "wind_vmax_300", "wind_vmin_300", "prec", "prec_tot_count", "PA", "PA_mbar")
  
  colnames(db.samet.rad)  <- c("UTC", "SwIn", "SwOut", "LwIn", "LwOut", "Tsen_cnr4_C", "Tsen_cnr4_K",
                               "LwIn_rawcor", "LwOut_rawcor", "SwNet", "LwNet", "Albedo", "RadNet")
  colnames(db.samet.soil) <- c("UTC", "Ts_1", "Ts_3", "Ts_6", "WT_raw", "WT", "WL")
  
  ###...........................................................................
  ## step 1.18 calculate missing parameters -----
  ##
  ###...........................................................................
  
  if (year >= 2014) {
    db.samet.meteo$prec <- as.numeric(db.samet.meteo$prec_tot_count)*prec.fac.2 ## prec_tot_count --> prec
    db.samet.meteo$PA <- as.numeric(db.samet.meteo$PA_mbar)*0.1 ## PA_mbar --> PA(kPa)
  }
  for (m in 2:length(db.samet.meteo[1, ])) {# replace Nan with NA
    db.samet.meteo[is.nan(as.numeric(db.samet.meteo[, m])), m] <- NA  }
  for (m in 2:length(db.samet.rad[1, ])) {# replace Nan with NA
    db.samet.rad[is.nan(as.numeric(db.samet.rad[, m])), m] <- NA  }
  for (m in 2:length(db.samet.soil[1, ])) {# replace Nan with NA
    db.samet.soil[is.nan(as.numeric(db.samet.soil[, m])), m] <- NA  }
  
  
  ###...........................................................................
  ## step 1.19 calculate waterlevel ----
  ##
  ###...........................................................................
  # - [CS616_PA_uS] < 19 --> no value --> WL waterlevel is too low or frozing/thawing
  # - [CS616_PA_uS] < 27 --> use this calculation: (0.01831394*[CS616_PA_uS]^3 - 1.2398*[CS616_PA_uS]^2 + 28.84699187*[CS616_PA_uS] - 224.42499308)
  # - [CS616_PA_uS] > 27 --> use this calculation: (0.06194726*[CS616_PA_uS]^2 - 1.7673294*[CS616_PA_uS] + 13.66709591)
  #db.samet.soil <- as.data.frame(db.samet.soil)
  
  #data.in <- db.samet.soil$WT_raw;lower.v=19;threshold=27
  calc.wt <- function(data.in, lower.v = 19, threshold = 27) {
    if (length(na.omit(as.numeric(data.in))) < 1) {return(data.in)} else {
      data.out <- data.in <- as.numeric(data.in)
      no     <- which(data.in < lower.v )
      low    <- which(data.in >= lower.v & data.in < threshold)
      upper  <- which(data.in >= threshold) 
      data.out[no] <- NA
      if (length(low) >= 1) {
        data.out[low  ] <- ((0.01831394 * data.in[low]^3) - (1.2398 * data.in[low]^2) + (28.84699187 * data.in[low]) - 224.42499308)
      }
      if (length(upper) >= 1) {
        data.out[upper] <- ((0.06194726 * data.in[upper]^2) - (1.7673294 * data.in[upper]) + 13.66709591)
      }
      return(data.out)
    }
  }
  if (year %in% c(2007:2009)) {
    db.samet.soil$WT             <- round(calc.wt(as.numeric(db.samet.soil$WT_raw)), 2) / 100
    db.samet.soil$WL             <- round(calc.wt(as.numeric(db.samet.soil$WT_raw)) - 15, 2) / 100
  } else if (year %in% c(2010)) {
    # change of sensor at 26.7.2010
    db.samet.soil$WT             <- round(calc.wt(as.numeric(db.samet.soil$WT_raw)), 2) / 100
    db.samet.soil$WL[1:9913]     <- round(calc.wt(as.numeric(db.samet.soil$WT_raw[1:9913])) - 15, 2) / 100
    db.samet.soil$WL[9914:17520] <- round(calc.wt(as.numeric(db.samet.soil$WT_raw[9914:17520])) - 11.5, 2) / 100
    
  } else if (year %in% c(2011:2025)) {
    db.samet.soil$WT             <- round(calc.wt(as.numeric(db.samet.soil$WT_raw)), 2) / 100
    db.samet.soil$WL             <- round(calc.wt(as.numeric(db.samet.soil$WT_raw)) - 11.5, 2) / 100
  }
  
  ###...........................................................................
  ## step 1.20 safe data to txt-file ----
  ##
  ###...........................................................................
  
  
  db.samet <- cbind(db.samet.meteo[, -c(16, 18)], db.samet.rad[, -1], db.samet.soil[, -1])
  write.table(db.samet, paste0(p.1$w[p.1$n == "LV0.p"], "SaMet2002/00_full_dataset/SaMet2002_", year, "_lv0.dat"), quote = F, dec = ".", sep = ",", row.names = F)
  #
  write.table(db.samet.meteo[, -c(16, 18)], paste0(p.1$w[p.1$n == "LV0.p"], "SaMet2002/01_meteo/SaMet2002_Meteo_", year, "_lv0.dat"), quote = F, dec = ".", sep = ",", row.names = F)
  write.table(db.samet.rad, paste0(p.1$w[p.1$n == "LV0.p"], "SaMet2002/02_radiation/SaMet2002_Rad_", year, "_lv0.dat"), quote = F, dec = ".", sep = ",", row.names = F)
  write.table(db.samet.soil, paste0(p.1$w[p.1$n == "LV0.p"], "SaMet2002/03_soil/SaMet2002_Soil_", year, "_lv0.dat"), quote = F, dec = ".", sep = ",", row.names = F)
  #
  cat("\n#\n# SaMet2002 ", year, " without problems!\n#\n") # main output
  
} # end loop over years



