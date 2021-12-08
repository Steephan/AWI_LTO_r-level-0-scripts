###............................................................................
##
##            RAW to Level0 -----
##   (SaSoil2012)
##   equal time steps, no gaps
##
##   by: Stephan.Lange@awi.de
##   last modified:
##   2021-11-11 SL rename WT to WL
##   2021-11-10 SL adapted to runapp, new dsn corrections and waterlevel calculations
##   2020-10-30 CL adapted to script guidelines and implementation in Samoylov_MAIN.R
##
###............................................................................
##
##   5 different input data file formats
##
##   check every inputs headers!!!!
##
##
##
###............................................................................
## step 1.01
## set path settings for different systems linux vs. windoof
###............................................................................
#  to run this script separately, you have to uncomment the next 10 lines!
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
###............................................................................
## step 1.02
## set running options years, ...
###............................................................................
# to run this script separately, you have to set run.year:
#
origin <- "1970-01-01"
# recent.year <- as.numeric(format(Sys.Date(), "%Y"))
# run.year <- 2014

# set option
ll <- 4

###............................................................................
## step 1.03
## loop 1 over years
###............................................................................
for (year_i in run.year) {
  ###............................................................................
  ## step 1.04
  ## set 2 empty tables with length of year_i
  ## columns: 2 (date table) and number of input table (storing table)
  ###............................................................................
  ##cat("\nProcessing year", year_i, "\n====================\n\n")
  start.date <- as.POSIXct(paste(year_i, "-01-01 00:00:00", sep = ""), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  end.date <- as.POSIXct(paste(year_i, "-", 12, "-", 31, " 23:30:00", sep = ""), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  # create empty data frame with UTC time stamp every 30 min
  db.sasoil.t <- matrix(ncol = 50, nrow = length(seq(start.date, end.date, by = "30 min")), -999)
  db.sasoil.t[, c(2:50)] <- NA
  db.sasoil.tdr <- matrix(ncol = 43, nrow = length(seq(start.date, end.date, by = "30 min")), -999)
  db.sasoil.tdr[, c(2:43)] <- NA
  db.sasoil.cold <- matrix(ncol = 9, nrow = length(seq(start.date, end.date, by = "30 min")), -999)
  db.sasoil.cold[, c(2:9)] <- NA
  db.sasoil.warm <- matrix(ncol = 13, nrow = length(seq(start.date, end.date, by = "30 min")), -999)
  db.sasoil.warm[, c(2:13)] <- NA
  db.sasoil.dyna <- matrix(ncol = 9, nrow = length(seq(start.date, end.date, by = "30 min")), -999)
  db.sasoil.dyna[, c(2:9)] <- NA
  compl.t <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "30 min")))
  compl.tdr <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "30 min")))
  compl.cold <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "30 min")))
  compl.warm <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "30 min")))
  compl.dyna <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "30 min")))
  
  db.sasoil.t[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = "%Y-%m-%d %H:%M:%S"))
  db.sasoil.tdr[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = "%Y-%m-%d %H:%M:%S"))
  db.sasoil.cold[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = "%Y-%m-%d %H:%M:%S"))
  db.sasoil.warm[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = "%Y-%m-%d %H:%M:%S"))
  db.sasoil.dyna[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = "%Y-%m-%d %H:%M:%S"))
  
  compl.t[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = "%Y-%m-%d %H:%M:%S"))
  compl.tdr[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = "%Y-%m-%d %H:%M:%S"))
  compl.cold[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = "%Y-%m-%d %H:%M:%S"))
  compl.warm[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = "%Y-%m-%d %H:%M:%S"))
  compl.dyna[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = "%Y-%m-%d %H:%M:%S"))
  colnames(compl.t) <- c("UTC", "erste")
  colnames(compl.tdr) <- c("UTC", "erste")
  colnames(compl.cold) <- c("UTC", "erste")
  colnames(compl.warm) <- c("UTC", "erste")
  colnames(compl.dyna) <- c("UTC", "erste")
  ###............................................................................
  ## step 1.05
  ## set input.path and list all files
  ###............................................................................
  inz.01.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaSoil2012/01_Temp/")
  files.01 <- list.files(inz.01.path, pattern = "*.dat")
  
  inz.02.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaSoil2012/02_Data_TDR/")
  files.02 <- list.files(inz.02.path, pattern = "*.dat")
  
  inz.03.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaSoil2012/06_Data_Cold/")
  files.03 <- list.files(inz.03.path, pattern = "*.dat")
  
  inz.04.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaSoil2012/07_Data_Warm/")
  files.04 <- list.files(inz.04.path, pattern = "*.dat")
  
  inz.05.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaSoil2012/08_Data_Dyna/")
  files.05 <- list.files(inz.05.path, pattern = "*.dat")
  ###............................................................................main input tasks             main input tasks
  ## soil temperature -----
  ## 
  ###............................................................................
  if (ll == 4) {
    for (i in 1:length(files.01)) { # soil temperature 1
      
      ###............................................................................
      ## step 1.07 read one file (skip headers, set NA-values)  ----
      ## 
      ## set temporal colnames
      ###............................................................................
      # #cat("\nprocessing ",files.01[i],"\n====================\n\n")
      dada.t <- read.table(paste(inz.01.path, files.01[i], sep = ""), sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")
      
      colnames(dada.t) <- paste0("V", seq_len(ncol(dada.t)))
      ###............................................................................
      ## step 1.09 ----
      ## check file if dates are in running year_i of loop 1
      ###............................................................................
      
      if (as.numeric(substr(lapply(dada.t[1, 1], as.character), 1, 4)) > year_i || as.numeric(substr(lapply(dada.t[length(dada.t[, 1]), 1], as.character), 1, 4)) < year_i) {
        next
      } # skip file if wrong year
      #cat(paste(dada.t[1, 1], "     to     ", dada.t[length(dada.t[, 1]), 1], "    ", files.01[i], "\n"))
      ###............................................................................
      ## step 1.10 ----
      ## check file for double entries
      ###............................................................................
      if (length(which(duplicated(dada.t[, 1]) == TRUE)) > 0) dada.t <- dada.t[-which(duplicated(dada.t[, 1]) == T), ]
      ###............................................................................
      ## step 1.11 ----
      ## convert date to numeric value
      ###............................................................................
      
      dada.t[, 1] <- as.numeric(as.POSIXct(dada.t[, 1], format = "%Y-%m-%d %H:%M:%S", origin = origin, tz = "UTC"))
      ###............................................................................
      ## step 1.12a ----
      ## special case: former files with different columns
      ## set colnames
      ###............................................................................
      colnames(dada.t) <- c(
        "UTC", "RECORD", "Tpan_CR3000", "Ubat",
        "Ts_rim_1", "Ts_rim_7", "Ts_rim_22", "Ts_rim_38", # 2x8 soil temperatures
        "Ts_rim_52", "Ts_rim_69", "Ts_rim_95", "Ts_rim_104",
        "Ts_cen_5", "Ts_cen_15", "Ts_cen_28", "Ts_cen_45",
        "Ts_cen_52", "Ts_cen_72", "Ts_cen_90", "Ts_cen_100",
        
        # "CM3Up_1"     ,"CM3Up_2"     ,"CM3Dn_1"     ,"CM3Dn_2"      ,# 2 radiation sensors
        "SwIn_rim", "SwIn_cen", "SwOut_rim", "SwOut_cen", # 2 radiation sensors
        # "CG3UpCo_1"   ,"CG3UpCo_2"   ,"CG3DnCo_1"   ,"CG3DnCo_2"    ,# calculated with wrong calibration ?
        "LwIn_rim", "LwIn_cen", "LwOut_rim", "LwOut_cen",
        "G_cen_95", "G_cen_13", "G_rim_98", "G_rim_13", # 4 Heatflux plates
        
        "distcor", "Tair_70", # 1 snowdepth sensor + temperature at 70cm
        # "CG3Up_1"     ,"CG3Up_2"     ,"CG3Dn_1"     ,"CG3Dn_2"      ,
        "LwIn_rim_raw", "LwIn_cen_raw", "LwOut_rim_raw", "LwOut_cen_raw",
        
        # "NR01_rim_1"    ,"NR01_rim_2"    ,"NR01_rim_3"    ,"NR01_rim_4"     ,
        # "NR01_cen_1"    ,"NR01_cen_2"    ,"NR01_cen_3"    ,"NR01_cen_4"     ,
        "SwIn_rim_mV", "SwOut_rim_mV", "LwIn_rim_mV", "LwOut_rim_mV",
        "SwIn_cen_mV", "SwOut_cen_mV", "LwIn_cen_mV", "LwOut_cen_mV",
        
        "Tsen_NR01_rim", "Tsen_NR01_cen",
        "distraw", "SQsn"
      )
      
      if (i <= 50) {
        
        # ab 2016
        # Const CM3Up_Sens_1 = 11.5
        # Const CM3Dn_Sens_1 = 15.09
        # Const CG3Up_Sens_1 = 12.65
        # Const CG3Dn_Sens_1 = 10.97
        # Const CM3Up_Sens_2 = 22.95
        # Const CM3Dn_Sens_2 = 19.63
        # Const CG3Up_Sens_2 = 7.97
        # Const CG3Dn_Sens_2 = 7.47
        dada.t$SwIn_rim       <- (dada.t$SwIn_rim_mV  * 1000) / 13.79
        dada.t$SwOut_rim      <- (dada.t$SwOut_rim_mV * 1000) / 15.84
        dada.t$LwIn_rim_raw   <- (dada.t$LwIn_rim_mV  * 1000) /  9.34
        dada.t$LwOut_rim_raw  <- (dada.t$LwOut_rim_mV * 1000) /  7.79
        dada.t$SwIn_cen       <- (dada.t$SwIn_cen_mV  * 1000) / 14.02
        dada.t$SwOut_cen      <- (dada.t$SwOut_cen_mV * 1000) / 14.76
        dada.t$LwIn_cen_raw   <- (dada.t$LwIn_cen_mV  * 1000) /  7.19
        dada.t$LwOut_cen_raw  <- (dada.t$LwOut_cen_mV * 1000) /  8.88
        
        
        dada.t$LwIn_rim       <- dada.t$LwIn_rim_raw  + 5.67 * 10^-8 * (dada.t$Tsen_NR01_rim + 273.15)^4
        dada.t$LwOut_rim      <- dada.t$LwOut_rim_raw + 5.67 * 10^-8 * (dada.t$Tsen_NR01_rim + 273.15)^4
        dada.t$LwIn_cen       <- dada.t$LwIn_cen_raw  + 5.67 * 10^-8 * (dada.t$Tsen_NR01_cen + 273.15)^4
        dada.t$LwOut_cen      <- dada.t$LwOut_cen_raw + 5.67 * 10^-8 * (dada.t$Tsen_NR01_cen + 273.15)^4
      }
      ##  hier fehlen noch die einzelnen Tiefen aller Jahre
      
      dada.t$Dsn <- 1.38 - dada.t$distcor
      
      
      
      
      
      
      
      
      dada.t <- dada.t[, c(
        "UTC", "Tpan_CR3000", "Ubat",
        "Tair_70",
        "Ts_cen_5", "Ts_cen_15", "Ts_cen_28", "Ts_cen_45",
        "Ts_cen_52", "Ts_cen_72", "Ts_cen_90", "Ts_cen_100",
        "Ts_rim_1", "Ts_rim_7", "Ts_rim_22", "Ts_rim_38",
        "Ts_rim_52", "Ts_rim_69", "Ts_rim_95", "Ts_rim_104",
        
        "SwIn_cen", "SwIn_rim",
        "SwOut_cen", "SwOut_rim",
        "SwIn_cen_mV", "SwIn_rim_mV",
        "SwOut_cen_mV", "SwOut_rim_mV",
        
        "LwIn_cen", "LwIn_rim",
        "LwOut_cen", "LwOut_rim",
        "LwIn_cen_raw", "LwIn_rim_raw",
        "LwOut_cen_raw", "LwOut_rim_raw",
        "LwIn_cen_mV", "LwIn_rim_mV",
        "LwOut_cen_mV", "LwOut_rim_mV",
        
        "Tsen_NR01_cen", "Tsen_NR01_rim",
        
        "G_cen_13", "G_cen_95", "G_rim_13", "G_rim_98",
        
        "Dsn", "distcor", "distraw", "SQsn"
      )]
      
      
      newdf.t <- merge(compl.t, dada.t, all.x = T, by = "UTC")
      
      ###............................................................................
      ## step 1.15 ----
      ## merge date table with storing table
      ###............................................................................
      
      for (k in 2:(length(db.sasoil.t[1, ]))) {
        db.sasoil.t[, k] <- rowMeans(cbind(db.sasoil.t[, k], newdf.t[, k + 1]), na.rm = T) #
      }
    }
  } # soil temperature
  ###............................................................................soil temperature
  ## soil moisture -----
  ## loop 2 over all datatdr files
  ###............................................................................
  if (ll == 4) {
    for (i in 1:length(files.02)) { # soil moisture  1
      ###............................................................................
      ## step 1.07b
      ## read one file (skip headers, set NA-values)
      ## set temporal colnames
      ###............................................................................
      # cat("\nprocessing ",files.02[i],"\n====================\n\n")
      dada.tdr <- read.table(paste(inz.02.path, files.02[i], sep = ""), sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")
      
      colnames(dada.tdr) <- paste0("V", seq_len(ncol(dada.tdr)))
      ###............................................................................
      ## step 1.09b ----
      ## check file if dates are in running year_i of loop 1
      ###............................................................................
      
      if (as.numeric(substr(lapply(dada.tdr[1, 1], as.character), 1, 4)) > year_i || as.numeric(substr(lapply(dada.tdr[length(dada.tdr[, 1]), 1], as.character), 1, 4)) < year_i) {
        next
      } # skip file if wrong year
      # cat(paste(dada.tdr[1, 1], "     to     ", dada.tdr[length(dada.tdr[, 1]), 1], "    ", files.02[i], "\n"))
      ###............................................................................
      ## step 1.10b ----
      ## check file for double entries
      ###............................................................................
      if (length(which(duplicated(dada.tdr[, 1]) == TRUE)) > 0) dada.tdr <- dada.tdr[-which(duplicated(dada.tdr[, 1]) == T), ]
      ###............................................................................
      ## step 1.11b ----
      ## convert date to numeric value
      ###............................................................................
      
      dada.tdr[, 1] <- as.numeric(as.POSIXct(dada.tdr[, 1], format = "%Y-%m-%d %H:%M:%S", origin = origin, tz = "UTC"))
      ###............................................................................
      ## step 1.12b ----
      ## special case: former files with different columns
      ## set colnames
      ###............................................................................
      
      colnames(dada.tdr) <- c(
        "UTC", "RECORD",
        "E2_cen_95", "E2_cen_72", "E2_cen_44", "E2_cen_14", "E2_rim_9", "E2_sn_20", "E2_sn_5",
        "E2_cen_v_16", "E2_rim_v_22", "E2_rim_24", "E2_rim_38", "E2_rim_52", "E2_rim_100",
        
        "cond_cen_95", "cond_cen_72", "cond_cen_44", "cond_cen_14", "cond_rim_9", "cond_sn_20", "cond_sn_5",
        "cond_cen_v_16", "cond_rim_v_22", "cond_rim_24", "cond_rim_38", "cond_rim_52", "cond_rim_100",
        
        "vwc_cen_95", "vwc_cen_72", "vwc_cen_44", "vwc_cen_14", "vwc_rim_9", "vwc_sn_20", "vwc_sn_5",
        "vwc_cen_v_16", "vwc_rim_v_22", "vwc_rim_24", "vwc_rim_38", "vwc_rim_52", "vwc_rim_100"
      )
      
      ###............................................................................
      ## step 1.12b ----
      ## add additional columns to former dataset
      ###............................................................................
      for (i in 3:15) { # convert La/L to dielectricity
        dada.tdr[, i] <- dada.tdr[, i]^2
      }
      dada.tdr$WL_cen_1 <- NA ## TDR
      dada.tdr$WL_cen_2 <- NA ## SR50
      dada.tdr$WL_rim_1 <- NA ## TDR
      
      dada.tdr <- dada.tdr[, c(
        "UTC", "WL_cen_1", "WL_cen_2", "WL_rim_1",
        "E2_sn_5", "E2_sn_20",
        "E2_cen_v_16", "E2_cen_14", "E2_cen_44", "E2_cen_72", "E2_cen_95",
        "E2_rim_v_22", "E2_rim_9", "E2_rim_24", "E2_rim_38", "E2_rim_52", "E2_rim_100",
        "cond_sn_5", "cond_sn_20",
        "cond_cen_v_16", "cond_cen_14", "cond_cen_44", "cond_cen_72", "cond_cen_95",
        "cond_rim_v_22", "cond_rim_9", "cond_rim_24", "cond_rim_38", "cond_rim_52", "cond_rim_100",
        "vwc_sn_5", "vwc_sn_20",
        "vwc_cen_v_16", "vwc_cen_14", "vwc_cen_44", "vwc_cen_72", "vwc_cen_95",
        "vwc_rim_v_22", "vwc_rim_9", "vwc_rim_24", "vwc_rim_38", "vwc_rim_52", "vwc_rim_100"
      )]
      
      ###............................................................................
      ## step 1.14b ----
      ## merge input data with date table
      ###............................................................................
      
      newdf.tdr <- merge(compl.tdr, dada.tdr, all.x = T, by = "UTC")
      
      ###............................................................................
      ## step 1.15b ----
      ## merge date table with storing table
      ###............................................................................
      
      for (k in 2:(length(db.sasoil.tdr[1, ]))) {
        db.sasoil.tdr[, k] <- rowMeans(cbind(db.sasoil.tdr[, k], newdf.tdr[, k + 1]), na.rm = T) #
      }
    }
  } # soil moisture
  ###............................................................................soil temperature
  ## datacold -----
  ## loop 2 over all datacold files
  ###............................................................................
  if (ll == 4) {
    for (i in 1:length(files.03)) { # datacold
      ###............................................................................
      ## step 1.07b ----
      ## read one file (skip headers, set NA-values)
      ## set temporal colnames
      ###............................................................................
      # #cat("\nprocessing ",files.03[i],"\n====================\n\n")
      dada.cold <- read.table(paste(inz.03.path, files.03[i], sep = ""), sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")
      
      colnames(dada.cold) <- paste0("V", seq_len(ncol(dada.cold)))
      ###............................................................................
      ## step 1.09b ----
      ## check file if dates are in running year_i of loop 1
      ###............................................................................
      
      if (as.numeric(substr(lapply(dada.cold[1, 1], as.character), 1, 4)) > year_i || as.numeric(substr(lapply(dada.cold[length(dada.cold[, 1]), 1], as.character), 1, 4)) < year_i) {
        next
      } # skip file if wrong year
      #cat(paste(dada.cold[1, 1], "     to     ", dada.cold[length(dada.cold[, 1]), 1], "    ", files.03[i], "\n"))
      ###............................................................................
      ## step 1.10b ----
      ## check file for double entries
      ###............................................................................
      if (length(which(duplicated(dada.cold[, 1]) == TRUE)) > 0) dada.cold <- dada.cold[-which(duplicated(dada.cold[, 1]) == T), ]
      ###............................................................................
      ## step 1.11b ----
      ## convert date to numeric value
      ###............................................................................
      
      dada.cold[, 1] <- as.numeric(as.POSIXct(dada.cold[, 1], format = "%Y-%m-%d %H:%M:%S", origin = origin, tz = "UTC"))
      ###............................................................................
      ## step 1.12b ----
      ## special case: former files with different columns
      ## set colnames
      ###............................................................................
      ## ' Sensor output values in cold, stable state in addition also the actual battery
      ## ' voltage is saved.
      ##   DataTable (DataCold,write,-1)
      ##    CardOut (0 ,-1)
      ##   Sample (4,U_sen(),FP2)
      ##   Sample (4,U_heat(),FP2)
      ##    EndTable
      ###............................................................................
      # colnames(dada.cold)<-c("UTC","RECORD",
      #                     "U_sen1","U_sen2","U_sen3","U_sen4",
      #                     "U_heat1","U_heat2","U_heat3","U_heat4")
      colnames(dada.cold) <- c(
        "UTC", "RECORD",
        "Usen_w_cen_95", "Usen_w_cen_45", "Usen_w_rim_99", "Usen_w_rim_16",
        "Usen_h_cen_95", "Usen_h_cen_45", "Usen_h_rim_99", "Usen_h_rim_16"
      )
      
      ###............................................................................
      ## step 1.12b ----
      ## add additional columns to former dataset
      ###............................................................................
      # dada.cold<-dada.cold[,c("UTC",
      #                         "U_sen1","U_sen2","U_sen3","U_sen4",
      #                         "U_heat1","U_heat2","U_heat3","U_heat4")]
      dada.cold <- dada.cold[, c(
        "UTC",
        "Usen_w_cen_45", "Usen_w_cen_95", "Usen_w_rim_16", "Usen_w_rim_99",
        "Usen_h_cen_45", "Usen_h_cen_95", "Usen_h_rim_16", "Usen_h_rim_99"
      )]
      ###............................................................................
      ## step 1.14b ----
      ## merge input data with date table
      ###............................................................................
      
      newdf.cold <- merge(compl.cold, dada.cold, all.x = T, by = "UTC")
      
      ###............................................................................
      ## step 1.15b ----
      ## merge date table with storing table
      ###............................................................................
      
      for (k in 2:(length(db.sasoil.cold[1, ]))) {
        db.sasoil.cold[, k] <- rowMeans(cbind(db.sasoil.cold[, k], newdf.cold[, k + 1]), na.rm = T) #
      }
    }
  } # datacold
  ###............................................................................soil temperature
  ## datawarm -----
  ## loop 3 over all datawarm files
  ###............................................................................
  if (ll == 4) {
    for (i in 1:length(files.04)) { # soil moisture  1
      
      ###............................................................................
      ## step 1.07b ----
      ## read one file (skip headers, set NA-values)
      ## set temporal colnames
      ###............................................................................
      # #cat("\nprocessing ",files.04[i],"\n====================\n\n")
      dada.warm <- read.table(paste(inz.04.path, files.04[i], sep = ""), sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")
      
      colnames(dada.warm) <- paste0("V", seq_len(ncol(dada.warm)))
      ###............................................................................
      ## step 1.09b ----
      ## check file if dates are in running year_i of loop 1
      ###............................................................................
      
      if (as.numeric(substr(lapply(dada.warm[1, 1], as.character), 1, 4)) > year_i || as.numeric(substr(lapply(dada.warm[length(dada.warm[, 1]), 1], as.character), 1, 4)) < year_i) {
        next
      } # skip file if wrong year
      #cat(paste(dada.warm[1, 1], "     to     ", dada.warm[length(dada.warm[, 1]), 1], "    ", files.04[i], "\n"))
      ###............................................................................
      ## step 1.10b ----
      ## check file for double entries
      ###............................................................................
      if (length(which(duplicated(dada.warm[, 1]) == TRUE)) > 0) dada.warm <- dada.warm[-which(duplicated(dada.warm[, 1]) == T), ]
      ###............................................................................
      ## step 1.11b ----
      ## convert date to numeric value
      ###............................................................................
      
      dada.warm[, 1] <- as.numeric(as.POSIXct(dada.warm[, 1], format = "%Y-%m-%d %H:%M:%S", origin = origin, tz = "UTC"))
      ###............................................................................
      ## step 1.12b ----
      ## special case: former files with different columns
      ## set colnames
      ###............................................................................
      ## 'Sensor output values and computed parameters in warm, stable state
      ##   DataTable (DataWarm,true,-1)
      ##    CardOut (0 ,-1)
      ##   Sample (4,U_sen0(),FP2)
      ##   Sample (4,U_senamp(),FP2)
      ##   Sample (4,condTh(),FP2)
      ##  EndTable
      ###............................................................................
      # colnames(dada.warm)<-c("UTC","RECORD",
      #                        "U_sen01","U_sen02","U_sen03","U_sen04",
      #                        "U_senamp1","U_senamp2","U_senamp3","U_senamp4",
      #                        "tcond1","condTh2","condTh3","condTh4")
      colnames(dada.warm) <- c(
        "UTC", "RECORD",
        "Usen_c_cen_95", "Usen_c_cen_45", "Usen_c_rim_99", "Usen_c_rim_16",
        "Usen_amp_cen_95", "Usen_amp_cen_45", "Usen_amp_rim_99", "Usen_amp_rim_16",
        "tcond_cen_95", "tcond_cen_45", "tcond_rim_99", "tcond_rim_16"
      )
      
      ###............................................................................
      ## step 1.12b ----
      ## add additional columns to former dataset
      ###............................................................................
      
      
      # dada.warm<-dada.warm[,c("UTC",#"RECORD",
      #                         "U_sen01","U_sen02","U_sen03","U_sen04",
      #                         "U_senamp1","U_senamp2","U_senamp3","U_senamp4",
      #                         "condTh1","condTh2","condTh3","condTh4")]
      dada.warm <- dada.warm[, c(
        "UTC",
        "Usen_c_cen_45", "Usen_c_cen_95", "Usen_c_rim_16", "Usen_c_rim_99",
        "Usen_amp_cen_45", "Usen_amp_cen_95", "Usen_amp_rim_16", "Usen_amp_rim_99",
        "tcond_cen_45", "tcond_cen_95", "tcond_rim_16", "tcond_rim_99"
      )]
      
      ###............................................................................
      ## step 1.14b ----
      ## merge input data with date table
      ###............................................................................
      
      newdf.warm <- merge(compl.warm, dada.warm, all.x = T, by = "UTC")
      
      ###............................................................................
      ## step 1.15b ----
      ## merge date table with storing table
      ###............................................................................
      
      for (k in 2:(length(db.sasoil.warm[1, ]))) {
        db.sasoil.warm[, k] <- rowMeans(cbind(db.sasoil.warm[, k], newdf.warm[, k + 1]), na.rm = T) #
      }
    }
  } # datawarm
  ###............................................................................soil temperature
  ## datadyna -----
  ## loop 4 over all datadyna files
  ###............................................................................
  if (ll == 4) {
    for (i in 1:length(files.05)) { # soil moisture
      
      ###............................................................................
      ## step 1.07b ----
      ## read one file (skip headers, set NA-values)
      ## set temporal colnames
      ###............................................................................
      # #cat("\nprocessing ",files.05[i],"\n====================\n\n")
      dada.dyna <- read.table(paste(inz.05.path, files.05[i], sep = ""), sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")
      
      colnames(dada.dyna) <- paste0("V", seq_len(ncol(dada.dyna)))
      ###............................................................................
      ## step 1.09b ----
      ## check file if dates are in running year_i of loop 1
      ###............................................................................
      
      if (as.numeric(substr(lapply(dada.dyna[1, 1], as.character), 1, 4)) > year_i || as.numeric(substr(lapply(dada.dyna[length(dada.dyna[, 1]), 1], as.character), 1, 4)) < year_i) {
        next
      } # skip file if wrong year
      #cat(paste(dada.dyna[1, 1], "     to     ", dada.dyna[length(dada.dyna[, 1]), 1], "    ", files.02[i], "\n"))
      ###............................................................................
      ## step 1.10b ----
      ## check file for double entries
      ###............................................................................
      if (length(which(duplicated(dada.dyna[, 1]) == TRUE)) > 0) dada.dyna <- dada.dyna[-which(duplicated(dada.dyna[, 1]) == T), ]
      ###............................................................................
      ## step 1.11b
      ## convert date to numeric value
      ###............................................................................
      
      dada.dyna[, 1] <- as.numeric(as.POSIXct(dada.dyna[, 1], format = "%Y-%m-%d %H:%M:%S", origin = origin, tz = "UTC"))
      ###............................................................................
      ## step 1.12b ----
      ## special case: former files with different columns
      ## set colnames
      ###............................................................................
      ## 'Save data computed using the found time constant
      ## DataTable (DynaData,true,-1)
      ## CardOut (0 ,-1)
      ## Sample (4,td(),FP2)
      ## Sample (4,VHC(),FP2)
      ## EndTable
      ###............................................................................
      
      colnames(dada.dyna) <- c(
        "UTC", "RECORD",
        "td_cen_95", "td_cen_45", "td_rim_99", "td_rim_16",
        "Cv_cen_95", "Cv_cen_45", "Cv_rim_99", "Cv_rim_16"
      )
      
      ###............................................................................
      ## step 1.12b ----
      ## add additional columns to former dataset
      ###............................................................................
      
      
      dada.dyna <- dada.dyna[, c(
        "UTC",
        "td_cen_45", "td_cen_95", "td_rim_16", "td_rim_99",
        "Cv_cen_45", "Cv_cen_95", "Cv_rim_16", "Cv_rim_99"
      )]
      
      ###............................................................................
      ## step 1.14b ----
      ## merge input data with date table
      ###............................................................................
      
      newdf.dyna <- merge(compl.dyna, dada.dyna, all.x = T, by = "UTC")
      
      ###............................................................................
      ## step 1.15b ----
      ## merge date table with storing table
      ###............................................................................
      
      for (k in 2:ncol(dada.dyna)) {
        db.sasoil.dyna[, k] <- rowMeans(cbind(db.sasoil.dyna[, k], newdf.dyna[, k + 1]), na.rm = T) #
      }
    }
  } # datadyna
  
  ###............................................................................soil moisture
  ## step 1.16 -----
  ## convert numeric dates back to date format
  ###............................................................................
  
  
  db.sasoil.t[, 1]    <- format(as.POSIXct(db.sasoil.t[, 1], origin = origin, tz = "UTC"), format = "%Y-%m-%d %H:%M")
  db.sasoil.tdr[, 1]  <- format(as.POSIXct(db.sasoil.tdr[, 1], origin = origin, tz = "UTC"), format = "%Y-%m-%d %H:%M")
  db.sasoil.cold[, 1] <- format(as.POSIXct(db.sasoil.cold[, 1], origin = origin, tz = "UTC"), format = "%Y-%m-%d %H:%M")
  db.sasoil.warm[, 1] <- format(as.POSIXct(db.sasoil.warm[, 1], origin = origin, tz = "UTC"), format = "%Y-%m-%d %H:%M")
  db.sasoil.dyna[, 1] <- format(as.POSIXct(db.sasoil.dyna[, 1], origin = origin, tz = "UTC"), format = "%Y-%m-%d %H:%M")
  
  ###............................................................................
  ## step 1.17 ----
  ## set "sparc" colnames
  ###............................................................................
  #
  colnames(db.sasoil.t) <- c(
    "UTC", "Tpan_CR3000", "Ubat",
    "Tair_70",
    "Ts_cen_5", "Ts_cen_15", "Ts_cen_28", "Ts_cen_45",
    "Ts_cen_52", "Ts_cen_72", "Ts_cen_90", "Ts_cen_100",
    "Ts_rim_1", "Ts_rim_7", "Ts_rim_22", "Ts_rim_38",
    "Ts_rim_52", "Ts_rim_69", "Ts_rim_95", "Ts_rim_104",
    
    "SwIn_cen", "SwIn_rim",
    "SwOut_cen", "SwOut_rim",
    "SwIn_cen_mV", "SwIn_rim_mV",
    "SwOut_cen_mV", "SwOut_rim_mV",
    
    "LwIn_cen", "LwIn_rim",
    "LwOut_cen", "LwOut_rim",
    "LwIn_cen_raw", "LwIn_rim_raw",
    "LwOut_cen_raw", "LwOut_rim_raw",
    "LwIn_cen_mV", "LwIn_rim_mV",
    "LwOut_cen_mV", "LwOut_rim_mV",
    
    "Tsen_NR01_cen", "Tsen_NR01_rim",
    
    "G_cen_13", "G_cen_95", "G_rim_13", "G_rim_98",
    
    "Dsn", "distcor", "distraw", "SQsn"
  )
  db.sasoil.tdr <- as.data.frame(db.sasoil.tdr)
  colnames(db.sasoil.tdr) <- c(
    "UTC", "WL_cen_1", "WL_cen_2", "WL_rim_1", "E2_sn_5", "E2_sn_20",
    "E2_cen_v_16", "E2_cen_14", "E2_cen_44", "E2_cen_72", "E2_cen_95",
    "E2_rim_v_22", "E2_rim_9", "E2_rim_24", "E2_rim_38", "E2_rim_52", "E2_rim_100",
    
    "cond_sn_5", "cond_sn_20",
    "cond_cen_v_16", "cond_cen_14", "cond_cen_44", "cond_cen_72", "cond_cen_95",
    "cond_rim_v_22", "cond_rim_9", "cond_rim_24", "cond_rim_38", "cond_rim_52", "cond_rim_100",
    
    "vwc_sn_5", "vwc_sn_20",
    "vwc_cen_v_16", "vwc_cen_14", "vwc_cen_44", "vwc_cen_72", "vwc_cen_95",
    "vwc_rim_v_22", "vwc_rim_9", "vwc_rim_24", "vwc_rim_38", "vwc_rim_52", "vwc_rim_100"
  )
  for (i in 2:42) {
    db.sasoil.tdr[, i] <- as.numeric(as.character(db.sasoil.tdr[, i]))
  }
  colnames(db.sasoil.cold) <- c(
    "UTC",
    "Usen_w_cen_45", "Usen_w_cen_95", "Usen_w_rim_16", "Usen_w_rim_99",
    "Usen_h_cen_45", "Usen_h_cen_95", "Usen_h_rim_16", "Usen_h_rim_99"
  )
  colnames(db.sasoil.warm) <- c(
    "UTC",
    "Usen_c_cen_45", "Usen_c_cen_95", "Usen_c_rim_16", "Usen_c_rim_99",
    "Usen_amp_cen_45", "Usen_amp_cen_95", "Usen_amp_rim_16", "Usen_amp_rim_99",
    "tcond_cen_45", "tcond_cen_95", "tcond_rim_16", "tcond_rim_99"
  )
  colnames(db.sasoil.dyna) <- c(
    "UTC",
    "td_cen_45", "td_cen_95", "td_rim_16", "td_rim_99",
    "Cv_cen_45", "Cv_cen_95", "Cv_rim_16", "Cv_rim_99"
  )
  
  ###............................................................................
  ###............................................................................
  ##
  ##  calculation of snow height and WL ----
  ##
  db.sasoil.t <- as.data.frame(db.sasoil.t)
  dsn.corr    <- read.table(paste0(p.1$w[p.1$n == "settings.p"], "DSNcorr.files/SaSoil2012_DSN_correction.dat"), sep = ",", dec = ".", header = T, fill = TRUE, na = "NAN")
  
  # for (i in 1) {  
    spring.corr <- as.numeric(c(dsn.corr$FValue[which(dsn.corr$YEAR==(year_i-1))])) # the old from last year_i
    autum.corr  <- as.numeric(c(dsn.corr$FValue[which(dsn.corr$YEAR==(year_i))]))   # the new one based on maximum dist in august
    WL.corr     <- as.numeric(c(dsn.corr$WL_cen_2[which(dsn.corr$YEAR==(year_i))])) # the new one based on maximum dist in august
    aa1 <- which(db.sasoil.t[, 1] == c(dsn.corr$LDaySnow[which(dsn.corr$YEAR==(year_i))]))
    aa2 <- which(db.sasoil.t[, 1] == c(dsn.corr$FDaySnow[which(dsn.corr$YEAR==(year_i))]))
    aa3 <- which(db.sasoil.t[, 1] == paste0(year_i,"-12-31 23:30"))
    
    # Dsn
    db.sasoil.t$Dsn[  1:(aa1 - 1)] <- spring.corr - as.numeric(db.sasoil.t$distcor[1:(aa1 - 1)]) # spring (winter last year)
    db.sasoil.t$Dsn[aa1:(aa2 - 1)] <- as.numeric(db.sasoil.t$distcor[aa1:(aa2 - 1)]) - as.numeric(db.sasoil.t$distcor[aa1:(aa2 - 1)]) # summer
    db.sasoil.t$Dsn[aa2:(aa3    )] <- autum.corr - as.numeric(db.sasoil.t$distcor[aa2:aa3]) # winter
    # WL
    db.sasoil.tdr$WL_cen_1[aa1:(aa2 - 1)] <- (3.4408 * db.sasoil.tdr$E2_cen_v_16[aa1:(aa2 - 1)]^(0.5) - 16) / 100
    db.sasoil.tdr$WL_rim_1[aa1:(aa2 - 1)] <- (3.4408 * db.sasoil.tdr$E2_rim_v_22[aa1:(aa2 - 1)]^(0.5) - 22) / 100
    db.sasoil.tdr$WL_cen_2[aa1:(aa2 - 1)] <- WL.corr - as.numeric(db.sasoil.t$distcor[aa1:(aa2 - 1)])
    
    if(dsn.corr$ex2[which(dsn.corr$YEAR==(year_i))]!=0){
      # special case if sensor is newly installed at a different heigth
      aa4     <- which(db.sasoil.t[, 1] == c(dsn.corr$ex1[which(dsn.corr$YEAR==(year_i))]))
      ex.corr <- as.numeric(c(dsn.corr$ex2[which(dsn.corr$YEAR==(year_i))]))

      db.sasoil.tdr$WL_cen_2[aa1:(aa4 - 1)] <- WL.corr - as.numeric(db.sasoil.t$distcor[aa1:(aa4 - 1)])
      db.sasoil.tdr$WL_cen_2[aa4:(aa2 - 1)] <- ex.corr - as.numeric(db.sasoil.t$distcor[aa4:(aa2 - 1)])
    }
    
    
  
  
  #
  # replace Nan with NA
  for (m1 in 2:50) {
    db.sasoil.t[is.nan(as.numeric(db.sasoil.t[, m1])), m1] <- NA
  }
  for (m2 in 2:42) {
    db.sasoil.tdr[is.nan(as.numeric(db.sasoil.tdr[, m2])), m2] <- NA
  }
  for (m3 in 2:9) {
    db.sasoil.cold[is.nan(as.numeric(db.sasoil.cold[, m3])), m3] <- NA
  }
  for (m4 in 2:13) {
    db.sasoil.warm[is.nan(as.numeric(db.sasoil.warm[, m4])), m4] <- NA
  }
  for (m5 in 2:9) {
    db.sasoil.dyna[is.nan(as.numeric(db.sasoil.dyna[, m5])), m5] <- NA
  }
  
  ###............................................................................
  ## step 1.18 safe data to txt-file ----
  ## 
  ###............................................................................
  #
  total <- cbind(db.sasoil.t, db.sasoil.tdr[, -1], db.sasoil.cold[, -1], db.sasoil.warm[, -1], db.sasoil.dyna[, -1])
  
  write.table(total,
              paste0(p.1$w[p.1$n == "LV0.p"], "SaSoil2012/00_full_dataset/SaSoil2012_", year_i, "_lv0.dat"),
              quote = F, dec = ".", sep = ",", row.names = F
  )
  
  write.table(db.sasoil.t,
              paste0(p.1$w[p.1$n == "LV0.p"], "SaSoil2012/01_soiltemperature/SaSoil2012_ts_", year_i, "_lv0.dat"),
              quote = F, dec = ".", sep = ",", row.names = F
  )
  write.table(db.sasoil.tdr,
              paste0(p.1$w[p.1$n == "LV0.p"], "SaSoil2012/02_Data_TDR/SaSoil2012_tdr_", year_i, "_lv0.dat"),
              quote = F, dec = ".", sep = ",", row.names = F
  )
  write.table(db.sasoil.cold,
              paste0(p.1$w[p.1$n == "LV0.p"], "SaSoil2012/06_Data_Cold/SaSoil2012_cold_", year_i, "_lv0.dat"),
              quote = F, dec = ".", sep = ",", row.names = F
  )
  write.table(db.sasoil.warm,
              paste0(p.1$w[p.1$n == "LV0.p"], "SaSoil2012/07_Data_Warm/SaSoil2012_warm_", year_i, "_lv0.dat"),
              quote = F, dec = ".", sep = ",", row.names = F
  )
  write.table(db.sasoil.dyna,
              paste0(p.1$w[p.1$n == "LV0.p"], "SaSoil2012/08_Data_Dyna/SaSoil2012_dyna_", year_i, "_lv0.dat"),
              quote = F, dec = ".", sep = ",", row.names = F
  )
  
  cat("\n#\n# sasoil2012 ", year_i, " without problems!\n#\n") # main output
} # end loop over years
