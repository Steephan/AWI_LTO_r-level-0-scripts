###............................................................................
##
##   RAW to Level0 ------------------------------------
##   SaSoil2002 (and 2010)
##   equal time steps, no gaps
##
##   by: Stephan.Lange@awi.de, Niko.Bornemann@awi.de, Peter.Schreiber@awi.de
##   last modified:
##   2021-05-06 SL adapted to refresh app
##   2021-03-25 SL git path 
##   2020-10-30 CL adapted to script guidelines and implementation in Samoylov_MAIN.R
##   2019-05-22 PSc (changed file paths)
##
###............................................................................
##
###............................................................................
##
##   
##   - reorder all columns surface downwards
##   - create full dataset for next lvl
##   - calculate E2 with extra calib-offset
##
##
##
##
##     no cond values before 2010
##     still no temp correction for Dsn !!!!!
##
###............................................................................
## step 0.01 set path settings for different systems linux vs. windoof ----
## 
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
## step 0.02 set running options years, ... ----
## 
###............................................................................
# to run this script separately, you have to set runyear:
#
# origin <- "1970-01-01"
# recent.year <- as.numeric(format(Sys.Date(), "%Y"))
# runyear <- 2018

###............................................................................
## step 0.03 loop 1 over years ----
##
###............................................................................
for (year_i in runyear) {
  ###............................................................................
  ## step 0.04 set 2 empty tables with length of year_i ----
  ##
  ## columns: 2 (date table) and number of input table (storing table)
  ###............................................................................
  # cat("\nProcessing year", year_i, "\n====================\n\n")
  start.date <- as.POSIXct(paste(year_i, "-01-01 00:00:00", sep = ""), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  end.date <- as.POSIXct(paste(year_i, "-", 12, "-", 31, " 23:00:00", sep = ""), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  # create empty data frame with UTC time stamp every 30 min
  db.sasoil.t <- matrix(ncol = 71, nrow = length(seq(start.date, end.date, by = "hour")), -999)
  db.sasoil.t[, c(2:71)] <- NA
  db.sasoil.tdr <- matrix(ncol = 65, nrow = length(seq(start.date, end.date, by = "hour")), -999)
  db.sasoil.tdr[, c(2:65)] <- NA
  compl.t <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "hour")))
  compl.tdr <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "hour")))
  db.sasoil.t[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "hour"), format = "%Y-%m-%d %H:%M:%S"))
  db.sasoil.tdr[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "hour"), format = "%Y-%m-%d %H:%M:%S"))
  
  compl.t[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "hour"), format = "%Y-%m-%d %H:%M:%S"))
  compl.tdr[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "hour"), format = "%Y-%m-%d %H:%M:%S"))
  colnames(compl.t) <- c("UTC", "erste")
  colnames(compl.tdr) <- c("UTC", "erste")
  
  ###............................................................................
  ##
  ##  step 0.05 b) Airtemperature for Snowdepth correction ----
  
  clima <- read.table(paste0(p.1$w[p.1$n == "LV1.p"], "SaMet2002/00_full_dataset/SaMet2002_", year_i, "_lv1.dat"), sep = ",", dec = ".", header = T, fill = TRUE, na = "NA")[seq(1, 2 * length(compl.tdr[, 1]), 2), 1:13]
  
  ##
  ##
  ###............................................................................
  
  
  ###............................................................................
  ## step 0.06 set input.path and list all files ----
  ##
  ###............................................................................
  
  
  inz.01.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaSoil2002/01_Temp_v2/")
  files.01 <- list.files(inz.01.path, pattern = "*.dat")
  bui <- matrix(nrow = length(files.01), ncol = 2, 1)
  for (i in 1:length(files.01)) {
    bui[i, 2] <- as.numeric(substr(files.01[i], 1, 4))
    bui[i, 1] <- as.numeric(substr(files.01[i], 14, 17))
  }
  sel.01 <- which(bui[, 1] <= year_i & bui[, 2] >= year_i)
  inz.02.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaSoil2002/02_Data_TDR/01_joined_dataset/")
  files.02 <- list.files(inz.02.path, pattern = "*.dat")
  hui <- matrix(nrow = length(files.02), ncol = 2, 1)
  for (i in 1:length(files.02)) {
    hui[i, 2] <- as.numeric(substr(files.02[i], 1, 4))
    hui[i, 1] <- as.numeric(substr(files.02[i], 14, 17))
  }
  sel.02 <- which(hui[, 1] <= year_i & hui[, 2] >= year_i)
  ###............................................................................
  ## step 1.01 loop 2 over all soil temperature files -----
  ##
  ###............................................................................
  
  for (i in (1:length(files.01))[sel.01]) { # soil temperature 1:length(files.01)
    ###............................................................................
    ## step 1.02 read one file (skip headers, set NA-values) ----
    ## 
    ## set temporal colnames
    ###............................................................................
    # cat("processing: ", files.01[i], "\n")
    dada.t <- read.table(paste(inz.01.path, files.01[i], sep = ""), sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")
    
    colnames(dada.t) <- paste0("V", seq_len(ncol(dada.t)))
    ###............................................................................
    ## step 1.03 check file if dates are in running year_i of loop 1 -----
    ##
    ###............................................................................
    
    if (as.numeric(substr(lapply(dada.t[1, 1], as.character), 1, 4)) > year_i || as.numeric(substr(lapply(dada.t[length(dada.t[, 1]), 1], as.character), 1, 4)) < year_i) {
      next
    } # skip file if wrong year
    # cat(paste(dada.t[1,1],"     to     ",dada.t[length(dada.t[,1]),1],"    ",files.01[i]))
    ###............................................................................
    ## step 1.04 check file for double entries ----
    ##
    ###............................................................................
    dada.t <- check.double.entry(dada.t)
    ###............................................................................
    ## step 1.05 convert date to numeric value ----
    ##
    ###............................................................................
    
    dada.t[, 1] <- as.numeric(as.POSIXct(dada.t[, 1], format = "%Y-%m-%d %H:%M:%S", origin = origin, tz = "UTC"))
    ###............................................................................
    ## step 1.06 a) special case: former files with different columns ----
    ##
    ## set colnames
    ###............................................................................
    if (ncol(dada.t) == 36) { # special case for 2002-2010-file
      colnames(dada.t) <- c(
        "UTC", "Tpan", "G_rim", "G_center",
        "Ts_center_40", "Ts_center_30", "Ts_center20", "Ts_center_10", "Ts_center_5", "Ts_center_1",
        "Ts_slope_42", "Ts_slope_32", "Ts_slope_22", "Ts_slope_16", "Ts_slope_7", "Ts_slope_3",
        "Ts_rim_71", "Ts_rim_61", "Ts_rim_51", "Ts_rim_38", "Ts_rim_33",
        "Ts_rim_27", "Ts_rim_21", "Ts_rim_16", "Ts_rim_11", "Ts_rim_6", "Ts_rim_2",
        "Ts_icew_271", "Ts_icew_241", "Ts_icew_211", "Ts_icew_181", "Ts_icew_151",
        "Ts_icew_121", "Ts_icew_91", "Ts_icew_61", "Ts_icew_41"
      )
      dada.t[c(
        "Ubat",
        "U_center_40", "U_center_30", "U_center20", "U_center_10", "U_center_5", "U_center_1",
        "U_slope_42", "U_slope_32", "U_slope_22", "U_slope_16", "U_slope_7", "U_slope_3",
        "U_rim_71", "U_rim_61", "U_rim_51", "U_rim_38", "U_rim_33",
        "U_rim_27", "U_rim_21", "U_rim_16", "U_rim_11", "U_rim_6", "U_rim_2",
        "U_icew_271", "U_icew_241", "U_icew_211", "U_icew_181", "U_icew_151",
        "U_icew_121", "U_icew_91", "U_icew_61", "U_icew_41",
        "Tsurf_raw", "Tsurf_cor"
      )] <- NA
      # reorder
      dada.t <- dada.t[c(
        "UTC", "Ubat", "Tpan", "G_rim", "G_center",
        "U_center_40", "U_center_30", "U_center20", "U_center_10", "U_center_5", "U_center_1",
        "U_slope_42", "U_slope_32", "U_slope_22", "U_slope_16", "U_slope_7", "U_slope_3",
        "U_rim_71", "U_rim_61", "U_rim_51", "U_rim_38", "U_rim_33",
        "U_rim_27", "U_rim_21", "U_rim_16", "U_rim_11", "U_rim_6", "U_rim_2",
        "U_icew_271", "U_icew_241", "U_icew_211", "U_icew_181", "U_icew_151",
        "U_icew_121", "U_icew_91", "U_icew_61", "U_icew_41",
        
        "Ts_center_40", "Ts_center_30", "Ts_center20", "Ts_center_10", "Ts_center_5", "Ts_center_1",
        "Ts_slope_42", "Ts_slope_32", "Ts_slope_22", "Ts_slope_16", "Ts_slope_7", "Ts_slope_3",
        "Ts_rim_71", "Ts_rim_61", "Ts_rim_51", "Ts_rim_38", "Ts_rim_33",
        "Ts_rim_27", "Ts_rim_21", "Ts_rim_16", "Ts_rim_11", "Ts_rim_6", "Ts_rim_2",
        "Ts_icew_271", "Ts_icew_241", "Ts_icew_211", "Ts_icew_181", "Ts_icew_151",
        "Ts_icew_121", "Ts_icew_91", "Ts_icew_61", "Ts_icew_41",
        
        "Tsurf_raw", "Tsurf_cor"
      )]
    } else {
      colnames(dada.t) <- c(
        "UTC", "number", "Ubat", "Tpan", "G_rim", "G_center",
        "U_center_40", "U_center_30", "U_center20", "U_center_10", "U_center_5", "U_center_1",
        "U_slope_42", "U_slope_32", "U_slope_22", "U_slope_16", "U_slope_7", "U_slope_3",
        "U_rim_71", "U_rim_61", "U_rim_51", "U_rim_38", "U_rim_33",
        "U_rim_27", "U_rim_21", "U_rim_16", "U_rim_11", "U_rim_6", "U_rim_2",
        "U_icew_271", "U_icew_241", "U_icew_211", "U_icew_181", "U_icew_151",
        "U_icew_121", "U_icew_91", "U_icew_61", "U_icew_41",
        
        "Ts_center_40", "Ts_center_30", "Ts_center20", "Ts_center_10", "Ts_center_5", "Ts_center_1",
        "Ts_slope_42", "Ts_slope_32", "Ts_slope_22", "Ts_slope_16", "Ts_slope_7", "Ts_slope_3",
        "Ts_rim_71", "Ts_rim_61", "Ts_rim_51", "Ts_rim_38", "Ts_rim_33",
        "Ts_rim_27", "Ts_rim_21", "Ts_rim_16", "Ts_rim_11", "Ts_rim_6", "Ts_rim_2",
        "Ts_icew_271", "Ts_icew_241", "Ts_icew_211", "Ts_icew_181", "Ts_icew_151",
        "Ts_icew_121", "Ts_icew_91", "Ts_icew_61", "Ts_icew_41",
        
        "Tgs", "Tsurf_cor"
      )
      dada.t$G_rim <- dada.t$G_rim * (1000 / 60.4)
      dada.t$G_center <- dada.t$G_center * (1000 / 61.9)
      dada.t <- dada.t[, -2]
    }
    
    newdf.t <- merge(compl.t, dada.t, all.x = T, by = "UTC")
    
    ###............................................................................
    ## step 1.07 merge date table with storing table ----
    ##
    ###............................................................................
    
    for (k in 2:(ncol(db.sasoil.t))) {
      db.sasoil.t[, k] <- rowMeans(cbind(db.sasoil.t[, k], newdf.t[, k + 1]), na.rm = T) #
    }
  } # soil temperature
  ###............................................................................
  ## step 1.01 b) loop 3 over all soil moisture files -----
  ##
  ###............................................................................
  # if(year_i<2015){files.02<-files.03;inz.02.path<-inz.03.path;}
  for (i in (1:length(files.02))[sel.02]) { # soil moisture
    ###............................................................................
    ## step 1.02 b) read one file (skip headers, set NA-values) ----
    ##
    ## set temporal colnames
    ###............................................................................
    #cat("processing: ", files.02[i], "\n")
    dada.tdr <- read.table(paste(inz.02.path, files.02[i], sep = ""), sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")
    
    colnames(dada.tdr) <- paste0("V", seq_len(ncol(dada.tdr)))
    ###............................................................................
    ## step 1.03 b) check file if dates are in running year_i of loop 1 ----
    ##
    ###............................................................................
    
    if (as.numeric(substr(lapply(dada.tdr[1, 1], as.character), 1, 4)) > year_i || as.numeric(substr(lapply(dada.tdr[length(dada.tdr[, 1]), 1], as.character), 1, 4)) < year_i) {
      next
    } # skip file if wrong year
    # cat(paste(dada.tdr[1,1],"     to     ",dada.t[length(dada.tdr[,1]),1],"    ",files.02[i]))
    ###............................................................................
    ## step 1.04 b) check file for double entries ----
    ##
    ###............................................................................
    dada.tdr <- check.double.entry(dada.tdr)
    ###............................................................................
    ## step 1.05 b) convert date to numeric value ----
    ##
    ###............................................................................
    
    dada.tdr[, 1] <- as.numeric(as.POSIXct(dada.tdr[, 1], format = "%Y-%m-%d %H:%M:%S", origin = origin, tz = "UTC"))
    ###............................................................................
    ## step 1.06 b) special case: former files with different columns ----
    ##
    ## set colnames
    ###............................................................................
    if (ncol(dada.tdr) == 42) { ### all files before 2010
      colnames(dada.tdr) <- c(
        "UTC", "Dsn",
        
        "E2_center_43", "E2_center_33", "E2_center_23", "E2_center_13", "E2_center_8",
        "E2_slope_43", "E2_slope_33", "E2_slope_23", "E2_slope_14", "E2_slope_5",
        "E2_rim_70", "E2_rim_60", "E2_rim_50", "E2_rim_37", "E2_rim_34",
        "E2_rim_26", "E2_rim_22", "E2_rim_15", "E2_rim_12", "E2_rim_5",
        
        "vwc_center_43", "vwc_center_33", "vwc_center_23", "vwc_center_13", "vwc_center_8",
        "vwc_slope_43", "vwc_slope_33", "vwc_slope_23", "vwc_slope_14", "vwc_slope_5",
        "vwc_rim_70", "vwc_rim_60", "vwc_rim_50", "vwc_rim_37", "vwc_rim_34",
        "vwc_rim_26", "vwc_rim_22", "vwc_rim_15", "vwc_rim_12", "vwc_rim_5"
      )
      dada.tdr[c(
        "distraw", "snowsq", "distcor",
        "cond_center_43", "cond_center_33", "cond_center_23", "cond_center_13", "cond_center_8",
        "cond_slope_43", "cond_slope_33", "cond_slope_23", "cond_slope_14", "cond_slope_5",
        "cond_rim_70", "cond_rim_60", "cond_rim_50", "cond_rim_37", "cond_rim_34",
        "cond_rim_26", "cond_rim_22", "cond_rim_15", "cond_rim_12", "cond_rim_5"
      )] <- NA
      
      dada.tdr <- dada.tdr[, c(
        "UTC", "distraw", "snowsq", "distcor", "Dsn",
        
        "E2_center_43", "E2_center_33", "E2_center_23", "E2_center_13", "E2_center_8",
        "E2_slope_43", "E2_slope_33", "E2_slope_23", "E2_slope_14", "E2_slope_5",
        "E2_rim_70", "E2_rim_60", "E2_rim_50", "E2_rim_37", "E2_rim_34",
        "E2_rim_26", "E2_rim_22", "E2_rim_15", "E2_rim_12", "E2_rim_5",
        
        "cond_center_43", "cond_center_33", "cond_center_23", "cond_center_13", "cond_center_8",
        "cond_slope_43", "cond_slope_33", "cond_slope_23", "cond_slope_14", "cond_slope_5",
        "cond_rim_70", "cond_rim_60", "cond_rim_50", "cond_rim_37", "cond_rim_34",
        "cond_rim_26", "cond_rim_22", "cond_rim_15", "cond_rim_12", "cond_rim_5",
        
        "vwc_center_43", "vwc_center_33", "vwc_center_23", "vwc_center_13", "vwc_center_8",
        "vwc_slope_43", "vwc_slope_33", "vwc_slope_23", "vwc_slope_14", "vwc_slope_5",
        "vwc_rim_70", "vwc_rim_60", "vwc_rim_50", "vwc_rim_37", "vwc_rim_34",
        "vwc_rim_26", "vwc_rim_22", "vwc_rim_15", "vwc_rim_12", "vwc_rim_5"
      )]
      
      
      # if(length(which(dada.tdr$E2_rim_70>50))>=1){
      #   ## Error in database output-file: some values of E2_rim_70 lost 1 zero-digit ... for example 6->0.6
      #   ##
      #   ## this and also the ( 0.3 * 10 ) ^ 2 = 9
      #
      #  temp1 <-(dada.tdr$E2_rim_70[which(dada.tdr$E2_rim_70>50)]-9)/10
      #
      #  dada.tdr$E2_rim_70[which(dada.tdr$E2_rim_70>50)]<-temp1
      #
      # }
      
      # plot(db.sasoil.tdr$E2_rim_70[2000:3000])
      ###............................................................................
      ## step 1.07 b) add additional columns to former dataset ----
      ##
      ###............................................................................     
    } else if (ncol(dada.tdr) == 64) { ### all files before 2015
      colnames(dada.tdr) <- c(
        "UTC", "number", "distraw", "Dsn",
        
        "E2_center_43", "E2_center_33", "E2_center_23", "E2_center_13", "E2_center_8",
        "E2_slope_43", "E2_slope_33", "E2_slope_23", "E2_slope_14", "E2_slope_5",
        "E2_rim_70", "E2_rim_60", "E2_rim_50", "E2_rim_37", "E2_rim_34",
        "E2_rim_26", "E2_rim_22", "E2_rim_15", "E2_rim_12", "E2_rim_5",
        
        "cond_center_43", "cond_center_33", "cond_center_23", "cond_center_13", "cond_center_8",
        "cond_slope_43", "cond_slope_33", "cond_slope_23", "cond_slope_14", "cond_slope_5",
        "cond_rim_70", "cond_rim_60", "cond_rim_50", "cond_rim_37", "cond_rim_34",
        "cond_rim_26", "cond_rim_22", "cond_rim_15", "cond_rim_12", "cond_rim_5",
        
        "vwc_center_43", "vwc_center_33", "vwc_center_23", "vwc_center_13", "vwc_center_8",
        "vwc_slope_43", "vwc_slope_33", "vwc_slope_23", "vwc_slope_14", "vwc_slope_5",
        "vwc_rim_70", "vwc_rim_60", "vwc_rim_50", "vwc_rim_37", "vwc_rim_34",
        "vwc_rim_26", "vwc_rim_22", "vwc_rim_15", "vwc_rim_12", "vwc_rim_5"
      )
      ###............................................................................
      ## step 1.08 b) convert la/l to E2 by square ----
      ##
      ###............................................................................
      dada.tdr$snowsq <- NA
      dada.tdr$distcor <- NA
      dada.tdr$E2_center_43 <- (dada.tdr$E2_center_43 - 0.3)^2
      dada.tdr$E2_center_33 <- (dada.tdr$E2_center_33 - 0.3)^2
      dada.tdr$E2_center_23 <- (dada.tdr$E2_center_23 - 0.3)^2
      dada.tdr$E2_center_13 <- (dada.tdr$E2_center_13 - 0.3)^2
      dada.tdr$E2_center_8 <- (dada.tdr$E2_center_8 - 0.3)^2
      dada.tdr$E2_slope_43 <- (dada.tdr$E2_slope_43 - 0.3)^2
      dada.tdr$E2_slope_33 <- (dada.tdr$E2_slope_33 - 0.3)^2
      dada.tdr$E2_slope_23 <- (dada.tdr$E2_slope_23 - 0.3)^2
      dada.tdr$E2_slope_14 <- (dada.tdr$E2_slope_14 - 0.3)^2
      dada.tdr$E2_slope_5 <- (dada.tdr$E2_slope_5 - 0.3)^2
      dada.tdr$E2_rim_70 <- (dada.tdr$E2_rim_70 - 0.3)^2
      dada.tdr$E2_rim_60 <- (dada.tdr$E2_rim_60 - 0.3)^2
      dada.tdr$E2_rim_50 <- (dada.tdr$E2_rim_50 - 0.3)^2
      dada.tdr$E2_rim_37 <- (dada.tdr$E2_rim_37 - 0.3)^2
      dada.tdr$E2_rim_34 <- (dada.tdr$E2_rim_34 - 0.3)^2
      dada.tdr$E2_rim_26 <- (dada.tdr$E2_rim_26 - 0.3)^2
      dada.tdr$E2_rim_22 <- (dada.tdr$E2_rim_22 - 0.3)^2
      dada.tdr$E2_rim_15 <- (dada.tdr$E2_rim_15 - 0.3)^2
      dada.tdr$E2_rim_12 <- (dada.tdr$E2_rim_12 - 0.3)^2
      dada.tdr$E2_rim_5 <- (dada.tdr$E2_rim_5 - 0.3)^2 #
      

      dada.tdr <- dada.tdr[, c(
        "UTC", "distraw", "snowsq", "distcor", "Dsn",
        
        "E2_center_43", "E2_center_33", "E2_center_23", "E2_center_13", "E2_center_8",
        "E2_slope_43", "E2_slope_33", "E2_slope_23", "E2_slope_14", "E2_slope_5",
        "E2_rim_70", "E2_rim_60", "E2_rim_50", "E2_rim_37", "E2_rim_34",
        "E2_rim_26", "E2_rim_22", "E2_rim_15", "E2_rim_12", "E2_rim_5",
        
        "cond_center_43", "cond_center_33", "cond_center_23", "cond_center_13", "cond_center_8",
        "cond_slope_43", "cond_slope_33", "cond_slope_23", "cond_slope_14", "cond_slope_5",
        "cond_rim_70", "cond_rim_60", "cond_rim_50", "cond_rim_37", "cond_rim_34",
        "cond_rim_26", "cond_rim_22", "cond_rim_15", "cond_rim_12", "cond_rim_5",
        
        "vwc_center_43", "vwc_center_33", "vwc_center_23", "vwc_center_13", "vwc_center_8",
        "vwc_slope_43", "vwc_slope_33", "vwc_slope_23", "vwc_slope_14", "vwc_slope_5",
        "vwc_rim_70", "vwc_rim_60", "vwc_rim_50", "vwc_rim_37", "vwc_rim_34",
        "vwc_rim_26", "vwc_rim_22", "vwc_rim_15", "vwc_rim_12", "vwc_rim_5"
      )]
    } else { #### after 2015
      ###............................................................................
      ## step 1.06 c) standard case after 2015 ----
      ##
      ## set original colnames
      ###............................................................................
      
      colnames(dada.tdr) <- c(
        "UTC", "record", "distraw", "snowsq", "distcor", "Dsn",
        
        "E2_center_43", "E2_center_33", "E2_center_23", "E2_center_13", "E2_center_8",
        "E2_slope_43", "E2_slope_33", "E2_slope_23", "E2_slope_14", "E2_slope_5",
        "E2_rim_70", "E2_rim_60", "E2_rim_50", "E2_rim_37", "E2_rim_34",
        "E2_rim_26", "E2_rim_22", "E2_rim_15", "E2_rim_12", "E2_rim_5",
        
        "cond_center_43", "cond_center_33", "cond_center_23", "cond_center_13", "cond_center_8",
        "cond_slope_43", "cond_slope_33", "cond_slope_23", "cond_slope_14", "cond_slope_5",
        "cond_rim_70", "cond_rim_60", "cond_rim_50", "cond_rim_37", "cond_rim_34",
        "cond_rim_26", "cond_rim_22", "cond_rim_15", "cond_rim_12", "cond_rim_5",
        
        "vwc_center_43", "vwc_center_33", "vwc_center_23", "vwc_center_13", "vwc_center_8",
        "vwc_slope_43", "vwc_slope_33", "vwc_slope_23", "vwc_slope_14", "vwc_slope_5",
        "vwc_rim_70", "vwc_rim_60", "vwc_rim_50", "vwc_rim_37", "vwc_rim_34",
        "vwc_rim_26", "vwc_rim_22", "vwc_rim_15", "vwc_rim_12", "vwc_rim_5"
      )
      dada.tdr <- dada.tdr[, -2]
      dada.tdr$E2_center_43 <- (dada.tdr$E2_center_43 - 0.3)^2
      dada.tdr$E2_center_33 <- (dada.tdr$E2_center_33 - 0.3)^2
      dada.tdr$E2_center_23 <- (dada.tdr$E2_center_23 - 0.3)^2
      dada.tdr$E2_center_13 <- (dada.tdr$E2_center_13 - 0.3)^2
      dada.tdr$E2_center_8 <- (dada.tdr$E2_center_8 - 0.3)^2
      dada.tdr$E2_slope_43 <- (dada.tdr$E2_slope_43 - 0.3)^2
      dada.tdr$E2_slope_33 <- (dada.tdr$E2_slope_33 - 0.3)^2
      dada.tdr$E2_slope_23 <- (dada.tdr$E2_slope_23 - 0.3)^2
      dada.tdr$E2_slope_14 <- (dada.tdr$E2_slope_14 - 0.3)^2
      dada.tdr$E2_slope_5 <- (dada.tdr$E2_slope_5 - 0.3)^2
      dada.tdr$E2_rim_70 <- (dada.tdr$E2_rim_70 - 0.3)^2
      dada.tdr$E2_rim_60 <- (dada.tdr$E2_rim_60 - 0.3)^2
      dada.tdr$E2_rim_50 <- (dada.tdr$E2_rim_50 - 0.3)^2
      dada.tdr$E2_rim_37 <- (dada.tdr$E2_rim_37 - 0.3)^2
      dada.tdr$E2_rim_34 <- (dada.tdr$E2_rim_34 - 0.3)^2
      dada.tdr$E2_rim_26 <- (dada.tdr$E2_rim_26 - 0.3)^2
      dada.tdr$E2_rim_22 <- (dada.tdr$E2_rim_22 - 0.3)^2
      dada.tdr$E2_rim_15 <- (dada.tdr$E2_rim_15 - 0.3)^2
      dada.tdr$E2_rim_12 <- (dada.tdr$E2_rim_12 - 0.3)^2
      dada.tdr$E2_rim_5 <- (dada.tdr$E2_rim_5 - 0.3)^2 #
    }
    ###............................................................................
    ## step 1.09 b) merge input data with date table ----
    ##
    ###............................................................................
    
    newdf.tdr <- merge(compl.tdr, dada.tdr, all.x = T, by = "UTC")
    
    ###............................................................................
    ## step 1.10 b) merge date table with storing table ----
    ##
    ###............................................................................
    
    for (k in 2:(length(db.sasoil.tdr[1, ]))) {
      db.sasoil.tdr[, k] <- rowMeans(cbind(db.sasoil.tdr[, k], newdf.tdr[, k + 1]), na.rm = T) #
    }
  } # soil moisture
  
  ###............................................................................
  ## step 1.11  convert numeric dates back to date format-----
  ##
  ###............................................................................
  
  db.sasoil.tdr <- as.data.frame(db.sasoil.tdr)
  db.sasoil.tdr[, 4] <- round(db.sasoil.tdr[, 2] * ((clima$Tair_a_50 + 273.15) / 273.15)^0.5, 3)
  
  if (year_i >= 2016) {
    db.sasoil.tdr[, 5] <- round(1.07 - db.sasoil.tdr[, 4], 3)
  } else if (year_i == 2015) {
    ### 4 phases !!! the systems falls over 2 times, Molo repairs it
    db.sasoil.tdr[1:4399, 5] <- round(1.29 - db.sasoil.tdr[1:4399, 4], 3) ## 03.07.2015
    db.sasoil.tdr[4400:4736, 5] <- round(1.10 - db.sasoil.tdr[4400:4736, 4], 3) ## 17.07.2015
    db.sasoil.tdr[4737:4977, 5] <- round(1.12 - db.sasoil.tdr[4737:4977, 4], 3) ## 27.07.2015
    db.sasoil.tdr[4978:8760, 5] <- round(1.07 - db.sasoil.tdr[4978:8760, 4], 3)
  } else if (year_i == 2014) { #
    db.sasoil.tdr[, 5] <- round(1.27 - db.sasoil.tdr[, 4], 3) ##  14.08.2013
    # db.sasoil.tdr[5468:6477,19]<-db.sasoil.tdr[5468:6477,19]-3 # E2_rim_37 hat spruenge
  } else if (year_i == 2013) {
    db.sasoil.tdr[1:5400, 5] <- round(1.24 - db.sasoil.tdr[1:5400, 4], 3) ##  14.08.2013
    db.sasoil.tdr[5401:8760, 5] <- round(1.25 - db.sasoil.tdr[5401:8760, 4], 3)
    # db.sasoil.tdr[5501:7204,19]<-db.sasoil.tdr[5501:7204,19]-3 # E2_rim_37 hat spruenge
    # browser()
  } else if (year_i == 2012) {
    db.sasoil.tdr[, 5] <- round(1.23 - db.sasoil.tdr[, 4], 3)
  } else if (year_i == 2011) {
    db.sasoil.tdr[1:3300, 5] <- round(1.24 - db.sasoil.tdr[1:3300, 4], 3) ##  20.05.2011
    db.sasoil.tdr[3301:8760, 5] <- round(1.23 - db.sasoil.tdr[3301:8760, 4], 3)
  } else if (year_i == 2010) { # no correction
  } else if (year_i == 2009) {
    # db.sasoil.tdr[5364:5508,19]<-db.sasoil.tdr[5364:5508,19]-3 # E2_rim_37 hat spruenge
  }
  
  # plot(db.sasoil.tdr[,5],ylim=c(-.1,1.5))
  ###............................................................................
  ## step 2.01 set "sparc" colnames ----
  ##
  ###............................................................................
  #
  colnames(db.sasoil.t) <- c(
    "UTC", "Ubat", "Tpan", "G_rim", "G_center",
    "U_center_40", "U_center_30", "U_center20", "U_center_10", "U_center_5", "U_center_1",
    "U_slope_42", "U_slope_32", "U_slope_22", "U_slope_16", "U_slope_7", "U_slope_3",
    "U_rim_71", "U_rim_61", "U_rim_51", "U_rim_38", "U_rim_33",
    "U_rim_27", "U_rim_21", "U_rim_16", "U_rim_11", "U_rim_6", "U_rim_2",
    "U_icew_271", "U_icew_241", "U_icew_211", "U_icew_181", "U_icew_151",
    "U_icew_121", "U_icew_91", "U_icew_61", "U_icew_41",
    
    "Ts_center_40", "Ts_center_30", "Ts_center_20", "Ts_center_10", "Ts_center_5", "Ts_center_1",
    "Ts_slope_42", "Ts_slope_32", "Ts_slope_22", "Ts_slope_16", "Ts_slope_7", "Ts_slope_3",
    "Ts_rim_71", "Ts_rim_61", "Ts_rim_51", "Ts_rim_38", "Ts_rim_33",
    "Ts_rim_27", "Ts_rim_21", "Ts_rim_16", "Ts_rim_11", "Ts_rim_6", "Ts_rim_2",
    "Ts_icew_271", "Ts_icew_241", "Ts_icew_211", "Ts_icew_181", "Ts_icew_151",
    "Ts_icew_121", "Ts_icew_91", "Ts_icew_61", "Ts_icew_41",
    
    "Tgs", "Tsurf_cor"
  )
  
  colnames(db.sasoil.tdr) <- c(
    "UTC", "distraw", "snowsq", "distcor", "Dsn",
    
    "E2_center_43", "E2_center_33", "E2_center_23", "E2_center_13", "E2_center_8",
    "E2_slope_43", "E2_slope_33", "E2_slope_23", "E2_slope_14", "E2_slope_5",
    "E2_rim_70", "E2_rim_60", "E2_rim_50", "E2_rim_37", "E2_rim_34",
    "E2_rim_26", "E2_rim_22", "E2_rim_15", "E2_rim_12", "E2_rim_5",
    
    "cond_center_43", "cond_center_33", "cond_center_23", "cond_center_13", "cond_center_8",
    "cond_slope_43", "cond_slope_33", "cond_slope_23", "cond_slope_14", "cond_slope_5",
    "cond_rim_70", "cond_rim_60", "cond_rim_50", "cond_rim_37", "cond_rim_34",
    "cond_rim_26", "cond_rim_22", "cond_rim_15", "cond_rim_12", "cond_rim_5",
    
    "vwc_center_43", "vwc_center_33", "vwc_center_23", "vwc_center_13", "vwc_center_8",
    "vwc_slope_43", "vwc_slope_33", "vwc_slope_23", "vwc_slope_14", "vwc_slope_5",
    "vwc_rim_70", "vwc_rim_60", "vwc_rim_50", "vwc_rim_37", "vwc_rim_34",
    "vwc_rim_26", "vwc_rim_22", "vwc_rim_15", "vwc_rim_12", "vwc_rim_5"
  )
  
  ###............................................................................
  ## step 2.02 reorder columns ----
  ##
  ###............................................................................
  #
  db.sasoil.U <- db.sasoil.t[, c(1, 37:6)] ## voltage
  db.sasoil.t <- db.sasoil.t[, c(1:3, 5, 4, 43:38, 49:44, 60:50, 69:61, 70, 71)] ## remove voltage and new order
  db.sasoil.tdr <- db.sasoil.tdr[, c(
    1:5, 10:6, 15:11, 25:16, ## E2
    30:26, 35:31, 45:36,     ## cond
    50:46, 55:51, 65:56      ## vwc
  )] 
  db.sasoil.total <- cbind(db.sasoil.t, db.sasoil.tdr[, -1])
  
  ###............................................................................
  ## step 2.03 safe data to txt-file ----
  ##
  ###............................................................................
  db.sasoil.total[, 1] <- format(as.POSIXct(db.sasoil.t[, 1], origin = origin, tz = "UTC"), format = "%Y-%m-%d %H:%M")
  # db.sasoil.t[,1]<-format( as.POSIXct(db.sasoil.tdr[,1],origin=origin,tz="UTC"),format='%Y-%m-%d %H:%M')
  # db.sasoil.tdr[,1]<-format( as.POSIXct(db.sasoil.tdr[,1],origin=origin,tz="UTC"),format='%Y-%m-%d %H:%M')
  
  write.table(db.sasoil.total,
              paste0(p.1$w[p.1$n == "LV0.p"], "SaSoil2002/00_full_dataset/SaSoil2002_", year_i, "_lv0.dat"),
              na = "NA", quote = F, dec = ".", sep = ",", row.names = F
  )
  
  # write.table(db.sasoil.t,
  #             paste0(p.1$w[p.1$n=="LV0.p"],"SaSoil2010/01_soiltemperature/SaSoil2010_Ts_",year_i,"_lv0.dat"),quote = F,dec=".",sep=",",row.names=F)
  #
  # write.table(db.sasoil.tdr,
  #             paste0(p.1$w[p.1$n=="LV0.p"],"SaSoil2010/02_soilmoisture/SaSoil2010_TDR_",year_i,"_lv0.dat"),quote = F,dec=".",sep=",",row.names=F)
  #
  # write.table(db.sasoil.tdr[,1:5],
  #             paste0(p.1$w[p.1$n=="LV0.p"],"SaSoil2010/05_snowdepth/SaSoil2010_Dsn_",year_i,"_lv0.dat"),quote = F,dec=".",sep=",",row.names=F)
  #
  
  cat("\n#\n# sasoil2002 ", year_i, " without problems!\n#\n") # main output
} # end loop over years
