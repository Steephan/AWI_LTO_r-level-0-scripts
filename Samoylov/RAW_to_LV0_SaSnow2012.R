###............................................................................
## SaSnow2012  RAW to Level0 ----
##
## equal time steps, no gaps, table flag
##
## by: Stephan.Lange@awi.de & Niko.Bornemann@awi.de
## last modified: 2021-03-25
##
###............................................................................
##
## last modifications:
## 2021-10-14 SL changed SWE calc to sensor 2 and inheritate flag in lvl1 script
## 2021-10-08 SL T correction with diffenrent T values if first option is flagged
## 2021-09-22 SL Snow correction set from 1.sep 0:00 to 31.aug next year (attention leap years implemted)
## 2021-07-28 SL add SWE calculation
## 2021-05-06 SL adapted to refresh app
## 2021-03-25 SL git path and temperature correction
## 2021-02-11 Sl out source of Dsn correction to file SaSnow2012_DSN_correction.dat
## 2020-09-09 CL after discussion with Niko "Dist_centre" and "Dist_crack" added
##               ==> "Dsn_centre" and "Dsn_crack" would be wrong because the sensors measure only the horizontal distance.
##               Please note, that the first letter has to be a capital letter because column names with the pattern "dist"
##               are filtered out in script LV0_to_LV1_SaAll.R
##
##
###............................................................................

### path definitions ----
## to run this script separately, you have to uncomment the next 10 lines!
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


options(scipen = 100) # for non-exponential display of numeric values

origin <- "1970-01-01"
# run.year <- 2018
recent.year <- as.numeric(format(Sys.Date(), "%Y"))

### loop over years ----
for (year.i in as.numeric(run.year)) {# 2012:recent.year
  
  #cat("\nProcessing year", year.i, "\n ==================== \n\n")
  start.date <- as.POSIXct(paste(year.i, "-01-01 00:00:00", sep = ""), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date <- as.POSIXct(paste(year.i, "-", 12, "-", 31, " 23:30:00", sep = ""), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  
  # create empty data frame with UTC time stamp every 30 min
  db.sasnow <- matrix(ncol = 98, nrow = length(seq(start.date, end.date, by = "30 min")), -999)
  db.sasnow[, c(2:98)] <- NA
  compl.temp <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "30 min")))
  db.sasnow[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = '%Y-%m-%d %H:%M:%S'))
  compl.temp[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = '%Y-%m-%d %H:%M:%S'))
  colnames(compl.temp) <- c("UTC", "erste")
  
  
  inz.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaSnow2012/01_SnowStation/")
  ##cat("\nnow in ", folders[paz], "\n ==================== \n\n")
  files2read <- list.files(inz.path, pattern = "*.dat")
  
  for (i in 1:length(files2read)) {#1:length(files2read)
    
    #cat("\nprocessing ", files2read[i], "\n ==================== \n\n")
    dada <- read.table(paste(inz.path, files2read[i], sep = ""), sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")
    
    colnames(dada) = paste0("V", seq_len(ncol(dada)))
    # skip file if wrong year.i
    if (as.numeric(substr(lapply(dada[1, 1], as.character), 1, 4)) > year.i || as.numeric(substr(lapply(dada[length(dada[, 1]), 1], as.character), 1, 4)) < year.i) {next}
    # #cat(paste(dada[1, 1], " to ", dada[length(dada[, 1]), 1], " ", files2read[i]))
    
    # check and remove double entries
    # check for fully double entries
    if (("TRUE" %in% duplicated(dada)) == TRUE) {
      doouble <- duplicated(dada)
      ##cat(paste(length(which(doouble == "TRUE")), "duplicated records found in file", files2read[i], "\n",
      #"first entry:", dada[which(doouble == "TRUE")[1], 1], "\n last entry:", dada[which(doouble == "TRUE")[length(which(doouble == "TRUE"))], 1], "\n\n"))
      dada <- unique(dada) # remove double entries
    } else if (("TRUE" %in% duplicated(dada[, 1])) == TRUE) {
      # check for multiple different data records for same! timestamp
      doouble <- duplicated(dada[, 1])
      ##cat(paste(length(which(doouble == "TRUE")), "multiple records found in file", files2read[i], "\n",
      #"first entry:", dada[which(doouble == "TRUE")[1], 1], "\n last entry:", dada[which(doouble == "TRUE")[length(which(doouble == "TRUE"))], 1], "\n\n"))
      dd <- which(dada[, 1] %in% dada[which(doouble == "TRUE"), 1])
      dada <- dada[-dd, ] # remove double entries
      
    } else {
      #cat("No double data entries found in", files2read[i], "\n\n")
    }
    
    dada[, 1] <- as.numeric(as.POSIXct(dada[, 1], format = '%Y-%m-%d %H:%M:%S', origin = origin, tz = "UTC"))
    if (length(dada[1, ]) == 78) {
      colnames(dada) <- c("UTC", "RECORD", "BattV_Min", "PTemp_C_Avg", "S2_C_highFq_Avg", "S2_C_lowFq_Avg", "S2_Phi_highFq_Avg",
                          "S2_Phi_lowFq_Avg", "S3_C_highFq_Avg", "S3_C_lowFq_Avg", "S3_Phi_highFq_Avg", "S3_Phi_lowFq_Avg",
                          "S4_C_highFq_Avg", "S4_C_lowFq_Avg", "S4_Phi_highFq_Avg", "S4_Phi_lowFq_Avg", "ChipTemp_SPA_Max",
                          "Temp_Corr_Distance_0_Avg", "Temp_Corr_Distance_1_Avg", "Temp_Corr_Distance_2_Avg", "Temp_Corr_Distance_3_Avg", "Temp_Corr_Distance_4_Avg",
                          "Temp_Corr_Distance_5_Avg", "Temp_Corr_Distance_6_Avg", "Temp_Corr_Distance_7_Avg", "Temp_Corr_Distance_8_Avg", "Temp_Corr_Distance_9_Avg",
                          "Raw_Distance_0_Avg", "Raw_Distance_1_Avg", "Raw_Distance_2_Avg", "Raw_Distance_3_Avg", "Raw_Distance_4_Avg", "Raw_Distance_5_Avg", "Raw_Distance_6_Avg",
                          "Raw_Distance_7_Avg", "Raw_Distance_8_Avg", "Raw_Distance_9_Avg", "QA_Distance_0_Avg", "QA_Distance_1_Avg", "QA_Distance_2_Avg", "QA_Distance_3_Avg",
                          "QA_Distance_4_Avg", "QA_Distance_5_Avg", "QA_Distance_6_Avg", "QA_Distance_7_Avg", "QA_Distance_8_Avg", "QA_Distance_9_Avg", "Snow_Temp_Avg(1)",
                          "Snow_Temp_Avg(2)", "Snow_Temp_Avg(3)", "Snow_Temp_Avg(4)", "Snow_Temp_Avg(5)", "Snow_Temp_Avg(6)", "Snow_Temp_Avg(7)", "Snow_Temp_Avg(8)",
                          "Snow_Temp_Avg(9)", "Snow_Temp_Avg(10)", "Snow_Temp_Avg(11)", "Snow_Temp_Avg(12)", "Surf_Temp_Avg(1)", "Surf_Temp_Avg(2)", "Surf_Temp_Avg(3)",
                          "Surf_Temp_Avg(4)", "Air_Temp_Avg", "Soil_Temp_Avg(1)", "Soil_Temp_Avg(2)", "Soil_Temp_Avg(3)", "Soil_Temp_Avg(4)", "Soil_Temp_Avg(5)", "Soil_Temp_Avg(6)",
                          "Soil_Temp_Avg(7)", "Soil_Temp_Avg(8)", "Temp_Corr_Distance_Centre_Avg", "Temp_Corr_Distance_Crack_Avg", "Raw_Distance_Centre_Avg", "Raw_Distance_Crack_Avg",
                          "QA_Distance_Centre_Avg", "QA_Distance_Crack_Avg")
      # new variables from april 2013
      dada$S2_ice_Avg <- NA
      dada$S2_water_Avg <- NA
      dada$S2_rho_Avg <- NA
      dada$S3_ice_Avg <- NA
      dada$S3_water_Avg <- NA
      dada$S3_rho_Avg <- NA
      dada$S4_ice_Avg <- NA
      dada$S4_water_Avg <- NA
      dada$S4_rho_Avg <- NA
    } else {# 87
      colnames(dada) <- c("UTC", "RECORD", "BattV_Min", "PTemp_C_Avg", "S2_ice_Avg", "S2_water_Avg", "S2_rho_Avg", "S2_C_highFq_Avg", "S2_C_lowFq_Avg", "S2_Phi_highFq_Avg",
                          "S2_Phi_lowFq_Avg", "S3_ice_Avg", "S3_water_Avg", "S3_rho_Avg", "S3_C_highFq_Avg", "S3_C_lowFq_Avg", "S3_Phi_highFq_Avg", "S3_Phi_lowFq_Avg",
                          "S4_ice_Avg", "S4_water_Avg", "S4_rho_Avg", "S4_C_highFq_Avg", "S4_C_lowFq_Avg", "S4_Phi_highFq_Avg", "S4_Phi_lowFq_Avg", "ChipTemp_SPA_Max",
                          "Temp_Corr_Distance_0_Avg", "Temp_Corr_Distance_1_Avg", "Temp_Corr_Distance_2_Avg", "Temp_Corr_Distance_3_Avg", "Temp_Corr_Distance_4_Avg",
                          "Temp_Corr_Distance_5_Avg", "Temp_Corr_Distance_6_Avg", "Temp_Corr_Distance_7_Avg", "Temp_Corr_Distance_8_Avg", "Temp_Corr_Distance_9_Avg",
                          "Raw_Distance_0_Avg", "Raw_Distance_1_Avg", "Raw_Distance_2_Avg", "Raw_Distance_3_Avg", "Raw_Distance_4_Avg", "Raw_Distance_5_Avg", "Raw_Distance_6_Avg",
                          "Raw_Distance_7_Avg", "Raw_Distance_8_Avg", "Raw_Distance_9_Avg", "QA_Distance_0_Avg", "QA_Distance_1_Avg", "QA_Distance_2_Avg", "QA_Distance_3_Avg",
                          "QA_Distance_4_Avg", "QA_Distance_5_Avg", "QA_Distance_6_Avg", "QA_Distance_7_Avg", "QA_Distance_8_Avg", "QA_Distance_9_Avg", "Snow_Temp_Avg(1)",
                          "Snow_Temp_Avg(2)", "Snow_Temp_Avg(3)", "Snow_Temp_Avg(4)", "Snow_Temp_Avg(5)", "Snow_Temp_Avg(6)", "Snow_Temp_Avg(7)", "Snow_Temp_Avg(8)",
                          "Snow_Temp_Avg(9)", "Snow_Temp_Avg(10)", "Snow_Temp_Avg(11)", "Snow_Temp_Avg(12)", "Surf_Temp_Avg(1)", "Surf_Temp_Avg(2)", "Surf_Temp_Avg(3)",
                          "Surf_Temp_Avg(4)", "Air_Temp_Avg", "Soil_Temp_Avg(1)", "Soil_Temp_Avg(2)", "Soil_Temp_Avg(3)", "Soil_Temp_Avg(4)", "Soil_Temp_Avg(5)", "Soil_Temp_Avg(6)",
                          "Soil_Temp_Avg(7)", "Soil_Temp_Avg(8)", "Temp_Corr_Distance_Centre_Avg", "Temp_Corr_Distance_Crack_Avg", "Raw_Distance_Centre_Avg", "Raw_Distance_Crack_Avg",
                          "QA_Distance_Centre_Avg", "QA_Distance_Crack_Avg")
    }
    # new order
    dada <- dada[, c("UTC", "RECORD", "BattV_Min", "PTemp_C_Avg", "ChipTemp_SPA_Max",
                     "S2_ice_Avg", "S3_ice_Avg", "S4_ice_Avg", "S2_water_Avg", "S3_water_Avg", "S4_water_Avg", "S2_rho_Avg", "S3_rho_Avg", "S4_rho_Avg",
                     "S2_C_highFq_Avg", "S2_C_lowFq_Avg", "S3_C_highFq_Avg", "S3_C_lowFq_Avg", "S4_C_highFq_Avg", "S4_C_lowFq_Avg",
                     "S2_Phi_highFq_Avg", "S2_Phi_lowFq_Avg", "S3_Phi_highFq_Avg", "S3_Phi_lowFq_Avg", "S4_Phi_highFq_Avg", "S4_Phi_lowFq_Avg",
                     "Temp_Corr_Distance_0_Avg", "Temp_Corr_Distance_1_Avg", "Temp_Corr_Distance_2_Avg", "Temp_Corr_Distance_3_Avg", "Temp_Corr_Distance_4_Avg",
                     "Temp_Corr_Distance_5_Avg", "Temp_Corr_Distance_6_Avg", "Temp_Corr_Distance_7_Avg", "Temp_Corr_Distance_8_Avg", "Temp_Corr_Distance_9_Avg",
                     "Temp_Corr_Distance_Centre_Avg", "Temp_Corr_Distance_Crack_Avg", "Raw_Distance_0_Avg", "Raw_Distance_1_Avg", "Raw_Distance_2_Avg",
                     "Raw_Distance_3_Avg", "Raw_Distance_4_Avg", "Raw_Distance_5_Avg", "Raw_Distance_6_Avg", "Raw_Distance_7_Avg", "Raw_Distance_8_Avg",
                     "Raw_Distance_9_Avg", "Raw_Distance_Centre_Avg", "Raw_Distance_Crack_Avg", "QA_Distance_0_Avg", "QA_Distance_1_Avg", "QA_Distance_2_Avg",
                     "QA_Distance_3_Avg", "QA_Distance_4_Avg", "QA_Distance_5_Avg", "QA_Distance_6_Avg", "QA_Distance_7_Avg", "QA_Distance_8_Avg",
                     "QA_Distance_9_Avg", "QA_Distance_Centre_Avg", "QA_Distance_Crack_Avg",
                     "Snow_Temp_Avg(1)",
                     "Snow_Temp_Avg(5)", "Snow_Temp_Avg(9)", "Snow_Temp_Avg(10)", "Snow_Temp_Avg(11)", "Snow_Temp_Avg(12)", "Snow_Temp_Avg(8)", "Snow_Temp_Avg(4)",
                     "Snow_Temp_Avg(3)", "Snow_Temp_Avg(2)", "Snow_Temp_Avg(6)", "Snow_Temp_Avg(7)", "Surf_Temp_Avg(1)", "Surf_Temp_Avg(2)", "Surf_Temp_Avg(3)",
                     "Surf_Temp_Avg(4)", "Air_Temp_Avg", "Soil_Temp_Avg(1)", "Soil_Temp_Avg(2)", "Soil_Temp_Avg(3)", "Soil_Temp_Avg(4)", "Soil_Temp_Avg(5)", "Soil_Temp_Avg(6)",
                     "Soil_Temp_Avg(8)", "Soil_Temp_Avg(7)")] 
    # merge all data into one dataframe
    newdf.a <- merge(compl.temp, dada, all.x = T, by = "UTC")
    # achtung ohne dsn spalten
    for (k in 2:86) {
      db.sasnow[, k] <- rowMeans(cbind(db.sasnow[, k], newdf.a[, k + 2]), na.rm = T)#
    }
  }
  
  ##
  for (kl in 2:ncol(db.sasnow)) {
    db.sasnow[is.nan(as.numeric(db.sasnow[, kl])), kl] <- NA
  }
  
  
  ###............................................................................
  ###............................................................................
  ## Snowdepth correction ----
  ## read SaMet2002 Airtemperature for Snowdepth correction
  ## use A) tair values in 50 cm with flag==0
  ## use B) tair values in 50 cm of side b if A) is not True
  ## use C) tair values in 200 cm of side a if A) and B) is not True
  ##
  clima <- read.table(paste0(p.1$w[p.1$n == "LV1.p"], "SaMet2002/00_full_dataset/SaMet2002_", year.i, "_lv1.dat"), sep = ",", dec = ".", header = T, fill = TRUE, na = "NA")[, 1:9]
  tmp1 <- which(clima$Tair_a_50_fl>0)
  clima$Tair_a_50[tmp1] <- clima$Tair_b_50[tmp1]
  clima$Tair_a_50_fl[tmp1] <- clima$Tair_b_50_fl[tmp1]
  
  tmp2 <- which(clima$Tair_a_50_fl>0)
  clima$Tair_a_50[tmp2] <- clima$Tair_a_200[tmp2]
  clima$Tair_a_50_fl[tmp2] <- clima$Tair_a_200_fl[tmp2]
  
  for (ii in 1:12) {
    # overwrite distcor_0-11
    db.sasnow[,25 + ii] <- round(db.sasnow[, 37+ii] * ((clima$Tair_a_50 + 273.15) / 273.15)^0.5, 3)
  }
  ##
  ##
  ###............................................................................
  ###............................................................................
  
  
  
  
  db.sasnow[, 1] <- format( as.POSIXct(db.sasnow[, 1], origin = origin, tz = "UTC"), format = '%Y-%m-%d %H:%M')
  
  
  ###............................................................................
  ## special calculation of snowdepth ----
  # ==> offset correction
  #
  
  dsn.corr <- read.table(paste0(p.1$w[p.1$n == "settings.p"], "DSNcorr.files/SaSnow2012_DSN_correction.dat"), sep = ",", dec = ".", header = T, fill = TRUE, na = "NAN")
  
  spring.corr <- as.numeric(c(dsn.corr[which(dsn.corr$YEAR==(year.i-1)),3:14])) # the old from last year.i
  autum.corr <-  as.numeric(c(dsn.corr[which(dsn.corr$YEAR==(year.i)),3:14])) # the new one based on maximum dist in august
  
  snow.free <- c(which(db.sasnow[, 1]==paste0(year.i,"-09-01 00:00")),which(db.sasnow[, 1]==paste0(year.i,"-09-01 00:00")))
  
  
  for (i in 1:12) {
    db.sasnow[1:snow.free[1], 86 + i] <- na.minus(spring.corr[i], as.numeric(db.sasnow[1:snow.free[1], 25 + i]))
    #db.sasnow[snow.free[1]:snow.free[2], 86 + i] <- 0 # snowfree
    db.sasnow[snow.free[2]:length(db.sasnow[, 1]), 86 + i] <- na.minus(autum.corr[i], db.sasnow[snow.free[2]:length(db.sasnow[, 1]), 25 + i])
  }
  
  
  
  ## new names ----
  colnames(db.sasnow) <- c("UTC", "Ubat", "Tpan_CR1000", "Tpan_SPA",
                           
                           "ice_2", "ice_3", "ice_4",
                           "water_2", "water_3", "water_4",
                           "rho_2", "rho_3", "rho_4",
                           "cap_highFq_2", "cap_lowFq_2", "cap_highFq_3", 	"cap_lowFq_3", "cap_highFq_4", "cap_lowFq_4",
                           "phi_highFq_2", "phi_lowFq_2", "phi_highFq_3", 	"phi_lowFq_3", "phi_highFq_4", "phi_lowFq_4",
                           
                           "distcor_0", "distcor_1", "distcor_2", "distcor_3", "distcor_4", "distcor_5", "distcor_6", "distcor_7", "distcor_8", "distcor_9", "distcor_Centre", "distcor_Crack",
                           "distraw_0", "distraw_1", "distraw_2", "distraw_3", "distraw_4", "distraw_5", "distraw_6", "distraw_7", "distraw_8", "distraw_9", "distraw_Centre", "distraw_Crack",
                           "QA_dist_0", "QA_dist_1", "QA_dist_2", "QA_dist_3", "QA_dist_4", "QA_dist_5", "QA_dist_6", "QA_dist_7", "QA_dist_8", "QA_dist_9", "QA_dist_Centre", "QA_dist_Crack",
                           
                           "Tsn_m4", "Tsn_00", "Tsn_05", "Tsn_10", "Tsn_15", "Tsn_20", "Tsn_25", "Tsn_30", "Tsn_35", "Tsn_40", "Tsn_45", "Tsn_50", # needles
                           "Tgs_1", 	"Tgs_2", 	"Tgs_3", "Tgs_4", 	"Tair_80",
                           "Ts_00", 	"Ts_05", 	"Ts_10", 	"Ts_20", 	"Ts_40", 	"Ts_60", "Ts_80", 	"Ts_100",
                           "Dsn_0", "Dsn_1", "Dsn_2", "Dsn_3", "Dsn_4", "Dsn_5", "Dsn_6", "Dsn_7", "Dsn_8", "Dsn_9", "Dist_center", "Dist_crack")
  ###............................................................................
  ## SWE calculation ----
  #
  db.sasnow <- as.data.frame(db.sasnow)
  
  # db.sasnow$SWE_2 <- as.numeric(db.sasnow$rho_2) * as.numeric(apply(db.sasnow[,26:30],1,max))
  # db.sasnow$SWE_3 <- as.numeric(db.sasnow$rho_3) * as.numeric(apply(db.sasnow[,26:30],1,max))
  # db.sasnow$SWE_4 <- as.numeric(db.sasnow$rho_4) * as.numeric(apply(db.sasnow[,26:30],1,max))
  db.sasnow$SWE_2 <- as.numeric(db.sasnow$rho_2) * as.numeric(db.sasnow$distcor_2) 
  db.sasnow$SWE_3 <- as.numeric(db.sasnow$rho_3) * as.numeric(db.sasnow$distcor_2) 
  db.sasnow$SWE_4 <- as.numeric(db.sasnow$rho_4) * as.numeric(db.sasnow$distcor_2) 
  
  db.sasnow<-db.sasnow[,c(1:13,99:101,14:98)]
  
  write.table(db.sasnow[as.numeric(format(as.POSIXct(db.sasnow[, 1], format = '%Y-%m-%d %H:%M', origin = origin, tz = "UTC"), format = '%Y')) == year.i, -2],
              paste0(p.1$w[p.1$n == "LV0.p"], "SaSnow2012/00_full_dataset/SaSnow2012_", year.i, "_lv0.dat"), quote = F, dec = ".", sep = ",", row.names = F)
  
  cat("\n#\n# level 0  SaSnow2012 : ", year.i, " without problems!\n#\n")
  
} # end loop over year.is



