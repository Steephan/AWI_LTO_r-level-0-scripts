###.........................................................................
##
##   BaMet2009         RAW to Level0 ----
##
##   equal time steps, no gaps, table flag
##
##   written by: Stephan.Lange@awi.de
##               
##
##   last check: 2020-02-03
##   checked by: christian.lehr@awi.de
##
###.........................................................................
##
## open issues:
##
##  Attention!!!
##  2009 and 2010(1/2) without precipitation!    (bk0)
##   - dsn correction to file
##   - Protocols says that they should be in BaSoil, but they didnt
##   - use instead BaMet98 data from former heidelberg database
##
##
##
###.........................................................................
#
#  last modifications: ----
#  2021-05-12 SL adapted to runnerapp and content management
#  2020-09-22 CL correction of the description of column names for file "20150909BaMet2010_bk2Online.dat".
#                The file was wrongly stored in paste0(p.1$w[p.1$n == "RAW.p"], "BaMet2010/bk2/2015/") and is
#                moved now to folder paste0(p.1$w[p.1$n == "RAW.p"], "BaMet2015/")
#  2020-08-12 NB new offset for Dsn in 2020
#  2020-04-30 CL add re-calculation of SwNet, LwNet and Albedo after year 2017, because of re-calculation of the In and Out radiation
#                add calculation of RadNet, analogue to the netto radiation for the Pangaea data set
#  2020-04-14 CL change loop to (year in run.year) to allow the selection of the processed year in Bayelva_MAIN.R
#  2019-10-02 CL change / correct the assignment of names of columns of long-wave in and out in db.bamet
#                correction short-wave and long-wave radiation with multiplier values for the years 2017-2019 == > preprocessing step c)
#  2019-02-06 SL new path
#  2017_04_24 SL add prec for 2009 and 2010 from BaMet98 Heidelberg database
#  2017_04_06 SL switch SwIn and SwOut in colnaming at the End
#  2017_02_06 SL correction of all snowdepth, rawdist and distcor-problems
#  2016-09-22 SL edit snow/air temperature ... everything to 2009
#  2016-02-15 SL sort/name soil
#  2015-12-14 SL add empty colums to 2009-2015; new order of colums; add new in.path(2015)
#
###.........................................................................
##
## Comments: ----
##
##  Corrections:
##
##  a) snow distance manipulation
##  b) get prec from heidelberg db for 2009 & 2010
##  c) correct short-wave and long-wave radiation with multiplier values
##     short- / long-wave in ==> upper sensor
##     short- / long-wave out ==> lower sensor
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


# options(scipen = 100) # for non-exponential display of numeric values
# 
# origin <- "1970-01-01"
# input folders  ----
folders <- c(paste0(p.1$w[p.1$n == "RAW.p"], "BaMet2010/bk0/"),
           paste0(p.1$w[p.1$n == "RAW.p"], "BaMet2010/bk1/"),
           paste0(p.1$w[p.1$n == "RAW.p"], "BaMet2010/bk2/2015/"),
           #   paste0(p.1$w[p.1$n == "RAW.p"], "BaMet2010/bk2/2016/" ),
           paste0(p.1$w[p.1$n == "RAW.p"], "BaMet2015/"),
           paste0(p.1$w[p.1$n == "RAW.p"], "BaMet2010/bk2/"),
           paste0(p.1$w[p.1$n == "ONL.p"], "BaMet2015/"))


###.........................................................................
#### to run this script separately, you have to set run.year:
# recent.year <- as.numeric(format(Sys.Date(), "%Y"))
# run.year <- 2020#recent.year
###.........................................................................
# loop over years ----
for (year in run.year) {
  start.date <- as.POSIXct(paste(year, "-01-01 00:00:00", sep = ""), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date <- as.POSIXct(paste(year, "-", 12, "-", 31, " 23:30:00", sep = ""), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")

  # create empty data frame with UTC time stamp every 30 min
  db.bamet <- matrix(ncol = 43, nrow = length(seq(start.date, end.date, by = "30 min")), -999)
  db.bamet[, c(2:43)] <- NA
  compl.temp <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "30 min")))
  db.bamet[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = '%Y-%m-%d %H:%M:%S'))
  compl.temp[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = '%Y-%m-%d %H:%M:%S'))
  colnames(compl.temp) <- c("UTC", "erste")

  # loop for all folders ----
  for (paz in c(1:length(folders))) { # 1:6 for all years
    inz.path <- folders[paz]
    #  cat("\nnow in ", folders[paz], "\n ==================== \n\n")
    files2read <- list.files(inz.path, pattern = "*.dat")

    # loop for all files
    for (i in (1:length(files2read))) {
      # if (paz == 3 && files2read[i] == "20150903_20150101_BaMet2010_bk2Online.dat") {next}  # skip newest 2015 data
      # if (paz == 3 && files2read[i] == "20150819_20130827_BaMet2010_bk2Online.dat") {next}  # skip newest 2015 data
      # if (paz == 3 && files2read[i] == "Teil2.dat") {next}  # skip newest 2015 data
      #    cat("\nprocessing ", files2read[i], "\n ==================== \n\n")
      dada <- read.table(paste(inz.path, files2read[i], sep = ""),
                         sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")

      # replace infinity values with NA
      #    cat("\nreplace ", length(dada[as.character(dada[, ncol(dada)]) == "Inf", ncol(dada)]), " Inf-values \n")
      is.na(dada) <- sapply(dada, is.infinite)
      #
      colnames(dada) <- paste0("V", seq_len(ncol(dada)))
      # skip file if wrong year
      if ((as.numeric(substr(lapply(dada[1, 1], as.character), 1, 4)) > year) ||
          (as.numeric(substr(lapply(dada[length(dada[, 1]), 1], as.character), 1, 4)) < year)) {
        next
      }
      dada <- check.double.entry(dada)
      dada[, 1] <- as.numeric(as.POSIXct(dada[, 1], format = '%Y-%m-%d %H:%M:%S', origin = origin, tz = "UTC"))

      if (paz == 1) {    
        # folder 1 ----
        # Rain_Tot not in bk0
        colnames(dada) <- c("UTC", "RECORD", "Batt_Volt_Avg", "Temp_200_Avg", "RH_200_Avg", "wind_speed", "wind_dir", "wind_dir_sd",
                          "Raw_Dist", "distance_corr",
                          "Tair_4", "Tair_100", "Tair_20", "Ts_252_2", "Ts_252_12", "Ts_252_32", "Ts_252_62",
                          "Ts_252_102", "Ts_252_152", "Ts_203_2", "Ts_203_5", "Ts_203_23", "Ts_203_53",
                          "Ts_203_93", "Ts_203_143", "Tpan_RAD",
                          "SR01Up_Avg", "SR01Dn_Avg", "IR01Up_Avg", "IR01Dn_Avg",
                          "IR01DnCo_Avg", "IR01UpCo_Avg", "SR01Up_raw_Avg", "SR01Dn_raw_Avg", "IR01Up_raw_Avg", "IR01Dn_raw_Avg", "NetRs_Avg",
                          "NetRl_Avg", "Albedo_Avg")
        dada$Rain_Tot <- NA; dada$SignalQuality <- NA
        dada$wind_speed_Max <- NA; dada$wind_speed_Min <- NA
        dada$Dsn <- NA #1.45-distance_corr
        # reorder data frame for missing rain_tot
        dada <- dada[, c("UTC", "RECORD", "Batt_Volt_Avg", "Tair_100", "Temp_200_Avg", "RH_200_Avg", "wind_speed", "wind_dir", "wind_dir_sd",
                        "wind_speed_Max", "wind_speed_Min", "Rain_Tot",
                        "Raw_Dist", "SignalQuality", "distance_corr", "Dsn",
                        "Tair_4", "Tair_20",
                        "Ts_252_2", "Ts_252_12", "Ts_252_32", "Ts_252_62", "Ts_252_102", "Ts_252_152",
                        "Ts_203_2", "Ts_203_5", "Ts_203_23", "Ts_203_53", "Ts_203_93", "Ts_203_143",
                        "SR01Up_Avg", "SR01Dn_Avg", "IR01Up_Avg", "IR01Dn_Avg",
                        "IR01DnCo_Avg", "IR01UpCo_Avg", "SR01Up_raw_Avg", "SR01Dn_raw_Avg", "IR01Up_raw_Avg", "IR01Dn_raw_Avg", "NetRs_Avg",
                        "NetRl_Avg", "Albedo_Avg", "Tpan_RAD")]
      } else if ((paz == 2) | (paz == 3)) {
        # folder 2 & 3 ----
        colnames(dada) <- c("UTC", "RECORD", "Batt_Volt_Avg", "Temp_200_Avg", "RH_200_Avg", "wind_speed", "wind_dir", "wind_dir_sd",
                          "Rain_Tot", "Raw_Dist", "distance_corr",
                          "Tair_4",
                          "Tair_100", "Tair_20", "Ts_252_2", "Ts_252_12", "Ts_252_32", "Ts_252_62",
                          "Ts_252_102", "Ts_252_152", "Ts_203_2", "Ts_203_5", "Ts_203_23", "Ts_203_53",
                          "Ts_203_93", "Ts_203_143", "Tpan_RAD",
                          "SR01Up_Avg", "SR01Dn_Avg", "IR01Up_Avg", "IR01Dn_Avg",
                          "IR01DnCo_Avg", "IR01UpCo_Avg", "SR01Up_raw_Avg", "SR01Dn_raw_Avg", "IR01Up_raw_Avg", "IR01Dn_raw_Avg", "NetRs_Avg",
                          "NetRl_Avg", "Albedo_Avg")
        ## add empty colums
        dada$SignalQuality <- NA; dada$wind_speed_Max <- NA;
        dada$wind_speed_Min <- NA
        dada$Dsn <- NA #1.45-distance_corr

        # reorder data frame for missing rain_tot
        dada <- dada[, c("UTC", "RECORD", "Batt_Volt_Avg", "Tair_100", "Temp_200_Avg", "RH_200_Avg", "wind_speed", "wind_dir", "wind_dir_sd",
                        "wind_speed_Max", "wind_speed_Min", "Rain_Tot",
                        "Raw_Dist", "SignalQuality", "distance_corr", "Dsn",
                        "Tair_4", "Tair_20",
                        "Ts_252_2", "Ts_252_12", "Ts_252_32", "Ts_252_62", "Ts_252_102", "Ts_252_152",
                        "Ts_203_2", "Ts_203_5", "Ts_203_23", "Ts_203_53", "Ts_203_93", "Ts_203_143",
                        "SR01Up_Avg", "SR01Dn_Avg", "IR01Up_Avg", "IR01Dn_Avg",
                        "IR01DnCo_Avg", "IR01UpCo_Avg", "SR01Up_raw_Avg", "SR01Dn_raw_Avg", "IR01Up_raw_Avg", "IR01Dn_raw_Avg", "NetRs_Avg",
                        "NetRl_Avg", "Albedo_Avg", "Tpan_RAD")]
      } else {
        # folder 4 ----
        #       colnames(dada) <- c("UTC", "RECORD", "Batt_Volt_Avg", "Temp_200_Avg", "RH_200_Avg", "wind_speed_WVc(1)", "wind_speed_WVc(2)",
        #                         "wind_speed_WVc(3)", "Rain_Tot", "Raw_Dist", "snowheight", "PT100_T_Avg(1)", "PT100_T_Avg(2)", "PT100_T_Avg(3)",
        #                         "PT100_T_Avg(4)", "PT100_T_Avg(5)", "PT100_T_Avg(6)", "PT100_T_Avg(7)", "PT100_T_Avg(8)", "PT100_T_Avg(9)",
        #                         "PT100_T_Avg(10)", "PT100_T_Avg(11)", "PT100_T_Avg(12)", "PT100_T_Avg(13)", "PT100_T_Avg(14)", "PT100_T_Avg(15)",
        #                         "PT100_T_Avg(16)", "CM3Up_Avg", "CM3Dn_Avg", "CG3Up_Avg", "CG3Dn_Avg", "CG3DnCo_Avg", "CG3UpCo_Avg", "CM3Up_raw_Avg",
        #                         "CM3Dn_raw_Avg", "CG3Up_raw_Avg", "CG3Dn_raw_Avg", "NetRs_Avg", "NetRl_Avg", "Albedo_Avg")
        ## since sep 2015
        #print(dada[1, 1:13])
        colnames(dada) <- c("UTC", "RECORD", "Batt_U", "Tair_200", "RH_200", "wind_speed", "wind_dir", "wind_dir_sd",
                          "wind_speed_Max", "wind_speed_Min", "Rain_Tot",
                          "Raw_Dist", "SignalQuality", "distance_corr",
                          "Tair_4",
                          "Tair_100", "Tair_20", "Ts_252_2", "Ts_252_12", "Ts_252_32", "Ts_252_62",
                          "Ts_252_102", "Ts_252_152", "Ts_203_2", "Ts_203_5", "Ts_203_23", "Ts_203_53",
                          "Ts_203_93", "Ts_203_143", "Tpan_RAD",
                          "SR01Up_Avg", "SR01Dn_Avg", "IR01Up_Avg", "IR01Dn_Avg",
                          "IR01DnCo_Avg", "IR01UpCo_Avg", "SR01Up_raw_Avg", "SR01Dn_raw_Avg", "IR01Up_raw_Avg", "IR01Dn_raw_Avg", "NetRs_Avg",
                          "NetRl_Avg", "Albedo_Avg")
        dada$Dsn <- NA
        dada <- dada[, c("UTC", "RECORD", "Batt_U", "Tair_100", "Tair_200", "RH_200", "wind_speed", "wind_dir", "wind_dir_sd",
                        "wind_speed_Max", "wind_speed_Min", "Rain_Tot",
                        "Raw_Dist", "SignalQuality", "distance_corr", "Dsn",
                        "Tair_4", "Tair_20",
                        "Ts_252_2", "Ts_252_12", "Ts_252_32", "Ts_252_62", "Ts_252_102", "Ts_252_152",
                        "Ts_203_2", "Ts_203_5", "Ts_203_23", "Ts_203_53", "Ts_203_93", "Ts_203_143",
                        "SR01Up_Avg", "SR01Dn_Avg", "IR01Up_Avg", "IR01Dn_Avg",
                        "IR01DnCo_Avg", "IR01UpCo_Avg", "SR01Up_raw_Avg", "SR01Dn_raw_Avg", "IR01Up_raw_Avg", "IR01Dn_raw_Avg", "NetRs_Avg",
                        "NetRl_Avg", "Albedo_Avg", "Tpan_RAD")]
      }
      # merge all data into one dataframe
      newdf.a <- merge(compl.temp, dada, all.x = T, by = "UTC")
      for (k in 2:43) {
        db.bamet[, k] <- rowMeans(cbind(db.bamet[, k], newdf.a[, k + 2]), na.rm = T)#
      }
    }
  }

  # name columns of db.bamet according to data -----
  colnames(db.bamet) <- c("UTC", "batt_U", "Tair_100", "Tair_200", "RH_200", "wind_v_200", "wind_deg_200", "wind_sddeg_200",
                        "wind_vmax_200", "wind_vmin_200", "prec",
                        "distraw", "snowsq", "distcor", "Dsn",
                        "Tair_4", "Tair_20",
                        "Ts_252_2", "Ts_252_12", "Ts_252_32", "Ts_252_62", "Ts_252_102", "Ts_252_152",
                        "Ts_203_2", "Ts_203_5", "Ts_203_23", "Ts_203_53", "Ts_203_93", "Ts_203_143",
                        "SwIn", "SwOut", "LwIn_rawcor", "LwOut_rawcor",
                        "LwOut", "LwIn", "SwIn_raw", "SwOut_raw", "LwIn_raw", "LwOut_raw",
                        "SwNet", "LwNet", "Albedo", "Tpan_NR1")
  #db.bamet[1:10, ]

  ###.........................................................................
  ## corrections ----
  ##### a) snow distance manipulation ----
  ##### b) get prec from heidelberg db for 2009 & 2010 ----
  ###
  if (year == 2009) {
    #db.bamet[, 15]                            <- round(1.45-db.bamet[, 14], 3)
    db.bamet[, "Dsn"] <- round(1.45 - db.bamet[, "distcor"], 3)
    old.prec <- read.table("N:/geo5/SoilData/data/level0/BaMet1998/02_prec/BaMet1998prec_2009.dat", sep = ",", dec = ".", header = T)
    #db.bamet[seq(1, 17520, 2), 11] <- old.prec[, 2]
    #db.bamet[1:14570, 11] <- NA # like the other sensors
    db.bamet[seq(1, 17520, 2), "prec"]  <- old.prec[, 2]
    db.bamet[1:14570, "prec"]  <- NA # like the other sensors
  }

  if (year == 2010) {
    db.bamet[11395:length(db.bamet[, "snowsq"]), "distcor"] <- 1.5 - db.bamet[11395:length(db.bamet[, "distcor"]), "distcor"]
    db.bamet[, "Dsn"] <- round(1.5 - db.bamet[, "distcor"], 3)
    #db.bamet[11395:length(db.bamet[, 13]), 14] <- 1.5-db.bamet[11395:length(db.bamet[, 14]), 14]
    #db.bamet[, 15]                            <- round(1.5-db.bamet[, 14], 3)
    old.prec <- read.table("N:/geo5/SoilData/data/level0/BaMet1998/02_prec/BaMet1998prec_2010.dat", sep = ",", dec = ".", header = T)
    db.bamet[seq(1, 11355, 2), "prec"] <- old.prec[1:5678, 2]
  }

  if ((year >= 2011) & (year <= 2014)) {
    #            {db.bamet[, 15]              <- db.bamet[, 14]
    #db.bamet[, 14]                            <- round(1.45-db.bamet[, 15], 3) }
    db.bamet[, "Dsn"] <- db.bamet[, "distcor"]
    db.bamet[, "distcor"] <- round(1.45 - db.bamet[, "Dsn"], 3)
  }

  if (year == 2015) {#db.bamet[, 12] <- round(db.bamet[, 12], 3)
    db.bamet[, "distcor"] <- round(     db.bamet[, "distcor"], 3)
    db.bamet[1:11799, "distcor"] <- round(1.45 - db.bamet[1:11799, "distcor"], 3)## 11799 == 2015-09-03 19:00
    db.bamet[, "Dsn"] <- round(1.45 - db.bamet[, "distcor"], 3)

    #db.bamet[, 14]                            <- round(     db.bamet[, 14], 3)
    #db.bamet[1:11799, 14]                     <- round(1.45 - db.bamet[1:11799, 14], 3)## 11799 == 2015-09-03 19:00
    #db.bamet[, 15]                            <- round(1.45 - db.bamet[, 14], 3)
  }

  if ((year > 2015) & (year < 2017)) {
    db.bamet[, "Dsn"] <- round(1.45 - db.bamet[, "distcor"], 3)
  }
              #{ db.bamet[, 15]                            <- round(1.45 - db.bamet[, 14], 3)}

  if (year == 2017) {
    db.bamet[1:9350, "Dsn"] <- round(1.45 - db.bamet[1:9350, "distcor"], 3)
    db.bamet[9351:17520, "Dsn"] <- round(1.57 - db.bamet[9351:17520, "distcor"], 3)
  } ## 11799 == 2017-07-14 19:00
  #db.bamet[1:9350, 15]                                     <- round(1.45 - db.bamet[1:9350, 14], 3)
  #db.bamet[9351:17520, 15]                                 <- round(1.57 - db.bamet[9351:17520, 14], 3)}## 11799 == 2017-07-14 19:00

  if ((year > 2017) & (year < 2020)) {
    db.bamet[, "Dsn"] <- round(1.57 - db.bamet[, "distcor"], 3)
  }
              # { db.bamet[, 15]                            <- round(1.57-db.bamet[, 14], 3)}

  if (year == 2020) {
    db.bamet[1:8524, "Dsn"] <- round(1.57 - db.bamet[1:8524, "distcor"], 3) #till 2020-06-26 13:30
    db.bamet[8524:17568, "Dsn"] <- round(1.56 - db.bamet[8524:17568, "distcor"], 3) #from 2020-06-26 13:30
  }

  if (year > 2020) {
      db.bamet[, "Dsn"] <- round(1.56 - db.bamet[, "distcor"], 3)
  }


  for (m in 2:43) {# replace Nan with NA
    # db.bamet[, m] <- as.numeric(db.bamet[, m])
    db.bamet[is.nan(as.numeric(db.bamet[, m])), m] <- NA
    #if (is.na(db.bamet[, m]) == FALSE) { db.bamet[, m] <- sprintf('%1.4f', db.bamet[, m]) }
  }


  ###.........................................................................
  ##### c) correct short-wave and long-wave radiation with multiplier values ----
  ## short- / long-wave in ==> upper sensor
  ## short- / long-wave out ==> lower sensor
  ## re-calculate the netto radiation

  # create vector with dates
  dates <- format(as.POSIXct(db.bamet[, 1], origin = origin, tz = "UTC"), format = '%Y-%m-%d %H:%M')

  if (year == 2017) {
    # update of sensor software at 2017/07/12 16:30:00
    # http://sparcwiki.awi-potsdam.de/doku.php?id = observatory:nyalesund:stations:bamet2009:bamet2009log
    # Index of values to be changed
    #ind <- which(db.bamet[, "UTC"] ==  "2017-07-12 16:30"):length(db.bamet[, "UTC"])
    ind <- which(dates == "2017-07-12 16:30"):length(db.bamet[, "UTC"])

    ### correction of raw values with multiplier values
    # short-wave
    db.bamet[ind, "SwIn"] <- (db.bamet[ind, "SwIn_raw"] * 1000) / 14.61
    db.bamet[ind, "SwOut"] <- (db.bamet[ind, "SwOut_raw"] * 1000) / 11.09
    # long-wave
    db.bamet[ind, "LwIn_rawcor"] <- (db.bamet[ind, "LwIn_raw"] * 1000) / 14.58
    db.bamet[ind, "LwOut_rawcor"] <- (db.bamet[ind, "LwOut_raw"] * 1000) / 14.30

    ### correction of long-wave radiation with temperature of panel
    db.bamet[ind, "LwIn"] <- db.bamet[ind, "LwIn_rawcor"]  + 5.67 * 10^-8 * (db.bamet[ind, "Tpan_NR1"] + 273.15)^4
    db.bamet[ind, "LwOut"] <- db.bamet[ind, "LwOut_rawcor"] + 5.67 * 10^-8 * (db.bamet[ind, "Tpan_NR1"] + 273.15)^4

    # correction of SwNet and LwNet
    db.bamet[ind, "SwNet"] <- db.bamet[ind, "SwIn"] - db.bamet[ind, "SwOut"]
    db.bamet[ind, "LwNet"] <- db.bamet[ind, "LwIn"] - db.bamet[ind, "LwOut"]

    # Albedo
    db.bamet[ind, "Albedo"] <- db.bamet[ind, "SwOut"] / db.bamet[ind, "SwIn"]
  }

  if (year == 2018) {
    # update of sensor software at 2019-08-28 17:24
    # http://sparcwiki.awi-potsdam.de/doku.php?id = observatory:nyalesund:stations:bamet2009:bamet2009log
    # select entire year 2018

    ### correction of raw values with multiplier values
    # short-wave
    db.bamet[, "SwIn"] <- (db.bamet[, "SwIn_raw"] * 1000) / 14.61
    db.bamet[, "SwOut"] <- (db.bamet[, "SwOut_raw"] * 1000) / 11.09
    # long-wave
    db.bamet[, "LwIn_rawcor"] <- (db.bamet[, "LwIn_raw"] * 1000) / 14.58
    db.bamet[, "LwOut_rawcor"] <- (db.bamet[, "LwOut_raw"] * 1000) / 14.30

    ### correction of long-wave radiation with temperature of panel
    db.bamet[, "LwIn"] <- db.bamet[, "LwIn_rawcor"]  + 5.67 * 10^-8 * (db.bamet[, "Tpan_NR1"] + 273.15)^4
    db.bamet[, "LwOut"] <- db.bamet[, "LwOut_rawcor"] + 5.67 * 10^-8 * (db.bamet[, "Tpan_NR1"] + 273.15)^4

    # correction of SwNet and LwNet
    db.bamet[, "SwNet"] <- db.bamet[, "SwIn"] - db.bamet[, "SwOut"]
    db.bamet[, "LwNet"] <- db.bamet[, "LwIn"] - db.bamet[, "LwOut"]

    # Albedo
    db.bamet[, "Albedo"] <- db.bamet[, "SwOut"] / db.bamet[, "SwIn"]
  }

  if (year == 2019) {
    # update of sensor software at 2019-08-28 17:24
    # http://sparcwiki.awi-potsdam.de/doku.php?id = observatory:nyalesund:stations:bamet2009:bamet2009log
    # Index of values to be changed
    ind <- 1:which(dates == "2019-08-28 17:00")#which(db.bamet[, "UTC"] ==  "2019-08-28 17:00")

    ### correction of raw values with multiplier values
    # short-wave
    db.bamet[ind, "SwIn"] <- (db.bamet[ind, "SwIn_raw"] * 1000) / 14.61
    db.bamet[ind, "SwOut"] <- (db.bamet[ind, "SwOut_raw"] * 1000) / 11.09
    # long-wave
    db.bamet[ind, "LwIn_rawcor"] <- (db.bamet[ind, "LwIn_raw"] * 1000) / 14.58
    db.bamet[ind, "LwOut_rawcor"] <- (db.bamet[ind, "LwOut_raw"] * 1000) / 14.30

    ### correction of long-wave radiation with temperature of panel
    db.bamet[ind, "LwIn"] <- db.bamet[ind, "LwIn_rawcor"]  + 5.67 * 10^(-8) * (db.bamet[ind, "Tpan_NR1"] + 273.15)^4
    db.bamet[ind, "LwOut"] <- db.bamet[ind, "LwOut_rawcor"] + 5.67 * 10^(-8) * (db.bamet[ind, "Tpan_NR1"] + 273.15)^4

    # correction of SwNet and LwNet
    db.bamet[ind, "SwNet"] <- db.bamet[ind, "SwIn"] - db.bamet[ind, "SwOut"]
    db.bamet[ind, "LwNet"] <- db.bamet[ind, "LwIn"] - db.bamet[ind, "LwOut"]

    # Albedo
    db.bamet[ind, "Albedo"] <- db.bamet[ind, "SwOut"] / db.bamet[ind, "SwIn"]
  }

  ###.........................................................................
  # add new column RadNet netto radiation
  # calculation of RadNet analogue to Pangaea-dataset
  db.bamet <- cbind(db.bamet, round((db.bamet[, "SwIn"] + db.bamet[, "LwIn"]) - (db.bamet[, "SwOut"] + db.bamet[, "LwOut"]), 3))
  colnames(db.bamet)[44] <- "RadNet"
  ###.........................................................................
  ## format dates
  db.bamet[, 1] <- format( as.POSIXct(db.bamet[, 1], origin = origin, tz = "UTC"), format = '%Y-%m-%d %H:%M')
  db.bamet[(as.character(db.bamet[, 43]) == "inf"), 43] <- NA

  # re-arrange columns
  db.bamet <- db.bamet[, c("UTC", "batt_U", "Tair_4", "Tair_20", "Tair_100", "Tair_200", "RH_200",
                        "wind_v_200", "wind_deg_200", "wind_sddeg_200",
                        "wind_vmax_200", "wind_vmin_200", "prec",
                        "distraw", "snowsq", "distcor", "Dsn",

                        "Ts_252_2", "Ts_252_12", "Ts_252_32", "Ts_252_62", "Ts_252_102", "Ts_252_152",
                        "Ts_203_2", "Ts_203_5", "Ts_203_23", "Ts_203_53", "Ts_203_93", "Ts_203_143",
                        "SwIn", "SwOut", "LwIn_rawcor", "LwOut_rawcor",
                        "LwIn", "LwOut", "SwIn_raw", "SwOut_raw", "LwIn_raw", "LwOut_raw",
                        "SwNet", "LwNet", "Albedo", "RadNet", "Tpan_NR1")]
#  db.bamet[, 1] <- format( as.POSIXct(db.bamet[, 1], origin = origin, tz = "UTC"), format = '%Y-%m-%d %H:%M')

  ###.........................................................................
  ## write tables ----
  #if (file.exists(paste(out.path, "data", sep = "")) ==  FALSE) { dir.create(paste(out.path, "data/", sep = "")) }
  # write.table(db.bamet[as.numeric(format(as.POSIXct(db.bamet[, 1], format = '%Y-%m-%d %H:%M', origin = origin, tz = "UTC"), format = '%Y')) == year, ],
  #             paste0(paste(p.1$w[p.1$n == "LV0.p"]) , "BaMet2009/00_full_dataset/BaMet2009_", year, ".dat"), quote = F, dec = ".", sep = ",", row.names = F)

  write.table(db.bamet, paste0(paste(p.1$w[p.1$n == "LV0.p"]), "BaMet2009/00_full_dataset/BaMet2009_", year, "_lv0.dat"),
              quote = F, dec = ".", sep = ",", row.names = F)
  write.table(db.bamet[, c(1, 14:17)], paste0(paste(p.1$w[p.1$n == "LV0.p"]), "BaMet2009/05_snow_depth/BaMet2009_Dsn_", year, "_lv0.dat"),
              quote = F, dec = ".", sep = ",", row.names = F)
  cat("#\n# BaMet2009 ", year, " without problems!\n#\n")

} # end loop over years

