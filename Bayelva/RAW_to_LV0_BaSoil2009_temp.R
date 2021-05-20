###..........................................................................
##
##   BaSoil2009      RAW to Level0 ----
##
##   equal time steps, no gaps, (table flag, not implemented)
##
##   by: stephan.lange@awi.de
##   last modified: 2020-04-14
##
##   last check: 2020-04-14
##   checked by: christian.lehr@awi.de
##
###..........................................................................
##
## open issues: ----
## - merge with tdr file
###..........................................................................
##
## last modifications: ----
##  2021-05-12 SL adapted to runnerapp and content management
##  2020-10-09 CL origin <- "01-01-1970" removed because it is not used and might create confusion with other scripts in the sequence of Bayelva_MAIN.R
##  2020-04-14 CL change loop to (year in run.year) to allow the selection of the processed year in Bayelva_MAIN.R
##
###..........................................................................
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
###..........................................................................


###..........................................................................
# to run this script separately, you have to set run.year:
#
# recent.year <- as.numeric(format(Sys.Date(), "%Y"))
# run.year <- recent.year
# run.year <- 2020
###..........................................................................

## loop over years ----
for (year in run.year) {
  #print(paste("running", year, "of BaSoil2009-temperature" ))
  start.date <- as.POSIXct(paste0(year, "-01-01 00:00:00"), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date <- as.POSIXct(paste0(year, "-", 12, "-", 31, " 23:00:00"), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  #end.date    <- as.POSIXct(paste(year, "-", monat, "-", tag, " 23:00:00", sep = ""), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  
  db.now <- matrix(ncol = 12, nrow = length(seq(start.date, end.date, by = "hour")), -999)
  compl.temp <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "hour")))
  compl.temp[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "hour"), format = '%Y-%m-%d %H:%M:%S', tz = "UTC"))
  colnames(compl.temp) <- c("V1", "leer")
  colnames(db.now) <- c("V1", "Panel_temp_Avg", "batt_volt_Avg", "T107_1_Avg", "T107_2_Avg",
                        "T107_3_Avg", "T107_4_Avg", "T107_5_Avg", "T107_6_Avg", "T107_7_Avg", "T107_8_Avg", "TableFlag")
  
  db.now[, c(2:11)] <- NA
  db.now[, 12] <- 0
  files.temp <- dir(paste0(p.1$w[p.1$n == "BaS.raw3.p"]), pattern = glob2rx("*.dat"))
  
  if (year >= 2015) {
    files.temp <- c(files.temp, dir(paste0(p.1$w[p.1$n == "BaS.onl.p"]), pattern = glob2rx("*TempOnline.dat")))
  }
  
  for (lola in 1:length(files.temp)) {
    inz.path <- paste0(p.1$w[p.1$n == "BaS.raw3.p"])
    if (year >= 2015 && lola == length(files.temp)) {
      inz.path <- paste0(p.1$w[p.1$n == "BaS.onl.p"])
    }
    
    data.temp <- read.table(paste(inz.path, files.temp[lola], sep = ""),
                            header = F, skip = 4, dec = ".", sep = ",", na = "NAN")[, -2]
    data.temp[, 1] <- as.numeric(as.POSIXct(data.temp[, 1], format = '%Y-%m-%d %H:%M:%S', tz = "UTC"))
    colnames(data.temp) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11")
    
    newdf.a <- merge(compl.temp, data.temp, all.x = T, by = "V1")
    newdf.a <- check.double.entry(newdf.a)
    db.now[, 2] <- round(rowMeans(cbind(db.now[, 2], newdf.a[, 3]), na.rm = T), 3)
    db.now[, 3] <- round(rowMeans(cbind(db.now[, 3], newdf.a[, 4]), na.rm = T), 3)
    db.now[, 4] <- round(rowMeans(cbind(db.now[, 4], newdf.a[, 5]), na.rm = T), 3)
    db.now[, 5] <- round(rowMeans(cbind(db.now[, 5], newdf.a[, 6]), na.rm = T), 3)
    db.now[, 6] <- round(rowMeans(cbind(db.now[, 6], newdf.a[, 7]), na.rm = T), 3)
    db.now[, 7] <- round(rowMeans(cbind(db.now[, 7], newdf.a[, 8]), na.rm = T), 3)
    db.now[, 8] <- round(rowMeans(cbind(db.now[, 8], newdf.a[, 9]), na.rm = T), 3)
    db.now[, 9] <- round(rowMeans(cbind(db.now[, 9], newdf.a[, 10]), na.rm = T), 3)
    db.now[, 10] <- round(rowMeans(cbind(db.now[, 10], newdf.a[, 11]), na.rm = T), 3)
    db.now[, 11] <- round(rowMeans(cbind(db.now[, 11], newdf.a[, 12]), na.rm = T), 3)
  }
  rm(inz.path)
  
  ## NA problem ----
  for (kl in 2:ncol(db.now)) {
    db.now[is.nan(as.numeric(db.now[, kl])), kl] <- NA
  }
  # input names ----
  # colnames(db.now) <- c("UTC", "Panel_temp_Avg", "batt_volt_Avg", "T107_1_Avg", "T107_2_Avg",
  #                     "T107_3_Avg", "T107_4_Avg", "T107_5_Avg", "T107_6_Avg", "T107_7_Avg", "T107_8_Avg", "TableFlag")
  # output names ----
  colnames(db.now) <- c("UTC", "Tpan", "batt_U", rev(c("Ts_1", "Ts_11", "Ts_21", "Ts_37",
                                                       "Ts_55", "Ts_71", "Ts_89")), "Ts_141", "TableFlag")
  # reorder of columns ----
  db.now <- db.now[, c(1, 2, 3, rev(4:10), 11, 12)]
  # colnames(db.now) <- c("UTC", "Tpan", "batt_U", "Ts_1", "Ts_11", "Ts_21", "Ts_37",
  #                     "Ts_55", "Ts_71", "Ts_89", "Ts_141", "TableFlag")
  db.now[, 1] <- format(as.POSIXct(seq(start.date, end.date, by = "hour"), format = '%Y-%m-%d %H:%M:%S', tz = "UTC"), format = '%Y-%m-%d %H:%M')
  # write data ----
  write.table(db.now, paste0(paste0(p.1$w[p.1$n == "LV0.p"]) , "BaSoil2009/01_temperature/BaSoil2009_Ts_", year, "_lv0.dat"),
              quote = F, dec = ".", sep = ",", row.names = F)
  
  cat("#\n# BaSoil2009_temp ", year, "without problems!\n#\n")
}

