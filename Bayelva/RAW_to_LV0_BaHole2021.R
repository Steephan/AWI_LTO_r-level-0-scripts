###.........................................................................
##
##   BaHole2021        RAW to Level0 ----
##
##   equal time steps, no gaps
##
##   by: Stephan.Lange@awi.de
##   last modified: 2021-05-12
##
###.........................................................................
## last modifications: ----
##  2021-05-12 SL adapted to runner app and content management
##
###.........................................................................
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
###.........................................................................

## Input header
## "TOA5","CR300-BaHole2021","CR300","9125","CR310.Std.10.02","CPU:BaHole2021_v202102_2_1h.CR300","35031","Data_temp_cal"
## "TIMESTAMP","RECORD","BattV_Min","PTemp_C_Avg","Ts_0_Avg","Ts_50_Avg","Ts_100_Avg","Ts_150_Avg","Ts_200_Avg","Ts_250_Avg","Ts_350_Avg","Ts_550_Avg","Ts_750_Avg","Ts_900_Avg"
## "TS","RN","Volts","Deg C","Deg C","Deg C","Deg C","Deg C","Deg C","Deg C","Deg C","Deg C","Deg C","Deg C"
## "","","Min","Avg","Avg","Avg","Avg","Avg","Avg","Avg","Avg","Avg","Avg","Avg"


#tag <- format(Sys.Date()-1, format = '%d')
#tagdavor <- format(Sys.Date()-2, format = '%d')
#monat <- format(Sys.Date(), format = '%m')
#jahr <- format(Sys.Date(), format = '%Y')
# recent.year <- as.numeric(format(Sys.Date(), "%Y"))

###.........................................................................
# to run this script separately, you have to set run.year:
#
# recent.year <- as.numeric(format(Sys.Date(), "%Y"))
# run.year <- recent.year
# run.year <- 2021
###.........................................................................
## loop over years ----
for (year in run.year) {
  start.date <- as.POSIXct(paste(year, "-01-01 00:00:00", sep = ""), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date <- as.POSIXct(paste(year, "-", 12, "-", 31, " 23:00:00", sep = ""), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  #end.date    <- as.POSIXct(paste(year, "-", monat, "-", tag, " 23:00:00", sep = ""), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")

  db.bahole <- matrix(ncol = 13, nrow = length(seq(start.date, end.date, by = "hour")), -999)
  compl.temp <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "hour")))
  compl.temp[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "hour"), format = '%Y-%m-%d %H:%M:%S', tz = "UTC"))
  colnames(compl.temp) <- c("UTC", "leer")

  db.bahole[, c(2:13)] <- NA
  files2read <- list.files(paste0(p.1$w[p.1$n == "ONL.p"], "BaHole2021/"), pattern = "*cal*")

  for (i in 1:length(files2read)) {
    dada <- read.table(paste0(p.1$w[p.1$n == "ONL.p"], "BaHole2021/", files2read[i], sep = ""),
                       sep = ",", dec = ".", header = F, skip = 4, col.names = paste0("V", seq_len(14)), fill = TRUE)
    dada[, 2] <- as.numeric(as.POSIXct(dada[, 1], format = '%Y-%m-%d %H:%M:%S', origin = origin, tz = "UTC"))
    #dada[, 2] <- round(dada[, 2], -2) # not neccessary
    dada[, 2:14] <- sapply(dada[, 2:14], as.num)
    colnames(dada) <- c( "TIMESTAMP","UTC","BattV_Min","PTemp_C_Avg","Ts_0_Avg","Ts_50_Avg","Ts_100_Avg","Ts_150_Avg","Ts_200_Avg","Ts_250_Avg","Ts_350_Avg","Ts_550_Avg","Ts_750_Avg","Ts_900_Avg")

    newdf.a <- merge(compl.temp, dada, all.x = T, by = "UTC")

    db.bahole[, 2] <- rowMeans(cbind(db.bahole[, 2], newdf.a[, 5]), na.rm = T)# Tpan
    db.bahole[, 3] <- rowMeans(cbind(db.bahole[, 3], newdf.a[, 6]), na.rm = T)# Ts_0       
    db.bahole[, 4] <- rowMeans(cbind(db.bahole[, 4], newdf.a[, 7]), na.rm = T)# Ts_50      
    db.bahole[, 5] <- rowMeans(cbind(db.bahole[, 5], newdf.a[, 8]), na.rm = T)# Ts_100       
    db.bahole[, 6] <- rowMeans(cbind(db.bahole[, 6], newdf.a[, 9]), na.rm = T)# Ts_150
    db.bahole[, 7] <- rowMeans(cbind(db.bahole[, 7], newdf.a[, 10]), na.rm = T)# Ts_200
    db.bahole[, 8] <- rowMeans(cbind(db.bahole[, 8], newdf.a[, 11]), na.rm = T)# Ts_250
    db.bahole[, 9] <- rowMeans(cbind(db.bahole[, 9], newdf.a[, 12]), na.rm = T)# Ts_350
    db.bahole[, 10] <- rowMeans(cbind(db.bahole[, 10], newdf.a[, 13]), na.rm = T)# Ts_550
    db.bahole[, 11] <- rowMeans(cbind(db.bahole[, 11], newdf.a[, 14]), na.rm = T)# Ts_750
    db.bahole[, 12] <- rowMeans(cbind(db.bahole[, 12], newdf.a[, 15]), na.rm = T)# Ts_900
    db.bahole[, 13] <- rowMeans(cbind(db.bahole[, 13], newdf.a[, 4]), na.rm = T)# HK-Bat_V
  }
  colnames(db.bahole) <- c("UTC", "Tpan", "Ts_0", "Ts_50", "Ts_100",  "Ts_150", "Ts_200", "Ts_250", "Ts_350", "Ts_550", "Ts_750", "Ts_900", "batt_U_min")
  db.bahole[, 1] <- format(as.POSIXct(seq(start.date, end.date, by = "hour"), format = '%Y-%m-%d %H:%M:%S', tz = "UTC"), format = '%Y-%m-%d %H:%M')

  ## NA problem
  for (kl in 2:ncol(db.bahole)) {
    db.bahole[is.nan(as.numeric(db.bahole[, kl])), kl] <- NA
  }


  write.table(db.bahole, paste0(p.1$w[p.1$n == "LV0.p"] , "BaHole2021/00_full_dataset/BaHole2021_", year, "_lv0.dat", sep = ""),
              quote = F, dec = ".", sep = ",", row.names = F)
  cat("#\n# level0 BaHole2021: ", year, "without problems!\n#\n")
}



