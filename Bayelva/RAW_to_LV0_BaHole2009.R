#############################################################################
##
##   BaHole2009        RAW to Level0
##
##   equal time steps, no gaps
##
##   by: Stephan.Lange@awi.de
##   last modified: 2020-04-14
##
#############################################################################
## last modifications:
##  2020-10-09 CL origin <- "01-01-1970" removed because it is not used and might create confusion with other scripts in the sequence of Bayelva_MAIN.R
##  2020-04-14 CL change loop to (year in run.year) to allow the selection of the processed year in Bayelva_MAIN.R
##
#############################################################################
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
#############################################################################

## Input header
#M-Log1 #A50282 - Incremental Data (Pos: 0) (Parameter Set: 'A50282_0307_240509.par')
#Nr  Time	#3;Dig.Temp;oC	#4;Dig.Temp;oC	#5;Dig.Temp;oC	#6;Dig.Temp;oC	#7;Dig.Temp;oC	#8;Dig.Temp;oC	#9;Dig.Temp;oC	#10;Dig.Temp;oC	#11;Dig.Temp;oC	#12;Dig.Temp;oC	#HK-Bat;V

#############################################################################
#
#  Attention!!!
#
#  wrong order of temperature sensors
#  see line 115-117
#
#############################################################################

# if you want to check of multiple values per date
# set check.inconsitense to 1
# (it takes time >> to reduce set line 66 to just one variable)
as.num <- function(x){as.numeric(as.character(x))}

######################################################

#tag <- format(Sys.Date()-1, format = '%d')
#tagdavor <- format(Sys.Date()-2, format = '%d')
#monat <- format(Sys.Date(), format = '%m')
#jahr <- format(Sys.Date(), format = '%Y')
recent.year <- as.numeric(format(Sys.Date(), "%Y"))

########
# to run this script separately, you have to set run.year:
#
# recent.year <- as.numeric(format(Sys.Date(), "%Y"))
# run.year <- recent.year
# run.year <- 2020
#######

for (year in run.year) {
  start.date <- as.POSIXct(paste(year, "-01-01 00:00:00", sep = ""), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date <- as.POSIXct(paste(year, "-", 12, "-", 31, " 23:00:00", sep = ""), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  #end.date    <- as.POSIXct(paste(year, "-", monat, "-", tag, " 23:00:00", sep = ""), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")

  db.bahole <- matrix(ncol = 12, nrow = length(seq(start.date, end.date, by = "hour")), -999)
  compl.temp <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "hour")))
  compl.temp[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "hour"), format = '%Y-%m-%d %H:%M:%S', tz = "UTC"))
  colnames(compl.temp) <- c("UTC", "leer")

  db.bahole[, c(2:12)] <- NA
  files2read <- list.files(paste0(p.1$w[p.1$n == "RAW.p"], "/BaHole2009/"), pattern = "*.dat")

  for (i in 1:length(files2read)) {
    dada <- read.table(paste0(p.1$w[p.1$n == "RAW.p"], "/BaHole2009/", files2read[i], sep = ""),
                       sep = "\t", dec = ".", header = F, skip = 2, col.names = paste0("V", seq_len(13)), fill = TRUE)
    dada[, 2] <- as.numeric(as.POSIXct(dada[, 2], format = '%d.%m.%Y %H:%M:%S', origin = origin, tz = "UTC"))
    dada[, 2] <- round(dada[, 2], -2)
    #dada[, 1:13] <- data.matrix(dada[, 1:13], rownames.force = NA)
    dada[, 1:13] <- sapply(dada[, 1:13], as.num)
    colnames(dada) <- c("nr", "UTC", "Tair_50", "Ts_0", "Ts_50", "Ts_100",  "Ts_150", "Ts_250", "Ts_350", "Ts_550", "Ts_750", "Ts_900", "HK-Bat_V")
    #print(paste(dada[1, 2], "     to     ", dada[length(dada[, 1]), 2]))

    newdf.a <- merge(compl.temp, dada, all.x = T, by = "UTC")
    ## NA problem
    # for(kl in 2:ncol(newdf.a)){
    #   newdf.a[is.nan(as.numeric(newdf.a[, kl])), kl] <- NA
    # }
    # for(kl in 2:ncol(db.bahole)){
    #   db.bahole[is.nan(as.numeric(db.bahole[, kl])), kl] <- NA
    # }
    db.bahole[, 2] <- rowMeans(cbind(db.bahole[, 2], newdf.a[, 4]), na.rm = T)# Tair_50
    db.bahole[, 3] <- rowMeans(cbind(db.bahole[, 3], newdf.a[, 7]), na.rm = T)# Ts_0       !!!!!!!!!!!
    db.bahole[, 4] <- rowMeans(cbind(db.bahole[, 4], newdf.a[, 5]), na.rm = T)# Ts_50       !!!!!!!!!!!   wrong order
    db.bahole[, 5] <- rowMeans(cbind(db.bahole[, 5], newdf.a[, 6]), na.rm = T)# Ts_100       !!!!!!!!!!!
    db.bahole[, 6] <- rowMeans(cbind(db.bahole[, 6], newdf.a[, 8]), na.rm = T)# Ts_150
    db.bahole[, 7] <- rowMeans(cbind(db.bahole[, 7], newdf.a[, 9]), na.rm = T)# Ts_250
    db.bahole[, 8] <- rowMeans(cbind(db.bahole[, 8], newdf.a[, 10]), na.rm = T)# Ts_350
    db.bahole[, 9] <- rowMeans(cbind(db.bahole[, 9], newdf.a[, 11]), na.rm = T)# Ts_550
    db.bahole[, 10] <- rowMeans(cbind(db.bahole[, 10], newdf.a[, 12]), na.rm = T)# Ts_750
    db.bahole[, 11] <- rowMeans(cbind(db.bahole[, 11], newdf.a[, 13]), na.rm = T)# Ts_900
    db.bahole[, 12] <- rowMeans(cbind(db.bahole[, 12], newdf.a[, 14]), na.rm = T)# HK-Bat_V
  }
  colnames(db.bahole) <- c("UTC", "Tair_50", "Ts_0", "Ts_50", "Ts_100",  "Ts_150", "Ts_250", "Ts_350", "Ts_550", "Ts_750", "Ts_900", "HK-Bat_V")
  db.bahole[, 1] <- format(as.POSIXct(seq(start.date, end.date, by = "hour"), format = '%Y-%m-%d %H:%M:%S', tz = "UTC"), format = '%Y-%m-%d %H:%M')

  ## NA problem
  for (kl in 2:ncol(db.bahole)) {
    db.bahole[is.nan(as.numeric(db.bahole[, kl])), kl] <- NA
  }


  write.table(db.bahole, paste0(p.1$w[p.1$n == "LV0.p"] , "BaHole2009/00_full_dataset/BaHole2009_", year, "_lv0.dat", sep = ""),
              quote = F, dec = ".", sep = ",", row.names = F)
  cat("#\n# level0 BaHole2009: ", year, "without problems!\n#\n")
}
## and update the big one
#write.table(db.bahole, paste(out.path , "BaHole2009_all.dat", sep = ""), quote = F, dec = ".", sep = ",", row.names = F)
#print(paste("Bahole2009 without problems!"))


