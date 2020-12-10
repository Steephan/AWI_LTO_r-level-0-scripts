#############################################################################
##
##   RAW to Level0
##
##   equal time steps, no gaps, (table flag, not implemented)
##
##   by: stephan.lange@awi.de
##   last modified: 2020-04-14
##
#############################################################################
##
## last modifications:
##  2020-10-09 CL origin <- "01-01-1970" removed because it is not used and might create confusion with other scripts in the sequence of Bayelva_MAIN.R
##  2020-04-14 CL change loop to (year in run.year) to allow the selection of the processed year in Bayelva_MAIN.R
#############################################################################
# to run this script separately, you have to uncomment the next 10 lines!
# rm(list = ls())
# if (.Platform$OS.type  ==  "windows") {
#   path <- read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt", sep = "\t", header = T)
#   maint <- read.table("N:/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)
#   p.1 <- read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt", sep = "\t", header = T)
#   p.1maint <- read.table("N:/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)
#
#   source("N:/sparc/LTO/R_database/database_R/settings/db_func.R")
# } else {
#   path <- read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
#   maint <- read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)
#   p.1 <- read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
#   p.1maint <- read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)
#
#   source("/sparc/LTO/R_database/database_R/settings/db_func.R")
# }
#############################################################################


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

  db.now <- matrix(ncol = 11, nrow = length(seq(start.date, end.date, by = "hour")), -999)
  compl.temp <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "hour")))
  compl.temp[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "hour"), format = '%Y-%m-%d %H:%M:%S', tz = "UTC"))
  colnames(compl.temp) <- c("V1", "leer")
  colnames(db.now) <- c("V1", "channel01", "channel02", "channel03", "channel04",
                      "channel05", "channel06", "channel07", "channel08", "channel09", "channel10")

  db.now[, c(2:11)] <- NA

  #files.zip  <- dir(paste0(path$w[path$n == "RAW.p"], "/BaHole2015/04_txt_export/"), pattern = glob2rx("*.zip"))
  files.zip <- list.dirs(paste0(path$w[path$n == "RAW.p"], "/BaHole2015/04_txt_export/"), full.names = FALSE)[-1]

  #files.temp  <- dir(paste0(path$w[path$n == "BaHo.onl.p"], "/realtime_rsk/"), pattern = glob2rx("*realtime.rsk"))
  for (i in 1:length(files.zip)) {
    data.temp <- read.table(paste0(path$w[path$n == "RAW.p"], "/BaHole2015/04_txt_export/", files.zip[i], "/", files.zip[i], "_data.txt"), sep = ",", header = T)
    colnames(data.temp) <- c("V1", "channel01", "channel02", "channel03", "channel04",
                           "channel05", "channel06", "channel07", "channel08", "channel09", "channel10")
    data.temp[, 1] <- as.numeric(as.POSIXct(data.temp[, 1], format = '%Y-%m-%d %H:%M:%S.000', tz = "UTC"))
    newdf.a <- merge(compl.temp, data.temp, all.x = T, by = "V1")
      for (paff in 2:11) {
        db.now[, paff] <- round(rowMeans(cbind(db.now[, paff], newdf.a[, paff + 1]), na.rm = T), 3)
      }
    }



  # if(year == 2015) {#import minute_based file
  #   min.raw  <- dir(paste0(path$w[path$n == "RAW.p"], "/BaHole2015/01_minute_based/"), pattern = glob2rx("*.rsk"))
  #   db <- dbConnect(SQLite(), dbname = paste0(paste0(path$w[path$n == "RAW.p"], "/BaHole2015/01_minute_based/"), min.raw))
  #   start.tach <- as.numeric(as.character("20150904"))
  #   start.zeit <- as.numeric(as.character("1313"))
  #   data.temp <- matrix(ncol = 11, nrow = length(dbReadTable(db, "data")[, 1]), -999)
  #   for (piff in 1:11) {  data.temp[, piff] <- dbReadTable(db, "data")[, piff]}
  #   data.temp[, 1] <- seq(as.numeric(as.POSIXct(paste0(start.tach, start.zeit), format = '%Y%m%d%H%M', tz = "UTC")), by = 60,
  #                      length.out = length(dbReadTable(db, "data")[, 1]))
  #   colnames(data.temp) <- c("V1", "channel01", "channel02", "channel03", "channel04",
  #                          "channel05", "channel06", "channel07", "channel08", "channel09", "channel10")
  #
  #   newdf.a <- merge(compl.temp, data.temp, all.x = T, by = "V1")
  #   for (paff in 2:11) {
  #     db.now[, paff] <- round(rowMeans(cbind(db.now[, paff], newdf.a[, paff+1]), na.rm = T), 3)
  #   }
  # }



  # files.raw  <- dir(paste0(path$w[path$n == "RAW.p"], "/BaHole2015/02_30_minute_based/"), pattern = glob2rx("*.rsk"))
  # files.temp  <- dir(paste0(path$w[path$n == "BaHo.onl.p"], "/realtime_rsk/"), pattern = glob2rx("*realtime.rsk"))
  # if(year == 2015) {#import minute_based file
  #   min.raw  <- dir(paste0(path$w[path$n == "RAW.p"], "/BaHole2015/01_minute_based/"), pattern = glob2rx("*.rsk"))
  #   db <- dbConnect(SQLite(), dbname = paste0(paste0(path$w[path$n == "RAW.p"], "/BaHole2015/01_minute_based/"), min.raw))
  #   start.tach <- as.numeric(as.character("20150904"))
  #   start.zeit <- as.numeric(as.character("1313"))
  #   data.temp <- matrix(ncol = 11, nrow = length(dbReadTable(db, "data")[, 1]), -999)
  #   for (piff in 1:11) {  data.temp[, piff] <- dbReadTable(db, "data")[, piff]}
  #   data.temp[, 1] <- seq(as.numeric(as.POSIXct(paste0(start.tach, start.zeit), format = '%Y%m%d%H%M', tz = "UTC")), by = 60,
  #                      length.out = length(dbReadTable(db, "data")[, 1]))
  #   colnames(data.temp) <- c("V1", "channel01", "channel02", "channel03", "channel04",
  #                          "channel05", "channel06", "channel07", "channel08", "channel09", "channel10")
  #
  #   newdf.a <- merge(compl.temp, data.temp, all.x = T, by = "V1")
  #   for (paff in 2:11) {
  #     db.now[, paff] <- round(rowMeans(cbind(db.now[, paff], newdf.a[, paff+1]), na.rm = T), 3)
  #   }
  # }

  # for (lola in (1:length(files.raw))) {
  #   db <- dbConnect(SQLite(), dbname = paste0(paste0(path$w[path$n == "RAW.p"], "/BaHole2015/02_30_minute_based/"), files.raw[lola]))
  #
  #   end.tach <- as.numeric(substr(as.character(files.raw[lola]), 8, 15))
  #   end.zeit <- as.numeric(substr(as.character(files.raw[lola]), 17, 18))
  #
  #   data.temp <- matrix(ncol = 11, nrow = length(dbReadTable(db, "data")[, 1]), -999)
  #   if(length(dbReadTable(db, "data")[1, ]) == 11) {
  #     for (piff in 1:11) {  data.temp[, piff] <- dbReadTable(db, "data")[, piff]}
  #   }else{
  #     for (piff in 1:11) {  data.temp[, piff] <- dbReadTable(db, "data")[, piff+1]}
  #   }
  #
  #   data.temp[, 1] <- seq(as.numeric(as.POSIXct(paste0(end.tach, end.zeit), format = '%Y%m%d%H', tz = "UTC")), by = 60*60,
  #                      length.out = length(dbReadTable(db, "data")[, 1]))-length(dbReadTable(db, "data")[, 1])*60*60
  #
  #   colnames(data.temp) <- c("V1", "channel01", "channel02", "channel03", "channel04",
  #                       "channel05", "channel06", "channel07", "channel08", "channel09", "channel10")
  #
  #    newdf.a <- merge(compl.temp, data.temp, all.x = T, by = "V1")
  #   for (paff in 2:11) {
  #     db.now[, paff] <- round(rowMeans(cbind(db.now[, paff], newdf.a[, paff+1]), na.rm = T), 3)
  #   }
  # }
  # if(year == 2017) {#import 5-minute_based file
  #   min.raw  <- dir(paste0(path$w[path$n == "RAW.p"], "/BaHole2015/01_five_minute_based/"), pattern = glob2rx("*.rsk"))
  #   for (lola in (1:length(min.raw))) {
  #   db <- dbConnect(SQLite(), dbname = paste0(paste0(path$w[path$n == "RAW.p"], "/BaHole2015/01_five_minute_based/"), min.raw[lola]))
  #   start.tach <- as.numeric(as.character("20150904"))
  #   start.zeit <- as.numeric(as.character("1313"))
  #
  #   data.temp <- matrix(ncol = 11, nrow = length(dbReadTable(db, "data")[, 1]), -999)
  #   for (piff in 1:11) {  data.temp[, piff] <- dbReadTable(db, "data")[, piff]}
  #   as.Date(dbGetQuery(db, "SELECT tstamp FROM data"))
  #   # data.temp[, 1] <- seq(as.numeric(as.POSIXct(paste0(start.tach, start.zeit), format = '%Y%m%d%H%M', tz = "UTC")), by = 60,
  #   #                    length.out = length(dbReadTable(db, "data")[, 1]))
  #   colnames(data.temp) <- c("V1", "channel01", "channel02", "channel03", "channel04",
  #                          "channel05", "channel06", "channel07", "channel08", "channel09", "channel10")
  #
  #   newdf.a <- merge(compl.temp, data.temp, all.x = T, by = "V1")
  #   for (paff in 2:11) {
  #     db.now[, paff] <- round(rowMeans(cbind(db.now[, paff], newdf.a[, paff+1]), na.rm = T), 3)
  #   }
  # }

  #length(files.temp)



  for (kl in 2:ncol(db.now)) {
    db.now[is.nan(as.numeric(db.now[, kl])), kl] <- NA
  }
  # rename and sort from low to high
  colnames(db.now) <- c("UTC", "Ts_900", "Ts_750", "Ts_550", "Ts_350", "Ts_250", "Ts_200", "Ts_150", "Ts_100", "Ts_50", "Ts_0")
  db.now <- db.now[, c("UTC", rev(c("Ts_900", "Ts_750", "Ts_550", "Ts_350", "Ts_250", "Ts_200", "Ts_150", "Ts_100", "Ts_50", "Ts_0")))]


  db.now[, 1] <- format(as.POSIXct(seq(start.date, end.date, by = "hour"), format = '%Y-%m-%d %H:%M:%S', tz = "UTC"), format = '%Y-%m-%d %H:%M')

  write.table(db.now, paste0(paste0(path$w[path$n == "LV0.p"]) , "BaHole2015/00_full_dataset/BaHole2015_", year, "_lv0.dat"),
              quote = F, dec = ".", sep = ",", row.names = F)

  cat("#\n# level0 BaHole2015: ", year, "without problems!\n#\n")
}
















#     #############################################################################################################
#     ##
#     ##  next part was a combination of Bahole2009
#     ##
#     #############################################################################################################

# if(year> = 2016) {
#   old.style  <- dir(paste0(path$w[path$n == "RAW.p"], "/BaHole2015"), pattern = glob2rx("*.txt"))
#   for (i in 1:length(old.style)) {
#   dada <- read.table(paste0(path$w[path$n == "RAW.p"], "BaHole2015/", old.style[i]), sep = "\t", dec = ".", header = F, skip = 2, col.names = paste0("V", seq_len(13)), fill = TRUE)
#   #print(paste(dada[1, 2], "     to     ", dada[length(dada[, 1]), 2]))
#   dada[, 2] <- as.numeric(as.POSIXct(dada[, 2], format = '%d.%m.%Y %H:%M:%S', origin = origin, tz = "UTC"))
#   dada[, 2] <- round(dada[, 2], -2)
#   #dada[, 1:13] <- data.matrix(dada[, 1:13], rownames.force = NA)
#   dada[, 1:13] <- sapply(dada[, 1:13], as.num)
#   colnames(dada) <- c("nr", "V1", "Tair_50", "Ts_50", "Ts_100", "Ts_0", "Ts_150", "Ts_250", "Ts_350", "Ts_550", "Ts_750", "Ts_900", "HK-Bat_V")
#   #print(paste(dada[1, 2], "     to     ", dada[length(dada[, 1]), 2]))
#
#   newdf.a <- merge(compl.temp, dada, all.x = T, by = "V1")
#   colnames(db.now) <- c("UTC", "Ts_900", "Ts_750", "Ts_550", "Ts_350", "Ts_250", "Ts_200", "Ts_150", "Ts_100", "Ts_50", "Ts_0")
#   #db.now[, 11] <- rowMeans(cbind(db.now[, 11], newdf.a[, 4]) , na.rm = T)# Tair_50
#   db.now[, 11] <- rowMeans(cbind(db.now[, 11], newdf.a[, 7]) , na.rm = T)# Ts_0      !!!!!!!!!!!
#   db.now[, 10] <- rowMeans(cbind(db.now[, 10], newdf.a[, 5]) , na.rm = T)# Ts_50     !!!!!!!!!!!   wrong order
#   db.now[, 9]  <- rowMeans(cbind(db.now[, 9], newdf.a[, 6]) , na.rm = T)# Ts_100    !!!!!!!!!!!
#   db.now[, 8]  <- rowMeans(cbind(db.now[, 8], newdf.a[, 8]) , na.rm = T)# Ts_150
#   db.now[, 6]  <- rowMeans(cbind(db.now[, 6], newdf.a[, 9]) , na.rm = T)# Ts_250
#   db.now[, 5]  <- rowMeans(cbind(db.now[, 5], newdf.a[, 10]), na.rm = T)# Ts_350
#   db.now[, 4]  <- rowMeans(cbind(db.now[, 4], newdf.a[, 11]), na.rm = T)# Ts_550
#   db.now[, 3]  <- rowMeans(cbind(db.now[, 3], newdf.a[, 12]), na.rm = T)# Ts_750
#   db.now[, 2]  <- rowMeans(cbind(db.now[, 2], newdf.a[, 13]), na.rm = T)# Ts_900
#  # db.now[, 12] <- rowMeans(cbind(db.now[, 12], newdf.a[, 14]), na.rm = T)# HK-Bat_V
# }
# }
#colnames(db.now) <- c("UTC", "Tair_50", "Ts_0", "Ts_50", "Ts_100", "Ts_150", "Ts_250", "Ts_350", "Ts_550", "Ts_750", "Ts_900", "HK-Bat_V")



# ### not neccesary at this moment
# if(year == 2016 & length(files.temp)>0) {# import the online files
#   for (lola in (1:length(files.temp))) {
#     db <- dbConnect(SQLite(), dbname = paste0(paste0(path$w[path$n == "BaHo.onl.p"], "/realtime_rsk/"), files.temp[lola]))
#     #############################################################################################################
#     ##
#     ##  difference to raw-files in name as date
#     ##
#     ##  starting time is in the name!!!!!!
#     ##
#     #############################################################################################################
#       start.tach <- as.numeric(substr(as.character(files.temp[lola]), 7, 14))
#       start.zeit <- as.numeric(substr(as.character(files.temp[lola]), 16, 17))+1
#
#
#     data.temp <- matrix(ncol = 11, nrow = length(dbReadTable(db, "data")[, 1]), -999)
#     for (piff in 1:11) {  data.temp[, piff] <- dbReadTable(db, "data")[, piff]}
#     data.temp[, 1] <- seq(as.numeric(as.POSIXct(paste0(start.tach, start.zeit), format = '%Y%m%d%H', tz = "UTC")), by = 60*60,
#                        length.out = length(dbReadTable(db, "data")[, 1]))
#
#     colnames(data.temp) <- c("V1", "channel01", "channel02", "channel03", "channel04",
#                            "channel05", "channel06", "channel07", "channel08", "channel09", "channel10")
#
#     newdf.a <- merge(compl.temp, data.temp, all.x = T, by = "V1")
#     for (paff in 2:11) {
#       db.now[, paff] <- round(rowMeans(cbind(db.now[, paff], newdf.a[, paff+1]), na.rm = T), 3)
#     }
#   }
# }






