#############################################################################
##
##   BaEddy2007       RAW to Level0
##
##   written by: stephan.lange@awi.de
##   last modified: 2018-03-20
##
##   last check: 2020-01-22
##   checked by: christian.lehr@awi.de
##
#############################################################################
##
## open issues:
##
##
##
#############################################################################
##
## last modification:
##
## - adjusted to folder structure with individual years in N:/sparc/data/LTO/raw/BaEddy2007/SlowSeq
## - for the reading of the data the option na.strings = c("NA", "NaN") was set.
##   (Though this works also without specifying the na.strings.)
##   The old version was: na = "NaN" which produced data format "factor" and therefore artefacts (==> option na = .... does not exist).
##
#############################################################################
##
## Comments:
##
## special case
## in year 2014:
## a file is written to N:/sparc/data/LTO/level0/BaEddy2007/01_2014er_BaEddy2007
## in order to merge with the data of BaEddy2014
##
#############################################################################
# to run this script separate, you have to uncomment the next 10 lines!
rm(list = ls())
if (.Platform$OS.type == "windows") {
  path <- read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt", sep = "\t", header = T)
  maint <- read.table("N:/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)
  p.1 <- read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt", sep = "\t", header = T)
  p.1maint <- read.table("N:/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)

  source("N:/sparc/LTO/R_database/database_R/settings/db_func.R")
} else {
  path <- read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
  maint <- read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)
  p.1 <- read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
  p.1maint <- read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)

  source("/sparc/LTO/R_database/database_R/settings/db_func.R")
}
#############################################################################
options(scipen = 100) # for non-exponential display of numeric values
origin  <- "1970-01-01"
aktuell <- as.numeric(format(Sys.Date(), "%Y"))

station  <- 'BaEddy2007' # Dsn at EDDY
years  <- 2007:2014


for (year in years) {

  cat("\nProcessing year", year, "\n====================\n\n")
  start.date <- as.POSIXct(paste(year, "-01-01 00:00:00", sep = ""), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date <- as.POSIXct(paste(year, "-", 12, "-", 31, " 23:00:00", sep = ""), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  # create empty data frame with UTC time stamp every 30 min
  db.basnow <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "hour")), -999)
  db.basnow[, 2] <- NA
  compl.temp <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "hour")))
  db.basnow[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "hour"), format = '%Y-%m-%d %H:%M:%S'))
  compl.temp[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "hour"), format = '%Y-%m-%d %H:%M:%S'))
  colnames(compl.temp) <- c("UTC", "erste")

  inz.path <- paste0(path$w[path$n == "RAW.p"], "BaEddy2007/SlowSeq/", year, "/")
  files2read <- list.files(inz.path, pattern = "*.dat")

  for (i in 1:length(files2read)) {#1:length(files2read)
    #cat("\nprocessing ", files2read[i], "\n ==================== \n\n")
    dada <- read.table(paste(inz.path, files2read[i], sep = ""), sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na.strings = c("NA", "NaN"))
    dada <- check.double.entry(dada)
    colnames(dada) <- c("UTC", "record", "Dsn")
    dada[, 1] <- as.numeric(as.POSIXct(dada[, 1], format = '%Y-%m-%d %H:%M:%S', origin = origin, tz = "UTC"))

    newdf.a <- merge(compl.temp, dada, all.x = T, by = "UTC")

  if (length(newdf.a[, 4]) == length(db.basnow[, 2])) {
    db.basnow[, 2] <- rowMeans(cbind(db.basnow[, 2], newdf.a[, 4]), na.rm = T)#
    } else {cat("\n====================\n==== Achtung! =====\n====================\n\n")}
  }

    db.basnow[, 1] <- format( as.POSIXct(db.basnow[, 1], origin = origin, tz = "UTC"), format = '%Y-%m-%d %H:%M')
    db.basnow <- as.data.frame(db.basnow)
    # calculation of snow height based on sensor height
    db.basnow$Dsn <- 2.4 - as.numeric(as.character(db.basnow[, 2]))

    colnames(db.basnow) <- c("UTC", "distraw", "Dsn")
    if (year == 2014) {
      # for merge with data from BaEddy2014
      write.table(db.basnow, paste0(path$w[path$n == "LV0.p"], station, "/01_2014er_BaEddy2007/", station, "_", year, "_lv0.dat"), quote = F, dec = ".", sep = ",", row.names = F)
      } else {
      write.table(db.basnow, paste0(path$w[path$n == "LV0.p"], station, "/00_full_dataset/", station, "_", year, "_lv0.dat"), quote = F, dec = ".", sep = ",", row.names = F)}

  #plot(lv0.data$Dsn)
  }











# for(t.year in run.year){
#
#   #files2read <- list.files(in.path, pattern = "*.dat")
#   #cat(t.year)
# file.name.main  <- paste0(p.1$w[p.1$n == "RAW.p"], station, "/temporal_version/", station, "_", t.year, ".dat")
#
#
#
# raw.data <- read.table(file.name.main, sep = ", ", dec = ".", header = T)[c("UTC", "Dsn")]
# start.date <- as.POSIXct(paste("01.01.", t.year, " 00:00", sep = ""), format = '%d.%m.%Y %H:%M', tz = "UTC")
# end.date <- as.POSIXct(paste("31.12.", t.year, " 23:00", sep = ""), format = '%d.%m.%Y %H:%M', tz = "UTC")
# db.basnow <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "hour")), -999)
# db.basnow[, 2] <- NA;colnames(db.basnow) <- c("UTC", "Dsn")
# db.basnow[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "hour"), format = '%Y-%m-%d %H:%M:%S'))
# # distraw = = 2.5m
# raw.data[, 2] <- 2.5-raw.data[, 2]
# raw.data[, 1] <- as.numeric(as.POSIXct(raw.data[, 1], format = '%Y-%m-%d %H:%M:%S'))
# lv0.data <- merge(db.basnow, raw.data, all.x = T, by = "UTC")[c(1, 3)]
# lv0.data[, 1] <- as.POSIXct(seq(start.date, end.date, by = "hour"), format = '%Y-%m-%d %H:%M:%S')
# colnames(lv0.data) <- c("UTC", "Dsn")
# # set time format
# lv0.data[, 1]  <- format(as.POSIXct(lv0.data[, 1], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M')
#
#
# write.table(lv0.data , paste0(p.1$w[p.1$n == "LV0.p"], station, "/00_full_dataset/", station, "_", t.year, "_lv0.dat"), quote = F, dec = ".", sep = ", ", row.names = F)
# #plot(lv0.data$Dsn)
# }


