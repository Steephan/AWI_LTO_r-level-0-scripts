#############################################################################
##
## SaHole2010  RAW to Level0
##
## equal time steps
##
## by: Stephan.Lange@awi.de and Niko.Bornemann@awi.de
##
## last modified: 2017/11/02
##
#############################################################################

## Input header
# RBR XR-420 5.62 010312 (Windows: 5.25 - Minimum required: 5.20)

#############################################################################
#
# last modifications:
# 2021-11-01 SL reconstruction of depth by recherche and comparism to other datasets
#                    Ts1 - +50cm  - Niko cut it 50cm above ground and above next sensor
#                    Ts2 -  0cm   - next sensor at 0cm
#                    Ts3 - -50cm  - next 50 cm, compare with sasoil2002 ts_center_50
#                    Ts4 - -100cm - next 50 cm, compare with sasoil2012 ts_center_100
#                    Ts5 - -125cm - next 25 cm, then regular steps to max depth
#                    Ts6 - -150cm - next 25 cm, 
#                    Ts7 - -180cm - next 25 cm, Ts7 and 8 are more or less at same height, Ts7 is a bit deeper
#                    Ts8 - -175cm - next 25 cm, 
# 2020-07-29 CL introduced the variable n.col, harmonized the colnames of data.temp with db.sahole
# - implement new path
#
#############################################################################


### path definitions ----
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
###............................................................................

#run.year <- 2010:2018

#############################################################################

options(warn = -1)# necessary! ... there are some strange warnings
in.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaHole2010/")
out.path <- paste0(p.1$w[p.1$n == "LV0.p"], "SaHole2010/00_full_dataset/")
origin <- "1970-01-01"

# read files
files2read <- list.files(in.path, pattern = "*.csv")
for (year in run.year) {
  # define start and end date
  start.date <- as.POSIXct(paste("01.01.2010", sep = ""), format = '%d.%m.%Y', tz = "UTC")
  end.date <- as.POSIXct(paste("31.12.", format(Sys.Date(), "%Y"), sep = ""), format = '%d.%m.%Y', tz = "UTC")
  
  # set up empty matrices
  n.col <- 10
  db.sahole <- matrix(ncol = n.col, nrow = length(seq(start.date, end.date, by = "hour")))
  # compl.temp: dummy matrix for later merging
  compl.temp <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "hour")))
  
  # format first column as time column in numerical format
  db.sahole[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "hour"), format = '%Y-%m-%d %H:%M'))
  compl.temp[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "hour"), format = '%Y-%m-%d %H:%M'))
  colnames(compl.temp) <- c("UTC", "erste")
  
  
  for (kk in 1:length(files2read)) {#1:length(files2read)
    data.temp <- read.table(paste(in.path, files2read[kk], sep = ""), sep = ",", dec = ".",
                            header = F, skip = 2, col.names = paste0("V", seq_len(n.col + 1)), fill = TRUE)[, 2:(n.col + 1)]
    data.temp$V2 <- round(as.POSIXct(data.temp$V2, format = '%d.%m.%Y %H:%M:%S', origin = origin, tz = "UTC"), "hour")
    data.temp$V2 <- as.numeric(data.temp$V2)
    #data.temp[duplicated(data.temp$V2) == T, ]
    data.temp <- check.double.entry(data.temp)
    data.temp[, 1:n.col] <- sapply(data.temp[, 1:n.col], as.num)
    
    # old:
    # colnames(data.temp) <- c("UTC", "Ts_0", "Ts_1", "Ts_2", "Ts_3", "Ts_4", "Ts_5", "Ts_6", "Ts_7", "Ts_8")
    colnames(data.temp) <- c("UTC", "Ts_1", "Ts_2", "Ts_3", "Ts_4", "Ts_5", "Ts_6", "Ts_7", "Ts_8", "Ubat")
    
    newdf.a <- merge(compl.temp, data.temp, all.x = T, by = "UTC")
    for (i in 2:n.col) {
      db.sahole[, i] <- rowMeans(cbind(db.sahole[, i], newdf.a[, i + 1]), na.rm = T)
    }
  }
  
  #colnames(db.sahole) <- c("UTC", "Ts_1"   , "Ts_2", "Ts_3" , "Ts_4"  , "Ts_5"  , "Ts_6"  , "Ts_7"  , "Ts_8"  , "Ubat")
  colnames(db.sahole) <- c("UTC", "Tair_50", "Ts_0", "Ts_50", "Ts_100", "Ts_125", "Ts_150", "Ts_180", "Ts_175", "Ubat")
  db.sahole <- db.sahole[,c(1:7,9,8,10)]
  db.sahole[, 1] <- format(as.POSIXct(db.sahole[, 1], origin = origin, tz = "UTC"), format = '%Y-%m-%d %H:%M')
  
  
  write.table(db.sahole[as.numeric(format(as.POSIXct(db.sahole[, 1], format = '%Y-%m-%d %H:%M', origin = origin, tz = "UTC"), format = '%Y')) == year, ],
              paste(out.path , "SaHole2010_", year, "_lv0.dat", sep = ""),
              quote = F, dec = ".", sep = ",", row.names = F)
  cat("#\n# SaHole2010 ",year,"without problems!\n#\n") 
}




