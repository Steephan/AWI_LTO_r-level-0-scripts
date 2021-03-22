#############################################################################
##
##   BaSnow2013        RAW to Level0
##
##   Snow depth data from sensor SHM 30
##   manual: http://sparcwiki.awi-potsdam.de/lib/exe/fetch.php?media=public:sensors:jenoptik:shm30_v201508.pdf
##
##   written by: Stephan.Lange@awi.de
##               christian.lehr@awi.de
##   last modified: 2020-04-14
##
##   last check: 2020-01-22
##   checked by: christian.lehr@awi.de
##
#############################################################################
##
## open issues:
##  - include error flags from the sensor:
##    p. 41 in manual
##
##   ?? equal time steps(30min), no gaps,( table flag)-fehlt noch!! ??
##
## ??? - save constant digits (2.1000 instead of 2.1) ???
##
#############################################################################
##
## last modifications:
##  2020-10-09 CL origin <- "01-01-1970" removed because it is not used and might create confusion with other scripts in the sequence of Bayelva_MAIN.R
##  2020-10-07 CL add snowdepth correction starting at 2019-06-29 00:00
##  2020-04-14 CL change loop to (year in run.year) to allow the selection of the processed year in Bayelva_MAIN.R
##  2019-11-04 CL:
## - read only complete lines of txt.laser ... txt.laser[nchar(txt.laser, type = "bytes") == 45]
## - exclude data quality check column of data.laser matrix (former third column), colnames(data.laser) and remove one column in newdf.c
##
## ??? Start at: 2013-08-22 ????
##
## 2015-12-11 SL
## - comma-separation
## - convert "NaN" to "NA"
## - remove values with error-flag "15"
##    !!! ==> this function is currently uncommented !!!!
##
##
#############################################################################
##
## Comments:
##
##
## the raw data is formatted in a special format
## ==> section 5.2.1 sda telegram in manual (p. 29+30)
##
## example of raw data (sda telegram):
## "18.01.20 00:00:33 >+00.1721 025.855 +08 00 ¢<"
## >eee.eeee sss.sss TTT EE P< [CR] [LF]
## e ... snow depth in m for sf=1, for sf=10 the decimal point is shifted to eeee.eee
## s ... signal strength/ signal intensity,
## T ... temperature
## EE ... error code,
## P ... check byte
##
## ==> select only those lines of the txt.laser file which consist of 45 characters (==> Attention! For the format of the character string, the type "bytes" has to be used)
##
################
## ??
## Input header:
##
##  ... no header!
##
## !! ATTENTION there are some rows missing in 2015-06-13 !!
##
#############################################################################
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
#############################################################################


recent.year <- as.numeric(format(Sys.Date(), "%Y"))

########
# to run this script separately, you have to set run.year:
#
# recent.year <- as.numeric(format(Sys.Date(), "%Y"))
# run.year <- recent.year
# run.year <- 2020
#######

for (year in run.year) {

  start.date <- as.POSIXct(paste("01.01.", year, " 00:00", sep = ""), format = '%d.%m.%Y %H:%M', tz = "UTC")
  end.date <- as.POSIXct(paste("31.12.", year, " 23:30", sep = ""), format = '%d.%m.%Y %H:%M', tz = "UTC")
  db.basnow <- matrix(ncol = 3, nrow = length(seq(start.date, end.date, by = "30 min")), -999)
  db.basnow[, 2:3] <- NA
  compl.temp2 <- matrix(ncol = 3, nrow = length(seq(start.date, end.date, by = "30 min")))
  db.basnow[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = '%Y-%m-%d %H:%M:%S'))
  compl.temp2[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = '%Y-%m-%d %H:%M:%S'))
  #colnames(compl.temp)<-c("UTC","erste")

  if (year == 2013) {
    files.laser <- list.files(paste0(paste0(p.1$w[p.1$n == "BaSn.raw.p"]), year, "/"), pattern = ".dat")
    ##### nchste zeile muss aber raus!!!!!
    files.laser <- files.laser[-c(1, 2, 8)]# fragwrdige 2 Startdateien, 8 fast komplett Ausreisser
  } else if (year == 2015) {
    files.laser <- list.files(paste0(paste0(p.1$w[p.1$n == "BaSn.raw.p"]), year, "/"), pattern = ".dat")
  } else {
    files.laser <- list.files(paste0(paste0(p.1$w[p.1$n == "BaSn.raw.p"]), year, "/"), pattern = ".dat")
  }

  for (lola in (1:length(files.laser))) {
    #data.laser <- suppressWarnings(read.table(paste0(p.1$w[p.1$n=="BaSn.raw.p"],year,"/",files.laser[lola]),header=F,dec=".",sep=" ", fileEncoding="latin1"))[,1:6]
    txt.laser <- readLines(paste0(p.1$w[p.1$n == "BaSn.raw.p"], year, "/", files.laser[lola]), encoding = "unknown", warn = F)
    # cat(txt.laser[1],"\n")
    ###
    # select only those lines of the txt.laser file which consist of 45 characters (==> Attention! For the format of the character string, the type "bytes" has to be used)
    txt.laser <- txt.laser[nchar(txt.laser, type = "bytes") == 45]
    # cat(txt.laser[1],"\n")
    ##
    data.laser <- data.frame(matrix(nrow = length(txt.laser), ncol = 2, NA))
    for (zz in 1:length(txt.laser)) {
      data.laser[zz, 1] <- format(strptime(substr(txt.laser[zz], 1, 14), format = "%d.%m.%y %H:%M"), format = '%Y-%m-%d %H:%M')
      data.laser[zz, 2] <- suppressWarnings(as.numeric(substr(txt.laser[zz], 22, 27)))
      ##################
      ## ??? remove values with error-flag "15" ???
      ## this function is uncommented at the moment!!!
      ##################
      #  data.laser[zz,3] <- suppressWarnings(as.num(substr(txt.laser[zz],40,42)))
      ##################

    }

    colnames(data.laser) <- c("V1", "V2")#,"V3")
    ## laser signal returns after 35 seconds ... round down to get 00:00:00 time
    data.laser$V1 <- as.numeric(as.POSIXct(data.laser[, 1], format = '%Y-%m-%d %H:%M', tz = "UTC"))
    ##################
    ## ??? remove values with error-flag "15" ???
    ## this function is uncommented at the moment!!!
    # data.laser           <- data.laser[!(data.laser[,3]>=15),]
    if (length(data.laser$V1) > 0) {### if it is empty, you do not have to aggregate
      newdf.c <- merge(compl.temp2, data.laser, all.x = T, by = "V1")
      #############
      # duplicated values:
      ind <- which(duplicated(newdf.c[, 1]) == TRUE)
      # if there are duplicated values, remove the duplicated rows
      if (length(ind) > 0) {
        for (j in 1:length(ind)) {
          # replace values with index ind - 1 with average of values with index from ind and ind - 1
          newdf.c[ind[j - 1], 4] <- (newdf.c[ind[j], 4] + newdf.c[ind[j - 1], 4]) / 2
        }
        # remove duplicated rows
        newdf.c <- newdf.c[-ind, ]
      }
      #############
      db.basnow[, 2] <- round(rowMeans(cbind(db.basnow[, 2], newdf.c[, 4]), na.rm = T), 4)
      ## db.basnow[,3]        <- round(rowMeans(cbind(db.basnow[,3],newdf.c[,4]),na.rm=T),4)
    }
  }

  ## NA problem
  for (kl in 2:ncol(db.basnow)) {# ist in diesem Fall nur eine Schleife fuer 1 Spalte
    db.basnow[, kl] <- as.numeric(db.basnow[, kl])
    db.basnow[is.nan(db.basnow[, kl]), kl] <- NA
  }

  db.basnow <- data.frame(db.basnow)

  db.basnow[, 1] <- format(as.POSIXct(db.basnow[, 1], origin = "1970-01-01", tz = "UTC"), format = '%Y-%m-%d %H:%M')
  colnames(db.basnow) <- c("UTC", "Dsn", "Dsn_fl_logger")
  db.basnow <- db.basnow[, 1:2]

  #######################
  # data manipulation
  #####
  # correct snow distance
  if (year == 2019) {
    ind1 <- which(db.basnow$UTC == "2019-06-29 00:00")
    db.basnow[ind1:length(db.basnow$Dsn), "Dsn"] <- round(db.basnow[ind1:length(db.basnow$Dsn), "Dsn"] - 0.01, 3)
  }

  if (year > 2019) {
    db.basnow[, "Dsn"] <- round(db.basnow[, "Dsn"] - 0.01, 3)
  }
  #######################

  write.table(db.basnow, paste0(paste(p.1$w[p.1$n == "LV0.p"]), "BaSnow2013/00_full_dataset/BaSnow2013_", year, "_lv0.dat"),
              quote = F, dec = ".", sep = ",", row.names = F)

  cat("#\n# BaSnow2013 ", year, "without problems!\n#\n")
}

