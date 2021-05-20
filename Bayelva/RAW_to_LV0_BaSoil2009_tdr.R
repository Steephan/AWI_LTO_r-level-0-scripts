###..........................................................................
##
##   RAW to Level0 ----
##
##   equal time steps,  no gaps,  (table flag, not implemented)
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
## - merge with temperature file
###..........................................................................
##
## last modifications: ----
##  2021-05-12 SL adapted to runner app and content management
##  2020-10-09 CL origin <- "01-01-1970" removed because it is not used and might create confusion with other scripts in the sequence of Bayelva_MAIN.R
##  2020-04-14 CL change loop to (year in run.year) to allow the selection of the processed year in Bayelva_MAIN.R
##  2015-12-11 SL comma-seperation,  date-format to "%Y-%m-%d %H:%M",  NaN to NA
##  inclusive data of online status (up-to-date)
###..........................................................................
# to run this script separately,  you have to uncomment the next 10 lines!
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
# to run this script separately,  you have to set run.year:
#
# recent.year <- as.numeric(format(Sys.Date(),  "%Y"))
# run.year <- recent.year
# run.year <- 2020
###..........................................................................

## loop over years ----
for (year in run.year) {
  #print(paste("running", year,  "of BaSoil2009-tdr's" ))
  start.date <- as.POSIXct(paste0(year, "-01-01 00:00:00"), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date   <- as.POSIXct(paste0(year, "-", 12, "-", 31, " 23:00:00"), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  #end.date    <- as.POSIXct(paste(year, "-", monat, "-", tag, " 23:00:00", sep = ""), format = '%Y-%m-%d %H:%M:%S',  tz = "UTC")

  db.now <- matrix(ncol = 26, nrow = length(seq(start.date, end.date, by = "hour")), -999)
  compl.temp <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "hour")))
  compl.temp[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "hour"), format = '%Y-%m-%d %H:%M:%S', tz = "UTC"))
  colnames(compl.temp) <- c("V1", "leer")
  ## input names  ----
  colnames(db.now) <- c("V1", "LaL(1)", "LaL(2)", "LaL(3)", "LaL(4)", "LaL(5)", "LaL(6)", "LaL(7)", "LaL(8)", "TDR_Cond(1)",
                      "TDR_Cond(2)", "TDR_Cond(3)", "TDR_Cond(4)", "TDR_Cond(5)", "TDR_Cond(6)", "TDR_Cond(7)", "TDR_Cond(8)",
                      "VWC(1)", "VWC(2)", "VWC(3)", "VWC(4)", "VWC(5)", "VWC(6)", "VWC(7)", "VWC(8)", "TableFlag")

  db.now[, c(2:25)] <- NA
  db.now[, 26] <- 0
  files.temp <- dir(paste0(p.1$w[p.1$n == "BaS.raw1.p"]),  pattern = glob2rx("*.dat"))
  if (year >= 2015) {
    files.temp <- c(files.temp, dir(paste0(p.1$w[p.1$n == "BaS.onl.p"]), pattern = glob2rx("*TDROnline.dat")))
  }

  for (lola in 1:length(files.temp)) {
    inz.path <- paste0(p.1$w[p.1$n == "BaS.raw1.p"])
    if (year >= 2015 && lola == length(files.temp)) {
      inz.path <- paste0(p.1$w[p.1$n == "BaS.onl.p"])
    }

    data.temp <- read.table(paste(inz.path, files.temp[lola], sep = ""),
                            header = F, skip = 4, dec = ".", sep = ",", na = "NAN")[, -2]
    data.temp[, 1] <- as.numeric(as.POSIXct(data.temp[, 1], format = '%Y-%m-%d %H:%M:%S',  tz = "UTC"))
    colnames(data.temp) <- c("V1", "LaL(1)", "LaL(2)", "LaL(3)", "LaL(4)", "LaL(5)", "LaL(6)", "LaL(7)", "LaL(8)", "TDR_Cond(1)",
                           "TDR_Cond(2)", "TDR_Cond(3)", "TDR_Cond(4)", "TDR_Cond(5)", "TDR_Cond(6)", "TDR_Cond(7)", "TDR_Cond(8)",
                           "VWC(1)", "VWC(2)", "VWC(3)", "VWC(4)", "VWC(5)", "VWC(6)", "VWC(7)", "VWC(8)")
    #print(paste(lola))
    newdf.a <- merge(compl.temp, data.temp, all.x = T,  by = "V1")
    newdf.a <- check.double.entry(newdf.a)
    for (marakov in 2:25) {
      db.now[, marakov] <- round(rowMeans(cbind(db.now[, marakov], newdf.a[, marakov + 1]), na.rm = T), 6)
    }

  }
  rm(inz.path)
  ## NA problem ----
  for (kl in 2:ncol(db.now)) {
    db.now[, kl] <- as.numeric(db.now[, kl])
    db.now[is.nan(db.now[, kl]), kl] <- NA
  }

  ## input names  ----
  # colnames(db.now) <- c("UTC", "LaL(1)", "LaL(2)", "LaL(3)", "LaL(4)", "LaL(5)", "LaL(6)", "LaL(7)", "LaL(8)", "TDR_Cond(1)",
  #                     "TDR_Cond(2)", "TDR_Cond(3)", "TDR_Cond(4)", "TDR_Cond(5)", "TDR_Cond(6)", "TDR_Cond(7)", "TDR_Cond(8)",
  #                     "VWC(1)", "VWC(2)", "VWC(3)", "VWC(4)", "VWC(5)", "VWC(6)", "VWC(7)", "VWC(8)", "TableFlag")

  db.now <- db.now[, c(1, 8, 7, 6, 5, 4, 3, 2, 9, 16, 15, 14, 13, 12, 11, 10, 17, 24, 23, 22, 21, 20, 19, 18, 25)]     #with new/correct order,  remove TableFlag
  # set Lal data to dielectricity
  for (quil in 2:9) {
    db.now[, (quil)] <- db.now[, (quil)]^2
  }
  colnames(db.now) <- c("UTC", "E2_h_1", "E2_h_11", "E2_h_21", "E2_h_37",
                              "E2_h_55", "E2_h_71", "E2_h_89",                 "E2_sn_v_0",
                              "cond_h_1", "cond_h_11", "cond_h_21", "cond_h_37",
                              "cond_h_55", "cond_h_71", "cond_h_89",         "cond_sn_v_0",
                              "vwc_h_1", "vwc_h_11", "vwc_h_21", "vwc_h_37",
                              "vwc_h_55", "vwc_h_71", "vwc_h_89",             "vwc_sn_v_0")


  db.now <- as.data.frame(db.now)
  db.now[, 1] <- format(as.POSIXct(seq(start.date, end.date, by = "hour"), format = '%Y-%m-%d %H:%M:%S', tz = "UTC"), format = '%Y-%m-%d %H:%M')
  write.table(db.now, paste0(paste0(p.1$w[p.1$n == "BaS.lv0.p"]) , "02_tdr/BaSoil2009_tdr_", year, "_lv0.dat"),
              quote = F, dec = ".", sep = ",", row.names = F)

  db.temp <- read.table(paste0(p.1$w[p.1$n == "BaS.lv0.p"], "01_temperature/BaSoil2009_Ts_", year, "_lv0.dat"),
                        header = T, dec = ".", sep = ",", na = "NA")

  db.full <- cbind( db.temp[-12],  db.now[-1])
  ## write data ----
  write.table(db.full, paste0(p.1$w[p.1$n == "BaS.lv0.p"] , "00_full_dataset/BaSoil2009_", year, "_lv0.dat"),
              quote = F, dec = ".", sep = ",", row.names = F)


  cat("#\n# BaSoil2009_tdr ", year, "without problems!\n#\n")
}








