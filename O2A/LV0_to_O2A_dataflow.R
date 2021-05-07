###..............................................................................
##
##   Level0 to O2A -----
##
##   daily raw files
##
##   by:  stephan.lange@awi.de
##
##
###..............................................................................
#
#
###..............................................................................
##
## last modification:
## 2021-05-07 SL add BaSnow2013 & 2019, SaMet2002, SaSnow2012, SaSoil2002 & 2012
## 2021-04-20 SL add Bamet2009 and BaHole2021
## 2021-04-15 SL create skript and add BaSoil2009
##
###..............................................................................
## path definitions -----
#to run this script separately, you have to uncomment the next 10 lines!
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
###..............................................................................
## extra inputs --------
# to run this script separately, you have to uncomment the next 3 lines and choose station, years and run.year
# origin <- "1970-01-01"
# recent.year <- as.numeric(format(Sys.Date(),"%Y"))
# station <- 'BaSnow2013'
# run.year <- 2021
# day.shift <- 140
###..............................................................................


stations <- c('BaSoil2009', 'BaMet2009','BaHole2021',
              'BaSnow2013','BaSnow2019cs','BaSnow2019sr',
              'SaMet2002','SaSnow2012',
              'SaSoil2002','SaSoil2012')

list.years <- list(2009:recent.year, 2009:recent.year, 2021:recent.year,
                   2013:recent.year, 2019:recent.year, 2019:recent.year,
                   2002:recent.year, 2012:recent.year,
                   2002:recent.year, 2012:recent.year)

years <- list.years[[which(stations == station)]]


file.name.O2A <- paste0(p.1$w[p.1$n == "settings.p"],  "O2A_name.files/O2A_names_", station,".txt")
O2A.names     <- read.table(file.name.O2A, sep = ";", dec = ".", header = T)

for (year_i in run.year){
  #if(year_i == as.numeric(format(Sys.Date(),"%Y"))){}
  file.name.main <- paste0(p.1$w[p.1$n == "LV0.p"], station, "/00_full_dataset/", station,"_", year_i, "_lv0.dat")
  lv0.data <- read.table(file.name.main, sep = ",", dec = ".", header = T)
  # set time format
  lv0.data[, 1] <- format(as.POSIXct(lv0.data[, 1], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M')
  
  

  O2A.names1         <- O2A.names[match(colnames(lv0.data), O2A.names$Lv0),]
  colnames(lv0.data) <- O2A.names1[,3]
  
  
  for (dayshift_i in 1:day.shift) {
    dupp <- which(format(as.POSIXct(lv0.data$time, format = '%Y-%m-%d %H:%M',origin = origin, tz = "UTC"),"%Y-%m-%d") == format(Sys.Date() - dayshift_i, "%Y-%m-%d"))
    yesterday <- format(Sys.Date() - dayshift_i, "%Y%m%d")
    lv0.dayly <- lv0.data[dupp, ]
    write.table(lv0.dayly,
                paste0(p.1$w[p.1$n == "LV0.p"], station, "/01_O2A/", year_i, "/", yesterday, "_", station, "_lv0.dat"), quote = F, dec = ".", sep = ";", row.names = F)
  }
  cat("#\n# Lv0 to O2A for ",station, year_i, " updated ",dayshift_i," files\n#\n")
}





