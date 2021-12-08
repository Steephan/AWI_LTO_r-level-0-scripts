###............................................................................
##
## SaHole2018  RAW to Level0
##
## equal time steps
##
## by: Stephan.Lange@awi.de
##     Niko.Bornemann@awi.de
##     
##
## last modified: 2021-10-26
##
###............................................................................

## Input header
# RBR XR-420 5.62 010312 (Windows: 5.25 - Minimum required: 5.20)

### last modifications ----
#
# 
# 2021-10-26 SL remove some special issues of input data
# 2021-10-26 SL update to runapp
# 2020-07-29 CL set up the script using the script of SaHole2010 as template
#
###............................................................................

###............................................................................

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

# run.year <- 2018:2021


options(warn = -1)# necessary! ... there are some strange warnings
in.path <- paste0(p.1$w[p.1$n == "RAW.p"], "SaHole2018/")
out.path <- paste0(p.1$w[p.1$n == "LV0.p"], "SaHole2018/00_full_dataset/")
origin <- "1970-01-01"

# read files
files2read <- list.files(in.path, pattern = "*.csv")

for (year in run.year) {
  # define start and end date
  start.date <- as.POSIXct(paste("01.01.2018", sep = ""), format = '%d.%m.%Y', tz = "UTC")
  end.date <- as.POSIXct(paste("31.12.", format(Sys.Date(), "%Y"), sep = ""), format = '%d.%m.%Y', tz = "UTC")
  
  # set up empty matrices
  n.col <- 34
  db.sahole <- matrix(ncol = n.col, nrow = length(seq(start.date, end.date, by = "hour")), NA)
  # compl.temp: dummy matrix for later merging
  compl.temp <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "hour")))
  
  # format first column as time column in numerical format
  db.sahole[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "hour"), format = '%Y-%m-%d %H:%M'))
  compl.temp[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "hour"), format = '%Y-%m-%d %H:%M'))
  colnames(compl.temp) <- c("UTC", "erste")
  
  
  for (kk in 1:length(files2read)) {
    #data.temp <- read.table(file = paste(in.path, files2read[kk], sep = ""), sep = ",", dec = ".", header = TRUE, skip = 8)
    data.temp <- read.table(file = paste(in.path, files2read[kk], sep = ""), sep = ",", dec = ".",
                            header = F, skip = 9, col.names = paste0("V", seq_len(n.col + 1)), fill = TRUE)[, 2:(n.col + 1)]
    # time vector with minutes resolution
    data.temp.time.min <- round(as.POSIXct(data.temp$V2, format = '%d.%m.%Y %H:%M:%S', origin = origin, tz = "UTC"), "min")
    # data.temp.time.min.num <- as.numeric(data.temp.time.min)
    
    # remove rows with entries that were recorded on minutes other than minute "00" of an hour
    ### !!!!!!!!!!
    # Attention
    # It has to be checked manually for the read files, whether this makes sense.
    # In case that the logger is recording systematically off the regular "00" setting, this makes no sense!
    # In that case values that are properly recorded in an hourly resolution (maybe all), would be excluded, because of this common shift.
    ## !!!!!!!!!!
    # dim(data.temp)
    # data.temp <- data.temp[-which(format(data.temp.time.min, "%M") != "00"), ]
    # dim(data.temp)
    
    # format time column in hourly resolution as numerical value
    data.temp$V2 <- round(as.POSIXct(data.temp$V2, format = '%d.%m.%Y %H:%M:%S', origin = origin, tz = "UTC"), "hour")
    data.temp$V2 <- as.numeric(data.temp$V2)
    
    # remove duplicated rows
    # dim(data.temp)
    data.temp <- check.double.entry(data.temp)
    #dim(data.temp)
    # remove rows with NA values for date
    # data.temp <- data.temp[-which(is.na(data.temp$V2)), ]
    # dim(data.temp)
    
    # as.num converts the data to numeric values ==> defined in db_helper.R
    data.temp[, 1:n.col] <- sapply(data.temp[, 1:n.col], as.num)
    
    colnames(data.temp) <- c("UTC", paste("Ts_", c(1:20, 22, 24, 26, 28, 30, 35, 40, 45, 50, 55, 60), sep = ""), "Tpan", "Bat_V")
    
    newdf.a <- merge(compl.temp, data.temp, all.x = T, by = "UTC")
    for (i in 2:n.col) {
      db.sahole[, i] <- rowMeans(cbind(db.sahole[, i], newdf.a[, i + 1]), na.rm = T)
    }
  }
  
  colnames(db.sahole) <- colnames(data.temp)
  db.sahole[, 1] <- format(as.POSIXct(db.sahole[, 1], origin = origin, tz = "UTC"), format = '%Y-%m-%d %H:%M')
  
  
  write.table(db.sahole[as.numeric(format(as.POSIXct(db.sahole[, 1], format = '%Y-%m-%d %H:%M', origin = origin, tz = "UTC"), format = '%Y')) == year, ],
              paste(out.path , "SaHole2018_", year, "_lv0.dat", sep = ""),
              quote = F, dec = ".", sep = ",", row.names = F)
  cat("#\n# Sahole2018 ",year,"without problems!\n#\n") 
}





