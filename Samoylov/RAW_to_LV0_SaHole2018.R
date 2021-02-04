#############################################################################
##
## SaHole2018  RAW to Level0
##
## equal time steps
##
## by: Stephan.Lange@awi.de
##     Niko.Bornemann@awi.de
##     Christian.Lehr@awi.de
##
## last modified: 2020-07-29
##
#############################################################################

## Input header
# RBR XR-420 5.62 010312 (Windows: 5.25 - Minimum required: 5.20)

#############################################################################
#
# last modifications:
# 2020-07-29 CL set up the script using the script of SaHole2010 as template
#
#############################################################################

#############################################################################
# to run this script separately,  you have to uncomment the next 10 lines!
# rm(list = ls())
# if (.Platform$OS.type == "windows") {
#   path <- read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt", sep = "\t", header = T)
#   maint <- read.table("N:/sparc/LTO/R_database/database_R/settings/sa_maintance.txt", sep = "\t", header = T)
#   source("N:/sparc/LTO/R_database/database_R/settings/db_func.R")
# } else {
#   path <- read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt", sep = "\t", header = T,  fileEncoding = "UTF-8")
#   maint <- read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)
#   source("/sparc/LTO/R_database/database_R/settings/db_func.R")
# }
############################################################################

#############################################################################
# update if necessary:
start.year <- 2018
end.year <- 2019

#############################################################################

options(warn = -1)# necessary! ... there are some strange warnings
in.path <- paste0(path$w[path$n == "RAW.p"], "SaHole2018/")
out.path <- paste0(path$w[path$n == "LV0.p"], "SaHole2018/00_full_dataset/")
origin <- "1970-01-01"

# read files
files2read <- list.files(in.path, pattern = "*.csv")

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
  dim(data.temp)
  data.temp <- data.temp[-which(format(data.temp.time.min, "%M") != "00"), ]
  dim(data.temp)

  # format time column in hourly resolution as numerical value
  data.temp$V2 <- round(as.POSIXct(data.temp$V2, format = '%d.%m.%Y %H:%M:%S', origin = origin, tz = "UTC"), "hour")
  data.temp$V2 <- as.numeric(data.temp$V2)

  # remove duplicated rows
  dim(data.temp)
  data.temp <- check.double.entry(data.temp)
  dim(data.temp)
  # remove rows with NA values for date
  data.temp <- data.temp[-which(is.na(data.temp$V2)), ]
  dim(data.temp)

  # as.num converts the data to numeric values ==> defined in db_helper.R
  data.temp[, 1:n.col] <- sapply(data.temp[, 1:n.col], as.num)
  #data.temp[duplicated(data.temp$V2) == T, ]

  ###############
  # Assignment of column names
  # replaced "HK-Temp" with "Tpan" and "HK-BAT" with "Ubat"
  # ===> check whether this is correct
  #colnames(data.temp) <- c("UTC", paste("Ts_", 1:31, sep = ""), "HK-Temp", "HK-BAT")
  #colnames(data.temp) <- c("UTC", paste("Ts_", 1:31, sep = ""), "Tpan", "Bat_V")
  ###############
  # Assignment of column names according to
  # : sparc\data\LTO\excel\SaHole2018\20180712_SaHole2018-SaHole2006_calibration.xlsx
  #  1m      2m      3m      4m      5m      6m      7m      8m      9m      10m     11m     12m     13m     14m     15m     16m     17m     18m     19m     20m     22m     24m     26m     28m     30m     35m     40m     45m     50m     55m     60m
  # NO      TIME    #1(oC)  #2(oC)  #3(oC)  #4(oC)  #5(oC)  #6(oC)  #7(oC)  #8(oC)  #9(oC)  #10(oC) #11(oC) #12(oC) #13(oC) #14(oC) #15(oC) #16(oC) #17(oC) #18(oC) #19(oC) #20(oC) #21(oC) #22(oC) #23(oC) #24(oC) #25(oC) #26(oC) #27(oC) #28(oC) #29(oC) #30(oC) #31(oC) HK-TEMP(oC)     HK-BAT(V)
  ###############
  colnames(data.temp) <- c("UTC", paste("Ts_", c(1:20, 22, 24, 26, 28, 30, 35, 40, 45, 50, 55, 60), sep = ""), "Tpan", "Bat_V")

  newdf.a <- merge(compl.temp, data.temp, all.x = T, by = "UTC")
  for (i in 2:n.col) {
    db.sahole[, i] <- rowMeans(cbind(db.sahole[, i], newdf.a[, i + 1]), na.rm = T)
  }
}

colnames(db.sahole) <- colnames(data.temp)
db.sahole[, 1] <- format(as.POSIXct(db.sahole[, 1], origin = origin, tz = "UTC"), format = '%Y-%m-%d %H:%M')

for (years in start.year:end.year) {
  write.table(db.sahole[as.numeric(format(as.POSIXct(db.sahole[, 1], format = '%Y-%m-%d %H:%M', origin = origin, tz = "UTC"), format = '%Y')) == years, ],
            paste(out.path , "SaHole2018_", years, "_lv0.dat", sep = ""),
            quote = F, dec = ".", sep = ",", row.names = F)
}
## and update the big one
#write.table(db.sahole, paste(out.path , "SaHole2018_all.dat", sep = ""), quote = F, dec = ".", sep = ",", row.names = F)
cat("\n#\n# Sahole2018 without problems!\n#\n")




