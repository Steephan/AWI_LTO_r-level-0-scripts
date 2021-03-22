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
# 2020-07-29 CL introduced the variable n.col, harmonized the colnames of data.temp with db.sahole
# - implement new path
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
start.year <- 2010
end.year <- 2017

#############################################################################

options(warn = -1)# necessary! ... there are some strange warnings
in.path <- paste0(path$w[path$n == "RAW.p"], "SaHole2010/")
out.path <- paste0(path$w[path$n == "LV0.p"], "SaHole2010/00_full_dataset/")
origin <- "1970-01-01"

# read files
files2read <- list.files(in.path, pattern = "*.csv")

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

colnames(db.sahole) <- c("UTC", "Ts_1", "Ts_2", "Ts_3", "Ts_4", "Ts_5", "Ts_6", "Ts_7", "Ts_8", "Ubat")
db.sahole[, 1] <- format(as.POSIXct(db.sahole[, 1], origin = origin, tz = "UTC"), format = '%Y-%m-%d %H:%M')

for (years in start.year:end.year) {## 2009:2017
  write.table(db.sahole[as.numeric(format(as.POSIXct(db.sahole[, 1], format = '%Y-%m-%d %H:%M', origin = origin, tz = "UTC"), format = '%Y')) == years, ],
            paste(out.path , "SaHole2010_", years, "_lv0.dat", sep = ""),
            quote = F, dec = ".", sep = ",", row.names = F)
}
## and update the big one
#write.table(db.sahole, paste(out.path , "SaHole2010_all.dat", sep = ""), quote = F, dec = ".", sep = ",", row.names = F)
cat("\n#\n# Sahole2010 without problems!\n#\n")




