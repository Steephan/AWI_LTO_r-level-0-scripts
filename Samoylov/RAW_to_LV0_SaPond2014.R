#############################################################################
##
##   SaPond2014       RAW to Level0
##
##   equal time steps
##
##   written by:  stephan.lange@awi.de
##                Niko.Bornemann@awi.de
##
##   last modified: 2016-09-07
##
##   last check: 2020-01-29
##   checked by: christian.lehr@awi.de
##
#############################################################################
##
## open issues:
##
##
#############################################################################
##
##  last modifications:
##  - implement new path
##  - new header,
##
#############################################################################
##
## Comments:
## Input header
## RBR XR-420  5.62 010312 (Windows: 5.25 - Minimum required: 5.20)
##
##
#############################################################################
# to run this script separate, you have to uncomment the next 10 lines!
# rm(list = ls())
# if (.Platform$OS.type == "windows") {
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





#############################################################################
# update if necessary:
start.year <- 2014
end.year <- 2019

#############################################################################

options(warn = (-1))# necessary! ... there are some strange warnings
in.path <- paste0(path$w[path$n == "RAW.p"], "SaPond2014/")
out.path <- paste0(path$w[path$n == "LV0.p"], "SaPond2014/00_full_dataset/")
origin <- "1970-01-01"


files2read <- list.files(in.path, pattern = "*.csv")

grr <- length(files2read)

start.date <- as.POSIXct(paste("01.01.2014", sep = ""), format = '%d.%m.%Y', tz = "UTC")
end.date <- as.POSIXct(paste("31.12.", format(Sys.Date(), "%Y"), sep = ""), format = '%d.%m.%Y', tz = "UTC")

db.sapond <- matrix(ncol = 11, nrow = length(seq(start.date, end.date, by = "hour")), -999)
db.sapond[ ,c(2:11)] <- NA

compl.temp <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "hour")))

db.sapond[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "hour"), format = '%Y-%m-%d %H:%M'))
compl.temp[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "hour"), format = '%Y-%m-%d %H:%M'))
colnames(compl.temp) <- c("UTC", "erste")



for (kk in 1:length(files2read)) {#1:length(files2read)
  data.temp <- read.table(paste(in.path,files2read[kk], sep = ""), sep = ",", dec = ".", header = F, skip = 5, col.names = paste0("V", seq_len(12)), fill = TRUE)[, 2:12]
  data.temp$V2 <- round(as.POSIXct(data.temp[, 1], format = '%d.%m.%Y %H:%M:%S', origin = origin, tz = "UTC"), "hour")
  data.temp$V2 <- as.numeric(data.temp$V2)

  data.temp[, 1:11] <- sapply(data.temp[, 1:11], as.num)
  # check double entries:
  doouble <- duplicated(data.temp[, 1])
  dd <- which(data.temp[, 1] %in% data.temp[which(doouble == "TRUE"), 1])
  data.temp <- data.temp[-dd, ]  # remove double entries


colnames(data.temp) <- c("UTC", "Tw_40", "Tw_30", "Tw_20", "Tw_10", "Ts_0", "Ts_10", "Ts_20", "Ts_30", "batt_U", "Tpan")

newdf.a <- merge(compl.temp, data.temp,all.x = T, by = "UTC")
  for (i in 2:11) {
    db.sapond[, i] <- rowMeans(cbind(db.sapond[, i], newdf.a[, i + 1]), na.rm = T)
  }
}

colnames(db.sapond) <- c("UTC", "Tw_40", "Tw_30", "Tw_20", "Tw_10", "Ts_0", "Ts_10", "Ts_20", "Ts_30", "batt_U", "Tpan")
db.sapond[, 1] <- format( as.POSIXct(db.sapond[, 1], origin = origin, tz = "UTC"), format = '%Y-%m-%d %H:%M')
for (m in 2:11) {# replace Nan with NA
  db.sapond[is.nan(as.numeric(db.sapond[, m])), m] <- NA
}


for (years in start.year:end.year) {
write.table(db.sapond[as.numeric(format(as.POSIXct(db.sapond[, 1], format = '%Y-%m-%d %H:%M', origin = origin, tz = "UTC"),
                                        format = '%Y')) == years, ], paste(out.path , "SaPond2014_", years, "_lv0.dat", sep = ""),
            quote = F, dec = ".", sep = ",", row.names = F)
    cat("\n#\n# SaPond2014", years, "without problems!\n#\n")
}
## and update the big one
#write.table(db.sapond,paste(out.path ,"SaPond2014_all.dat",sep=""),quote = F,dec=".",sep=",",row.names=F)
#cat("\n#\n# SaPond2014 without problems!\n#\n")




