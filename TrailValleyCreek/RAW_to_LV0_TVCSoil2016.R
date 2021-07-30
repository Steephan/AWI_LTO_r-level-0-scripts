###...........................................................................
##
##   TVCSoil2016     RAW to Level0 -----
##
##   equal time steps, no gaps
##
##   by: Stephan.Lange@awi.de
##   last modified: 2021-05-11
##
###...........................................................................


###...........................................................................
## step 1.01 set path settings for different systems linux vs. windoof -----
##
###...........................................................................

# to run this script separately, you have to uncomment the next 10 lines!
# rm(list = ls())
# if (.Platform$OS.type == "windows") {
#   p.1 <- read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt", sep = "\t", header = T)
#   p.1maint <- read.table("N:/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)
# 
#   source("N:/sparc/LTO/R_database/database_R/settings/db_func.R")
# } else {
#   p.1 <- read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
#   p.1maint <- read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)
# 
#   source("/sparc/LTO/R_database/database_R/settings/db_func.R")
# }
###...........................................................................
## step 1.02 set running options years, ... -----
##
###...........................................................................
options(scipen = 100) # for non-exponential display of numeric values
origin <- "1970-01-01"
recent.year <- as.numeric(format(Sys.Date(), "%Y"))
# run.year <- 2016:recent.year
#run.year <- years
###...........................................................................
## step 1.03 loop 1 over years -----
##
###...........................................................................

for (t.year in run.year) {
  
  ###...........................................................................
  ## step 1.04 set 2 empty tables with length of t.year -----
  ##
  ## columns: 2 (date table) and number of input table (storing table)
  ###...........................................................................
  
  #cat("\nProcessing t.year", t.year, "\n ==================== \n\n")
  start.date <- as.POSIXct(paste(t.year, "-01-01 00:00:00", sep = ""), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date <- as.POSIXct(paste(t.year, "-", 12, "-", 31, " 23:30:00", sep = ""), format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
  
  # create empty data frame with UTC time stamp every 60 min
  db.TVCsoil <- matrix(ncol = 32, nrow = length(seq(start.date, end.date, by = "60 min")), -999)
  db.TVCsoil[, c(2:32)] <- NA
  compl.temp <- matrix(ncol = 2, nrow = length(seq(start.date, end.date, by = "60 min")))
  db.TVCsoil[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "60 min"), format = '%Y-%m-%d %H:%M:%S'))
  compl.temp[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "60 min"), format = '%Y-%m-%d %H:%M:%S'))
  colnames(compl.temp) <- c("UTC", "erste")
  
  ###...........................................................................
  ## step 1.05 set input.path and list all files -----
  ##
  ###...........................................................................
  inz.path <- paste0(p.1$w[p.1$n == "RAW.p"], "TVCSoil2016/")
  files2read <- list.files(inz.path, pattern = "*.dat")#[2]
  ###...........................................................................
  ## step 1.06 loop 2 over all files -----
  ##
  ###...........................................................................
  
  for (i in 1:length(files2read)) {#1:length(files2read)
    ###...........................................................................
    ## step 1.07 read one file (skip headers, set NA-values) -----
    ##
    ## set temporal colnames
    ###...........................................................................
    #cat("\nprocessing ", files2read[i], "\n ==================== \n\n")
    
    # This block here is not needed any more??
    ###...........................................................................
    # dudu <- read.table(paste(inz.path, files2read[i], sep = ""),
    #                    sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")[1:4, ]
    #
    # if (length(dudu[1, ]) == 43) {
    #   dada <- read.table(paste(inz.path, files2read[i], sep = ""),
    #                      sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")
    #   # hier muss das neue file rein
    # } else {
    #   dada <- read.table(paste(inz.path, files2read[i], sep = ""),
    #                      sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")
    # }
    ###...........................................................................
    
    dada <- read.table(paste(inz.path, files2read[i], sep = ""),
                       sep = ",", dec = ".", header = F, skip = 4, fill = TRUE, na = "NAN")
    
    colnames(dada) = paste0("V", seq_len(ncol(dada)))
    ###...........................................................................
    ## step 1.09 check file if dates are in running t.year of loop 1 -----
    ##
    ###...........................................................................
    
    # skip file if wrong t.year
    if (as.numeric(substr(lapply(dada[1, 1], as.character), 1, 4)) > t.year ||
        as.numeric(substr(lapply(dada[length(dada[, 1]), 1], as.character), 1, 4)) < t.year) {
      next
    }
    #cat(paste(dada[1, 1], "     to     ", dada[length(dada[, 1]), 1], "    ", files2read[i]))
    
    ###...........................................................................
    ## step 1.10 convert date to numeric value -----
    ##
    ###...........................................................................
    
    dada[, 1] <- as.numeric(as.POSIXct(dada[, 1], format = '%Y-%m-%d %H:%M:%S', origin = origin, tz = "UTC"))
    dada[8972:17424, 1] <- dada[8972:17424, 1] + 21600
    ###...........................................................................
    ## step 1.11 check file for double entries  -----
    ##
    ###...........................................................................
    if (("TRUE" %in% duplicated(dada)) == TRUE) { # check for fully double entries
      doouble <- duplicated(dada)
      #cat(paste(length(which(doouble == "TRUE")), "duplicated records found in file", files2read[i], "\n",
      #         "first entry:", dada[which(doouble == "TRUE")[1], 1],
      #         "\n  last entry:", dada[which(doouble == "TRUE")[length(which(doouble == "TRUE"))], 1], "\n\n"))
      dada <- unique(dada)  # remove double entries
      
    } else if (("TRUE" %in% duplicated(dada[, 1])) == TRUE) {  # check for multiple different data records for same! timestamp
      doouble <- duplicated(dada[, 1])
      #cat(paste(length(which(doouble == "TRUE")), "multiple records found in file", files2read[i], "\n",
      #         "first entry:", dada[which(doouble == "TRUE")[1], 1],
      #         "\n  last entry:", dada[which(doouble == "TRUE")[length(which(doouble == "TRUE"))], 1], "\n\n"))
      dd <- which(dada[, 1] %in% dada[which(doouble == "TRUE"), 1])
      dada <- dada[-dd, ]  # remove double entries
      
    } else {
      #cat("No double data entries found in", files2read[i], "\n\n")
    }
    
    ###...........................................................................
    ## step 1.12 standard case -----
    ## 
    ## set original colnames
    ###...........................................................................
    
    # colnames(dada) <- c("UTC", "RECORD", "BattV_Min", "PTemp_C_Avg",
    #                   "Temp_C_Avg(1)", "Temp_C_Avg(2)", "Temp_C_Avg(3)", "Temp_C_Avg(4)",
    #                   "LaL(1)", "LaL(2)", "LaL(3)", "LaL(4)",
    #                   "LaL(5)", "LaL(6)", "LaL(7)", "LaL(8)",
    #                   "LaL2(1)", "LaL2(2)", "LaL2(3)", "LaL2(4)",
    #                   "LaL2(5)", "LaL2(6)", "LaL2(7)", "LaL2(8)",
    #                   "ToppVWC(1)", "ToppVWC(2)", "ToppVWC(3)", "ToppVWC(4)",
    #                   "ToppVWC(5)", "ToppVWC(6)", "ToppVWC(7)", "ToppVWC(8)",
    #                   "TDR_Cond(1)", "TDR_Cond(2)", "TDR_Cond(3)", "TDR_Cond(4)",
    #                   "TDR_Cond(5)", "TDR_Cond(6)", "TDR_Cond(7)", "TDR_Cond(8)")
    
    
    colnames(dada) <- c("UTC", "RECORD", "batt_U_min", "Tpan_CR1000",
                        "Ts_20", "Ts_10", "Ts_5", "Ts_2",
                        "LaL(1)", "LaL(2)", "LaL(3)", "LaL(4)",
                        "LaL(5)", "LaL(6)", "LaL(7)", "LaL(8)",
                        "E2_h_20", "E2_h_10", "E2_h_5", "E2_h_2",
                        "E2_v_0", "E2_a_v_0", "E2_b_v_0", "E2_sn_v_0",
                        "vwc_h_20", "vwc_h_10", "vwc_h_5", "vwc_h_2",
                        "vwc_v_0", "vwc_a_v_0", "vwc_b_v_0", "vwc_sn_v_0",
                        "cond_h_20", "cond_h_10", "cond_h_5", "cond_h_2",
                        "cond_v_0", "cond_a_v_0", "cond_b_v_0", "cond_sn_v_0")
    
    ###...........................................................................
    ## step 1.13 new arrangement / order of columns (all Temperatures together, ascending, ... ) -----
    ##
    ###...........................................................................
    dada <- dada[, c("UTC",
                     "Ts_2", "Ts_5", "Ts_10", "Ts_20",
                     
                     "E2_h_2", "E2_h_5", "E2_h_10", "E2_h_20",
                     "E2_v_0", "E2_a_v_0", "E2_b_v_0", "E2_sn_v_0",
                     
                     "vwc_h_2", "vwc_h_5", "vwc_h_10", "vwc_h_20",
                     "vwc_v_0", "vwc_a_v_0", "vwc_b_v_0", "vwc_sn_v_0",
                     
                     "cond_h_2", "cond_h_5", "cond_h_10", "cond_h_20",
                     "cond_v_0", "cond_a_v_0", "cond_b_v_0", "cond_sn_v_0",
                     
                     "batt_U_min", "Tpan_CR1000")]
    ###...........................................................................
    ## step 1.14 merge input data with date table -----
    ##
    ###...........................................................................
    
    newdf.a <- merge(compl.temp, dada, all.x = T, by = "UTC")
    
    ###...........................................................................
    ## step 1.15 merge date table with storing table -----
    ##
    ###...........................................................................
    
    for (k in 2:(length(db.TVCsoil[1, ]))) {
      db.TVCsoil[, k] <- rowMeans(cbind(db.TVCsoil[, k], newdf.a[, k]), na.rm = T)#
    }
  }
  
  ###...........................................................................
  ## step 1.16 convert numeric dates back to date format  -----
  ##
  ###...........................................................................
  
  
  db.TVCsoil[, 1] <- format( as.POSIXct(db.TVCsoil[, 1], origin = origin, tz = "UTC"), format = '%Y-%m-%d %H:%M')
  
  ###...........................................................................
  ## step 1.17 set "sparc" colnames -----
  ##
  ###...........................................................................
  
  colnames(db.TVCsoil) <- c("UTC", "placeholder",
                            "Ts_2", "Ts_5", "Ts_10", "Ts_20",
                            
                            "E2_h_2", "E2_h_5", "E2_h_10", "E2_h_20",
                            "E2_v_0", "E2_a_v_0", "E2_b_v_0", "E2_sn_v_0",
                            
                            "vwc_h_2", "vwc_h_5", "vwc_h_10", "vwc_h_20",
                            "vwc_v_0", "vwc_a_v_0", "vwc_b_v_0", "vwc_sn_v_0",
                            
                            "cond_h_2", "cond_h_5", "cond_h_10", "cond_h_20",
                            "cond_v_0", "cond_a_v_0", "cond_b_v_0", "cond_sn_v_0",
                            
                            "batt_U_min", "Tpan_CR1000")
  ###...........................................................................
  ## step 1.18 safe data to txt-file -----
  ##
  ###...........................................................................
  
  write.table(db.TVCsoil[as.numeric(format(as.POSIXct(db.TVCsoil[, 1], format = '%Y-%m-%d %H:%M', origin = origin, tz = "UTC"), format = '%Y')) == t.year, -2],
              paste0(p.1$w[p.1$n == "LV0.p"], "TVCSoil2016/00_full_dataset/TVCSoil2016_", t.year, "_lv0.dat"), quote = F, dec = ".", sep = ",", row.names = F)
  cat("#\n# level0 TVCsoil2016:", t.year, "without problems!\n#\n")
  
} # end loop over t.years



