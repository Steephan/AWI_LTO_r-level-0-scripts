###..........................................................................
##
##   SdHole2009        RAW to Level0 ----
##
##   equal time steps, no gaps, table flag
##
##   by: Stephan.Lange@awi.de and Niko.Bornemann@awi.de
##
##   last modified: 2021-05-12
##
###..........................................................................
##
##   last modification: ----
##   2021-11-01 SL cable incorrectly switched implementation
##   2021-05-12 SL adapted to git, runnerapp and content management
##      -  cable incorrectly switched form "2018-07-10 06:00" to "2021-09-21 00:00"
##   
##
##
###..........................................................................
## step 1.01 set path settings for different systems linux vs. windoof ----
##
###..........................................................................
# to run this script seperat, you have to uncomment the next 10 lines!
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
## step 1.02 set running options years, ... ----
##
###..........................................................................
options(scipen=100) # for non-exponential display of numeric values
origin  <- "1970-01-01"
#run.year <- 2021
###..........................................................................
## step 1.03 loop 1 over years ----
##
###..........................................................................
for (year_i in run.year){#2009:aktuell
  ###..........................................................................
  ## step 1.04 set 2 empty tables with length of year ----
  ##
  ## columns: 2 (date table) and number of input table (storing table)
  ###..........................................................................
  
  
  #cat("\nProcessing year",year_i,"\n====================\n\n")
  start.date <-as.POSIXct(paste(year_i,"-01-01 00:00:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date   <-as.POSIXct(paste(year_i,"-",12,"-",31," 23:30:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
  # create empty data frame with UTC time stamp every 360 min
  db.sdhole<-matrix(ncol=27,nrow=length(seq(start.date,end.date,by="360 min")),-999)
  db.sdhole[,c(2:27)]<-NA
  compl.temp<-matrix(ncol=2,nrow=length(seq(start.date,end.date,by="360 min")))
  db.sdhole[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="360 min"),format='%Y-%m-%d %H:%M:%S'))
  compl.temp[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="360 min"),format='%Y-%m-%d %H:%M:%S'))
  colnames(compl.temp)<-c("UTC","erste")
  ###..........................................................................
  ## step 1.05 set input.path and list all files ----
  ##
  ###..........................................................................
  inz.path<-paste0(p.1$w[p.1$n=="RAW.p"],"SdHole2009/")
  files2read<-list.files(inz.path,pattern="*.dat")
  ###..........................................................................
  ## step 1.06 loop 2 over all files ----
  ##
  ###..........................................................................
  
  for(i in 1:length(files2read)){#1:length(files2read)
    ###..........................................................................
    ## step 1.07 read one file (skip headers, set NA-values) ----
    ##
    ## set temporal colnames
    #############################################################################
    
    #cat("\nprocessing ",files2read[i],"\n====================\n\n")
    dada<-read.table(paste(inz.path,files2read[i],sep=""),sep=",",dec=".",header=F,skip=4, fill = TRUE,na="NAN")
    
    colnames(dada) = paste0("V",seq_len(ncol(dada)))
    ###..........................................................................
    ## step 1.09 check file if dates are in running year of loop 1 ----
    ##
    ###..........................................................................
    
    if(as.numeric(substr(lapply(dada[1,1],as.character),1,4))>year_i || as.numeric(substr(lapply(dada[length(dada[,1]),1],as.character),1,4))<year_i) {next} # skip file if wrong year
    #cat(paste(dada[1,1],"     to     ",dada[length(dada[,1]),1],"    ",files2read[i]))
    ###..........................................................................
    ## step 1.10 check file for double entries ----
    ##
    ###..........................................................................
    if(("TRUE" %in% duplicated(dada))==TRUE) { # check for fully double entries
      doouble <- duplicated(dada)
      #cat(paste(length(which(doouble=="TRUE")),"duplicated records found in file",files2read[i],"\n",
      #          "first entry:",dada[which(doouble=="TRUE")[1],1],"\n  last entry:",dada[which(doouble=="TRUE")[length(which(doouble=="TRUE"))],1],"\n\n"))
      dada <- unique(dada)  # remove double entries
      
    } else if(("TRUE" %in% duplicated(dada[,1]))==TRUE){  # check for multiple different data records for same! timestamp
      doouble <- duplicated(dada[,1])
      #cat(paste(length(which(doouble=="TRUE")),"multiple records found in file",files2read[i],"\n",
      #          "first entry:",dada[which(doouble=="TRUE")[1],1],"\n  last entry:",dada[which(doouble=="TRUE")[length(which(doouble=="TRUE"))],1],"\n\n"))
      dd<-which(dada[,1] %in% dada[which(doouble=="TRUE"),1])
      dada <- dada[-dd,]  # remove double entries
      
    } else { #cat("No double data entries found in",files2read[i],"\n\n")    
    }
    ###..........................................................................
    ## step 1.11 convert date to numeric value ----
    ##
    ###..........................................................................
    
    dada[,1]<-as.numeric(as.POSIXct(dada[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC"))
    
    ###..........................................................................
    ## step 1.12a special case: former files with different columns ----
    ##
    ## set colnames
    ###..........................................................................
    if(length(dada[1,])==68){
      colnames(dada)<-  c("UTC","RECORD","Battery_Volts","Panel_Temp","Thermistor_volt(1)","Thermistor_volt(2)",
                          "Thermistor_volt(3)","Thermistor_volt(4)","Thermistor_volt(5)","Thermistor_volt(6)","Thermistor_volt(7)",
                          "Thermistor_volt(8)","Thermistor_volt(9)","Thermistor_volt(10)","Thermistor_volt(11)",
                          "Thermistor_volt(12)","Thermistor_volt(13)","Thermistor_volt(14)","Thermistor_volt(15)",
                          "Thermistor_volt(16)","Thermistor_volt(17)","Thermistor_volt(18)","Thermistor_volt(19)",
                          "Thermistor_volt(20)","Thermistor_volt(21)","Thermistor_volt(22)","Thermistor_volt(23)",
                          "Thermistor_volt(24)","Thermistor_volt(25)","Thermistor_volt(26)","Thermistor_volt(27)",
                          "Thermistor_volt(28)","Thermistor_volt(29)","Thermistor_volt(30)","Thermistor_volt(31)",
                          "Thermistor_volt(32)","Thermistor_temp(1)","Thermistor_temp(2)","Thermistor_temp(3)",
                          "Thermistor_temp(4)","Thermistor_temp(5)","Thermistor_temp(6)","Thermistor_temp(7)",
                          "Thermistor_temp(8)","Thermistor_temp(9)","Thermistor_temp(10)","Thermistor_temp(11)",
                          "Thermistor_temp(12)","Thermistor_temp(13)","Thermistor_temp(14)","Thermistor_temp(15)",
                          "Thermistor_temp(16)","Thermistor_temp(17)","Thermistor_temp(18)","Thermistor_temp(19)",
                          "Thermistor_temp(20)","Thermistor_temp(21)","Thermistor_temp(22)","Thermistor_temp(23)",
                          "Thermistor_temp(24)","Thermistor_temp(25)","Thermistor_temp(26)","Thermistor_temp(27)",
                          "Thermistor_temp(28)","Thermistor_temp(29)","Thermistor_temp(30)","Thermistor_temp(31)",
                          "Thermistor_temp(32)")
      ###..........................................................................
      ## step 1.12b add additional columns to former dataset ----
      ##
      ###..........................................................................
      
      #       dada$S2_ice_Avg  <-NA
      #       dada$S2_water_Avg<-NA
      
      
    }else{
      ###..........................................................................
      ## step 1.12 standard case ----
      ##
      ## set original colnames
      ###..........................................................................
      
      colnames(dada)<-  c("UTC","RECORD",...)
    }
    ###..........................................................................
    ## step 1.13 new arrangement / order of columns  ----
    ##(all Temperatures together, ascending, ... )
    ###..........................................................................
    
    dada<-dada[,c("UTC","Battery_Volts","Panel_Temp","Thermistor_temp(1)","Thermistor_temp(2)","Thermistor_temp(3)",
                  "Thermistor_temp(4)","Thermistor_temp(5)","Thermistor_temp(6)","Thermistor_temp(7)",
                  "Thermistor_temp(8)","Thermistor_temp(9)","Thermistor_temp(10)","Thermistor_temp(11)",
                  "Thermistor_temp(12)","Thermistor_temp(13)","Thermistor_temp(14)","Thermistor_temp(15)",
                  "Thermistor_temp(16)","Thermistor_temp(17)","Thermistor_temp(18)","Thermistor_temp(19)",
                  "Thermistor_temp(20)","Thermistor_temp(21)","Thermistor_temp(22)","Thermistor_temp(23)",
                  "Thermistor_temp(24)")]
    
    
    ###..........................................................................
    ## step 1.14 merge input data with date table ----
    ##
    ###..........................................................................
    
    newdf.a <- merge(compl.temp,dada,all.x=T, by="UTC")
    
    ###..........................................................................
    ## step 1.15 merge date table with storing table ----
    ##
    ###..........................................................................
    
    for(k in 2:(length(db.sdhole[1,]))){
      db.sdhole[,k]<-rowMeans(cbind(db.sdhole[,k],newdf.a[,k+1]),na.rm=T)#
    }
  }
  
  ###..........................................................................
  ## step 1.16 convert numeric dates back to date format ----
  ##
  ###..........................................................................
  
  
  db.sdhole[,1]<-format( as.POSIXct(db.sdhole[,1],origin=origin,tz="UTC"),format='%Y-%m-%d %H:%M')
  
  ###..........................................................................
  ## step 1.17 set "sparc" colnames ----
  ##
  ###..........................................................................
  
  colnames(db.sdhole)<-c("UTC","batt_U","Tpan_CR800",
                         "Ts_0","Ts_40","Ts_80","Ts_120","Ts_160","Ts_200","Ts_250",
                         "Ts_300","Ts_400","Ts_500","Ts_700","Ts_900","Ts_1100",
                         "Ts_1300","Ts_1500","Ts_2000","Ts_3000","Ts_4000","Ts_5000",
                         "Ts_6000","Ts_7000","Ts_8000","Ts_9000","Ts_10000")
  
  ###..........................................................................
  ## step 1.18 any corrections ----
  ##
  ###..........................................................................
  
  if(year_i==2018){
    # cable incorrectly switched form "2018-07-10 06:00" to "2021-09-21 00:00"
    reihe.a <- db.sdhole[762:1460, "Ts_5000"]
    reihe.b <- db.sdhole[762:1460, "Ts_9000"]
    db.sdhole[762:1460, "Ts_5000"] <- reihe.b
    db.sdhole[762:1460, "Ts_9000"] <- reihe.a
  }else if(year_i%in%c(2019,2020)){
    # cable incorrectly switched form "2018-07-10 06:00" to "2021-09-21 00:00"
    reihe.a <- db.sdhole[, "Ts_5000"]
    reihe.b <- db.sdhole[, "Ts_9000"]
    db.sdhole[, "Ts_5000"] <- reihe.b
    db.sdhole[, "Ts_9000"] <- reihe.a
  }else if(year_i==2021){
    # cable incorrectly switched form "2018-07-10 06:00" to "2021-09-21 00:00"
    reihe.a <- db.sdhole[1:1053, "Ts_5000"]
    reihe.b <- db.sdhole[1:1053, "Ts_9000"]
    db.sdhole[1:1053, "Ts_5000"] <- reihe.b
    db.sdhole[1:1053, "Ts_9000"] <- reihe.a
  }
  
  
  ###..........................................................................
  ## step 1.19 safe data to txt-file ----
  ##
  ###..........................................................................
  
  write.table(db.sdhole[as.numeric(format(as.POSIXct(db.sdhole[,1],format='%Y-%m-%d %H:%M',origin=origin, tz = "UTC"),format='%Y'))==year_i,-2],
              paste0(p.1$w[p.1$n=="LV0.p"],"SdHole2009/00_full_dataset/SdHole2009_",year_i,"_lv0.dat"),quote = F,dec=".",sep=",",row.names=F)
  
  cat("#\n# level0 SdHole2009: ",year_i,"without problems!\n#\n")
  
}# end loop over years




