###.........................................................................
##
##   SaPrec2019         RAW to Level0 ----
##   
##   equal time steps, no gaps
##
##   by: Stephan.Lange@awi.de
##   last modified: 2021-05-28
##
###.........................................................................
##
##
# "UTC", ----------------"UTC",
# "RECORD", -------------"RECORD",
# "PrecPulse_Tot",-------"Counts_Tot",
# "Prec_NRT_Tot",--------"prec_NRT",
# "Prec_Total_NRT",------"prec",
# "Prec_Bucket_NRT",-----"prec_bucket",
# "Pluvio_heating_Max",--"heat_max",
# "Pluvio_status_Max",---"status_max",
# "Pluvio_Temp_E_Min",---"Tpan_min",
# "Pluvio_Volt_Min",-----"U_min",
# "Pluvio_Temp_Ring_Avg"-"Tring"
#
##

###.........................................................................
## step 1.01 set path settings for different systems linux vs. windoof ----
##
###.........................................................................
# to run this script seperate, you have to uncomment the next 10 lines!
rm(list = ls())
if (.Platform$OS.type == "windows") {
  p.1 <- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/path_win.txt", sep = "\t", header = T)
  p.1maint <- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)

  source("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_func.R")
} else {
  p.1 <- read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
  p.1maint <- read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)

  source("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_func.R")
}
###.........................................................................
## step 1.02 set running options years, ... ----
##
###.........................................................................
options(scipen=100) # for non-exponential display of numeric values
origin  <- "1970-01-01"
aktuell <-as.numeric(format(Sys.Date(),"%Y"))
###.........................................................................
## step 1.03 loop 1 over years ----
##
###.........................................................................
for (year in 2019:aktuell){#2013:2016 2012:aktuell
  ###.........................................................................
  ## step 1.04 set 2 empty tables with length of year ----
  ##
  ## columns: 2 (date table) and number of input table (storing table)
  ###.........................................................................
  cat("\nProcessing year",year,"\n====================\n\n")
  start.date <-as.POSIXct(paste(year,"-01-01 00:00:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date   <-as.POSIXct(paste(year,"-",12,"-",31," 23:30:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
  read.int <- "1 h" # reading interval of measurements
  ###.........................................................................
  # set number of columns in db.saprec ==> check step 1.17 length(c("UTC","batt_U","Tpan_CR1000","Tpan_SPA","ice_2","ice_3","ice_4","water_2","water_3","water_4","rho_2","rho_3","rho_4","cap_highFq_2","cap_lowFq_2","cap_highFq_3","cap_lowFq_3","cap_highFq_4","cap_lowFq_4","phi_highFq_2","phi_lowFq_2","phi_highFq_3","phi_lowFq_3","phi_highFq_4","phi_lowFq_4","distcor_0","distcor_1","distcor_2","distcor_3","distcor_4","distcor_5","distcor_6","distcor_7","distcor_8","distcor_9","distcor_Centre","distcor_Crack","distraw_0","distraw_1","distraw_2","distraw_3","distraw_4","distraw_5","distraw_6","distraw_7","distraw_8","distraw_9","distraw_Centre","distraw_Crack","QA_dist_0","QA_dist_1","QA_dist_2","QA_dist_3","QA_dist_4","QA_dist_5","QA_dist_6","QA_dist_7","QA_dist_8","QA_dist_9","QA_dist_Centre","QA_dist_Crack","Tsn_m4","Tsn_00","Tsn_05","Tsn_10","Tsn_15","Tsn_20","Tsn_25","Tsn_30","Tsn_35","Tsn_40","Tsn_45","Tsn_50","Ts_00_1","Ts_00_2","Ts_00_3","Ts_00_4","Tair_80","Ts_00","Ts_05","Ts_10","Ts_20","Ts_40","Ts_60","Ts_80","Ts_100" ))
  ncol.db.saprec <- 11
  # create empty data frame with UTC time stamps according to the reading interval
  db.saprec<-matrix(ncol=ncol.db.saprec,nrow=length(seq(start.date,end.date,by=read.int)),-999)
  db.saprec[,c(2:ncol.db.saprec)]<-NA
  compl.temp<-matrix(ncol=2,nrow=length(seq(start.date,end.date,by=read.int)))
  db.saprec[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by=read.int),format='%Y-%m-%d %H:%M:%S'))
  compl.temp[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by=read.int),format='%Y-%m-%d %H:%M:%S'))
  colnames(compl.temp)<-c("UTC","erste")
  ###.........................................................................
  ## step 1.05 set input.path and list all files ----
  ##
  ###.........................................................................
  inz.path<-paste0(p.1$w[p.1$n=="RAW.p"],"SaPrec2019/")
  files2read<-list.files(inz.path,pattern="*.dat")
  ###.........................................................................
  ## step 1.06 loop 2 over all files ----
  ##
  ###.........................................................................

  for(i in 1:length(files2read)){#1:length(files2read)
    ###.........................................................................
    ## step 1.07 read one file (skip headers, set NA-values) ----
    ##
    ## set temporal colnames
    ###.........................................................................
    cat("\nprocessing ",files2read[i],"\n====================\n\n")
    dada<-read.table(paste(inz.path,files2read[i],sep=""),sep=",",dec=".",header=F,skip=4, fill = TRUE,na="NAN")

    colnames(dada) = paste0("V",seq_len(ncol(dada)))
    ###.........................................................................
    ## step 1.09 check file if dates are in running year of loop 1 ----
    ##
    ###.........................................................................

    if(as.numeric(substr(lapply(dada[1,1],as.character),1,4))>year || as.numeric(substr(lapply(dada[length(dada[,1]),1],as.character),1,4))<year) {next} # skip file if wrong year
    cat(paste(dada[1,1],"     to     ",dada[length(dada[,1]),1],"    ",files2read[i]))
    ###.........................................................................
    ## step 1.10 check file for double entries ----
    ##
    ###.........................................................................
    if(("TRUE" %in% duplicated(dada))==TRUE) { # check for fully double entries
      doouble <- duplicated(dada)
      cat(paste(length(which(doouble=="TRUE")),"duplicated records found in file",files2read[i],"\n",
                "first entry:",dada[which(doouble=="TRUE")[1],1],"\n  last entry:",dada[which(doouble=="TRUE")[length(which(doouble=="TRUE"))],1],"\n\n"))
      dada <- unique(dada)  # remove double entries

    } else if(("TRUE" %in% duplicated(dada[,1]))==TRUE){  # check for multiple different data records for same! timestamp
      doouble <- duplicated(dada[,1])
      cat(paste(length(which(doouble=="TRUE")),"multiple records found in file",files2read[i],"\n",
                "first entry:",dada[which(doouble=="TRUE")[1],1],"\n  last entry:",dada[which(doouble=="TRUE")[length(which(doouble=="TRUE"))],1],"\n\n"))
      dd<-which(dada[,1] %in% dada[which(doouble=="TRUE"),1])
      dada <- dada[-dd,]  # remove double entries

    } else { cat("No double data entries found in",files2read[i],"\n\n")    }
    ###.........................................................................
    ## step 1.11 convert date to numeric value ----
    ##
    ###.........................................................................

    dada[,1]<-as.numeric(as.POSIXct(dada[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC"))
      ###.........................................................................
      ## step 1.12 standard case ----
      ##
      ## set original colnames
      ###.........................................................................

      colnames(dada)<-c("UTC","RECORD","PrecPulse_Tot","Prec_NRT_Tot","Prec_Total_NRT","Prec_Bucket_NRT","Pluvio_heating_Max","Pluvio_status_Max","Pluvio_Temp_E_Min","Pluvio_Volt_Min","Pluvio_Temp_Ring_Avg"
)
    
    ###.........................................................................
    ## step 1.13 new arrangement / order of columns ----
    ## (all Temperatures together, ascending, ... )
    ###.........................................................................
    #dada<-dada[,c("UTC","RECORD","PrecPulse_Tot","Prec_NRT_Tot","Prec_Total_NRT","Prec_Bucket_NRT","Pluvio_heating_Max","Pluvio_status_Max","Pluvio_Temp_E_Min","Pluvio_Volt_Min","Pluvio_Temp_Ring_Avg"
#)] # 
    ###.........................................................................
    ## step 1.14 merge input data with date table ----
    ##
    ###.........................................................................

    newdf.a <- merge(compl.temp,dada,all.x=T, by="UTC")

    ###.........................................................................
    ## step 1.15 merge date table with storing table ----
    ##
    ###.........................................................................

    for(k in 2:(length(dada[1,]))){
      db.saprec[,k]<-rowMeans(cbind(db.saprec[,k],newdf.a[,k+1]),na.rm=T)#
    }
  }

  ###.........................................................................
  ## step 1.16 convert numeric dates back to date format ----
  ##
  ###.........................................................................


  db.saprec[,1]<-format( as.POSIXct(db.saprec[,1],origin=origin,tz="UTC"),format='%Y-%m-%d %H:%M')

  ###.........................................................................
  ## step 1.17 set "sparc" colnames ----
  ##
  ###.........................................................................

  colnames(db.saprec)<-c("UTC","RECORD","Counts_Tot","prec_NRT","prec","prec_bucket",
                         "heat_max","status_max","Tpan_min","U_min","Tring")

  ###.........................................................................
  ## step 1.18 Set NAN to NA ----
  ##
  ###.........................................................................

  for(val in 2:ncol.db.saprec){
  db.saprec[which(is.nan(as.numeric(db.saprec[,(val)]))==TRUE),val] <- NA   # set NAN to NA
  }
  

  ###.........................................................................
  ## step 1.19 safe data to txt-file ----
  ##
  ###.........................................................................

  write.table(db.saprec[as.numeric(format(as.POSIXct(db.saprec[,1],format='%Y-%m-%d %H:%M',origin=origin, tz = "UTC"),format='%Y'))==year, ],
              paste0(p.1$w[p.1$n=="LV0.p"],"SaPrec2019/00_full_dataset/SaPrec2019_",year,"_lv0.dat"),quote = F,dec=".",sep=",",row.names=F)


} # end loop over years

cat("\n#\n# SaPrec2019 ",year," without problems!\n#\n")


