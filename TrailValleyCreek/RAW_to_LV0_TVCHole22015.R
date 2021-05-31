###.........................................................................
##
##   Template         RAW to Level0 ----
##   (SaSnow2012)
##   equal time steps, no gaps
##
##   by: Stephan.Lange@awi.de
##   last modified: 2021-05-20
##
###.........................................................................

##
##
##   19 steps to get wonderful data
##
##
##

###.........................................................................
## step 1.01 set path settings for different systems linux vs. windoof ----
##
###.........................................................................
# to run this script seperate, you have to uncomment the next 10 lines!
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
###.........................................................................
## step 1.02 set running options years, ... ----
##
###.........................................................................
options(scipen=100) # for non-exponential display of numeric values
origin  <- "1970-01-01"
aktuell <-as.numeric(format(Sys.Date(),"%Y"))
 # run.year <- 2015:2021 #2015:aktuell
###.........................................................................
## step 1.03 loop 1 over years ----
##
###.........................................................................
for (year_i in run.year){#2013:2016 2012:aktuell
  ###.........................................................................
  ## step 1.04 set 2 empty tables with length of year ----
  ##
  ## columns: 2 (date table) and number of input table (storing table)
  ###.........................................................................
  cat("\nProcessing year",year_i,"\n====================\n\n")
  start.date <-as.POSIXct(paste(year_i,"-01-01 00:00:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date   <-as.POSIXct(paste(year_i,"-",12,"-",31," 23:30:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
  read.int <- "1 h" # reading interval of measurements
  ###.........................................................................
  # set number of columns in db.tvchole ==> check step 1.17 length(c("UTC","batt_U","Tpan_CR1000","Tpan_SPA","ice_2","ice_3","ice_4","water_2","water_3","water_4","rho_2","rho_3","rho_4","cap_highFq_2","cap_lowFq_2","cap_highFq_3","cap_lowFq_3","cap_highFq_4","cap_lowFq_4","phi_highFq_2","phi_lowFq_2","phi_highFq_3","phi_lowFq_3","phi_highFq_4","phi_lowFq_4","distcor_0","distcor_1","distcor_2","distcor_3","distcor_4","distcor_5","distcor_6","distcor_7","distcor_8","distcor_9","distcor_Centre","distcor_Crack","distraw_0","distraw_1","distraw_2","distraw_3","distraw_4","distraw_5","distraw_6","distraw_7","distraw_8","distraw_9","distraw_Centre","distraw_Crack","QA_dist_0","QA_dist_1","QA_dist_2","QA_dist_3","QA_dist_4","QA_dist_5","QA_dist_6","QA_dist_7","QA_dist_8","QA_dist_9","QA_dist_Centre","QA_dist_Crack","Tsn_m4","Tsn_00","Tsn_05","Tsn_10","Tsn_15","Tsn_20","Tsn_25","Tsn_30","Tsn_35","Tsn_40","Tsn_45","Tsn_50","Ts_00_1","Ts_00_2","Ts_00_3","Ts_00_4","Tair_80","Ts_00","Ts_05","Ts_10","Ts_20","Ts_40","Ts_60","Ts_80","Ts_100" ))
  ncol.db.tvchole <- 5
  # create empty data frame with UTC time stamps according to the reading interval
  db.tvchole<-matrix(ncol=ncol.db.tvchole,nrow=length(seq(start.date,end.date,by=read.int)),-999)
  db.tvchole[,c(2:ncol.db.tvchole)]<-NA
  compl.temp<-matrix(ncol=2,nrow=length(seq(start.date,end.date,by=read.int)))
  db.tvchole[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by=read.int),format='%Y-%m-%d %H:%M:%S'))
  compl.temp[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by=read.int),format='%Y-%m-%d %H:%M:%S'))
  colnames(compl.temp)<-c("UTC","erste")
  ###.........................................................................
  ## step 1.05 set input.path and list all files ----
  ##
  ###.........................................................................
  inz.path<-paste0(p.1$w[p.1$n=="RAW.p"],"TVCHole22015/")
  files2read<-list.files(inz.path,pattern="*.csv")
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
    dada<-read.table(paste(inz.path,files2read[i],sep=""),sep=",",dec=".",header=F,skip=2, fill = TRUE,na="NAN")[,-1]

    colnames(dada) = paste0("V",seq_len(ncol(dada)))
    ###.........................................................................
    ## step 1.09 check file if dates are in running year of loop 1 ----
    ##
    ###.........................................................................

    # if(as.numeric(substr(lapply(dada[1,1],as.character),1,4))>year_i || as.numeric(substr(lapply(dada[length(dada[,1]),1],as.character),1,4))<year_i) {next} # skip file if wrong year
    # cat(paste(dada[1,1],"     to     ",dada[length(dada[,1]),1],"    ",files2read[i]))
    ###.........................................................................
    ## step 1.10 check file for double entries ----
    ##
    ###.........................................................................
    # if(("TRUE" %in% duplicated(dada))==TRUE) { # check for fully double entries
    #   doouble <- duplicated(dada)
    #   cat(paste(length(which(doouble=="TRUE")),"duplicated records found in file",files2read[i],"\n",
    #             "first entry:",dada[which(doouble=="TRUE")[1],1],"\n  last entry:",dada[which(doouble=="TRUE")[length(which(doouble=="TRUE"))],1],"\n\n"))
    #   dada <- unique(dada)  # remove double entries
    # 
    # } else if(("TRUE" %in% duplicated(dada[,1]))==TRUE){  # check for multiple different data records for same! timestamp
    #   doouble <- duplicated(dada[,1])
    #   cat(paste(length(which(doouble=="TRUE")),"multiple records found in file",files2read[i],"\n",
    #             "first entry:",dada[which(doouble=="TRUE")[1],1],"\n  last entry:",dada[which(doouble=="TRUE")[length(which(doouble=="TRUE"))],1],"\n\n"))
    #   dd<-which(dada[,1] %in% dada[which(doouble=="TRUE"),1])
    #   dada <- dada[-dd,]  # remove double entries
    # 
    # } else { cat("No double data entries found in",files2read[i],"\n\n")    }
    ###.........................................................................
    ## step 1.11 convert date to numeric value ----
    ##
    ###.........................................................................

    dada[,1]<-as.numeric(as.POSIXct(dada[,1],format='%y.%m.%d %H:%M:%S',origin=origin, tz = "UTC"))
      ## step 1.12 standard case ----
      ##
      ## set original colnames
      ###.........................................................................

      colnames(dada)<-c("UTC","Ts_4","Ts_20","Ts_117","Ts_217")
    
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
      db.tvchole[,k]<-rowMeans(cbind(db.tvchole[,k],newdf.a[,k+1]),na.rm=T)#
    }
  }

  ###.........................................................................
  ## step 1.16 convert numeric dates back to date format ----
  ##
  ###.........................................................................


  db.tvchole[,1]<-format( as.POSIXct(db.tvchole[,1],origin=origin,tz="UTC"),format='%Y-%m-%d %H:%M')

  ###.........................................................................
  ## step 1.17 set "sparc" colnames ----
  ##
  ###.........................................................................

  colnames(db.tvchole)<-c("UTC","Ts_4","Ts_27","Ts_118","Ts_218")

  ###.........................................................................
  ## step 1.18 Set NAN to NA ----
  ##
  ###.........................................................................

  for(val in 2:ncol.db.tvchole){
  db.tvchole[which(is.nan(as.numeric(db.tvchole[,(val)]))==TRUE),val] <- NA   # set NAN to NA
  }
  

  ###.........................................................................
  ## step 1.19 safe data to txt-file ----
  ##
  ###.........................................................................

  write.table(db.tvchole[as.numeric(format(as.POSIXct(db.tvchole[,1],format='%Y-%m-%d %H:%M',origin=origin, tz = "UTC"),format='%Y'))==year_i, ],
              paste0(p.1$w[p.1$n=="LV0.p"],"TVCHole22015/00_full_dataset/TVCHole22015_",year_i,"_lv0.dat"),quote = F,dec=".",sep=",",row.names=F)

  cat("\n#\n# TVCHole12015 ",year_i,"without problems!\n#\n")
} # end loop over years




