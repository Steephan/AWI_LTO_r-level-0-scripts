#############################################################################
##
##   SaSnow2016         RAW to Level0
##   (SaSnow2012)
##   equal time steps, no gaps
##
##   by: Stephan.Lange@awi.de
##       Niko.Bornemann@awi.de
##
##   last modified: 2016-12-xx
##
#############################################################################

##
##
##   18 steps to get wonderful data
##
##
##

#############################################################################
## step 1.01
## set path settings for different systems linux vs. windoof
#############################################################################
# to run this script seperat, you have to uncomment the next 10 lines!
rm(list=ls())
if (.Platform$OS.type == "windows") {
  path<-read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt",sep="\t",header=T)
  maint<-read.table("N:/sparc/LTO/R_database/database_R/settings/sa_maintance.txt",sep="\t",header=T)
  source("N:/sparc/LTO/R_database/database_R/settings/db_func.R")
}else{
  path<-read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt",sep="\t",header=T, fileEncoding="UTF-8")
  maint<-read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt",sep="\t",header=T)
  source("/sparc/LTO/R_database/database_R/settings/db_func.R")
}
#############################################################################
## step 1.02
## set running options years, ...
#############################################################################
options(scipen=100) # for non-exponential display of numeric values
origin  <- "1970-01-01"
aktuell <-as.numeric(format(Sys.Date(),"%Y"))
#############################################################################
## step 1.03
## loop 1 over years
#############################################################################
for (year in 2016:aktuell){#2016:aktuell
  #############################################################################
  ## step 1.04
  ## set 2 empty tables with length of year
  ## columns: 2 (date table) and number of input table (storing table)
  #############################################################################
  cat("\nProcessing year",year,"\n====================\n\n")
  start.date <-as.POSIXct(paste(year,"-01-01 00:00:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date   <-as.POSIXct(paste(year,"-",12,"-",31," 23:30:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
  # create empty data frame with UTC time stamp every 30 min
  db.sasnow<-matrix(ncol=10,nrow=length(seq(start.date,end.date,by="30 min")),-999)
  db.sasnow[,c(2:10)]<-NA
  compl.temp<-matrix(ncol=2,nrow=length(seq(start.date,end.date,by="30 min")))
  db.sasnow[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="30 min"),format='%Y-%m-%d %H:%M:%S'))
  compl.temp[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="30 min"),format='%Y-%m-%d %H:%M:%S'))
  colnames(compl.temp)<-c("UTC","erste")
  #############################################################################
  ## step 1.05
  ## set input.path and list all files
  #############################################################################
  inz.path   <- paste0(path$w[path$n=="RAW.p"],"SaSnow2016/")
  files2read <- list.files(inz.path,pattern="*.dat")
  #############################################################################
  ## step 1.06
  ## loop 2 over all files
  #############################################################################

  for(i in 1:length(files2read)){#1:length(files2read)
    #############################################################################
    ## step 1.07
    ## read one file (skip headers, set NA-values)
    ## set temporal colnames
    #############################################################################
    cat("\nprocessing ",files2read[i],"\n====================\n\n")
    dada <- read.table(paste(inz.path,files2read[i],sep=""),sep=",",dec=".",header=F,skip=4, fill = TRUE,na="NAN")

    colnames(dada) = paste0("V",seq_len(ncol(dada)))
    #############################################################################
    ## step 1.09
    ## check file if dates are in running year of loop 1
    #############################################################################

    if(as.numeric(substr(lapply(dada[1,1],as.character),1,4))>year || as.numeric(substr(lapply(dada[length(dada[,1]),1],as.character),1,4))<year) {next} # skip file if wrong year
    cat(paste(dada[1,1],"     to     ",dada[length(dada[,1]),1],"    ",files2read[i]))
    #############################################################################
    ## step 1.10
    ## check file for double entries
    #############################################################################
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
    #############################################################################
    ## step 1.11
    ## convert date to numeric value
    #############################################################################

    dada[,1]<-as.numeric(as.POSIXct(dada[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC"))
    #############################################################################
    ## step 1.12a
    ## special case: former files with different columns
    ## set colnames
    #############################################################################
    if(length(dada[1,])==10){
      colnames(dada)<-  c("UTC","RECORD","batt_volt_Min","PTemp_SMM","Air_T_Avg",
                          "Air_T_Std","Raw_Dist_Avg","SignalQuality_Avg",
                          "SignalQuality_Std","T_Corr_Dist_Avg")
     #############################################################################
     ## step 1.12b
     ## add additional columns to former dataset
     #############################################################################

      #dada$S2_ice_Avg  <-NA


    }else{
      #############################################################################
      ## step 1.12
      ## standard case
      ## set original colnames
      #############################################################################

      #colnames(dada)<-c("UTC","RECORD","BattV_Min")
    }
    #############################################################################
    ## step 1.13
    ## new arrangement / order of columns (all Temperatures together, ascending, ... )
    #############################################################################
    dada<-dada[,c("UTC","batt_volt_Min","PTemp_SMM","Air_T_Avg",
                  "Air_T_Std","Raw_Dist_Avg","SignalQuality_Avg",
                  "SignalQuality_Std","T_Corr_Dist_Avg")]
    #############################################################################
    ## step 1.14
    ## merge input data with date table
    #############################################################################

    newdf.a <- merge(compl.temp,dada,all.x=T, by="UTC")

    #############################################################################
    ## step 1.15
    ## merge date table with storing table
    #############################################################################

    for(k in 2:(length(db.sasnow[1,])-1)){
      db.sasnow[,k]<-rowMeans(cbind(db.sasnow[,k],newdf.a[,k+1]),na.rm=T)#
    }
  }
  
  #############################################################################
  ## step 1.16
  ## convert numeric dates back to date format
  #############################################################################
  
  
  db.sasnow[,1]<-format( as.POSIXct(db.sasnow[,1],origin=origin,tz="UTC"),format='%Y-%m-%d %H:%M')
  
  #############################################################################
  ## step 1.17
  ## set "sparc" colnames
  #############################################################################
  
  colnames(db.sasnow)<-c("UTC","batt_U","Tpan_CR1000","Tair_170","Tair_sd_170",
                         "distraw","QA","QA_sd","distcor","Dsn")
  
  #############################################################################
  ## step 1.16
  ## calculate Snowheight from distcorr
  #############################################################################
  for(val in 2:9){
    db.sasnow[which(is.nan(as.numeric(db.sasnow[,(val)]))==TRUE),val] <- NA   # set NA to NAN
  }


  if(year==2016) {###  special calculation of snowdepth
    ####
    snow.free   <- c(which(db.sasnow[,1]=="2016-07-27 08:00"),
                     which(db.sasnow[,1]=="2016-07-27 08:00")) # vector of two timesteps
    # two vectors for snowdepth calculation
    spring.corr <- c(1.905) # the old from last year ####### Dsn correction #########
    autum.corr  <- c(1.905) # the new one based on maximum dist in august ######## distcor correction #########

    db.sasnow[1:snow.free[1],10] <- round(spring.corr[1]-as.numeric(db.sasnow[1:snow.free[1],9]),3)
    #db.sasnow[snow.free[1]:snow.free[2],9*i]<-0 # snowfree
    db.sasnow[snow.free[2]:length(db.sasnow[,1]),10] <-  round(autum.corr[1]-as.numeric(db.sasnow[snow.free[2]:length(db.sasnow[,1]),9]),3)

  }else if(year==2017) {###  special calculation of snowdepth
    ####
    snow.free   <- c(which(db.sasnow[,1]=="2017-07-27 08:00"),
                     which(db.sasnow[,1]=="2017-07-27 08:00")) # vector of two timesteps
    # two vectors for snowdepth calculation
    spring.corr <- c(1.905) # the old from last year ####### Dsn correction #########
    autum.corr  <- c(1.905) # the new one based on maximum dist in august ######## distcor correction #########
    
    db.sasnow[1:snow.free[1],10] <- round(spring.corr[1]-as.numeric(db.sasnow[1:snow.free[1],9]),3)
    #db.sasnow[snow.free[1]:snow.free[2],9*i]<-0 # snowfree
    db.sasnow[snow.free[2]:length(db.sasnow[,1]),10] <-  round(autum.corr[1]-as.numeric(db.sasnow[snow.free[2]:length(db.sasnow[,1]),9]),3)
    
  }else if(year==2018) {###  special calculation of snowdepth
    ####
    snow.free   <- c(which(db.sasnow[,1]=="2018-07-27 08:00"),
                     which(db.sasnow[,1]=="2018-07-27 08:00")) # vector of two timesteps
    # two vectors for snowdepth calculation
    spring.corr <- c(1.905) # the old from last year ####### Dsn correction #########
    autum.corr  <- c(1.905) # the new one based on maximum dist in august ######## distcor correction #########
    
    db.sasnow[1:snow.free[1],10] <- round(spring.corr[1]-as.numeric(db.sasnow[1:snow.free[1],9]),3)
    #db.sasnow[snow.free[1]:snow.free[2],9*i]<-0 # snowfree
    db.sasnow[snow.free[2]:length(db.sasnow[,1]),10] <-  round(autum.corr[1]-as.numeric(db.sasnow[snow.free[2]:length(db.sasnow[,1]),9]),3)
    
  }else if(year==2019) {###  special calculation of snowdepth
    ####
    snow.free   <- c(which(db.sasnow[,1]=="2019-07-27 08:00"),
                     which(db.sasnow[,1]=="2019-07-27 08:00")) # vector of two timesteps
    # two vectors for snowdepth calculation
    spring.corr <- c(1.905) # the old from last year ####### Dsn correction #########
    autum.corr  <- c(1.905) # the new one based on maximum dist in august ######## distcor correction #########
    
    db.sasnow[1:snow.free[1],10] <- round(spring.corr[1]-as.numeric(db.sasnow[1:snow.free[1],9]),3)
    #db.sasnow[snow.free[1]:snow.free[2],9*i]<-0 # snowfree
    db.sasnow[snow.free[2]:length(db.sasnow[,1]),10] <-  round(autum.corr[1]-as.numeric(db.sasnow[snow.free[2]:length(db.sasnow[,1]),9]),3)
    
  }


  #############################################################################
  ## step 1.18
  ## safe data to txt-file
  #############################################################################

  write.table(db.sasnow[as.numeric(format(as.POSIXct(db.sasnow[,1],format='%Y-%m-%d %H:%M',origin=origin, tz = "UTC"),format='%Y'))==year,],
              paste0(path$w[path$n=="LV0.p"],"SaSnow2016/00_full_dataset/SaSnow2016_",year,"_lv0.dat"),quote = F,dec=".",sep=",",row.names=F)

cat("\n#\n# SaSnow2016 ",year,"without problems!\n#\n")
} # end loop over years




