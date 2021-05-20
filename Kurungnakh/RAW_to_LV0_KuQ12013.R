###..........................................................................
##
##  KuQ1      RAW to level 0 ----
##
##
##
##
##
##  last modified: 19-03-2019
##
###..........................................................................
#
# Erklaerung Tabellenkopf:
# Dist_mV_Avg - Distanz als Impuls in milliVolt
# Dist_m_5_Avg - Distanz zum Wasserlevel in m (berechnet aus bezogen auf 5m offset)
# (-> siehe Nikos Programmierskript bei Software)
# T109_C_Avg/Tsen_VEGA_Puls ist die Temperatur des Sonsors/ Geraetes
# BattV_Min scheint nicht wichtig zu sein
#
# Erweiterung des Tabellenkopfes:
# WT_mm - Pegel in m, entspricht SH1 in der Gleichung
# Q - berechneter Abfluss in l/s entsprechend der Glechung hier:
#   http://sparcwiki.awi-potsdam.de/doku.php?id=public:sensors:rbc_flumes#calculation

###..........................................................................
## step 1.01 set path ----
## settings for different systems linux vs. windoof
###..........................................................................
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
###..........................................................................
## step 1.02 set running options years, ... ----
##
###..........................................................................
options(scipen=100) # for non exonential display of numeric values
origin  <- "1970-01-01"
aktuell <-as.numeric(format(Sys.Date(),"%Y"))
#run.year<-2013
###..........................................................................
## step 1.03 loop 1 over years ----
##
###..........................................................................
for(year in run.year){ #2013:aktuell
  
  ###..........................................................................
  ## step 1.04 set 2 empty tables ----
  ## with length of year
  ## columns: 2 (date table) and number of input table
  ###..........................................................................
  #cat("\nProcessing year",year,"\n====================\n\n")
  start.date <- as.POSIXct(paste(year,"-01-01 00:00:00",sep=""),format="%Y-%m-%d %H:%M:%S", tz = "UTC")
  end.date   <- as.POSIXct(paste(year,"-12-31 23:50:00",sep=""),format="%Y-%m-%d %H:%M:%S", tz = "UTC")
  # create empty data frame with UTC time stamp every 10 min
  db.kuq1<-matrix(ncol=6,nrow=length(seq(start.date,end.date,by="10 min")))
  db.kuq1[,c(2:6)]<-NA
  compl.q<-matrix(ncol=2,nrow=length(seq(start.date,end.date,by="10 min")))
  db.kuq1[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="10 min"),format="%Y-%m-%d %H:%M:%S"))
  compl.q[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="10 min"),format="%Y-%m-%d %H:%M:%S"))
  colnames(compl.q)<-c("UTC","erste")
  #colnames(db.kuq1)<-c("UTC")
  
  ###..........................................................................
  ## step 1.05 ----
  ## set input.path and list all files
  ###..........................................................................
  files2read<-list.files(paste0(p.1$w[p.1$n == "RAW.p"],"KuQ12013/"),pattern="*.dat")
  
  ###..........................................................................
  ## step 1.06 loop 2 over years ----
  ##
  ###..........................................................................
  for(i in 1:length(files2read)){
    
    ###..........................................................................
    ## step 1.07 read one file ----
    ## (skip headers, set NA-values)
    ## set temporal colnames
    ###..........................................................................
    #cat("\nprocessing ",files2read[i],"\n====================\n\n")
    dada<-read.table(paste0(p.1$w[p.1$n == "RAW.p"],"KuQ12013/",files2read[i]),sep=",",dec=".",header=F,skip=4,fill=T,na.strings = "n/a")
    colnames(dada)<-paste("V",seq_len(ncol(dada)))
    
   
    ###..........................................................................
    ## step 1.09 check file ----
    ## if dates are in running years of loop 1
    ###..........................................................................
    # skip file if wrong year
    if(as.numeric(substr(lapply(dada[1,1],as.character),1,4))>year || as.numeric(substr(lapply(dada[length(dada[,1]),1],
                                                                                               as.character),1,4))<year)
    {next}
    #cat(paste(dada[1,1],"to",dada[length(dada[,1]),1],"",files2read[i]))
    
    ###..........................................................................
    ## step 1.10 ----
    ## check file for double entries
    ###..........................................................................
    if("TRUE" %in% duplicated(dada) == TRUE){ # check for fully double entries
      double<-duplicated(dada)
      cat(paste(length(which(double=="TRUE")),"duplicated records found in file",files2read[i],"\n",
                "first entry:",dada(which(double=="TRUE")[1],1),"\n last entry:",dada(which(double=="TRUE")
                                                                                      [length(which(double=="TRUE"))]),"\n\n"))
      dada<-unique(dada) # remove double entries
      
    } else if (("TRUE" %in% duplicated(dada[,1])) == TRUE){ # chech for multiple entries with the same timestamp
      double<-duplicated(dada[,1])
      cat(paste(length(which(double=="TRUE")),"multiple records found in file",files2read[i],"\n",
                "first entry:",dada[which(double=="TRUE")[1],1],"\n last entry:",dada[which(double=="TRUE")
                                                                                      [length(which(double=="TRUE"))],1],"\n\n"))
      dd<-which(dada[,1] %in% dada[which(double=="TRUE"),1])
      dada<-dada[-dd,] # remove multiple records
      
    } else {
      #cat(paste("\n","no double entries found in file",files2read[i],"\n\n"))
    }
    
    ###..........................................................................
    ## step 1.11 ----
    ## convert date to numeric vector
    ###..........................................................................
    dada[,1]<-as.numeric(as.POSIXct(dada[,1],format="%Y-%m-%d %H:%M:%S", origin=origin, tz="UTC"))
    
    ###..........................................................................
    ## step 1.12 set original colnames ----
    ##
    ###..........................................................................
    colnames(dada)<-c("UTC","RECORD","BattV_Min","Dist_mV_Avg","Dist_m_5_Avg","T109_C_Avg")
    
    
    ###..........................................................................
    ## step 1.14 ----
    ## merge input table with date table
    ###..........................................................................
    newdf.a <- merge(compl.q,dada,all.x=T,by="UTC")
    
    ###..........................................................................
    ## step 1.15 ----
    ## merge date table with storing table
    ###..........................................................................
    #db.kuq1 <- merge(newdf.a,db.kuq1,all.x=T,by="UTC")
    
    for(k in 2:(length(db.kuq1[1,]))){
     db.kuq1[,k]<-newdf.a[,k+1]
    }
  }
  
  # write values into first columns and remove merging columns
  # if(m>1){
  #   for (k in 1:(m-1)){
  #     for(l in 2:(length(newdf.a[1,]))){
  #       for (s in 1:length(db.kuq1[,1])){
  #         if (is.na(db.kuq1[s,l])){
  #           db.kuq1[s,l]<-db.kuq1[s,(l+length(newdf.a[1,])-1)]
  #         }
  #       }
  #     }
  #     db.kuq1<-db.kuq1[,-c((length(newdf.a)+1):(2*length(newdf.a)-1))]
  #   }
  # }
  #db.kuq1<-db.kuq1[,-2]
  
  # convert matrix to data frame
  db.kuq1<-as.data.frame(db.kuq1)
  
  ###..........................................................................
  ## step 1.16 ----
  ## convert numeric dates back to date format
  ###..........................................................................
  db.kuq1[,1]<-format(as.POSIXct(db.kuq1[,1],origin=origin,tz="UTC"),format="%Y-%m-%d %H:%M:%S")
  
  ###..........................................................................
  ## step 1.16a calculate discharge ----
  ##
  ###..........................................................................
  # Distance sensor to flume: 774.5mm
  # Dis_m_5_Avg gives distance sensor to waterlevel (has to be in the fifth column!)
  # distance flume to waterlevel (SH1) is needed, so
  db.kuq1 <- cbind(db.kuq1, "ref_wl_mm_SH1"=77.45 - (as.numeric(db.kuq1[,5])*100))

  # Warning message:
  #   In sqrt(as.numeric(db.kuq1[, 7] * 10)) : NaNs produced
  db.kuq1 <- suppressWarnings(cbind(db.kuq1,"Q" = 0.0000004 * (as.numeric(db.kuq1[,7]*10))^3 +
                   0.0011 * (as.numeric(db.kuq1[,7]*10))^2 +
                   0.1358 * as.numeric(db.kuq1[,7]*10) -
                   sqrt(as.numeric(db.kuq1[,7]*10)) + 3.488))
  
  ###..........................................................................
  ## step 1.16b deal with NaN from calculation ----
  ##
  ###..........................................................................
  #define NaN's from sqrt as NA, changes column to character
  # db.kuq1[,8][grep("NaN",db.kuq1[,8])]<-"NA"
  # #convert character back to numerical
  # db.kuq1[,8]<-as.numeric(db.kuq1[,8])
  
  
  ###..........................................................................
  ## step 1.17 set "sparc" colnames ----
  ## (in this case: set column names)
  ###..........................................................................
  colnames(db.kuq1)<-c("UTC","RECORD","BattV_Min","Dist_mV_Avg","Dist_m_5_Avg","Tsen_VEGA_Puls","WT","Q")
  
  ###..........................................................................
  ## step 1.18 save data to txt-file ----
  ## 
  ###..........................................................................
  write.table(db.kuq1,
              paste0(p.1$w[p.1$n == "LV0.p"],"KuQ12013/00_full_dataset/KuQ12013_",year,"_lv0.dat"),quote=F,dec=".",sep=",",row.names=F)
  cat("\n#\n# KuQ12013", year,"without problems!\n#\n")
}


###..........................................................................
