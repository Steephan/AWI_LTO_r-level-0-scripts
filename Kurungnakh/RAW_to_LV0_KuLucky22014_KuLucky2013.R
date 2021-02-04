##################################################################
##
##  KuLucky22014      RAW to level 0
##   including : KuLucky2013
##
##
##
##
##  last modified: 23-04-2019
##
##################################################################


##################################################################
## step 1.01
## set path settings for different systems linux vs. windoof
##################################################################
# to run this script seperate, you have to uncomment the next 10 lines!
rm(list=ls())
#if (.Platform$OS.type == "windows") {
#  path<-read.table("N:/geo5/SoilData/doc/scripts/database_R/settings/sa_path_windoof.txt",sep="\t",header=T)
#  maint<-read.table("N:/geo5/SoilData/doc/scripts/database_R/settings/sa_maintance.txt",sep="\t",header=T)
#  source("N:/geo5/SoilData/doc/scripts/database_R/settings/db_func.R")
#}else{
#  path<-read.table("/geo5/SoilData/doc/scripts/database_R/settings/path_linux.txt",sep="\t",header=T,
#                   fileEncoding="UTF-8")
#  maint<-read.table("/geo5/SoilData/doc/scripts/database_R/settings/sa_maintance.txt",sep="\t",header=T)
#  source("/geo5/SoilData/doc/scripts/database_R/settings/db_func.R")
#}

library(tidyverse)
library(tidyr)

##################################################################
## step 1.02
## set running options years, ...
##################################################################
options(scipen=100) # for non exonential display of numeric values
options(digits=17)
origin  <- "1970-01-01"
aktuell <-as.numeric(format(Sys.Date(),"%Y"))
path<-"N:/sparc/data/LTO/"

##################################################################
## step 1.03
## loop 1 over years
##################################################################
for(year in 2013:2017){ #2014:aktuell

  ##################################################################
  ## step 1.04
  ## set 2 empty tables with length of year
  ## columns: 2 (date table) and number of input table
  ##################################################################
  cat("\nProcessing year",year,"\n====================\n\n")
  start.date <- as.POSIXct(paste(year,"-01-01 00:00:00",sep=""),format="%Y-%m-%d %H:%M:%S", tz = "UTC")
  end.date   <- as.POSIXct(paste(year,"-12-31 23:50:00",sep=""),format="%Y-%m-%d %H:%M:%S", tz = "UTC")
  # create empty data frame with UTC time stamp every 10 min
  db.kull2<-matrix(ncol=1,nrow=length(seq(start.date,end.date,by="1 h")))
  #db.kull2[,c(2:3)]<-NA
  compl.q<-matrix(ncol=2,nrow=length(seq(start.date,end.date,by="1 h")))
  db.kull2[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="1 h"),format="%Y-%m-%d %H:%M:%S"))
  compl.q[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="1 h"),format="%Y-%m-%d %H:%M:%S"))
  colnames(db.kull2)<-c("UTC")
  colnames(compl.q)<-c("UTC","erste")
  
  ##################################################################
  ## step 1.05
  ## set input.path and list all files
  ##################################################################
  #inz.path<-paste0(path$w[path$n=="RAW.p"],"KuQ1/")
  #files2read<-list.files(inz.path,pattern="*.dat")
  files2read<-list.files(paste(path,"raw/KuLucky22014/",sep=""),pattern="*.txt")
  
  if(year==2013){
    dada<-read.table(paste0(path,"raw/KuLucky2013/data_sascha/water_level.csv"),sep=",",dec=".",header=F,skip=2)
    dada<-dada[,c("V2","V3","V4")]
    colnames(dada)<-c("UTC","raw_P","Tw_unknown")
    
    # transform timestamp
    dada$UTC<-as.POSIXct(dada$UTC,format="%d.%m.%Y %I:%M:%S %p",tz="UTC")
    
    # transform pressure into kPa
    dada$raw_P<-dada$raw_P/10
    
    newdf.a <- merge(compl.q,dada,all.x=T,by="UTC")
    db.kull2 <- merge(newdf.a,db.kull2,all.x=T,by="UTC")
    
  }
  m<-0
  
  if(year>=2014){  
   
   ##################################################################
   ## step 1.06
   ## loop 2 over years
   ##################################################################
   for(i in 1:length(files2read)){
      
      ##################################################################
      ## step 1.07
      ## read one file (skip headers, set NA-values)
      ## set temporal colnames
      ##################################################################
      cat("\nprocessing ",files2read[i],"\n====================\n\n")
      dada<-read.table(paste0(path,"raw/KuLucky22014/",files2read[i]),sep=",",dec=".",header=T,fill=T,na.strings = "n/a")
      colnames(dada)<-paste("V",seq_len(ncol(dada)))
      
      ##################################################################
      ## step 1.08
      ## 
      ##################################################################
      
      ##################################################################
      ## step 1.09
      ## check file if dates are in running years of loop 1
      ##################################################################
      # skip file if wrong year
      if(as.numeric(substr(lapply(dada[1,1],as.character),1,4))>year || as.numeric(substr(lapply(dada[length(dada[,1]),1],
                                                                                                 as.character),1,4))<year)
      {next}
      cat(paste(dada[1,1],"to",dada[length(dada[,1]),1],"",files2read[i]))
      
      m=m+1
      ##################################################################
      ## step 1.09a
      ## remove battery column for 2016
      ##################################################################
      if(year==2016 || year==2015 || year==2017){
        dada<-dada[,-4]
      }
      
      ##################################################################
      ## step 1.09b
      ## remove empty lines
      ##################################################################
      dada<-drop_na(dada)
      
      ##################################################################
      ## step 1.10
      ## check file for double entries
      ##################################################################
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
        cat(paste("\n","no double entries found in file",files2read[i],"\n\n"))
      }
      
      ##################################################################
      ## step 1.11
      ## convert date to numeric vector
      ##################################################################
      dada[,1]<-as.numeric(as.POSIXct(dada[,1],format="%Y-%m-%d %H:%M:%S", origin=origin, tz="UTC"))
      
      ##################################################################
      ## step 1.12
      ## set original colnames
      ##################################################################
      colnames(dada)<-c("UTC","raw_P","Tw_unknown")
      
      ##################################################################
      ## step 1.13
      ## new arrangement/ order of columns (all temperatures together, ...)
      ##################################################################
   
    ##################################################################
    ## step 1.14
    ## merge input table with date table
    ##################################################################
    newdf.a <- merge(compl.q,dada,all.x=T,by="UTC")
      
    ##################################################################
    ## step 1.15
    ## merge date table with storing table
    ##################################################################
    db.kull2 <- merge(newdf.a,db.kull2,all.x=T,by="UTC")
  
  
    #for(k in 2:(length(db.kull2[1,]))){
    #  db.kull2[,k]<-newdf.a[,k+1]
    #}
   } 
  }
    # write values into first columns and remove merging columns
    if(m>1){
      for (k in 1:(m-1)){
        for(l in 2:(length(newdf.a[1,]))){
          for (s in 1:length(db.kull2[,1])){
            if (is.na(db.kull2[s,l])){
              db.kull2[s,l]<-db.kull2[s,(l+length(newdf.a[1,])-1)]
            }
          }
        }
        db.kull2<-db.kull2[,-c((length(newdf.a)+1):(2*length(newdf.a)-1))]
      }
    }
    db.kull2<-db.kull2[,-2]
    
    db.kull2<-as.data.frame(db.kull2)
    
   
    ##################################################################
    ## step 1.16
    ## convert numeric dates back to date format, name columns
    ##################################################################
    db.kull2[,1]<-format(as.POSIXct(db.kull2[,1],origin=origin,tz="UTC"),format="%Y-%m-%d %H:%M:%S")
    colnames(db.kull2)<-c("UTC","raw_P","Tw_unknown")
    
    
    ###################################################################
    ## step 1.16b
    ## correct pressure
    ###################################################################
    if(year==2013){
      db.kull2[4682,"P"]<-db.kull2[4682,"raw_P"]
      for(s in 4683:length(db.kull2[,1])){
        db.kull2[s,"diff"]<-db.kull2[s,"raw_P"]-db.kull2[s+1,"raw_P"]
        if(db.kull2[s,'diff'] > 10 || is.na(db.kull2[s,'diff'])){
          db.kull2[s,'P']<-db.kull2[s-1,'P']
          db.kull2[s,'diff']<-0
        }
        if(db.kull2[s,'diff'] < -10){
          db.kull2[s,'P']<-db.kull2[s-1,'P']
          db.kull2[s,'diff']<-0
        }
        db.kull2[s,'P']<-db.kull2[s-1,'P']-db.kull2[s,'diff']
      }
    }
    
    # pressure since 2014-08-22 -> data before not relevant -> start line 5598
    
    if(year==2014){
      db.kull2[5598,'P']<-db.kull2[5598,'raw_P']
      for (s in 5598:length(db.kull2[,1])){
        db.kull2[s,'diff']<-db.kull2[s,'raw_P']-db.kull2[s+1,'raw_P']
        if(db.kull2[s,'diff'] > 10 || is.na(db.kull2[s,'diff'])){
          db.kull2[s,'P']<-db.kull2[s-1,'P']
          db.kull2[s,'diff']<-0
        }
        if(db.kull2[s,'diff'] < -10){
          db.kull2[s,'P']<-db.kull2[s-1,'P']
          db.kull2[s,'diff']<-0
        }
        db.kull2[s+1,'P']<-db.kull2[s,'P']-db.kull2[s,'diff']
      }
    }
    if(year==2015|year==2016|year==2017){
      if(year==2016){ # drop NA's
        db.kull2<-drop_na(db.kull2)
      }
      for (s in 1:length(db.kull2[,1])){
        db.kull2[s,'diff']<-db.kull2[s,'raw_P']-db.kull2[s+1,'raw_P']
        if(db.kull2[s,'diff'] > 10 || is.na(db.kull2[s,'diff'])){
          db.kull2[s,'P']<-db.kull2[s-1,'P']
          db.kull2[s,'diff']<-0
        }
        if(db.kull2[s,'diff'] < -10){
          db.kull2[s,'P']<-db.kull2[s-1,'P']
          db.kull2[s,'diff']<-0
        }
        db.kull2[1,'P']<-end.value #db.kull2[1,'P']
        db.kull2[s+1,'P']<-db.kull2[s,'P']-db.kull2[s,'diff']
      }
    }
    
    if(year==2016){
      start.na<-as.POSIXct("2016-07-13 10:00:00",format="%Y-%m-%d %H:%M:%S",tz="UTC")
      end.na<-as.POSIXct("2016-07-22 05:00:00",format="%Y-%m-%d %H:%M:%S",tz="UTC")
      data.na<-data.frame(matrix(ncol=length(db.kull2),
                                 nrow=length(seq(start.na,end.na,by="1 h"))))
      data.na[,1]<-as.POSIXct(seq(start.na,end.na,by="1 h"))
      colnames(data.na)<-c("UTC","raw_P","Tw_unknown","diff","P")
      db.kull2[,1]<-as.POSIXct(db.kull2[,1],format="%Y-%m-%d %H:%M:%S", tz="UTC")
      db.kull2<-rbind(db.kull2,data.na)
      db.kull2<-db.kull2[order(db.kull2$UTC),]
      db.kull2[,1]<-as.character(db.kull2[,1])
    }
    
    if(year>=2014){
      db.kull2<-db.kull2[-length(db.kull2[,1]),]
      end.value<-db.kull2[length(db.kull2$P),'P']
    }
    
  ##################################################################
  ## step 1.16a
  ## load SaMet2002 for atmospheric pressure (kPa, every 30 min)
  ##################################################################
  SaMet<-read.table(paste0("N:/sparc/data/LTO/level1/SaMet2002/00_full_dataset/SaMet2002_",
                           year,"_lv1.dat"),sep=",",dec=".",header=T)
  SaMet$PA[which(SaMet$PA_fl!=0)]<-NA
  
  ##################################################################
  ## step1.16b
  ## read Eddy data
  ##################################################################
  if(year==2013){
    SaMet13<-read.table("N:/sparc/personal_accounts/02_Praktika/Anne_Udke/01_data/01_raw_data/SaEddy_2013_PA.CSV",
                        sep=",",dec=".",header=F,skip=2)
    colnames(SaMet13)<-c("UTC","PA")
  }
  if(year==2014){
    SaMet14<-read.table("N:/sparc/personal_accounts/02_Praktika/Anne_Udke/01_data/01_raw_data/SaEddy_2014-08_PA.CSV",
                        sep=";",dec=".",header=F,skip=2)
    colnames(SaMet14)<-c("UTC","PA")
  }
  
  ##################################################################
  ## step 1.16b
  ## calculate hourly mean for atmospheric pressure and add to Lucky Lake Data Frame
  ##################################################################
  l<-1
  for (s in 1:length(db.kull2[,1])){
    db.kull2[s,'PA']<-mean(c(SaMet[l,'PA'],SaMet[l+1,'PA']),na.rm = T)
    l=l+2
  }
  db.kull2$PA[which(db.kull2$PA=="NaN")]<-NA
  
  if(year==2013){
    l<-9363
    for (s in 4682:5763){
      db.kull2[s,'PA']<-mean(c(SaMet13[l,'PA'],SaMet13[l+1,'PA']),na.rm = T)
      l=l+2
      db.kull2[s,'PA']<-db.kull2[s,'PA']/1000
    }
    db.kull2$PA[which(db.kull2$PA=="NaN")]<-NA
  }
  
  if(year==2014){
    l<-1
    for (s in 5089:5601){
      db.kull2[s,'PA']<-mean(c(SaMet14[l,'PA'],SaMet14[l+1,'PA']),na.rm = T)
      l=l+2
      db.kull2[s,'PA']<-db.kull2[s,'PA']/1000
    }
    db.kull2$PA[which(db.kull2$PA=="NaN")]<-NA
  }
  
  ##################################################################
  ## step 1.16b
  ## calculate water density from water temperature
  ##################################################################
  db.kull2$roh_unknown<-1000*(1-((db.kull2$Tw_unknown+288.9414)/
                                       (508929*(db.kull2$Tw_unknown+68.129630)))*
                                    (db.kull2$Tw_unknown-3.9863)^2)
  
  # source of equation: https://calculator.tutorvista.com/water-density-calculator.html
  ##################################################################
  ## step 1.16c
  ## calculate water level
  ##################################################################
  db.kull2<-cbind(db.kull2,'WT'= 100*((db.kull2$P*1000-db.kull2$PA*1000)/
                    (db.kull2$roh_unknown*9.81)))
  
  ##################################################################
  ## step 1.16d
  ## remove columns which are not needed anymore
  ##################################################################
  db.kull2<-db.kull2[,-c(5,6,7)]
  
  ##################################################################
  ## step 1.17
  ## set "sparc" colnames
  ##################################################################
  colnames(db.kull2)<-c("UTC","raw_P","Tw_unknown","P","WT")
  
  ##################################################################
  ## step 1.18
  ## save data to txt-file
  ##################################################################
  if(year==2013){
    write.table(db.kull2,paste0(path,"level0/KuLucky2013/00_full_dataset/KuLucky2013_",year,"_lv0.dat"),
                                quote=F,dec=".",sep=",",row.names=F)
  }
  
  if(year>=2014){
    write.table(db.kull2,#[as.numeric(format(as.POSIXct(db.kull2[,1],format="%Y-%m-%d %H:%M:%D",origin=origin,tz="UTC"),
              # format="%Y"))==year,-2],
              paste0(path,"level0/KuLucky22014/00_full_dataset/KuLucky22014_",year,"_lv0.dat"),
              quote=F,dec=".",sep=",",row.names=F)
  }
}
cat("\n#\n# KuQ1 without problems! \n#\n")

#control plot
#ggplot(db.kull2)+
#  geom_point(mapping = aes(UTC,P_raw),size=0.1)+
#  geom_point(mapping = aes(UTC,P),size=0.1,col="blue")
#ggplot(db.kull2)+
#  geom_point(mapping = aes(UTC,WT),size=0.1,col="red")
###################################################################
#
# Erklärung Tabellenkopf:
# UTC - time (format: YY-MM-DD HH:MM:SS)
# P - hydrostatic pressure, measured by U20-Onset Sensor
# Tw_unknown - water temperature in unknown depth, measured by U20-Onset sensor
# roh_unknown - water density in unknown water depth
# PA - atmospheric pressure at Samoylov Meteorological Sataion (SaMet2002)
# WL - water level