#############################################################################
##
##   TVCibut2016     RAW to Level0
##   
##   equal time steps, no gaps
##
##   by: Stephan.Lange@awi.de,  Marie.Eckert@awi.de
##   last modified: 2019-02-26
##
#############################################################################
## 1. *.txt- files for years 2016 and 2017
##
#############################################################################
## step 1.01
## set path settings for different systems linux vs. windoof
#############################################################################
# to run this script seperat, you have to uncomment the next 10 lines!
rm(list=ls())

##Needed packages:
library("caTools") 
library("zoo")
library("stringr")
library("plyr")

library("lubridate")
library("testthat")
library("DescTools")


if (.Platform$OS.type == "windows") {
  path<-read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt",sep="\t",header=T)
  maint<-read.table("N:/sparc/LTO/R_database/database_R/settings/maintance.txt",sep="\t",header=T)
  p.1<-read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt",sep="\t",header=T)
  p.1maint<-read.table("N:/sparc/LTO/R_database/database_R/settings/maintance.txt",sep="\t",header=T)
  
  source("N:/sparc/LTO/R_database/database_R/settings/db_func.R")
}else{
  path<-read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt",sep="\t",header=T, fileEncoding="UTF-8") 
  maint<-read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt",sep="\t",header=T)
  p.1<-read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt",sep="\t",header=T, fileEncoding="UTF-8") 
  p.1maint<-read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt",sep="\t",header=T)
  
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

for (year in 2016:2017){#2013:2016 2012:aktuell
  
  #############################################################################
  ## step 1.04
  ## set 2 empty tables with length of year
  ## columns: 2 (date table) and number of input table (storing table)
  #############################################################################
  
  cat("\nProcessing year",year,"\n====================\n\n")
  start.date <-as.POSIXct(paste(year,"-01-01 00:00:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date   <-as.POSIXct(paste(year,"-",12,"-",31," 23:00:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
  
  # create empty data frame with UTC time stamp every 180 min
  db.TVCibut<-matrix(ncol=101,nrow=length(seq(start.date,end.date,by="180 min")),-999)
  db.TVCibut[,c(2:101)]<-NA
  compl.temp<-matrix(ncol=1,nrow=length(seq(start.date,end.date,by="180 min")))
  #convert date to numeric and round to nearest hour
  db.TVCibut[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="180 min"),format='%Y-%m-%d %H:%M:%S'))
  compl.temp[,1]<-round(as.numeric(as.POSIXct(seq(start.date,end.date,by="180 min"),format='%Y-%m-%d %H:%M:%S')),-3)
  # compl.temp[,1]<-(as.numeric(as.POSIXct(seq(start.date,end.date,by="180 min"),format='%Y-%m-%d %H:%M:%S')))/3600
  # compl.temp[,1]<- (ceiling(compl.temp[,1]))*3600
  colnames(compl.temp)<-c("UTC")
  
  #############################################################################
  ## step 1.05
  ## set input.path and list all files
  #############################################################################
  inz.path<-paste0(path$w[path$n=="RAW.p"],"TVCibut2015/ibutton_2016_17/")
  files2read<-list.files(inz.path,pattern="*.txt")

    #############################################################################
    ## step 1.06
    ## read first file to get UTC column and save it in dada
    ## set temporal colnames
    #############################################################################
    dada<-read.table(paste(inz.path,files2read[1],sep=""),sep=",",dec=".",header=T,skip=33, fill = TRUE,na="NAN")
    colnames(dada) = paste0("V",seq_len(ncol(dada)))
    
    ######Split the columns and write AM/PM to am and pm in 1st column
    dada <-str_split_fixed(dada$V1, ("M "), 2)
    dada[,1]<- sub("A", "a", dada[,1], fixed = T)
    dada[,1]<- sub("P", "p", dada[,1], fixed = T)
    dada[,1] <- str_c(dada[,1], 'm')
    colnames(dada) = c("UTC", "V2")
    dada=as.data.frame(dada) 
    ## delete 2nd column, to get UTC only 
    dada$V2 <- NULL
    
    ## Convert AM/PM
    dada[,1]<-format(strptime(dada[,1], "%Y-%m-%d %I:%M:%S %p"), format="%Y-%m-%d %H:%M:%S")
    ##round to nearest hour and convert to numeric
    dada[,1]<-round(as.numeric(as.POSIXct(dada[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC")),-3)
    # dada[,1]<-(as.numeric(as.POSIXct(dada[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC")))/3600
    # dada[,1]<- (ceiling(dada[,1]))*3600

    #############################################################################
    ## step 1.07
    ## read file 85.txt to get correct end date for dada
    #############################################################################       
    dada2<-read.table(paste(inz.path,files2read[60],sep=""),sep=",",dec=".",header=T,skip=33, fill = TRUE,na="NAN")
    colnames(dada2) = paste0("V",seq_len(ncol(dada2)))
    ######Split the columns and write AM/PM to am and pm in 1st column
    dada2 <-str_split_fixed(dada2$V1, ("M "), 2)
    dada2[,1]<- sub("A", "a", dada2[,1], fixed = T)
    dada2[,1]<- sub("P", "p", dada2[,1], fixed = T)
    dada2[,1] <- str_c(dada2[,1], 'm')
    dada2=as.data.frame(dada2)
    ## Convert AM/PM
    dada2[,1]<-format(strptime(dada2[,1], "%Y-%m-%d %I:%M:%S %p"), format="%Y-%m-%d %H:%M:%S")
    ##round to nearest hour (with -3) and convert to numeric
    dada2[,1]<-round(as.numeric(as.POSIXct(dada2[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC")),-3)

    # dada2[,1]<-(as.numeric(as.POSIXct(dada2[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC")))/3600
    # dada2[,1]<- (ceiling(dada2[,1]))*3600
    dada2$V2 <- NULL
    ##subset needed rows
    dada2 <- subset(dada2, V1 > 1504451000)
    names(dada2) <- names(dada) 
    newdf <- rbind(dada, dada2)
   
    #############################################################################
    ## step 1.08
    ## loop 2 over all files
    #############################################################################
    
    for(i in 1:length(files2read)){
    
    ##read all files
    ibutany<-read.table(paste(inz.path,files2read[i],sep=""),sep=",",dec=".",header=F,skip=34, fill = TRUE,na="NAN")
    colnames(ibutany) = paste0("V",seq_len(ncol(ibutany)))
    cat("\nprocessing ",files2read[i],"\n====================\n\n")
    ##convert columns
    ibutany <-str_split_fixed(ibutany$V1, ("M "), 2)
    ibutany[,1]<- sub("A", "a", ibutany[,1], fixed = T)
    ibutany[,1]<- sub("P", "p", ibutany[,1], fixed = T)
    ibutany[,1] <- str_c(ibutany[,1], 'm')
    colnames(ibutany) = c("UTC", paste0("V2",i))
    ibutany=as.data.frame(ibutany) 
    ## Convert AM/PM
    ibutany[,1]<-format(strptime(ibutany[,1], "%Y-%m-%d %I:%M:%S %p"), format="%Y-%m-%d %H:%M:%S")
    ibutany[,2]<- as.numeric(as.character(ibutany[,2]))
    ##round to nearest hour and convert to numeric
    ibutany[,1]<-round(as.numeric(as.POSIXct(ibutany[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC")),-3)
    # ibutany[,1]<-(as.numeric(as.POSIXct(ibutany[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC")))/3600
    # ibutany[,1]<-(ceiling(ibutany[,1]))*3600
    # 
    ##merge files and UTC  by date
    newdf <- merge(x= newdf, y= ibutany, by = 'UTC', all.x= T)
    # newdf[,1] <- (newdf[,1])/3600
    # newdf[,1]<- (ceiling(newdf[,1]))*3600
   }

    #############################################################################
    ## step 1.09
    ## standard case
    ## set original colnames
    #############################################################################
    newdf[,67:101] <- as.numeric(NA)
    colnames(newdf)<-c("UTC", "Ts_i21", "Ts_i22", "Ts_i23", "Ts_i24", "Ts_i25" ,"Ts_i26", "Ts_i27", "Ts_i28",
                      "Ts_i30","Ts_i31","Ts_i32","Ts_i33","Ts_i34","Ts_i35","Ts_i36","Ts_i37","Ts_i38","Ts_i39",
                      "Ts_i40","Ts_i41","Ts_i42","Ts_i43","Ts_i44","Ts_i45","Ts_i46","Ts_i47","Ts_i49",
                      "Ts_i50","Ts_i51","Ts_i52","Ts_i53","Ts_i54","Ts_i55","Ts_i56","Ts_i57","Ts_i58","Ts_i59",
                      "Ts_i60","Ts_i61","Ts_i62","Ts_i63","Ts_i64","Ts_i65","Ts_i66","Ts_i68",  
                      "Ts_i70","Ts_i71","Ts_i72","Ts_i73","Ts_i74","Ts_i76","Ts_i77","Ts_i78","Ts_i79",
                      "Ts_i80","Ts_i81","Ts_i82","Ts_i83","Ts_i84","Ts_i85","Ts_i86","Ts_i87","Ts_i88","Ts_i89",
                      "Ts_i90", "Ts_i1", "Ts_i2","Ts_i3","Ts_i4","Ts_i5","Ts_i6","Ts_i7","Ts_i8","Ts_i9",
                      "Ts_i10","Ts_i11","Ts_i12","Ts_i13","Ts_i14","Ts_i15","Ts_i16","Ts_i17","Ts_i18","Ts_i19","Ts_i20",
                      "Ts_i29","Ts_i48","Ts_i67","Ts_i69","Ts_i75",
                      "Ts_i91","Ts_i92","Ts_i93","Ts_i94","Ts_i95","Ts_i96","Ts_i97","Ts_i98","Ts_i99","Ts_i100")
    #############################################################################
    ## step 1.10
    ## new arrangement / order of columns (all Temperatures together, ascending, ... )
    #############################################################################
    newdf<-newdf[,c("UTC","Ts_i1", "Ts_i2","Ts_i3","Ts_i4","Ts_i5","Ts_i6","Ts_i7","Ts_i8","Ts_i9", 
                   "Ts_i10","Ts_i11","Ts_i12","Ts_i13","Ts_i14","Ts_i15","Ts_i16","Ts_i17","Ts_i18","Ts_i19",
                   "Ts_i20","Ts_i21","Ts_i22","Ts_i23","Ts_i24","Ts_i25","Ts_i26","Ts_i27","Ts_i28","Ts_i29",
                   "Ts_i30","Ts_i31","Ts_i32","Ts_i33","Ts_i34","Ts_i35","Ts_i36","Ts_i37","Ts_i38","Ts_i39",
                   "Ts_i40","Ts_i41","Ts_i42","Ts_i43","Ts_i44","Ts_i45","Ts_i46","Ts_i47","Ts_i48","Ts_i49",
                   "Ts_i50","Ts_i51","Ts_i52","Ts_i53","Ts_i54","Ts_i55","Ts_i56","Ts_i57","Ts_i58","Ts_i59",
                   "Ts_i60","Ts_i61","Ts_i62","Ts_i63","Ts_i64","Ts_i65","Ts_i66","Ts_i67","Ts_i68","Ts_i69",
                   "Ts_i70","Ts_i71","Ts_i72","Ts_i73","Ts_i74","Ts_i75","Ts_i76","Ts_i77","Ts_i78","Ts_i79",
                   "Ts_i80","Ts_i81","Ts_i82","Ts_i83","Ts_i84","Ts_i85","Ts_i86","Ts_i87","Ts_i88","Ts_i89",
                   "Ts_i90", "Ts_i91","Ts_i92","Ts_i93","Ts_i94","Ts_i95","Ts_i96","Ts_i97","Ts_i98","Ts_i99","Ts_i100")]

    #############################################################################
    ## step 1.11
    ## merge input data with date table and round to correct date
    #############################################################################
    
    newdf.a <- merge(compl.temp,newdf, all.x=T, by="UTC")
    newdf.a[,1] <- (newdf.a[,1])/3600
    newdf.a[,1]<- (ceiling(newdf.a[,1]))*3600
    
    #############################################################################
    ## step 1.12
    ## merge date table with storing table
    #############################################################################

          
    for(k in 2:(length(db.TVCibut[1,]))){
       
      db.TVCibut[,k]<-rowMeans(cbind(db.TVCibut[,k],newdf.a[,k]),na.rm=T)
    }

  #############################################################################
  ## step 1.13
  ## convert numeric dates back to date format
  #############################################################################
  
  
  db.TVCibut[,1]<-format(as.POSIXct(db.TVCibut[,1],origin=origin,tz="UTC"),format='%Y-%m-%d %H:%M')
 #   format(as.POSIXct(1481209200,origin=origin,tz="UTC"),format='%Y-%m-%d %H:%M')
  #############################################################################
  ## step 1.14
  ## set "sparc" colnames
  #############################################################################
 colnames(db.TVCibut)<-c("UTC","Ts_i1", "Ts_i2","Ts_i3","Ts_i4","Ts_i5","Ts_i6","Ts_i7","Ts_i8","Ts_i9", 
                              "Ts_i10","Ts_i11","Ts_i12","Ts_i13","Ts_i13","Ts_i15","Ts_i16","Ts_i17","Ts_i18","Ts_i19",
                              "Ts_i20","Ts_i21", "Ts_i22", "Ts_i23", "Ts_i24", "Ts_i25" ,"Ts_i26", "Ts_i27", "Ts_i28","Ts_i29",
                              "Ts_i30","Ts_i31","Ts_i32","Ts_i33","Ts_i34","Ts_i35","Ts_i36","Ts_i37","Ts_i38","Ts_i39",
                              "Ts_i40","Ts_i41","Ts_i42","Ts_i43","Ts_i44","Ts_i45","Ts_i46","Ts_i47","Ts_i48","Ts_i49",
                              "Ts_i50","Ts_i51","Ts_i52","Ts_i53","Ts_i54","Ts_i55","Ts_i56","Ts_i57","Ts_i58","Ts_i59",
                              "Ts_i60","Ts_i61","Ts_i62","Ts_i63","Ts_i64","Ts_i65","Ts_i66","Ts_i67","Ts_i68","Ts_i69",
                              "Ts_i70","Ts_i71","Ts_i72","Ts_i73","Ts_i74","Ts_i75","Ts_i76","Ts_i77","Ts_i78","Ts_i79",
                              "Ts_i80","Ts_i81","Ts_i82","Ts_i83","Ts_i84","Ts_i85","Ts_i86","Ts_i87","Ts_i88","Ts_i89",
                              "Ts_i90", "Ts_i91","Ts_i92","Ts_i93","Ts_i94","Ts_i95","Ts_i96","Ts_i97","Ts_i98","Ts_i99","Ts_i100")
    
  #############################################################################
  ## step 1.15
  ## safe data to txt-file
  #############################################################################
  
    write.table(db.TVCibut,
              paste0(path$w[path$n=="LV0.p"],"TVCibut2015/00_full_dataset/TVCibut2015_",year,"_lv0.dat"),quote = F,dec=".",sep=",",row.names=F)
 

} # end loop over years
 
  cat("\n#\n# TVCibut2015 without problems!\n#\n")

#################################################################################################################################
  #############################################################################
  ## 2. *.csv- files for years 2017 and 2018
  #############################################################################
  ## step 2.01
  ## loop 1 over years 
  #############################################################################
  
  for (year in 2017:2018){#2013:2016 2012:aktuell
    
    #############################################################################
    ## step 2.02
    ## set 2 empty tables with length of year
    ## columns: 2 (date table) and number of input table (storing table)
    #############################################################################
    
    cat("\nProcessing year",year,"\n====================\n\n")
    start.date <-as.POSIXct(paste(year,"-01-01 00:00:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
    end.date   <-as.POSIXct(paste(year,"-",12,"-",31," 23:00:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
    
    # create empty data frame with UTC time stamp every 180 min
    db.TVCibut.2<-matrix(ncol=101,nrow=length(seq(start.date,end.date,by="180 min")),-999)
    db.TVCibut.2[,c(2:101)]<-NA
    compl.temp.2<-matrix(ncol=1,nrow=length(seq(start.date,end.date,by="180 min")))
    #convert date to numeric and round to nearest hour
    db.TVCibut.2[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="180 min"),format='%Y-%m-%d %H:%M:%S'))
    compl.temp.2[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="180 min"),format='%Y-%m-%d %H:%M:%S'))
    colnames(compl.temp.2)<-c("UTC")
    
    #############################################################################
    ## step 2.03
    ## set input.path and list all files
    #############################################################################
    inz.path.2<-paste0(path$w[path$n=="RAW.p"],"TVCibut2015/ibutton_2017_18/")
    files2read.2<-list.files(inz.path.2,pattern="*.csv")
    
    #############################################################################
    ## step 2.04
    ## read 50th file to get start of UTC column and save it in dada
    ## set temporal colnames
    #############################################################################
    dada.2start <- read.table(paste(inz.path.2,files2read.2[50],sep=""),sep=",",dec=".",header=T,skip=6, fill = TRUE,na="NAN")
    colnames(dada.2start) = paste0("V",seq_len(ncol(dada.2start)))
    dada.2start <-as.data.frame(dada.2start)
    dada.2start <- dada.2start[-nrow(dada.2start),] 
    dada.2start$V1 <- as.numeric(as.character(dada.2start$V1))
    dada.2start$V2 <- NULL
    colnames(dada.2start)<- c("UTC")

    dada.2start$UTC <-XLDateToPOSIXct(dada.2start$UTC, tz = "UTC", xl1904 = F)
    dada.2start$UTC<-(as.numeric(as.POSIXct(dada.2start$UTC,format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC")))/3600
    dada.2start$UTC <- (floor( dada.2start$UTC))*3600
    #############################################################################
    ## step 2.05
    ## read 7th file to get end of UTC column and save it in dada
    ## set temporal colnames, convert excel dates to POSIXct, convert into numeric
    #############################################################################
    dada.2end <- read.table(paste(inz.path.2,files2read.2[7],sep=""),sep=",",dec=".",header=T,skip=6, fill = TRUE,na="NAN")
    colnames(dada.2end) = paste0("V",seq_len(ncol(dada.2end)))
    dada.2end<-as.data.frame(dada.2end)
    dada.2end <- dada.2end[-nrow(dada.2end),] 
    dada.2end$V1 <- as.numeric(as.character(dada.2end$V1))
    dada.2end$V2 <- NULL
    colnames(dada.2end)<- c("UTC")
    # dada.2end$UTC <- as.POSIXct(dada.2end[,1]*24*3600 + as.POSIXct("1899-12-29 23:00") )
    dada.2end$UTC <- XLDateToPOSIXct(dada.2end$UTC, tz = "UTC", xl1904 = F)
    dada.2end$UTC<-(as.numeric(as.POSIXct(dada.2end$UTC,format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC")))/3600
    dada.2end$UTC <- (floor(dada.2end$UTC))*3600
    ##subset needed rows
    dada.2end <- subset(dada.2end, UTC > 1536235200)
    names(dada.2end) <- names(dada.2start) 
    newdf.2 <- rbind(dada.2start, dada.2end)
    
    #############################################################################
    ## step 2.06
    ## loop 2 over all files
    #############################################################################
    
    for(i in 1:length(files2read.2)){
      ##read all files
      ibutany.2<-read.table(paste(inz.path.2,files2read.2[i],sep=""),sep=",",dec=".",header=F,skip=6, fill = TRUE,na="NAN")
      colnames(ibutany.2) = c("UTC", paste0("V2",i))
      cat("\nprocessing ",files2read.2[i],"\n====================\n\n")
      ##convert columns
      ibutany.2=as.data.frame(ibutany.2) 
      ibutany.2 <- ibutany.2[-nrow(ibutany.2),] 
      ibutany.2$UTC<- as.numeric(as.character(ibutany.2$UTC)) 
      # ibutany.2$UTC <- as.POSIXct(((ibutany.2$UTC*24*3600) + as.POSIXct("1899-12-29 23:00")), tz = "UTC" )
      ibutany.2$UTC <- XLDateToPOSIXct(ibutany.2$UTC, tz = "UTC", xl1904 = F)
      ibutany.2$UTC<-(as.numeric(as.POSIXct(ibutany.2$UTC,format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC")))/3600
      ibutany.2$UTC <- (floor(  ibutany.2$UTC))*3600
      ##merge files and UTC  by date
      newdf.2<- merge(x= newdf.2, y= ibutany.2, by = 'UTC', all.x= T)

   }
    #############################################################################
    ## step 2.07
    ## standard case
    ## set original colnames
    #############################################################################
    newdf.2[,67:101] <- as.numeric(NA)
    colnames(newdf.2)<-c("UTC", "Ts_i21", "Ts_i22", "Ts_i23", "Ts_i24", "Ts_i25" ,"Ts_i26", "Ts_i27", "Ts_i28","Ts_i29",
                       "Ts_i30","Ts_i31","Ts_i32","Ts_i33","Ts_i34","Ts_i35","Ts_i36","Ts_i37","Ts_i38","Ts_i39",
                       "Ts_i40","Ts_i41","Ts_i42","Ts_i43","Ts_i44","Ts_i45","Ts_i46","Ts_i47","Ts_i49",
                       "Ts_i50","Ts_i51","Ts_i52","Ts_i53","Ts_i54","Ts_i55","Ts_i56","Ts_i57","Ts_i58","Ts_i59",
                       "Ts_i60","Ts_i61","Ts_i62","Ts_i63","Ts_i64","Ts_i65","Ts_i66","Ts_i68", "Ts_i69",
                       "Ts_i70","Ts_i74","Ts_i75", "Ts_i76","Ts_i77","Ts_i78","Ts_i79",
                       "Ts_i80","Ts_i81","Ts_i82","Ts_i83","Ts_i84","Ts_i85","Ts_i86","Ts_i87","Ts_i88","Ts_i89",
                       "Ts_i90", "Ts_i1", "Ts_i2","Ts_i3","Ts_i4","Ts_i5","Ts_i6","Ts_i7","Ts_i8","Ts_i9",
                       "Ts_i10","Ts_i11","Ts_i12","Ts_i13","Ts_i14","Ts_i15","Ts_i16","Ts_i17","Ts_i18","Ts_i19","Ts_i20",
                       "Ts_i48","Ts_i67","Ts_i71","Ts_i72","Ts_i73",
                       "Ts_i91","Ts_i92","Ts_i93","Ts_i94","Ts_i95","Ts_i96","Ts_i97","Ts_i98","Ts_i99","Ts_i100")
   
    #############################################################################
    ## step 2.08
    ## new arrangement / order of columns (all Temperatures together, ascending, ... )
    #############################################################################
    newdf.2<-newdf.2[,c("UTC","Ts_i1", "Ts_i2","Ts_i3","Ts_i4","Ts_i5","Ts_i6","Ts_i7","Ts_i8","Ts_i9", 
                    "Ts_i10","Ts_i11","Ts_i12","Ts_i13","Ts_i14","Ts_i15","Ts_i16","Ts_i17","Ts_i18","Ts_i19",
                    "Ts_i20","Ts_i21", "Ts_i22", "Ts_i23", "Ts_i24", "Ts_i25" ,"Ts_i26", "Ts_i27", "Ts_i28","Ts_i29",
                    "Ts_i30","Ts_i31","Ts_i32","Ts_i33","Ts_i34","Ts_i35","Ts_i36","Ts_i37","Ts_i38","Ts_i39",
                    "Ts_i40","Ts_i41","Ts_i42","Ts_i43","Ts_i44","Ts_i45","Ts_i46","Ts_i47","Ts_i48","Ts_i49",
                    "Ts_i50","Ts_i51","Ts_i52","Ts_i53","Ts_i54","Ts_i55","Ts_i56","Ts_i57","Ts_i58","Ts_i59",
                    "Ts_i60","Ts_i61","Ts_i62","Ts_i63","Ts_i64","Ts_i65","Ts_i66","Ts_i67","Ts_i68","Ts_i69",
                    "Ts_i70","Ts_i71","Ts_i72","Ts_i73","Ts_i74","Ts_i75","Ts_i76","Ts_i77","Ts_i78","Ts_i79",
                    "Ts_i80","Ts_i81","Ts_i82","Ts_i83","Ts_i84","Ts_i85","Ts_i86","Ts_i87","Ts_i88","Ts_i89",
                    "Ts_i90", "Ts_i91","Ts_i92","Ts_i93","Ts_i94","Ts_i95","Ts_i96","Ts_i97","Ts_i98","Ts_i99","Ts_i100")]
    
    #############################################################################
    ## step 2.09
    ## merge input data with date table and round to correct date
    #############################################################################
    
    newdf.a.2 <- merge(compl.temp.2,newdf.2, all.x=T, by="UTC")
    newdf.a.2[,1] <- (newdf.a.2[,1])/3600
    newdf.a.2[,1]<- (ceiling(newdf.a.2[,1]))*3600
    
    #############################################################################
    ## step 2.10
    ## merge date table with storing table
    #############################################################################
  
    for(k in 2:(length(db.TVCibut.2[1,]))){
      
      db.TVCibut.2[,k]<-rowMeans(cbind(db.TVCibut.2[,k],newdf.a.2[,k]),na.rm=T)
    }
    
    #############################################################################
    ## step 2.11
    ## convert numeric dates back to date format
    #############################################################################

    db.TVCibut.2[,1]<-format(as.POSIXct(db.TVCibut.2[,1],origin=origin,tz="UTC"),format='%Y-%m-%d %H:%M')
    #############################################################################
    ## step 2.12
    ## set "sparc" colnames
    #############################################################################
    
    colnames(db.TVCibut.2)<-c("UTC","Ts_i1", "Ts_i2","Ts_i3","Ts_i4","Ts_i5","Ts_i6","Ts_i7","Ts_i8","Ts_i9", 
                              "Ts_i10","Ts_i11","Ts_i12","Ts_i13","Ts_i13","Ts_i15","Ts_i16","Ts_i17","Ts_i18","Ts_i19",
                              "Ts_i20","Ts_i21", "Ts_i22", "Ts_i23", "Ts_i24", "Ts_i25" ,"Ts_i26", "Ts_i27", "Ts_i28","Ts_i29",
                              "Ts_i30","Ts_i31","Ts_i32","Ts_i33","Ts_i34","Ts_i35","Ts_i36","Ts_i37","Ts_i38","Ts_i39",
                              "Ts_i40","Ts_i41","Ts_i42","Ts_i43","Ts_i44","Ts_i45","Ts_i46","Ts_i47","Ts_i48","Ts_i49",
                              "Ts_i50","Ts_i51","Ts_i52","Ts_i53","Ts_i54","Ts_i55","Ts_i56","Ts_i57","Ts_i58","Ts_i59",
                              "Ts_i60","Ts_i61","Ts_i62","Ts_i63","Ts_i64","Ts_i65","Ts_i66","Ts_i67","Ts_i68","Ts_i69",
                              "Ts_i70","Ts_i71","Ts_i72","Ts_i73","Ts_i74","Ts_i75","Ts_i76","Ts_i77","Ts_i78","Ts_i79",
                              "Ts_i80","Ts_i81","Ts_i82","Ts_i83","Ts_i84","Ts_i85","Ts_i86","Ts_i87","Ts_i88","Ts_i89",
                              "Ts_i90", "Ts_i91","Ts_i92","Ts_i93","Ts_i94","Ts_i95","Ts_i96","Ts_i97","Ts_i98","Ts_i99","Ts_i100")
    #############################################################################
    ## step 2.13
    ## safe data to txt-file
    #############################################################################
    
    write.table(db.TVCibut.2,
                paste0(path$w[path$n=="LV0.p"],"TVCibut2015/00_full_dataset/TVCibut2015_csv_",year,"_lv0.dat"),quote = F,dec=".",sep=",",row.names=F)
    
    
  } # end loop over years
  
  cat("\n#\n# TVCibut2015 without problems!\n#\n")
  
#################################################################################################################################
  #############################################################################
  ## 3. *.csv- files for year 2018 
  #############################################################################
  
  for (year in 2018){#2013:2016 2012:aktuell
    
    #############################################################################
    ## step 3.01
    ## set 2 empty tables with length of year
    ## columns: 2 (date table) and number of input table (storing table)
    #############################################################################
    
    cat("\nProcessing year",year,"\n====================\n\n")
    start.date <-as.POSIXct(paste(year,"-01-01 00:00:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
    end.date   <-as.POSIXct(paste(year,"-",12,"-",31," 23:00:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
    # create empty data frame with UTC time stamp every 180 min
    db.TVCibut.3<-matrix(ncol=101,nrow=length(seq(start.date,end.date,by="180 min")),-999)
    db.TVCibut.3[,c(2:101)]<-NA
    compl.temp.3<-matrix(ncol=1,nrow=length(seq(start.date,end.date,by="180 min")))
    #convert date to numeric and round to nearest hour
    db.TVCibut.3[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="180 min"),format='%Y-%m-%d %H:%M:%S'))
    compl.temp.3[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="180 min"),format='%Y-%m-%d %H:%M:%S'))
    colnames(compl.temp.3)<-c("UTC")
    
    #############################################################################
    ## step 3.02
    ## set input.path and list all files
    #############################################################################
    inz.path.3<-paste0(path$w[path$n=="RAW.p"],"TVCibut2015/ibutton_2018/")
    files2read.3<-list.files(inz.path.3,pattern="*.csv")
    
    #############################################################################
    ## step 3.03
    ## read 1st file to get start of UTC column and save it in dada
    ## set temporal colnames
    #############################################################################
    dada.3start <- read.table(paste(inz.path.3,files2read.3[1],sep=""),sep=",",dec=".",header=T,skip=6, fill = TRUE,na="NAN")
    colnames(dada.3start) = paste0("V",seq_len(ncol(dada.3start)))
    dada.3start <-as.data.frame(dada.3start)
    dada.3start <- dada.3start[-nrow(dada.3start),] 
    dada.3start$V1 <- as.numeric(as.character(dada.3start$V1))
    dada.3start$V2 <- NULL
    colnames(dada.3start)<- c("UTC")
    # 
    #     dada.2start$UTC <- as.POSIXct( dada.2start[,1]*24*3600 + as.POSIXct("1899-12-29 23:00") )
    dada.3start$UTC <-XLDateToPOSIXct(dada.3start$UTC, tz = "UTC", xl1904 = F)
    dada.3start$UTC<-(as.numeric(as.POSIXct(dada.3start$UTC,format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC")))/3600
    dada.3start$UTC <- (floor( dada.3start$UTC))*3600

    newdf.3 <- dada.3start
    
    #############################################################################
    ## step 3.04
    ## loop 2 over all files
    #############################################################################
    
    for(i in 1:length(files2read.3)){
      ##read all files
      ibutany.3<-read.table(paste(inz.path.3,files2read.3[i],sep=""),sep=",",dec=".",header=F,skip=6, fill = TRUE,na="NAN")
      colnames(ibutany.3) = c("UTC", paste0("V2",i))
      cat("\nprocessing ",files2read.3[i],"\n====================\n\n")
      ##convert columns
      ibutany.3=as.data.frame(ibutany.3) 
      ibutany.3 <- ibutany.3[-nrow(ibutany.3),] 
      ibutany.3$UTC<- as.numeric(as.character(ibutany.3$UTC)) 
      # ibutany.3$UTC <- as.POSIXct(((ibutany.3$UTC*24*3600) + as.POSIXct("1899-12-29 23:00")), tz = "UTC" )
      ibutany.3$UTC <- XLDateToPOSIXct(ibutany.3$UTC, tz = "UTC", xl1904 = F)
      ibutany.3$UTC<-(as.numeric(as.POSIXct(ibutany.3$UTC,format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC")))/3600
      ibutany.3$UTC <- (floor(  ibutany.3$UTC))*3600
      ##merge files and UTC  by date
      newdf.3<- merge(x= newdf.3, y= ibutany.3, by = 'UTC', all.x= T)
      
    }
    
    #############################################################################
    ## step 3.05
    ## standard case
    ## set original colnames
    #############################################################################
    
    newdf.3[,10:101] <- as.numeric(NA)
    colnames(newdf.3)<-c("UTC", "Ts_i11", "Ts_i13", "Ts_i14","Ts_i15","Ts_i16","Ts_i17","Ts_i18","Ts_i19","Ts_i20",
                         "Ts_i21", "Ts_i22", "Ts_i23", "Ts_i24", "Ts_i25" ,"Ts_i26", "Ts_i27", "Ts_i28","Ts_i29",
                         "Ts_i30","Ts_i31","Ts_i32","Ts_i33","Ts_i34","Ts_i35","Ts_i36","Ts_i37","Ts_i38","Ts_i39",
                         "Ts_i40","Ts_i41","Ts_i42","Ts_i43","Ts_i44","Ts_i45","Ts_i46","Ts_i47","Ts_i49",
                         "Ts_i50","Ts_i51","Ts_i52","Ts_i53","Ts_i54","Ts_i55","Ts_i56","Ts_i57","Ts_i58","Ts_i59",
                         "Ts_i60","Ts_i61","Ts_i62","Ts_i63","Ts_i64","Ts_i65","Ts_i66","Ts_i68", "Ts_i69",
                         "Ts_i70","Ts_i74","Ts_i75", "Ts_i76","Ts_i77","Ts_i78","Ts_i79",
                         "Ts_i80","Ts_i81","Ts_i82","Ts_i83","Ts_i84","Ts_i85","Ts_i86","Ts_i87","Ts_i88","Ts_i89",
                         "Ts_i90", "Ts_i1", "Ts_i2","Ts_i3","Ts_i4","Ts_i5","Ts_i6","Ts_i7","Ts_i8","Ts_i9",
                         "Ts_i10","Ts_i12", "Ts_i48","Ts_i67","Ts_i71","Ts_i72","Ts_i73",
                         "Ts_i91","Ts_i92","Ts_i93","Ts_i94","Ts_i95","Ts_i96","Ts_i97","Ts_i98","Ts_i99","Ts_i100")
    
    #############################################################################
    ## step 3.06
    ## new arrangement / order of columns (all Temperatures together, ascending, ... )
    #############################################################################
    newdf.3<-newdf.3[,c("UTC","Ts_i1", "Ts_i2","Ts_i3","Ts_i4","Ts_i5","Ts_i6","Ts_i7","Ts_i8","Ts_i9", 
                        "Ts_i10","Ts_i11","Ts_i12","Ts_i13","Ts_i14","Ts_i15","Ts_i16","Ts_i17","Ts_i18","Ts_i19",
                        "Ts_i20","Ts_i21", "Ts_i22", "Ts_i23", "Ts_i24", "Ts_i25" ,"Ts_i26", "Ts_i27", "Ts_i28","Ts_i29",
                        "Ts_i30","Ts_i31","Ts_i32","Ts_i33","Ts_i34","Ts_i35","Ts_i36","Ts_i37","Ts_i38","Ts_i39",
                        "Ts_i40","Ts_i41","Ts_i42","Ts_i43","Ts_i44","Ts_i45","Ts_i46","Ts_i47","Ts_i48","Ts_i49",
                        "Ts_i50","Ts_i51","Ts_i52","Ts_i53","Ts_i54","Ts_i55","Ts_i56","Ts_i57","Ts_i58","Ts_i59",
                        "Ts_i60","Ts_i61","Ts_i62","Ts_i63","Ts_i64","Ts_i65","Ts_i66","Ts_i67","Ts_i68","Ts_i69",
                        "Ts_i70","Ts_i71","Ts_i72","Ts_i73","Ts_i74","Ts_i75","Ts_i76","Ts_i77","Ts_i78","Ts_i79",
                        "Ts_i80","Ts_i81","Ts_i82","Ts_i83","Ts_i84","Ts_i85","Ts_i86","Ts_i87","Ts_i88","Ts_i89",
                        "Ts_i90", "Ts_i91","Ts_i92","Ts_i93","Ts_i94","Ts_i95","Ts_i96","Ts_i97","Ts_i98","Ts_i99","Ts_i100")]
    
    #############################################################################
    ## step 3.07
    ## merge input data with date table and round to correct date
    #############################################################################
    
    newdf.a.3 <- merge(compl.temp.3,newdf.3, all.x=T, by="UTC")
    newdf.a.3[,1] <- (newdf.a.3[,1])/3600
    newdf.a.3[,1]<- (floor(newdf.a.3[,1]))*3600
    
    #############################################################################
    ## step 3.08
    ## merge date table with storing table
    #############################################################################
    
    for(k in 2:(length(db.TVCibut.3[1,]))){
      
      db.TVCibut.3[,k]<-rowMeans(cbind(db.TVCibut.3[,k],newdf.a.3[,k]),na.rm=T)
    }
    
    #############################################################################
    ## step 3.09
    ## convert numeric dates back to date format
    #############################################################################
    
    db.TVCibut.3[,1]<-format(as.POSIXct(db.TVCibut.3[,1],origin=origin,tz="UTC"),format='%Y-%m-%d %H:%M')
    #############################################################################
    ## step 3.10
    ## set "sparc" colnames
    #############################################################################
    
    colnames(db.TVCibut.3)<-c("UTC","Ts_i1", "Ts_i2","Ts_i3","Ts_i4","Ts_i5","Ts_i6","Ts_i7","Ts_i8","Ts_i9", 
                              "Ts_i10","Ts_i11","Ts_i12","Ts_i13","Ts_i13","Ts_i15","Ts_i16","Ts_i17","Ts_i18","Ts_i19",
                              "Ts_i20","Ts_i21", "Ts_i22", "Ts_i23", "Ts_i24", "Ts_i25" ,"Ts_i26", "Ts_i27", "Ts_i28","Ts_i29",
                              "Ts_i30","Ts_i31","Ts_i32","Ts_i33","Ts_i34","Ts_i35","Ts_i36","Ts_i37","Ts_i38","Ts_i39",
                              "Ts_i40","Ts_i41","Ts_i42","Ts_i43","Ts_i44","Ts_i45","Ts_i46","Ts_i47","Ts_i48","Ts_i49",
                              "Ts_i50","Ts_i51","Ts_i52","Ts_i53","Ts_i54","Ts_i55","Ts_i56","Ts_i57","Ts_i58","Ts_i59",
                              "Ts_i60","Ts_i61","Ts_i62","Ts_i63","Ts_i64","Ts_i65","Ts_i66","Ts_i67","Ts_i68","Ts_i69",
                              "Ts_i70","Ts_i71","Ts_i72","Ts_i73","Ts_i74","Ts_i75","Ts_i76","Ts_i77","Ts_i78","Ts_i79",
                              "Ts_i80","Ts_i81","Ts_i82","Ts_i83","Ts_i84","Ts_i85","Ts_i86","Ts_i87","Ts_i88","Ts_i89",
                              "Ts_i90", "Ts_i91","Ts_i92","Ts_i93","Ts_i94","Ts_i95","Ts_i96","Ts_i97","Ts_i98","Ts_i99","Ts_i100")
    #############################################################################
    ## step 3.11
    ## safe data to txt-file
    #############################################################################
    
    write.table(db.TVCibut.3,
                paste0(path$w[path$n=="LV0.p"],"TVCibut2015/00_full_dataset/TVCibut2015_",year,"_lv0.dat"),quote = F,dec=".",sep=",",row.names=F)
    
    
  } # end loop over years
  
  cat("\n#\n# TVCibut2015 without problems!\n#\n")
  
#################################################################################################################################
  ############################################################################# 
  ## 4. Merging 2018 file 1 and 2
  ############################################################################
  inz.path.3<-paste0(path$w[path$n=="LV0.p"],"TVCibut2015/00_full_dataset/")
  files2read.3<-list.files(inz.path.3,pattern="*.dat")
  
  file12018 <- read.table(paste(inz.path.3,files2read.3[3],sep=""),sep=",",dec=".",header=T,skip=0, fill = TRUE,na="NAN")
  file12018$UTC<-  as.numeric(as.POSIXct( file12018$UTC,format='%Y-%m-%d %H:%M',origin=origin, tz = "UTC"))
  
  file22018 <- read.table(paste(inz.path.3,files2read.3[5],sep=""),sep=",",dec=".",header=T,skip=0, fill = TRUE,na="NAN")
  file22018$UTC<-  as.numeric(as.POSIXct(  file22018$UTC,format='%Y-%m-%d %H:%M',origin=origin, tz = "UTC"))
  
  complete2018 <- merge(file22018, file12018, by= "UTC", all= T, no.dups = T, incomparables = NULL)
  complete2018[1859:2045, 12:20] <- file12018[1859:2045, 12:20]
  
  complete2018[,1]<-format(as.POSIXct(complete2018[,1],origin=origin,tz="UTC"),format='%Y-%m-%d %H:%M')
  
  write.table(complete2018,
              paste0(path$w[path$n=="LV0.p"],"TVCibut2015/00_full_dataset/TVCibut2015_",year,"_lv0.dat"),quote = F,dec=".",sep=",",row.names=F)
  
#################################################################################################################################
  #############################################################################
  ## 5. Merging the two files for 2017
  #############################################################################
  ##5.1. read the two files
  #############################################################################
  inz.path.3<-paste0(path$w[path$n=="LV0.p"],"TVCibut2015/00_full_dataset/")
  files2read.3<-list.files(inz.path.3,pattern="*.dat")

  txt2017 <- read.table(paste(inz.path.3,files2read.3[2],sep=""),sep=",",dec=".",header=T,skip=0, fill = TRUE,na="NAN")
  txt2017$UTC<-  as.numeric(as.POSIXct(txt2017$UTC,format='%Y-%m-%d %H:%M',origin=origin, tz = "UTC"))
  
  csv2017 <- read.table(paste(inz.path.3,files2read.3[5],sep=""),sep=",",dec=".",header=T,skip=0, fill = TRUE,na="NAN")
  csv2017$UTC<-  as.numeric(as.POSIXct(csv2017$UTC,format='%Y-%m-%d %H:%M',origin=origin, tz = "UTC"))
  #############################################################################
  ##5.2. Merge them, result: doubled rows
  #############################################################################
  complete2017 <- merge.data.frame(txt2017, csv2017, by= c("UTC","Ts_i1", "Ts_i2","Ts_i3","Ts_i4","Ts_i5","Ts_i6","Ts_i7","Ts_i8","Ts_i9", 
                                   "Ts_i10","Ts_i11","Ts_i12","Ts_i13","Ts_i13","Ts_i15","Ts_i16","Ts_i17","Ts_i18","Ts_i19",
                                   "Ts_i20","Ts_i21", "Ts_i22", "Ts_i23", "Ts_i24", "Ts_i25" ,"Ts_i26", "Ts_i27", "Ts_i28","Ts_i29",
                                   "Ts_i30","Ts_i31","Ts_i32","Ts_i33","Ts_i34","Ts_i35","Ts_i36","Ts_i37","Ts_i38","Ts_i39",
                                   "Ts_i40","Ts_i41","Ts_i42","Ts_i43","Ts_i44","Ts_i45","Ts_i46","Ts_i47","Ts_i48","Ts_i49",
                                   "Ts_i50","Ts_i51","Ts_i52","Ts_i53","Ts_i54","Ts_i55","Ts_i56","Ts_i57","Ts_i58","Ts_i59",
                                   "Ts_i60","Ts_i61","Ts_i62","Ts_i63","Ts_i64","Ts_i65","Ts_i66","Ts_i67","Ts_i68","Ts_i69",
                                   "Ts_i70","Ts_i71","Ts_i72","Ts_i73","Ts_i74","Ts_i75","Ts_i76","Ts_i77","Ts_i78","Ts_i79",
                                   "Ts_i80","Ts_i81","Ts_i82","Ts_i83","Ts_i84","Ts_i85","Ts_i86","Ts_i87","Ts_i88","Ts_i89",
                                   "Ts_i90", "Ts_i91","Ts_i92","Ts_i93","Ts_i94","Ts_i95","Ts_i96","Ts_i97","Ts_i98","Ts_i99","Ts_i100"), all = T)
  #############################################################################
  ##5.3. Remove all rows containing only NA (by Ts_i21 because it runs from beginning of 2017 to the end)
  #############################################################################
  complete2017 <- complete2017[which(complete2017$Ts_i21!="NaN"),]
  ##append missing dates at the end
   complete2017[1967:2920,] <- txt2017[1967:2920,]

  #############################################################################
  ##5.4. Indentify rows with double entries, remove the originals
  #############################################################################
  # complete2017 <- complete2017[!duplicated( complete2017[c("UTC")]),]     
  # complete2017 <- complete2017[!duplicated( complete2017$UTC),] 
  # 
 ## !
  complete2017 <- complete2017[!(duplicated(complete2017$UTC)|duplicated(complete2017$UTC, fromLast=T)),, drop=FALSE]
##
 
  
  
  #############################################################################
  ##5.5. convert numeric dates back to date format
  ##      safe data to txt-file
  #############################################################################
  complete2017[,1]<-format(as.POSIXct(complete2017[,1],origin=origin,tz="UTC"),format='%Y-%m-%d %H:%M')
 
  write.table(complete2017,
             paste0(path$w[path$n=="LV0.p"],"TVCibut2015/00_full_dataset/TVCibut2015_","2017","_lv0_new.dat"),quote = F,dec=".",sep=",",row.names=F)
 
 
