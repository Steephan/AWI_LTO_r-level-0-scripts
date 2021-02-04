#############################################################################
##
##   BaMet 1998        RAW to Level0
##
##   equal time steps, no gaps, table flag
##
##   by: Stephan.Lange@awi.de
##   last modified: 2017-04-21
##
#############################################################################
#
#  last modifications:
#  add LwOut & Tpan_CG1
#  rename some variables
#############################################################################
#
#
#
#
#############################################################################

#############################################################################
# to run this script separately, you have to uncomment the next 10 lines!
rm(list = ls())
if (.Platform$OS.type == "windows") {
  path <- read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt", sep = "\t", header = T)
  maint <- read.table("N:/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)
  p.1 <- read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt", sep = "\t", header = T)
  p.1maint <- read.table("N:/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)

  source("N:/sparc/LTO/R_database/database_R/settings/db_func.R")
} else {
  path <- read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
  maint <- read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)
  p.1 <- read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
  p.1maint <- read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)

  source("/sparc/LTO/R_database/database_R/settings/db_func.R")
}
#############################################################################
cat("read data\n")
data.all <-read.table(paste0(path$w[path$n=="RAW.p"],"BaMet1998/DataSortedUTC.dat"),fill = TRUE,header=T,dec=".",sep="\t",na="NAN")
## fill empty fields with NA
for(zack in 1:44){data.all[na.omit(data.all[,zack]==""),zack] <- NA}
## remove double entrys!!!!!!!!
data.all<-data.all[-which(duplicated(data.all[,1])),]
##
cat("separate climate data\n")
data.temp<-data.frame(as.numeric(as.POSIXct(data.all$time,format='%Y-%m-%d %H:%M:%S', tz = "UTC")),as.numeric(data.all$temperature_air))
data.prec<-data.frame(as.numeric(as.POSIXct(data.all$time,format='%Y-%m-%d %H:%M:%S', tz = "UTC")),as.numeric(data.all$precipitation))
data.hum <-data.frame(as.numeric(as.POSIXct(data.all$time,format='%Y-%m-%d %H:%M:%S', tz = "UTC")),as.numeric(data.all$humidity))
data.snow<-data.frame(as.numeric(as.POSIXct(data.all$time,format='%Y-%m-%d %H:%M:%S', tz = "UTC")),as.numeric(data.all$snowheight))
data.rad <-data.frame(as.numeric(as.POSIXct(data.all$time,format='%Y-%m-%d %H:%M:%S', tz = "UTC")),
                      as.numeric(data.all$radiation_netto),as.numeric(data.all$radiation_global),
                      as.numeric(data.all$radiation_longwave),as.numeric(data.all$temperature_longwave))
data.wind1<-data.frame(data.all$time,data.all$wind_speed,data.all$wind_direction)
data.wind2<-data.wind1[complete.cases(data.wind1),]
data.wind<-data.frame(as.numeric(as.POSIXct(data.wind2$data.all.time,format='%Y-%m-%d %H:%M:%S', tz = "UTC")),
                      as.numeric(data.wind2$data.all.wind_speed),
                      unlist(strsplit(as.character(data.wind2$data.all.wind_direction), ","))[seq(1,length(data.wind2$data.all.wind_direction)*2,2)],
                      unlist(strsplit(as.character(data.wind2$data.all.wind_direction), ","))[seq(2,length(data.wind2$data.all.wind_direction)*2,2)])
data.flux1<-data.frame(data.all$time,data.all$heatflux)#   heatflux
data.flux2<-data.flux1[complete.cases(data.flux1),]
data.flux<-data.frame(as.numeric(as.POSIXct(data.flux2$data.all.time,format='%Y-%m-%d %H:%M:%S', tz = "UTC")))
for(flux in 1:2){data.flux[,flux+1]<-as.numeric(unlist(strsplit(as.character(data.flux2$data.all.heatflux), ","))[seq(flux,length(data.flux2$data.all.heatflux)*2,2)])}

cat("separate soil moisture data\n")
data.theta1<-data.frame(data.all$time,data.all$theta)#   vwc
data.theta2<-data.theta1[complete.cases(data.theta1),]
data.theta<-data.frame(as.numeric(as.POSIXct(data.theta2$data.all.time,format='%Y-%m-%d %H:%M:%S', tz = "UTC")))
for(theta in 1:32){data.theta[,theta+1]<-as.numeric(unlist(strsplit(as.character(data.theta2$data.all.theta), ","))[seq(theta,length(data.theta2$data.all.theta)*32,32)])}
data.cond1<-data.frame(data.all$time,data.all$conductivity)
data.cond2<-data.cond1[complete.cases(data.cond1),]
data.cond<-data.frame(as.numeric(as.POSIXct(data.cond2$data.all.time,format='%Y-%m-%d %H:%M:%S', tz = "UTC")))
for(cond in 1:35){data.cond[,cond+1]<-as.numeric(unlist(strsplit(as.character(data.cond2$data.all.conductivity), ","))[seq(cond,length(data.cond2$data.all.conductivity)*35,35)])}
data.E2.1<-data.frame(data.all$time,data.all$dielectric_const)
data.E2.2<-data.E2.1[complete.cases(data.E2.1),]
data.E2<-data.frame(as.numeric(as.POSIXct(data.E2.2$data.all.time,format='%Y-%m-%d %H:%M:%S', tz = "UTC")))
for(E2 in 1:32){data.E2[,E2+1]<-as.numeric(unlist(strsplit(as.character(data.E2.2$data.all.dielectric_const), ","))[seq(E2,length(data.E2.2$data.all.dielectric_const)*32,32)])}
cat("separate soil temperature data\n")
data.tprof1<-data.frame(data.all$time,data.all$temperature_profile)
data.tprof2<-data.tprof1[complete.cases(data.tprof1),]
data.tprof<-data.frame(as.numeric(as.POSIXct(data.tprof2$data.all.time,format='%Y-%m-%d %H:%M:%S', tz = "UTC")))
for(prof in 1:32){data.tprof[,prof+1]<-as.numeric(unlist(strsplit(as.character(data.tprof2$data.all.temperature_profile), ","))[seq(prof,length(data.tprof2$data.all.temperature_profile)*32,32)])}
# data.bore1<-data.frame(data.all$time,data.all$temperature_boreholes)
# data.bore2<-data.bore1[complete.cases(data.bore1),]
# data.bore<-data.frame(as.numeric(as.POSIXct(data.bore2$data.all.time,format='%Y-%m-%d %H:%M:%S', tz = "UTC")))
data.bore <-read.table(paste0(path$w[path$n=="RAW.p"],"BaMet1998/BaMet1998borehole.dat"),fill = TRUE,header=T,dec=".",sep=",",na="NA")
data.bore[,1]<-as.numeric(as.POSIXct(data.bore[,1],format='%Y-%m-%d %H:%M:%S', tz = "UTC"))
#for(borh in 1:12){data.bore[,borh+1]<-as.numeric(unlist(strsplit(as.character(data.bore2$data.all.temperature_boreholes), ","))[seq(borh,length(data.bore2$data.all.temperature_boreholes)*10,10)])}

##########################################################################################################################
data.tsnow <-read.table(paste0(path$w[path$n=="RAW.p"],"BaMet1998/BaMet1998snowt_c.dat"),fill = TRUE,header=T,dec=".",sep=",",na="NA")
## there are 3 sensors in the beginning, four at the end ... messy data!
## it turns at 18.04.2000
data.tsnow[,1]<-as.numeric(as.POSIXct(data.tsnow[,1],format='%Y-%m-%d %H:%M:%S', tz = "UTC"))

##########################################################################################################################


#rm(data.all)

colnames(data.temp)[1]  <-c( "V1" )
colnames(data.prec)[1]  <-c( "V1" )
colnames(data.hum)[1]  <- c( "V1" )
colnames(data.snow)[1]  <-c( "V1" )
colnames(data.rad)[1]  <- c( "V1" )
colnames(data.wind)[1]  <-c( "V1" )
colnames(data.theta)[1] <-c( "V1" )
colnames(data.cond)[1] <- c( "V1" )
colnames(data.E2)[1]    <-c( "V1" )
colnames(data.tsnow)[1] <-c( "V1" )
colnames(data.tprof)[1] <-c( "V1" )
colnames(data.bore)[1]  <-c( "V1" )
colnames(data.flux)[1]  <-c( "V1" )
cat("walk througth the years\n")
for(year in 1998:2013){#1999:2013  1998:2011 1998:2013
  cat(year,"\n")
  start.date <-as.POSIXct(paste(year,"-01-01 00:00:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date   <-as.POSIXct(paste(year,"-",12,"-",31," 23:00:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")

  db.temp=db.prec=db.hum=db.snow<-data.frame(matrix(ncol=2,nrow=length(seq(start.date,end.date,by="hour")),-999))
  db.flux                       <-data.frame(matrix(ncol=3,nrow=length(seq(start.date,end.date,by="hour")),-999))
  db.wind                       <-data.frame(matrix(ncol=4,nrow=length(seq(start.date,end.date,by="hour")),-999))
  db.rad                        <-data.frame(matrix(ncol=5,nrow=length(seq(start.date,end.date,by="hour")),-999))
  db.tsnow                      <-data.frame(matrix(ncol=6,nrow=length(seq(start.date,end.date,by="hour")),-999))
  db.bore                       <-data.frame(matrix(ncol=13,nrow=length(seq(start.date,end.date,by="hour")),-999))
  db.theta =db.E2 =db.tprof     <-data.frame(matrix(ncol=33,nrow=length(seq(start.date,end.date,by="hour")),-999))
  db.cond                       <-data.frame(matrix(ncol=36,nrow=length(seq(start.date,end.date,by="hour")),-999))

  compl.temp<-data.frame(matrix(ncol=2,nrow=length(seq(start.date,end.date,by="hour"))))
  compl.temp[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="hour"),format='%Y-%m-%d %H:%M:%S', tz = "UTC"))
  colnames(compl.temp)<-c("V1","leer")

  colnames(db.temp)[1]<-c( "V1" );db.temp[,1]<-compl.temp[,1]
  colnames(db.prec)[1]<-c( "V1" );db.prec[,1]<-compl.temp[,1]
  colnames(db.hum)[1]<- c( "V1" );db.hum[,1]<-compl.temp[,1]
  colnames(db.snow)[1]<-c( "V1" );db.snow[,1]<-compl.temp[,1]
  colnames(db.rad)[1]<- c( "V1" );db.rad[,1]<-compl.temp[,1]
  colnames(db.wind)[1]<-c( "V1" );db.wind[,1]<-compl.temp[,1]
  colnames(db.theta)[1]<-c( "V1" );db.theta[,1]<-compl.temp[,1]
  colnames(db.cond)[1]<- c( "V1" );db.cond[,1]<-compl.temp[,1]
  colnames(db.E2)[1]<-c( "V1" );db.E2[,1]<-compl.temp[,1]
  colnames(db.tsnow)[1]<-c( "V1" );db.tsnow[,1]<-compl.temp[,1]
  colnames(db.bore)[1]<-c( "V1" );db.bore[,1]<-compl.temp[,1]
  colnames(db.tprof)[1]<-c( "V1" );db.tprof[,1]<-compl.temp[,1]
  colnames(db.flux)[1]<-c( "V1" );db.flux[,1]<-compl.temp[,1]

  db.temp[,c(2)]<-NA;db.prec[,c(2)]<-NA;db.hum[,c(2)]<-NA;db.snow[,c(2)]<-NA;db.rad[,c(2:5)]<-NA;db.flux[,c(2:3)]<-NA
  db.wind[,c(2:4)]<-NA;db.theta[,c(2:33)]<-NA;db.cond[,c(2:36)]<-NA;db.E2[,c(2:33)]<-NA
  db.tsnow[,c(2:6)]<-NA;db.bore[,c(2:13)]<-NA;db.tprof[,c(2:33)]<-NA

  newdf.temp <- merge(compl.temp,data.temp,all.x=T, by="V1")
  newdf.prec <- merge(compl.temp,data.prec,all.x=T, by="V1")
  newdf.hum  <- merge(compl.temp,data.hum, all.x=T, by="V1")
  newdf.snow <- merge(compl.temp,data.snow, all.x=T, by="V1")
  newdf.rad  <- merge(compl.temp,data.rad, all.x=T, by="V1")
  newdf.wind <- merge(compl.temp,data.wind, all.x=T, by="V1")
  newdf.theta<- merge(compl.temp,data.theta, all.x=T, by="V1")
  newdf.cond <- merge(compl.temp,data.cond, all.x=T, by="V1")
  newdf.E2   <- merge(compl.temp,data.E2, all.x=T, by="V1")
  newdf.tsnow<- merge(compl.temp,data.tsnow, all.x=T, by="V1")
  newdf.bore <- merge(compl.temp,data.bore, all.x=T, by="V1")
  newdf.tprof<- merge(compl.temp,data.tprof, all.x=T, by="V1")
  newdf.flux <- merge(compl.temp,data.flux, all.x=T, by="V1")

  #db.temp[,2]<-rowMeans(cbind(db.temp[,2],as.numeric(as.character(newdf.temp[,3]))),na.rm=T) moved to line 182ff
  db.prec[,2]<-rowMeans(cbind(db.prec[,2],as.numeric(as.character(newdf.prec[,3]))),na.rm=T)
  db.hum[,2] <-rowMeans(cbind(db.hum[,2], as.numeric(as.character(newdf.hum[,3]))), na.rm=T)
  db.snow[,2]<-rowMeans(cbind(db.snow[,2], as.numeric(as.character(newdf.snow[,3]))), na.rm=T)
  db.rad[,2] <-rowMeans(cbind(db.rad[,2], as.numeric(as.character(newdf.rad[,3]))), na.rm=T)
  db.rad[,3] <-rowMeans(cbind(db.rad[,3], as.numeric(as.character(newdf.rad[,4]))), na.rm=T)
  db.rad[,4] <-rowMeans(cbind(db.rad[,4], as.numeric(as.character(newdf.rad[,5]))), na.rm=T)
  db.rad[,5] <-rowMeans(cbind(db.rad[,5], as.numeric(as.character(newdf.rad[,6]))), na.rm=T)
  db.flux[,2] <-rowMeans(cbind(db.flux[,2], as.numeric(as.character(newdf.flux[,3]))), na.rm=T)
  db.flux[,3] <-rowMeans(cbind(db.flux[,3], as.numeric(as.character(newdf.flux[,4]))), na.rm=T)

  db.wind[,2:4] <- newdf.wind[,3:5]
  db.theta[,2:33] <- newdf.theta[,3:34]
  db.cond[,2:36] <- newdf.cond[,3:37]
  db.E2[,2:33] <- newdf.E2[,3:34]
  db.bore[,2:13] <- newdf.bore[,3:14]
  db.tprof[,2:33] <- newdf.tprof[,3:34]


  # ziel: "UTC"   ,"Tair_5","Tair_20","Tair_35/Tair_40","Tair_48","Tair_100"       Und Tair_200(vor April2000)
  if(year %in% c(1998,1999)){
    # nur 3 Spalten vorhanden 1(Tair_35),2(Tair_200),3(Tair_48)
   db.tsnow[,4] <- as.numeric(as.character(newdf.tsnow[,3]))  # snow_air-spalte 1 --> 35cm
   db.temp[,2]  <- as.numeric(as.character(newdf.tsnow[,4]))  # snow_air-spalte 2 --> 200cm
   db.tsnow[,5] <- as.numeric(as.character(newdf.tsnow[,5]))  # snow_air-spalte 3 --> 48cm

  }else if(year==2000){
    # nur 3 Spalten vorhanden 1(Tair_35),2(Tair_200),3(Tair_48)
    tmp <- as.POSIXlt("18Apr00", format = "%d%b%y")$yday*24+10
    db.tsnow[1:tmp,4] <-as.numeric(as.character(newdf.tsnow[1:tmp,3]))# snow_air-spalte 1 --> 35cm
    db.temp[1:tmp,2]  <-as.numeric(as.character(newdf.tsnow[1:tmp,4]))# snow_air-spalte 2 --> 200cm
    db.tsnow[1:tmp,5] <-as.numeric(as.character(newdf.tsnow[1:tmp,5]))# snow_air-spalte 3 --> 48cm
    ende<-length(newdf.tsnow[,1])
    # ab 18.04.2000 4 Spalten 1(Tair_35),2(Tair_100),3(Tair_48),4(Tair_20)
    db.tsnow[tmp:ende,4] <-as.numeric(as.character(newdf.tsnow[tmp:ende,3]))# snow_air-spalte 1 --> 35cm
    db.tsnow[tmp:ende,6] <-as.numeric(as.character(newdf.tsnow[tmp:ende,4]))# snow_air-spalte 2 --> 100cm
    db.tsnow[tmp:ende,5] <-as.numeric(as.character(newdf.tsnow[tmp:ende,5]))# snow_air-spalte 3 --> 48cm
    db.tsnow[tmp:ende,3] <-as.numeric(as.character(newdf.tsnow[tmp:ende,6]))# snow_air-spalte 4 --> 20cm
    db.temp[tmp:ende,2]  <-as.numeric(as.character(newdf.temp[tmp:ende,3]))     # Tair              --> 200cm

  }else if(year %in% c(2001,2002)){
    # ab 18.04.2000 4 Spalten 1(Tair_35),2(Tair_100),3(Tair_48),4(Tair_20)
    db.tsnow[,4] <-as.numeric(as.character(newdf.tsnow[,3]))# snow_air-spalte 1 --> 35cm
    db.tsnow[,6] <-as.numeric(as.character(newdf.tsnow[,4]))# snow_air-spalte 2 --> 100cm
    db.tsnow[,5] <-as.numeric(as.character(newdf.tsnow[,5]))# snow_air-spalte 3 --> 48cm
    db.tsnow[,3] <-as.numeric(as.character(newdf.tsnow[,6]))# snow_air-spalte 4 --> 20cm
    db.temp[,2]  <-as.numeric(as.character(newdf.temp[,3])) # Tair              --> 200cm

  }else if(year==2003){
    # after caribou attack and reinstallation
    # ab 24.09.2003 caribou attack ...4 Spalten 1(Tair_35),2(Tair_100),3(Tair_35),4(Tair_20)
    tmp <- as.POSIXlt("24Sep03", format = "%d%b%y")$yday*24+10
    ende<-length(newdf.tsnow[,1])
    db.tsnow[1:tmp,4] <-as.numeric(as.character(newdf.tsnow[1:tmp,3]))# snow_air-spalte 1 --> 35cm
    db.tsnow[,6] <-as.numeric(as.character(newdf.tsnow[,4]))# snow_air-spalte 2 --> 100cm
    db.tsnow[,5] <-as.numeric(as.character(newdf.tsnow[,5]))# snow_air-spalte 3 --> 48cm
    db.tsnow[,3] <-as.numeric(as.character(newdf.tsnow[,6]))# snow_air-spalte 3 --> 20cm
    db.temp[,2]  <-as.numeric(as.character(newdf.temp[,3])) # Tair              --> 200cm
    # ab 24.09.2003 caribou attack ...
    db.tsnow[(tmp+1):ende,2] <-as.numeric(as.character(newdf.tsnow[(tmp+1):ende,3]))# snow_air-spalte 1 --> 1cm

  }else if(year==2004){# after caribou attack and reinstallation
    # #               4 Spalten 1(Tair_5),2(Tair_100),3(Tair_35),4(Tair_20)
    tmp <- as.POSIXlt("9Sep04", format = "%d%b%y")$yday*24+12
    ende<-length(newdf.tsnow[,1])
    db.tsnow[,2] <-as.numeric(as.character(newdf.tsnow[,3]))# snow_air-spalte 1 --> 1cm
    db.tsnow[,6] <-as.numeric(as.character(newdf.tsnow[,4]))# snow_air-spalte 2 --> 100cm
    db.tsnow[1:tmp,5] <-as.numeric(as.character(newdf.tsnow[1:tmp,5]))# snow_air-spalte 3 --> 48cm
    db.tsnow[1:tmp,3] <-as.numeric(as.character(newdf.tsnow[1:tmp,6]))# snow_air-spalte 4 --> 20cm
    db.temp[,2]  <-as.numeric(as.character(newdf.temp[,3])) # Tair              --> 200cm
    # ab 24.09.2003 caribou attack ...
    db.tsnow[(tmp+1):ende,3] <-as.numeric(as.character(newdf.tsnow[(tmp+1):ende,5]))# snow_air-spalte 3 --> 20cm
    db.tsnow[(tmp+1):ende,4] <-as.numeric(as.character(newdf.tsnow[(tmp+1):ende,6]))# snow_air-spalte 3 --> 35cm

    ####################
    ###  shift for 1 day of all other data
    db.temp[1416:8760,2]<-db.temp[1440:8784,2]
    db.prec[1416:8760,2]<-db.prec[1440:8784,2]
    db.hum[1416:8760,2] <-db.hum[1440:8784,2]
    db.snow[1416:8760,2]<-db.snow[1440:8784,2]
    db.rad[1416:8760,2:5] <-db.rad[1440:8784,2:5]
    db.wind[1416:8760,2:4] <- db.wind[1440:8784,2:4]
    db.tsnow[1416:8760,2:6] <- db.tsnow[1440:8784,2:6]
    db.bore[1416:8760,2:13]<- db.bore[1440:8784,2:13]
    ### and delete the last day
    db.temp[8761:8784,2]<-NA
    db.prec[8761:8784,2]<-NA
    db.hum[8761:8784,2]<-NA
    db.snow[8761:8784,2]<-NA
    db.rad[8761:8784,2:5]<-NA
    db.wind[8761:8784,2:4]<-NA
    db.tsnow[8761:8784,2:6]<-NA
    db.bore[8761:8784,2:13]<-NA


  }else if(year>2004){# after caribou attack and reinstallation
    # #               4 Spalten 1(Tair_5),2(Tair_100),3(Tair_35),4(Tair_20)
    db.tsnow[,2] <-as.numeric(as.character(newdf.tsnow[,3]))# snow_air-spalte 1 --> 1cm
    db.tsnow[,6] <-as.numeric(as.character(newdf.tsnow[,4]))# snow_air-spalte 2 --> 100cm
    db.tsnow[,3] <-as.numeric(as.character(newdf.tsnow[,5]))# snow_air-spalte 3 --> 20cm
    db.tsnow[,4] <-as.numeric(as.character(newdf.tsnow[,6]))# snow_air-spalte 4 --> 35cm
    db.temp[,2]  <-as.numeric(as.character(newdf.temp[,3])) # Tair              --> 200cm

  }


## NA problem

    db.temp[is.nan(as.numeric(db.temp[,2])),2] <- NA
    db.prec[is.nan(as.numeric(db.prec[,2])),2] <- NA
    db.hum[ is.nan(as.numeric( db.hum[,2])),2] <- NA
    db.snow[is.nan(as.numeric(db.snow[,2])),2] <- NA
  for(kl in 2:ncol(db.rad)){
    db.rad[is.nan(as.numeric(db.rad[,kl])),kl] <- NA

  }
  for(kl in 2:ncol(db.flux)){
    db.flux[is.nan(as.numeric(db.flux[,kl])),kl] <- NA

  }
  for(kl in 2:ncol(db.wind)){
    db.wind[is.nan(as.numeric(db.wind[,kl])),kl] <- NA

  }
  for(kl in 2:ncol(db.theta)){ # 32

    db.theta[is.nan(as.numeric(db.theta[,kl])),kl] <- NA
    #db.cond[is.nan(as.numeric(db.cond[,kl])),kl] <- NA
    db.E2[is.nan(as.numeric(db.E2[,kl])),kl] <- NA
    db.tprof[is.nan(as.numeric(db.tprof[,kl])),kl] <- NA
  }
    for(kl in 2:ncol(db.cond)){ # 32

      db.cond[is.nan(as.numeric(db.cond[,kl])),kl] <- NA
    }

  for(kl in 2:ncol(db.tsnow)){
    db.tsnow[is.nan(as.numeric(db.tsnow[,kl])),kl] <- NA
  }
  for(kl in 2:ncol(db.bore)){ # 10
    db.bore[is.nan(as.numeric(db.bore[,kl])),kl] <- NA
  }

  colnames(db.temp) <-c( "UTC"  ,"Tair_200" )
  colnames(db.prec) <-c( "UTC"  ,"prec" )
  colnames(db.hum)  <-c( "UTC"  ,"RH_200" )
  colnames(db.snow) <-c( "UTC"  ,"Dsn" )
  colnames(db.rad)  <-c( "UTC"  ,"RadNet","SwIn","LwOut","Tpan_CG1" )
  colnames(db.flux) <-c( "UTC"  ,"G_cen_18","G_left_24" )
  colnames(db.wind) <-c( "UTC"  ,"wind_v_200","wind_deg_200","wind_sddeg_200" )
  colnames(db.tsnow)<-c( "UTC"  ,"Tair_5","Tair_20","Tair_35","Tair_48","Tair_100")
  colnames(db.bore) <-c( "UTC"  ,"Ts_252_2","Ts_252_12","Ts_252_32","Ts_252_62","Ts_252_102","Ts_252_152",
                        "Ts_203_2","Ts_203_5","Ts_203_23","Ts_203_53","Ts_203_93","Ts_203_143")

  colnames(db.tprof)<-c( "UTC"  ,"Ts_95_117_v"  ,"Ts_95_108" , "Ts_95_99" , "Ts_135_112"  ,"Ts_135_125_v"  ,"Ts_200_114"  ,
                         "Ts_200_118_v",  "Ts_95_91",  "Ts_145_99" , "Ts_205_90" , "Ts_95_66"  ,"Ts_151_76" , "Ts_205_61" ,
                         "Ts_80_38"  ,"Ts_145_62"  ,"Ts_210_50"  ,"Ts_95_33",  "Ts_145_40",  "Ts_205_31" , "Ts_95_17" ,
                         "Ts_145_24"  ,"Ts_205_13" , "Ts_124_25" , "Ts_177_19"  ,"Ts_67_4"  ,"Ts_124_8"  ,"Ts_145_6",
                         "Ts_169_9",  "Ts_204_5" , "Ts_67_16" , "Ts_185_52" , "Ts_115_60" )
  colnames(db.E2)   <-c( "UTC"  ,"E2_95_120_v"  ,"E2_95_113" , "E2_95_99" , "E2_135_112"  ,"E2_135_127_v"  ,"E2_200_114"  ,
                         "E2_200_121_v",  "E2_90_90",  "E2_140_98" , "E2_200_90" , "E2_90_66"  ,"E2_146_76" , "E2_200_61" ,
                         "E2_85_39"  ,"E2_140_62"  ,"E2_205_51"  ,"E2_90_32",  "E2_140_39",  "E2_200_31" , "E2_90_16" ,
                         "E2_140_23"  ,"E2_200_13" , "E2_115_22" , "E2_172_20"  ,"E2_80_1"  ,"E2_114_6"  ,"E2_140_6",
                         "E2_174_8",  "E2_204_5" , "E2_sn_67_0_v" , "E2_180_54" , "E2_110_59" )
  colnames(db.theta)<-c("UTC"  ,"vwc_95_120_v"  ,"vwc_95_113" , "vwc_95_99" , "vwc_135_112"  ,"vwc_135_127_v"  ,"vwc_200_114"  ,
                        "vwc_200_121_v",  "vwc_90_90",  "vwc_140_98" , "vwc_200_90" , "vwc_90_66"  ,"vwc_146_76" , "vwc_200_61" ,
                        "vwc_85_39"  ,"vwc_140_62"  ,"vwc_205_51"  ,"vwc_90_32",  "vwc_140_39",  "vwc_200_31" , "vwc_90_16" ,
                        "vwc_140_23"  ,"vwc_200_13" , "vwc_115_22" , "vwc_172_20"  ,"vwc_80_1"  ,"vwc_114_6"  ,"vwc_140_6",
                        "vwc_174_8",  "vwc_204_5" , "vwc_sn_67_0_v" , "vwc_180_54" , "vwc_110_59" )
  colnames(db.cond) <-c( "UTC"  ,"cond_95_120_v"  ,"cond_95_113" , "cond_95_99" , "cond_135_112"  ,"cond_135_127_v"  ,"cond_200_114"  ,
                         "cond_200_121_v",  "cond_90_90",  "cond_140_98" , "cond_200_90" , "cond_90_66"  ,"cond_146_76" , "cond_200_61" ,
                         "cond_85_39"  ,"cond_140_62"  ,"cond_205_51"  ,"cond_90_32",  "cond_140_39",  "cond_200_31" , "cond_90_16" ,
                         "cond_140_23"  ,"cond_200_13" , "cond_115_22" , "cond_172_20"  ,"cond_80_1"  ,"cond_114_6"  ,"cond_140_6",
                         "cond_174_8",  "cond_204_5" , "cond_sn_67_0_v" , "cond_180_54" , "cond_110_59","cond_a" , "cond_b" , "cond_c" )
  db.tprof    <- db.tprof[,c(1,26,31,21,18,15,12,9,4,3,2,27,24,33,28,22,19,16,13,10,5,6,29,25,32,30,23,20,17,14,11,7,8)]
  db.theta    <- db.theta[,c(1,26,31,21,18,15,12,9,4,3,2,27,24,33,28,22,19,16,13,10,5,6,29,25,32,30,23,20,17,14,11,7,8)]
  db.E2       <- db.E2[   ,c(1,26,31,21,18,15,12,9,4,3,2,27,24,33,28,22,19,16,13,10,5,6,29,25,32,30,23,20,17,14,11,7,8)]
  db.cond     <- db.cond[ ,c(1,26,31,21,18,15,12,9,4,3,2,27,24,33,28,22,19,16,13,10,5,6,29,25,32,30,23,20,17,14,11,7,8,34,35,36)]
  db.temp[,1] <-format(as.POSIXct(seq(start.date,end.date,by="hour"),format='%Y-%m-%d %H:%M:%S', tz = "UTC"),format='%Y-%m-%d %H:%M')
  db.prec[,1] <-db.temp[,1]
  db.hum[ ,1] <-db.temp[,1]
  db.snow[,1] <-db.temp[,1]
  db.rad[,1]  <-db.temp[,1]
  db.wind[,1] <-db.temp[,1]
  db.tprof[,1]<-db.temp[,1]
  db.theta[,1]<-db.temp[,1]
  db.cond[,1] <-db.temp[,1]
  db.E2[,1]   <-db.temp[,1]
  db.tsnow[,1]<-db.temp[,1]
  db.bore[,1] <-db.temp[,1]
  db.flux[,1] <-db.temp[,1]

#write.table(data.bore1,paste0(path$w[path$n=="RAW.p"] ,"BaMet1998/BaMet1998borehole.dat"),quote = F,dec=".",sep=",",row.names=F)
  db.Tair<-cbind( db.tsnow[,-6], db.temp[2],db.tsnow[6])

write.table(db.Tair,paste0(path$w[path$n=="LV0.p"] ,"BaMet1998/01_air_temp/BaMet1998temp_",year,".dat"),quote = F,dec=".",sep=",",row.names=F)
write.table(db.prec,paste0(path$w[path$n=="LV0.p"] ,"BaMet1998/02_prec/BaMet1998prec_",year,".dat"),quote = F,dec=".",sep=",",row.names=F)
write.table(db.wind,paste0(path$w[path$n=="LV0.p"] ,"BaMet1998/03_wind/BaMet1998wind_",year,".dat"),quote = F,dec=".",sep=",",row.names=F)
write.table(db.hum, paste0(path$w[path$n=="LV0.p"] ,"BaMet1998/04_humidity/BaMet1998hum_" ,year,".dat"),quote = F,dec=".",sep=",",row.names=F)
write.table(db.snow,paste0(path$w[path$n=="LV0.p"] ,"BaMet1998/05_snowdepth/BaMet1998snow_",year,".dat"),quote = F,dec=".",sep=",",row.names=F)
write.table(db.rad, paste0(path$w[path$n=="LV0.p"] ,"BaMet1998/06_radiation/BaMet1998rad_",year,".dat"),quote = F,dec=".",sep=",",row.names=F)
#write.table(db.tsnow,paste0(path$w[path$n=="LV0.p"],"BaMet1998/08_snow_temp/BaMet1998_",year,"_snowt.dat"),quote = F,dec=".",sep=",",row.names=F)
write.table(db.flux,paste0(path$w[path$n=="LV0.p"] ,"BaMet1998/09_heat_flux/BaMet1998_",year,"_heatflux.dat"),quote = F,dec=".",sep=",",row.names=F)

write.table(db.tprof,paste0(path$w[path$n=="LV0.p"],"BaSoil1998/01_temperature/BaSoil1998_ts1_",year,".dat"),quote = F,dec=".",sep=",",row.names=F)
write.table(db.theta,paste0(path$w[path$n=="LV0.p"],"BaSoil1998/05_volumetric_water_content/BaSoil1998_vwc_",year,".dat"),quote = F,dec=".",sep=",",row.names=F)
write.table(db.cond[,1:33],paste0(path$w[path$n=="LV0.p"] ,"BaSoil1998/03_conductivity/BaSoil1998_cond_",year,".dat"),quote = F,dec=".",sep=",",row.names=F)
write.table(db.cond,paste0(path$w[path$n=="LV0.p"] ,"BaSoil1998/03_conductivity/BaSoil1998_cond_new_",year,".dat"),quote = F,dec=".",sep=",",row.names=F)
write.table(db.E2,paste0(path$w[path$n=="LV0.p"]   ,"BaSoil1998/04_dielectric_constant/BaSoil1998_E2_",year,".dat"),quote = F,dec=".",sep=",",row.names=F)
db.tdr<-cbind( db.theta[], db.cond[-1],db.E2[-1])
write.table(db.tdr,paste0(path$w[path$n=="LV0.p"]  ,"BaSoil1998/02_tdr/BaSoil1998_tdr_",year,".dat"),quote = F,dec=".",sep=",",row.names=F)
db.soil<-cbind( db.tprof[], db.theta[-1], db.cond[-1],db.E2[-1])
write.table(db.soil,paste0(path$w[path$n=="LV0.p"] ,"BaSoil1998/00_full_dataset/BaSoil1998_",year,"_lv0.dat"),quote = F,dec=".",sep=",",row.names=F)


write.table(db.bore,paste0(path$w[path$n=="LV0.p"] ,"BaMet1998/07_soil/BaMet1998borehole_",year,".dat"),quote = F,dec=".",sep=",",row.names=F)


#compl<-cbind( db.temp[], db.prec[-1],db.hum[-1], db.snow[-1],db.rad[-1],db.wind[-1], db.tprof[-1], db.theta[-1], db.cond[-1],db.E2[-1],db.tsnow[-1], db.bore[-1],db.flux[-1])
compl<-cbind( db.temp[], db.prec[-1],db.hum[-1], db.snow[-1],db.rad[-1],db.wind[-1],db.tsnow[-1], db.bore[-1],db.flux[-1])
write.table(compl,paste0(path$w[path$n=="LV0.p"]   ,"BaMet1998/00_full_dataset/BaMet1998_",year,"_lv0.dat"),quote = F,dec=".",sep=",",row.names=F)

}

cat("\n#\n# BaMet1998 without problems!\n#\n")


