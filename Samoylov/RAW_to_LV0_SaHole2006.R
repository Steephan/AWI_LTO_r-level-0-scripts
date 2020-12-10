#############################################################################
##
##   SaHole2006        RAW to Level0
##
##   equal time steps, no gaps, table flag
##
##   by: Stephan.Lange@awi.de, Peter.Schreiber@awi.de,  Niko.Bornemann@awi.de
##
##   last modified: 2019/05/16
##    - change directories
##
##
#############################################################################

#############################################################################
# to run this script seperat, you have to uncomment the next 10 lines!
rm(list=ls())
if (.Platform$OS.type == "windows") {
  path<-read.table("N:/sparc/LTO/R_database/database_R/settings/sa_path_windoof.txt",sep="\t",header=T)
  maint<-read.table("N:/sparc/LTO/R_database/database_R/settings/sa_maintance.txt",sep="\t",header=T)
  source("N:/sparc/LTO/R_database/database_R/settings/db_func.R")
}else{
  path<-read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt",sep="\t",header=T, fileEncoding="UTF-8")
  maint<-read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt",sep="\t",header=T)
  source("/sparc/LTO/R_database/database_R/settings/db_func.R")
}

#############################################################################

#############################################################################
#
#  Attention!!!
# 
#
# time steps very unround
# problems in 2013 timestep 2013/04/20 05:00:00 next file 2013/04/20 05:17:50
# round is a problem file: [10] "20130508064736_SaHole2006.dat" if
#
#
#############################################################################

# if you want to check of multiple values per date
# set check.inconsitense to 1
# (it takes time >> to reduce set line 66 to just one variable)
check.inconsitense=0
options(warn=-1)
#############################################################################
# Start at:
start.year<-2018
end.year<-2019

# still to do:
# -
#
# last additions:
# -
# -
#
#############################################################################

#####
## has to be outsourced
as.num<-function(x){as.numeric(as.character(x))}
#as.num<-function(x) {seq_along(levels(x))[x]}
mao<-function(x){length(na.omit(x))}
miau<-function(x){
  if(length(na.omit(x))<=1){
    return(0)
  }else{
    return(sd(na.omit(x)))
  }
}
#####
in.path<-"N:/sparc/data/LTO/raw/SaHole2006/"
out.path<-"N:/sparc/data/LTO/level0/SaHole2006/00_full_dataset/"
# in.path<-"/sparc/data/LTO/raw/SaHole2006/"
# out.path<-"/sparc/data/LTO/level0/SaHole2006/00_full_dataset/"

origin="1970-01-01"
files2read<-list.files(in.path,pattern="*.dat")
#files2read<-files2read[-6]
grr<-length(files2read)
#format(Sys.Date(),"%Y")
# start.date<-as.POSIXct(paste("01.01.2006",sep=""),format='%d.%m.%Y', tz = "UTC")
# end.date<-as.POSIXct(paste("31.12.",format(Sys.Date(),"%Y"),sep=""),format='%d.%m.%Y', tz = "UTC")
cat("\nProcessing year",end.year,"\n====================\n\n")
start.date <-as.POSIXct(paste("2006-01-01 00:00:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
end.date   <-as.POSIXct(paste(end.year,"-",12,"-",31," 23:00:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
db.sahole<-matrix(ncol=25,nrow=length(seq(start.date,end.date,by="hour")),-999)
db.sahole[,c(2:25)]<-NA

compl.temp<-matrix(ncol=2,nrow=length(seq(start.date,end.date,by="hour")))

db.sahole[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="hour"),format='%Y-%m-%d %H:%M'))
compl.temp[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="hour"),format='%Y-%m-%d %H:%M'))
colnames(compl.temp)<-c("UTC","erste")



for(i in 1:length(files2read)){#1:length(files2read)

  data.temp<-read.table(paste0(in.path,files2read[i]),sep="",dec=".",header=F,skip=0,col.names = paste0("V",seq_len(26)), fill = TRUE)
  skipper<-which(data.temp[,1]=="Temp")
  data.temp<-read.table(paste0(in.path,files2read[i]),sep="",dec=".",header=F,skip=skipper,col.names = paste0("V",seq_len(26)), fill = TRUE)
  data.temp$V1<-paste(data.temp$V1,data.temp$V2)
  data.temp$V2<- NULL
  data.temp$V1<-round(as.POSIXct(data.temp[,1],format='%Y/%m/%d %H:%M:%S', origin=origin,tz = "UTC"),"hour")
  data.temp$V1<-as.numeric(data.temp$V1)

data.temp[,1:25] <- sapply(data.temp[,1:25], as.num)

data.temp<-check.double.entry(data.temp)
# order temperatures from 0.0m-26.75m
data.temp<-data.temp[,c(1,rev(2:25))]


colnames(data.temp)<-c("UTC","Tair_0","Ts_75","Ts_175","Ts_275","Ts_375","Ts_475","Ts_575","Ts_675","Ts_775","Ts_875",
                       "Ts_975","Ts_1075","Ts_1175","Ts_1275","Ts_1375","Ts_1475","Ts_1575","Ts_1675","Ts_1775",
                       "Ts_1875","Ts_2075","Ts_2275","Ts_2475","Ts_2675")

newdf.a <- merge(compl.temp,data.temp,all.x=T, by="UTC")
for(i in 2:25){db.sahole[,i] <-rowMeans(cbind(db.sahole[,i],newdf.a[,i+1]),na.rm=T)}
}
colnames(db.sahole)<-c("UTC","Tair_0","Ts_75","Ts_175","Ts_275","Ts_375","Ts_475","Ts_575","Ts_675","Ts_775","Ts_875",
                       "Ts_975","Ts_1075","Ts_1175","Ts_1275","Ts_1375","Ts_1475","Ts_1575","Ts_1675","Ts_1775",
                       "Ts_1875","Ts_2075","Ts_2275","Ts_2475","Ts_2675")
db.sahole[,1]<-format( as.POSIXct(db.sahole[,1],origin=origin,tz="UTC"),format='%Y-%m-%d %H:%M')



for(years in start.year:end.year){## 2009:2018
write.table(db.sahole[as.numeric(format(as.POSIXct(db.sahole[,1],format='%Y-%m-%d %H:%M',origin=origin, tz = "UTC"),
                                        format='%Y'))==years,],paste(out.path ,"SaHole2006_",years,"_lv0.dat",sep=""),
            quote = F,dec=".",sep=",",row.names=F)
}
## and update the big one
# write.table(db.sahole,paste(out.path ,"SaHole2006_all.dat",sep=""),quote = F,dec=".",sep=",",row.names=F)
# print(paste("Sahole2006 without problems!"))





