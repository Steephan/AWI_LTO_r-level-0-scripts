#############################################################################
##
##   BaHole2009        RAW to Level0
##
##   equal time steps, no gaps, table flag
##
##   by: Stephan.Lange@awi.de
##   last modified: 2015-12-11
##
#############################################################################

## Input header
#M-Log1 #A50282 - Incremental Data (Pos: 0) (Parameter Set: 'A50282_0307_240509.par')
#Nr  Time	#3;Dig.Temp;oC	#4;Dig.Temp;oC	#5;Dig.Temp;oC	#6;Dig.Temp;oC	#7;Dig.Temp;oC	#8;Dig.Temp;oC	#9;Dig.Temp;oC	#10;Dig.Temp;oC	#11;Dig.Temp;oC	#12;Dig.Temp;oC	#HK-Bat;V

#############################################################################
#
#  Attention!!!  
#
#  wrong order of temperature sensors
#  see line 115-117
#
#############################################################################

# if you want to check of multiple values per date
# set check.inconsitense to 1
# (it takes time >> to reduce set line 66 to just one variable)
check.inconsitense=0
options(warn=-1)
#############################################################################
# Start at: 22/08/2013
# still to do:
# - save constant digits (2.1000 instead of 2.1)
# - add flag-column
# - convert "NaN" to "NA" (see BaSnow)
#
# last additions:
# - 
# - 
#
#############################################################################





#setwd("C:/Users/stlange/Desktop/")
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

in.path<-"N:/geo5/SoilData/data/raw/BaHole2009/"
out.path<-"N:/geo5/SoilData/data/level0/BaHole2009/00_full_dataset/"
origin="1970-01-01"
files2read<-list.files(in.path,pattern="*.dat")
#files2read<-files2read[-6]
grr<-length(files2read)
#format(Sys.Date(),"%Y")
start.date<-as.POSIXct(paste("01.01.2009",sep=""),format='%d.%m.%Y', tz = "UTC")
end.date<-as.POSIXct(paste0("31.12.",format(Sys.Date(),"%Y")," 23:00:00"),format='%d.%m.%Y %H:%M:%S', tz = "UTC")

db.bahole<-matrix(ncol=12,nrow=length(seq(start.date,end.date,by="hour")),-999)
db.bahole[,c(2:12)]<-NA
compl.temp<-matrix(ncol=2,nrow=length(seq(start.date,end.date,by="hour")))
db.bahole[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="hour"),format='%Y-%m-%d %H:%M:%S'))
compl.temp[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="hour"),format='%Y-%m-%d %H:%M:%S'))
colnames(compl.temp)<-c("UTC","erste")
for (year in 2017){
if(check.inconsitense==1){
  test.matrix<-matrix(ncol=(4+grr),nrow=length(seq(start.date,end.date,by="hour")),-999)
  test.matrix[,1]<-NA 
  test.matrix[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="hour"),format='%Y-%m-%d %H:%M:%S'))
  colnames(test.matrix)<-c("UTC",paste("spalte",(1:(grr+3))+1,sep=""))
for(testspalte in 2:11){#2:11
# testlauf ob doppelungen auftreten
for(i in 1:length(files2read)){#length(files2read)
  dudu<-read.table(paste(in.path,files2read[i],sep=""),sep="\t",dec=".",header=F,skip=2,col.names = paste0("V",seq_len(13)), fill = TRUE)[,c(2,testspalte+2)]
  dudu[,1]<-as.numeric(as.POSIXct(dudu[,1],format='%d.%m.%Y %H:%M:%S',origin=origin, tz = "UTC"))
  dudu[,1]<-round(dudu[,1],-1)
  #dudu[,1:13]<-data.matrix(dudu[,1:13], rownames.force = NA)
  
  colnames(dudu)<-c("UTC","test")
  #print(paste(dudu[1,2],"     to     ",dudu[length(dudu[,1]),2])) 
  
  newdf.a <- merge(compl.temp,dudu,all.x=T, by="UTC")
  test.matrix[,1+i]<-newdf.a[,3]
}


#miau(c(2,2.00001))
test.matrix[,grr+2]<-apply((test.matrix[,2:(grr+1)]), 1, mao)
test.matrix[,grr+3]<-apply((test.matrix[,2:(grr+1)]), 1, miau)
test.matrix[,grr+4]<-as.numeric(format( as.POSIXct(test.matrix[,1],origin=origin,tz="UTC"),format='%Y'))
#mao(test.matrix[,7])
if(length(na.omit(test.matrix[test.matrix[,(grr+3)]!=0,(grr+3)]))!=0 && test.matrix[,grr+4]==year){# & test.matrix[,grr+4]==year
print(paste("double values:",length(na.omit(test.matrix[test.matrix[,(grr+2)]>=2,(grr+2)]))," for test var",testspalte ))
print(paste("double values:",length(na.omit(test.matrix[test.matrix[,(grr+3)]!=0,(grr+3)]))," for test var",testspalte ))
}else{
  print(paste("test var",testspalte - 1,"in ",year,"is consitent" ))
  
}}}}





##### hier gehts los!!!!

for(i in 1:length(files2read)){#1:length(files2read)
#dir(data.path)
dada<-read.table(paste(in.path,files2read[i],sep=""),sep="\t",dec=".",header=F,skip=2,col.names = paste0("V",seq_len(13)), fill = TRUE)#[,c(1,2,3,6,4,5,7:13)]
#print(paste(dada[1,2],"     to     ",dada[length(dada[,1]),2])) 
dada[,2]<-as.numeric(as.POSIXct(dada[,2],format='%d.%m.%Y %H:%M:%S',origin=origin, tz = "UTC"))
dada[,2]<-round(dada[,2],-2)
#dada[,1:13]<-data.matrix(dada[,1:13], rownames.force = NA)
dada[,1:13] <- sapply(dada[,1:13], as.num)
colnames(dada)<-c("nr","UTC","Tair_50","Ts_0","Ts_50","Ts_100",  "Ts_150", "Ts_250", "Ts_350", "Ts_550", "Ts_750", "Ts_900","HK-Bat_V")
#print(paste(dada[1,2],"     to     ",dada[length(dada[,1]),2])) 
  
newdf.a <- merge(compl.temp,dada,all.x=T, by="UTC")
db.bahole[,2]<-rowMeans(cbind(db.bahole[,2],newdf.a[,4]),na.rm=T)# Tair_50
db.bahole[,3]<-rowMeans(cbind(db.bahole[,3],newdf.a[,7]),na.rm=T)# Ts_0       !!!!!!!!!!!    
db.bahole[,4]<-rowMeans(cbind(db.bahole[,4],newdf.a[,5]),na.rm=T)# Ts_50       !!!!!!!!!!!   wrong order
db.bahole[,5]<-rowMeans(cbind(db.bahole[,5],newdf.a[,6]),na.rm=T)# Ts_100       !!!!!!!!!!!   
db.bahole[,6]<-rowMeans(cbind(db.bahole[,6],newdf.a[,8]),na.rm=T)# Ts_150  
db.bahole[,7]<-rowMeans(cbind(db.bahole[,7],newdf.a[,9]),na.rm=T)# Ts_250  
db.bahole[,8]<-rowMeans(cbind(db.bahole[,8],newdf.a[,10]),na.rm=T)# Ts_350
db.bahole[,9]<-rowMeans(cbind(db.bahole[,9],newdf.a[,11]),na.rm=T)# Ts_550
db.bahole[,10]<-rowMeans(cbind(db.bahole[,10],newdf.a[,12]),na.rm=T)# Ts_750
db.bahole[,11]<-rowMeans(cbind(db.bahole[,11],newdf.a[,13]),na.rm=T)# Ts_900
db.bahole[,12]<-rowMeans(cbind(db.bahole[,12],newdf.a[,14]),na.rm=T)# HK-Bat_V
}
colnames(db.bahole)<-c("UTC","Tair_50","Ts_0","Ts_50","Ts_100",  "Ts_150", "Ts_250", "Ts_350", "Ts_550", "Ts_750", "Ts_900","HK-Bat_V")
db.bahole[,1]<-format( as.POSIXct(db.bahole[,1],origin=origin,tz="UTC"),format='%Y-%m-%d %H:%M')
#db.bahole[,1]<-format( as.POSIXct(db.bahole[,1],origin=origin,tz="UTC"),format='%Y%m%d%H%M%S')

## NA problem
for(kl in 2:ncol(db.bahole)){
  #db.bahole[,kl] <- as.numeric(db.bahole[,kl])
  db.bahole[is.nan(as.numeric(db.bahole[,kl])),kl] <- NA
}


for(years in 2009:2018){## 2009:2015
write.table(db.bahole[as.numeric(format(as.POSIXct(db.bahole[,1],format='%Y-%m-%d %H:%M',origin=origin, tz = "UTC"),format='%Y'))==years,],
            paste(out.path ,"BaHole2009_",years,"_lv0.dat",sep=""),quote = F,dec=".",sep=",",row.names=F)
}
## and update the big one
#write.table(db.bahole,paste(out.path ,"BaHole2009_all.dat",sep=""),quote = F,dec=".",sep=",",row.names=F)
print(paste("Bahole2009 without problems!"))





