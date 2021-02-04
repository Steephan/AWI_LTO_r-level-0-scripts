#script to create usable raw data from Pangaea data
#create yearly files in 30 min steps and fill them with data from Pangaea
# by Pete
# last modification: 05.04.2018


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
#############################################################################




options(scipen=100) # for non-exponential display of numeric values
origin  <- "1970-01-01"
aktuell <-as.numeric(format(Sys.Date(),"%Y"))

for (year in (1998:2002)) {
  
#############################################################################
## step 1.04
## set 2 empty tables with length of year
## columns: 2 (date table) and number of input table (storing table)
#############################################################################
cat("\nProcessing year",year,"\n====================\n\n")
start.date            <-as.POSIXct(paste0(year,"-01-01 00:00:00"),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
end.date              <-as.POSIXct(paste0(year,"-",12,"-",31," 23:45:00"),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
## create empty data frame with UTC time stamp every 30 min
db.samet.meteo        <-as.data.frame(matrix(ncol=10,nrow=length(seq(start.date,end.date,by="15 min")),-999))
db.samet.meteo[,1]    <-as.numeric(as.POSIXct(seq(start.date,end.date,by="15 min"),format='%Y-%m-%d %H:%M:%S'))
db.samet.meteo[,c(2:10)]<-NA  

compl.meteo           <-matrix(ncol=2,nrow=length(seq(start.date,end.date,by="15 min")),-999)
compl.meteo[,1]       <-as.numeric(as.POSIXct(seq(start.date,end.date,by="15 min"),format='%Y-%m-%d %H:%M:%S'))

colnames(compl.meteo) <-c("UTC","erste");

####################################
## set path tzo Pangaea data
####################################
inz.01.path           <-paste0("N:/sparc/data/LTO/raw/SaMet1998/")
files.01              <-list.files(inz.01.path,pattern=".csv")



dada.T1  <-read.table(paste0(inz.01.path,"temp1_9802.csv") ,sep=",",dec=".",header=T,skip=0, fill = TRUE,na="NAN")
dada.T2  <-read.table(paste0(inz.01.path,"temp2_9802.csv") ,sep=",",dec=".",header=T,skip=0, fill = TRUE,na="NAN")
dada.RH1 <-read.table(paste0(inz.01.path,"humid1_9802.csv"),sep=",",dec=".",header=T,skip=0, fill = TRUE,na="NAN")
dada.RH2 <-read.table(paste0(inz.01.path,"humid2_9802.csv"),sep=",",dec=".",header=T,skip=0, fill = TRUE,na="NAN")
dada.prec<-read.table(paste0(inz.01.path,"prec9802.csv")   ,sep=",",dec=".",header=T,skip=0, fill = TRUE,na="NAN")
dada.Dsn <-read.table(paste0(inz.01.path,"snowheight9802.csv"),sep=",",dec=".",header=T,skip=0, fill = TRUE,na="NAN")
dada.wv  <-read.table(paste0(inz.01.path,"windspeed9802.csv"),sep=",",dec=".",header=T,skip=0, fill = TRUE,na="NAN")
dada.wd  <-read.table(paste0(inz.01.path,"winddirection9802.csv"),sep=",",dec=".",header=T,skip=0, fill = TRUE,na="NAN")
dada.rad <-read.table(paste0(inz.01.path,"nettorad9802.csv"),sep=",",dec=".",header=T,skip=0, fill = TRUE,na="NAN")
# 
dada.T1[,1]<-as.numeric(as.POSIXct(dada.T1[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC"))
dada.T2[,1]<-as.numeric(as.POSIXct(dada.T2[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC"))
dada.RH1[,1]<-as.numeric(as.POSIXct(dada.RH1[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC"))
dada.RH2[,1]<-as.numeric(as.POSIXct(dada.RH2[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC"))
dada.prec[,1]<-as.numeric(as.POSIXct(dada.prec[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC"))
dada.Dsn[,1]<-as.numeric(as.POSIXct(dada.Dsn[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC"))
dada.wv[,1]<-as.numeric(as.POSIXct(dada.wv[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC"))
dada.wd[,1]<-as.numeric(as.POSIXct(dada.wd[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC"))
dada.rad[,1]<-as.numeric(as.POSIXct(dada.rad[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC"))
# 
newdf.T1 <- merge(compl.meteo,dada.T1,all.x=T, by="UTC")
newdf.T2 <- merge(compl.meteo,dada.T2,all.x=T, by="UTC")
newdf.RH1 <- merge(compl.meteo,dada.RH1,all.x=T, by="UTC")
newdf.RH2 <- merge(compl.meteo,dada.RH2,all.x=T, by="UTC")
newdf.prec <- merge(compl.meteo,dada.prec,all.x=T, by="UTC")
newdf.Dsn <- merge(compl.meteo,dada.Dsn,all.x=T, by="UTC")
newdf.wv <- merge(compl.meteo,dada.wv,all.x=T, by="UTC")
newdf.wd <- merge(compl.meteo,dada.wd,all.x=T, by="UTC")
newdf.rad <- merge(compl.meteo,dada.rad,all.x=T, by="UTC")

db.samet.meteo[,2]  <-newdf.T1[,3]
db.samet.meteo[,3]  <-newdf.T2[,3]
db.samet.meteo[,4]  <-newdf.RH1[,3]
db.samet.meteo[,5]  <-newdf.RH2[,3] 
db.samet.meteo[,6]  <-newdf.prec[,3]
db.samet.meteo[,7]  <-newdf.Dsn[,3]
db.samet.meteo[,8]  <-newdf.wv[,3]
db.samet.meteo[,9]  <-newdf.wd[,3]
db.samet.meteo[,10] <-newdf.rad[,3]


colnames(db.samet.meteo)<-c("UTC","Tair_50","Tair_200","RH_50","RH_200",
                           "prec",  "Dsn",  "wind_v_300",  "wind_deg_300",  "NetRad")



####################################
## read Pangaea data from file
####################################
inz.01.path           <-paste0("N:/sparc/data/LTO/raw/SaMet1998/")
files.02              <-list.files(inz.01.path,pattern=".dat")

dada.meteo<-read.table(paste0(inz.01.path,files.02[1]),sep=",",dec=".",header=F,skip=1, fill = TRUE,na="NAN")
colnames(dada.meteo) <- paste0("V",seq_len(ncol(dada.meteo)))
dada.meteo[,1]<-as.numeric(as.POSIXct(dada.meteo[,1],format='%Y-%m-%dT%H:%M',origin=origin, tz = "UTC"))

dada.wind<-read.table(paste0(inz.01.path,files.02[2]),sep=",",dec=".",header=F,skip=1, fill = TRUE,na="NAN")
colnames(dada.wind) <- paste0("V",seq_len(ncol(dada.wind)))
dada.wind[,1]<-as.numeric(as.POSIXct(dada.wind[,1],format='%Y-%m-%dT%H:%M',origin=origin, tz = "UTC"))

####################################
## set original colnames
####################################
# colnames(dada.meteo)   <-c("UTC","T2 [?]","RH [%]","NET [W/m**2]","Precip [mm/h]")
# colnames(dada.wind)   <-c("UTC","ff [m/s]")
colnames(dada.meteo)   <-c("UTC","Tair_200","RH_200","NetRad","prec")
colnames(dada.wind)   <-c("UTC","wind_v_300")

####################################
## merge input data with date table
####################################
newdf.meteo <- merge(compl.meteo,dada.meteo,all.x=T, by="UTC")
newdf.wind2 <- merge(compl.meteo,dada.wind,all.x=T, by="UTC")
welche.spalte<-c(3,5,10,6)
for(k in 1:4){
  db.samet.meteo[,welche.spalte[k]]<-rowMeans(cbind(db.samet.meteo[,welche.spalte[k]],newdf.meteo[,k+2]),na.rm=T)# here check if all not needed colums are removed
}
##for(k in 2:2){
db.samet.meteo[,8]<-rowMeans(cbind(db.samet.meteo[,8],newdf.wind2[,3]),na.rm=T)# here check if all not needed colums are removed
#}


## convert numeric dates back to date format


db.samet.meteo[,1]  <-format( as.POSIXct(db.samet.meteo[,1],origin=origin,tz="UTC"),format='%Y-%m-%d %H:%M')
#db.samet.wind[,1]  <-format( as.POSIXct(db.samet.wind[,1],origin=origin,tz="UTC"),format='%Y-%m-%d %H:%M')

## set "sparc" colnames

# colnames(db.samet.meteo) <- c("UTC","Tair_a_200","RH_200","NetRad","prec")
# colnames(db.samet.wind)  <- c("UTC","Wind_v_300")


## step 1.20
## safe data to txt-file

# db.samet<-cbind(db.samet.meteo,db.samet.wind[,-1])
# colnames(db.samet) <- c("UTC","Tair_a_200","RH_200","NetRad","prec","wind_v_300")
# ## db.samet<-cbind(db.samet.meteo,db.samet.wind[,-1])
# ##db.samet<-db.samet.meteo
write.table(db.samet.meteo,       paste0(path$w[path$n=="LV0.p"],"SaMet1998/00_full_dataset/SaMet1998_",year,"_lv0.dat"),quote = F,dec=".",sep=",",row.names=F)




cat("\n#\n# SaMet1998 ",year," without problems!\n#\n") # main output

}
