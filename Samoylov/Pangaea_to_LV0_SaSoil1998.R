# create dataset from original matlab dataset from Britta Kattenstroht


rm(list=ls())
if (.Platform$OS.type == "windows") {
  path<-read.table("N:/geo5/SoilData/doc/scripts/database_R/settings/sa_path_windoof.txt",sep="\t",header=T)
  maint<-read.table("N:/geo5/SoilData/doc/scripts/database_R/settings/sa_maintance.txt",sep="\t",header=T)
  source("N:/geo5/SoilData/doc/scripts/database_R/settings/db_func.R")
}else{
  path<-read.table("/geo5/SoilData/doc/scripts/database_R/settings/path_linux.txt",sep="\t",header=T, fileEncoding="UTF-8")
  maint<-read.table("/geo5/SoilData/doc/scripts/database_R/settings/maintance.txt",sep="\t",header=T)
  source("/geo5/SoilData/doc/scripts/database_R/settings/db_func.R")
}



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
end.date              <-as.POSIXct(paste0(year,"-",12,"-",31," 23:00:00"),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
## create empty data frame with UTC time stamp every 30 min
db.samet.soil        <-as.data.frame(matrix(ncol=48,nrow=length(seq(start.date,end.date,by="hour")),-999))
db.samet.soil[,c(2:48)]<-NA  

compl.soil           <-matrix(ncol=2,nrow=length(seq(start.date,end.date,by="hour")),-999)

db.samet.soil[,1]    <-as.numeric(as.POSIXct(seq(start.date,end.date,by="hour"),format='%Y-%m-%d %H:%M:%S'))

compl.soil[,1]       <-as.numeric(as.POSIXct(seq(start.date,end.date,by="hour"),format='%Y-%m-%d %H:%M:%S'))

colnames(compl.soil) <-c("UTC","erste");

# ####################################
# ## set path tzo Pangaea data
# ####################################
inz.01.path           <-paste0("n:/geo5/SoilData/data/raw/SaSoil1998/")
#files.01              <-list.files(inz.01.path,pattern=".csv")

dada.ts<-read.table(paste0(inz.01.path,"soiltemps9802.csv"),sep=",",dec=".",header=T,skip=0, fill = TRUE,na="NAN")
dada.G<-read.table(paste0(inz.01.path,"heatflux9802.csv"),sep=",",dec=".",header=T,skip=0, fill = TRUE,na="NAN")
dada.vwc<-read.table(paste0(inz.01.path,"thetaroth9802.csv"),sep=",",dec=".",header=T,skip=0, fill = TRUE,na="NAN")
dada.diel<-read.table(paste0(inz.01.path,"dielectric_const9802.csv"),sep=",",dec=".",header=T,skip=0, fill = TRUE,na="NAN")

dada.ts[,1]<-as.numeric(as.POSIXct(dada.ts[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC"))
dada.G[,1]<-as.numeric(as.POSIXct(dada.G[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC"))
dada.vwc[,1]<-as.numeric(as.POSIXct(dada.vwc[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC"))
dada.diel[,1]<-as.numeric(as.POSIXct(dada.diel[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC"))

newdf.ts <- merge(compl.soil,dada.ts,all.x=T, by="UTC")
newdf.G <- merge(compl.soil,dada.G,all.x=T, by="UTC")
newdf.vwc <- merge(compl.soil,dada.vwc,all.x=T, by="UTC")
newdf.diel <- merge(compl.soil,dada.diel,all.x=T, by="UTC")

db.samet.soil[,2:3]  <-newdf.G[,3:4]
db.samet.soil[,4:19]  <-newdf.ts[,3:18]
db.samet.soil[,20:34]  <-newdf.diel[,3:17]
db.samet.soil[,35:48]  <-newdf.vwc[,3:16]

colnames(db.samet.soil)<-c("UTC","G_5","G_52",
                           "Ts_cen_7",  "Ts_cen_32",  "Ts_3off",  "Ts_4off",  "Ts_cen_23",  "Ts_cen_13",  "Ts_cen_42",
                           "Ts_rim_9",  "Ts_9off",  "Ts_rim_15", "Ts_rim_28", "Ts_rim_37", "Ts_13off", "Ts_rim_47", "Ts_rim_58", "Ts_G_5",
                           "E2_cen_7",  "E2_cen_13",  "E2_cen_23",  "E2_cen_32",  "E2_cen_42",
                           "E2_rim_9",  "E2_rim_15",  "E2_rim_28",  "E2_rim_37",  "E2_rim_47", "E2_rim_58",
                           "E2_lys1", "E2_lys2", "E2_v_0", "E2_sn_0",
                           "vwc_cen_7",  "vwc_cen_13",  "vwc_cen_23",  "vwc_cen_32",  "vwc_cen_42",
                           "vwc_rim_9",  "vwc_rim_15",  "vwc_rim_28",  "vwc_rim_37",  "vwc_rim_47", "vwc_rim_58",
                           "vwc_lys1", "vwc_lys2", "vwc_v_0")

db.samet.soil<-db.samet.soil[,c(1:4,19,9,8,5,10,11,13:15,17,18,20:48)]

# ####################################
# ## set path tzo Pangaea data
# ####################################
# inz.01.path           <-paste0("n:/geo5/SoilData/data/raw/SaSoil1998/")
# files.01              <-list.files(inz.01.path,pattern="Samoylov_1998-2002_soil.dat")
# 
# ####################################
# ## read Pangaea data from file
# ####################################
# dada.soil<-read.table(paste0(inz.01.path,files.01),sep="\t",dec=".",header=T,skip=0, fill = TRUE,na="NAN")
# 
# #colnames(dada.soil) <- paste0("V",seq_len(ncol(dada.soil)))
# dada.soil[,1]<-as.numeric(as.POSIXct(dada.soil[,1],format='%Y-%m-%dT%H:%M',origin=origin, tz = "UTC"))
# 
# ####################################
# ## rearrangement of pangaea
# ####################################
# 
# #hui <- matrix(nrow=length(unique(dada.soil[,1])),ncol=5,-99)
# 
# #hui[,1] <- unique(dada.soil[,1])
# null7<-dada.soil[dada.soil[,2]==0.07,]
# null9<-dada.soil[dada.soil[,2]==0.09,]
# vier2<-dada.soil[dada.soil[,2]==0.42,]
# vier7<-dada.soil[dada.soil[,2]==0.47,]
# newdf.null7 <- merge(compl.soil,null7,all.x=T, by="Date")
# newdf.null9 <- merge(compl.soil,null9,all.x=T, by="Date")
# newdf.vier2 <- merge(compl.soil,vier2,all.x=T, by="Date")
# newdf.vier7 <- merge(compl.soil,vier7,all.x=T, by="Date")
# 
# db.samet.soil[,2]<-newdf.null7[,5]
# db.samet.soil[,4]<-newdf.null9[,5]
# db.samet.soil[,3]<-newdf.vier2[,5]
# db.samet.soil[,5]<-newdf.vier7[,5]
# ####################################
# ## merge input data with date table
# ####################################

db.samet.soil[,1]  <-format( as.POSIXct(db.samet.soil[,1],origin=origin,tz="UTC"),format='%Y-%m-%d %H:%M')
# 
# colnames(db.samet.soil) <- c("UTC","Ts_center_7","Ts_center_42","Ts_rim_9","Ts_rim_47")
# 
write.table(db.samet.soil,       paste0(path$w[path$n=="LV0.p"],"SaSoil1998/00_full_dataset/SaSoil1998_",year,"_lv0.dat"),quote = F,dec=".",sep=",",row.names=F)




cat("\n#\n# SaMet1998 ",year," without problems!\n#\n") # main output

}
