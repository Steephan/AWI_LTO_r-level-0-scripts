#############################################################################
##
##   BaMast2010        RAW to Level0
##
##   equal time steps, no gaps, table flag
##
##   by: Stephan.Lange@awi.de
##   last modified: 2015/07/15
##
#############################################################################


#############################################################################
#
#  Attention!!!  
#  timesteps 30min and wrong order!!!
#  
#  since 2011.02.03-08:30:00 noData from Tair_50
#
#############################################################################

#############################################################################
# Start at: 2010.09.02-21:30:00
# End at:   2014.06.02-06:30:00
# still to do:
# - save constant digits (2.1000 instead of 2.1)
# - add flag-column
# - 
#
# last additions:
# - 
# - 
#
#############################################################################
as.num<-function(x){as.numeric(as.character(x))}

in.path<-"N:/geo5/SoilData/data/raw/BaMast2010/"
out.path<-"N:/geo5/SoilData/data/level0/BaMast2010/"
origin="1970-01-01"
files2read<-list.files(in.path,pattern="*.dat")
#files2read<-files2read[-c(5,8,20)]

start.date<-as.POSIXct(paste("01.01.2010",sep=""),format='%d.%m.%Y', tz = "UTC")
end.date<-as.POSIXct(paste("31.12.2014",sep=""),format='%d.%m.%Y', tz = "UTC")

db.bamast<-matrix(ncol=12,nrow=length(seq(start.date,end.date,by="30 min")),-999)
db.bamast[,c(2:12)]<-NA
compl.temp<-matrix(ncol=2,nrow=length(seq(start.date,end.date,by="30 min")))
db.bamast[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="30 min"),format='%Y-%m-%d %H:%M:%S'))
compl.temp[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="30 min"),format='%Y-%m-%d %H:%M:%S'))
colnames(compl.temp)<-c("UTC","erste")



for(i in 1:length(files2read)){#1:length(files2read)

dada<-read.table(paste(in.path,files2read[i],sep=""),sep=",",dec=".",header=F,skip=4,col.names = paste0("V",seq_len(13)), fill = TRUE)
print(paste(dada[1,1],"     to     ",dada[length(dada[,1]),1],"    ",files2read[i])) 
dada[,1]<-as.numeric(as.POSIXct(dada[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC"))
dada[,1]<-round(dada[,1],-1)
dada[,1:13]<-data.matrix(dada[,1:13], rownames.force = NA)
dada[,1:13] <- sapply(dada[,1:13], as.num)
colnames(dada)<-c("UTC","RECORD","Batt_Volt_Avg","T107_C_1_Avg","T107_C_2_Avg",
                  "T107_C_3_Avg","T107_C_4_Avg","T107_C_5_Avg","T107_C_6_Avg","T107_C_7_Avg","T107_C_8_Avg","T107_C_9_Avg","T107_C_10_Avg")
  
 newdf.a <- merge(compl.temp,dada,all.x=T, by="UTC")
 db.bamast[,12]<-rowMeans(cbind(db.bamast[,12],newdf.a[,4]),na.rm=T)# Batt_Volt_Avg
 db.bamast[,2]<-rowMeans(cbind(db.bamast[,2],newdf.a[,5]),na.rm=T)# T107_C_1_Avg...8m    !!!!!!!!!!!    
 db.bamast[,3]<-rowMeans(cbind(db.bamast[,3],newdf.a[,6]),na.rm=T)# T107_C_2_Avg...4m    !!!!!!!!!!!   wrong order
 db.bamast[,4]<-rowMeans(cbind(db.bamast[,4],newdf.a[,7]),na.rm=T)# T107_C_3_Avg...2m    !!!!!!!!!!!   
 db.bamast[,5]<-rowMeans(cbind(db.bamast[,5],newdf.a[,8]),na.rm=T)# T107_C_4_Avg...1m    !!!!!!!!!!!
 db.bamast[,6]<-rowMeans(cbind(db.bamast[,6],newdf.a[,9]),na.rm=T)# T107_C_5_Avg...0.5m    !!!!!!!!!!!
 db.bamast[,8]<-rowMeans(cbind(db.bamast[,8],newdf.a[,10]),na.rm=T)# T107_C_6_Avg...0.1m    !!!!!!!!!!!
 db.bamast[,9]<-rowMeans(cbind(db.bamast[,9],newdf.a[,11]),na.rm=T)# T107_C_7_Avg...-0.01m    !!!!!!!!!!!
 db.bamast[,10]<-rowMeans(cbind(db.bamast[,10],newdf.a[,12]),na.rm=T)# T107_C_8_Avg...-0.05m    !!!!!!!!!!!
 db.bamast[,7]<-rowMeans(cbind(db.bamast[,7],newdf.a[,13]),na.rm=T)# T107_C_9_Avg...0.2m    !!!!!!!!!!!
 db.bamast[,11]<-rowMeans(cbind(db.bamast[,11],newdf.a[,14]),na.rm=T)# T107_C_10_Avg...-0.1m    !!!!!!!!!!!
}


for(m in 2:12){# replace Nan with NA
db.bamast[,m] <- as.numeric(db.bamast[,m])
db.bamast[is.nan(db.bamast[,m]),m] <- NA
}
colnames(db.bamast)<-c("UTC","Tair_800","Tair_400","Tair_200","Tair_100",  "Tair_50", "Tair_20", "Tair_10", "Ts_1", "Ts_5", "Ts_10","HK-Bat_V")
db.bamast[,1]<-format( as.POSIXct(db.bamast[,1],origin=origin,tz="UTC"),format='%Y.%m.%d-%H:%M:%S')


# filter
# Tair_50 with no data since: 2011.02.03-08:30:00
db.bamast[format(as.POSIXct(db.bamast[,1],format='%Y.%m.%d-%H:%M:%S',origin=origin, tz = "UTC"),format='%Y.%m.%d-%H:%M:%S')>="2011.02.03-08:30:00",6]<-NA


for(years in 2010:2014){## 2010:2014
write.table(db.bamast[as.numeric(format(as.POSIXct(db.bamast[,1],format='%Y.%m.%d-%H:%M:%S',origin=origin, tz = "UTC"),format='%Y'))==years,],paste(out.path ,"BaMast2010_",years,".dat",sep=""),quote = F,dec=".",sep="\t",row.names=F)
}


## and update the big one
#write.table(db.bamast,paste(out.path ,"BaMast2010_all.dat",sep=""),quote = F,dec=".",sep="\t",row.names=F)
print(paste("BaMast2010 without problems!"))





