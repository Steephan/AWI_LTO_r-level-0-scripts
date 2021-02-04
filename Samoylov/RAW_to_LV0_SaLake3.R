#############################################################################
##
##   SaLake2         RAW to Level0
##   
##   equal time steps, no gaps
##
##   by: Christoph.Georgi@awi.de, Stephan.Lange@awi.de
##   last modified: 2018-03-12
##
#############################################################################

##
##
##   18 steps to get wonderful data
##     \\ modified from template to fulfill the special needs of SaLakes
##
##

#############################################################################
## step 1.01
## set path settings for different systems linux vs. windoof
#############################################################################
# to run this script seperat, you have to uncomment the next 10 lines!
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
#############################################################################
## step 1.02
## set running options years, ...
#############################################################################

{# options
options(scipen=100) # for non-exponential display of numeric values
origin  <- "1970-01-01"
aktuell <-as.numeric(format(Sys.Date(),"%Y"))
}

#############################################################################
## ~~ drastic changes, reading all data BEFORE looping through years ~~
##      ~~ other routine would cause overhead - sorry Stephan! ~~
## step 1.03 
## set input.path and list all files
#############################################################################

{# files
inz.path<-"/home/christoph/Dokumente/AWI/SaLakes/SaLakes/SaLake3_DryLake/" # adjust
meteo_dir<-"/home/christoph/Dokumente/AWI/Lake/FLake/inputdata_extended/"  # adjust
#~ inz.path<-paste0(path$w[path$n=="RAW.p"],"SaLakes/")
files2read<-list.files(inz.path,pattern="*.csv")
file1<-"/home/christoph/Dokumente/AWI/Lake/FLake/inputdata/laketemplevel_corr_Liz.csv" # adjust
}

#############################################################################
## step 1.04
## read all files, set sparc colnames
#############################################################################

{# first data circle, 2009-2012
dataset1<-read.table(file1,sep=";",dec=",",skip=24)
#~ lakenames<-c("#","SA_Lake_4","?????","SA_Lake_3","SA_Lake_1","KU_Lake_1","SA_Lake_2")
lakenames<-c("#","SaLake4","?????","SaLake3","SaLake1","KuLake1","SaLake2")

lake<-unlist(strsplit(readLines(file1,n=1), split=";")) #read lake names from table
lake[ncol(dataset1)]<-tail(unique(lake),1) #fix missing last element (strsplit)
for(i in 1:length(lake)) if(lake[i]=="") lake[i]<-lake[i-1] #close gaps
colnames(dataset1)<-unlist(strsplit(readLines(file1,n=3)[3], split=";")) #read header
dataset1[,1]<-as.POSIXct(strptime(dataset1[,1], "%d.%m.%Y%H:%M")) #format time

dataset1.crop<-dataset1[,c(1,which(lake=="DryLake"))]
colnames(dataset1.crop)<-c("UTC","P_0","Tw_0","Tw_200","Tw_400")[1:ncol(dataset1.crop)]
}

{# second data circle 2012-ongoing, NO DATA
}

#############################################################################
## step 1.05
## check and correct/interpolate/round all data (manually)
#############################################################################

{# some checks and corrections, NO DATA
}

{### P_0 -> water level calculation: waterlevel = (P_0 - PA)/Rho_water
}

#############################################################################
## step 1.06
## generate one object with all data, kick out duplicates (, check again)
#############################################################################
{# put all together, FIRST PART NOT NEEDED
#~ minmaxdate<-range(data.0m[,1],data.2m[,1],data.4m[,1]) # ,na.rm=T
#~ db.salakes<-data.frame(UTC=as.numeric(seq(minmaxdate[1],minmaxdate[2],"1 hour")))
#~ data.0m[,1]<-as.numeric(data.0m[,1])
#~ data.2m[,1]<-as.numeric(data.2m[,1])
#~ data.4m[,1]<-as.numeric(data.4m[,1])
#~ data.6m[,1]<-as.numeric(data.6m[,1])

#~ db.salakes<-Reduce(function(x,y) merge(x,y,all.x=T),list(db.salakes,data.0m,data.2m,data.4m)) # ,data.6m
#~ db.salakes<-db.salakes[-which(duplicated(db.salakes[,1])),] # delete duplicates

# first dataset
#~ db.dataset1<-as.numeric(seq(dataset1.crop[1,1],minmaxdate[1],"1 hour"))
#~ db.dataset1<-db.dataset1[-length(db.dataset1)] # delete last element
#~ db.dataset1<-data.frame(UTC=db.dataset1)
#~ dataset1.crop[,1]<-as.numeric(dataset1.crop[,1])

#~ db.dataset1<-merge(db.dataset1,dataset1.crop,all.x=T)
#~ db.dataset1<-db.dataset1[-which(duplicated(db.dataset1[,1])),] # delete duplicates

# combine
db.salakes<-dataset1.crop
}

#############################################################################
## step 1.07
## convert numeric dates back to date format #~ plot whole thing
#############################################################################

db.salakes[,1]<-as.POSIXct(db.salakes[,1],origin=origin,tz="UTC")

png("/tmp/Sa_Lake_3.png",height=600,width=900)
plot(db.salakes[,c(1,2)],type="l",ylim=c(-30,30),ylab="")
lines(db.salakes[,c(1,3)],col=2)
lines(db.salakes[,c(1,4)],col=3)
lines(db.salakes[,c(1,5)],col=4)
dev.off()

#############################################################################
## step 1.08
## loop 1 over years (in this script different as usual: after data treatment)
#############################################################################

for (year in 2009:aktuell){
  cat("\nProcessing year",year,"\n====================\n\n")
  start.date <-as.POSIXct(paste0(year,"-01-01 00:00:00"),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date   <-as.POSIXct(paste0(year,"-",12,"-",31," 23:00:00"),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
  
  write.table(db.salakes[as.numeric(format(as.POSIXct(db.salakes[,1],format='%Y-%m-%d %H:%M',origin=origin, tz = "UTC"),format='%Y'))==year,],
              paste0("/tmp/Sa_Lake3_",year,".dat"),quote = F,dec=".",sep=",",row.names=F)
              #paste0(path$w[path$n=="LV0.p"],"/SaLakes/00_full_dataset/Sa_Lake2_",year,".dat"),quote = F,dec=".",sep=",",row.names=F)
              #  # like this?
}


cat("\n#\n# SaLakes without problems!\n#\n")


