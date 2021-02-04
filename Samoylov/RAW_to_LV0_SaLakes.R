#############################################################################
##
##   Template         RAW to Level0
##   (SaSnow2012)
##   equal time steps, no gaps
##
##   by: Stephan.Lange@awi.de
##   last modified: 2016-09-14
##
#############################################################################

##
##
##   18 steps to get wonderful data
##
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
options(scipen=100) # for non-exponential display of numeric values
origin  <- "1970-01-01"
aktuell <-as.numeric(format(Sys.Date(),"%Y"))
#############################################################################
## step 1.03
## loop 1 over years 
#############################################################################
for (year in 2012:aktuell){#2013:2016 2012:aktuell
  #############################################################################
  ## step 1.04
  ## set 2 empty tables with length of year
  ## columns: 2 (date table) and number of input table (storing table)
  #############################################################################
  cat("\nProcessing year",year,"\n====================\n\n")
  start.date <-as.POSIXct(paste(year,"-01-01 00:00:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date   <-as.POSIXct(paste(year,"-",12,"-",31," 23:30:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
  # create empty data frame with UTC time stamp every 30 min
  db.sasnow<-matrix(ncol=86,nrow=length(seq(start.date,end.date,by="30 min")),-999)
  db.sasnow[,c(2:86)]<-NA
  compl.temp<-matrix(ncol=2,nrow=length(seq(start.date,end.date,by="hour")))
  db.sasnow[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="30 min"),format='%Y-%m-%d %H:%M:%S'))
  compl.temp[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="30 min"),format='%Y-%m-%d %H:%M:%S'))
  colnames(compl.temp)<-c("UTC","erste")
  #############################################################################
  ## step 1.05
  ## set input.path and list all files
  #############################################################################
  inz.path<-paste0(path$w[path$n=="RAW.p"],"SaSnow2012/")
  files2read<-list.files(inz.path,pattern="*.dat")
  #############################################################################
  ## step 1.06
  ## loop 2 over all files
  #############################################################################
  
  for(i in 1:length(files2read)){#1:length(files2read)
    #############################################################################
    ## step 1.07
    ## read one file (skip headers, set NA-values)
    ## set temporal colnames
    #############################################################################
    cat("\nprocessing ",files2read[i],"\n====================\n\n")
    dada<-read.table(paste(inz.path,files2read[i],sep=""),sep=",",dec=".",header=F,skip=4, fill = TRUE,na="NAN")
    
    colnames(dada) = paste0("V",seq_len(ncol(dada)))
    #############################################################################
    ## step 1.09
    ## check file if dates are in running year of loop 1
    #############################################################################
    
    if(as.numeric(substr(lapply(dada[1,1],as.character),1,4))>year || as.numeric(substr(lapply(dada[length(dada[,1]),1],as.character),1,4))<year) {next} # skip file if wrong year
    cat(paste(dada[1,1],"     to     ",dada[length(dada[,1]),1],"    ",files2read[i]))
    #############################################################################
    ## step 1.10
    ## check file for double entries
    #############################################################################

    #############################################################################
    ## step 1.11
    ## convert date to numeric value
    #############################################################################
    
    dada[,1]<-as.numeric(as.POSIXct(dada[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC"))
    #############################################################################
    ## step 1.12a
    ## special case: former files with different columns
    ## set colnames
    #############################################################################
    if(length(dada[1,])==4){
      colnames(dada)<-  c("RECORD","UTC","temp","U_Bat")
      #############################################################################
      ## step 1.12b
      ## add additional columns to former dataset
      #############################################################################
      
      dada$S2_ice_Avg  <-NA
      dada$S2_water_Avg<-NA
      dada$S2_rho_Avg  <-NA
      dada$S3_ice_Avg  <-NA
      dada$S3_water_Avg<-NA
      dada$S3_rho_Avg  <-NA
      dada$S4_ice_Avg  <-NA
      dada$S4_water_Avg<-NA
      dada$S4_rho_Avg  <-NA
      
    }else{
      #############################################################################
      ## step 1.12
      ## standard case
      ## set original colnames
      #############################################################################
      
      colnames(dada)<-c("UTC","RECORD","BattV_Min","PTemp_C_Avg","S2_ice_Avg","S2_water_Avg","S2_rho_Avg","S2_C_highFq_Avg","S2_C_lowFq_Avg",	"S2_Phi_highFq_Avg",
                        "S2_Phi_lowFq_Avg",	"S3_ice_Avg",	"S3_water_Avg",	"S3_rho_Avg",	"S3_C_highFq_Avg",	"S3_C_lowFq_Avg",	"S3_Phi_highFq_Avg",	"S3_Phi_lowFq_Avg",
                        "S4_ice_Avg",	"S4_water_Avg",	"S4_rho_Avg",	"S4_C_highFq_Avg",	"S4_C_lowFq_Avg",	"S4_Phi_highFq_Avg",	"S4_Phi_lowFq_Avg",	"ChipTemp_SPA_Max",
                        "Temp_Corr_Distance_0_Avg",	"Temp_Corr_Distance_1_Avg",	"Temp_Corr_Distance_2_Avg",	"Temp_Corr_Distance_3_Avg",	"Temp_Corr_Distance_4_Avg",
                        "Temp_Corr_Distance_5_Avg",	"Temp_Corr_Distance_6_Avg",	"Temp_Corr_Distance_7_Avg",	"Temp_Corr_Distance_8_Avg",	"Temp_Corr_Distance_9_Avg",
                        "Raw_Distance_0_Avg",	"Raw_Distance_1_Avg",	"Raw_Distance_2_Avg",	"Raw_Distance_3_Avg",	"Raw_Distance_4_Avg",	"Raw_Distance_5_Avg",	"Raw_Distance_6_Avg",
                        "Raw_Distance_7_Avg",	"Raw_Distance_8_Avg",	"Raw_Distance_9_Avg",	"QA_Distance_0_Avg",	"QA_Distance_1_Avg",	"QA_Distance_2_Avg",	"QA_Distance_3_Avg",
                        "QA_Distance_4_Avg",	"QA_Distance_5_Avg",	"QA_Distance_6_Avg",	"QA_Distance_7_Avg",	"QA_Distance_8_Avg",	"QA_Distance_9_Avg",	"Snow_Temp_Avg(1)",
                        "Snow_Temp_Avg(2)",	"Snow_Temp_Avg(3)",	"Snow_Temp_Avg(4)",	"Snow_Temp_Avg(5)",	"Snow_Temp_Avg(6)",	"Snow_Temp_Avg(7)",	"Snow_Temp_Avg(8)",
                        "Snow_Temp_Avg(9)",	"Snow_Temp_Avg(10)",	"Snow_Temp_Avg(11)",	"Snow_Temp_Avg(12)",	"Surf_Temp_Avg(1)",	"Surf_Temp_Avg(2)",	"Surf_Temp_Avg(3)",
                        "Surf_Temp_Avg(4)",	"Air_Temp_Avg",	"Soil_Temp_Avg(1)",	"Soil_Temp_Avg(2)",	"Soil_Temp_Avg(3)",	"Soil_Temp_Avg(4)",	"Soil_Temp_Avg(5)",	"Soil_Temp_Avg(6)",
                        "Soil_Temp_Avg(7)",	"Soil_Temp_Avg(8)",	"Temp_Corr_Distance_Centre_Avg",	"Temp_Corr_Distance_Crack_Avg",	"Raw_Distance_Centre_Avg",	"Raw_Distance_Crack_Avg",
                        "QA_Distance_Centre_Avg",	"QA_Distance_Crack_Avg")
    }
    #############################################################################
    ## step 1.13
    ## new arrangement / order of columns (all Temperatures together, ascending, ... )
    #############################################################################
    dada<-dada[,c("UTC","RECORD","BattV_Min","PTemp_C_Avg",	"ChipTemp_SPA_Max",
                  "S2_ice_Avg",	"S3_ice_Avg","S4_ice_Avg","S2_water_Avg",	"S3_water_Avg","S4_water_Avg",	"S2_rho_Avg","S3_rho_Avg","S4_rho_Avg",
                  "S2_C_highFq_Avg","S2_C_lowFq_Avg","S3_C_highFq_Avg",	"S3_C_lowFq_Avg","S4_C_highFq_Avg",	"S4_C_lowFq_Avg",
                  "S2_Phi_highFq_Avg",  "S2_Phi_lowFq_Avg",			"S3_Phi_highFq_Avg",	"S3_Phi_lowFq_Avg",	"S4_Phi_highFq_Avg",	"S4_Phi_lowFq_Avg",
                  "Temp_Corr_Distance_0_Avg",	"Temp_Corr_Distance_1_Avg",	"Temp_Corr_Distance_2_Avg",	"Temp_Corr_Distance_3_Avg",	"Temp_Corr_Distance_4_Avg",
                  "Temp_Corr_Distance_5_Avg",	"Temp_Corr_Distance_6_Avg",	"Temp_Corr_Distance_7_Avg",	"Temp_Corr_Distance_8_Avg",	"Temp_Corr_Distance_9_Avg",
                  "Temp_Corr_Distance_Centre_Avg",	"Temp_Corr_Distance_Crack_Avg",	 "Raw_Distance_0_Avg",	"Raw_Distance_1_Avg",	"Raw_Distance_2_Avg",
                  "Raw_Distance_3_Avg",	"Raw_Distance_4_Avg",	"Raw_Distance_5_Avg",	"Raw_Distance_6_Avg",    "Raw_Distance_7_Avg",	"Raw_Distance_8_Avg",	
                  "Raw_Distance_9_Avg","Raw_Distance_Centre_Avg",	"Raw_Distance_Crack_Avg",	"QA_Distance_0_Avg",	"QA_Distance_1_Avg",	"QA_Distance_2_Avg",
                  "QA_Distance_3_Avg","QA_Distance_4_Avg",	"QA_Distance_5_Avg",	"QA_Distance_6_Avg",	"QA_Distance_7_Avg",	"QA_Distance_8_Avg",	
                  "QA_Distance_9_Avg","QA_Distance_Centre_Avg",	"QA_Distance_Crack_Avg",
                  "Snow_Temp_Avg(1)",
                  "Snow_Temp_Avg(5)",	"Snow_Temp_Avg(9)",	"Snow_Temp_Avg(10)",	"Snow_Temp_Avg(11)",	"Snow_Temp_Avg(12)",	"Snow_Temp_Avg(8)",	"Snow_Temp_Avg(4)",
                  "Snow_Temp_Avg(3)",	"Snow_Temp_Avg(2)",	"Snow_Temp_Avg(6)",	"Snow_Temp_Avg(7)",	"Surf_Temp_Avg(1)",	"Surf_Temp_Avg(2)",	"Surf_Temp_Avg(3)",
                  "Surf_Temp_Avg(4)",	"Air_Temp_Avg",	"Soil_Temp_Avg(1)",	"Soil_Temp_Avg(2)",	"Soil_Temp_Avg(3)",	"Soil_Temp_Avg(4)",	"Soil_Temp_Avg(5)",	"Soil_Temp_Avg(6)",
                  "Soil_Temp_Avg(8)", "Soil_Temp_Avg(7)")] # 7 & 8 sind hier vertauscht!!!
    #############################################################################
    ## step 1.14
    ## merge input data with date table
    #############################################################################
    
    newdf.a <- merge(compl.temp,dada,all.x=T, by="UTC")
    
    #############################################################################
    ## step 1.15
    ## merge date table with storing table
    #############################################################################
    
    for(k in 2:(length(db.sasnow[1,]))){
      db.sasnow[,k]<-rowMeans(cbind(db.sasnow[,k],newdf.a[,k+2]),na.rm=T)#
    }
  }
  
  #############################################################################
  ## step 1.16
  ## convert numeric dates back to date format
  #############################################################################
  
  
  db.sasnow[,1]<-format( as.POSIXct(db.sasnow[,1],origin=origin,tz="UTC"),format='%Y-%m-%d %H:%M')
  
  #############################################################################
  ## step 1.17
  ## set "sparc" colnames
  #############################################################################
  
  colnames(db.sasnow)<-c("UTC","batt_U","Tpan_CR1000","Tpan_SPA",
                         
                         "ice_2","ice_3","ice_4",  "water_2","water_3","water_4",  "rho_2","rho_3","rho_4",
                         "cap_highFq_2","cap_lowFq_2","cap_highFq_3",	"cap_lowFq_3","cap_highFq_4","cap_lowFq_4",
                         "phi_highFq_2","phi_lowFq_2","phi_highFq_3",	"phi_lowFq_3","phi_highFq_4","phi_lowFq_4",
                         
                         "distcor_0","distcor_1","distcor_2","distcor_3","distcor_4","distcor_5","distcor_6","distcor_7","distcor_8","distcor_9","distcor_Centre","distcor_Crack",
                         "distraw_0","distraw_1","distraw_2","distraw_3","distraw_4","distraw_5","distraw_6","distraw_7","distraw_8","distraw_9","distraw_Centre","distraw_Crack",
                         "QA_dist_0","QA_dist_1","QA_dist_2","QA_dist_3","QA_dist_4","QA_dist_5","QA_dist_6","QA_dist_7","QA_dist_8","QA_dist_9","QA_dist_Centre","QA_dist_Crack",
                         
                         "Tsn_m4","Tsn_00","Tsn_05","Tsn_10","Tsn_15","Tsn_20","Tsn_25","Tsn_30","Tsn_35","Tsn_40","Tsn_45","Tsn_50", # needles
                         "Ts_00_1",	"Ts_00_2",	"Ts_00_3","Ts_00_4",      	"Tair_80",
                         "Ts_00",	"Ts_05",	"Ts_10",	"Ts_20",	"Ts_40",	"Ts_60", "Ts_80",	"Ts_100" )
  #############################################################################
  ## step 1.18
  ## safe data to txt-file
  #############################################################################
  
  write.table(db.sasnow[as.numeric(format(as.POSIXct(db.sasnow[,1],format='%Y-%m-%d %H:%M',origin=origin, tz = "UTC"),format='%Y'))==year,-2],
              paste0(path$w[path$n=="LV0.p"],"SaSnow2012/00_full_dataset/SaSnow2012_",year,".dat"),quote = F,dec=".",sep=",",row.names=F)
  
  
} # end loop over years

cat("\n#\n# SaSnow2012 without problems!\n#\n")


