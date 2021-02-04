#############################################################################
##
##   BaMet2009         RAW to Level0
##
##   equal time steps, no gaps, table flag
##
##   by: Stephan.Lange@awi.de
##   last modified: 2019-02-06
##
#############################################################################
#
#  last modifications:
#  2019-02-06 SL new path
#  2017_04_24 SL add prec for 2009 and 2010 from BaMet98 Heidelberg database
#  2017_04_06 SL switch SwIn and SwOut in colnaming at the End
#  2017_02_06 SL correction of all snowdepth, rawdist and distcor-problems
#  2016-09-22 SL edit snow/air temperature ... everything to 2009
#  2016-02-15 SL sort/name soil
#  2015-12-14 SL add empty colums to 2009-2015; new order of colums; add new in.path(2015)
#
#############################################################################
#
#  Attention!!!
#  2009 and 2010(1/2) without precipitation!    (bk0)
#   - Protocols says that they should be in BaSoil, but they didnt
#   - use instead BaMet98 data from former heideberg database
#
#
#############################################################################

#############################################################################
# to run this script seperat, you have to uncomment the next 10 lines!
rm(list=ls())
if (.Platform$OS.type == "windows") {
  path<-read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt",sep="\t",header=T)
  maint<-read.table("N:/sparc/LTO/R_database/database_R/settings/maintance.txt",sep="\t",header=T)
  source("N:/sparc/LTO/R_database/database_R/settings/db_func.R")
}else{
  path<-read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt",sep="\t",header=T, fileEncoding="UTF-8")
  maint<-read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt",sep="\t",header=T)

  source("/sparc/LTO/R_database/database_R/settings/db_func.R")
}
#############################################################################


options(scipen=100) # for non-exponential display of numeric values

origin="1970-01-01"
folders<-c(paste0(path$w[path$n=="RAW.p"],"BaMet2010/bk0/"      ),
           paste0(path$w[path$n=="RAW.p"],"BaMet2010/bk1/"      ),
           paste0(path$w[path$n=="RAW.p"],"BaMet2010/bk2/2015/" ),
        #   paste0(path$w[path$n=="RAW.p"],"BaMet2010/bk2/2016/" ),
           paste0(path$w[path$n=="RAW.p"],"BaMet2015/"),
           paste0(path$w[path$n=="RAW.p"],"BaMet2010/bk2/"),
           paste0(path$w[path$n=="ONL.p"],"BaMet2015/"))

aktuell<-as.numeric(format(Sys.Date(),"%Y"))

for (year in aktuell){#2009:2019 aktuell
  start.date <-as.POSIXct(paste(year,"-01-01 00:00:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date   <-as.POSIXct(paste(year,"-",12,"-",31," 23:30:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")

  # create empty data frame with UTC time stamp every 30 min
  db.bamet <- matrix(ncol=43,nrow=length(seq(start.date,end.date,by="30 min")),-999)
  db.bamet[,c(2:43)] <- NA
  compl.temp <- matrix(ncol=2,nrow=length(seq(start.date,end.date,by="30 min")))
  db.bamet[,1] <- as.numeric(as.POSIXct(seq(start.date,end.date,by="30 min"),format='%Y-%m-%d %H:%M:%S'))
  compl.temp[,1] <- as.numeric(as.POSIXct(seq(start.date,end.date,by="30 min"),format='%Y-%m-%d %H:%M:%S'))
  colnames(compl.temp) <- c("UTC","erste")


  for(paz in c(1:6)){# 1:6 for all years
    inz.path <- folders[paz]
      cat("\nnow in ",folders[paz],"\n====================\n\n")
    files2read<-list.files(inz.path,pattern="*.dat")

    for(i in (1:length(files2read))){
      # if(paz==3 && files2read[i]=="20150903_20150101_BaMet2010_bk2Online.dat") {next}  # skip newest 2015 data
      # if(paz==3 && files2read[i]=="20150819_20130827_BaMet2010_bk2Online.dat") {next}  # skip newest 2015 data
      # if(paz==3 && files2read[i]=="Teil2.dat") {next}  # skip newest 2015 data
          cat("\nprocessing ",files2read[i],"\n====================\n\n")
      dada<-read.table(paste(inz.path,files2read[i],sep=""),sep=",",dec=".",header=F,skip=4, fill = TRUE,na="NAN")


      #    cat("\nreplace ",length(dada[as.character(dada[,ncol(dada)])=="Inf",ncol(dada)])," Inf-values \n")
      is.na(dada) <- sapply(dada, is.infinite)
      #
      colnames(dada) = paste0("V",seq_len(ncol(dada)))
      if(as.numeric(substr(lapply(dada[1,1],as.character),1,4))>year || as.numeric(substr(lapply(dada[length(dada[,1]),1],as.character),1,4))<year) {next} # skip file if wrong year
      dada <- check.double.entry(dada)
      dada[,1] <- as.numeric(as.POSIXct(dada[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC"))

      if(paz==1){    #Rain_Tot not in bk0
        colnames(dada)<-c("UTC","RECORD","Batt_Volt_Avg","Temp_200_Avg","RH_200_Avg","wind_speed","wind_dir","wind_dir_sd",
                          "Raw_Dist","distance_corr",
                          "Tair_4","Tair_100","Tair_20","Ts_252_2","Ts_252_12","Ts_252_32","Ts_252_62",
                          "Ts_252_102","Ts_252_152","Ts_203_2","Ts_203_5","Ts_203_23","Ts_203_53",
                          "Ts_203_93","Ts_203_143","Tpan_RAD",
                          "SR01Up_Avg","SR01Dn_Avg","IR01Up_Avg","IR01Dn_Avg",
                          "IR01DnCo_Avg","IR01UpCo_Avg","SR01Up_raw_Avg","SR01Dn_raw_Avg","IR01Up_raw_Avg","IR01Dn_raw_Avg","NetRs_Avg",
                          "NetRl_Avg","Albedo_Avg")
        dada$Rain_Tot<-NA; dada$SignalQuality<-NA
        dada$wind_speed_Max<-NA; dada$wind_speed_Min<-NA
        dada$Dsn<-NA #1.45-distance_corr
        # reorder data frame for missing rain_tot
        dada <- dada[,c("UTC","RECORD","Batt_Volt_Avg","Tair_100","Temp_200_Avg","RH_200_Avg","wind_speed","wind_dir","wind_dir_sd",
                        "wind_speed_Max","wind_speed_Min","Rain_Tot",
                        "Raw_Dist","SignalQuality","distance_corr","Dsn",
                        "Tair_4","Tair_20",
                        "Ts_252_2","Ts_252_12","Ts_252_32","Ts_252_62","Ts_252_102","Ts_252_152",
                        "Ts_203_2","Ts_203_5","Ts_203_23","Ts_203_53","Ts_203_93","Ts_203_143",
                        "SR01Up_Avg","SR01Dn_Avg","IR01Up_Avg","IR01Dn_Avg",
                        "IR01DnCo_Avg","IR01UpCo_Avg","SR01Up_raw_Avg","SR01Dn_raw_Avg","IR01Up_raw_Avg","IR01Dn_raw_Avg","NetRs_Avg",
                        "NetRl_Avg","Albedo_Avg","Tpan_RAD")]



      }else if(paz==2|paz==3){

        colnames(dada)<-c("UTC","RECORD","Batt_Volt_Avg","Temp_200_Avg","RH_200_Avg","wind_speed","wind_dir","wind_dir_sd",
                          "Rain_Tot","Raw_Dist","distance_corr",
                          "Tair_4",
                          "Tair_100","Tair_20","Ts_252_2","Ts_252_12","Ts_252_32","Ts_252_62",
                          "Ts_252_102","Ts_252_152","Ts_203_2","Ts_203_5","Ts_203_23","Ts_203_53",
                          "Ts_203_93","Ts_203_143","Tpan_RAD",
                          "SR01Up_Avg","SR01Dn_Avg","IR01Up_Avg","IR01Dn_Avg",
                          "IR01DnCo_Avg","IR01UpCo_Avg","SR01Up_raw_Avg","SR01Dn_raw_Avg","IR01Up_raw_Avg","IR01Dn_raw_Avg","NetRs_Avg",
                          "NetRl_Avg","Albedo_Avg")
        ## add empty colums
        dada$SignalQuality<-NA; dada$wind_speed_Max<-NA;
        dada$wind_speed_Min<-NA
        dada$Dsn<-NA #1.45-distance_corr

        # reorder data frame for missing rain_tot
        dada <- dada[,c("UTC","RECORD","Batt_Volt_Avg","Tair_100","Temp_200_Avg","RH_200_Avg","wind_speed","wind_dir","wind_dir_sd",
                        "wind_speed_Max","wind_speed_Min","Rain_Tot",
                        "Raw_Dist","SignalQuality","distance_corr","Dsn",
                        "Tair_4","Tair_20",
                        "Ts_252_2","Ts_252_12","Ts_252_32","Ts_252_62","Ts_252_102","Ts_252_152",
                        "Ts_203_2","Ts_203_5","Ts_203_23","Ts_203_53","Ts_203_93","Ts_203_143",
                        "SR01Up_Avg","SR01Dn_Avg","IR01Up_Avg","IR01Dn_Avg",
                        "IR01DnCo_Avg","IR01UpCo_Avg","SR01Up_raw_Avg","SR01Dn_raw_Avg","IR01Up_raw_Avg","IR01Dn_raw_Avg","NetRs_Avg",
                        "NetRl_Avg","Albedo_Avg","Tpan_RAD")]

      }else { # paz == 4
        #       colnames(dada)<-c("UTC","RECORD","Batt_Volt_Avg","Temp_200_Avg","RH_200_Avg","wind_speed_WVc(1)","wind_speed_WVc(2)",
        #                         "wind_speed_WVc(3)","Rain_Tot","Raw_Dist","snowheight","PT100_T_Avg(1)","PT100_T_Avg(2)","PT100_T_Avg(3)",
        #                         "PT100_T_Avg(4)","PT100_T_Avg(5)","PT100_T_Avg(6)","PT100_T_Avg(7)","PT100_T_Avg(8)","PT100_T_Avg(9)",
        #                         "PT100_T_Avg(10)","PT100_T_Avg(11)","PT100_T_Avg(12)","PT100_T_Avg(13)","PT100_T_Avg(14)","PT100_T_Avg(15)",
        #                         "PT100_T_Avg(16)","CM3Up_Avg","CM3Dn_Avg","CG3Up_Avg","CG3Dn_Avg","CG3DnCo_Avg","CG3UpCo_Avg","CM3Up_raw_Avg",
        #                         "CM3Dn_raw_Avg","CG3Up_raw_Avg","CG3Dn_raw_Avg","NetRs_Avg","NetRl_Avg","Albedo_Avg")
        ## since sep 2015
        #print(dada[1,1:13])
        colnames(dada)<-c("UTC","RECORD","Batt_U","Tair_200","RH_200","wind_speed","wind_dir","wind_dir_sd",
                          "wind_speed_Max","wind_speed_Min","Rain_Tot",
                          "Raw_Dist","SignalQuality","distance_corr",
                          "Tair_4",
                          "Tair_100","Tair_20","Ts_252_2","Ts_252_12","Ts_252_32","Ts_252_62",
                          "Ts_252_102","Ts_252_152","Ts_203_2","Ts_203_5","Ts_203_23","Ts_203_53",
                          "Ts_203_93","Ts_203_143","Tpan_RAD",
                          "SR01Up_Avg","SR01Dn_Avg","IR01Up_Avg","IR01Dn_Avg",
                          "IR01DnCo_Avg","IR01UpCo_Avg","SR01Up_raw_Avg","SR01Dn_raw_Avg","IR01Up_raw_Avg","IR01Dn_raw_Avg","NetRs_Avg",
                          "NetRl_Avg","Albedo_Avg")
        dada$Dsn<-NA
        dada <- dada[,c("UTC","RECORD","Batt_U","Tair_100","Tair_200","RH_200","wind_speed","wind_dir","wind_dir_sd",
                        "wind_speed_Max","wind_speed_Min","Rain_Tot",
                        "Raw_Dist","SignalQuality","distance_corr","Dsn",
                        "Tair_4","Tair_20",
                        "Ts_252_2","Ts_252_12","Ts_252_32","Ts_252_62","Ts_252_102","Ts_252_152",
                        "Ts_203_2","Ts_203_5","Ts_203_23","Ts_203_53","Ts_203_93","Ts_203_143",
                        "SR01Up_Avg","SR01Dn_Avg","IR01Up_Avg","IR01Dn_Avg",
                        "IR01DnCo_Avg","IR01UpCo_Avg","SR01Up_raw_Avg","SR01Dn_raw_Avg","IR01Up_raw_Avg","IR01Dn_raw_Avg","NetRs_Avg",
                        "NetRl_Avg","Albedo_Avg","Tpan_RAD")]

      }
      # merge all data into one dataframe
      newdf.a <- merge(compl.temp,dada,all.x=T, by="UTC")
      for(k in 2:43){
        db.bamet[,k]<-rowMeans(cbind(db.bamet[,k],newdf.a[,k+2]),na.rm=T)#
      }
    }
  }

  ##### a) snow distance manipulation
  ##### b) get prec from heidelberg db for 2009 & 2010
  #####
  if(year==2009){
    db.bamet[,15]                           <-round(1.45-db.bamet[,14] ,3)
    old.prec<-read.table("N:/geo5/SoilData/data/level0/BaMet1998/02_prec/BaMet1998prec_2009.dat",sep=",",dec=".",header=T)
    db.bamet[seq(1,17520,2),11]<-old.prec[,2]
    db.bamet[1:14570,11]<-NA # like the other sensors
  }
  if(year==2010){
    db.bamet[11395:length(db.bamet[,13]),14]<-1.5-db.bamet[11395:length(db.bamet[,14]),14]
    db.bamet[,15]                           <-round(1.5-db.bamet[,14] ,3)
    old.prec<-read.table("N:/geo5/SoilData/data/level0/BaMet1998/02_prec/BaMet1998prec_2010.dat",sep=",",dec=".",header=T)
    db.bamet[seq(1,11355,2),11]<-old.prec[1:5678,2]
                 }
  if(year>=2011 & year<=2014){db.bamet[,15]              <-db.bamet[,14]
                 db.bamet[,14]                           <-round(1.45-db.bamet[,15] ,3) }
  if(year==2015){#db.bamet[,12]<-round(db.bamet[,12],3)
                 db.bamet[,14]                           <-round(     db.bamet[,14] ,3)
                 db.bamet[1:11799,14]                    <-round(1.45-db.bamet[1:11799,14],3)## 11799==2015-09-03 19:00
                 db.bamet[,15]                           <-round(1.45-db.bamet[,14] ,3)}
  if(year>2015){ db.bamet[,15]                           <-round(1.45-db.bamet[,14] ,3)}
  
  
  if(year==2017){
  db.bamet[1:9350,15]                                    <-round(1.45-db.bamet[1:9350,14] ,3)
  db.bamet[9351:17520,15]                                <-round(1.57-db.bamet[9351:17520,14],3)}## 11799==2017-07-14 19:00

  if(year>2017){ db.bamet[,15]                           <-round(1.57-db.bamet[,14] ,3)}

  for(m in 2:43){# replace Nan with NA
    # db.bamet[,m] <- as.numeric(db.bamet[,m])
    db.bamet[is.nan(as.numeric(db.bamet[,m])),m] <- NA
    #if(is.na(db.bamet[,m])==FALSE) { db.bamet[,m] <- sprintf('%1.4f', db.bamet[,m]) }
  }


  db.bamet[(as.character(db.bamet[,43])=="inf"),43] <- NA



  colnames(db.bamet)<-c("UTC","batt_U","Tair_100","Tair_200","RH_200","wind_v_200","wind_deg_200","wind_sddeg_200",
                        "wind_vmax_200","wind_vmin_200","prec",
                        "distraw","snowsq","distcor","Dsn",
                        "Tair_4","Tair_20",
                        "Ts_252_2","Ts_252_12","Ts_252_32","Ts_252_62","Ts_252_102","Ts_252_152",
                        "Ts_203_2","Ts_203_5","Ts_203_23","Ts_203_53","Ts_203_93","Ts_203_143",
                        "SwIn","SwOut","LwOut_rawcor","LwIn_rawcor",
                        "LwOut","LwIn","SwIn_raw","SwOut_raw","LwOut_raw","LwIn_raw",
                        "SwNet","LwNet","Albedo","Tpan_NR1")
  # re-arange
  db.bamet<-db.bamet[,c("UTC","batt_U","Tair_4","Tair_20","Tair_100","Tair_200","RH_200",
                        "wind_v_200","wind_deg_200","wind_sddeg_200",
                        "wind_vmax_200","wind_vmin_200","prec",
                        "distraw","snowsq","distcor","Dsn",

                        "Ts_252_2","Ts_252_12","Ts_252_32","Ts_252_62","Ts_252_102","Ts_252_152",
                        "Ts_203_2","Ts_203_5","Ts_203_23","Ts_203_53","Ts_203_93","Ts_203_143",
                        "SwIn","SwOut","LwIn_rawcor","LwOut_rawcor",
                        "LwIn","LwOut","SwIn_raw","SwOut_raw","LwIn_raw","LwOut_raw",
                        "SwNet","LwNet","Albedo","Tpan_NR1")]
  db.bamet[,1]<-format( as.POSIXct(db.bamet[,1],origin=origin,tz="UTC"),format='%Y-%m-%d %H:%M')


  #if(file.exists(paste(out.path,"data",sep="")) == FALSE) { dir.create(paste(out.path,"data/",sep="")) }
  # write.table(db.bamet[as.numeric(format(as.POSIXct(db.bamet[,1],format='%Y-%m-%d %H:%M',origin=origin, tz = "UTC"),format='%Y'))==year,],
  #             paste0(paste(path$w[path$n=="LV0.p"]) ,"BaMet2009/00_full_dataset/BaMet2009_",year,".dat"),quote = F,dec=".",sep=",",row.names=F)

  write.table(db.bamet, paste0(paste(path$w[path$n=="LV0.p"]) ,"BaMet2009/00_full_dataset/BaMet2009_",year,"_lv0.dat"),quote = F,dec=".",sep=",",row.names=F)
  write.table(db.bamet[,c(1,14:17)], paste0(paste(path$w[path$n=="LV0.p"]) ,"BaMet2009/05_snow_depth/BaMet2009_Dsn_",year,"_lv0.dat"),quote = F,dec=".",sep=",",row.names=F)
  cat("#\n# BaMet2009 ",year," without problems!\n#\n")

} # end loop over years

# source('N:/geo5/SoilData/doc/scripts/database_R/Ba_02_Lvl0_Lvl1/LV0_to_LV1_BaMet2009.R')
# source('N:/geo5/SoilData/doc/scripts/database_R/Ba_03_Lvl1/LV1_plots_BaMet2009.R')
