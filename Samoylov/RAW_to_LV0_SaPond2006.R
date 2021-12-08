#############################################################################
##
##   SaPond2006       RAW to Level0
##
##   equal time steps
##
##   by: Stephan.Lange@awi.de and Niko.Bornemann@awi.de
##
##   last modified: 2016/09/08
##
#############################################################################
##
##   no data since 2014 ... the station is not running anymore!!!
#    new station SaPond2014 on same position, but with new data collection (not comparable)
#
#############################################################################
#
#  last modifications:
#  - implement new path
#  - 
#############################################################################

#############################################################################
# to run this script seperat, you have to uncomment the next 10 lines!
# rm(list = ls())
# if (.Platform$OS.type == "windows") {
#   p.1 <- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/path_win.txt", sep = "\t", header = T)
#   p.1maint <- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)
#   
#   source("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_func.R")
# } else {
#   p.1 <- read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
#   p.1maint <- read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)
#   
#   source("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_func.R")
# }
###...........................................................................
#############################################################################
#
# there is a software update in 2009 
# some value are new, some values are gone
# 
#
#############################################################################
# update if necessary:
start.year <-2006 ## 2006
end.year   <-2014 ## 2014
# run.year <- start.year:end.year
#############################################################################

options(warn=-1)# necessary! ... there are some strange warnings 
in.path    <-paste0(p.1$w[p.1$n=="RAW.p"],"SaPond2006/")
out.path   <-paste0(p.1$w[p.1$n=="LV0.p"],"SaPond2006/00_full_dataset/")
origin     <-"1970-01-01"



files2read <-list.files(in.path,pattern="*.dat")

grr        <-length(files2read)
#,format(Sys.Date(),"%Y")
start.date <-as.POSIXct(paste0("01.01.",start.year),format='%d.%m.%Y', tz = "UTC")
end.date   <-as.POSIXct(paste0("31.12.",  end.year),format='%d.%m.%Y', tz = "UTC")

db.sapond  <-matrix(ncol=34,nrow=length(seq(start.date,end.date,by="hour")),-999)
db.sapond[,c(2:34)]<-NA

compl.temp <-matrix(ncol=2,nrow=length(seq(start.date,end.date,by="hour")))

db.sapond[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="hour"),format='%Y-%m-%d %H:%M'))
compl.temp[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="hour"),format='%Y-%m-%d %H:%M'))
colnames(compl.temp)<-c("UTC","erste")



for(kk in (1:length(files2read))){#1:length(files2read)
  cat("# ",files2read[kk],"\n#\n")
  data.temp<-read.table(paste0(in.path,files2read[kk]),sep=",",dec=".",header=F,skip=4)
  #cat("# ",length(data.temp[1,])," x ",length(data.temp[,1]),"\n#\n")
  if(length(data.temp[1,])==44){
    data.temp<-data.temp[,-c(2,4:19)]
    colnames(data.temp)<-c("UTC",
                    #       "RECORD",
                           "Batt_Volt_Avg",
                    #       "PT100_VR_Avg(1)","PT100_VR_Avg(2)","PT100_VR_Avg(3)","PT100_VR_Avg(4)","PT100_VR_Avg(5)","PT100_VR_Avg(6)",
                     #      "PT100_VR_Avg(7)","PT100_VR_Avg(8)","PT100_VR_Avg(9)","PT100_VR_Avg(10)","PT100_VR_Avg(11)","PT100_VR_Avg(12)",
                      #     "PT100_VR_Avg(13)","PT100_VR_Avg(14)","PT100_VR_Avg(15)","PT100_VR_Avg(16)",
                           "PT100_T_Avg(1)","PT100_T_Avg(2)","PT100_T_Avg(3)","PT100_T_Avg(4)","PT100_T_Avg(5)","PT100_T_Avg(6)",
                           "PT100_T_Avg(7)","PT100_T_Avg(8)","PT100_T_Avg(9)","PT100_T_Avg(10)","PT100_T_Avg(11)","PT100_T_Avg(12)",
                           "PT100_T_Avg(13)","PT100_T_Avg(14)","PT100_T_Avg(15)","PT100_T_Avg(16)",
                           "Waterlevel_mV",
                           "PA_uS_Avg1","PA_uS_Avg2","PA_uS_Avg3","PA_uS_Avg4","PA_uS_Avg5",#fallen weg
                           "radio1_mV","radio2_mV",
                           "snow_m_DT")
   data.temp$Waterlevel_mV<-data.temp$Waterlevel_mV/(-2500)
   data.temp$radio1_Wm2 <-data.temp$radio1_mV * 1000/13.3 # sehr dumm gelaufen, im cr1000 programm wurden die gemessen
   data.temp$radio2_Wm2 <-data.temp$radio2_mV * 1000/14.7 # radio-werte mit deren sd-werte ueberschrieben...nutzlos!!!
   data.temp$snow_m_TCDT<-data.temp$snow_m_DT * ((data.temp[,8]+273.15)/273.15)^.5
   data.temp$ATT_C_1_Avg<-NA
   data.temp$ATT_C_2_Avg<-NA
   data.temp$CTT_C_1_Avg<-NA
   data.temp$CTT_C_2_Avg<-NA
   data.temp<-data.temp[,c(1:18,20:26,28,29,27,30:34,19)]
   #data.temp<-data.temp[,c(1:18,29:33,27,28,19:22,23:26)]
 
  }else{
    data.temp<-data.temp[,-c(2)]
    colnames(data.temp)<-   c("UTC",
                             # "RECORD",
                              "Batt_Volt_Avg",
                              "PT100_T_Avg(1)","PT100_T_Avg(2)","PT100_T_Avg(3)","PT100_T_Avg(4)","PT100_T_Avg(5)","PT100_T_Avg(6)",
                              "PT100_T_Avg(7)","PT100_T_Avg(8)","PT100_T_Avg(9)","PT100_T_Avg(10)","PT100_T_Avg(11)","PT100_T_Avg(12)",
                              "PT100_T_Avg(13)","PT100_T_Avg(14)","PT100_T_Avg(15)","PT100_T_Avg(16)",
                              "radio1_Wm2","radio2_Wm2",# hier wird schon gerechnet
                              "snow_m_DT_Avg","snow_m_TCDT_Avg",
                              "ATT_C_1_Avg","ATT_C_2_Avg","CTT_C_1_Avg","CTT_C_2_Avg")# kommen dazu

    data.temp$radio1_mV <-data.temp$radio1_Wm2*13.3/1000
    data.temp$radio2_mV <-data.temp$radio2_Wm2*14.7/1000
    data.temp$PA_uS_Avg1<-NA
    data.temp$PA_uS_Avg2<-NA
    data.temp$PA_uS_Avg3<-NA
    data.temp$PA_uS_Avg4<-NA
    data.temp$PA_uS_Avg5<-NA
    data.temp$Waterlevel_mV<-NA
    data.temp<-data.temp[,c(1:18,29:33,27,28,19:26,34)]
    
    
  }
  data.temp$UTC<-round(as.POSIXct(data.temp[,1],format='%Y-%m-%d %H:%M:%S', origin=origin,tz = "UTC"),"hour")
  data.temp$UTC<-as.numeric(data.temp$UTC)
  
  data.temp[,2:length(data.temp[1,])] <- sapply(data.temp[,2:length(data.temp[1,])], as.num)

  newdf.a   <- merge(compl.temp,data.temp,all.x=T, by="UTC")

    for(i in 2:length(db.sapond[1,])){
    db.sapond[,i] <-rowMeans(cbind(db.sapond[,i],newdf.a[,i+1]),na.rm=T)
    }

}
colnames(db.sapond)<-c("UTC","batt_U",
                       "Tw_fx_cen_101","Tw_fx_cen_86","Tw_fx_cen_67","Tw_5","Tair_cen_5","Tair_cen_108",
                       "Tw_fx_edg_33","Tw_fx_edg_20","Tw_fx_edg_0","Tair_edg_9","Tair_edg_111",
                       "Ts_0", "Ts_16","Ts_33","Tw_10","Tw_15",
                       "PA_uS_1","PA_uS_2","PA_uS_3","PA_uS_4","PA_uS_5",
                       "Rad_raw_edg","Rad_raw_cen","RadNet_edg","RadNet_cen",
                       "distraw","distcor",
                       "ATT_dw","ATT_up","CTT_dw","CTT_up",
                       "WT")

db.sapond<-db.sapond[,-c(24:25)]# remove "Rad_raw_..."
db.sapond<-db.sapond[,c(1,2,14,15,16,13:9,8,7,6,17,18,5,4,3,19:32)]# rearrangement
db.sapond[,1]<-format( as.POSIXct(db.sapond[,1],origin=origin,tz="UTC"),format='%Y-%m-%d %H:%M')



for(m in 2:length(db.sapond[1,])){# replace Nan with NA
  db.sapond[is.nan(as.numeric(db.sapond[,m])),m] <- NA
}



for(years in start.year:end.year){
  write.table(db.sapond[as.numeric(format(as.POSIXct(db.sapond[,1],format='%Y-%m-%d %H:%M',origin=origin, tz = "UTC"),
                                          format='%Y'))==years,],paste(out.path ,"SaPond2006_",years,"_lv0.dat",sep=""),
              quote = F,dec=".",sep=",",row.names=F)
  cat("\n#\n# SaPond2006",years," without problems!\n#\n")
}
## and update the big one
#write.table(db.sapond,paste(out.path ,"SaPond2006_all.dat",sep=""),quote = F,dec=".",sep=",",row.names=F)




