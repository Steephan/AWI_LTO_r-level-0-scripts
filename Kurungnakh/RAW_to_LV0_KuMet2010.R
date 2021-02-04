#############################################################################
##
##   RAW to Level0
##   (KuMet2010)
##   equal time steps, no gaps
##
##   by: Stephan.Lange@awi.de & Niko.Bornemann@awi.de
##   last modified: 2016-11-22
##
#############################################################################
##
## -----
##   4 different input data file formats
##
##
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
for (year in 2010:2010){#2013:2016 2012:aktuell
  #############################################################################
  ## step 1.04
  ## set 2 empty tables with length of year
  ## columns: 2 (date table) and number of input table (storing table)
  #############################################################################
  cat("\nProcessing year",year,"\n====================\n\n")
  start.date <-as.POSIXct(paste(year,"-01-01 00:00:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date   <-as.POSIXct(paste(year,"-",12,"-",31," 23:00:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
  # create empty data frame with UTC time stamp every 30 min
  db.kumet.air<-matrix(ncol=71,nrow=length(seq(start.date,end.date,by="hour")),-999)
  db.kumet.air[,c(2:71)]<-NA
  db.kumet.soil<-matrix(ncol=65,nrow=length(seq(start.date,end.date,by="hour")),-999)
  db.kumet.soil[,c(2:65)]<-NA

  compl.met<-matrix(ncol=2,nrow=length(seq(start.date,end.date,by="hour")))
  compl.soil<-matrix(ncol=2,nrow=length(seq(start.date,end.date,by="hour")))

  db.kumet.air[,1] <-as.numeric(as.POSIXct(seq(start.date,end.date,by="hour"),format='%Y-%m-%d %H:%M:%S'))
  db.kumet.soil[,1] <-as.numeric(as.POSIXct(seq(start.date,end.date,by="hour"),format='%Y-%m-%d %H:%M:%S'))

  compl.met[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="hour"),format='%Y-%m-%d %H:%M:%S'))
  compl.soil[,1]<-as.numeric(as.POSIXct(seq(start.date,end.date,by="hour"),format='%Y-%m-%d %H:%M:%S'))

  colnames(compl.met)<-c("UTC","erste");  colnames(compl.soil)<-c("UTC","erste")
  #############################################################################
  ## step 1.05
  ## set input.path and list all files
  #############################################################################
  inz.01.path<-paste0(path$w[path$n=="RAW.p"],"KuMet2010/01_air/")
  files.01<-list.files(inz.01.path,pattern="*.dat")

  inz.02.path<-paste0(path$w[path$n=="RAW.p"],"KuMet2010/02_soil/")
  files.02<-list.files(inz.02.path,pattern="*.dat")

  inz.03.path<-paste0(path$w[path$n=="RAW.p"],"KuMet2010/03_wind/")
  files.03<-list.files(inz.03.path,pattern="*.dat")

#############################################################################   main input tasks             main input tasks
## step 1.06 -----
## loop 2 over all temperature files
#############################################################################

  for(i in 1:length(files.01)){# soil temperature 1:length(files.01)
    #############################################################################
    ## step 1.07
    ## read one file (skip headers, set NA-values)
    ## set temporal colnames
    #############################################################################
    cat("\nprocessing ",files.01[i],"\n====================\n\n")
    dada.met<-read.table(paste(inz.01.path,files.01[i],sep=""),sep=",",dec=".",header=F,skip=4, fill = TRUE,na="NAN")

    colnames(dada.met) = paste0("V",seq_len(ncol(dada.met)))
    #############################################################################
    ## step 1.09
    ## check file if dates are in running year of loop 1
    #############################################################################

    if(as.numeric(substr(lapply(dada.met[1,1],as.character),1,4))>year || as.numeric(substr(lapply(dada.met[length(dada.met[,1]),1],as.character),1,4))<year) {next} # skip file if wrong year
    cat(paste(dada.met[1,1],"     to     ",dada.met[length(dada.met[,1]),1],"    ",files.01[i]))
    #############################################################################
    ## step 1.10
    ## check file for double entries
    #############################################################################
    if(("TRUE" %in% duplicated(dada.met))==TRUE) { # check for fully double entries
      doouble <- duplicated(dada.met)
      cat(paste(length(which(doouble=="TRUE")),"duplicated records found in file",files.01[i],"\n",
                "first entry:",dada.met[which(doouble=="TRUE")[1],1],"\n  last entry:",dada.met[which(doouble=="TRUE")[length(which(doouble=="TRUE"))],1],"\n\n"))
      dada.met <- unique(dada.met)  # remove double entries

    } else if(("TRUE" %in% duplicated(dada.met[,1]))==TRUE){  # check for multiple different data records for same! timestamp
      doouble <- duplicated(dada.met[,1])
      cat(paste(length(which(doouble=="TRUE")),"multiple records found in file",files.01[i],"\n",
                "first entry:",dada.met[which(doouble=="TRUE")[1],1],"\n  last entry:",dada.met[which(doouble=="TRUE")[length(which(doouble=="TRUE"))],1],"\n\n"))
      dd<-which(dada.met[,1] %in% dada.met[which(doouble=="TRUE"),1])
      dada.met <- dada.met[-dd,]  # remove double entries

    } else { cat("\n No double data entries found in",files.01[i],"\n\n")    }
    #############################################################################
    ## step 1.11
    ## convert date to numeric value
    #############################################################################

    dada.met[,1]<-as.numeric(as.POSIXct(dada.met[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC"))
    #############################################################################
    ## step 1.12a
    ## special case: former files with different columns
    ## set colnames
    ## replace "TIMESTAMP" -> "UTC"
    #############################################################################
       colnames(dada.met)<-c("UTC","RECORD","Batt_Volt_Avg","PTemp_C_Avg","Rain_Tot",
                             "AirTC_Avg(1)","AirTC_Avg(2)","RH_Avg(1)","RH_Avg(2)","SW_Avg(1)",
                             "SW_Avg(2)","LW_Avg(1)","LW_Avg(2)","NR01TC_Avg","IRTSP_Avg","T_surf_Avg")


    newdf.t <- merge(compl.met,dada.met,all.x=T, by="UTC")

    #############################################################################
    ## step 1.15
    ## merge date table with storing table
    #############################################################################

    for(k in 2:(length(db.kumet.air[1,]))){
      db.kumet.air[,k]<-rowMeans(cbind(db.kumet.air[,k],newdf.t[,k+2]),na.rm=T)#
    }
  } # soil temperature
#############################################################################   soil temperature
## step 1.06b -----
## loop 2 over all files
#############################################################################
  #if(year<2015){files.02<-files.03;inz.02.path<-inz.03.path;}
  for(i in 1:length(files.02)){# soil moisture
    #############################################################################
    ## step 1.07b
    ## read one file (skip headers, set NA-values)
    ## set temporal colnames
    #############################################################################
    cat("\nprocessing ",files.02[i],"\n====================\n\n")
    dada.soil<-read.table(paste(inz.02.path,files.02[i],sep=""),sep=",",dec=".",header=F,skip=4, fill = TRUE,na="NAN")[,-2]

    colnames(dada.soil) = paste0("V",seq_len(ncol(dada.soil)))
    #############################################################################
    ## step 1.09b
    ## check file if dates are in running year of loop 1
    #############################################################################

    if(as.numeric(substr(lapply(dada.soil[1,1],as.character),1,4))>year || as.numeric(substr(lapply(dada.soil[length(dada.soil[,1]),1],as.character),1,4))<year) {next} # skip file if wrong year
    cat(paste(dada.soil[1,1],"     to     ",dada.met[length(dada.soil[,1]),1],"    ",files.02[i]))
    #############################################################################
    ## step 1.10b
    ## check file for double entries
    #############################################################################
    if(("TRUE" %in% duplicated(dada.met))==TRUE) { # check for fully double entries
      doouble <- duplicated(dada.soil)
      cat(paste(length(which(doouble=="TRUE")),"duplicated records found in file",files.02[i],"\n",
                "first entry:",dada.soil[which(doouble=="TRUE")[1],1],"\n  last entry:",dada.soil[which(doouble=="TRUE")[length(which(doouble=="TRUE"))],1],"\n\n"))
      dada.soil <- unique(dada.soil)  # remove double entries

    } else if(("TRUE" %in% duplicated(dada.soil[,1]))==TRUE){  # check for multiple different data records for same! timestamp
      doouble <- duplicated(dada.soil[,1])
      cat(paste(length(which(doouble=="TRUE")),"multiple records found in file",files.02[i],"\n",
                "first entry:",dada.soil[which(doouble=="TRUE")[1],1],"\n  last entry:",dada.soil[which(doouble=="TRUE")[length(which(doouble=="TRUE"))],1],"\n\n"))
      dd<-which(dada.soil[,1] %in% dada.soil[which(doouble=="TRUE"),1])
      dada.soil <- dada.soil[-dd,]  # remove double entries

    } else { cat("\n No double data entries found in",files.02[i],"\n\n")    }
    #############################################################################
    ## step 1.11b
    ## convert date to numeric value
    #############################################################################

    dada.soil[,1]<-as.numeric(as.POSIXct(dada.soil[,1],format='%Y-%m-%d %H:%M:%S',origin=origin, tz = "UTC"))
    #############################################################################
    ## step 1.12b
    ## special case: former files with different columns
    ## set colnames
    #############################################################################
    if(length(dada.soil[1,])==9){
      colnames(dada.soil)<-c("UTC","RECORD","T107(1)","T107(2)","T107(3)","CS(1)",
                             "CS(2)","CS(3)","Snow_Depth")
      #############################################################################
      ## step 1.12b
      ## add additional columns to former dataset
      #############################################################################


      dada.soil$snowsq  <-NA
      dada.soil$distcor <-NA
      #
      dada.soil<-dada.soil[,c("TIMESTAMP","RECORD","T107(1)","T107(2)","T107(3)","CS(1)",
                              "CS(2)","CS(3)","Snow_Depth","Raw_Dist","SignalQuality",
                              "Temp_Corr_Distance"
)]
    }else{
      #############################################################################
      ## step 1.12b
      ## standard case
      ## set original colnames
      #############################################################################

      colnames(dada.soil)<-c("UTC","RECORD","T107(1)","T107(2)","T107(3)","CS(1)",
                             "CS(2)","CS(3)","Snow_Depth")
    }
    #############################################################################
    ## step 1.14b
    ## merge input data with date table
    #############################################################################

    newdf.soil <- merge(compl.soil,dada.soil,all.x=T, by="UTC")

    #############################################################################
    ## step 1.15b
    ## merge date table with storing table
    #############################################################################

    for(k in 2:(length(db.kumet.soil[1,]))){
      db.kumet.soil[,k]<-rowMeans(cbind(db.kumet.soil[,k],newdf.soil[,k+1]),na.rm=T)#
    }
  } # soil moisture

#############################################################################  soil moisture
## step 1.16 -----
## convert numeric dates back to date format
#############################################################################


  db.kumet.air[,1]  <-format( as.POSIXct(db.kumet.air[,1],  origin=origin,tz="UTC"),format='%Y-%m-%d %H:%M')
  db.kumet.soil[,1]<-format( as.POSIXct(db.kumet.soil[,1],origin=origin,tz="UTC"),format='%Y-%m-%d %H:%M')

  #############################################################################
  ## step 1.17
  ## set "sparc" colnames
  #############################################################################
  #
  colnames(db.kumet.air)<- c("UTC","batt_U","Tpan","G_wall","G_center",
                            "U_center_40","U_center_30","U_center20","U_center_10","U_center_5","U_center_1",
                            "U_rim_42","U_rim_32","U_rim_22","U_rim_16","U_rim_7","U_rim_3",
                            "U_wall_71","U_wall_61","U_wall_51","U_wall_38","U_wall_33",
                            "U_wall_27","U_wall_21","U_wall_16","U_wall_11","U_wall_6","U_wall_2",
                            "U_icew_271","U_icew_241","U_icew_211","U_icew_181","U_icew_151",
                            "U_icew_121","U_icew_91","U_icew_61","U_icew_41",

                            "Ts_center_40","Ts_center_30","Ts_center20","Ts_center_10","Ts_center_5","Ts_center_1",
                            "Ts_rim_42","Ts_rim_32","Ts_rim_22","Ts_rim_16","Ts_rim_7","Ts_rim_3",
                            "Ts_wall_71","Ts_wall_61","Ts_wall_51","Ts_wall_38","Ts_wall_33",
                            "Ts_wall_27","Ts_wall_21","Ts_wall_16","Ts_wall_11","Ts_wall_6","Ts_wall_2",
                            "Ts_icew_271","Ts_icew_241","Ts_icew_211","Ts_icew_181","Ts_icew_151",
                            "Ts_icew_121","Ts_icew_91","Ts_icew_61","Ts_icew_41",

                            "Tsurf_raw","Tsurf_cor")

  colnames(db.kumet.soil)<- c("UTC","distraw","snowsq","distcor","Dsn",

                              "E2_center_43","E2_center_33","E2_center_23","E2_center_13","E2_center_8",
                              "E2_rim_43","E2_rim_33","E2_rim_23","E2_rim_14","E2_rim_5",
                              "E2_wall_70","E2_wall_60","E2_wall_50","E2_wall_37","E2_wall_34",
                              "E2_wall_26","E2_wall_22","E2_wall_15","E2_wall_12","E2_wall_5",

                              "cond_center_43","cond_center_33","cond_center_23","cond_center_13","cond_center_8",
                              "cond_rim_43","cond_rim_33","cond_rim_23","cond_rim_14","cond_rim_5",
                              "cond_wall_70","cond_wall_60","cond_wall_50","cond_wall_37","cond_wall_34",
                              "cond_wall_26","cond_wall_22","cond_wall_15","cond_wall_12","cond_wall_5",

                              "vwc_center_43","vwc_center_33","vwc_center_23","vwc_center_13","vwc_center_8",
                              "vwc_rim_43","vwc_rim_33","vwc_rim_23","vwc_rim_14","vwc_rim_5",
                              "vwc_wall_70","vwc_wall_60","vwc_wall_50","vwc_wall_37","vwc_wall_34",
                              "vwc_wall_26","vwc_wall_22","vwc_wall_15","vwc_wall_12","vwc_wall_5")

  #############################################################################
  ## step 1.18
  ## safe data to txt-file
  #############################################################################
  #
  write.table(db.kumet.air[as.numeric(format(as.POSIXct(db.kumet.air[,1],format='%Y-%m-%d %H:%M',origin=origin, tz = "UTC"),format='%Y'))==year,-2],
              paste0(path$w[path$n=="LV0.p"],"KuMet2010/01_met/kumet2010_met_",year,".dat"),quote = F,dec=".",sep=",",row.names=F)

  write.table(db.kumet.soil[as.numeric(format(as.POSIXct(db.kumet.soil[,1],format='%Y-%m-%d %H:%M',origin=origin, tz = "UTC"),format='%Y'))==year,],
              paste0(path$w[path$n=="LV0.p"],"KuMet2010/02_soil/kumet2010_soil_",year,".dat"),quote = F,dec=".",sep=",",row.names=F)


} # end loop over years

cat("\n#\n# kumet2010 ",year," without problems!\n#\n") # main output


