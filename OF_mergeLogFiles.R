# Read all OpenFeeder logfiles from "path" and merge with subfolder name and Config data.
# save result and error in "out_f"
# consier all visit in delay < "gap" as the same visit
# return "d" all logfiles from "path" merged

# mcauchoixxx@gmail.com, 3/13/19
#--------------------------------------------------------
#to do
#1- Compute duration on the feeder.
#--------------------------------------------------------
OF_mergeLogFiles<-function(path, out_f,gap){
  

# list all data files
allLogFiles=list.files(path,all.files=FALSE,pattern="201+",recursive=TRUE)# must be from 2017
allLogFiles=allLogFiles[c(grep("*.CSV*",allLogFiles))]#,grep("*eight*",allLogFiles))]#,grep("*eight*",allLogFiles)

allConfigFiles=list.files(path,all.files=FALSE,pattern="CONFIG+",recursive=TRUE)# must be from 2017
# subselect
#allLogFiles=allLogFiles[-c(grep("*GJ*",allLogFiles))]#,grep("*eight*",allLogFiles))]#,grep("*eight*",allLogFiles)
#allConfigFiles=allConfigFiles[-c(grep("*GJ/*",allConfigFiles))]#,grep("*eight*",allLogFiles))]#,grep("*eight*",allLogFiles)

# Prepare marge: put data in a list, add fields from folder names and INI
dataList <- list()
bad_dataList <- list()
badFile=0# to store bad csv filename
badFileName=c()
badLine=0# to store bad lines
obsNb=0# nb of observations
goodCSVind=1# to store good CSV file
for (i in 1:length(allLogFiles)) {
  # read each file
  dt=read.csv(paste(path,allLogFiles[i], sep = ""),header =F)
  # check the quality of the file
  correctLinAndCol=dim(dt)[1]>0&dim(dt)[2]>12&dim(dt)[2]<17
  correctDate= nchar(as.character(dt[[1,1]]))==8 #substr(dt[[1,1]],7,9) %in% c("18","19")
  
  if (correctLinAndCol&correctDate){
    # file name and index
    print(paste(allLogFiles[i],i,'on',length(allLogFiles)))
    
    #Check if there is bad lines
    #-------------
    goodDate=substr(dt[,1],7,9) %in% c("18","19")&substr(dt[,1],3,3)=='/'&substr(dt[,1],6,6)=='/'&!is.na(as.numeric(substr(dt[,1],4,5)))&!is.na(as.numeric(substr(dt[,1],1,2)))# must be 2018 or 2019
    goodHour=nchar(as.character(dt[,2]))==8&substr(dt[,2],3,3)==':'&substr(dt[,2],6,6)==':'&!is.na(as.numeric(substr(dt[,2],1,2)))&!is.na(as.numeric(substr(dt[,2],4,5)))&!is.na(as.numeric(substr(dt[,2],7,8)))
    goodSite=nchar(as.character(dt[,3]))==2
    goodOF=nchar(as.character(dt[,4]))==4&substr(dt[,4],1,2)=='OF'
    goodSc=dt[,5]%in% c(1,2,30,31,32,33)
    goodTag=nchar(as.character(dt[,6]))==10
    gooddenied=dt[,7] %in% c(1,0)
    goodreward=dt[,8] %in% c(1,0)
    goodred=dt[,9] %in% c(0,70,100)
    goodgreen=dt[,10] %in% c(0,70,100)
    goodblue=dt[,11] %in% c(0,70,100)
    goodDoorStat=dt[,12] %in% c(0,1)
    
    # correct lines
    goodLine=goodDate&goodHour&goodSite&goodOF&goodSc&goodTag&gooddenied&
      goodreward&goodred&goodgreen&goodblue&goodDoorStat
    # name to now where the error is coming from
    prob=paste("problem: Date",sum(!goodDate),'Hour',sum(!goodHour),
         'Site',sum(!goodSite),'OFname',sum(!goodOF),'Sc',sum(!goodSc),
         'Tag',sum(!goodTag),'Denied',sum(!gooddenied),'Reward',sum(!goodreward),
         'Red',sum(!goodred),'Green',sum(!goodgreen),'Blue',sum(!goodblue),
         'DoorStat',sum(!goodDoorStat))
    # number of bad lines
    NbBadLine=dim(dt)[1]-sum(goodLine)
    
    # add col name
    #-------------
    finalName=c("day","hour","site","OF","scenario","tag","denied","reward","red","green","blue","door_status_at_arrival","door.open","LEDpattern","Video","visit_duration")
    # deal with the diffrent version of the Firmware that produce data with a different number of column
    if (dim(dt)[2]==13){# before 19/09/2018
      # add row name
      names(dt)=c("day","hour","site","OF","scenario","tag","denied","reward","red","green","blue","door_status_at_arrival","visit_duration")
      # add extra from last FM
      dt$door.open[dt$reward==1]=1
      dt$door.open[dt$reward==0]=0
      dt$LEDpattern=NA
      dt$Video=NA
      dt=dt[,finalName]# put in the correct order
    }
    else if (dim(dt)[2]==14){#before FM26
      # add row name
      names(dt)=c("day","hour","site","OF","scenario","tag","denied","reward","red","green","blue","door_status_at_arrival","door.open","visit_duration")
      # add extra from last FM
      dt$LEDpattern=NA
      dt$Video=NA
      dt=dt[,finalName]
      
    }
    else if (dim(dt)[2]==15){#FM26
      # add row name
      names(dt)=c("day","hour","site","OF","scenario","tag","denied","reward","red","green","blue","door_status_at_arrival","door.open","LEDpattern","visit_duration")
      # add extra from last FM
      dt$Video=NA
      dt=dt[,finalName]
    }
    else if (dim(dt)[2]==16){#FM27
      # add row name
      names(dt)=c("day","hour","site","OF","scenario","tag","denied","reward","red","green","blue","door_status_at_arrival","door.open","LEDpattern","Video","visit_duration")
    }
    
    # add folder variables
    #----------------------
    # get delimitaion of subfolders
    pos = unlist(gregexpr('/', allLogFiles[i]))
    # add OF
    dt$site_folder=substr(allLogFiles[i],1, pos[1]-1)
    # add task
    dt$task=substr(allLogFiles[i], pos[1]+1, pos[2]-1)
    # add file
    dt$file=basename(allLogFiles[i])
    # add path
    dt$path=allLogFiles[i]	
    
    # add config data
    #----------------
    pat=substr(allLogFiles[i], 1, max(pos))
    conf=paste0(pat,"CONFIG.INI")#allConfigFiles[c(grep(pat,allConfigFiles))]
    ini=read.ini(paste0(path,conf))
    # Percentage door open
    if (ini$scenario$num=="2")
    {
      dt$ini.perc.door.open=as.numeric(ini$door$habituation)
    }
    else if (ini$scenario$num=="1")
    {
      dt$ini.perc.door.open=100# allways open in scenario1
    }
    else
    {
      dt$ini.perc.door.open=0# allways close in other scenario
    }
    
    # remove corrupted lines
    #-----------------------
    if (NbBadLine>0){

      # to save
      #--------
      # name in a text
      badFile=badFile+1
      badFileName[badFile]=paste(allLogFiles[i],NbBadLine,'bad lines',prob)
      # data 
      badLine=badLine+1
      bad_dataList[[badLine]]=dt[!goodLine,]
      # remove bad lines
      dt=dt[goodLine,]
    }
    
    if (dim(dt)[1]>0){
    # remove bird multipe event of the same visit and code full time (and duration, TO DO)
    #----------------------------------------------------------------------
    # deal with bird recorded successively on the same visit
    #on decalle tous les RFID d'un cran
    Id2<-c("NA",as.character(dt$tag))
    Id2<-Id2[-length(Id2)] #suppression de la derni?re valeur pour garder la m?me longueur
    #on decalle tous les temps d'un cran
    dt$time<-chron::times(as.character(dt$hour))
    time2<-c(chron::times(0),dt$time)
    time2<-time2[-length(time2)] #suppression de la derni??re valeur pour garder la m?me longueur
    #suppression des donnees si RFID identique la ligne precedente ET la difference de temps est inferieure au seuil
    samevisit=(dt$tag==Id2|dt$tag=='??????????'|Id2=='??????????') & (dt$time-time2)*86400<gap
    # COMPUTE DURATION HERE
    # samevisit2=c(NA,samevisit[-length(samevisit)])
    # samevisit-samevisit2
    
    dt<-dt[!samevisit,]
    
    # 
    #--------------------------------------------
    
    
    
    # time ADD DURATION HERE
    #------------------------------
    
    # store in a list
    #----------------
    dataList[[goodCSVind]]=dt
    goodCSVind=goodCSVind+1;
    # check
    obsNb= obsNb+dim(dt)[1]
    }
    
  }
  
  
  
  
  else
    
  {
    badFile=badFile+1
    badFileName[badFile]=allLogFiles[i]
  }
  
}
# print
print(paste(badFile,'corrupted file'))

# merge
d=rbindlist(dataList)
bad_d=rbindlist(bad_dataList)
#save
#write.table(d,paste(out_f,"alldata.csv",sep=""))
write.table(bad_d,paste(out_f,"corruptedLines.csv",sep=""))



#erros
sink(paste0(out_f,"corrupted_data.txt"))# store erros
print(badFileName)
sink()

return(d)
}




