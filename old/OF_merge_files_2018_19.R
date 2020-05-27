# Read OpenFeeder logfiles and merge with subfolder name
# mcauchoixxx@gmail.com, 1/12/17
# !!!!!!!not updated after jan 2019
#--------------------------------------------------------
#to do
#1- Compute duration on the feeder.
#--------------------------------------------------------


# libraries
#install.packages('data.table',type="mac.binary.mavericks")
library('data.table')
library('ini')
library('chron') #cr?er des donn?es temporelles


# variables
path='/Users/maximecauchoix/IAST Dropbox/maxime cauchoix/wild_cog_OF/data/2018-2019/'
out='/Users/maximecauchoix/IAST Dropbox/maxime cauchoix/wild_cog_OF/results/2018-2019/'

# list all data files
allLogFiles=list.files(path,all.files=FALSE,pattern="201+",recursive=TRUE)# must be from 2017
allLogFiles=allLogFiles[c(grep("*.CSV*",allLogFiles))]#,grep("*eight*",allLogFiles))]#,grep("*eight*",allLogFiles)

allConfigFiles=list.files(path,all.files=FALSE,pattern="CONFIG+",recursive=TRUE)# must be from 2017
# subselect
#allLogFiles=allLogFiles[c(grep("*GJ*",allLogFiles))]#,grep("*eight*",allLogFiles))]#,grep("*eight*",allLogFiles)
#allConfigFiles=allConfigFiles[c(grep("*GJ*",allConfigFiles))]#,grep("*eight*",allLogFiles))]#,grep("*eight*",allLogFiles)

# Prepare marge: put data in a list, add fields from folder names and INI
dataList <- list()
badFile=0
badFileName=c()
obsNb=0
for (i in 1:length(allLogFiles)) {
  # read each file
  dt=read.csv(paste(path,allLogFiles[i], sep = ""),header =F)
  
  if (dim(dt)[1]>0&dim(dt)[2]>12){
    print(allLogFiles[i])
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
    
    # remove bird multipe event of the same visit and code full time (and duration, TO DO)
    #----------------------------------------------------------------------
    # deal with bird recorded successively on the same visit
    gap=3
    #on d??calle tous les RFID d'un cran
    Id2<-c("NA",as.character(dt$tag))
    Id2<-Id2[-length(Id2)] #suppression de la derni?re valeur pour garder la m?me longueur
    
    #on d?calle tous les temps d'un cran
    
    dt$time<-times(as.character(dt$hour))
    
    time2<-c(times(0),dt$time)
    time2<-time2[-length(time2)] #suppression de la derni??re valeur pour garder la m?me longueur
    
    #suppression des donn??es si RFID identique  la ligne pr?c?dente ET la diff?rence de temps est inf?rieure au seuil
    dt<-dt[!(dt$tag==Id2 & (dt$time-time2)*86400<gap),]
    
    # time ADD ITI AND DURATION HERE
    
    # add folder variables
    #----------------------
    # get delimitaion of subfolders
    pos = unlist(gregexpr('/', allLogFiles[i]))
    # add OF
    dt$site_folder=substr(allLogFiles[i], 1, 2)
    # add task
    dt$task=substr(allLogFiles[i], pos[1]+1, pos[2]-1)
    # add file
    dt$file=basename(allLogFiles[i])
    # add path
    dt$path=allLogFiles[i]	
    

    
    # add config data
    #----------------
    pat=substr(allLogFiles[i], 1, max(pos))
    conf=allConfigFiles[c(grep(pat,allConfigFiles))]
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
    
    # store in a list
    dataList[[i]]=dt
    # check
    obsNb= obsNb+dim(dt)[1]
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

#save
write.table(d,paste(out,"alldata.csv",sep=""))



#erros
sink(paste(out,"corrupted_data.txt"))# store erros
print(badFileName)
sink()







#INTERESTING
# merge two data frames by ID
#total <- merge(data frameA,data frameB,by="ID")
# merge two data frames by ID and Country
#total <- merge(data frameA,data frameB,by=c("ID","Country"))
