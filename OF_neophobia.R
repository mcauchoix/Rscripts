# look at repeatability of neophobia and population difference in neophobia
#  ---------------------------------------------
library("suncalc")
#install.packages("StreamMetabolism")
library("StreamMetabolism")
#
# VD
#---
# daily visit rate: compare visit rate during S1 et S2-75% and S2-0% et S2_red ou S2_gre
# hour visit rate: time to reteave normal visit rate
# delay: compare time between change and next visit and average ITI
# - time to return to the feeder
# - time to return to the feeder/mean ITI
# to do
#------
# for latency code if bird present that day before the neophobia task
# for latency code mean ITI
# use Nory's tag instead of hour in the hour in timeline (don't remove it from data)


# Analysis
#---------
# repeatability of VD in the 4 Neophobia task, in motor only (2), visual only (2, 4? with learning)
# species and elevation difference

# Load experimental timeline
#----------------
# IN
#experimental change# SAVE IN CSV
ec=read.csv2('/Users/maximecauchoix/Documents/wild_cog_OF/logistique/OF timeline.csv')
ec=ec[,1:7]
ec$dayDate=as.Date(ec$Day,"%d/%m/%y")# convert date
#ec$Hour.end=substr(ec$Hour.end,12,19)

#---------------------------------
# Preprocess data
#---------------------------------

# subsample keep only openbar and habituation scenario
#----------
data=d[d$scenario %in% c(1,2),]
##
## Compute time from sunRise and sunset 
#------------------------------
# morning sunset time
data$dayDate=as.Date(data$day,"%d/%m/%y")
data$dayChar=as.character(data$dayDate)
sun=sunrise.set(42.986462, 1.144040, min(data$dayDate), timezone = "UTC+1", num.days = 150)
sun$dayChar=substr(sun$sunrise,1,10)
sun$hourSunrise=format(sun$sunrise,'%H:%M:%S')
sun$hourSunset=format(sun$sunset,'%H:%M:%S')
# merge by day
dataSun <- merge(data,sun,by="dayChar", all.x=F,sort=F)#all.x=F (keep only bird with banding data)
# time according to sunrise
dataSun$timeRise=(chron::times(as.character(dataSun$hour))-dataSun$hourSunrise)*86400
# time according to sunset
dataSun$timeSet=(dataSun$hourSunset-chron::times(as.character(dataSun$hour)))*86400
# save
write.csv2(dataSun,paste0(out_n,"Data_scenario1-2withTimeFromSunrise.csv"))


# neophobia variable
#-------------------
d$neo[d$ini.perc.door.open==75]=1
d$neo[d$ini.perc.door.open==100]=2
d$neo[d$green==100]=3
d$neo[d$red==100]=4

# Get neo event onset
#--------------------
# loop over site
uSite=c("C1","C4","M1","BA")#unique(d$site_folder)#remove gajan for now

site=c();neo1=c();neo2=c();neo3=c();neo4=c();
#time of change by site
datlist=list()
for (s in 1:length(uSite))
{
  site=uSite[s]
  #  neo1 door 75
  ind=which(ec$change=="S2 75%"&ec$Location==uSite[s])
  neo1=strptime(paste(ec$day[ind],ec$Hour.end[ind]),"%Y-%m-%d %H:%M:%S")
  sun=sunrise.set(42.986462, 1.144040, ec$day[ind], timezone = "UTC+1", num.days = 1)
  neo1SunRise=(chron::times(as.character(ec$Hour.end[ind]))-format(sun$sunrise,'%H:%M:%S'))*86400
  # arrival expe
  neo1_arr=strptime(paste(ec$day[ind],ec$Hour.start[ind]),"%Y-%m-%d %H:%M:%S")
  sun=sunrise.set(42.986462, 1.144040, ec$day[ind], timezone = "UTC+1", num.days = 1)
  neo1SunRise_arr=(chron::times(as.character(ec$Hour.start[ind]))-format(sun$sunrise,'%H:%M:%S'))*86400
  
  #  neo2 100%
  ind=which(ec$change=="S2 0%"&ec$Location==uSite[s])
  neo2=strptime(paste(ec$day[ind],ec$Hour.end[ind]),"%Y-%m-%d %H:%M:%S")
  sun=sunrise.set(42.986462, 1.144040, ec$day[ind], timezone = "UTC+1", num.days = 1)
  neo2SunRise=(chron::times(as.character(ec$Hour.end[ind]))-format(sun$sunrise,'%H:%M:%S'))*86400
  # arrival expe
  neo2_arr=strptime(paste(ec$day[ind],ec$Hour.start[ind]),"%Y-%m-%d %H:%M:%S")
  sun=sunrise.set(42.986462, 1.144040, ec$day[ind], timezone = "UTC+1", num.days = 1)
  neo2SunRise_arr=(chron::times(as.character(ec$Hour.start[ind]))-format(sun$sunrise,'%H:%M:%S'))*86400
  
  
  
  #  neo3 green
  ind=which(ec$change=="S2 green"&ec$Location==uSite[s])
  neo3=strptime(paste(ec$day[ind],ec$Hour.end[ind]),"%Y-%m-%d %H:%M:%S")
  sun=sunrise.set(42.986462, 1.144040, ec$day[ind], timezone = "UTC+1", num.days = 1)
  neo3SunRise=(chron::times(as.character(ec$Hour.end[ind]))-format(sun$sunrise,'%H:%M:%S'))*86400
  # arrival expe
  neo3_arr=strptime(paste(ec$day[ind],ec$Hour.start[ind]),"%Y-%m-%d %H:%M:%S")
  sun=sunrise.set(42.986462, 1.144040, ec$day[ind], timezone = "UTC+1", num.days = 1)
  neo3SunRise_arr=(chron::times(as.character(ec$Hour.start[ind]))-format(sun$sunrise,'%H:%M:%S'))*86400
  
  
   #  neo4 red
  ind=which(ec$change=="S2 red"&ec$Location==uSite[s])
  neo4=strptime(paste(ec$day[ind],ec$Hour.end[ind]),"%Y-%m-%d %H:%M:%S")
  sun=sunrise.set(42.986462, 1.144040, ec$day[ind], timezone = "UTC+1", num.days = 1)
  neo4SunRise=(chron::times(as.character(ec$Hour.end[ind]))-format(sun$sunrise,'%H:%M:%S'))*86400
  # arrival expe
  neo4_arr=strptime(paste(ec$day[ind],ec$Hour.start[ind]),"%Y-%m-%d %H:%M:%S")
  sun=sunrise.set(42.986462, 1.144040, ec$day[ind], timezone = "UTC+1", num.days = 1)
  neo4SunRise_arr=(chron::times(as.character(ec$Hour.start[ind]))-format(sun$sunrise,'%H:%M:%S'))*86400
  # End neo4
  ind=which(ec$change=="S3 all 25% 5s"&ec$Location==uSite[s])
  End4=strptime(paste(ec$day[ind],ec$Hour.start[ind]),"%Y-%m-%d %H:%M:%S")
  
  # store
  dat=data.frame(site=site,neo1=neo1,neo2=neo2,neo3=neo3,neo4=neo4,neo1SunRise=neo1SunRise,neo2SunRise=neo2SunRise,neo3SunRise=neo3SunRise,neo4SunRise=neo4SunRise,
                 neo1_arr=neo1_arr,neo2_arr=neo2_arr,neo3_arr=neo3_arr,neo4_arr=neo4_arr,
                 neo1SunRise_arr=neo1SunRise_arr,neo2SunRise_arr=neo2SunRise_arr,neo3SunRise_arr=neo3SunRise_arr,neo4SunRise_arr=neo4SunRise_arr,End4=End4)
  datlist[[s]]=dat
}
#merge
neo=do.call(rbind,datlist)

# duration of each step
neo$motor1_duration=neo$neo2_arr-neo$neo1
neo$motor2_duration=neo$neo3_arr-neo$neo2
neo$visual1_duration=neo$neo4_arr-neo$neo3
neo$visual2_duration=neo$End4-neo$neo4


# verify
# neo$neo1-neo$neo1_arr
# neo$neo2-neo$neo2_arr
# neo$neo3-neo$neo3_arr
# neo$neo4-neo$neo4_arr

# save
write.csv2(neo,paste0(out_n,"TimelineNeophoia.csv"))


#---------------------------------
# Neophoia at the population level
#---------------------------------

for(s in 1:length(uSite)){
  pdf(paste0(out_n,uSite[s],"_neophobia population.pdf"), height=10, width=7)
  
  par(mfrow=c(3,1),mar=c(5, 8, 4, 2)/2)
  #neo1
  dayOfChange=as.Date(substr(neo$neo1[neo$site==uSite[s]],1,10),'%Y-%m-%d')
  x=dataSun$timeRise[dataSun$dayDate==dayOfChange & dataSun$site_folder==uSite[s]]
  hist(x,breaks = seq(from = min(x)-1, to = max(x)+500, by = 5*60))
  abline(v=neo$neo1SunRise[neo$site==uSite[s]],col='red')
  abline(v=neo$neo1SunRise_arr[neo$site==uSite[s]],col='green')
  
  title("Door Movement")
  #neo2
  dayOfChange=as.Date(substr(neo$neo2[neo$site==uSite[s]],1,10),'%Y-%m-%d')
  hist(dataSun$timeRise[dataSun$dayDate==dayOfChange & dataSun$site_folder==uSite[s]]/3600,breaks = 100)
  abline(v=neo$neo2SunRise[neo$site==uSite[s]]/3600,col='red')
  title("Green Led")
  #neo3
  dayOfChange=as.Date(substr(neo$neo3[neo$site==uSite[s]],1,10),'%Y-%m-%d')
  hist(dataSun$timeRise[dataSun$dayDate==dayOfChange & dataSun$site_folder==uSite[s]]/3600,breaks = 100)
  abline(v=neo$neo3SunRise[neo$site==uSite[s]]/3600,col='red')
  title("Red Led")
  s
  dev.off()
}

# Visit rate pop
#------------------
win=seq(from = 5, to = 30, by = 5)
# compute before after
dataList=list()
n=1
neoph=c("neo1","neo2","neo3","neo4")
for(i in 1:length(neoph)){
  for (w in 1:length(win)){
    for(s in 1:length(uSite)){
      # door mov
      #--------
      day_ind=data$site_folder==uSite[s]&data$dayDate==as.Date(substr(neo[[neoph[i]]][neo$site==uSite[s]],1,10),'%Y-%m-%d')
      # day vis
      day_vis=as.POSIXct(data$fullTime[day_ind],tz="",'%d/%m/%y %H:%M:%OS')
      # nb visit before
      before=sum(day_vis>as.POSIXct((neo[[paste0(neoph[i],'_arr')]][neo$site==uSite[s]]-(win[w]+30)*60),tz="",'%d/%m/%y %H:%M:%OS')
                 &day_vis<as.POSIXct(neo[[paste0(neoph[i],'_arr')]][neo$site==uSite[s]]-30*60,tz="",'%d/%m/%y %H:%M:%OS'))
      # nb visit after
      after=sum(day_vis<as.POSIXct((neo[[neoph[i]]][neo$site==uSite[s]]+win[w]*60),tz="",'%d/%m/%y %H:%M:%OS')
                &day_vis>as.POSIXct(neo[[neoph[i]]][neo$site==uSite[s]],tz="",'%d/%m/%y %H:%M:%OS'))
      
      dataList[[n]]=data.frame(site=uSite[s],neo=neoph[i],wind=win[w],
                               time="before",nbVisit=before)
      dataList[[n+1]]=data.frame(site=uSite[s],neo=neoph[i],wind=win[w],
                                 time="after",nbVisit=after)
      
      n=n+2
    }
  }
}
neoPop=rbindlist(dataList)
# plot
ind=neoPop$win==10#neoPop$neo=="neo1"&
boxplot(neoPop$nbVisit[ind]~neoPop$time[ind]*neoPop$site[ind])


#---------------------------------
# Neophobia at the individual level
#----------------------------------

# Visit rate ind
#------------------
win=seq(from = 10, to = 240, by = 20)
# compute before after
dataList=list()
n=1
for (w in 1:length(win)){#
  lag=win[w]*60
  print(paste(w,'/',length(win)))
  for(s in 1:length(uSite)){
    # neo1
    #--------$
    print("neo1")
    day_neo=data$site_folder==uSite[s]&data$dayDate==as.Date(substr(neo$neo1[neo$site==uSite[s]],1,10),'%Y-%m-%d')
    dayOfChange=as.Date(substr(neo$neo1[neo$site==uSite[s]],1,10),'%Y-%m-%d')
    timeOfChange=neo$neo1SunRise[neo$site==uSite[s]]
    # ind present
    uIDpres=unique(data$tag[day_neo])
    for (i in 1:length(uIDpres)){
      # day vis
      day_vis=as.POSIXct(data$fullTime[day_neo&data$tag==uIDpres[i]],tz="",'%d/%m/%y %H:%M:%OS')
      # nb visit before
      before=sum(day_vis>as.POSIXct((neo$neo1_arr[neo$site==uSite[s]]-lag),tz="",'%d/%m/%y %H:%M:%OS')
                 &day_vis<as.POSIXct(neo$neo1_arr[neo$site==uSite[s]],tz="",'%d/%m/%y %H:%M:%OS'))
      # nb visit after
      after=sum(day_vis<as.POSIXct((neo$neo1[neo$site==uSite[s]]+lag),tz="",'%d/%m/%y %H:%M:%OS')
                &day_vis>as.POSIXct(neo$neo1[neo$site==uSite[s]],tz="",'%d/%m/%y %H:%M:%OS'))
      # baseline same period
      post1=sum(dataSun$tag==uIDpres[i]&dataSun$dayDate==dayOfChange&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag)
      base1=mean(c(sum(dataSun$tag==uIDpres[i]&dataSun$dayDate==dayOfChange-1&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag),
                   sum(dataSun$tag==uIDpres[i]&dataSun$dayDate==dayOfChange-2&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag)))
      # average nb visit on this timewindow
      indDayOfChangeUnique=which(dataSun$dayDate==dayOfChange-1)[1]
      baseAll=as.numeric(mean(c(sum(dataSun$tag==uIDpres[i]&dataSun$dayDate==dayOfChange-1)/((chron::times(dataSun$hourSunset[indDayOfChangeUnique])-chron::times(dataSun$hourSunrise[indDayOfChangeUnique]))*86400)*lag,
                                sum(dataSun$tag==uIDpres[i]&dataSun$dayDate==dayOfChange-2)/((chron::times(dataSun$hourSunset[indDayOfChangeUnique])-chron::times(dataSun$hourSunrise[indDayOfChangeUnique]))*86400)*lag)))
      # store
      dataList[[n]]=data.frame(site=uSite[s],neo="neo1",wind=win[w],tag=uIDpres[i],
                               time="before",nbVisit=before,totalVisitChangeDay=length(day_vis))
      dataList[[n+1]]=data.frame(site=uSite[s],neo="neo1",wind=win[w],tag=uIDpres[i],
                                 time="after",nbVisit=after,totalVisitChangeDay=length(day_vis))
      dataList[[n+2]]=data.frame(site=uSite[s],neo="neo1",wind=win[w],tag=uIDpres[i],
                                 time="baseSameTW",nbVisit=base1,totalVisitChangeDay=length(day_vis))
      dataList[[n+3]]=data.frame(site=uSite[s],neo="neo1",wind=win[w],tag=uIDpres[i],
                                 time="afterSunRise",nbVisit=post1,totalVisitChangeDay=length(day_vis))
      dataList[[n+4]]=data.frame(site=uSite[s],neo="neo1",wind=win[w],tag=uIDpres[i],
                                 time="baseAll",nbVisit=baseAll,totalVisitChangeDay=length(day_vis))
      n=n+5
    }
    # neo2
    #--------
    print("neo2")
    day_neo=data$site_folder==uSite[s]&data$dayDate==as.Date(substr(neo$neo2[neo$site==uSite[s]],1,10),'%Y-%m-%d')
    dayOfChange=as.Date(substr(neo$neo2[neo$site==uSite[s]],1,10),'%Y-%m-%d')
    timeOfChange=neo$neo2SunRise[neo$site==uSite[s]]
    # ind present
    uIDpres=unique(data$tag[day_neo])
    for (i in 1:length(uIDpres)){
      # day vis
      day_vis=as.POSIXct(data$fullTime[day_neo&data$tag==uIDpres[i]],tz="",'%d/%m/%y %H:%M:%OS')
      # nb visit before
      before=sum(day_vis>as.POSIXct((neo$neo2_arr[neo$site==uSite[s]]-lag),tz="",'%d/%m/%y %H:%M:%OS')
                 &day_vis<as.POSIXct(neo$neo2_arr[neo$site==uSite[s]],tz="",'%d/%m/%y %H:%M:%OS'))
      # nb visit after
      after=sum(day_vis<as.POSIXct((neo$neo2[neo$site==uSite[s]]+lag),tz="",'%d/%m/%y %H:%M:%OS')
                &day_vis>as.POSIXct(neo$neo2[neo$site==uSite[s]],tz="",'%d/%m/%y %H:%M:%OS'))
      # baseline same period
      post1=sum(dataSun$tag==uIDpres[i]&dataSun$dayDate==dayOfChange&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag)
      base1=mean(c(sum(dataSun$tag==uIDpres[i]&dataSun$dayDate==dayOfChange-1&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag),
                   sum(dataSun$tag==uIDpres[i]&dataSun$dayDate==dayOfChange-2&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag)))
      # average nb visit on this timewindow
      indDayOfChangeUnique=which(dataSun$dayDate==dayOfChange-1)[1]
      baseAll=as.numeric(mean(c(sum(dataSun$tag==uIDpres[i]&dataSun$dayDate==dayOfChange-1)/((chron::times(dataSun$hourSunset[indDayOfChangeUnique])-chron::times(dataSun$hourSunrise[indDayOfChangeUnique]))*86400)*lag,
                                sum(dataSun$tag==uIDpres[i]&dataSun$dayDate==dayOfChange-2)/((chron::times(dataSun$hourSunset[indDayOfChangeUnique])-chron::times(dataSun$hourSunrise[indDayOfChangeUnique]))*86400)*lag)))
      # store
      dataList[[n]]=data.frame(site=uSite[s],neo="neo2",wind=win[w],tag=uIDpres[i],
                                 time="before",nbVisit=before,totalVisitChangeDay=length(day_vis))
      dataList[[n+1]]=data.frame(site=uSite[s],neo="neo2",wind=win[w],tag=uIDpres[i],
                                 time="after",nbVisit=after,totalVisitChangeDay=length(day_vis))
      dataList[[n+2]]=data.frame(site=uSite[s],neo="neo2",wind=win[w],tag=uIDpres[i],
                                 time="baseSameTW",nbVisit=base1,totalVisitChangeDay=length(day_vis))
      dataList[[n+3]]=data.frame(site=uSite[s],neo="neo2",wind=win[w],tag=uIDpres[i],
                                 time="afterSunRise",nbVisit=post1,totalVisitChangeDay=length(day_vis))
      dataList[[n+4]]=data.frame(site=uSite[s],neo="neo2",wind=win[w],tag=uIDpres[i],
                                 time="baseAll",nbVisit=baseAll,totalVisitChangeDay=length(day_vis))
      n=n+5
    }
    # neo3
    #--------
    print("neo3")
    day_neo=data$site_folder==uSite[s]&data$dayDate==as.Date(substr(neo$neo3[neo$site==uSite[s]],1,10),'%Y-%m-%d')
    dayOfChange=as.Date(substr(neo$neo3[neo$site==uSite[s]],1,10),'%Y-%m-%d')
    timeOfChange=neo$neo3SunRise[neo$site==uSite[s]]
    # ind present
    uIDpres=unique(data$tag[day_neo])
    for (i in 1:length(uIDpres)){
      # day vis
      day_vis=as.POSIXct(data$fullTime[day_neo&data$tag==uIDpres[i]],tz="",'%d/%m/%y %H:%M:%OS')
      # nb visit before
      before=sum(day_vis>as.POSIXct((neo$neo3_arr[neo$site==uSite[s]]-lag),tz="",'%d/%m/%y %H:%M:%OS')
                 &day_vis<as.POSIXct(neo$neo3_arr[neo$site==uSite[s]],tz="",'%d/%m/%y %H:%M:%OS'))
      # nb visit after
      after=sum(day_vis<as.POSIXct((neo$neo3[neo$site==uSite[s]]+lag),tz="",'%d/%m/%y %H:%M:%OS')
                &day_vis>as.POSIXct(neo$neo3[neo$site==uSite[s]],tz="",'%d/%m/%y %H:%M:%OS'))
      # baseline same period
      post1=sum(dataSun$tag==uIDpres[i]&dataSun$dayDate==dayOfChange&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag)
      base1=mean(c(sum(dataSun$tag==uIDpres[i]&dataSun$dayDate==dayOfChange-1&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag),
                   sum(dataSun$tag==uIDpres[i]&dataSun$dayDate==dayOfChange-2&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag)))
      # average nb visit on this timewindow
      indDayOfChangeUnique=which(dataSun$dayDate==dayOfChange-1)[1]
      baseAll=as.numeric(mean(c(sum(dataSun$tag==uIDpres[i]&dataSun$dayDate==dayOfChange-1)/((chron::times(dataSun$hourSunset[indDayOfChangeUnique])-chron::times(dataSun$hourSunrise[indDayOfChangeUnique]))*86400)*lag,
                                sum(dataSun$tag==uIDpres[i]&dataSun$dayDate==dayOfChange-2)/((chron::times(dataSun$hourSunset[indDayOfChangeUnique])-chron::times(dataSun$hourSunrise[indDayOfChangeUnique]))*86400)*lag)))
      # store
      dataList[[n]]=data.frame(site=uSite[s],neo="neo3",wind=win[w],tag=uIDpres[i],
                                 time="before",nbVisit=before,totalVisitChangeDay=length(day_vis))
      dataList[[n+1]]=data.frame(site=uSite[s],neo="neo3",wind=win[w],tag=uIDpres[i],
                                 time="after",nbVisit=after,totalVisitChangeDay=length(day_vis))
      dataList[[n+2]]=data.frame(site=uSite[s],neo="neo3",wind=win[w],tag=uIDpres[i],
                                 time="baseSameTW",nbVisit=base1,totalVisitChangeDay=length(day_vis))
      dataList[[n+3]]=data.frame(site=uSite[s],neo="neo3",wind=win[w],tag=uIDpres[i],
                                 time="afterSunRise",nbVisit=post1,totalVisitChangeDay=length(day_vis))
      dataList[[n+4]]=data.frame(site=uSite[s],neo="neo3",wind=win[w],tag=uIDpres[i],
                                 time="baseAll",nbVisit=baseAll,totalVisitChangeDay=length(day_vis))
      
      n=n+5
    }
    # neo4
    #--------
    print("neo4")
    day_neo=data$site_folder==uSite[s]&data$dayDate==as.Date(substr(neo$neo4[neo$site==uSite[s]],1,10),'%Y-%m-%d')
    dayOfChange=as.Date(substr(neo$neo4[neo$site==uSite[s]],1,10),'%Y-%m-%d')
    timeOfChange=neo$neo4SunRise[neo$site==uSite[s]]
    # ind present
    uIDpres=unique(data$tag[day_neo])
    for (i in 1:length(uIDpres)){
      # day vis
      day_vis=as.POSIXct(data$fullTime[day_neo&data$tag==uIDpres[i]],tz="",'%d/%m/%y %H:%M:%OS')
      # nb visit before
      before=sum(day_vis>as.POSIXct((neo$neo4_arr[neo$site==uSite[s]]-lag),tz="",'%d/%m/%y %H:%M:%OS')
                 &day_vis<as.POSIXct(neo$neo4_arr[neo$site==uSite[s]],tz="",'%d/%m/%y %H:%M:%OS'))
      # nb visit after
      after=sum(day_vis<as.POSIXct((neo$neo4[neo$site==uSite[s]]+lag),tz="",'%d/%m/%y %H:%M:%OS')
                &day_vis>as.POSIXct(neo$neo4[neo$site==uSite[s]],tz="",'%d/%m/%y %H:%M:%OS'))
      # baseline same period
      post1=sum(dataSun$tag==uIDpres[i]&dataSun$dayDate==dayOfChange&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag)
      base1=mean(c(sum(dataSun$tag==uIDpres[i]&dataSun$dayDate==dayOfChange-1&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag),
                   sum(dataSun$tag==uIDpres[i]&dataSun$dayDate==dayOfChange-2&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag)))
      # average nb visit on this timewindow
      indDayOfChangeUnique=which(dataSun$dayDate==dayOfChange-1)[1]
      baseAll=as.numeric(mean(c(sum(dataSun$tag==uIDpres[i]&dataSun$dayDate==dayOfChange-1)/((chron::times(dataSun$hourSunset[indDayOfChangeUnique])-chron::times(dataSun$hourSunrise[indDayOfChangeUnique]))*86400)*lag,
                                sum(dataSun$tag==uIDpres[i]&dataSun$dayDate==dayOfChange-2)/((chron::times(dataSun$hourSunset[indDayOfChangeUnique])-chron::times(dataSun$hourSunrise[indDayOfChangeUnique]))*86400)*lag)))
      # store
      dataList[[n]]=data.frame(site=uSite[s],neo="neo4",wind=win[w],tag=uIDpres[i],
                               time="before",nbVisit=before,totalVisitChangeDay=length(day_vis))
      dataList[[n+1]]=data.frame(site=uSite[s],neo="neo4",wind=win[w],tag=uIDpres[i],
                                 time="after",nbVisit=after,totalVisitChangeDay=length(day_vis))
      dataList[[n+2]]=data.frame(site=uSite[s],neo="neo4",wind=win[w],tag=uIDpres[i],
                                 time="baseSameTW",nbVisit=base1,totalVisitChangeDay=length(day_vis))
      dataList[[n+3]]=data.frame(site=uSite[s],neo="neo4",wind=win[w],tag=uIDpres[i],
                                 time="afterSunRise",nbVisit=post1,totalVisitChangeDay=length(day_vis))
      dataList[[n+4]]=data.frame(site=uSite[s],neo="neo4",wind=win[w],tag=uIDpres[i],
                                 time="baseAll",nbVisit=baseAll,totalVisitChangeDay=length(day_vis))
      
      n=n+5
    }
    
  }
}

# merge
neoInd=rbindlist(dataList)
# elevation
neoInd$elev="Low"
neoInd$elev[neoInd$site %in% c('BA','C4')]="High"
# add individual values

# save
write.csv2(neoInd,paste0(out_n,"Neophobia_visit.csv"))


# plot
ind=neoInd$wind==30
boxplot(sqrt(neoInd$nbVisit[ind]+1)~neoInd$time[ind])
hist(neoInd$nbVisit[neoInd$time=="after"&ind]-neoInd$nbVisit[neoInd$time=="before"&ind])

# estim plot
ind=neoInd$neo=="neo2"&neoInd$wind==10
estim<- dabest(neoInd[ind,],time,nbVisit,idx=c("before","after"),
               paired = TRUE,id.column = tag)
plot(estim)

sink(paste0(out_n,"SI_est.txt"))# store erros
print(estim)
sink()

m1=glmer(nbVisit~time*elev+neo+(1|tag),family=poisson, data=neoInd,subset = neoInd$wind==10&neoInd$time %in% c("before","after"))
summary(m1)

m1=glmer(nbVisit~time+site+wind+(1|tag),family=poisson, data=neoInd)




# Repeatability visit rate
#---------------------------
#[1] before       after        baseSameTW    baseAll 
nboot=10;npermut=10

resW=list()
n=1
for (w in 1:length(win)){
  neoIndivClean=neoInd[neoInd$wind==win[w]&neoInd$time=='after',]
  rep=rpt(nbVisit~site+neo+(1|tag), grname = c("tag"), data = neoIndivClean, datatype = "Gaussian",nboot = nboot, npermut = npermut) 
  resWind=data.frame(base="after",wind=win[w],R=rep$R[[1]],p=rep$P[[1]],lowCI=rep$CI_emp$`2.5%`,highCI=rep$CI_emp$`97.5%`)
  mean(neoIndivClean$nbVisit)
  
   resW[[n]]=resWind
  # try cbind(neoInd$nbVisit[neoInd$wind==wind[1]&neoInd$time=='after'],neoInd$nbVisit[neoInd$wind==wind[12]&neoInd$time=='after'])
  # before
  neoIndivClean$diff=neoIndivClean$nbVisit-(neoInd$nbVisit[neoInd$wind==win[w]&neoInd$time=='before'])
  neoIndivClean$base=neoInd$nbVisit[neoInd$wind==win[w]&neoInd$time=='before']
  rep=rpt(diff~site+neo+(1|tag), grname = c("tag"), data = neoIndivClean, datatype = "Gaussian",nboot = nboot, npermut = npermut) 
  resWind=data.frame(base="after-before",wind=win[w],R=rep$R[[1]],p=rep$P[[1]],lowCI=rep$CI_emp$`2.5%`,highCI=rep$CI_emp$`97.5%`)
  resW[[n+1]]=resWind
  rep=rpt(base~site+neo+(1|tag), grname = c("tag"), data = neoIndivClean, datatype = "Gaussian",nboot = nboot, npermut = npermut) 
  resWind=data.frame(base="before",wind=win[w],R=rep$R[[1]],p=rep$P[[1]],lowCI=rep$CI_emp$`2.5%`,highCI=rep$CI_emp$`97.5%`)
  resW[[n+2]]=resWind
  
  # baseSameTW
  neoIndivClean$base=neoInd$nbVisit[neoInd$wind==win[w]&neoInd$time=='baseSameTW']
  neoIndivClean$diff=neoIndivClean$nbVisit-(neoInd$nbVisit[neoInd$wind==win[w]&neoInd$time=='baseSameTW'])
  rep=rpt(diff~site+neo+(1|tag), grname = c("tag"), data = neoIndivClean, datatype = "Gaussian",nboot = nboot, npermut = npermut) 
  resWind=data.frame(base="after-baseSameTW",wind=win[w],R=rep$R[[1]],p=rep$P[[1]],lowCI=rep$CI_emp$`2.5%`,highCI=rep$CI_emp$`97.5%`)
  resW[[n+3]]=resWind
  rep=rpt(base~site+neo+(1|tag), grname = c("tag"), data = neoIndivClean, datatype = "Gaussian",nboot = nboot, npermut = npermut) 
  resWind=data.frame(base="baseSameTW",wind=win[w],R=rep$R[[1]],p=rep$P[[1]],lowCI=rep$CI_emp$`2.5%`,highCI=rep$CI_emp$`97.5%`)
  resW[[n+4]]=resWind
  
  
  # baseAll
  neoIndivClean$base=neoInd$nbVisit[neoInd$wind==win[w]&neoInd$time=='baseAll']
  neoIndivClean$diff=neoIndivClean$nbVisit-(neoInd$nbVisit[neoInd$wind==win[w]&neoInd$time=='baseAll'])
  rep=rpt(diff~site+neo+(1|tag), grname = c("tag"), data = neoIndivClean, datatype = "Gaussian",nboot = nboot, npermut = npermut) 
  resWind=data.frame(base="after-baseAll",wind=win[w],R=rep$R[[1]],p=rep$P[[1]],lowCI=rep$CI_emp$`2.5%`,highCI=rep$CI_emp$`97.5%`)
  resW[[n+5]]=resWind
  rep=rpt(base~site+neo+(1|tag), grname = c("tag"), data = neoIndivClean, datatype = "Gaussian",nboot = nboot, npermut = npermut) 
  resWind=data.frame(base="baseAll",wind=win[w],R=rep$R[[1]],p=rep$P[[1]],lowCI=rep$CI_emp$`2.5%`,highCI=rep$CI_emp$`97.5%`)
  resW[[n+6]]=resWind
  neoIndivClean$diff=neoIndivClean$nbVisit/(neoInd$nbVisit[neoInd$wind==win[w]&neoInd$time=='before']+1)
  rep=rpt(diff~site+neo+(1|tag), grname = c("tag"), data = neoIndivClean, datatype = "Gaussian",nboot = nboot, npermut = npermut) 
  resWind=data.frame(base="after/before",wind=win[w],R=rep$R[[1]],p=rep$P[[1]],lowCI=rep$CI_emp$`2.5%`,highCI=rep$CI_emp$`97.5%`)
  resW[[n+7]]=resWind
  neoIndivClean$diff=neoIndivClean$nbVisit/(neoInd$nbVisit[neoInd$wind==win[w]&neoInd$time=='baseSameTW']+1)
  rep=rpt(diff~site+neo+(1|tag), grname = c("tag"), data = neoIndivClean, datatype = "Gaussian",nboot = nboot, npermut = npermut) 
  resWind=data.frame(base="after/baseSameTW",wind=win[w],R=rep$R[[1]],p=rep$P[[1]],lowCI=rep$CI_emp$`2.5%`,highCI=rep$CI_emp$`97.5%`)
  resW[[n+8]]=resWind
  neoIndivClean$diff=neoIndivClean$nbVisit/(neoInd$nbVisit[neoInd$wind==win[w]&neoInd$time=='baseAll']+1)
  rep=rpt(diff~site+neo+(1|tag), grname = c("tag"), data = neoIndivClean, datatype = "Gaussian",nboot = nboot, npermut = npermut) 
  resWind=data.frame(base="after/baseAll",wind=win[w],R=rep$R[[1]],p=rep$P[[1]],lowCI=rep$CI_emp$`2.5%`,highCI=rep$CI_emp$`97.5%`)
  resW[[n+9]]=resWind
  
  print(win[w])
  n=n+10
}
# merge
resWind=rbindlist(resW)
# save
write.csv2(resWind,paste0(out_n,"Neophobia_visit_repeatability.csv"))

# plot R on all time window
pdf(paste0(out_n,"Repeatability of number of visit depending on the time window.pdf"))
ggplot(resWind,aes(wind,R,colour=base))+geom_line()
dev.off()



ggplot(resWind, aes(wind, R, colour=base, fill=base)) +
  geom_line() +
  geom_ribbon(aes(x=wind, y=R, ymax=lowCI, ymin=highCI), 
              alpha=0.2)
# #store data
# res=data.frame('All')
# names(res)="species"
# res$R=round(rep$R[[1]],2)
# res$lowCI=round(rep$CI_emp[[1]],2)
# res$highCI=round(rep$CI_emp[[2]],2) 








# #-------------
# # Compute ITI
# #-------------
# uTag=unique(data$tag)
# for (i in 1:length(uTag)){
#   ind=data$tag==uTag[i]
#   uDay=unique(data$dayDate[ind])
#   
#   for (d in 1:length(uDay)){
#     indD=ind&data$dayDate==uDay[d]
#     time1<-chron::times(as.character(data$hour[indD]))
#     time2<-c(chron::times(0),time1)
#   }
# }
# 
# # remove bird multipe event of the same visit and code full time (and duration, TO DO)
# #----------------------------------------------------------------------
# # deal with bird recorded successively on the same visit
# #on decalle tous les RFID d'un cran
# Id2<-c("NA",as.character(dt$tag))
# Id2<-Id2[-length(Id2)] #suppression de la derni?re valeur pour garder la m?me longueur
# #on decalle tous les temps d'un cran
# dt$time<-chron::times(as.character(dt$hour))
# time2<-c(chron::times(0),dt$time)
# time2<-time2[-length(time2)] #suppression de la derni??re valeur pour garder la m?me longueur
# #suppression des donnees si RFID identique la ligne precedente ET la difference de temps est inferieure au seuil
# samevisit=(dt$tag==Id2|dt$tag=='??????????'|Id2=='??????????') & (dt$time-time2)*86400<gap
# # COMPUTE DURATION HERE
# # samevisit2=c(NA,samevisit[-length(samevisit)])
# # samevisit-samevisit2
# 
# dt<-dt[!samevisit,]





# 
# ## Rep by species
# uSpe=unique(d$species)
# for (s in 1:length(uSpe)){
#   rep=rpt(neo~site+(1|tag), grname = c("tag"), data = neoIndivClean[neoIndivClean$species==uSpe[s],], datatype = "Gaussian",nboot = nboot, npermut = npermut) 
#   temp=data.frame(uSpe[s],round(rep$R[[1]],2),round(rep$CI_emp[[1]],2),round(rep$CI_emp[[2]],2))
#   names(temp)=c("species","R","lowCI","highCI")
#   res = rbind(res,temp)
# }
# # Export
# pdf(paste0(out_n,j,"_Neophobia_repeat.pdf"), height=3, width=3)
# grid.table(res,rows =c())
# dev.off()
# 
# 
# 
# for (j in c(120,300,600,900)){
#   lag=j
#   #loop over site
#   n=1
#   datlist=list()
#   for(s in 1:length(uSite)){
#     uTag=unique(data$tag[data$site_folder==uSite[s]])
#     #loop over individuals
#     for (i in 1:length(uTag)){
#       #neo1
#       dayOfChange=as.Date(substr(neo$neo1[neo$site==uSite[s]],1,10),'%Y-%m-%d')
#       timeOfChange=neo$neo1SunRise[neo$site==uSite[s]]
#       post1=sum(dataSun$tag==uTag[i]&dataSun$dayDate==dayOfChange&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag)
#       base1=mean(c(sum(dataSun$tag==uTag[i]&dataSun$dayDate==dayOfChange-1&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag),
#                    sum(dataSun$tag==uTag[i]&dataSun$dayDate==dayOfChange-2&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag),
#                    sum(dataSun$tag==uTag[i]&dataSun$dayDate==dayOfChange-3&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag)))
#       #neo2
#       dayOfChange=as.Date(substr(neo$neo2[neo$site==uSite[s]],1,10),'%Y-%m-%d')
#       timeOfChange=neo$neo2SunRise[neo$site==uSite[s]]
#       post2=sum(dataSun$tag==uTag[i]&dataSun$dayDate==dayOfChange&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag)
#       base2=mean(c(sum(dataSun$tag==uTag[i]&dataSun$dayDate==dayOfChange-1&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag),
#                    sum(dataSun$tag==uTag[i]&dataSun$dayDate==dayOfChange-2&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag),
#                    sum(dataSun$tag==uTag[i]&dataSun$dayDate==dayOfChange-3&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag)))
#       
#       #neo3
#       dayOfChange=as.Date(substr(neo$neo3[neo$site==uSite[s]],1,10),'%Y-%m-%d')
#       timeOfChange=neo$neo3SunRise[neo$site==uSite[s]]
#       post3=sum(dataSun$tag==uTag[i]&dataSun$dayDate==dayOfChange&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag)
#       base3=mean(c(sum(dataSun$tag==uTag[i]&dataSun$dayDate==dayOfChange-1&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag),
#                    sum(dataSun$tag==uTag[i]&dataSun$dayDate==dayOfChange-2&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag),
#                    sum(dataSun$tag==uTag[i]&dataSun$dayDate==dayOfChange-3&dataSun$timeRise>timeOfChange&dataSun$timeRise<timeOfChange+lag)))
#       
#       
#       # store
#       #dat=data.frame(site=uSite[s],tag=uTag[i],neo=post1,base1=base1,post2=post2,base2=base2,post3=post3,base3=base3)
#       dat=data.frame(site=rep(uSite[s],3),tag=rep(uTag[i],3),species=rep(unique(dataSun$species[dataSun$tag==uTag[i]]),3),neo=c(post1-base1,post2-base2,post3-base3),base=c(base1,base2,base3),post=c(post1,post2,post3),neoType=c(1,2,3))
#       datlist[[n]]=dat
#       n=n+1
#       print(n)
#     }
#   }
#   #merge
#   neoIndiv=do.call(rbind,datlist)
#   #
#   neoIndiv$elev="Low"
#   neoIndiv$elev[neoIndiv$site %in% c('BA','C4')]="High"
#   
#   # Stats
#   #------
#   m1=lmer(neo~species*elev+neoType+(1|tag), data=neoIndiv,subset = neoIndiv$base>0)
#   
#   sink(paste0(out_n,j,"_Neophobia stats.txt"))
#   print(summary(m1))
#   sink()
#   # Repeatability first visit
#   #---------------------------
#   neoIndivClean=neoIndiv[neoIndiv$base>0,]
#   nboot=10;npermut=10
#   rep=rpt(neo~species+site+neoType+(1|tag), grname = c("tag"), data = neoIndivClean, datatype = "Gaussian",nboot = nboot, npermut = npermut) 
#   
#   #store data
#   res=data.frame('All')
#   names(res)="species"
#   res$R=round(rep$R[[1]],2)
#   res$lowCI=round(rep$CI_emp[[1]],2)
#   res$highCI=round(rep$CI_emp[[2]],2) 
#   
#   ## Rep by species
#   uSpe=unique(d$species)
#   for (s in 1:length(uSpe)){
#     rep=rpt(neo~site+(1|tag), grname = c("tag"), data = neoIndivClean[neoIndivClean$species==uSpe[s],], datatype = "Gaussian",nboot = nboot, npermut = npermut) 
#     temp=data.frame(uSpe[s],round(rep$R[[1]],2),round(rep$CI_emp[[1]],2),round(rep$CI_emp[[2]],2))
#     names(temp)=c("species","R","lowCI","highCI")
#     res = rbind(res,temp)
#   }
#   # Export
#   pdf(paste0(out_n,j,"_Neophobia_repeat.pdf"), height=3, width=3)
#   grid.table(res,rows =c())
#   dev.off()
#   
#   # Plot
#   #-----
#   pdf(paste0(out_n,j,"_Neophobia indiv.pdf"), height=10, width=7)
#   par(mfrow=c(2,2),mar=c(5, 8, 4, 2)/2)
#   hist(neoIndivClean$neo,main=round(median(neoIndivClean$neo),2))
#   boxplot(neoIndivClean$neo~neoIndivClean$species)
#   boxplot(neoIndivClean$neo~neoIndivClean$neoType)
#   boxplot(neoIndivClean$neo~neoIndivClean$elev)
#   dev.off()
#   
#   
# }# loop on lag
# 
# latency to come back 
#---------------------
# TO DO
# CODE NUMBER OF VISIT BEFORE CHANGE
# CODE average ITI in baseline (before and day before)

time=strptime(data$fullTime,"%d/%m/%y %H:%M:%S")
#loop over site
n=1
datlist=list()
data$dayFact=as.character(data$dayDate)
for(s in 1:length(uSite)){
  uTag=unique(data$tag[data$site_folder==uSite[s]])
  #loop over individuals
  nbVis=c()
  mITI=c()
  nbVisB=c()
  for (i in 1:length(uTag)){
    #neo1
    post1=difftime(min(time[data$tag==uTag[i]&time>neo$neo1[neo$site==uSite[s]]]),neo$neo1[neo$site==uSite[s]],units = "secs")
    dayOfChange=as.Date(substr(neo$neo1[neo$site==uSite[s]],1,10),'%Y-%m-%d')
    nbVis[1]=sum(data$tag==uTag[i]&data$dayFact==dayOfChange)
    # control ITI on 2 previous days
    meanITI=c()
    nbVisBase=c()
    for(d in 1:2){
      indD1=dataSun$tag==uTag[i]&dataSun$dayDate==dayOfChange-d
      time1<-chron::times(as.character(dataSun$hour[indD1]))
      time2<-c(chron::times(0),time1)
      time2<-time2[-length(time2)]
      iti=(time1-time2)*86400
      meanITI[d]=mean(iti[2:length(iti)])
      nbVisBase[d]=sum(indD1)
    }
    mITI[1]=mean(meanITI)
    nbVisB[1]=mean(nbVisBase)

    
    #neo2
    post2=difftime(min(time[data$tag==uTag[i]&time>neo$neo2[neo$site==uSite[s]]]),neo$neo2[neo$site==uSite[s]],units = "secs")
    dayOfChange=as.Date(substr(neo$neo2[neo$site==uSite[s]],1,10),'%Y-%m-%d')
    nbVis[2]=sum(data$tag==uTag[i]&data$dayFact==dayOfChange)  
    # control ITI on 2 previous days
    meanITI=c()
    nbVisBase=c()
    for(d in 1:2){
      indD1=dataSun$tag==uTag[i]&dataSun$dayDate==dayOfChange-d
      time1<-chron::times(as.character(dataSun$hour[indD1]))
      time2<-c(chron::times(0),time1)
      time2<-time2[-length(time2)]
      iti=(time1-time2)*86400
      meanITI[d]=mean(iti[2:length(iti)])
      nbVisBase[d]=sum(indD1)
    }
    mITI[2]=mean(meanITI)
    nbVisB[2]=mean(nbVisBase)
    
    #neo3
    post3=difftime(min(time[data$tag==uTag[i]&time>neo$neo3[neo$site==uSite[s]]]),neo$neo3[neo$site==uSite[s]],units = "secs")
    dayOfChange=as.Date(substr(neo$neo3[neo$site==uSite[s]],1,10),'%Y-%m-%d')
    nbVis[3]=sum(data$tag==uTag[i]&data$dayFact==dayOfChange)  
    # control ITI on 2 previous days
    # control ITI on 2 previous days
    meanITI=c()
    nbVisBase=c()
    for(d in 1:2){
      indD1=dataSun$tag==uTag[i]&dataSun$dayDate==dayOfChange-d
      time1<-chron::times(as.character(dataSun$hour[indD1]))
      time2<-c(chron::times(0),time1)
      time2<-time2[-length(time2)]
      iti=(time1-time2)*86400
      meanITI[d]=mean(iti[2:length(iti)])
      nbVisBase[d]=sum(indD1)
    }
    mITI[3]=mean(meanITI)
    nbVisB[3]=mean(nbVisBase)
    
    #neo4
    post4=difftime(min(time[data$tag==uTag[i]&time>neo$neo4[neo$site==uSite[s]]]),neo$neo4[neo$site==uSite[s]],units = "secs")
    dayOfChange=as.Date(substr(neo$neo4[neo$site==uSite[s]],1,10),'%Y-%m-%d')
    nbVis[4]=sum(data$tag==uTag[i]&data$dayFact==dayOfChange)  
    # control ITI on 2 previous days
    # control ITI on 2 previous days
    meanITI=c()
    nbVisBase=c()
    for(d in 1:2){
      indD1=dataSun$tag==uTag[i]&dataSun$dayDate==dayOfChange-d
      time1<-chron::times(as.character(dataSun$hour[indD1]))
      time2<-c(chron::times(0),time1)
      time2<-time2[-length(time2)]
      iti=(time1-time2)*86400
      meanITI[d]=mean(iti[2:length(iti)])
      nbVisBase[d]=sum(indD1)
    }
    mITI[4]=mean(meanITI)
    nbVisB[4]=mean(nbVisBase)
    
     # store
    dat=data.frame(site=rep(uSite[s],4),tag=rep(uTag[i],4),
                   species=rep(unique(dataSun$species[dataSun$tag==uTag[i]]),4),
                   neo=c(post1,post2,post3,post4),neoType=c(1:4),
                   nbVisit=nbVis,mITI=mITI,nbVisB=nbVisB)
    datlist[[n]]=dat
    n=n+1
    print(paste(n,"on",length(uTag)))
  }
}
#merge
lat=do.call(rbind,datlist)
# new variable
lat$neoLog=log(as.numeric(lat$neo))
lat$ratio=log(as.numeric(lat$neo))/log(as.numeric(lat$mITI))
lat$mITIlog=log(as.numeric(lat$mITI))

#store
write.csv2(lat,paste0(out_n,"Neophobia_latency.csv"))



rep=rpt(mITIlog~species+site+neoType+(1|tag), grname = c("tag"), data = lat[lat$nbVisit>0&lat$nbVisB>0&lat$neoType<3,], datatype = "Gaussian",nboot = nboot, npermut = npermut)
rep=rpt(nbVisB~species+site+neoType+(1|tag), grname = c("tag"), data = lat[lat$nbVisit>0&lat$nbVisB>0&lat$neoType<3,], datatype = "Gaussian",nboot = nboot, npermut = npermut)


# repeatability of latency
rep=rpt(ratio~species+site+neoType+(1|tag), grname = c("tag"), data = lat[lat$nbVisit>0&lat$nbVisB>0&lat$neoType<3,], datatype = "Gaussian",nboot = nboot, npermut = npermut)
#store data
res=data.frame('All')
names(res)="species"
res$R=round(rep$R[[1]],2)
res$lowCI=round(rep$CI_emp[[1]],2)
res$highCI=round(rep$CI_emp[[2]],2)

## Rep by species
uSpe=unique(data$species)
for (s in 1:length(uSpe)){
  rep=rpt(ratio~neoType+(1|tag), grname = c("tag"), data = lat[lat$nbVisit>0&lat$nbVisB>0&lat$neoType<3&lat$species==uSpe[s],], datatype = "Gaussian",nboot = nboot, npermut = npermut)
  temp=data.frame(uSpe[s],round(rep$R[[1]],2),round(rep$CI_emp[[1]],2),round(rep$CI_emp[[2]],2))
  names(temp)=c("species","R","lowCI","highCI")
  res = rbind(res,temp)
}
# Export
pdf(paste0(out_n,"_Neophobia_Latency_MOTOR_repeat.pdf"), height=3, width=3)
grid.table(res,rows =c())
dev.off()

# Stats
lat$elev="Low"
lat$elev[lat$site %in% c('BA','C4')]="High"

m1=lmer(neoLog~species*elev+neoType+(1|tag), data=lat,subset = !is.na(lat$neo))
summary(m1)

# Plot
pdf(paste0(out_n,"Neophobia indiv latency.pdf"), height=5, width=15)
par(mfrow=c(1,3),mar=c(5, 8, 4, 2)/2)
hist(as.numeric(lat$neo[!is.na(lat$neo)&lat$neo<36000&lat$neoType>2]),breaks=100,main="")
boxplot(lat$neoLog[!is.na(lat$neo)&lat$neo<36000&lat$neoType>1]~lat$elev[!is.na(lat$neo)&lat$neo<36000&lat$neoType>1])
boxplot(lat$neoLog[!is.na(lat$neo)]~lat$neoType[!is.na(lat$neo)])
dev.off()


# # Raster plot before and after change
# #-------------------------------------
# 
# uTag=unique(dataSun$tag)
# for (i in 1:length(uTag))
# {
#   ind=which(dataSun$tag==uTag[i])
#   if (length(ind)>100){# at least a 100 visit
#     name=paste(unique(dataSun$species[ind]),unique(dataSun$site_folder[ind]))
#     pdf(paste0(out_r,"Neophobia",name,' ',uTag[i],".pdf"), height=5, width=7)
#     
#     barcode(split(dataSun$timeSet[ind]/3600, dataSun$dayDate[ind]),main=name)
#     dev.off()
#   }
#   print(paste(i,'/',length(uTag)))
# }
# 
# 
# 
# 
# 
# d$fullTime=paste(d$day,d$hour)
# time=strptime(d$fullTime,"%d/%m/%y %H:%M:%S")
# 
# # Daily visit rate
# #------------------
# DT <- as.data.table(dataSun)
# DT=DT[,list(nbtrial = .N,species=unique(species),sex=unique(sex),age=unique(age),site=unique(site_folder),site=unique()), by="day,site_folder,scenario,tag"]
# dt=as.data.frame(DT)
# dt$dayDate=as.Date(dt$day,"%d/%m/%y")

#---------------------------------------
# Plot and Stats for Openfeeder Article
#---------------------------------------
# load neophobia variables
#-------------------------
out_n="/Users/maximecauchoix/Documents/wild_cog_OF/results/2018-2019/Neophobia/"
# data
dataSun=read.csv2(out_n,"Data_scenario1-2withTimeFromSunrise.csv")
# timeline
neo=read.csv2(paste0(out_n,"TimelineNeophoia.csv"))

# visit
neoInd=read.csv2(paste0(out_n,"Neophobia_visit.csv"))
resWind=read.csv2(paste0(out_n,"Neophobia_visit_repeatability.csv"))
# latency
lat=read.csv2(paste0(out_n,"Neophobia_latency.csv"))



# plot
ind=neoInd$wind==30
boxplot(sqrt(neoInd$nbVisit[ind]+1)~neoInd$time[ind])
hist(neoInd$nbVisit[neoInd$time=="after"&ind]-neoInd$nbVisit[neoInd$time=="before"&ind])