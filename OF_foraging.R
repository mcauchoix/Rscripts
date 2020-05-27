# to do
#----
# peak of visit during the day
# spetiality on feeder
#library
library("StreamMetabolism")

# Compute time from sunset in S
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
dataSun$timeRise=(times(as.character(dataSun$hour))-dataSun$hourSunrise)*86400
# time according to sunset
dataSun$timeSet=(dataSun$hourSunset-times(as.character(dataSun$hour)))*86400


# raster plot
#------------
 uTag=unique(dataSun$tag)
 for (i in 1:length(uTag))
{
ind=which(dataSun$tag==uTag[i])
if (length(ind)>100){# at least a 100 visit
name=paste(unique(dataSun$species[ind]),unique(dataSun$site_folder[ind]))
pdf(paste0(out_r,"Raster",name,' ',uTag[i],".pdf"), height=5, width=7)

barcode(split(dataSun$timeSet[ind]/3600, dataSun$dayDate[ind]),main=name)
dev.off()
}
print(paste(i,'/',length(uTag)))
}

# Daily pattern of first and last visit
#--------------------------------------
DT <- as.data.table(dataSun)
DT=DT[,list(firstVisit =min(timeRise),lastVisit =min(timeSet),nbtrial = .N,species=unique(species),sex=unique(sex),age=unique(age),site=unique(site_folder)), by="day,site_folder,scenario,tag"]
dt=as.data.frame(DT)
dt$dayDate=as.Date(dt$day,"%d/%m/%y")
dt$firstVisitPos=log(dt$firstVisit+1+min(dt$firstVisit)*-1)
dt$lastVisitPos=log(dt$lastVisit+1+min(dt$lastVisit)*-1)

# repeatability first visit
#---------------------------
rep=rpt(firstVisitPos~species+site_folder+(1|tag), grname = c("tag"), data = dt, datatype = "Gaussian",nboot = nboot, npermut = npermut) 

#store data
res=data.frame('All')
names(res)="species"
res$R=round(rep$R[[1]],2)
res$lowCI=round(rep$CI_emp[[1]],2)
res$highCI=round(rep$CI_emp[[2]],2) 

## Rep by species
uSpe=unique(d$species)
for (s in 1:length(uSpe)){
  rep=rpt(firstVisitPos~site_folder+(1|tag), grname = c("tag"), data = dt[dt$species==uSpe[s],], datatype = "Gaussian",nboot = nboot, npermut = npermut) 
  temp=data.frame(uSpe[s],round(rep$R[[1]],2),round(rep$CI_emp[[1]],2),round(rep$CI_emp[[2]],2))
  names(temp)=c("species","R","lowCI","highCI")
  res = rbind(res,temp)
}
# Export
pdf(paste0(out_f,"Foraging_repeat_FirstVisit.pdf"), height=3, width=3)
grid.table(res,rows =c())
dev.off()

# repeatability last visit
#---------------------------
rep=rpt(lastVisitPos~species+site_folder+(1|tag), grname = c("tag"), data = dt, datatype = "Gaussian",nboot = nboot, npermut = npermut) 

#store data
res=data.frame('All')
names(res)="species"
res$R=round(rep$R[[1]],2)
res$lowCI=round(rep$CI_emp[[1]],2)
res$highCI=round(rep$CI_emp[[2]],2) 

## Rep by species
uSpe=unique(d$species)
for (s in 1:length(uSpe)){
  rep=rpt(lastVisitPos~site_folder+(1|tag), grname = c("tag"), data = dt[dt$species==uSpe[s],], datatype = "Gaussian",nboot = nboot, npermut = npermut) 
  temp=data.frame(uSpe[s],round(rep$R[[1]],2),round(rep$CI_emp[[1]],2),round(rep$CI_emp[[2]],2))
  names(temp)=c("species","R","lowCI","highCI")
  res = rbind(res,temp)
}
# Export
pdf(paste0(out_f,"Foraging_repeat_LastVisit.pdf"), height=3, width=3)
grid.table(res,rows =c())
dev.off()



# Work on ITI
#------------
# sort data for increasing time
d$fullTime=paste(d$day,d$hour)
time=strptime(d$fullTime,"%d/%m/%y %H:%M:%S")
indOrder=order(time)
d=d[indOrder,]

# compute ITI by individual by dat
uTag=unique(d$tag)
for (i in 1:length(uTag)){
  #loop overday
  uDay=unique(d$day[d$tag==uTag[i]])
  for (j in 1:length(uDay)){
    ind=d$day==uDay[j]&d$tag==uTag[i]
    # compute iti
    time<-times(as.character(d$hour[ind]))#sort
    time2<-c(times(0),time)
    time2<-time2[-length(time2)]
    diff=(time-time2)*86400
    diff[1]=NaN
    d$diff[ind]=diff
    print(paste(i,length(uTag),uDay[j]))
  }
}

# ITI descriptives stats
#-----------------------
# nb of visit by tag, species and site by species
DT <- as.data.table(d)
DT=DT[,list(nbtrial = .N,meanITI=mean(diff,na.rm=T),medianITI=median(diff,na.rm=T),sdITI=sd(diff,na.rm=T),iqrITI=IQR(diff,na.rm = T),site=unique(site_folder),scenario=unique(scenario),species=unique(species)), by="tag,day"]
dv=as.data.frame(DT)
median(dv$medianITI[dv$nbtrial>30])
mean(dv$meanITI[dv$nbtrial>30])

# recode variables
dv$elevation="low"
dv$elevation[dv$site %in% c('C4','BA')]='high'
dv$logMeanITI=log(dv$meanITI+1)
dv$logMedianITI=log(dv$medianITI+1)
dv$logNbtral=log(dv$nbtrial+1)
dv$logsdITI=log(dv$sdITI+1)
dv$logiqrITI=log(dv$iqrITI+1)
dv$dayDate=as.Date(dv$day,'%d/%m/%y')
dv=dv[dv$dayDate>as.Date('01/09/18','%d/%m/%y'),]

var=c('logMeanITI','logMedianITI','logNbtral','logsdITI','logiqrITI')
for (i in 1:length(var)){
x=var[i]

# stats
m1=lmer(as.formula(as.formula(paste(x,'elevation+species+scenario+(1|tag)',sep='~'))),data=dv)
summary(m1)
sink(paste0(out_fo,x," stats.txt"))
print(summary(m1))
sink()

# Plot
pdf(paste0(out_fo,x,".pdf"), height=10, width=10)
par(mfrow=c(2,2),mar=c(5, 8, 4, 2)/2)
hist(eval(parse(text=paste('dv',x,sep='$'))))
boxplot(as.formula(paste(x,'elevation',sep='~')),data=dv)
boxplot(as.formula(paste(x,'species',sep='~')),data=dv)
boxplot(as.formula(paste(x,'scenario',sep='~')),data=dv)
dev.off()

# Repeatability 
nboot=10;npermut=10
rep=rpt(as.formula(as.formula(paste(x,'elevation+dayDate+species+scenario+(1|tag)',sep='~'))), grname = c("tag"), data = dv, datatype = "Gaussian",nboot = nboot, npermut = npermut) 

#store data
res=data.frame('All')
names(res)="species"
res$R=round(rep$R[[1]],2)
res$lowCI=round(rep$CI_emp[[1]],2)
res$highCI=round(rep$CI_emp[[2]],2) 

## Rep by species
uSpe=unique(d$species)
for (s in 1:length(uSpe)){
  rep=rpt(as.formula(as.formula(paste(x,'elevation+dayDate+scenario+(1|tag)',sep='~'))), grname = c("tag"), data = dv[dv$species==uSpe[s],], datatype = "Gaussian",nboot = nboot, npermut = npermut) 
  temp=data.frame(uSpe[s],round(rep$R[[1]],2),round(rep$CI_emp[[1]],2),round(rep$CI_emp[[2]],2))
  names(temp)=c("species","R","lowCI","highCI")
  res = rbind(res,temp)
}
# Export
pdf(paste0(out_fo,x,"repeat.pdf"), height=3, width=3)
grid.table(res,rows =c())
dev.off()
}

# Preprocess OF selectivity and daily distribution of foraging
#----------------------------------------------------------

datlist=list()
n=1
uTag=unique(d$tag)
for (i in 1:length(uTag)){
  #loop overday
  uDay=unique(d$day[d$tag==uTag[i]&d$task!='S1_Hab_1OF'])
  if (length(uDay)>1){
    for (j in 1:length(uDay)){
      ind=d$day==uDay[j]&d$tag==uTag[i]
      # compute selectivity
      site=factor(d$site[ind])
      OFmax=max(table(site)/sum(table(site)))
      OFmin=min(table(site)/sum(table(site)))
      # foraging peak and hollow
      c=hist(dataSun$timeRise[ind]/3600,breaks=10,plot=FALSE)
      hour_max=c$mids[c$counts==max(c$counts)]
      hour_min=c$mids[c$counts==min(c$counts)]
      dat=data.table(tag=uTag[i],day=uDay[j],site=unique(d$site_folder[ind]),species=unique(d$species[ind]),scenario=unique(d$scenario[ind]),
                     OFmax=OFmax,OFmin=OFmin,nbOF=sum(table(site)>1),
                     hour_max=hour_max[1],hour_min=hour_min[1])
      
      print(paste(i,length(uTag),uDay[j]))
      #store
      datlist[[n]]=dat
      n=n+1
    }
  }
}
fo=do.call(rbind,datlist)
# code variables
fo$elevation="low"
fo$elevation[fo$site %in% c('C4','BA')]='high'
fo$dayDate=as.Date(fo$day,'%d/%m/%y')
fo=fo[fo$dayDate>as.Date('01/09/18','%d/%m/%y'),]

# Results OF selectivity and daily distribution of foraging
#----------------------------------------------------------
#fucntion
source('~/IAST Dropbox/maxime cauchoix/cognitive repeatability/Rscripts/OF_resultsOutput.R')

var=names(fo)[6:10]
for (v in 1:length(var)){
OF_resultsOutput(var[v],fo,out_fo)
}
# save
write.table(fo,paste0(out_fo,"foragingData.txt"))
write.table(d,paste0(out_fo,"allData_ITI.txt"))