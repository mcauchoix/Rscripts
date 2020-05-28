# Analyse OF data
# mcauchoixxx@gmail.com, 5/12/17
#--------------------------------------------------------
#to do
#-------
# Cognition: ind variable to loop on criterim 1/9, 17/20 ou 24/30 check accuracy before and after
# Network: check gs and remove abherant ind
# MuMin: selection
#install.packages("basicTrendline") # to plot exp: https://github.com/PhDMeiwp/basicTrendline
#--------------------------------------------------------
rm(list = ls())
# library
#--------
# for plots
library("ggplot2")
library("dabestr")
library("GGally")
library("ggforce")# sina plot
# to read excel file
# library('rJava')
# library('xlsxjars')
# library('xlsx')
# export table
library("gridExtra")
# manipulate data
library("data.table")
library('chron') #for temporal data
# stats
library("lme4")
library("lmerTest") # 
library("rptR")# repeatability computation
library("MuMIn")
library("corrplot")
library("DHARMa")
library("gam")
# for Gamm
library("gamm4")
library(mgcv)
# other
library('ini')# read ini

# Rmd
library(knitr)
library(rmarkdown)
library(markdown)


# go into R script directory
#-------------------
setwd("/Users/maximecauchoix/Documents/wild_cog_OF/Rscripts/")

# function
#---------
# To compute learning curve
source('slideFunct.R')
source('OF_mergeLogFiles.R')
source('OF_summarise_banding_2018_19.R')
#files
#---------
path='/Users/maximecauchoix/Documents/wild_cog_OF/'

# OUT
pathD='~/Documents/wild_cog_OF/data/2018-2019/'
out_f=paste0(path,'results/2018-2019/')
out_lc=paste0(out_f,'learning-curves/')
out_ls=paste0(out_f,'learning-stats/')
out_v=paste0(out_f,'daily-visit/')
out_r=paste0(out_f,'raster/')
out_n=paste0(out_f,'neophobia/')
out_fo=paste0(out_f,'foraging/')
out_rep=paste0(out_f,'repeatability/')
out_sig=paste0(out_f,'sigmoid/')
out_ils=paste0(out_f,'individual-learning-stats/')
out_net=paste0(out_f,'network/')
out_fit=paste0(out_f,'fitness/')
out_dist=paste0(out_f,'cog_perf_distrib/')
out_bioVal=paste0(out_f,'biological value/')
dir.create(out_bioVal)

#dir.create(out_fit)
out_local='~/Documents/openfeeder/data/'

# IN
#experimental change# SAVE IN CSV
# ec=read.xlsx('/Users/maximecauchoix/Documents/wild_cog_OF/logistique/OF timeline.xlsx',sheetName = 1,header = T)
# ec=ec[,1:7]
# ec$dayDate=as.Date(ec$Day,"%d/%m/%y")# convert date
# ec$Hour.end=substr(ec$Hour.end,12,19)

# 1- merge
#-------------------------------
d=OF_mergeLogFiles(pathD, out_f,3)
d=d[rowSums(is.na(d))!=22,] # remove NA Line !!!!! Find where is it coming from in mergefiles (porbably from the 2 M1 corrupted files)
# 
#source('~/IAST Dropbox/maxime cauchoix/wild_cog_OF/Rscripts/OF_merge_files_2018_19.R')


# 2-Clean
#-------------------------------

# recode factor in numeric
d$scenario=as.numeric(as.character(d$scenario))
d$denied=as.numeric(as.character(d$denied))
d$reward=as.numeric(as.character(d$reward))
d$red=as.numeric(as.character(d$red))
d$green=as.numeric(as.character(d$green))
d$blue=as.numeric(as.character(d$blue))
d$door_status_at_arrival=as.numeric(as.character(d$door_status_at_arrival))
d$door.open=as.numeric(as.character(d$door.open))

# remove H1 and L1
d=d[!d$site_folder %in% c('H1','L1','GJ'),]
d=d[!d$task=="S1_Hab_1OF",]
# code reversal
d$scenario[d$task=="S3_R-L_4OF"]=32


# 3- Deal with not banded and incorrect perch 
#------------------------------------------
# count, test scenario, day, site and OF effect, remove
#source('~/IAST Dropbox/maxime cauchoix/wild_cog_OF/Rscripts/OF_notag_and_badPerch.R')
# no tag read
indNoTag=d$tag=="XXXXXXXXXX"
print(paste(sum(indNoTag)/dim(d)[1]*100,'% of bird without tag'))
d=d[!indNoTag,]
# bad read
indUnread=d$tag=="??????????"
print(paste(sum(indUnread)/dim(d)[1]*100,'% of bird unread'))
d=d[!indUnread,]
d$tag=factor(d$tag)
d$OFnb=factor(d$OFnb)

# 4- banding summary
#--------------------
#source('~/IAST Dropbox/maxime cauchoix/wild_cog_OF/Rscripts/OF_summarise_banding_2018_19.R')
source('~/Documents/wild_cog_OF/Rscripts/band_clean_2018_19.R')
# nb ind by year
b$capt="Adult Nest"
b$capt[is.na(b$Nest.Number)]='Adult Missnet'
b$capt[b$Age=='PUL']='PUL'
#b$capt[b$Site!="M1"]=NA
#b$capt[!b$Species %in% c("Blue","Great")]=NA
table(b$Year,b$capt)


# to correct error for Marine
#ball=OF_summarise_banding(unique(b$RFID.[b$TagLength==10&!is.na(b$RFID.)]),b,'Tagged bird')

source('OF_summarise_banding.R')
ball=OF_summarise_banding(intersect(unique(d$tag),unique(b$RFID.)),b)
print(paste("Nb bird that should have fitness data:",sum(ball$nest!="NA")))
print(paste("Nb GT that should have fitness data:",sum(ball$nest!="NA"&ball$species=="Great")))
print(paste("Nb BT that should have fitness data:",sum(ball$nest!="NA"&ball$species=="Blue")))
#ball[ball$tag=="011016BE94",]


# 4- match with banding keeping all tag 
#-------------------------------
dOri=d
dall <- merge(d,ball,by="tag", all.x=T,sort=F)#all.x=F (keep only bird with banding data)

# 5 - identify missing tag
# -------------------------
source('~/Documents/wild_cog_OF/Rscripts/OF_identify_tag_nobanding.R')

# 6- remove  tag 
# -------------------------
#bird with less than 10 visits
d=d[d$tag %in% v$tag[v$nbvisitTotal>100],]
#test tag Maxime
d=d[!(d$tag %in% c('0300030F40','0110170902','0700EDCD6C','011017939E')),]#Maxime
# check removed bird
dall=d
source('~/Documents/wild_cog_OF/Rscripts/OF_identify_tag_nobanding.R')
#write.table(vNot,paste0(out_f,'NotInBanding.csv'),sep=";",row.names = F)
# Remove NO DATA tag
#d=d[d$tag!='',] #!!!There is an issue in merge with folder info added while there is no data

# with banding info only
d <- merge(dall,ball,by="tag", all.x=F,sort=F)#all.x=F (keep only bird with banding data)

# to keep Nory's tag for Neophobia
d <- merge(dall,ball,by="tag", all.x=T,sort=F)#all.x=F (keep only bird with banding data)


#7- Clean data and add variables
#-------------------------------
# code elevation
d$elevation='low'
d$elevation[d$site_folder %in% c('C4','BA')]='high'
# remove nuthatch and coal
d=d[d$species %in% c('Blue','Great','Marsh'),]
# code day as date
d$dayDate=as.Date(d$day,"%d/%m/%y")

# sort data chronologicaly
d$fullTime=paste(d$day,d$hour)
time=strptime(d$fullTime,"%d/%m/%y %H:%M:%S")
indOrder=order(time)
d=d[indOrder,]

# code trial number
uSc=unique(d$scenario)
uTag=unique(d$tag)
for (i in 1:length(uTag)){
  print(i)
  for (s in 1:length(uSc)){
    ind=d$tag==uTag[i]&d$scenario==uSc[s]
    if (sum(ind)>0){
      d$trialNumber[ind]=1:sum(ind)
    }
  }
}

# code day number
uSite=unique(d$site_folder)
uSc=unique(d$scenario)
d$dayDate=as.Date(d$day,"%d/%m/%y")
# Bird day
for (si in 1:length(uSite)){
  print(uSite[si])
  for (sc in 1:length(uSc)){
    i=d$site_folder==uSite[si]&d$scenario==uSc[sc]
    d$dayNumber[i]=d$dayDate[i]-min(d$dayDate[i])+1# code day from beginning of experiment
    uTag=unique(d$tag[i])
    print(uSc[sc])
    for (t in 1:length(uTag)){
      indi=i&d$tag==uTag[t]
      if (sum(indi)>0){
        birdDay=sort(unique(d$dayDate[indi]))
        for (day in 1:length(birdDay)){
          indid=indi&d$dayDate==birdDay[day]
        d$BirdDaynumber[indid]=day}
      }
    }
  }
}
# EQUALIZE DAY LENGTH BETWEEN SITE FOR 31 and 32
  
# save localy
write.table(d,'/Users/maximecauchoix/Documents/openfeeder/data/StudySite_2018_19.csv')
# load
d=read.table('/Users/maximecauchoix/Documents/openfeeder/data/StudySite_2018_19.csv',h=T)


# 8- Number of visits
#-------------------------------
source('~/Dropbox/wild_cog_OF/Rscripts/OF_numberOFvisits.R')

# 9- Individual data (nb trial, learning, TTC , ACC etc): DO DTC. 
#-------------------------------
#source('~/Dropbox/wild_cog_OF/Rscripts/OF_individualStat_with learning.R')

# 10- Correlation between individual data
#-------------------------------------
source('~/Documents/wild_cog_OF/Rscripts/OF_individual_learning_correlation.R')




#----------------
# repeatability/ efffect species, elevation, sex, age and task effect, fitness and network
#----------------



# 10- Participation (add participation using TTC or DTC and do stat effect of species and elevation and Sex and Age for GT only)
#-------------------
source('~/Dropbox/wild_cog_OF/Rscripts/OF_participation.R')


# 11- Learning repeatability
#-------------------------------
source('~/Dropbox/wild_cog_OF/Rscripts/OF_repeatability.R')

# 10- Learning curves
#-------------------------------
source('~/Dropbox/wild_cog_OF/Rscripts/OF_learningCurves.R')


# 11- Neophobia
#------------------------------
# visit rate: compare visit rate during S1 et S2-75% and S2-0% et S2_red ou S2_gre
# delay: compare time between change and next visit and average ITI
source('~/Dropbox/wild_cog_OF/Rscripts/OF_neophobia.R')



# 12- Foraging
#------------------------------
data=d#[d$task=="S2_0%_4OF",]
source('~/Dropbox/wild_cog_OF/Rscripts/OF_foraging.R')

# 13- Social networks
#------------------------------
data=d[d$task=="S1_S2_Hab_4OF",]
write.table(data,paste0(out_f,"NetworkData_habituation.csv"))

# 13- Social networks and cognition # UPDATE GS REMOVE IND WITH NOT ENOUGH TRIALS 
#------------------------------


# 7- Descriptive stat: nb of bird
#-------------------------------
# new variable
d$dayDate=as.Date(d$day,"%d/%m/%y")

# count nb of bird by sites
dunique=d[!duplicated(d$tag),]
print(table(dunique$site_folder,dunique$species))
print(table(d$site_folder,d$species))

# nb of bird by scenario
DT <- as.data.table(d)
DT=DT[,list(nbbirds =length(unique(tag))), by="site_folder,scenario,ini.perc.door.open"]
dt=as.data.frame(DT)
print(dt)

# nb of bird by OF location
DT <- as.data.table(d[d$dayDate>ec$dayDate[1],])# G1 was also used for single feeder before 26/09/18
DT=DT[,list(nbbirds =length(unique(tag))), by="site_folder,site"]
dt=as.data.frame(DT)
print(dt)

# nb of bird by day
DT <- as.data.table(d)
DT=DT[,list(nbbirds =length(unique(tag))), by="site_folder,day"]
dt=as.data.frame(DT)
dt$dayDate=as.Date(dt$day,"%d/%m/%y")
uSite=unique(dt$site_folder)
for (i in 1:length(uSite)){
# plot number of birds recorded every days
pdf(paste0(out_f,uSite[i],"_Nb birds by day.pdf"), height=5, width=10)
plot(dt$dayDate[dt$site_folder==uSite[i]],dt$nbbirds[dt$site_folder==uSite[i]],xlab = "Day",ylab = 'Nb birds recorded',pch=19)
# add experimental changes
eci=ec[ec$type.of.change=='E'&ec$Location==uSite[i],]
if (length(eci$change)>0){
for (j in 1:length(eci$change))
{
  abline(v=eci$dayDate[j])
  text(eci$dayDate[j],20,labels = eci$change[j],cex=0.7,font = 4)
}
}
dev.off()

}

# 8- Descriptive stat: nb of visit 
#----------------------------------

# nb of visit by species
DT <- as.data.table(d)
DT=DT[,list(nb.visit.total= .N), by="species"]
dt=as.data.frame(DT)
print(dt)


# nb of visit by tag, species and site by species
DT <- as.data.table(d)
DT=DT[,list(nbtrial = .N), by="tag,day,site_folder,species"]
dv=as.data.frame(DT)


# mean visit by day ,
DT <- as.data.table(dv)
DT=DT[,list(meanVisitByDay=mean(nbtrial)), by="day"]
dt=as.data.frame(DT)
dt$dayDate=as.Date(dt$day,"%d/%m/%y")
# plot mean number of birds recorded every days
pdf(paste0(out_f,"Mean visit by day.pdf"), height=5, width=10)
plot(dt$dayDate,dt$meanVisitByDay,xlab = "Day",ylab = 'Mean number of visit',pch=19)
# add experimental changes
eci=ec[ec$type.of.change=='E'&ec$Location=='GJ',]
for (i in 1:length(eci$change))
{
  abline(v=eci$dayDate[i])
  text(eci$dayDate[i],10,labels = eci$change[i],cex=0.5,font = 4)
}
dev.off()
# mean visit by species ,
DT <- as.data.table(dv)
DT=DT[,list(meanVisitByDay=mean(nbtrial)), by="day,species"]
dt=as.data.frame(DT)
dt$dayDate=as.Date(dt$day,"%d/%m/%y")
# plot mean number of birds recorded every days
ggplot(dt, aes(dayDate,meanVisitByDay,colour=species)) + 
  geom_line() + 
  geom_point()
# add experimental changes
for (i in 1:length(eci$change))
{
 geom_vline(xintercept = as.numeric(eci$dayDate[i]))
  #text(ec$dayDate[i],10,labels = ec$change[i],cex=0.5,font = 4)
}

# 9- Descriptive stat: nb of feeding event 
#-----------------------------------------

# nb of visit by tag, species and site by species
DT <- as.data.table(d[d$door.open==1,])
DT=DT[,list(nbtrial = .N), by="tag,day,site_folder,species"]
dv=as.data.frame(DT)


# mean feeding event by day ,
DT <- as.data.table(dv)
DT=DT[,list(meanVisitByDay=mean(nbtrial)), by="day"]
dt=as.data.frame(DT)
dt$dayDate=as.Date(dt$day,"%d/%m/%y")
# plot mean number of birds recorded every days
plot(dt$dayDate,dt$meanVisitByDay,xlab = "Day",ylab = 'Mean door open event')
# add experimental changes
eci=ec[ec$type.of.change=='E'&ec$Location=='GJ',]
for (i in 1:length(eci$change))
{
  abline(v=eci$dayDate[i])
  text(eci$dayDate[i],10,labels = eci$change[i],cex=0.5,font = 4)
}

# 11- Neophobia
#--------------------


# 10- Learning by day: population level
#-------------------------------------
DT <- as.data.table(d)
DT=DT[,list(ACC =mean(door.open)), by="day,site_folder"]
dt=as.data.frame(DT)
dt$dayDate=as.Date(dt$day,"%d/%m/%y")
# plot mean number of birds recorded every days
plot(dt$dayDate,dt$ACC,xlab = "Day",ylab = 'Mean door open event')
eci=ec[ec$type.of.change=='E'&ec$Location=='GJ',]
for (i in 1:length(eci$change))
{
  abline(v=eci$dayDate[i])
  text(eci$dayDate[i],0.5,labels = eci$change[i],cex=0.5,font = 4)
}

# 11- Learning by day: individual level
#-------------------------------------
DT <- as.data.table(d[d$scenario==30,])
DT=DT[,list(ACC =mean(door.open),nbtrial = .N,species=unique(species)), by="day,tag,site_folder"]
dt=as.data.frame(DT)
dt$dayDate=as.Date(dt$day,"%d/%m/%y")
# plot accuracy by day by individual
uTag=unique(dt$tag)
for (i in 1:length(uTag)){
  #name
  iInd=which(d$tag==uTag[i])
  name=paste(unique(d$site_folder[iInd[1]]),unique(d$species[iInd[1]]),uTag[i])
  pdf(paste0(out_lc,name,"_day.pdf"))
  
  #plot
  par(mfcol=c(2,1),mar=c(5, 4, 4, 2)/2)
  plot(dt$dayDate[dt$tag==uTag[i]],dt$ACC[dt$tag==uTag[i]],xlab = "Day",ylab = 'Accuracy',pch=19,xlim = c(min(dt$dayDate),max(dt$dayDate)),main='Acc',ylim=c(0,1))
  plot(dt$dayDate[dt$tag==uTag[i]],dt$nbtrial[dt$tag==uTag[i]],xlab = "Day",ylab = 'Nb trials',pch=19,xlim = c(min(dt$dayDate),max(dt$dayDate)),main='Nb trials')
  dev.off()
}

# 12- Learning diffusion
#-------------------------
ggplot(subset(dt,nbtrial>30), aes(dayDate,ACC,colour=tag)) + 
  geom_line() + 
  geom_point()

# 13-Trial to trial learning curve go-nogo all 
#--------------------------------------------------
# deal with time
d$fullTime=paste(d$day,d$hour)
time=strptime(d$fullTime,"%d/%m/%y %H:%M:%S")
indOrder=order(time)
ds=d[indOrder,]
# Select go-no go all (s=30)
d30=ds[ds$scenario==31,]
print( paste('First day of experiment:', min(strptime(d30$day,"%d/%m/%y"))))
print( paste('Last day of experiment:', max(strptime(d30$day,"%d/%m/%y"))))
# By individual collect wrong answer according to time
uTag=unique(d30$tag)
wind=20
for (i in 1:length(uTag)){
  iInd=d30$tag==uTag[i]
  if (sum(iInd)>wind){
  name=paste(unique(d30$site_folder[iInd]),unique(d30$species[iInd]),uTag[i],sum(iInd),'trials')
  # total number of trial
  print(paste(sum(iInd)," trials in go no go"))
  lc=slideFunct(d30$door.open[iInd],wind,1)
  pdf(paste0(out_lc,name,".pdf"))
  plot(lc,type='l',ylab = 'Error rate',xlab = 'Trial',ylim=c(0,1))
  abline(h=0.75,col='red')
  abline(h=0.35,col='green')
  title(name)
  dev.off()
  }
}



# 3- list output for go-nogo 1
#-------------------------------
C1tag=unique(dall$tag[dall$OFnb=="C1"])
sink(paste0(out,'C1.txt'))
cat(as.character(C1tag),sep="")
sink()


# 3- check numbers 
#-------------------------------
# tag not read but detected

x=OF_check_data(d)



# 4- general stat 
#-------------------------------
# total nb visit by species
# total nb visit by individual by spe
# daily number of visit ...

# 5- foraging plot
#-------------------------------
par(mfrow=c(2,2))
hist(strptime(d$hour[d$site_folder=='GJ'&d$species=="Great"],"%H:%M:%S"),breaks=10)
hist(strptime(d$hour[d$site_folder=='GJ'&d$species=="Blue"],"%H:%M:%S"),breaks=10)
hist(strptime(d$hour[d$site_folder=='C4'&d$species=="Great"],"%H:%M:%S"),breaks=10)

# 6- social networks
#-------------------------------
