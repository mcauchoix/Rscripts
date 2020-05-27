# Analyse OF data
# mcauchoixxx@gmail.com, 5/12/17
#--------------------------------------------------------
#to do
# remove XXXXXXX and ???????
#--------------------------------------------------------
rm(list = ls())
# function
#---------
source('~/Documents/openfeeder/Rscripts/slideFunct.R')

#files
#---------
out='/Users/maximecauchoix/Dropbox/wild_cog_2017-18/results/'
out_lc='/Users/maximecauchoix/Dropbox/wild_cog_2017-18/results/learning_curves/'
# 1- merge
#-------------------------------
source('/Users/maximecauchoix/Documents/openfeeder/Rscripts/OF_merge_files.R')
#verifiy data merged
unique(d$path)
# quick look at data
summary(d)
# recode if issue

# 2- remove XXXXXXXX and ????
#-------------------------------
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

# 3- banding summary
#--------------------
source('/Users/maximecauchoix/Documents/openfeeder/Rscripts/OF_summarise_banding.R')

# 4- match with banding keeping all tag 
#-------------------------------
#dall <- merge(d,ball,by="tag", all.x=T,sort=F)#all.x=T

# 5- list output for go-nogo 1
#-------------------------------
#source("/Documents/openfeeder/Rscripts/OF_makeList_gonogo_one.R")

# 6 - identify missing tag
# -------------------------
#source('~/Documents/openfeeder/Rscripts/OF_identify_tag_nobanding.R')

# 7- general stat: nb visi by spe, by site
#-------------------------------
d <- merge(d,ball,by="tag", all.x=F,sort=F)
# remove M1 (as there is no data there because of the woodpecker)
d=d[d$site_folder!='M1',]
# count nb of bird by sites
dunique=d[!duplicated(d$tag),]
print(table(dunique$site_folder,dunique$species))
# nb of visit by bird by site by day
DT <- as.data.table(d)
DT=DT[,list(nbtrial = .N,species=unique(species)), by="tag,site_folder,day"]
dt=as.data.frame(DT)
# nb of visit by bird by site by day
DT <- as.data.table(dt)
DT=DT[,list(species=unique(species),meanVisitByDay=mean(nbtrial)), by="tag,site_folder"]
dt=as.data.frame(DT)
dt$duplicated=duplicated(dt$tag)

# keep only decent n
df=dt[dt$meanVisitByDay>5,]
df$duplicated=duplicated(df$tag)
sum(df$duplicated)
df=df[!df$duplicated,]
table(df$species,df$site_folder)

#plot
boxplot(dt$meanVisitByDay~dt$species+dt$site_folder)


# 8-learning go-nogo all (d4) and go-no go one (d1)
#--------------------------------------------------
#d4=d[d$scenario==30,]
#d1=d[d$scenario==33,]
# deal with time
d$fullTime=paste(d$day,d$hour)
time=strptime(d$fullTime,"%d/%m/%y %H:%M:%S")
indOrder=order(time)
ds=d[indOrder,]
# end expe
print( paste('First day of experiment:', min(strptime(d$day,"%d/%m/%y"))))
print( paste('Last day of experiment:', max(strptime(d$day,"%d/%m/%y"))))
# By individual collect wrong answer according to time
uTag=unique(d$tag)
wind=20
for (i in 1:length(uTag)){
  iInd=ds$tag==uTag[i]
  if (sum(iInd)>wind){
  name=paste(unique(ds$species[iInd]),unique(ds$site_folder[iInd]),uTag[i])
  # total number of trial
  print(paste(sum(iInd)," trials in go no go"))
  lc=slideFunct(ds$denied[iInd],wind,1)
  pdf(paste0(out_lc,name,".pdf"))
  plot(lc,type='l',ylab = 'Error rate',xlab = 'Trial',ylim=c(0,1))
  # add limit between go nogo all and go nogo one
  lim=sum(ds$scenario==30&iInd)
  abline(v=lim,col='red')
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

# 6- social networks
#-------------------------------
