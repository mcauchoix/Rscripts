# Analyse dominance data from pupitre and OF winter 2018-2019
# mcauchoixxx@gmail.com, 25/03/19
#--------------------------------------------------------
#to do
# convert MX to XM 
#-------
# rename 
#--------------------------------------------------------
rm(list = ls())
# library
#--------
# library("ggplot2")
# library("gridExtra")# export table
# #devtools::install_github("mastoffel/rptR", build_vignettes = F)
# library("rptR")# repeatability computation
 library("data.table")
# library("barcode")
# library("lme4")
# #library("lmtest") # 
# library("lmerTest") # 
# library('data.table')
# library('ini')
 library('chron') #cr?er des donn?es temporelles
# library('aniDom')
# library('igraph')
# library('stringr')
# library('reshape')
# function
#---------

# Data files
#---------
path='/Users/maximecauchoix/IAST Dropbox/maxime cauchoix/wild_cog_OF/dominance/2018-2019/data/'
out_f='/Users/maximecauchoix/IAST Dropbox/maxime cauchoix/wild_cog_OF/results/2018-2019/'
#-------------------------
# list all data files

allLogFiles = list.files(paste0(path,'Detection clean/'),pattern = "*.csv")
#---------------------------------
# merge all detection  files
#--------------------------------
dataList <- list()
for (i in 1:length(allLogFiles)) {
  print(paste(allLogFiles[i],i))
  d=read.csv2(paste0(path,'Detection clean/',allLogFiles[i]),header=T,na.strings = c(''))
  d=d[,1:10]
  print(names(d))
  #add names
  #names(d)=c("Date","Heure","Tag","Species","perching.type","Incorrect.tag","Stay.on.perch",
  #            "col.Left.sub","col.right.sub")
  #add variables
  d$site=substr(allLogFiles[i],1,2)
  d$day=substr(allLogFiles[i],14,23)
  d$position=substr(allLogFiles[i],4,5)
  d$OF=substr(allLogFiles[i],7,10)
  d$scenario=substr(allLogFiles[i],12,14)
  d$filename=allLogFiles[i]
  #store in the list
  dataList[[i]]=d
}
# merge all dataset
d=rbindlist(dataList)

# remove not analysed
d=d[!is.na(d$perching.type),]

# clean variables
d$Tag[d$Tag=="notdetect e"]="notdetecte"
d$perching.type[d$perching.type=="Y"]="y"

# remove bird multipe event of the same visit and code full time (and duration, TO DO)
#----------------------------------------------------------------------
gap=3
# deal with bird recorded successively on the same visit
#on decalle tous les RFID d'un cran
Id2<-c("NA",as.character(d$Tag))
Id2<-Id2[-length(Id2)] #suppression de la derni?re valeur pour garder la m?me longueur
#on decalle tous les temps d'un cran
d$time<-times(as.character(d$Heure))
time2<-c(times(0),d$time)
time2<-time2[-length(time2)] #suppression de la derni??re valeur pour garder la m?me longueur
#suppression des donnees si RFID identique la ligne precedente ET la difference de temps est inferieure au seuil
samevisit=(d$Tag==Id2|d$Tag=='??????????'|Id2=='??????????') & (d$time-time2)*86400<gap
# COMPUTE DURATION HERE
# samevisit2=c(NA,samevisit[-length(samevisit)])
# samevisit-samevisit2

d2<-d[!samevisit,]


# compute no detection rate
#--------------------------
ind=d2$Tag %in% c('??????????','XXXXXXXXXX','notdetecte')
sum(d$Tag=="notdetecte"&d$perching.type=="y",na.rm=T)/sum(d$perching.type=="y",na.rm=T)
rate=sum(d2$Tag=="notdetecte"&d2$perching.type=="y",na.rm=T)/sum(d2$perching.type=="y",na.rm=T)
bprate=sum(d2$perching.type=="bp")/(sum(d2$perching.type=="bp")+sum(d2$perching.type=="y"))

sum(d$Tag %in% c('??????????','XXXXXXXXXX','notdetecte')&d$perching.type=="y",na.rm=T)/sum(d$perching.type=="y",na.rm=T)

# time analysed
uFile=unique(d$filename)
dur=c()
for (i in 1:length(uFile)){
 t= as.POSIXlt(d$Heure[d$filename==uFile[i]],tryFormats ="%H:%M:%OS")
 dur[i]=max(t,na.rm = T)-min(t,na.rm = T)
}

print(paste("Time analysed=" ,paste(sum(dur)/60)))
print(paste("Nb OF=" ,length(unique(d2$OF))))

print(table(d2$perching.type))
print(paste("Missed rate=" ,rate*100))

print(paste("badPerched rate=" ,bprate*100))


