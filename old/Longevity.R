rm(list = ls())
library("data.table")

#out
#----
out="/Users/maximecauchoix/IAST Dropbox/maxime cauchoix/spring/Results/survival/"

# load banding
#-------------
source('~/IAST Dropbox/maxime cauchoix/wild_cog_OF/Rscripts/band_clean_2018_19.R')

# load OF data
#-------------
d=read.table('/Users/maximecauchoix/Documents/openfeeder/data/StudySite_2018_19.csv',h=T)


# compute age at last capture for each bird recaptured at least once
#-----------------------------
recap=unique(b$BandNumber[b$duplicat==T])
length(recap)

DT <- as.data.table(b)
DT=DT[,list(nbcature =length(unique(BandNumber)),FirstCapture=min(dateCorrected,na.rm = T),LastCaputre=max(dateCorrected,na.rm = T),
            FCsite=Site[dateCorrected==min(dateCorrected,na.rm = T)],LCsite=unique(Site[dateCorrected==max(dateCorrected,na.rm = T)])), by="BandNumber,Species"]
dt=as.data.frame(DT)

# age
dt$age=dt$LastCaputre-dt$FirstCapture
#  no recap
ind=dt$age<1
dt$age=as.numeric(dt$age)

DT <- as.data.table(dt[dt$Species %in% c('Great','Blue','Coal','Marsh'),])
DT=DT[,list(Nb.banded=.N,Nb.recap=length(unique(BandNumber[age>1])),
            Mean.time.between.first.last.capture=round(mean(age[age>1])),
            Nb.sup1an=sum(age>365),Nb.sup2ans=sum(age>365*2),Nb.sup3ans=sum(age>365*3),Nb.sup4ans=sum(age>365*4)), by="Species"]
ds=as.data.frame(DT)
ds$recap.Prop=round((ds$Nb.recap/ds$Nb.banded)*100)
pdf(paste0(out,"Longevity.pdf"), height=2, width=14)
grid.table(ds,rows=c())
dev.off()

min(b$dateCorrected[b$TagLength==10],na.rm=T)

length(unique(b$RFID.[b$TagLength==10&b$dateCorrected<"2015-09-01"]))

length(unique(b$RFID.[b$TagLength==10&b$dateCorrected<"2016-09-01"&b$Site %in% c("M1","M2","M5","L1")&b$Species=="Blue"]))
length(unique(b$RFID.[b$TagLength==10&b$dateCorrected<"2016-09-01"&b$Site %in% c("M1","M2","M5","L1")&b$Species=="Great"]))

