
table(b$InjuredLeg)
table(b$TypeOfInjury)

# remove other species
b=b[b$Species %in% c("Blue","Great","Marsh"),]
b$Species=factor(b$Species)

# tagged bird
uTagedBird=unique(b$BandNumber[grepl("T",b$Color, ignore.case = TRUE)])
uTagedBird=uTagedBird[!is.na(nchar(uTagedBird))&nchar(uTagedBird)==7]# keep only correct band number


# select injury
#------------------
injT=b$Injury=="Old Injury"&b$InjuredLeg=="Tag" # conservative
sum(injT,na.rm = T)
#injT=b$Injury!="New Injury"&b$InjuredLeg=="Tag" # liberal
#sum(injT,na.rm = T)
injO=b$Injury=="Old Injury"&b$InjuredLeg %in% c("Co2","CoM","MCo","xM")&b$BandNumber %in% uTagedBird

# Tag
#----
# take only 1st record of injury by bird
uBird=unique(b$BandNumber[which(injT)])
print(paste("total number of bird injured by tag",length(uBird)))
injTindUn=c()
for (i in 1:length(uBird)){
  injTindUn[i]=max(which(b$BandNumber==uBird[i]&injT))
}
# count number by spe by year
DT <- as.data.table(b)
DT=DT[injTindUn,list(N =length(unique(BandNumber))), by="Year,Species"]
dt=as.data.frame(DT)
print(table(b$Year[injTindUn],b$Species[injTindUn]))


# Other
#----
# take only 1st record of injury by bird
uBird=unique(b$BandNumber[which(injO)])
print(paste("total number of bird injured by Metal or color",length(uBird)))
injOindUn=c()
for (i in 1:length(uBird)){
  injOindUn[i]=max(which(b$BandNumber==uBird[i]&injO))
}
# count number by spe by year
DT <- as.data.table(b)
DT=DT[injOindUn,list(N =length(unique(BandNumber))), by="Year,Species"]
do=as.data.frame(DT)
# type of injury by species
print(table(b$InjuredLeg[injOindUn],b$Species[injOindUn]))


# inter-year
#-----------
# Compute delay from 1st time tag installed

# bird who that have been tagged
# uTagedBird=unique(b$BandNumber[grep("T",b$Color, ignore.case = TRUE)])
# uTagedBird=uTagedBird[!is.na(nchar(uTagedBird))&nchar(uTagedBird)==7]# keep only correct band number
#uTagedBird2=unique(b$BandNumber[b$dateCorrected>"2014-08-01"&b$Age!="PUL"])
#uNOTagedBird2=unique(b$BandNumber[!grepl("T",b$Color, ignore.case = TRUE)])
#uTagedBird=unique(b$BandNumber[nchar(as.character(b$RFID.))==10])



uRecap=unique(b$BandNumber[b$BandNumber %in% uTagedBird &b$duplicat==T])
print(length(uRecap)/length(uTagedBird))

# code first time with tag
b$delay=NA
b$delayFromTag=NA
b$InterCaptureDelay=NA
firstTag=b[1,]
lall=c()
ltag=c()
for (i in 1:length(uRecap)){
  ind=b$BandNumber==uRecap[i]
  tagPosDate=min(b$dateCorrected[which(ind&grepl("T",b$Color, ignore.case = TRUE))],na.rm=T)
  indTagPos=max(which(ind&b$dateCorrected==tagPosDate))
  
  indT=which(ind&b$dateCorrected>=tagPosDate)
  ind=which(ind)
  # keep information of when the tag was posed
  firstTag[i,]=b[indTagPos,]
    
  lall[i]=length(ind)
  ltag[i]=length(indT)
  # diff with tag banding
  if (length(indT)>1){
    b$delayFromTag[ind]=b$dateCorrected[ind]-min(b$dateCorrected[indT],na.rm=T)
    
    indT=sort(indT,decreasing = T)
    # diff with previous
    for (j in 2:length(indT)){
      b$InterCaptureDelay[indT[j]]=b$dateCorrected[indT[j]]-b$dateCorrected[indT[j-1]]
      
    }
  }
  
}  
# to verify
b$BandNumber[injTindUn[is.na(b$delayFromTag[injTindUn])]]

a=firstTag[!grepl("T",firstTag$Color, ignore.case = TRUE),]




#-------------------
# Recapture rate
#-------------------

# all year/all cofounded (sheldon's style)
#-------------------------------------
# # capture
# DT <- as.data.table(b)
# DT=DT[b$dateCorrected>"2014-08-01"&b$Age!="PUL",list(N =.N), by="Species"]
# dcapture=as.data.frame(DT)
# dcapture=dcapture[order(dcapture$Species),]
# 
# # recapture after tag
# DT <- as.data.table(b)
# DT=DT[b$duplicat==T&b$dateCorrected>"2014-08-01"&b$Age!="PUL",list(N =.N), by="Species"]
# drecaptureAll=as.data.frame(DT)
# drecaptureAll=drecaptureAll[order(drecaptureAll$Species),]

# capture
DT <- as.data.table(b)
DT=DT[grepl("T",b$Color, ignore.case = TRUE),list(equipped=length(unique(BandNumber))), by="Species"]
dcapture=as.data.frame(DT)
dcapture=dcapture[order(dcapture$Species),]

# recapture after tag
DT <- as.data.table(b)
DT=DT[!is.na(b$InterCaptureDelay),list(N =length(unique(BandNumber))), by="Species"]
drecapture=as.data.frame(DT)
drecapture=drecaptureTag[order(drecaptureTag$Species),]

# recaptureInterYear
DT <- as.data.table(b)
DT=DT[b$InterCaptureDelay>180,list(N =length(unique(BandNumber))), by="Species"]
drecaptureInterYear=as.data.frame(DT)
drecaptureInterYear=drecaptureInterYear[order(drecaptureInterYear$Species),]


# tag injury
DT <- as.data.table(b)
DT=DT[injTindUn,list(N =.N), by="Species"]
dblessureTag=as.data.frame(DT)
dblessureTag=dblessureTag[order(dblessureTag$Species),]

# tag injury
DT <- as.data.table(b)
DT=DT[injTindUn[which(b$InterCaptureDelay[injTindUn]>180)],list(N =.N), by="Species"]
dblessureTagInterYear=as.data.frame(DT)
dblessureTagInterYear=dblessureTagInterYear[order(dblessureTagInterYear$Species),]

# other injury
DT <- as.data.table(b)
DT=DT[injOindUn,list(N =.N), by="Species"]
dblessureOther=as.data.frame(DT)
dblessureOther=dblessureOther[order(dblessureOther$Species),]

# merge
dcapture$recapture=drecapture$N
dcapture$recaptureInterYear=drecaptureInterYear$N
dcapture$TagInjury=dblessureTag$N
dcapture$TagInjuryInterYear=dblessureTagInterYear$N
dcapture$OtherBandInjury=dblessureOther$N

# rates
dcapture$recapRate=round(dcapture$recapture/dcapture$equipped*100,2)
dcapture$recapRateInterYear=round(dcapture$recaptureInterYear/dcapture$equipped*100,2)

dcapture$TagInjuryRate=round(dcapture$TagInjury/dcapture$recapture*100,2)
dcapture$OtherBandInjuryRate=round(dcapture$OtherBandInjury/dcapture$recapture*100,2)

dcapture$TagInjuryRateInterYear=round(dcapture$TagInjuryInterYear/dcapture$recaptureInterYear*100,2)

# save
print(dcapture)
pdf(paste0(out,"InjuryRate_2014-2018_allRecap.pdf"), height=2, width=16)
grid.table(dcapture,rows =c())
dev.off()


# by Year
#---------------
DT <- as.data.table(b)
DT=DT[b$duplicat==1,list(N =length(unique(BandNumber))), by="Year,Species"]
dr_all=as.data.frame(DT)





# exploratory variable
#---------------------
ini=firstTag[firstTag$Species=="Great",]
ini$laterInjury="no"
ini$laterInjury[ini$BandNumber %in% b$BandNumber[injTindUn]]="yes"

table(ini$Bander,ini$laterInjury)
table(ini$Year,ini$laterInjury)
table(ini$dateCorrected)
boxplot(ini$Tarsus.num[ini$Tarsus.num>15]~ini$laterInjury[ini$Tarsus.num>15])

# Tarsus length
ind=ini$Tarsus.num>15
estim<- dabest(ini[ind,],laterInjury,Tarsus.num,idx=c("no","yes"))
pdf(paste0(out,"Tarsus lenght when equipped.pdf"), height=4, width=4)
plot(estim,rawplot.ylabel="Tasus length")
dev.off()

# weight length
ini$Weight=as.numeric(ini$Weight)
estim<- dabest(ini,laterInjury,Weight,idx=c("no","yes"))
pdf(paste0(out,"Tarsus lenght when equipped.pdf"), height=4, width=4)
plot(estim,rawplot.ylabel="Tasus length")
dev.off()



#recap=b[b$BandNumber %in% NbyID$BandNumber[NbyID$BandNumber!=0&NbyID$N>1], ]

# julian date (nest or net)
# recapture or not between first capture and injury
# nb day from 1st tag
# tarsus length
# bander id
# elevation
# site
# perso or not
# remove color tag or not


