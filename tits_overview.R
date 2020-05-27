# Summarise all dataset and there overlap:
# - Fitness
# - Banding
# - Cognition wild
# - Eli Feeder
# - Cognition aviary
# - ornement
# - insect
# - predation
# - plumes
# -
#-------------------
# Pakages
#-------------------

library(knitr)
library(rmarkdown)
library(markdown)
library(data.table)
library("gridExtra")
library("dabestr")


#-------------------
#     Folder
#-------------------

out="/Users/maximecauchoix/Documents/wild_cog_OF/results/overview/"
dir.create(out)

out_local='~/Documents/openfeeder/data/'
#-------------------
#     Fitness
#-------------------
source('~/Documents/wild_cog_OF/Rscripts/tits_overview_fitness.R')
#-------------------
#     OF Cognition
#-------------------
source('~/Documents/wild_cog_OF/Rscripts/tits_overview_OFcognition.R')
#-------------------
#     Banding
#-------------------
#source('~/Documents/wild_cog_OF/Rscripts/band_clean_2018_19.R')
source('~/Documents/wild_cog_OF/Rscripts/OF_cleanBanding_2020.R')
#-------------------
#     Injury
#-------------------
#-------------------
#     Summary table
#-------------------

# fall alone
#-----------
# Nb individual all
DT <- as.data.table(b)
DT=DT[ind,list(N.ind=length(unique(BandNumber))), by="Year"]
dyi=as.data.frame(DT)
dy=dyi[order(dyi$Year),]

spe=c("Blue","Great","Marsh")
for (i in 1:length(spe))
{
  DT <- as.data.table(b)
  DT=DT[ind&b$Species==spe[i],list(N=length(unique(BandNumber))), by="Year"]
  dyi=as.data.frame(DT)
  dyi=dyi[order(dyi$Year),]
  dy[[spe[i]]]=round((dyi$N/dy$N.ind)*100)
}

# plot
#-----
pdf(paste0(out_local,"Banding_Prop species by year .pdf"), height=5, width=6)
x <- barplot(t(as.matrix(dy[,c(3:5)])), col=c("blue","gold","brown"), 
              border=NA, xlim=c(0,8),names.arg=dy$Year, 
             ylab="%", xlab="Annee", col.axis = "White",col.lab="White")

text(x, dy$Blue-10, labels=round(dy$Blue), col="black")
text(x, dy$Blue+10, labels=round(dy$Great), col="black")
text(x, dy$Blue+dy$Great+2, labels=round(dy$Marsh), col="black")
dev.off()



# nb banded in fall each year
#----------------------------
# take only study sites and good species
ind=is.na(b$Nest.Number)&
  b$Site %in% c("M1","C1","C4","BA","H4","H1","L1")&b$Year>2012&
  b$Species %in% c("Blue","Great","Marsh")

# by year all capture
DT <- as.data.table(b)
DT=DT[ind,list(nb.capture.fall =.N), by="Year"]
dy=as.data.frame(DT)
dy=dy[order(dy$Year),]

# by year all capture
DT <- as.data.table(b)
DT=DT[ind,list(nb.individuals.fall=length(unique(BandNumber))), by="Year"]
dyi=as.data.frame(DT)
dyi=dyi[order(dyi$Year),]

dy$nb.individuals.fall=dyi$nb.individuals.fall

# nb nesting each year
#----------------------------

# nb nest occupied nesting
DT <- as.data.table(fi)
DT=DT[fi$eggORnot==1,list(n =.N), by="Year"]
dyi=as.data.frame(DT)
dyi=dyi[order(dyi$Year),]


dy$nb.nest.occupied.total=dyi$n[2:7]

# nb individuals nesting
DT <- as.data.table(fiB)
DT=DT[,list(n =.N), by="Year"]
dyi=as.data.frame(DT)
dyi=dyi[order(dyi$Year),]

dy$nb.nesting.individuals.total=dyi$n[2:7]

# overal fall banding and nesting
#----------------------------

# capture in fall and nesting the year before
n=1
for (y in 2013:2018){
  dy$nb.capture.in.fall.nesting.previous.spring[n]=length(intersect(unique(b$BandNumber[b$Year==y&is.na(b$Nest.Number)]),fiB$bandNumber[fiB$Year==y]))
  n=n+1
}


# capture in fall and nesting the year before
n=1
for (y in 2013:2018){
  dy$nb.capture.in.fall.nesting.next.spring[n]=length(intersect(unique(b$BandNumber[b$Year==y&is.na(b$Nest.Number)]),fiB$bandNumber[fiB$Year==y+1]))
  n=n+1
}


write.csv2(dy,paste0(out_local,"fall_spring_match.csv"),row.names = F)



# overal cogniton and fitness
#----------------------------
cf=data.table()
cf$species="All"

cf$nb.banded=length(unique(b$BandNumber[b$Year==2018&is.na(b$Nest.Number)]))
cf$nb.recordedOF=length(unique(coR$bandNumber))
cf$nb.cognition=length(unique(coco$bandNumber))
cf$nb.fitness.2018=length(unique(fiB$bandNumber[fiB$Year==2018]))
cf$nb.fitness.2019=length(unique(fiB$bandNumber[fiB$Year==2019]))


#cf$nb.ind.banded.recordedOF=length(intersect(unique(b$BandNumber[b$Year==2018&is.na(b$Nest.Number)]),coR$bandNumber))
#cf$nb.ind.banded.with.cognition=length(intersect(unique(b$BandNumber[b$Year==2018&is.na(b$Nest.Number)]),coco$bandNumber))
cf$nb.ind.cognition.fitness.2018=length(intersect(fiB$bandNumber[fiB$Year==2018],coco$bandNumber))
cf$nb.ind.cognition.fitness.2019=length(intersect(fiB$bandNumber[fiB$Year==2019],coco$bandNumber))

spe=unique(co$species)[c(3,1,2)]

for (i in 1:length(spe)){
  c=data.table()
  
  c$species=spe[i]
  
  c$nb.banded=length(unique(b$BandNumber[b$Year==2018&is.na(b$Nest.Number)&b$Species==as.character(spe[i])]))
  c$nb.recordedOF=length(unique(coR$bandNumber[coR$species==as.character(spe[i])]))
  c$nb.cognition=length(unique(coco$bandNumber[coco$species==as.character(spe[i])]))
  
  
  c$nb.fitness.2018=length(unique(fiB$bandNumber[fiB$Year==2018&fiB$Species==as.character(spe[i])]))
  c$nb.fitness.2019=length(unique(fiB$bandNumber[fiB$Year==2019&fiB$Species==as.character(spe[i])]))
  
  
  #c$nb.ind.banded.recordedOF=length(intersect(unique(b$BandNumber[b$Year==2018&is.na(b$Nest.Number)]),coR$bandNumber))
  #c$nb.ind.banded.with.cognition=length(intersect(unique(b$BandNumber[b$Year==2018&is.na(b$Nest.Number)]),coco$bandNumber))
  c$nb.ind.cognition.fitness.2018=length(intersect(fiB$bandNumber[fiB$Year==2018],coco$bandNumber[coco$species==as.character(spe[i])]))
  c$nb.ind.cognition.fitness.2019=length(intersect(fiB$bandNumber[fiB$Year==2019],coco$bandNumber[coco$species==as.character(spe[i])]))
  
  cf=rbind(cf,c)
  
}



write.csv2(cf,paste0(out_local,"fall_cognition_spring_match_2018-2019.csv"),row.names = F)




DT <- as.data.table(b)
DT=DT[ind,list(nbt =.N), by="Year,environement"]
desp=as.data.frame(DT)

desp=desp[order(desp$Year),]

#normalise
di$logTTC10=log(di$TTC10)
di$logTTC20=log(di$TTC20)
di$logTTC30=log(di$TTC30)
di$lognbts=log(di$nbts)
# loop on all variables
varnames=names(di)[c(13:37,39:45)+1]# +1 because I added bandNumber to check



print(paste("Nb of bird in fitness database",length(unique(fiB$bandNumber))))
print(paste(sum(unique(di$bandNumber) %in% unique(fiB$bandNumber)),"with fitness data"))

print(paste(sum(unique(di$bandNumber[di$species=="Great"]) %in% unique(fiB$bandNumber)),"Great with fitness data"))
print(paste(sum(unique(di$bandNumber[di$species=="Blue"]) %in% unique(fiB$bandNumber)),"Blue with fitness data"))

# mean by birds
DT <- as.data.table(fiB)
DT=DT[,list(nbY =.N,firstYear=min(Year),lastYear=max(Year),
            Species=unique(Species),sex=unique(sex),
            FledgeNb=mean(FledgeNb,na.rm=T),
            EggNb=mean(EggNb,na.rm=T)), by="bandNumber"]
f=as.data.frame(DT)



# merge cog and fit
all=merge(di,f,by="bandNumber",all.x=F)

# new variables
all$learn="Learning"
all$learn[is.na(all$TTC10)]="Nolearning"

# models
#-------
# trial effect of all cognitive variables
#----------------------------------------

stat=list()
for (r in 1:length(varnames)){
  ind=all$species=="Blue"&all$scenario=="30"
  
  formF <- as.formula(paste0("FledgeNb~",varnames[r]))
  formE <- as.formula(paste0("EggNb~",varnames[r]))
  
  
  e=summary(lm(formF,data=all,subset=ind))
  f=summary(lm(formE,data=all,subset=ind))
  #store
  statTemp=data.frame(variable=varnames[r],
                      pvalFledglings=round(f$coefficients[2,4],2),
                      pvalEggs=round(e$coefficients[2,4],2))
  stat[[r]]=statTemp
}

statFinal=rbindlist(stat)




# exploration plots
#--------------------
# Nb fledgings learning or not
ind=all$species=="Blue"&all$scenario=="30"

estim<- dabest(
  all[!is.na(all$FledgeNb)&ind,],learn,FledgeNb,idx=c("Learning","Nolearning"),
  paired = FALSE
)
pdf(paste0(out_fit,"NbFledge_Learning Or not_blueTits_30.pdf"),height = 4,width = 4)
plot(estim,rawplot.ylabel="Nb fledging")
dev.off()


# Plot NbFledge_AccFi100
pdf(paste0(out_fit,"NbFledge_AccFi100_blueTits_30.pdf"))
ggplot(subset(all,ind), aes(y=FledgeNb, x=AccFi100)) + 
  geom_point(aes(colour=sex.y),size=6) +  stat_smooth(method="lm", formula=y~x^2,aes(colour=sex.y))+  theme_bw()+
  theme(text = element_text(size=18))+
  ggtitle("Learning ON/OFF") +
  ylab("Nb fledglings") + xlab("Accuracy last 100 trials")
dev.off() 

# Plot AccPost50_10
pdf(paste0(out_fit,"NbFledge_AccPost50_10_blueTits_30.pdf"))
ggplot(subset(all,ind), aes(y=FledgeNb, x=AccPre50_10)) + 
  geom_point(aes(colour=sex.y),size=6) +  stat_smooth(method="lm", formula=y~x^2)+  theme_bw()+
  theme(text = element_text(size=18))+
  ggtitle("Learning ON/OFF") +
  ylab("Nb fledglings") + xlab("Accuracy post TTC")
dev.off() 


# Plot AccFi30 nb egg
pdf(paste0(out_fit,"NbEgg_AccFi30_blueTits_30.pdf"))
ggplot(subset(all,ind), aes(y=EggNb, x=AccFi30)) + 
  geom_point(aes(colour=sex.y),size=6) +  stat_smooth(method="lm", formula=y~x^2)+  theme_bw()+
  theme(text = element_text(size=18))+
  ggtitle("Learning ON/OFF") +
  ylab("Nb eggs") + xlab("AccFi30")
dev.off() 


#-------------------
# Winter nestcheck
#-------------------
wn=read.csv2(paste0(out_local,'winter_nest_check_2020.csv'))
ni=read.csv2(paste0(out_local,'NestBoxInformation.csv'))
ni$nest=toupper(substr(ni$name,1,4))
wn$nichoir=toupper(wn$nichoir)
diff=as.character(setdiff(ni$nest,wn$nichoir))
diffCast=diff[grep('H',diff)]
length(grep('H',ni$nest))
length(grep('H',wn$nichoir))
################################################################################
#-------------------
# Winter 2020 color combo for corelation with dominance !!!! NOW IN developped in DOM_main_2020.R
#------------------
# find last morpho info for each color for each site

# code color combo site
#---------------------
#from nest banding
b$SiteCorrect="Not a study site"
b$SiteCorrect[grep("H",b$Nest.Number)]="Castera"
b$SiteCorrect[grep("C",b$Nest.Number)]="Cescau"
b$SiteCorrect[grep("B",b$Nest.Number)]="Balacet"
b$SiteCorrect[grep("L",b$Nest.Number)]="Moulis" # same color combo
b$SiteCorrect[grep("M",b$Nest.Number)]="Moulis"
b$SiteCorrect[grep("G",b$Nest.Number)]="Galey"
#from winter banding
b$SiteCorrect[b$Site %in% c('C1','C4','C3','C5','C2','Castillon')]="Cescau"
b$SiteCorrect[b$Site %in% c('M1','L1','Aubert','Montegut','Ledar','AU')]="Moulis"
b$SiteCorrect[b$Site %in% c('BA')]="Balacet"

uSite=c("Cescau","Moulis","Balacet")
uSpe=c("Blue","Great","Marsh")
bcol=b[1,]
for (i in 1:length(uSite)){# loop on site
  for (j in 1:length(uSpe)){ # loop on species
    ind=b$SiteCorrect==uSite[i]&b$Species==uSpe[j]
    uCol=unique(b$Color[ind]) # unique color combo
    print(paste(uSite[i], uSpe[j]))
    
    for (k in 1:length(uCol)){
      indCol=ind&b$Color==uCol[k]
      indColWing=indCol&!is.na(b$Wing.chord.num) # last morpho recorded
      if (sum(indColWing,na.rm=T)>0 ) {
        indCol=indColWing
      }
      
      lastCaptureDate=max(b$dateCorrected[which(indCol)])
      goodLine=b[which(indCol&b$dateCorrected==lastCaptureDate),]
        
      bcol=rbind(bcol,goodLine)
      rm(indCol)
    }
    rm(ind)
  }

}

bcol=bcol[2:dim(bcol)[1],c("BandNumber","Color","RFID.","Species","SiteCorrect","dateCorrected",
                        "Sex","Age","Wing.chord.num","Tarsus.num","Head.num","Photo","TagModif")]
bcol$Age[bcol$dateCorrected<"2019-03-01"]="+1a"
#------------------
# merge with patch
#------------------

patch=read.csv2(paste0(out_local,"Mesures patchs_2019.csv"),h=T,sep=";",
            na.strings=c("?","","NA"))
patch$BandNumber=patch$Band.number
patch=patch[,c("BandNumber","Patch_head","Patch_cheek","Patch_tie")]

bcolpatch=merge(bcol,patch,by="BandNumber",sort=F,all.x=T)


# save
write.csv2(bcolpatch,paste0(out_local,"Color_banding.csv"),row.names = F)
