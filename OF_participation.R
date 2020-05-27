# Compute number of individuals reaching each phase and test elevation and species effect on perseveration through phases.
# using d and di 
#------------------
# by scenario
#-------------------
# brut
#------
DT <- as.data.table(d)
DT=DT[,list(nbIndiv = length(unique(tag))), by="scenario,species"]
dvBrut=as.data.frame(DT)

# concatenate 
sc=c(1,2,30,31,32)
x=dvBrut
dtemp=data.frame(species=c("Great","Blue","Marsh"))
for (i in 1:length(sc)){
  dtemp[,i+1]=c(x$nbIndiv[x$scenario==sc[i]&x$species=="Great"],x$nbIndiv[x$scenario==sc[i]&x$species=="Blue"],
                x$nbIndiv[x$scenario==sc[i]&x$species=="Marsh"])
}
names(dtemp)=c("species","openbar","door.habitutation","ON/OFF.learning", "Left/Righ.learning","Righ/Left.reversal")

pdf(paste0(out_f,"Nb birds by scenario brut.pdf"), height=2, width=12)
grid.table(dtemp,rows =c())
dev.off()



# % same of new from 1 and 2
#---------------------------
x=d
uSpe=c("Great","Blue","Marsh")
dtemp=data.frame(species=c("Great","Blue","Marsh"))
reslist=list()
for (s in 1:length(uSpe)){
  
  hab=as.character(unique(x$tag[x$species==uSpe[s]&x$scenario<3]))
  l30=as.character(unique(x$tag[x$species==uSpe[s]&x$scenario==30]))
  l31=as.character(unique(x$tag[x$species==uSpe[s]&x$scenario==31]))
  l32=as.character(unique(x$tag[x$species==uSpe[s]&x$scenario==32]))
  restemp=data.frame(species=uSpe[s],nb.habituation=length(hab),
                     Continue.ON_OFF=length(intersect(hab,l30)),
                     New.ON_OFF=length(l30)-length(intersect(hab,l30)),
                     Continue.L_R=length(intersect(unique(c(hab,l30)),l31)),
                     New.L_R=length(l31)-length(intersect(unique(c(hab,l30)),l31)),
                     Continue.R_L=length(intersect(unique(c(hab,l30,l31)),l32)),
                     New.R_L=length(l32)-length(intersect(unique(c(hab,l30,l31)),l32)))
  
  reslist[[s]]=restemp
}
res=rbindlist(reslist)

res$perc.continue.ON_OFF=round(res$Continue.ON_OFF/res$nb.habituation*100)
res$perc.continue.L_R=round(res$Continue.L_R/res$nb.habituation*100)
res$perc.continue.R_L=round(res$Continue.R_L/res$nb.habituation*100)

pdf(paste0(out_f,"Nb birds continuing by scenario all brut.pdf"), height=3, width=20)
grid.table(res,rows =c())
dev.off()

rsmall=res[,c(1,2,9,10,11)]
names(rsmall)=c("species","nb habituation","% continue ON_OFF","% continue L_R","% continue R_R")
pdf(paste0(out_f,"Nb birds continuing by scenario all brut_restricted.pdf"), height=2, width=9)
grid.table(rsmall,rows =c())
dev.off()


# select minimal number of day and nb of trial by day
DT <- as.data.table(d)
DT=DT[,list(nbtrial = .N, species=unique(species)), by="scenario,tag,day"]
dv=as.data.frame(DT)

# take only individuals that perform at least 30 trial a day
DT <- as.data.table(dv[dv$nbtrial>30,])
DT=DT[,list(nbday =length(unique(day)), species=unique(species)), by="scenario,tag"]
dd=as.data.frame(DT)

# take only individuals that perform at least 2/3 day
indDay=(dd$nbday>6&dd$scenario==1)|(dd$nbday>14&dd$scenario %in% c(30,2))|(dd$nbday>13&dd$scenario>30)
DT <- as.data.table(dd[indDay,])
DT=DT[,list(nbIndiv =.N), by="scenario,species"]
dv10t_10d=as.data.frame(DT)



# concatenate 
sc=c(1,2,30,31,32)
x=dv10t_10d
dtemp=data.frame(species=c("Great","Blue","Marsh"))
for (i in 1:length(sc)){
  dtemp[,i+1]=c(x$nbIndiv[x$scenario==sc[i]&x$species=="Great"],x$nbIndiv[x$scenario==sc[i]&x$species=="Blue"],
                x$nbIndiv[x$scenario==sc[i]&x$species=="Marsh"])
}
names(dtemp)=c("species","openbar","door.habitutation","ON/OFF.learning", "Left/Righ.learning","Righ/Left.reversal")

pdf(paste0(out_f,"Nb birds by scenario 2third days 30trials a day.pdf"), height=1.5, width=9)
grid.table(dtemp,rows =c())
dev.off()

x=dd[indDay,]
uSpe=c("Great","Blue","Marsh")
dtemp=data.frame(species=c("Great","Blue","Marsh"))
reslist=list()
for (s in 1:length(uSpe)){
  
  hab=as.character(unique(x$tag[x$species==uSpe[s]&x$scenario<3]))
  l30=as.character(unique(x$tag[x$species==uSpe[s]&x$scenario==30]))
  l31=as.character(unique(x$tag[x$species==uSpe[s]&x$scenario==31]))
  l32=as.character(unique(x$tag[x$species==uSpe[s]&x$scenario==32]))
  restemp=data.frame(species=uSpe[s],nb.habituation=length(hab),
                     Continue.ON_OFF=length(intersect(hab,l30)),
                     New.ON_OFF=length(l30)-length(intersect(hab,l30)),
                     Continue.L_R=length(intersect(unique(c(hab,l30)),l31)),
                     New.L_R=length(l31)-length(intersect(unique(c(hab,l30)),l31)),
                     Continue.R_L=length(intersect(unique(c(hab,l30,l31)),l32)),
                     New.R_L=length(l32)-length(intersect(unique(c(hab,l30,l31)),l32)))
  
  reslist[[s]]=restemp
}
res=rbindlist(reslist)

res$perc.continue.ON_OFF=round(res$Continue.ON_OFF/res$nb.habituation*100)
res$perc.continue.L_R=round(res$Continue.L_R/res$nb.habituation*100)
res$perc.continue.R_L=round(res$Continue.R_L/res$nb.habituation*100)

pdf(paste0(out_f,"Nb birds continuing by scenario all 2thrid 30 trials.pdf"), height=3, width=20)
grid.table(res,rows =c())
dev.off()

rsmall=res[,c(1,2,9,10,11)]
names(rsmall)=c("species","nb habituation","% continue ON_OFF","% continue L_R","% continue R_R")
pdf(paste0(out_f,"Nb birds continuing by scenario all 2thrid 30 trials_restricted.pdf"), height=2, width=9)
grid.table(rsmall,rows =c())
dev.off()


#--------------------
# Depending on learning criterium
#---------------------`
# read individual data
di=read.table(paste(out_f,"alldata.csv",sep=""),h=T)# data at the individual levels computed in OF_individualStat_with learning.R


# brut
#------
DT <- as.data.table(d)
DT=DT[,list(nbIndiv = length(unique(tag))), by="scenario,species"]
dvBrut=as.data.frame(DT)

# concatenate 
sc=c(30,31,32)
x=dv10t_10d
dtemp=data.frame(species=c("Great","Blue","Marsh"))
for (i in 1:length(sc)){
  dtemp[,i+1]=c(x$nbIndiv[x$scenario==sc[i]&x$species=="Great"],x$nbIndiv[x$scenario==sc[i]&x$species=="Blue"],
                x$nbIndiv[x$scenario==sc[i]&x$species=="Marsh"])
if(sc[i]>29){
  dtemp[,i+1]=c(length(unique(di$tag[!is.na(di$TTC10)&di$scenario==sc[i]&di$species=="Great"])),
                length(unique(di$tag[!is.na(di$TTC10)&di$scenario==sc[i]&di$species=="Blue"])),
                length(unique(di$tag[!is.na(di$TTC10)&di$scenario==sc[i]&di$species=="Marsh"])))
}
  }
names(dtemp)=c("Species","ON/OFF.learning", "Left/Righ.learning","Righ/Left.reversal")

pdf(paste0(out_f,"Nb birds learning.pdf"), height=1.5, width=6)
grid.table(dtemp,rows =c())
dev.off()

# bird performing same mean number of  trials in each 
#-------------------------------------------------
hab=di[di$scenario==2,c("tag","species","site","age","sex","nbtm")]
on=di[di$scenario==30,c("tag","nbtm")]
left=di[di$scenario==31,c("tag","nbtm")]
right=di[di$scenario==32,c("tag","nbtm")]

cont=merge(hab,on,by="tag",all.x=T)
cont=merge(cont,left,by="tag",all.x=T)
names(cont)=c("tag","species","site","age","sex","hab","on","left")
cont=merge(cont,right,by="tag",all.x=T)
sum(cont$on>=(cont$hab-5),na.rm=T)
sum(cont$left>=cont$hab,na.rm=T)
sum(cont$nbtm>=cont$hab,na.rm=T)
#sum(cont$on>=cont$hab&cont$species=="Great",na.rm=T)

# # % same of new from 1 and 2
# #---------------------------
# x=di
# uSpe=c("Great","Blue","Marsh")
# dtemp=data.frame(species=c("Great","Blue","Marsh"))
# reslist=list()
# for (s in 1:length(uSpe)){
#   
#   hab=as.character(unique(x$tag[x$species==uSpe[s]&x$scenario<3]))
#   l30=as.character(unique(x$tag[x$species==uSpe[s]&x$scenario==30]))
#   l31=as.character(unique(x$tag[x$species==uSpe[s]&x$scenario==31]))
#   l32=as.character(unique(x$tag[x$species==uSpe[s]&x$scenario==32]))
#   restemp=data.frame(species=uSpe[s],nb.habituation=length(hab),
#                      Continue.ON_OFF=length(intersect(hab,l30)),
#                      New.ON_OFF=length(l30)-length(intersect(hab,l30)),
#                      Continue.L_R=length(intersect(unique(c(hab,l30)),l31)),
#                      New.L_R=length(l31)-length(intersect(unique(c(hab,l30)),l31)),
#                      Continue.R_L=length(intersect(unique(c(hab,l30,l31)),l32)),
#                      New.R_L=length(l32)-length(intersect(unique(c(hab,l30,l31)),l32)))
#   
#   reslist[[s]]=restemp
# }
# res=rbindlist(reslist)
# 
# res$perc.continue.ON_OFF=round(res$Continue.ON_OFF/res$nb.habituation*100)
# res$perc.continue.L_R=round(res$Continue.L_R/res$nb.habituation*100)
# res$perc.continue.R_L=round(res$Continue.R_L/res$nb.habituation*100)
# 
# pdf(paste0(out_f,"Nb birds continuing by scenario all brut.pdf"), height=3, width=20)
# grid.table(res,rows =c())
# dev.off()
# 
# rsmall=res[,c(1,2,9,10,11)]
# names(rsmall)=c("species","nb habituation","% continue ON_OFF","% continue L_R","% continue R_R")
# pdf(paste0(out_f,"Nb birds continuing by scenario all brut_restricted.pdf"), height=2, width=9)
# grid.table(rsmall,rows =c())
# dev.off()

