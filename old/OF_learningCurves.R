# plot learning curve: ACC by day, nb trial, sliding acc
#-------------------------------------------------------
#To do
#-----
# code day xlim by site
# extract TTC, DTC, ACC after TTC, fit by exp


#d=d[d$site_folder=="GJ",]
# 1_Learning by day: site level
#-------------------------------------
DT <- as.data.table(d[d$scenario>29,])# change at the end of the experiment
DT=DT[,list(ACC =mean(door.open),nbtrial = .N,species=unique(species),elevation=unique(elevation)), by="day,site_folder,scenario,tag"]
DTI=DT[,list(ACC =mean(ACC),esACC=sd(ACC)/length(unique(tag))*1.96,nbtrial =mean(nbtrial),esNBT=sd(nbtrial)/length(unique(tag))*1.96), by="day,site_folder,scenario"]
DTS=DT[,list(ACC =mean(ACC),esACC=sd(ACC)/length(unique(tag))*1.96,nbtrial =mean(nbtrial),esNBT=sd(nbtrial)/length(unique(tag))*1.96), by="day,site_folder,species,scenario"]
DTA=DT[,list(ACC =mean(ACC),site=unique(site_folder),species=unique(species),elevation=unique(elevation),scenario=unique(scenario),nbtrial =mean(nbtrial)), by="day,tag"]

dt=as.data.frame(DT)
dti=as.data.frame(DTI)
dts=as.data.frame(DTS)
dta=as.data.frame(DTA)

dt$dayDate=as.Date(dt$day,"%d/%m/%y")
dti$dayDate=as.Date(dti$day,"%d/%m/%y")
dts$dayDate=as.Date(dts$day,"%d/%m/%y")
dta$dayDate=as.Date(dta$day,"%d/%m/%y")

#----------------------
# all sites: population 
#-----------------------
# code day as number from beginning of the scenario
uSc=unique(dta$scenario)
uSite=unique(dta$site)
for (i in 1:length(uSc)){
  print(uSc[i])
  for (s in 1:length(uSite)){
  ind=dta$site==uSite[s]&dta$scenario==uSc[i]
  dta$daynumber[ind]=dta$dayDate[ind]-min(dta$dayDate[ind])
  }
}
# compute SE
DT <- as.data.table(dta)# change at the end of the experiment
DTAS=DT[,list(ACC =mean(ACC),esACC=sd(ACC)/length(unique(tag))*1.96,nbtrial =mean(nbtrial),esNBT=sd(nbtrial)/length(unique(tag))*1.96), by="daynumber,species,scenario"]
DTASI=DT[,list(ACC =mean(ACC),esACC=sd(ACC)/length(unique(tag))*1.96,nbtrial =mean(nbtrial),esNBT=sd(nbtrial)/length(unique(tag))*1.96), by="daynumber,elevation,scenario"]

DTAA=DT[,list(ACC =mean(ACC),esACC=sd(ACC)/length(unique(tag))*1.96,nbtrial =mean(nbtrial),esNBT=sd(nbtrial)/length(unique(tag))*1.96), by="daynumber,scenario"]

dtasi=as.data.frame(DTASI)
dtas=as.data.frame(DTAS)
dtaa=as.data.frame(DTAA)


# ALL BIRDS
#----------

pdf(paste0(out_f,"Learning population.pdf"), height=4, width=8)
# nb graph
par(mfrow=c(1,3),mar=c(10, 8, 4, 2)/2)

# ON_OFF
ind=dtaa$scenario==30
plot(dtaa$daynumber[ind],dtaa$ACC[ind],xlab = "Daynumber",ylab = 'Accuracy',ylim = c(0.1,0.8),main = "Learning ON/OFF")
arrows(dtaa$daynumber[ind],dtaa$ACC[ind]-dtaa$esACC[ind],dtaa$daynumber[ind],dtaa$ACC[ind]+dtaa$esACC[ind], length=0.05, angle=90, code=3)
abline(h=0.25, col = "gray60",lty=2)

ind=dtaa$scenario==31
plot(dtaa$daynumber[ind],dtaa$ACC[ind],xlab = "Daynumber",ylab = '',ylim = c(0.1,0.8),xlim=c(1,14),main = "Learning RIGHT/LEFT")
arrows(dtaa$daynumber[ind],dtaa$ACC[ind]-dtaa$esACC[ind],dtaa$daynumber[ind],dtaa$ACC[ind]+dtaa$esACC[ind], length=0.05, angle=90, code=3)
abline(h=0.50, col = "gray60",lty=2)

ind=dtaa$scenario==32
plot(dtaa$daynumber[ind],dtaa$ACC[ind],xlab = "Daynumber",ylab = '',ylim = c(0.1,0.8),xlim=c(1,14),main = "Reversal LEFT/RIGHT")
arrows(dtaa$daynumber[ind],dtaa$ACC[ind]-dtaa$esACC[ind],dtaa$daynumber[ind],dtaa$ACC[ind]+dtaa$esACC[ind], length=0.05, angle=90, code=3)
abline(h=0.50, col = "gray60",lty=2)

dev.off()

# By SPECIES
#----------
p30=ggplot(subset(dtas,scenario==30), aes(x = daynumber, y = ACC)) +
  geom_line(aes(colour=species)) + 
  geom_pointrange(aes(ymin = ACC - esACC,
                      ymax = ACC + esACC,colour=species))+
  xlim(1,30)+
  coord_cartesian(ylim =c(0.1,0.6))+
  scale_color_manual(values=c("blue","gold","brown"))+
  theme_bw()+geom_hline(yintercept=0.25, linetype="dashed", color = "black")+
  theme(legend.position = c(0.15, 0.7),text = element_text(size=18))+
  ggtitle("Learning ON/OFF") +
  xlab("Daynumber") + ylab("Accuracy")

p31=ggplot(subset(dtas,scenario==31), aes(x = daynumber, y = ACC)) +
  geom_line(aes(colour=species)) + 
  geom_pointrange(aes(ymin = ACC - esACC,
                      ymax = ACC + esACC,colour=species))+
  xlim(1,14)+
  coord_cartesian(ylim =c(0.3,0.7))+
  scale_color_manual(values=c("blue","gold","brown"))+
  theme_bw()+geom_hline(yintercept=0.5, linetype="dashed", color = "black")+
  theme(legend.position = "none",text = element_text(size=18))+
  ggtitle("Learning Right/Left") +
  xlab("Daynumber") + ylab("")

p32=ggplot(subset(dtas,scenario==32), aes(x = daynumber, y = ACC)) +
  geom_line(aes(colour=species)) + 
  geom_pointrange(aes(ymin = ACC - esACC,
                      ymax = ACC + esACC,colour=species))+
  xlim(1,14)+
  coord_cartesian(ylim =c(0.3,0.7))+
  scale_color_manual(values=c("blue","gold","brown"))+
  theme_bw()+geom_hline(yintercept=0.5, linetype="dashed", color = "black")+
  theme(legend.position = "none",text = element_text(size=18))+
  ggtitle("Reversal Right/Left") +
  xlab("Daynumber") + ylab("")



# save graph
pdf(paste0(out_f,"Learning population by spe.pdf"), height=8, width=14)
grid.arrange(p30,p31,p32, nrow=1, ncol=3)
dev.off()

# BY ELEVATION
#-------------
p30=ggplot(subset(dtasi,scenario==30), aes(x = daynumber, y = ACC)) +
  geom_line(aes(colour=elevation)) + 
  geom_pointrange(aes(ymin = ACC - esACC,
                      ymax = ACC + esACC,colour=elevation))+
  xlim(1,30)+
  coord_cartesian(ylim =c(0.1,0.6))+
  scale_color_manual(values=c("blue","gold","brown"))+
  theme_bw()+geom_hline(yintercept=0.25, linetype="dashed", color = "black")+
  theme(legend.position = c(0.15, 0.7),text = element_text(size=18))+
  ggtitle("Learning ON/OFF") +
  xlab("Daynumber") + ylab("Accuracy")

p31=ggplot(subset(dtasi,scenario==31), aes(x = daynumber, y = ACC)) +
  geom_line(aes(colour=elevation)) + 
  geom_pointrange(aes(ymin = ACC - esACC,
                      ymax = ACC + esACC,colour=elevation))+
  xlim(1,14)+
  coord_cartesian(ylim =c(0.3,0.7))+
  scale_color_manual(values=c("blue","gold","brown"))+
  theme_bw()+geom_hline(yintercept=0.5, linetype="dashed", color = "black")+
  theme(legend.position = "none",text = element_text(size=18))+
  ggtitle("Learning Right/Left") +
  xlab("Daynumber") + ylab("")

p32=ggplot(subset(dtasi,scenario==32), aes(x = daynumber, y = ACC)) +
  geom_line(aes(colour=elevation)) + 
  geom_pointrange(aes(ymin = ACC - esACC,
                      ymax = ACC + esACC,colour=elevation))+
  xlim(1,14)+
  coord_cartesian(ylim =c(0.3,0.7))+
  scale_color_manual(values=c("blue","gold","brown"))+
  theme_bw()+geom_hline(yintercept=0.5, linetype="dashed", color = "black")+
  theme(legend.position = "none",text = element_text(size=18))+
  ggtitle("Reversal Right/Left") +
  xlab("Daynumber") + ylab("")



# save graph
pdf(paste0(out_f,"Learning population by elevation.pdf"), height=8, width=14)
grid.arrange(p30,p31,p32, nrow=1, ncol=3)
dev.off()




# by study site
#--------------
uSite=unique(dt$site_folder)
for (s in 1:length(uSite)) {
  pdf(paste0(out_f,"Learning sit_",uSite[s],".pdf"), height=10, width=7)
  # nb graph
  par(mfrow=c(3,2),mar=c(5, 8, 4, 2)/2)
  if (sum(dti$scenario==32&dti$site_folder==uSite[s])>1) {par(mfrow=c(3,3),mar=c(5, 8, 4, 2)/2)} 
  
  # ACC All individuals
  ind=dti$scenario==30&dti$site_folder==uSite[s]
  plot(dti$dayDate[ind],dti$ACC[ind],xlab = "Day",ylab = 'Accuracy',ylim = c(0.1,0.8))
  arrows(dti$dayDate[ind],dti$ACC[ind]-dti$esACC[ind],dti$dayDate[ind],dti$ACC[ind]+dti$esACC[ind], length=0.05, angle=90, code=3)
  abline(h=0.25, col = "gray60",lty=2)
  
  ind=dti$scenario==31&dti$site_folder==uSite[s]
  if(sum(ind)>1){
  plot(dti$dayDate[ind],dti$ACC[ind],xlab = "Day",ylab = '',ylim = c(0.2,0.8))
  arrows(dti$dayDate[ind],dti$ACC[ind]-dti$esACC[ind],dti$dayDate[ind],dti$ACC[ind]+dti$esACC[ind], length=0.05, angle=90, code=3)
  abline(h=0.5, col = "gray60",lty=2)
  }
  else 
  {
    plot(1,1)
    text(1,1,paste("No data"))
  }
  
  ind=dti$scenario==32&dti$site_folder==uSite[s]
  if(sum(ind)>1){
    plot(dti$dayDate[ind],dti$ACC[ind],xlab = "Day",ylab = '',ylim = c(0.2,0.8))
    arrows(dti$dayDate[ind],dti$ACC[ind]-dti$esACC[ind],dti$dayDate[ind],dti$ACC[ind]+dti$esACC[ind], length=0.05, angle=90, code=3)
    abline(h=0.5, col = "gray60",lty=2)
  }

  # NBT All individuals
  ind=dti$scenario==30&dti$site_folder==uSite[s]
  plot(dti$dayDate[ind],dti$nbtrial[ind],xlab = "Day",ylab = 'Nb trial',ylim = c(20,150))
  arrows(dti$dayDate[ind],dti$nbtrial[ind]-dti$esNBT[ind],dti$dayDate[ind],dti$nbtrial[ind]+dti$esNBT[ind], length=0.05, angle=90, code=3)

  ind=dti$scenario==31&dti$site_folder==uSite[s]
  if(sum(ind)>1){
  plot(dti$dayDate[ind],dti$nbtrial[ind],xlab = "Day",ylab = '',ylim = c(20,150))
  arrows(dti$dayDate[ind],dti$nbtrial[ind]-dti$esNBT[ind],dti$dayDate[ind],dti$nbtrial[ind]+dti$esNBT[ind], length=0.05, angle=90, code=3)
  }
  else 
  {
    plot(1,1)
    text(1,1,paste("No data"))
  }
  
  ind=dti$scenario==32&dti$site_folder==uSite[s]
  if(sum(ind)>1){
    plot(dti$dayDate[ind],dti$nbtrial[ind],xlab = "Day",ylab = '',ylim = c(20,150))
    arrows(dti$dayDate[ind],dti$nbtrial[ind]-dti$esNBT[ind],dti$dayDate[ind],dti$nbtrial[ind]+dti$esNBT[ind], length=0.05, angle=90, code=3)
  }
  
  # ACC by species
  ind=dts$scenario==30&dts$site_folder==uSite[s]
  plot(dts$dayDate[ind],dts$ACC[ind],xlab = "Day",ylab = 'Accuracy',ylim = c(0.1,0.8),col=as.factor(dts$species))
  abline(h=0.25, col = "gray60",lty=2)
  legend(min(dts$dayDate[ind]),0.3,legend = levels(as.factor(dts$species)),col=1:length(dts$species),pch=19,cex=1,y.intersp=0.8,bty="n")
  

  ind=dts$scenario==31&dts$site_folder==uSite[s]
  if(sum(ind)>1){
  plot(dts$dayDate[ind],dts$ACC[ind],xlab = "Day",ylab = '',ylim = c(0.2,0.8),col=as.factor(dts$species))
  abline(h=0.5, col = "gray60",lty=2)
  }
  else 
  {
    plot(1,1)
    text(1,1,paste("No data"))
  }
  
  ind=dts$scenario==32&dts$site_folder==uSite[s]
  if(sum(ind)>1){
    plot(dts$dayDate[ind],dts$ACC[ind],xlab = "Day",ylab = '',ylim = c(0.2,0.8),col=as.factor(dts$species))
    abline(h=0.5, col = "gray60",lty=2)
  }

  dev.off()
}

# 2_Learning individual level:  slidding window, by day,nb trial (ADD STATS)
#------------------------------------------------------

# Pre process Mean acc and nbt by day by individuals
#--------------------------------------------------
DT <- as.data.table(d[d$scenario>29,])
DT=DT[,list(ACC =mean(door.open),nbtrial = .N,species=unique(species),sex=unique(sex),age=unique(age),site=unique(site_folder)), by="day,scenario,tag"]
dt=as.data.frame(DT)
dt$dayDate=as.Date(dt$day,"%d/%m/%y")

# Pre-process sliding window
#--------------------------

# Select go-no go all (s=30)
d30=d[d$scenario>29,]

# deal with time
d30$fullTime=paste(d30$day,d30$hour)
time=strptime(d30$fullTime,"%d/%m/%y %H:%M:%S")
indOrder=order(time)
d30=d30[indOrder,]

#print( paste('First day of experiment:', min(strptime(d30$day,"%d/%m/%y"))))
#print( paste('Last day of experiment:', max(strptime(d30$day,"%d/%m/%y"))))
# By individual collect wrong answer according to time
uTag=unique(d30$tag)
wind=10

# Loop over individuals [ADD DAY ON SLIDING]
#----------------------
dataList=list();TTC=c();DTC=c();ACCpostTTC=c();ACCpostDTC=c()

for (i in 1:length(uTag)){
  iInd=d30$tag==uTag[i]
  name=paste(unique(d30$site_folder[iInd]),unique(d30$species[iInd]),unique(d30$sex[iInd]),unique(d30$age[iInd]),uTag[i],sum(iInd),'trials')
  print(name)
  pdf(paste0(out_lc,name,".pdf"), height=10, width=7)
  # nb graph
  par(mfrow=c(3,2),mar=c(5, 8, 4, 2)/2)
  if (sum(iInd&d30$scenario==32)>1) {par(mfrow=c(3,3),mar=c(5, 8, 4, 2)/2)} 
  
  #1-sliding window go-nogo
  #--------------------
  if (sum(iInd&d30$scenario==30)>wind){
    # total number of trial
    print(paste(sum(iInd&d30$scenario==30)," trials in go no go"))
    lc=slideFunct(d30$door.open[iInd&d30$scenario==30],wind,1)
    plot(lc,type='l',ylab = 'Accuracy',xlab = 'Trial',ylim=c(0,1))
    abline(h=0.25, col = "gray60",lty=2)
    abline(h=0.43,col='red')
    # first time reach criterium
    if (sum(lc>8/10)>0) {
      abline(v=min(which(lc>8/10)),col='green')
    }
    #name=paste(unique(d30$site_folder[iInd]),unique(d30$species[iInd]),unique(d30$sex[iInd]),unique(d30$age[iInd]),uTag[i],sum(iInd&d30$scenario=="30"),'trials')
    title(paste(sum(iInd&d30$scenario==30)," trials in ON/OFF"))
  }
  else 
  {
    plot(1,1)
    text(1,1,paste("Less than",wind,"trials"))
  }
  
  #2-sliding window L/R
  #--------------------
  if (sum(iInd&d30$scenario==31)>wind){
    # total number of trial
    print(paste("L/R ", sum(iInd&d30$scenario==31)," trials"))
    lc=slideFunct(d30$door.open[iInd&d30$scenario==31],wind,1)
    plot(lc,type='l',ylab = '',xlab = 'Trial',ylim=c(0,1))
    abline(h=0.5, col = "gray60",lty=2)
    abline(h=0.8,col='red')
    # first time reach criterium
    if (sum(lc>9/10)>0) {
      abline(v=min(which(lc>9/10)),col='green')
    }
    title(paste(sum(iInd&d30$scenario==31)," trials in L/R"))
  }
  else 
  {
    plot(1,1)
    text(1,1,paste("Less than",wind,"trials"))
  }
  
  #3-sliding window R/L
  #--------------------
  indScen=iInd&d30$scenario==32
  if (sum(indScen)>wind){
    # total number of trial
    print(paste("L/R ", sum(indScen)," trials"))
    lc=slideFunct(d30$door.open[indScen],wind,1)
    plot(lc,type='l',ylab = '',xlab = 'Trial',ylim=c(0,1))
    abline(h=0.5, col = "gray60",lty=2)
    abline(h=0.8,col='red')
    # first time reach criterium
    if (sum(lc>9/10)>0) {
      abline(v=min(which(lc>9/10)),col='green')
    }
    title(paste(sum(indScen)," trials in R/L"))
  }

  
  
  #4-Acc by day go-nogo
  #--------------------
indSen=dt$tag==uTag[i]&dt$scenario==30
  # stats
  if (sum(indSen)>1){
    error=dt$ACC[indSen]*dt$nbtrial[indSen]
    correct=(1-dt$ACC[indSen])*dt$nbtrial[indSen]
    pval=c()
    for (z in 1:length(error)){
      x=chisq.test(rbind(c(error[z],correct[z]),c((error[z]+correct[z])/(3/4),(error[z]+correct[z])/(1/4))))
      pval[z]=x$p.value
    }
    sig=as.factor(pval<0.01)
    plot(dt$dayDate[indSen],dt$ACC[indSen],xlab = "",ylab = 'Daily Accuracy',pch=19,xlim = c(min(dt$dayDate[dt$scenario==30&dt$site==unique(d30$site_folder[iInd])]),max(dt$dayDate[dt$scenario==30&dt$site==unique(d30$site_folder[iInd])])), col=sig,main='Accuracy daily',ylim=c(0,1))
    abline(h=0.25, col = "gray60",lty=2)
  }
  else
  {
    plot(1,1)
    text(1,1,paste("Less than",wind,"trials"))
  }
  #5-Acc by day window L/R
  #--------------------
  indSen=dt$tag==uTag[i]&dt$scenario==31
  # stats
  if (sum(indSen)>1){
    error=dt$ACC[indSen]*dt$nbtrial[indSen]
    correct=(1-dt$ACC[indSen])*dt$nbtrial[indSen]
    pval=c()
    for (z in 1:length(error)){
      x=chisq.test(rbind(c(error[z],correct[z]),c((error[z]+correct[z])/(1/2),(error[z]+correct[z])/(1/2))))
      pval[z]=x$p.value
    }
    sig=as.factor(pval<0.01)
    # if all days are significant
    if (sum(pval<0.01)==length(pval)){
      sig='red'
    }
    plot(dt$dayDate[indSen],dt$ACC[indSen],xlab = "",ylab = 'Daily Accuracy',pch=19,xlim = c(min(dt$dayDate[dt$scenario==31&dt$site==unique(d30$site_folder[iInd])]),max(dt$dayDate[dt$scenario==31&dt$site==unique(d30$site_folder[iInd])])), col=sig,main='Accuracy daily',ylim=c(0,1))
    abline(h=0.5, col = "gray60",lty=2)
  }
  else
  {
    plot(1,1)
    text(1,1,paste("Less than",wind,"trials"))
  }
  
  #6-Acc by day window R/L
  #--------------------
  indSen=dt$tag==uTag[i]&dt$scenario==32
  # stats
  if (sum(indSen)>1){
    error=dt$ACC[indSen]*dt$nbtrial[indSen]
    correct=(1-dt$ACC[indSen])*dt$nbtrial[indSen]
    pval=c()
    for (z in 1:length(error)){
      x=chisq.test(rbind(c(error[z],correct[z]),c((error[z]+correct[z])/(1/2),(error[z]+correct[z])/(1/2))))
      pval[z]=x$p.value
    }
    sig=as.factor(pval<0.01)
    # if all days are significant
    if (sum(pval<0.01)==length(pval)){
      sig='red'
    }
    plot(dt$dayDate[indSen],dt$ACC[indSen],xlab = "",ylab = 'Daily Accuracy',pch=19,xlim = c(min(dt$dayDate[dt$scenario==32&dt$site==unique(d30$site_folder[iInd])]),max(dt$dayDate[dt$scenario==32&dt$site==unique(d30$site_folder[iInd])])), col=sig,main='Accuracy daily',ylim=c(0,1))
    abline(h=0.5, col = "gray60",lty=2)
  }


  #7-Nbt by day window go-nogo
  #--------------------
  if (sum(dt$tag==uTag[i]&dt$scenario==30)>0){
  plot(dt$dayDate[dt$tag==uTag[i]&dt$scenario==30],dt$nbtrial[dt$tag==uTag[i]&dt$scenario==30],xlab = "",ylab = 'Nb trials',pch=19,xlim = c(min(dt$dayDate[dt$scenario==30&dt$site==unique(d30$site_folder[iInd])]),max(dt$dayDate[dt$scenario==30&dt$site==unique(d30$site_folder[iInd])])),main='Nb trials daily',ylim=c(1,300))
  }
  #8-Nbt by day window L/R
  #--------------------
  if (sum(dt$tag==uTag[i]&dt$scenario==31)>0){
  plot(dt$dayDate[dt$tag==uTag[i]&dt$scenario==31],dt$nbtrial[dt$tag==uTag[i]&dt$scenario==31],xlab = "",ylab = '',pch=19,xlim = c(min(dt$dayDate[dt$scenario==31&dt$site==unique(d30$site_folder[iInd])]),max(dt$dayDate[dt$scenario==31&dt$site==unique(d30$site_folder[iInd])])),main='Nb trials daily',ylim=c(1,300))
  }
  else
  {
    plot(1,1)
    text(1,1,paste("No data"))
  }
  
  #6-Nbt by day window R/L
  #--------------------
  indSen=dt$tag==uTag[i]&dt$scenario==32
  if (sum(indSen)>0){
    plot(dt$dayDate[indSen],dt$nbtrial[indSen],xlab = "",ylab = '',pch=19,xlim = c(min(dt$dayDate[dt$scenario==32&dt$site==unique(d30$site_folder[iInd])]),max(dt$dayDate[dt$scenario==32&dt$site==unique(d30$site_folder[iInd])])),main='Nb trials daily',ylim=c(1,300))
  }

  
  dev.off()
  
}






