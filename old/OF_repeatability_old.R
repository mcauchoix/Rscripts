# run repeatability analysis on learning
#---------------------------------------
#library
# param
nboot=10
npermut=10
# unique site
uSite=unique(d$site_folder)
# loop over sites

#for (i in 1:length(uSite)){
e=d#[d$site_folder=="GJ",]#uSite[i]
#--------------------------------------#
#Learning
#--------------------------------------#
# global accuracy and number of trials
#-------------------------------------
DT <- as.data.table(e[e$scenario>29,])
DT=DT[,list(ACC =mean(door.open),nbtrial = .N,species=unique(species)), by="tag,scenario"]
dt=as.data.frame(DT)
# repeatability
rep=rpt(ACC~species+(1|tag), grname = c("tag"), data = dt, datatype = "Gaussian",nboot = nboot, npermut = npermut) 
rep=rpt(nbtrial~species+(1|tag), grname = c("tag"), data = dt, datatype = "Gaussian",nboot = nboot, npermut = npermut) 


# Accuracy by day after start
#-----------------------------
DT <- as.data.table(e[e$scenario>29,])
DT=DT[,list(ACC =mean(door.open),nbtrial = .N,species=unique(species),site=unique(site_folder)), by="day,tag,scenario"]
dt=as.data.frame(DT)
dt$dayDate=as.Date(dt$day,"%d/%m/%y")

# compute stats by site
#----------------------
dt$groupDay='mid'
uSite=unique(dt$site)
max30=c()
max31=c()
for (i in 1 :length(uSite)){
  max30=max(dt$dayDate[dt$scenario==30&dt$site==uSite[i]])
  max31=max(dt$dayDate[dt$scenario==31&dt$site==uSite[i]])
  dt$groupDay[dt$scenario==30&dt$site==uSite[i]&dt$dayDate>(max30-3)]='last'
  dt$groupDay[dt$scenario==30&dt$site==uSite[i]&dt$dayDate<4]='first'
  dt$groupDay[dt$scenario==31&dt$site==uSite[i]&dt$dayDate>(max31-3)]='last'
  dt$groupDay[dt$scenario==31&dt$site==uSite[i]&dt$dayDate<4]='first'
  }
# # day number from experiement start
# dt$dayOrder[dt$scenario==30]=dt$dayDate[dt$scenario==30]-min(dt$dayDate[dt$scenario==30])
# dt$dayOrder[dt$scenario==31]=dt$dayDate[dt$scenario==31]-min(dt$dayDate[dt$scenario==31])
# #nb days
# max30=max(dt$dayOrder[dt$scenario==30])
# max31=max(dt$dayOrder[dt$scenario==31])
# # last days
# dt$groupDay='mid'
# dt$groupDay[dt$scenario==30&dt$dayOrder>(max30-3)]='last'
# dt$groupDay[dt$scenario==30&dt$dayOrder<4]='first'
# dt$groupDay[dt$scenario==31&dt$dayOrder>(max31-3)]='last'
# dt$groupDay[dt$scenario==31&dt$dayOrder<4]='first'

  
# # repeatability by day
# R=data.table(c(1:min(c(max30,max31))))
# names(R)="day"
# for (day in 1:14){
# rep=rpt(ACC~species+(1|tag), grname = c("tag"), data = dt[dt$dayOrder==day&dt$nbtrial>30,], datatype = "Gaussian",nboot = nboot, npermut = npermut) 
# R$R[day]=round(rep$R[[1]],2)
# R$p_LRT[day]=round(rep$P$LRT_P[[1]],2)
# }

# on last 3 days
DT <- as.data.table(dt)
DT=DT[,list(ACC =mean(ACC),nb=mean(nbtrial),species=unique(species),site=unique(site)), by="tag,scenario,groupDay"]
dt=as.data.frame(DT)
rep=rpt(ACC~species+site+(1|tag), grname = c("tag"), data = dt[dt$groupDay=="last",], datatype = "Gaussian",nboot = nboot, npermut = npermut) 
repName=paste0('R=',round(rep$R[[1]],2),' [',round(rep$CI_emp[[1]],2),' ',round(rep$CI_emp[[2]],2),']')
#store data
res=data.frame('All')
names(res)="species"
res$R=round(rep$R[[1]],2)
res$lowCI=round(rep$CI_emp[[1]],2)
res$highCI=round(rep$CI_emp[[2]],2) 
res$SS=length(unique(dt$tag))
# on nb trial
rep=rpt(nb~species+site+(1|tag), grname = c("tag"), data = dt[dt$groupDay=="last",], datatype = "Gaussian",nboot = nboot, npermut = npermut) 
res$Rnbtrial=round(rep$R[[1]],2)

# visualise correlation
cor30=dt[dt$groupDay=='last'&dt$scenario==30,]
cor31=dt[dt$groupDay=='last'&dt$scenario==31,]
co=merge(cor30,cor31,by="tag", all.x=T,sort=F)
co$species=as.factor(co$species.x)
pdf(paste(out_f,'Learning Repeatability.pdf'))
plot(co$ACC.x,co$ACC.y,col=co$species,xlab = "Accuracy (Go Nogo task)",ylab="Accuracy (L/R task)",pch=19,main=repName)
abline(h=0.5, col = "gray60",lty=2)
abline(v=0.25, col = "gray60",lty=2)
legend(0.8,0.55,legend = levels(co$species),col=1:length(co$species),pch=19,cex=1,y.intersp=1,bty="n")
dev.off()


# Rep by species
uSpe=unique(d$species)
for (s in 1:length(uSpe)){
rep=rpt(ACC~site+(1|tag), grname = c("tag"), data = dt[dt$groupDay=="last"&dt$species==uSpe[s],], datatype = "Gaussian",nboot = nboot, npermut = npermut) 
# on nb trial
rep2=rpt(nb~site+(1|tag), grname = c("tag"), data = dt[dt$groupDay=="last"&dt$species==uSpe[s],], datatype = "Gaussian",nboot = nboot, npermut = npermut) 


temp=data.frame(uSpe[s],round(rep$R[[1]],2),round(rep$CI_emp[[1]],2),round(rep$CI_emp[[2]],2),length(unique(dt$tag[dt$species==uSpe[s]])),round(rep2$R[[1]],2))
names(temp)=c("species","R","lowCI","highCI","SS","Rnbtrial")
res = rbind(res,temp)
}
# Export
pdf(paste0(out_f,"RepeatabilityACC_last3days_by Species.pdf"), height=3, width=3)
grid.table(res,rows =c())
dev.off()