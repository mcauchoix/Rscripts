# compute total number of visit, daily average and max by species (with and without reversal learning data)
# work direcly on d [without 1O and GJ]
# keep only last 11 days for scenario 1 for each site
# evaluate species, site, sex, age, scenario effect number of daily visit
#-------------------------------------
# to do
#-------
# remove days with several scenario for analysis that take scenario as fixed effect
# remove 2 individuals with several sites  that take site or elevation as fixed effect 
###########################################

#-----------
# Length of the experiement
#-----------
# Duration, start and end of the experiment 
print(paste("Nb days total all sites confonded",length(unique(d$day))))
print(paste("Start:", format(min(as.Date(d$day,"%d/%m/%y")),"%d-%b-%Y")))
print(paste("End:", format(max(as.Date(d$day,"%d/%m/%y")),"%d-%b-%Y")))

uSite=unique(d$site_folder)
for (i in 1: length(uSite)){
  ind=d$site_folder==uSite[i]&d$scenario!=32
  print(paste("Nb days total",uSite[i],length(unique(d$day[ind]))))
  print(paste("Start:", format(min(as.Date(d$day[ind],"%d/%m/%y")),"%d-%b-%Y")))
  print(paste("End:", format(max(as.Date(d$day[ind],"%d/%m/%y")),"%d-%b-%Y")))
  sc=c(1,2,30,31)
  for (j in 1:length(sc))
  {
    inds=ind&d$scenario==sc[j]
    print(paste("Nb days total",uSite[i],sc[j],length(unique(d$day[inds]))))
    print(paste("Start:", format(min(as.Date(d$day[inds],"%d/%m/%y")),"%d-%b-%Y")))
    print(paste("End:", format(max(as.Date(d$day[inds],"%d/%m/%y")),"%d-%b-%Y")))
  }
}


#---------------------------------
# Table with descritptive stats
#---------------------------------

# nb ind
dunique=d[!duplicated(d$tag),]
tab=data.table(table(dunique$species))
names(tab)=c('species','nb.ind')
# nb trials total
temp=table(d$species)
tab$total.trial=temp

# mean nb trial by day
# nb of visit by day, tag, species and site by species
DT <- as.data.table(d)
DT=DT[,list(nbtrial = .N), by="tag,day,species"]
dv=as.data.frame(DT)
# mean visit by day 
DT <- as.data.table(dv)
DT=DT[,list(mean.visit.daily=round(mean(nbtrial))), by="species"]
dt=as.data.frame(DT)
tab=merge(tab,dt,by="species", all.x=F,sort=F)
# max nb trial by day
# mean visit by day 
DT <- as.data.table(dv)
DT=DT[,list(max.visit.daily=max(nbtrial)), by="species"]
dt=as.data.frame(DT)
tab=merge(tab,dt,by="species", all.x=F,sort=F)
print(tab)
# All
tab2=data.table(species=factor("All"),nb.ind=sum(tab$nb.ind),
                total.trial=sum(tab$total.trial),
                mean.visit.daily=mean(tab$mean.visit.daily),
                max.visit.daily=max(tab$max.visit.daily))
# merge
tab$total.trial=as.numeric(tab$total.trial)
tab=rbind(tab,tab2)
tab$mean.visit.daily=round(tab$mean.visit.daily)

pdf(paste0(out_v,"DescritptiveStats_by Species all.pdf"), height=2, width=6)
grid.table(tab,rows =c())
dev.off()

# without reversal
#-----------------
dall=d
d=d[d$scenario<32,]

# keep only last 11 days for scenario 1 for each site
#------------------
uSite=unique(d$site_folder)
badInd=c()
for (i in 1: length(uSite)){
  ind=d$site_folder==uSite[i]&d$scenario==1
  day=sort(unique(d$dayDate[ind]))
  badInd=c(badInd,which(d$site_folder==uSite[i]&d$dayDate %in% day[1:(length(day)-10)]))
}
d=d[-badInd,]

# nb ind
dunique=d[!duplicated(d$tag),]
tab=data.table(table(dunique$species))
names(tab)=c('species','nb.ind')
# nb trials total
temp=table(d$species)
tab$total.trial=temp

# mean nb trial by day
# nb of visit by day, tag, species and site by species
DT <- as.data.table(d)
DT=DT[,list(nbtrial = .N), by="tag,day,species"]
dv=as.data.frame(DT)
# mean visit by day by individuals
DT <- as.data.table(dv)
DT=DT[,list(mean.visit.daily=round(mean(nbtrial))), by="tag,species"]
dt=as.data.frame(DT)
# mean visit by day by species
DT <- as.data.table(dt)
DT=DT[,list(mean.visit.daily=round(mean(mean.visit.daily))), by="species"]
dt=as.data.frame(DT)

tab=merge(tab,dt,by="species", all.x=F,sort=F)
# max nb trial by day
# mean visit by day 
DT <- as.data.table(dv)
DT=DT[,list(max.visit.daily=max(nbtrial)), by="species"]
dt=as.data.frame(DT)
tab=merge(tab,dt,by="species", all.x=F,sort=F)
print(tab)
# All
tab2=data.table(species=factor("All"),nb.ind=sum(tab$nb.ind),
                total.trial=sum(tab$total.trial),
                mean.visit.daily=mean(tab$mean.visit.daily),
                max.visit.daily=max(tab$max.visit.daily))
# merge
tab$total.trial=as.numeric(tab$total.trial)
tab=rbind(tab,tab2)
tab$mean.visit.daily=round(tab$mean.visit.daily)
names(tab)<-c("Species","Number \n of individuals","Total number \n of trials", "Mean daily number \n of trials","Maximum daily number \n of trials")
pdf(paste0(out_v,"DescritptiveStats_by Species no reversal.pdf"), height=2, width=8)
grid.table(tab,rows =c())
dev.off()

#---------------------------------------
# Total number of trials
#---------------------------------------

#Species, sex and age
#--------------------
# Pre-process nb trial by ind
DT <- as.data.table(d)
DT=DT[,list(nbtrial = .N), by="tag,species,age,sex"]
dv=as.data.frame(DT)

# Pre-process nb trial by species
DT <- as.data.table(dv)
DT=DT[,list(mean=round(mean(nbtrial)),min=min(nbtrial),max=max(nbtrial)), by="species"]
ds=as.data.frame(DT)

# Nb trials by species
pdf(paste0(out_v,"NbTrialsTotal_species.pdf"), height=2, width=3)
grid.table(ds,row.names(c()))
dev.off()

# difference in total number of trials by spe? estim plot
estim<- dabest(dv,species,nbtrial,idx=c("Blue","Great","Marsh"),
               paired = FALSE)
sink(paste0(out_v,"SI_totalnumberOFvisitBYspe.txt"))# store erros
print(estim)
sink()
pdf(paste0(out_v,"SI_totalnumberOFvisitBYspe.pdf"),height = 4,width = 4)
plot(estim,rawplot.ylabel="Total number of visit",effsize.markersize=1)
dev.off()


# difference in total number of trials by spe? model
m=lm(nbtrial~species,data=dv)
sink(paste0(out_v,"anova_TotalVisitBYspecies.txt"))# store erros
print(anova(m))
sink()
print(anova(m))
hist(residuals(m))

# Nb ind by species by sex and by age
dv=dv[dv$species!="Marsh",]
dv$species=factor(dv$species)
pdf(paste0(out_v,"Nbindividuals_sex.pdf"), height=1.5, width=3)
grid.table(table(dv$sex,dv$species))
dev.off()
pdf(paste0(out_v,"Nbindividuals_age.pdf"), height=1.5, width=3)
grid.table(table(dv$age,dv$species))
dev.off()


#Site and elevation
#--------------------
DT <- as.data.table(d)
DT=DT[,list(nbtrial = .N), by="tag,species,age,sex,site_folder,elevation"]
dv=as.data.frame(DT)
pdf(paste0(out_v,"Nbindividuals_site.pdf"), height=1.5, width=3)
grid.table(table(dv$site,dv$species))
dev.off()
pdf(paste0(out_v,"Nbindividuals_elevation.pdf"), height=1.5, width=3)
grid.table(table(dv$elevation,dv$species))
dev.off()
# identify bird recorded at 2 locations
dim(dv)# should be= length(unique(d$tag))
bird2sites=dv$tag[duplicated(dv$tag)]
table(d$site_folder[d$tag==bird2sites[1]])
unique(d$species[d$tag==bird2sites[1]])
unique(d$day[d$tag==bird2sites[1]&d$site_folder=="C1"])
unique(d$day[d$tag==bird2sites[1]&d$site_folder=="C4"])

table(d$site_folder[d$tag==bird2sites[2]])
unique(d$species[d$tag==bird2sites[2]])
unique(d$day[d$tag==bird2sites[2]&d$site_folder=="C1"])
unique(d$day[d$tag==bird2sites[2]&d$site_folder=="C4"])


# difference in total number of trials by spe? model
m=glm(nbtrial~species*elevation,data=dv,family = "quasipoisson")
sink(paste0(out_v,"anova_TotalTrialsBYspe*elevation.txt"))# store erros
print(anova(m,test="Chisq"))
sink()
print(anova(m,test="Chi"))
hist(residuals(m))


#--------------------------
# Daily visit 
#--------------------------

# pre-process daily number of visit
#-----------------------
DT <- as.data.table(d)
DT=DT[,list(nbtrial = .N), by="tag,day,species,scenario,age,sex,site_folder"]
dv=as.data.frame(DT)


DT <- as.data.table(dv)
DT=DT[,list(Av.Vis=mean(nbtrial)), by="tag,scenario,species,age,sex,site_folder"]
dm=as.data.frame(DT)
dm$species=as.factor(dm$species)
dm$scenario=as.factor(dm$scenario)
dm$site_folder=as.factor(dm$site_folder)


# species and scenario effect:
#-----------------------------
# model
m=lmer(sqrt(Av.Vis)~scenario*species+(1|site_folder/tag),data=dm)
sink(paste0(out_v,"Daily visit rate~species*scenario.txt"))# store erros
print(anova(m))
sink()
print(anova(m))
hist(residuals(m))
boxplot(residuals(m)~dm$scenario*dm$species)
plot(m)

# boxplot: Figure 2
pdf(paste(out_v,'dailyVisit.pdf'),width = 8, height=6)
ggplot(dm,aes(y=Av.Vis, x=scenario, colour=species))+
  geom_sina(size=0.7)+
  #geom_point(position=position_jitterdodge(dodge.width=0.7), size=1) +
  scale_color_manual(values=c("blue","gold","brown"))+
  geom_boxplot(alpha=0.5, position = position_dodge(width=0.8),outlier.alpha=0)+
  ylab("Average daily visit number")+
  xlab("")+
  scale_x_discrete(breaks=c("1","2","30","31"),labels=c("Openbar", "Door habituation", "ON/OFF learning","Left/Right learning"))+
  theme_classic()
dev.off()
# estimation plot 
#----------------------
# estim plot species: figure S3 A
DT <- as.data.table(d)
DT=DT[,list(nbtrial = .N), by="tag,day,species"]# pre process again to avoid days with 2 scenario
dv=as.data.frame(DT)
DT <- as.data.table(dv)
DT=DT[,list(Av.Vis=mean(nbtrial)), by="tag,species"]
dms=as.data.frame(DT)
# Species effect: B vs G and M
estim<- dabest(dms,species,Av.Vis,idx=c("Blue","Great","Marsh"),
               paired = FALSE)
sink(paste0(out_v,"SI_dailynumberOFvisitBYspe.txt"))# store erros
print(estim)
sink()
pdf(paste0(out_v,"SI_dailynumberOFvisitBYspe.pdf"),height = 4,width = 4)
plot(estim,rawplot.ylabel="Daily number of visit",effsize.markersize=1,axes.title.fontsize=12,palette="Greys",group.summaries = "median_quartiles")
dev.off()


# Species effect: GvsM
ind=dms$species!="Blue"
estim<- dabest(dms[ind,],species,Av.Vis,idx=c("Great","Marsh"),
               paired = FALSE)
pdf(paste0(out_v,"SInumberOFvisitBySpecies_GvsM.pdf"),height = 4,width = 4)
plot(estim,rawplot.ylabel="Daily number of visit",effsize.markersize=1,palette="Greys")

dev.off()
sink(paste0(out_v,"SInumberOFvisitBySpecies_GvsM.txt"))# store diff
print(estim)
sink()

# scenario effect by species: 
#---------------------------

uSpe=unique(d$species)

for (s in 1:length(uSpe))
{
  print(uSpe[s])

DT <- as.data.table(d)
DT=DT[,list(nbtrial = .N), by="tag,day,scenario,species"]# pre process again to avoid days with 2 scenario
dv=as.data.frame(DT)
# all vs 1: 4 scenarios
DT <- as.data.table(dv)
DT=DT[,list(Av.Vis=mean(nbtrial)), by="tag,scenario,species"]
dms=as.data.frame(DT)
# to keep only individual whod did all scenario
DT <- as.data.table(dms)
DT=DT[,list(NSc=.N), by="tag"]
dmsI=as.data.frame(DT)
dmsPaired=dms[dms$tag %in% dmsI$tag[dmsI$NSc==4],]

estim<- dabest(dmsPaired[dmsPaired$species==uSpe[s],],scenario,Av.Vis,idx=c("1","2","30","31"),id.column = tag,
               paired = TRUE)
sink(paste0(out_v,paste0("SI_dailynumberOFvisitBYscenario",uSpe[s],".txt")))# store erros
print(estim)
sink()
pdf(paste0(out_v,paste0("SI_dailynumberOFvisitBYscenario",uSpe[s],".pdf")),height = 4,width = 4)
print(plot(estim,rawplot.ylabel="Daily number of visit",effsize.markersize=1,axes.title.fontsize=12,palette = "Blues",group.summaries = "median_quartiles"))
dev.off()


# all vs 2: 3 scenarios
DT <- as.data.table(dv[dv$scenario>1,])
DT=DT[,list(Av.Vis=mean(nbtrial)), by="tag,scenario,species"]
dms=as.data.frame(DT)
# to keep only individual whod did all scenario
DT <- as.data.table(dms)
DT=DT[,list(NSc=.N), by="tag"]
dmsI=as.data.frame(DT)
dmsPaired=dms[dms$tag %in% dmsI$tag[dmsI$NSc==3],]

estim<- dabest(dmsPaired[dmsPaired$species==uSpe[s],],scenario,Av.Vis,idx=c("2","30","31"),id.column = tag,
               paired = TRUE)
sink(paste0(out_v,paste0("SI_dailynumberOFvisitVS2",uSpe[s],".txt")))# store erros
print(estim)
sink()
pdf(paste0(out_v,paste0("SI_dailynumberOFvisitVS2",uSpe[s],".pdf")),height = 4,width = 4)
print(plot(estim,rawplot.ylabel="Daily number of visit",effsize.markersize=1,axes.title.fontsize=12,palette = "Blues"))
dev.off()

# 30 vs 31
DT <- as.data.table(dv[dv$scenario>2,])
DT=DT[,list(Av.Vis=mean(nbtrial)), by="tag,scenario,species"]
dms=as.data.frame(DT)
# to keep only individual whod did all scenario
DT <- as.data.table(dms)
DT=DT[,list(NSc=.N), by="tag"]
dmsI=as.data.frame(DT)
dmsPaired=dms[dms$tag %in% dmsI$tag[dmsI$NSc==2],]

estim<- dabest(dmsPaired[dmsPaired$species==uSpe[s],],scenario,Av.Vis,idx=c("30","31"),id.column = tag,
               paired = TRUE)
sink(paste0(out_v,paste0("SI_dailynumberOFvisit30vs31",uSpe[s],".txt")))# store erros
print(estim)
sink()
pdf(paste0(out_v,paste0("SI_dailynumberOFvisit30vs31",uSpe[s],".pdf")),height = 4,width = 4)
print(plot(estim,rawplot.ylabel="Daily number of visit",effsize.markersize=1,axes.title.fontsize=12,palette = "Blues"))
dev.off()

}
#----------------------
# Sex and age effect
#---------------------

# Model
#------
# GT only

print("Daily visit rate GT")
m=lmer(sqrt(Av.Vis)~scenario*(age+sex)+(1|site_folder)+(1|tag),data=dm, subset = dm$species=="Great")
print(anova(m))
hist(residuals(m))
# Validity
simulationOutput <- simulateResiduals(fittedModel = m, n = 1000)
Unif=testUniformity(simulationOutput = simulationOutput)
plot(m)
sink(paste0(out_v,"anova_visit_Great.txt"))# store erros
print(anova(m))
sink()
plot(m)

# BT only
print("Daily visit rate GT")
m=lmer(sqrt(Av.Vis)~scenario*(age+sex)+(1|site_folder)+(1|tag),data=dm, subset = dm$species=="Blue")
print(anova(m))
hist(residuals(m))
plot(m)
sink(paste0(out_v,"anova_visit_Blue.txt"))# store erros
print(anova(m))
sink()

# estim plot: age GT
dm$age_sc=paste(dm$age,dm$scenario)
ind=dm$species=="Great"
estim<- dabest(dm[ind,],age_sc,Av.Vis,
               idx = list(c("1a 1", "+1a 1"), 
                          c("1a 2", "+1a 2"),
                          c("1a 30", "+1a 30"),
                          c("1a 31", "+1a 31")),
               paired = FALSE)
sink(paste0(out_v,paste0("SI_dailynumberOFvisit_GT_age.txt")))# store erros
print(estim)
sink()
pdf(paste0(out_v,paste0("SI_dailynumberOFvisit_GT_age.pdf")),height = 4,width = 8)
print(plot(estim,rawplot.ylabel="Daily number of visit",effsize.markersize=1,axes.title.fontsize=12,color.column = age,group.summaries = "median_quartiles"))
dev.off()

# estim plot: sex GT
dm$sex_sc=paste(dm$sex,dm$scenario)
ind=dm$species=="Great"
estim<- dabest(dm[ind,],sex_sc,Av.Vis,
               idx = list(c("F 1","M 1"), 
                          c("F 2","M 2"),
                          c("F 30","M 30"),
                          c("F 31","M 31")),
               paired = FALSE)
sink(paste0(out_v,paste0("SI_dailynumberOFvisit_GT_sex.txt")))# store erros
print(estim)
sink()
pdf(paste0(out_v,paste0("SI_dailynumberOFvisit_GT_sex.pdf")),height = 4,width = 8)
print(plot(estim,rawplot.ylabel="Daily number of visit",effsize.markersize=1,axes.title.fontsize=12,color.column = sex,group.summaries = "median_quartiles"))
dev.off()






# plot
#-----
# pdf(paste(out_v,'dailyVisit.pdf'),width = 8, height=6)
# ggplot(dm, aes(x=scenario, y=Av.Vis, fill=species)) +
#   geom_boxplot(outlier.alpha=0)+geom_jitter(aes( x = scenario),size=0.2, alpha = 0.2,position=position_jitterdodge(dodge.width=0.9))+scale_fill_manual(values=c("blue","gold","brown"))+
#   ylab("Average daily visit number")+
#   xlab("")+
#   scale_x_discrete(breaks=c("1","2","30","31"),labels=c("Openbar", "Door habituation", "ON/OFF learning","Left/Right learning"))
# dev.off()

# 
# # GT: Sex * scenario
# pdf(paste(out_v,'dailyVisit_Great_sex.pdf'),width = 8, height=6)
# ggplot(subset(dm,species=="Great"),aes(y=Av.Vis, x=scenario, colour=sex))+
#   #geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) +
#   geom_sina(size=0.7)+
#   scale_color_manual(values=c("pink","blue"))+
#   geom_boxplot(alpha=0.5, position = position_dodge(width=0.8),outlier.alpha=0)+
#   ylab("Average daily visit number")+
#   xlab("")+
#   scale_x_discrete(breaks=c("1","2","30","31"),labels=c("Openbar", "Door habituation", "ON/OFF learning","Left/Right learning"))+
#   theme_classic()
# dev.off()
# 
# # GT: Age * scenario
# pdf(paste(out_v,'dailyVisit_Great_age.pdf'),width = 8, height=6)
# ggplot(subset(dm,species=="Great"),aes(y=Av.Vis, x=scenario, colour=age))+
#   #geom_point(position=position_jitterdodge(dodge.width=0.7), size=2) +
#   geom_sina(size=0.7)+
#   scale_color_manual(values=c("orange","grey"))+
#   geom_boxplot(alpha=0.5, position = position_dodge(width=0.8),outlier.alpha=0)+
#   ylab("Average daily visit number")+
#   xlab("")+
#   scale_x_discrete(breaks=c("1","2","30","31"),labels=c("Openbar", "Door habituation", "ON/OFF learning","Left/Right learning"))+
#   theme_classic()
# dev.off()





#--------------------------
# re-initialise d
#--------------------------

d=dall