# Test for learning at the population level
# Report participation in the task: number of bird learning
#########################################
#d=read.table('/Users/maximecauchoix/Documents/openfeeder/data/StudySite_2018_19.csv',h=T)

# for Gamm
library("gamm4")
library(mgcv)
library(itsadug)

d=read.table('/Users/maximecauchoix/Documents/openfeeder/data/StudySite_2018_19.csv',h=T)
#---------------------
# Learning at the pop level
#---------------------
# precompute acc by day
DT <- as.data.table(d[d$scenario>29,])
DT=DT[,list(ACC =mean(door.open),nbtrial = .N,ncorr=sum(door.open==1)), by="day,scenario,tag,species,site_folder,sex,age,elevation"]
dt=as.data.frame(DT)
dt$dayDate=as.Date(dt$day,"%d/%m/%y")
# code day number
uSite=unique(dt$site_folder)
uSc=unique(dt$scenario)
uTag=unique(d$tag)
# Bird day
for (si in 1:length(uSite)){
  for (sc in 1:length(uSc)){
    i=dt$site_folder==uSite[si]&dt$scenario==uSc[sc]
    dt$dayNumber[i]=dt$dayDate[i]-min(dt$dayDate[i])+1
    for (t in 1:length(uTag)){
      indi=i&dt$tag==uTag[t]
      if (sum(indi)>0){
        dt$BirdDaynumber[indi]=1:sum(indi)
      }
    }
  }
}


# distrib
hist(sqrt(dt$ACC[dt$nbtrial>30]))
#dotchart(dt$ACC[dt$nbtrial>30])

############ GAMM  ####################
#----------------------------------------------------------------
# Scenario 30
#---------------------------------------------------------------

# All species
#------------
ind=dt$nbtrial>1&dt$scenario==30

# Learning pop
m1=gamm4(ACC~s(BirdDaynumber)+species,random = ~(1|site_folder/tag),data=dt,subset =ind,
         weights = dt$nbtrial,family = "binomial")

sink(paste0(out_ls,"SpeciesEffect_GAMM_30.txt"))# store erros
print(anova(m1$gam))
sink()
par(mfrow=c(2,2),mar=c(5, 4, 4, 2)/2)
plot(m1$gam,pages=1)
gam.check(m1$gam,pch=19,cex=.3)

# interaction
m2=gamm4(ACC~s(BirdDaynumber,by=species)+species,random = ~(1|site_folder/tag),data=dt,subset =ind,
         weights = dt$nbtrial,family = "binomial")

sink(paste0(out_ls,"SpeciesEffectInteraction_GAMM_30.txt"))# store erros
anova(m1$mer,m2$mer)
sink()


# GT
#------
ind=dt$species=="Great"&dt$scenario=="30"
m1gt=gamm4(ACC~s(BirdDaynumber)+age+sex,random = ~(1|site_folder/tag),data=dt,subset =ind,
           weights = dt$nbtrial,family = "binomial")
sink(paste0(out_ls,"GT_ageSex_GAMM_30.txt"))# store erros
anova(m1gt$gam)
sink()

m2gt=gamm4(ACC~s(BirdDaynumber,by=age)+age+sex,random = ~(1|site_folder/tag),data=dt,subset =ind,
           weights = dt$nbtrial,family = "binomial")
m3gt=gamm4(ACC~s(BirdDaynumber,by=sex)+age+sex,random = ~(1|site_folder/tag),data=dt,subset =ind,
           weights = dt$nbtrial,family = "binomial")

sink(paste0(out_ls,"GT_AgeEffectInteraction_GAMM_30.txt"))# store erros
anova(m1gt$mer,m2gt$mer)
sink()

par(mfrow=c(2,2),mar=c(5, 4, 4, 2)/2)
plot(m2gt$gam,pages=1)

sink(paste0(out_ls,"GT_SexEffectInteraction_GAMM_30.txt"))# store erros
anova(m1gt$mer,m3gt$mer)
sink()

par(mfrow=c(2,2),mar=c(5, 4, 4, 2)/2)
plot(m3gt$gam,pages=1)
gam.check(m1$gam,pch=19,cex=.3)

# plot sex
pdf(paste0(out_ls,"GT_SexEffect_30.pdf"))
indSex=dt$nbtrial>1&dt$scenario==30&dt$species=="Great"&!is.na(dt$sex)
ggplot(subset(dt,indSex), aes(y=ACC, x=BirdDaynumber)) + 
  geom_point(aes(colour=sex),size=0.5) +  
  geom_hline(yintercept=0.25, linetype="dashed", color = "black")+
  scale_color_manual(values=c("pink","blue"))+
  stat_smooth(method = "loess",aes(colour=sex))+  
  #theme_bw()+
  #ggtitle("Learning ON/OFF") +
  ylab("Daily accuracy") + xlab("Bird Day Number")+
  theme_classic()+
  theme(text = element_text(size=18))
dev.off() 

# Plot age
pdf(paste0(out_ls,"GT_AgeEffect_30.pdf"))
indAge=dt$nbtrial>1&dt$scenario==30&dt$species=="Great"&!is.na(dt$age)
ggplot(subset(dt,indAge), aes(y=ACC, x=BirdDaynumber)) + 
  geom_point(aes(colour=age),size=0.5) +  
  geom_hline(yintercept=0.25, linetype="dashed", color = "black")+
  scale_color_manual(values=c("green","orange"))+
  stat_smooth(method = "loess",aes(colour=age))+  
  #theme_bw()+
  #ggtitle("Learning ON/OFF") +
  ylab("Daily accuracy") + xlab("Bird Day Number")+
  theme_classic()+
  theme(text = element_text(size=18))
dev.off() 


# BT
#------
ind=dt$species=="Blue"&dt$scenario=="30"
m1gt=gamm4(ACC~s(BirdDaynumber)+age+sex,random = ~(1|site_folder/tag),data=dt,subset =ind,
           weights = dt$nbtrial,family = "binomial")
sink(paste0(out_ls,"BT_ageSex_GAMM_30.txt"))# store erros
anova(m1gt$gam)
sink()

m2gt=gamm4(ACC~s(BirdDaynumber,by=age)+age+sex,random = ~(1|site_folder/tag),data=dt,subset =ind,
           weights = dt$nbtrial,family = "binomial")
m3gt=gamm4(ACC~s(BirdDaynumber,by=sex)+age+sex,random = ~(1|site_folder/tag),data=dt,subset =ind,
           weights = dt$nbtrial,family = "binomial")

sink(paste0(out_ls,"BT_AgeEffectInteraction_GAMM_30.txt"))# store erros
anova(m1gt$mer,m2gt$mer)
sink()

sink(paste0(out_ls,"BT_SexEffectInteraction_GAMM_30.txt"))# store erros
anova(m1gt$mer,m3gt$mer)
sink()

par(mfrow=c(2,2),mar=c(5, 4, 4, 2)/2)
plot(m3gt$gam,pages=1)
gam.check(m1$gam,pch=19,cex=.3)

# plot sex
pdf(paste0(out_ls,"BT_SexEffect_30.pdf"))
indSex=dt$nbtrial>1&dt$scenario==30&dt$species=="Blue"&!is.na(dt$sex)
ggplot(subset(dt,indSex), aes(y=ACC, x=BirdDaynumber)) + 
  geom_point(aes(colour=sex),size=0.5) +  
  geom_hline(yintercept=0.25, linetype="dashed", color = "black")+
  scale_color_manual(values=c("pink","blue"))+
  stat_smooth(method = "loess",aes(colour=sex))+  
  #theme_bw()+
  #ggtitle("Learning ON/OFF") +
  ylab("Daily accuracy") + xlab("Bird Day Number")+
  theme_classic()+
  theme(text = element_text(size=18))
dev.off() 

# Plot age
pdf(paste0(out_ls,"BT_AgeEffect_30.pdf"))
indAge=dt$nbtrial>1&dt$scenario==30&dt$species=="Blue"&!is.na(dt$age)
ggplot(subset(dt,indAge), aes(y=ACC, x=BirdDaynumber)) + 
  geom_point(aes(colour=age),size=0.5) +  
  geom_hline(yintercept=0.25, linetype="dashed", color = "black")+
  scale_color_manual(values=c("green","orange"))+
  stat_smooth(method = "loess",aes(colour=age))+  #,level=0.83
  #theme_bw()+
  #ggtitle("Learning ON/OFF") +
  ylab("Daily accuracy") + xlab("Bird Day Number")+
  theme_classic()+
  theme(text = element_text(size=18))
dev.off() 




#----------------------------------------------------------------
# Scenario 31
#---------------------------------------------------------------

# All species
#------------
ind=dt$nbtrial>1&dt$scenario==31

# Learning pop
m1=gamm4(ACC~s(BirdDaynumber)+species,random = ~(1|site_folder/tag),data=dt,subset =ind,
         weights = dt$nbtrial,family = "binomial")

sink(paste0(out_ls,"SpeciesEffect_GAMM_31.txt"))# store erros
print(anova(m1$gam))
sink()
par(mfrow=c(2,2),mar=c(5, 4, 4, 2)/2)
plot(m1$gam,pages=1)
gam.check(m1$gam,pch=19,cex=.3)

# interaction
m2=gamm4(ACC~s(BirdDaynumber,by=species)+species,random = ~(1|site_folder/tag),data=dt,subset =ind,
         weights = dt$nbtrial,family = "binomial")

sink(paste0(out_ls,"SpeciesEffectInteraction_GAMM_31.txt"))# store erros
anova(m1$mer,m2$mer)
sink()


# GT
#------
ind=dt$species=="Great"&dt$scenario=="31"
m1gt=gamm4(ACC~s(BirdDaynumber)+age+sex,random = ~(1|site_folder/tag),data=dt,subset =ind,
           weights = dt$nbtrial,family = "binomial")
sink(paste0(out_ls,"GT_ageSex_GAMM_31.txt"))# store erros
anova(m1gt$gam)
sink()

m2gt=gamm4(ACC~s(BirdDaynumber,by=age)+age+sex,random = ~(1|site_folder/tag),data=dt,subset =ind,
           weights = dt$nbtrial,family = "binomial")
m3gt=gamm4(ACC~s(BirdDaynumber,by=sex)+age+sex,random = ~(1|site_folder/tag),data=dt,subset =ind,
           weights = dt$nbtrial,family = "binomial")

sink(paste0(out_ls,"GT_AgeEffectInteraction_GAMM_31.txt"))# store erros
anova(m1gt$mer,m2gt$mer)
sink()

par(mfrow=c(2,2),mar=c(5, 4, 4, 2)/2)
plot(m2gt$gam,pages=1)

sink(paste0(out_ls,"GT_SexEffectInteraction_GAMM_31.txt"))# store erros
anova(m1gt$mer,m3gt$mer)
sink()

par(mfrow=c(2,2),mar=c(5, 4, 4, 2)/2)
plot(m3gt$gam,pages=1)
gam.check(m1$gam,pch=19,cex=.3)

# plot sex
pdf(paste0(out_ls,"GT_SexEffect_31.pdf"))
indSex=dt$nbtrial>1&dt$scenario==31&dt$species=="Great"&!is.na(dt$sex)
ggplot(subset(dt,indSex), aes(y=ACC, x=BirdDaynumber)) + 
  geom_point(aes(colour=sex),size=0.5) +  
  geom_hline(yintercept=0.50, linetype="dashed", color = "black")+
  scale_color_manual(values=c("pink","blue"))+
  stat_smooth(method = "loess",aes(colour=sex))+  
  #theme_bw()+
  #ggtitle("Learning ON/OFF") +
  ylab("Daily accuracy") + xlab("Bird Day Number")+
  theme_classic()+
  theme(text = element_text(size=18))
dev.off() 

# Plot age
pdf(paste0(out_ls,"GT_AgeEffect_31.pdf"))
indAge=dt$nbtrial>1&dt$scenario==31&dt$species=="Great"&!is.na(dt$age)
ggplot(subset(dt,indAge), aes(y=ACC, x=BirdDaynumber)) + 
  geom_point(aes(colour=age),size=0.5) +  
  geom_hline(yintercept=0.50, linetype="dashed", color = "black")+
  scale_color_manual(values=c("green","orange"))+
  stat_smooth(method = "loess",aes(colour=age))+  
  #theme_bw()+
  #ggtitle("Learning ON/OFF") +
  ylab("Daily accuracy") + xlab("Bird Day Number")+
  theme_classic()+
  theme(text = element_text(size=18))
dev.off() 


# BT
#------
ind=dt$species=="Blue"&dt$scenario=="31"
m1gt=gamm4(ACC~s(BirdDaynumber)+age+sex,random = ~(1|site_folder/tag),data=dt,subset =ind,
           weights = dt$nbtrial,family = "binomial")
sink(paste0(out_ls,"BT_ageSex_GAMM_31.txt"))# store erros
anova(m1gt$gam)
sink()

m2gt=gamm4(ACC~s(BirdDaynumber,by=age)+age+sex,random = ~(1|site_folder/tag),data=dt,subset =ind,
           weights = dt$nbtrial,family = "binomial")
m3gt=gamm4(ACC~s(BirdDaynumber,by=sex)+age+sex,random = ~(1|site_folder/tag),data=dt,subset =ind,
           weights = dt$nbtrial,family = "binomial")

sink(paste0(out_ls,"BT_AgeEffectInteraction_GAMM_31.txt"))# store erros
anova(m1gt$mer,m2gt$mer)
sink()

sink(paste0(out_ls,"BT_SexEffectInteraction_GAMM_31.txt"))# store erros
anova(m1gt$mer,m3gt$mer)
sink()

par(mfrow=c(2,2),mar=c(5, 4, 4, 2)/2)
plot(m3gt$gam,pages=1)
gam.check(m1$gam,pch=19,cex=.3)

# plot sex
pdf(paste0(out_ls,"BT_SexEffect_31.pdf"))
indSex=dt$nbtrial>1&dt$scenario==31&dt$species=="Blue"&!is.na(dt$sex)
ggplot(subset(dt,indSex), aes(y=ACC, x=BirdDaynumber)) + 
  geom_point(aes(colour=sex),size=0.5) +  
  geom_hline(yintercept=0.50, linetype="dashed", color = "black")+
  scale_color_manual(values=c("pink","blue"))+
  stat_smooth(method = "loess",aes(colour=sex))+  
  #theme_bw()+
  #ggtitle("Learning ON/OFF") +
  ylab("Daily accuracy") + xlab("Bird Day Number")+
  theme_classic()+
  theme(text = element_text(size=18))
dev.off() 

# Plot age
pdf(paste0(out_ls,"BT_AgeEffect_31.pdf"))
indAge=dt$nbtrial>1&dt$scenario==31&dt$species=="Blue"&!is.na(dt$age)
ggplot(subset(dt,indAge), aes(y=ACC, x=BirdDaynumber)) + 
  geom_point(aes(colour=age),size=0.5) +  
  geom_hline(yintercept=0.50, linetype="dashed", color = "black")+
  scale_color_manual(values=c("green","orange"))+
  stat_smooth(method = "loess",aes(colour=age))+  #,level=0.83
  #theme_bw()+
  #ggtitle("Learning ON/OFF") +
  ylab("Daily accuracy") + xlab("Bird Day Number")+
  theme_classic()+
  theme(text = element_text(size=18))
dev.off() 

#----------------------------------------------------------------
# Task effect
#---------------------------------------------------------------


# normalise accuracy (because of different chance level)
#dt$ACCnormal[dt$scenario==30]=(dt$ACC[dt$scenario==30]+0.25)/max(dt$ACCnormal[dt$scenario==30])
#dt$ACCnormal[dt$scenario==31]=(dt$ACC[dt$scenario==31])/max(dt$ACCnormal[dt$scenario==31])

# All species
#-------------
ind=dt$dayNumber<16&dt$scenario %in% c(30,31)
m1=gamm4(ACC~s(BirdDaynumber)+scenario+species,random = ~(1|site_folder/tag),data=dt,subset =ind,
         weights = dt$nbtrial,family = "binomial")

sink(paste0(out_ls,"TaskEffect_GAMM.txt"))# store erros
print(anova(m1$gam))
sink()
par(mfrow=c(2,2),mar=c(5, 4, 4, 2)/2)
plot(m1$gam,pages=1)
gam.check(m1$gam,pch=19,cex=.3)

# interaction
dt$scenario=as.factor(dt$scenario)
m2=gamm4(ACC~s(BirdDaynumber,by=scenario)+scenario+species,random = ~(1|site_folder/tag),data=dt,subset =ind,
         weights = dt$nbtrial,family = "binomial")

sink(paste0(out_ls,"TaskEffectInteraction_GAMM.txt"))# store erros
anova(m1$mer,m2$mer)
sink()

# Plot task
pdf(paste0(out_ls,"taskEffect.pdf"))
indAge=dt$dayNumber<16&dt$scenario %in% c(30,31)

ggplot(subset(dt,indAge), aes(y=ACC, x=BirdDaynumber)) + 
  geom_point(aes(colour=scenario),size=0.5) +  
  geom_hline(yintercept=0.50, linetype="dashed", color = "black")+
  geom_hline(yintercept=0.25, linetype="dashed", color = "black")+
  scale_color_manual(values=c("black","grey"))+
  stat_smooth(method = "loess",aes(colour=scenario))+  #,level=0.83
  #theme_bw()+
  #ggtitle("Learning ON/OFF") +
  ylab("Daily accuracy") + xlab("Bird Day Number")+
  theme_classic()+
  theme(text = element_text(size=18))
dev.off() 

# MT only
#-------
ind=dt$dayNumber<16&dt$scenario %in% c(30,31)&dt$species=="Marsh"
m1=gamm4(ACC~s(BirdDaynumber)+scenario,random = ~(1|site_folder/tag),data=dt,subset =ind,
         weights = dt$nbtrial,family = "binomial")

sink(paste0(out_ls,"TaskEffect_Marsh_GAMM.txt"))# store erros
print(anova(m1$gam))
sink()
par(mfrow=c(2,2),mar=c(5, 4, 4, 2)/2)
plot(m1$gam,pages=1)
gam.check(m1$gam,pch=19,cex=.3)

# interaction
dt$scenario=as.factor(dt$scenario)
m2=gamm4(ACC~s(BirdDaynumber,by=scenario)+scenario,random = ~(1|site_folder/tag),data=dt,subset =ind,
         weights = dt$nbtrial,family = "binomial")

sink(paste0(out_ls,"TaskEffectInteraction_Marsh_GAMM.txt"))# store erros
anova(m1$mer,m2$mer)
sink()

# Plot task
pdf(paste0(out_ls,"taskEffect_Marsh.pdf"))
ggplot(subset(dt,ind), aes(y=ACC, x=BirdDaynumber)) + 
  geom_point(aes(colour=scenario),size=0.5) +  
  geom_hline(yintercept=0.50, linetype="dashed", color = "black")+
  geom_hline(yintercept=0.25, linetype="dashed", color = "black")+
  scale_color_manual(values=c("chocolate4","chocolate"))+
  stat_smooth(method = "loess",aes(colour=scenario))+  #,level=0.83
  #theme_bw()+
  #ggtitle("Learning ON/OFF") +
  ylab("Daily accuracy") + xlab("Bird Day Number")+
  theme_classic()+
  theme(text = element_text(size=18))
dev.off() 


# GT only
#-------
ind=dt$dayNumber<16&dt$scenario %in% c(30,31)&dt$species=="Great"
m1=gamm4(ACC~s(BirdDaynumber)+scenario,random = ~(1|site_folder/tag),data=dt,subset =ind,
         weights = dt$nbtrial,family = "binomial")

sink(paste0(out_ls,"TaskEffect_Great_GAMM.txt"))# store erros
print(anova(m1$gam))
sink()
par(mfrow=c(2,2),mar=c(5, 4, 4, 2)/2)
plot(m1$gam,pages=1)
gam.check(m1$gam,pch=19,cex=.3)

# interaction
dt$scenario=as.factor(dt$scenario)
m2=gamm4(ACC~s(BirdDaynumber,by=scenario)+scenario,random = ~(1|site_folder/tag),data=dt,subset =ind,
         weights = dt$nbtrial,family = "binomial")

sink(paste0(out_ls,"TaskEffectInteraction_Great_GAMM.txt"))# store erros
anova(m1$mer,m2$mer)
sink()

# Plot task
pdf(paste0(out_ls,"taskEffect_Great.pdf"))
ggplot(subset(dt,ind), aes(y=ACC, x=BirdDaynumber)) + 
  geom_point(aes(colour=scenario),size=0.5) +  
  geom_hline(yintercept=0.50, linetype="dashed", color = "black")+
  geom_hline(yintercept=0.25, linetype="dashed", color = "black")+
  scale_color_manual(values=c("orange","gold"))+
  stat_smooth(method = "loess",aes(colour=scenario))+  #,level=0.83
  #theme_bw()+
  #ggtitle("Learning ON/OFF") +
  ylab("Daily accuracy") + xlab("Bird Day Number")+
  theme_classic()+
  theme(text = element_text(size=18))
dev.off() 


# BT only
#-------
ind=dt$dayNumber<16&dt$scenario %in% c(30,31)&dt$species=="Blue"
m1=gamm4(ACC~s(BirdDaynumber)+scenario,random = ~(1|site_folder/tag),data=dt,subset =ind,
         weights = dt$nbtrial,family = "binomial")

sink(paste0(out_ls,"TaskEffect_Blue_GAMM.txt"))# store erros
print(anova(m1$gam))
sink()
par(mfrow=c(2,2),mar=c(5, 4, 4, 2)/2)
plot(m1$gam,pages=1)
gam.check(m1$gam,pch=19,cex=.3)

# interaction
dt$scenario=as.factor(dt$scenario)
m2=gamm4(ACC~s(BirdDaynumber,by=scenario)+scenario,random = ~(1|site_folder/tag),data=dt,subset =ind,
         weights = dt$nbtrial,family = "binomial")

sink(paste0(out_ls,"TaskEffectInteraction_Blue_GAMM.txt"))# store erros
anova(m1$mer,m2$mer)
sink()

# Plot task
pdf(paste0(out_ls,"taskEffect_Blue.pdf"))
ggplot(subset(dt,ind), aes(y=ACC, x=BirdDaynumber)) + 
  geom_point(aes(colour=scenario),size=0.5) +  
  geom_hline(yintercept=0.50, linetype="dashed", color = "black")+
  geom_hline(yintercept=0.25, linetype="dashed", color = "black")+
  scale_color_manual(values=c("darkblue","blue"))+
  stat_smooth(method = "loess",aes(colour=scenario))+  #,level=0.83
  #theme_bw()+
  #ggtitle("Learning ON/OFF") +
  ylab("Daily accuracy") + xlab("Bird Day Number")+
  theme_classic()+
  theme(text = element_text(size=18))
dev.off() 


############################# GLMM  ###########################################
# 
# # Plot 30
# #------------
# # All sites
# pdf(paste0(out_ls,"SpeciesEffect_30.pdf"))
# ggplot(subset(dt,ind), aes(y=ACC, x=BirdDaynumber)) + 
#   geom_point(aes(colour=species),size=0.5) +  
#   geom_hline(yintercept=0.25, linetype="dashed", color = "black")+
#   scale_color_manual(values=c("blue","gold","brown"))+
#   stat_smooth(method="lm", formula=y ~ poly(x, 2),aes(colour=species))+  
#   #theme_bw()+
#   #ggtitle("Learning ON/OFF") +
#   ylab("Daily accuracy") + xlab("Bird Day Number")+
#   theme_classic()+
#   theme(text = element_text(size=18))
# dev.off() 
# # By study site
# gplot=list()
# for (si in 1:length(uSite)){
#   ind=dt$nbtrial>1&dt$scenario==30&dt$site_folder==uSite[si]
#   legend="none"
#   if (si==1) {legend=c(0.2, 0.7)}
#   gplot[[si]]=ggplot(subset(dt,ind), aes(y=ACC, x=BirdDaynumber)) + 
#     geom_point(aes(colour=species),size=0.5) +  
#     geom_hline(yintercept=0.25, linetype="dashed", color = "black")+
#     scale_color_manual(values=c("blue","gold","brown"))+
#     stat_smooth(method="lm", formula=y ~ poly(x, 2),aes(colour=species))+  
#     #theme_bw()+
#     ggtitle(uSite[si]) +
#     ylab("Daily accuracy") + xlab("Bird Day Number")+
#     theme_classic()+
#     theme(text = element_text(size=16),legend.position = legend)
# }
# # save graph
# pdf(paste0(out_ls,"SpeciesEffect_30_bySite.pdf"), height=10, width=10)
# grid.arrange(gplot[[1]],gplot[[2]],gplot[[3]],gplot[[4]], nrow=2, ncol=2)
# dev.off()
# 
# 
# 
# 
# # Model 31
# #------------
# ind=dt$nbtrial>1&dt$scenario==31
# m=lmer(sqrt(ACC)~BirdDaynumber*species*site_folder+(1|tag),data=dt,subset =ind )
# sink(paste0(out_ls,"SpeciesEffect_31.txt"))# store erros
# print(anova(m))
# sink()
# hist(residuals(m))
# plot(m)
# 
# 
# # Plot 31
# #------------
# # All sites
# pdf(paste0(out_ls,"SpeciesEffect_31.pdf"))
# ggplot(subset(dt,ind), aes(y=ACC, x=BirdDaynumber)) + 
#   geom_point(aes(colour=species),size=0.5) +  
#   geom_hline(yintercept=0.5, linetype="dashed", color = "black")+
#   scale_color_manual(values=c("blue","gold","brown"))+
#   stat_smooth(method="lm", formula=y ~ poly(x, 2),aes(colour=species))+  
#   #theme_bw()+
#   #ggtitle("Learning ON/OFF") +
#   ylab("Daily accuracy") + xlab("Bird Day Number")+
#   theme_classic()+
#   theme(text = element_text(size=18))
# dev.off() 
# # By study site
# gplot=list()
# for (si in 1:length(uSite)){
#   ind=dt$nbtrial>1&dt$scenario==31&dt$site_folder==uSite[si]
#   legend="none"
#   if (si==1) {legend=c(0.2, 0.7)}
#   gplot[[si]]=ggplot(subset(dt,ind), aes(y=ACC, x=BirdDaynumber)) + 
#     geom_point(aes(colour=species),size=0.5) +  
#     geom_hline(yintercept=0.5, linetype="dashed", color = "black")+
#     scale_color_manual(values=c("blue","gold","brown"))+
#     stat_smooth(method="lm", formula=y ~ poly(x, 2),aes(colour=species))+  
#     #theme_bw()+
#     ggtitle(uSite[si]) +
#     ylab("Daily accuracy") + xlab("Bird Day Number")+
#     theme_classic()+
#     theme(text = element_text(size=16),legend.position = legend)
# }
# # save graph
# pdf(paste0(out_ls,"SpeciesEffect_31_bySite.pdf"), height=10, width=10)
# grid.arrange(gplot[[1]],gplot[[2]],gplot[[3]],gplot[[4]], nrow=2, ncol=2)
# dev.off()
# 
# 
# #--------
# # GT only
# #--------
# # Model 30
# ind=dt$nbtrial>1&dt$scenario==30&dt$species=="Great"
# m=lmer(sqrt(ACC)~BirdDaynumber*(sex+age)*site_folder+(1|tag),data=dt,subset =ind )
# sink(paste0(out_ls,"GT_SexAndAgeEffect_30.txt"))# store erros
# print(anova(m))
# sink()
# hist(residuals(m))
# plot(m)
# 
# 
# # Plot sex
# pdf(paste0(out_ls,"GT_SexEffect_30.pdf"))
# indSex=dt$nbtrial>1&dt$scenario==30&dt$species=="Great"&!is.na(dt$sex)
# ggplot(subset(dt,indSex), aes(y=ACC, x=BirdDaynumber)) + 
#   geom_point(aes(colour=sex),size=0.5) +  
#   geom_hline(yintercept=0.25, linetype="dashed", color = "black")+
#   scale_color_manual(values=c("pink","blue"))+
#   stat_smooth(method="lm", formula=y ~ poly(x, 2),aes(colour=sex))+  
#   #theme_bw()+
#   #ggtitle("Learning ON/OFF") +
#   ylab("Daily accuracy") + xlab("Bird Day Number")+
#   theme_classic()+
#   theme(text = element_text(size=18))
# dev.off() 
# 
# # By study site
# gplot=list()
# for (si in 1:length(uSite)){
#   indSex=dt$nbtrial>1&dt$scenario==30&dt$species=="Great"&!is.na(dt$sex)&dt$site_folder==uSite[si]
#   legend="none"
#   if (si==1) {legend=c(0.2, 0.7)}
#   gplot[[si]]=ggplot(subset(dt,indSex), aes(y=ACC, x=BirdDaynumber)) + 
#     geom_point(aes(colour=sex),size=0.5) +  
#     geom_hline(yintercept=0.25, linetype="dashed", color = "black")+
#     scale_color_manual(values=c("pink","blue"))+
#     stat_smooth(method="lm", formula=y ~ poly(x, 2),aes(colour=sex))+  
#     #theme_bw()+
#     ggtitle(uSite[si]) +
#     ylab("Daily accuracy") + xlab("Bird Day Number")+
#     theme_classic()+
#     theme(text = element_text(size=16),legend.position = legend)
# }
# # save graph
# pdf(paste0(out_ls,"GT_SexEffect_30_bySite.pdf"), height=10, width=10)
# grid.arrange(gplot[[1]],gplot[[2]],gplot[[3]],gplot[[4]], nrow=2, ncol=2)
# dev.off()
# 
# 
# # Plot age
# pdf(paste0(out_ls,"GT_AgeEffect_30.pdf"))
# indAge=dt$nbtrial>1&dt$scenario==30&dt$species=="Great"&!is.na(dt$age)
# ggplot(subset(dt,indAge), aes(y=ACC, x=BirdDaynumber)) + 
#   geom_point(aes(colour=age),size=0.5) +  
#   geom_hline(yintercept=0.25, linetype="dashed", color = "black")+
#   scale_color_manual(values=c("green","orange"))+
#   stat_smooth(method="lm", formula=y ~ poly(x, 2),aes(colour=age))+  
#   #theme_bw()+
#   #ggtitle("Learning ON/OFF") +
#   ylab("Daily accuracy") + xlab("Bird Day Number")+
#   theme_classic()+
#   theme(text = element_text(size=18))
# dev.off() 
# 
# # By study site
# gplot=list()
# for (si in 1:length(uSite)){
#   indAge=dt$nbtrial>1&dt$scenario==30&dt$species=="Great"&!is.na(dt$age)&dt$site_folder==uSite[si]
#   legend="none"
#   if (si==1) {legend=c(0.2, 0.7)}
#   gplot[[si]]=ggplot(subset(dt,indAge), aes(y=ACC, x=BirdDaynumber)) + 
#     geom_point(aes(colour=age),size=0.5) +  
#     geom_hline(yintercept=0.25, linetype="dashed", color = "black")+
#     scale_color_manual(values=c("green","orange"))+
#     stat_smooth(method="lm", formula=y ~ poly(x, 2),aes(colour=age))+  
#     #theme_bw()+
#     ggtitle(uSite[si]) +
#     ylab("Daily accuracy") + xlab("Bird Day Number")+
#     theme_classic()+
#     theme(text = element_text(size=16),legend.position = legend)
# }
# # save graph
# pdf(paste0(out_ls,"GT_AgeEffect_30_bySite.pdf"), height=10, width=10)
# grid.arrange(gplot[[1]],gplot[[2]],gplot[[3]],gplot[[4]], nrow=2, ncol=2)
# dev.off()
# 
# 
# # Model 31
# ind=dt$nbtrial>1&dt$scenario==31&dt$species=="Great"
# m=lmer(sqrt(ACC)~BirdDaynumber*(sex+age)*site_folder+(1|tag),data=dt,subset =ind )
# sink(paste0(out_ls,"GT_SexAndAgeEffect_31.txt"))# store erros
# print(anova(m))
# sink()
# hist(residuals(m))
# plot(m)
# 
# 
# # Plot sex
# pdf(paste0(out_ls,"GT_SexEffect_31.pdf"))
# indSex=dt$nbtrial>1&dt$scenario==31&dt$species=="Great"&!is.na(dt$sex)
# ggplot(subset(dt,indSex), aes(y=ACC, x=BirdDaynumber)) + 
#   geom_point(aes(colour=sex),size=0.5) +  
#   geom_hline(yintercept=0.5, linetype="dashed", color = "black")+
#   scale_color_manual(values=c("pink","blue"))+
#   stat_smooth(method="lm", formula=y ~ poly(x, 2),aes(colour=sex))+  
#   #theme_bw()+
#   #ggtitle("Learning ON/OFF") +
#   ylab("Daily accuracy") + xlab("Bird Day Number")+
#   theme_classic()+
#   theme(text = element_text(size=18))
# dev.off() 
# 
# # By study site
# gplot=list()
# for (si in 1:length(uSite)){
#   indSex=dt$nbtrial>1&dt$scenario==31&dt$species=="Great"&!is.na(dt$sex)&dt$site_folder==uSite[si]
#   legend="none"
#   if (si==1) {legend=c(0.2, 0.7)}
#   gplot[[si]]=ggplot(subset(dt,indSex), aes(y=ACC, x=BirdDaynumber)) + 
#     geom_point(aes(colour=sex),size=0.5) +  
#     geom_hline(yintercept=0.5, linetype="dashed", color = "black")+
#     scale_color_manual(values=c("pink","blue"))+
#     stat_smooth(method="lm", formula=y ~ poly(x, 2),aes(colour=sex))+  
#     #theme_bw()+
#     ggtitle(uSite[si]) +
#     ylab("Daily accuracy") + xlab("Bird Day Number")+
#     theme_classic()+
#     theme(text = element_text(size=16),legend.position = legend)
# }
# # save graph
# pdf(paste0(out_ls,"GT_SexEffect_31_bySite.pdf"), height=10, width=10)
# grid.arrange(gplot[[1]],gplot[[2]],gplot[[3]],gplot[[4]], nrow=2, ncol=2)
# dev.off()
# 
# 
# # Plot age
# pdf(paste0(out_ls,"GT_AgeEffect_31.pdf"))
# indAge=dt$nbtrial>1&dt$scenario==31&dt$species=="Great"&!is.na(dt$age)
# ggplot(subset(dt,indAge), aes(y=ACC, x=BirdDaynumber)) + 
#   geom_point(aes(colour=age),size=0.5) +  
#   geom_hline(yintercept=0.5, linetype="dashed", color = "black")+
#   scale_color_manual(values=c("green","orange"))+
#   stat_smooth(method="lm", formula=y ~ poly(x, 2),aes(colour=age))+  
#   #theme_bw()+
#   #ggtitle("Learning ON/OFF") +
#   ylab("Daily accuracy") + xlab("Bird Day Number")+
#   theme_classic()+
#   theme(text = element_text(size=18))
# dev.off() 
# 
# # By study site
# gplot=list()
# for (si in 1:length(uSite)){
#   indAge=dt$nbtrial>1&dt$scenario==31&dt$species=="Great"&!is.na(dt$age)&dt$site_folder==uSite[si]
#   legend="none"
#   if (si==1) {legend=c(0.2, 0.7)}
#   gplot[[si]]=ggplot(subset(dt,indAge), aes(y=ACC, x=BirdDaynumber)) + 
#     geom_point(aes(colour=age),size=0.5) +  
#     geom_hline(yintercept=0.5, linetype="dashed", color = "black")+
#     scale_color_manual(values=c("green","orange"))+
#     stat_smooth(method="lm", formula=y ~ poly(x, 2),aes(colour=age))+  
#     #theme_bw()+
#     ggtitle(uSite[si]) +
#     ylab("Daily accuracy") + xlab("Bird Day Number")+
#     theme_classic()+
#     theme(text = element_text(size=16),legend.position = legend)
# }
# # save graph
# pdf(paste0(out_ls,"GT_AgeEffect_31_bySite.pdf"), height=10, width=10)
# grid.arrange(gplot[[1]],gplot[[2]],gplot[[3]],gplot[[4]], nrow=2, ncol=2)
# dev.off()
# 
# 
# #--------
# # BT only
# #--------
# # Model 30
# ind=dt$nbtrial>1&dt$scenario==30&dt$species=="Blue"
# m=lmer(sqrt(ACC)~BirdDaynumber*(sex+age)*site_folder+(1|tag),data=dt,subset =ind )
# sink(paste0(out_ls,"BT_SexAndAgeEffect_30.txt"))# store erros
# print(anova(m))
# sink()
# hist(residuals(m))
# plot(m)
# 
# 
# # Plot sex
# pdf(paste0(out_ls,"BT_SexEffect_30.pdf"))
# indSex=dt$nbtrial>1&dt$scenario==30&dt$species=="Blue"&!is.na(dt$sex)
# ggplot(subset(dt,indSex), aes(y=ACC, x=BirdDaynumber)) + 
#   geom_point(aes(colour=sex),size=0.5) +  
#   geom_hline(yintercept=0.25, linetype="dashed", color = "black")+
#   scale_color_manual(values=c("pink","blue"))+
#   stat_smooth(method="lm", formula=y ~ poly(x, 2),aes(colour=sex))+  
#   #theme_bw()+
#   #ggtitle("Learning ON/OFF") +
#   ylab("Daily accuracy") + xlab("Bird Day Number")+
#   theme_classic()+
#   theme(text = element_text(size=18))
# dev.off() 
# 
# # By study site
# gplot=list()
# for (si in 1:length(uSite)){
#   indSex=dt$nbtrial>1&dt$scenario==30&dt$species=="Blue"&!is.na(dt$sex)&dt$site_folder==uSite[si]
#   legend="none"
#   if (si==1) {legend=c(0.2, 0.7)}
#   gplot[[si]]=ggplot(subset(dt,indSex), aes(y=ACC, x=BirdDaynumber)) + 
#     geom_point(aes(colour=sex),size=0.5) +  
#     geom_hline(yintercept=0.25, linetype="dashed", color = "black")+
#     scale_color_manual(values=c("pink","blue"))+
#     stat_smooth(method="lm", formula=y ~ poly(x, 2),aes(colour=sex))+  
#     #theme_bw()+
#     ggtitle(uSite[si]) +
#     ylab("Daily accuracy") + xlab("Bird Day Number")+
#     theme_classic()+
#     theme(text = element_text(size=16),legend.position = legend)
# }
# # save graph
# pdf(paste0(out_ls,"BT_SexEffect_30_bySite.pdf"), height=10, width=10)
# grid.arrange(gplot[[1]],gplot[[2]],gplot[[3]],gplot[[4]], nrow=2, ncol=2)
# dev.off()
# 
# 
# # Plot age
# pdf(paste0(out_ls,"BT_AgeEffect_30.pdf"))
# indAge=dt$nbtrial>1&dt$scenario==30&dt$species=="Blue"&!is.na(dt$age)
# ggplot(subset(dt,indAge), aes(y=ACC, x=BirdDaynumber)) + 
#   geom_point(aes(colour=age),size=0.5) +  
#   geom_hline(yintercept=0.25, linetype="dashed", color = "black")+
#   scale_color_manual(values=c("green","orange"))+
#   stat_smooth(method="lm", formula=y ~ poly(x, 2),aes(colour=age))+  
#   #theme_bw()+
#   #ggtitle("Learning ON/OFF") +
#   ylab("Daily accuracy") + xlab("Bird Day Number")+
#   theme_classic()+
#   theme(text = element_text(size=18))
# dev.off() 
# 
# # By study site
# gplot=list()
# for (si in 1:length(uSite)){
#   indAge=dt$nbtrial>1&dt$scenario==30&dt$species=="Blue"&!is.na(dt$age)&dt$site_folder==uSite[si]
#   legend="none"
#   if (si==1) {legend=c(0.2, 0.7)}
#   gplot[[si]]=ggplot(subset(dt,indAge), aes(y=ACC, x=BirdDaynumber)) + 
#     geom_point(aes(colour=age),size=0.5) +  
#     geom_hline(yintercept=0.25, linetype="dashed", color = "black")+
#     scale_color_manual(values=c("green","orange"))+
#     stat_smooth(method="lm", formula=y ~ poly(x, 2),aes(colour=age))+  
#     #theme_bw()+
#     ggtitle(uSite[si]) +
#     ylab("Daily accuracy") + xlab("Bird Day Number")+
#     theme_classic()+
#     theme(text = element_text(size=16),legend.position = legend)
# }
# # save graph
# pdf(paste0(out_ls,"BT_AgeEffect_30_bySite.pdf"), height=10, width=10)
# grid.arrange(gplot[[1]],gplot[[2]],gplot[[3]],gplot[[4]], nrow=2, ncol=2)
# dev.off()
# 
# 
# # Model 31
# ind=dt$nbtrial>1&dt$scenario==31&dt$species=="Blue"
# m=lmer(sqrt(ACC)~BirdDaynumber*(sex+age)*site_folder+(1|tag),data=dt,subset =ind )
# sink(paste0(out_ls,"BT_SexAndAgeEffect_31.txt"))# store erros
# print(anova(m))
# sink()
# hist(residuals(m))
# plot(m)
# 
# 
# # Plot sex
# pdf(paste0(out_ls,"BT_SexEffect_31.pdf"))
# indSex=dt$nbtrial>1&dt$scenario==31&dt$species=="Blue"&!is.na(dt$sex)
# ggplot(subset(dt,indSex), aes(y=ACC, x=BirdDaynumber)) + 
#   geom_point(aes(colour=sex),size=0.5) +  
#   geom_hline(yintercept=0.5, linetype="dashed", color = "black")+
#   scale_color_manual(values=c("pink","blue"))+
#   stat_smooth(method="lm", formula=y ~ poly(x, 2),aes(colour=sex))+  
#   #theme_bw()+
#   #ggtitle("Learning ON/OFF") +
#   ylab("Daily accuracy") + xlab("Bird Day Number")+
#   theme_classic()+
#   theme(text = element_text(size=18))
# dev.off() 
# 
# # By study site
# gplot=list()
# for (si in 1:length(uSite)){
#   indSex=dt$nbtrial>1&dt$scenario==31&dt$species=="Blue"&!is.na(dt$sex)&dt$site_folder==uSite[si]
#   legend="none"
#   if (si==1) {legend=c(0.2, 0.7)}
#   gplot[[si]]=ggplot(subset(dt,indSex), aes(y=ACC, x=BirdDaynumber)) + 
#     geom_point(aes(colour=sex),size=0.5) +  
#     geom_hline(yintercept=0.5, linetype="dashed", color = "black")+
#     scale_color_manual(values=c("pink","blue"))+
#     stat_smooth(method="lm", formula=y ~ poly(x, 2),aes(colour=sex))+  
#     #theme_bw()+
#     ggtitle(uSite[si]) +
#     ylab("Daily accuracy") + xlab("Bird Day Number")+
#     theme_classic()+
#     theme(text = element_text(size=16),legend.position = legend)
# }
# # save graph
# pdf(paste0(out_ls,"BT_SexEffect_31_bySite.pdf"), height=10, width=10)
# grid.arrange(gplot[[1]],gplot[[2]],gplot[[3]],gplot[[4]], nrow=2, ncol=2)
# dev.off()
# 
# 
# # Plot age
# pdf(paste0(out_ls,"BT_AgeEffect_31.pdf"))
# indAge=dt$nbtrial>1&dt$scenario==31&dt$species=="Blue"&!is.na(dt$age)
# ggplot(subset(dt,indAge), aes(y=ACC, x=BirdDaynumber)) + 
#   geom_point(aes(colour=age),size=0.5) +  
#   geom_hline(yintercept=0.5, linetype="dashed", color = "black")+
#   scale_color_manual(values=c("green","orange"))+
#   stat_smooth(method="lm", formula=y ~ poly(x, 2),aes(colour=age))+  
#   #theme_bw()+
#   #ggtitle("Learning ON/OFF") +
#   ylab("Daily accuracy") + xlab("Bird Day Number")+
#   theme_classic()+
#   theme(text = element_text(size=18))
# dev.off() 
# 
# # By study site
# gplot=list()
# for (si in 1:length(uSite)){
#   indAge=dt$nbtrial>1&dt$scenario==31&dt$species=="Blue"&!is.na(dt$age)&dt$site_folder==uSite[si]
#   legend="none"
#   if (si==1) {legend=c(0.2, 0.7)}
#   gplot[[si]]=ggplot(subset(dt,indAge), aes(y=ACC, x=BirdDaynumber)) + 
#     geom_point(aes(colour=age),size=0.5) +  
#     geom_hline(yintercept=0.5, linetype="dashed", color = "black")+
#     scale_color_manual(values=c("green","orange"))+
#     stat_smooth(method="lm", formula=y ~ poly(x, 2),aes(colour=age))+  
#     #theme_bw()+
#     ggtitle(uSite[si]) +
#     ylab("Daily accuracy") + xlab("Bird Day Number")+
#     theme_classic()+
#     theme(text = element_text(size=16),legend.position = legend)
# }
# # save graph
# pdf(paste0(out_ls,"BT_AgeEffect_31_bySite.pdf"), height=10, width=10)
# grid.arrange(gplot[[1]],gplot[[2]],gplot[[3]],gplot[[4]], nrow=2, ncol=2)
# dev.off()
# 
# #----------------------
# # Individual differences in learning
# #----------------------
# # SC30
# #-----
# # great
# ind=dt$nbtrial>30&dt$scenario==30&dt$species=="Great"
# m1=lmer(sqrt(ACC)~BirdDaynumber+(1|tag),data=dt,subset =ind )
# m2=lmer(sqrt(ACC)~BirdDaynumber+(BirdDaynumber|tag),data=dt,subset =ind )
# 
# 
# sink(paste0(out_ls,"IndivDifference_Great_30.txt"))# store erros
# print(anova(m1,m2))
# sink()
# hist(residuals(m1))
# plot(m1)
# 
# hist(residuals(m2))
# plot(m2)
# 
# # blue
# ind=dt$nbtrial>30&dt$scenario==30&dt$species=="Blue"
# m1=lmer(sqrt(ACC)~BirdDaynumber+(1|tag),data=dt,subset =ind )
# m2=lmer(sqrt(ACC)~BirdDaynumber+(BirdDaynumber|tag),data=dt,subset =ind )
# 
# 
# sink(paste0(out_ls,"IndivDifference_Blue_30.txt"))# store erros
# print(anova(m1,m2))
# sink()
# hist(residuals(m1))
# plot(m1)
# 
# hist(residuals(m2))
# plot(m2)
# 
# # marsh
# ind=dt$nbtrial>30&dt$scenario==30&dt$species=="Marsh"
# m1=lmer(sqrt(ACC)~BirdDaynumber+(1|tag),data=dt,subset =ind )
# m2=lmer(sqrt(ACC)~BirdDaynumber+(BirdDaynumber|tag),data=dt,subset =ind )
# 
# 
# sink(paste0(out_ls,"IndivDifference_Marsh_30.txt"))# store erros
# print(anova(m1,m2))
# sink()
# hist(residuals(m1))
# plot(m1)
# 
# hist(residuals(m2))
# plot(m2)
# 
# # SC31
# #-----
# # great
# ind=dt$nbtrial>30&dt$scenario==31&dt$species=="Great"
# m1=lmer(sqrt(ACC)~BirdDaynumber+(1|tag),data=dt,subset =ind )
# m2=lmer(sqrt(ACC)~BirdDaynumber+(BirdDaynumber|tag),data=dt,subset =ind )
# 
# 
# sink(paste0(out_ls,"IndivDifference_Great_31.txt"))# store erros
# print(anova(m1,m2))
# sink()
# hist(residuals(m1))
# plot(m1)
# 
# hist(residuals(m2))
# plot(m2)
# 
# # blue
# ind=dt$nbtrial>30&dt$scenario==31&dt$species=="Blue"
# m1=lmer(sqrt(ACC)~BirdDaynumber+(1|tag),data=dt,subset =ind )
# m2=lmer(sqrt(ACC)~BirdDaynumber+(BirdDaynumber|tag),data=dt,subset =ind )
# 
# 
# sink(paste0(out_ls,"IndivDifference_Blue_31.txt"))# store erros
# print(anova(m1,m2))
# sink()
# hist(residuals(m1))
# plot(m1)
# 
# hist(residuals(m2))
# plot(m2)
# 
# # marsh
# ind=dt$nbtrial>30&dt$scenario==31&dt$species=="Marsh"
# m1=lmer(sqrt(ACC)~BirdDaynumber+(1|tag),data=dt,subset =ind )
# m2=lmer(sqrt(ACC)~BirdDaynumber+(BirdDaynumber|tag),data=dt,subset =ind )
# 
# 
# sink(paste0(out_ls,"IndivDifference_Marsh_31.txt"))# store erros
# print(anova(m1,m2))
# sink()
# hist(residuals(m1))
# plot(m1)
# 
# hist(residuals(m2))
# plot(m2)
# 
# 
# 
# 
# 
# #---------------------
# # Participation
# #---------------------
# 
# # load individual variables
# #---------------------
# di=read.table(paste(out_f,"alldata.csv",sep=""),h=T)
# 
# ttc=di$TTC10
# # species
# #---------
# x=table(di$scenario[di$scenario==2],di$species[di$scenario==2])
# x=rbind(x,x,x)
# # absolute
# abs=table(di$scenario[!is.na(ttc)],di$species[!is.na(ttc)])
# print("Absolute numbber of bird")
# print(abs)
# #% participate
# print("%  of bird learning compare to bird present in habituation")
# print(round(abs/x,2))
# # % learn
# print("%  of bird learning the task")
# print(round(abs/table(di$scenario[di$scenario>2],di$species[di$scenario>2]),2))
# # % participate above chance in the last day
# absLastDay=table(di$scenario[di$LastDayAboveChance=="AboveChance"],di$species[di$LastDayAboveChance=="AboveChance"])
# # % participate above in at least a day
# abs1Day=table(di$scenario[!is.na(di$firstDayAboveChance)],di$species[!is.na(di$firstDayAboveChance)])
# # increase from first to last
# abs1DayvsLast=table(di$scenario[di$firstVSlastDayMore30trialsTest<0.05&di$firstVSlastDayMore30trialsES>0&!is.na(di$firstVSlastDayMore30trialsTest)],di$species[di$firstVSlastDayMore30trialsTest<0.05&di$firstVSlastDayMore30trialsES>0&!is.na(di$firstVSlastDayMore30trialsTest)])
# 
# 
# # by site
# #---------
# x=table(di$scenario[di$scenario==2],di$site[di$scenario==2])
# x=rbind(x,x,x)
# # absolute
# abs=table(di$scenario[!is.na(ttc)],di$site[!is.na(ttc)])
# print("Absolute number of bird")
# print(abs)
# #% participate
# print("%  of bird learning compare to bird present in habituation")
# print(round(abs/x,2))
# # % learn
# print("%  of bird learning the task")
# print(round(abs/table(di$scenario[di$scenario>2],di$site[di$scenario>2]),2))
# 
# # by age
# #---------
# x=table(di$scenario[di$scenario==2],di$age[di$scenario==2])
# x=rbind(x,x)
# # absolute
# abs=table(di$scenario[!is.na(ttc)],di$age[!is.na(ttc)])
# print("Absolute number of bird")
# print(abs)
# #% participate
# print("%  of bird learning compare to bird present in habituation")
# print(round(abs/x,2))
# # % learn
# print("%  of bird learning the task")
# print(round(abs/table(di$scenario[di$scenario>2],di$age[di$scenario>2]),2))
# 
# 
# # by sex
# #---------
# x=table(di$scenario[di$scenario==2],di$sex[di$scenario==2])
# x=rbind(x,x)
# # absolute
# abs=table(di$scenario[!is.na(ttc)],di$sex[!is.na(ttc)])
# print("Absolute number of bird")
# print(abs)
# #% participate
# print("%  of bird learning compare to bird present in habituation")
# print(round(abs/x,2))
# # % learn
# print("%  of bird learning the task")
# print(round(abs/table(di$scenario[di$scenario>2],di$sex[di$scenario>2]),2))
# 
# # ttc all
# print("Median ttc all")
# print(median(di$TTC10,na.rm=T))
# print("Meadian Nb day to learn all")
# print(median(di$dayTC_10,na.rm=T))
# 
# print("Median ttc 30")
# print(median(di$TTC10[di$scenario==30],na.rm=T))
# print("Meadian Nb day to learn 30")
# print(median(di$dayTC_10[di$scenario==30],na.rm=T))
# 
# print("Median ttc 31")
# print(median(di$TTC10[di$scenario==31],na.rm=T))
# print("Meadian Nb day to learn 31")
# print(median(di$dayTC_10[di$scenario==31],na.rm=T))
# 
# print("Median ttc 32")
# print(median(di$TTC10[di$scenario==32],na.rm=T))
# print("Meadian Nb day to learn 32")
# print(median(di$dayTC_10[di$scenario==32],na.rm=T))
# 
# 
# median(di$TTC20,na.rm=T)
# median(di$TTC30,na.rm=T)
# 
# #BACKUP
# #-----------------------
# # Time serie
# #-----------------------
# # dtTS <- start_event(dt, column="dayDate", event=c("tag", "BirdDaynumber"), label.event="Event")
# # head(dtTS)
# # m1 <- bam(ACC ~ s( BirdDaynumber)+s(tag, bs='re'), data=dtTS)
# 
# #-----------------------
# # Other methods
# #-----------------------
# 
# 
# # m=glmer(ACC~scale(BirdDaynumber)*species+(1|tag),data=dt,subset =ind,weights = dt$nbtrial,family = "binomial" )
# # sink(paste0(out_ls,"SpeciesEffect_30.txt"))# store erros
# # print(drop1(m))
# # sink()
# # hist(residuals(m))
# # plot(m)
# 
# # # GAMM
# # m1=gamm(ACC~s(BirdDaynumber)+species,random = list(tag=~1),data=dt,subset =ind,
# #         weights = nbtrial,family = "quasibinomial")
# # m2=gamm(ACC~s(BirdDaynumber,by =species)+species,random = list(tag=~1),data=dt,subset =ind,
# #         weights = nbtrial,family = "quasibinomial")
# # 
# # AIC(m1$lme,m2$lme)
# 
# 
# 
# 
# # # with BAM
# # m1 <- bam(ACC ~ species + s(BirdDaynumber, by=species) +
# #            s(tag, bs='re'), 
# #           data=dt, discrete=TRUE, method="fREML",
# #           weights = nbtrial,family = "quasibinomial",subset = ind)
# # 
# # m2 <- bam(ACC ~ species + s(BirdDaynumber) +
# #             s(tag, bs='re'), 
# #           data=dt, discrete=TRUE, method="fREML",
# #           weights = nbtrial,family = "quasibinomial",subset = ind)
# # 
# # plot(m2$gam$fitted.values,resid(m2$gam))
# # plot(dt$species[ind],resid(m2$gam))
# # plot(dt$BirdDaynumber[ind],resid(m2$gam))
# # plot(m2$gam)
# # summary(m2$gam)
# 
# 
