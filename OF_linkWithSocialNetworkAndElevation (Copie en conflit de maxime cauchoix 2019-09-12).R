# Look for cognition link with other variables
#-------------------
# load
#-------
# social networks
sn=read.table(paste0(out_local,"Network_nodeMetrics.csv"))
sn$tag=sn$ID
di=read.table(paste(out_f,"alldata.csv",sep=""),h=T)
print(paste(sum(unique(sn$tag) %in% unique(di$tag)),"with network data"))


# merge
all=merge(di,sn,by="tag",all.x=T)

# plot SN
#----------------

for (s in 30:32){
ind=all$scenario==s#&all$gs>10
pdf(paste0(out_fit,"SN_AccFi100_eigenvector",s, ".pdf"))
ggplot(subset(all,ind), aes(y=AccFi100, x=eigenvector)) + geom_point(aes(colour=species.x,size=8)) +  stat_smooth(method="lm", formula=y~x^2,aes(colour=species.x))+ scale_color_manual(values=c("blue","gold","brown"))
dev.off() 

pdf(paste0(out_fit,"SN_AccFi100_degree",s, ".pdf"))
ggplot(subset(all,ind), aes(y=AccFi100, x=degree)) + geom_point(aes(colour=species.x,size=8)) +  stat_smooth(method="lm", formula=y~x^2,aes(colour=species.x))+ scale_color_manual(values=c("blue","gold","brown"))
dev.off() 

pdf(paste0(out_fit,"SN_AccFi100_gs",s, ".pdf"))
ggplot(subset(all,ind), aes(y=AccFi100, x=gs)) + geom_point(aes(colour=species.x,size=8)) +  stat_smooth(method="lm", formula=y~x^2,aes(colour=species.x))+ scale_color_manual(values=c("blue","gold","brown"))
dev.off() 

}

# Difference by elevation
estim<- dabest(
  di[!is.na(di$AccFi100),],elevation,AccFi100,idx=c("high","low"),
  paired = FALSE
)
plot(estim,rawplot.ylabel="Accuracy")



summary(lm(AccFi100~degree+sex+age+elevation+eigenvector+gs,data=all,subset=all$scenario=="30"&all$species.x=="Blue"))

summary(lm(AccFi100~degree+sex+age+elevation+eigenvector+gs,data=all,subset=all$scenario=="30"&all$species.x=="Great"))

summary(lm(AccFi100~degree+age+elevation+eigenvector+gs,data=all,subset=all$scenario=="30"&all$species.x=="Marsh"))




# plot
plot(all$degree[all$scenario==31],all$AccFi100[all$scenario==31])
# model
m=lmer(log(TTC)~scenario+species.x+degree+betweenness+closeness+elevation+eigenvector+gs+(1|ID),data=all)
summary(m)
stats::step(m)


m=lm(AccFi100~elevation+gs+degree+species.x,data=all,subset = di$scenario==30)
summary(m)
stats::step(m)

di[di$scenario==32&!is.na(di$TTC),]


# Model selection
#------

all$degree=scale(all$degree)
all$eigenvector=scale(all$eigenvector)
all$gs=scale(all$gs)



# no na in the dataset
all_nona=na.omit(all[,c('AccDi100','scenario','degree','age','sex','site.x','species.x','betweenness','closeness','elevation','eigenvector','gs','tag')])

# look for colinearity
#ggpairs(all_nona[,c(1:9)])
# build full model
all.parms<-lmer(AccDi100~scenario+species.x+age+sex+degree+elevation+eigenvector+gs+(1|tag),data=all_nona)

# Great
all.parms<-lm(AccDi100~age+degree+elevation+eigenvector+gs,data=all_nona,subset = all_nona$scenario==30&all_nona$species.x=="Blue")
summary(all.parms)
# change na. action 
options(na.action = "na.fail") 
results<-dredge(all.parms) 
options(na.action = "na.omit") 
# importance of each param
a=importance(subset(results, delta <2))
#a=importance(results)
barplot(t(a))
# best model
subset(results, delta <2) 
# model averaging
MA.ests<-model.avg(results, subset= delta < 2, revised.var = TRUE) 
MA.ests$avg.model 


# 13- Environement  and cognition
#------------------------------
m=lmer(AccFi100~scenario*species*elevation+(1|site/tag),data=di)
summary(m)

m=lmer(AccFi100~elevation+(1|tag),data=di,subset = di$AboveChance100)
summary(m)


m=lmer(AccFin~scenario*species*elevation+(1|site/tag),data=di)
summary(m)

m=lm(AccDi50~species*elevation,data=di,subset = di$scenario==32)
summary(m)

# Difference by elevation
estim<- dabest(
  di[di$scenario==30&!is.na(di$AccTot),],elevation,,idx=c("high","low"),
  paired = FALSE
)
plot(estim,rawplot.ylabel="Accuracy")

# Difference by elevation
estim<- dabest(
  di[di$scenario==31&!is.na(di$AccDif)&di$species=='Great',],elevation,AccDif,idx=c("high","low"),
  paired = FALSE
)
plot(estim,rawplot.ylabel="Accuracy")

# Difference by elevation
estim<- dabest(
  di[di$scenario==31&!is.na(di$AccDif)&di$species=='Great',],site,AccDif,idx=c("C1","M1","BA","C4"),
  paired = FALSE
)
plot(estim,rawplot.ylabel="Accuracy")


# Difference by elevation: nb trial
estim<- dabest(
  di[di$scenario==31&!is.na(di$nbtm)&di$species=='Great',],elevation,nbtm,idx=c("high","low"),
  paired = FALSE
)
plot(estim,rawplot.ylabel="Nb trial")



# Difference by species
estim<- dabest(
  di[di$scenario==32&!is.na(di$AccDi50)&di$elevation=='high',],species,AccDi50,idx=c("Great","Blue","Marsh"),
  paired = FALSE
)
# pdf(paste0(out_f,"Estimation_learning_Accuracy 100 trials_Scenario30_Species effect.pdf"), height=4, width=8)
# plot(estim,rawplot.ylabel="Accuracy")
# dev.off()








