# Look for cognition link with other variables
#-------------------
# load
#-------
# social networks
sn=read.table(paste0(out_local,"Network_nodeMetrics.csv"))
sn$tag=sn$ID
di=read.table(paste(out_f,"alldata.csv",sep=""),h=T)
print(paste(sum(unique(sn$tag) %in% unique(di$tag)),"with network data"))



# dJosh=di[!duplicated(di$tag),1:13]
# write.table(dJosh,paste(out_local,"networkANDcognitionbird.csv"),sep=";",row.names = F)

# merge
all=merge(di,sn,by="tag",all.x=T)

# Exploration plot 
#--------------------
# SN
for (s in 30:32){
  ind=all$scenario==s#&all$gs>10
  pdf(paste0(out_fit,"SN_AccFi100_eigenvector",s, ".pdf"))
  ggplot(subset(all,ind), aes(y=AccFi100, x=eigenvector)) + geom_point(aes(colour=species.x,size=8)) +  stat_smooth(method="lm", formula=y~x^2,aes(colour=species.x))+ scale_color_manual(values=c("blue","gold","brown"))
  dev.off() 
  
  pdf(paste0(out_fit,"SN_AccFi100_degree",s, ".pdf"))
  ggplot(subset(all,ind), aes(y=AccFi100, x=degree)) + geom_point(aes(colour=species.x,size=8)) +  stat_smooth(method="lm", formula=y~x^2,aes(colour=species.x))+ scale_color_manual(values=c("blue","gold","brown"))
  dev.off() 
  
  pdf(paste0(out_fit,"SN_AccFi100gs",s, ".pdf"))
  ggplot(subset(all,ind), aes(y=AccFi100, x=gs)) + geom_point(aes(colour=species.x,size=8)) +  stat_smooth(method="lm", formula=y~x^2,aes(colour=species.x))+ scale_color_manual(values=c("blue","gold","brown"))
  dev.off() 
  
  
  pdf(paste0(out_fit,"SN_AccPostTTC10_eigenvector",s, ".pdf"))
  ggplot(subset(all,ind), aes(y=AccPostTTC10, x=eigenvector)) + geom_point(aes(colour=species.x,size=8)) +  stat_smooth(method="lm", formula=y~x^2,aes(colour=species.x))+ scale_color_manual(values=c("blue","gold","brown"))
  dev.off() 
  
  pdf(paste0(out_fit,"SN_AccPostTTC10_degree",s, ".pdf"))
  ggplot(subset(all,ind), aes(y=AccPostTTC10, x=degree)) + geom_point(aes(colour=species.x,size=8)) +  stat_smooth(method="lm", formula=y~x^2,aes(colour=species.x))+ scale_color_manual(values=c("blue","gold","brown"))
  dev.off() 
  
  pdf(paste0(out_fit,"SN_AccPostTTC10_gs",s, ".pdf"))
  ggplot(subset(all,ind), aes(y=AccPostTTC10, x=between)) + geom_point(aes(colour=species.x,size=8)) +  stat_smooth(method="lm", formula=y~x^2,aes(colour=species.x))+ scale_color_manual(values=c("blue","gold","brown"))
  dev.off() 
  
}

# all scenario degree
pdf(paste0(out_fit,"SN_AccPostTTC10_degree_all.pdf"))
ggplot(subset(all,species.x!="Marsh"), aes(y=AccPostTTC10, x=degree)) + geom_point(aes(colour=species.x,size=8)) +  stat_smooth(method="lm", formula=y~x^2,aes(colour=species.x))+ scale_color_manual(values=c("blue","gold","brown"))
dev.off() 

# all scenario degree
pdf(paste0(out_fit,"SN_AccPostTTC10_eigenvector_all.pdf"))
ggplot(subset(all,species.x!="Marsh"), aes(y=AccPostTTC10, x=eigenvector)) + geom_point(aes(colour=species.x,size=8)) +  stat_smooth(method="lm", formula=y~x^2,aes(colour=species.x))+ scale_color_manual(values=c("blue","gold","brown"))
dev.off() 


# # all scenario
# pdf(paste0(out_fit,"SN_AccPostTTC10_betweenness_Blue.pdf"))
# all$scenario=as.factor(all$scenario)
# ggplot(subset(all,species.x="Blue"&scenario>29), aes(y=AccPostTTC10, x=betweenness)) + geom_point(aes(colour=scenario,size=8)) +  stat_smooth(method="lm", formula=y~x^2,aes(colour=scenario))+ scale_color_manual(values=c("red","black","grey"))
# dev.off() 



# 
# # Elevation
# estim<- dabest(
#   di[!is.na(di$AccPostTTC10),],elevation,AccPostTTC10,idx=c("high","low"),
#   paired = FALSE
# )
# pdf(paste0(out_fit,"Elevation_AccPostTTC10",s, ".pdf"))
# plot(estim,rawplot.ylabel="Accuracy")
# dev.off()
# 
# # Elevation AccTot
# estim<- dabest(
#   di[!is.na(di$AccTot),],elevation,AccTot,idx=c("high","low"),
#   paired = FALSE
# )
# pdf(paste0(out_fit,"Elevation_AccTot",s, ".pdf"))
# plot(estim,rawplot.ylabel="Accuracy")
# dev.off()

# Difference by elevation by species
ind=di$species=="Great"

estim<- dabest(
  di[!is.na(di$AccPostTTC10)&ind,],elevation,AccPostTTC10,idx=c("high","low"),
  paired = FALSE
)
plot(estim,rawplot.ylabel="Accuracy")



# Difference by site
ind=di$species=="Blue"&di$scenario==31

estim<- dabest(
  di[!is.na(di$AccDif)&ind,],site,AccDif,idx=c("M1","C1","C4","BA"),
  paired = FALSE
)
plot(estim,rawplot.ylabel="Accuracy")



summary(lm(AccFi100~degree+sex+age+elevation+eigenvector+gs,data=all,subset=all$scenario=="30"&all$species.x=="Blue"))

summary(lm(AccFi100~degree+sex+age+elevation+eigenvector+gs,data=all,subset=all$scenario=="30"&all$species.x=="Great"))

summary(lm(AccFi100~degree+age+elevation+eigenvector+gs,data=all,subset=all$scenario=="30"&all$species.x=="Marsh"))




# plot
plot(all$degree[all$scenario==31],all$AccFi100[all$scenario==31])
# model
m=lmer(log(TTC10)~scenario+species.x+degree+betweenness+closeness+elevation+eigenvector+(1|ID),data=all)
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
all_nona=na.omit(all[,c('AccFi100','AccPostTTC10','scenario','degree','age','sex','site.x','species.x','betweenness','closeness','elevation','eigenvector','gs','tag')])

# BLUE
all.parms<-lmer(AccPostTTC10~scenario+age+eigenvector+degree+elevation+(1|tag),data=all_nona,subset =all_nona$species.x=="Blue" )
summary(all.parms)
# change na. action 
options(na.action = "na.fail") 
results<-dredge(all.parms) 
options(na.action = "na.omit") 
# importance of each param
a=importance(subset(results, delta <2))
a=importance(results)
pdf(paste0(out_fit,"Importance_Blue.pdf"),height = 4,width=8)
barplot(t(a))
dev.off()

# Great
all.parms<-lmer(AccPostTTC10~scenario+age+sex+degree+elevation+eigenvector+(1|tag),data=all_nona,subset =all_nona$species.x=="Great" )
summary(all.parms)
# change na. action 
options(na.action = "na.fail") 
results<-dredge(all.parms) 
options(na.action = "na.omit") 
# importance of each param
a=importance(subset(results, delta <2))
a=importance(results)
pdf(paste0(out_fit,"Importance_Great.pdf"),height = 4,width=8)
barplot(t(a))
dev.off()


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








