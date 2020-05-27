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
DTA=DT[,list(ACC =mean(ACC),site=unique(site_folder),species=unique(species),elevation=unique(elevation),scenario=unique(scenario),nbtrial =mean(nbtrial)), by="day,tag"]
dta=as.data.frame(DTA)
dta$dayDate=as.Date(dta$day,"%d/%m/%y")

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
dtas=as.data.frame(DTAS)


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
