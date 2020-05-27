# compute individual descritptive statistics
#-------------------------------------------
d$dayDate=as.Date(d$day,"%d/%m/%y")


# initial table with nbtrial by scenario
DT <- as.data.table(d[d$scenario<32,])
DT=DT[,list(nbt =.N,species=unique(species),sex=unique(sex),age=unique(age),site=unique(site_folder),elevation=unique(elevation)), by="scenario,dayDate,tag"]
dd=as.data.frame(DT)

DT <- as.data.table(dd)
DT=DT[,list(nbtm =mean(nbt),nbts=sum(nbt),nbd=length(unique(dayDate)),nbdsup=length(unique(dayDate[nbt>10])),species=unique(species),sex=unique(sex),age=unique(age),site=unique(site),elevation=unique(elevation)), by="scenario,tag"]
di=as.data.frame(DT)

dl=d[d$scenario %in% c(30,31),]

uTag=unique(di$tag)
for(i in 1:length(uTag)){
  print(i)
  ind=dl$tag==uTag[i]
  # total number of trials
  di$nbtt[di$tag==uTag[i]]=sum(di$nbts[di$tag==uTag[i]])
  # Acc tot by scenario 30 and 31
  di$AccTot[di$tag==uTag[i]&di$scenario==30]=mean(dl$door.open[ind&dl$scenario=='30'])
  di$AccTot[di$tag==uTag[i]&di$scenario==31]=mean(dl$door.open[ind&dl$scenario=='31'])
  # Acc final
  day30=unique(dd$dayDate[dd$tag==uTag[i]&dd$scenario=='30'&dd$nbt>10])
  day30=sort(day30)
  if (length(day30)>4){# take only birds with 10 days of experiment with more than 10 trials a day
    di$AccFin[di$tag==uTag[i]&di$scenario==30]=mean(dl$door.open[ind&dl$scenario=='30'&dl$dayDate %in% c(day30[(length(day30)-2):length(day30)])])
  }
  day31=unique(dd$dayDate[dd$tag==uTag[i]&dd$scenario=='31'&dd$nbt>10])
  day31=sort(day31)
  if (length(day31)>4){# take only birds with 10 days of experiment with more than 10 trials a day
    di$AccFin[di$tag==uTag[i]&di$scenario==31]=mean(dl$door.open[ind&dl$scenario=='31'&dl$dayDate %in% c(day31[(length(day31)-2):length(day31)])])
  }
  
  day31=unique(dl$dayDate[ind&dl$scenario=='31'])
  di$AccFin[di$tag==uTag[i]&di$scenario==31]=mean(dl$door.open[ind&dl$scenario=='31'&dl$dayDate>(max(day31)-3)])
  
  # Acc post
  # TTC
}
#save
write.table(di,paste(out_f,"alldata.csv",sep=""))


rep=rpt(AccFin~species+(1|tag), grname = c("tag"), data = di, datatype = "Gaussian",nboot = nboot, npermut = npermut) 
#store data
res=data.frame('All')
names(res)="species"
res$R=round(rep$R[[1]],2)
res$lowCI=round(rep$CI_emp[[1]],2)
res$highCI=round(rep$CI_emp[[2]],2) 
res$SS=length(unique(di$tag[di$scenario>29]))
repName=paste0('R=',round(rep$R[[1]],2),' [',round(rep$CI_emp[[1]],2),' ',round(rep$CI_emp[[2]],2),']')

# on nb trial
rep=rpt(nbts~species+site+(1|tag), grname = c("tag"), data = di[di$scenario>29,], datatype = "Gaussian",nboot = nboot, npermut = npermut) 
res$Rnbtrial=round(rep$R[[1]],2)



# visualise correlation

cor30=di[di$scenario==30,]
cor31=di[di$scenario==31,]
co=merge(cor30,cor31,by="tag", all.x=T,sort=F)
co$species=as.factor(co$species.x)
col=c()
col[co$species=="Marsh"]="brown"
col[co$species=="Great"]="gold"
col[co$species=="Blue"]="blue"

pdf(paste(out_f,'Learning Repeatability.pdf'))
plot(co$AccFin.x,co$AccFin.y,col=col,xlab = "Accuracy (Go Nogo task)",ylab="Accuracy (L/R task)",pch=19,main=repName)
abline(h=0.5, col = "gray60",lty=2)
abline(v=0.25, col = "gray60",lty=2)
legend(0.8,0.55,legend = c("Great","Blue","Marsh"),col=c("gold","blue","brown"),pch=19,cex=1,y.intersp=1,bty="n")
dev.off()

# Rep by species
uSpe=unique(di$species)
for (s in 1:length(uSpe)){
  rep=rpt(AccFin~site+(1|tag), grname = c("tag"), data = di[di$species==uSpe[s],], datatype = "Gaussian",nboot = nboot, npermut = npermut) 
  # on nb trial
  rep2=rpt(nbts~site+(1|tag), grname = c("tag"), data = di[di$species==uSpe[s]&di$scenario>29,], datatype = "Gaussian",nboot = nboot, npermut = npermut) 
  
  
  temp=data.frame(uSpe[s],round(rep$R[[1]],2),round(rep$CI_emp[[1]],2),round(rep$CI_emp[[2]],2),length(unique(di$tag[di$species==uSpe[s]&di$scenario>29])),round(rep2$R[[1]],2))
  names(temp)=c("species","R","lowCI","highCI","SS","Rnbtrial")
  res = rbind(res,temp)
}
# Export
pdf(paste0(out_f,"RepeatabilityACC_last3days_by Species.pdf"), height=3, width=3)
grid.table(res,rows =c())
dev.off()
