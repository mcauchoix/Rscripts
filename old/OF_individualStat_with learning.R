# compute individual descritptive statistics
#-------------------------------------------


# initial table with nbtrial by scenario
DT <- as.data.table(d)
DT=DT[,list(nbt =.N,species=unique(species),bandNumber=unique(bandNumber),sex=unique(sex),age=unique(age),site=unique(site_folder),elevation=unique(elevation)), by="scenario,dayDate,tag"]
dd=as.data.frame(DT)

# nbtm= mean number of trial per day,nbts=total number of trail by scenario,nbd= number of days,
DT <- as.data.table(dd)
DT=DT[,list(nbtm =mean(nbt),nbts=sum(nbt),nbd=length(unique(dayDate)),nbdsup=length(unique(dayDate[nbt>29])),species=unique(species),sex=unique(sex),bandNumber=unique(bandNumber),age=unique(age),site=unique(site),elevation=unique(elevation)), by="scenario,tag"]
di=as.data.frame(DT)

# keep only learning scenario
dl=d[d$scenario>29,]
# # sort trials by time
# dl$fullTime=paste(dl$day,dl$hour)
# time=strptime(dl$fullTime,"%d/%m/%y %H:%M:%S")
# indOrder=order(time)
# dl=dl[indOrder,]

d$dayDate=as.Date(d$dayDate,"%Y-%m-%d")
# extract accuray and learning
uTag=unique(di$tag)
for(i in 1:length(uTag)){
  print(i)
  ind=dl$tag==uTag[i]
  #ind=dl$tag=="01103F65F4"
  # total number of trials
  di$nbtt[di$tag==uTag[i]]=sum(di$nbts[di$tag==uTag[i]])
  for (s in c(30:32)){# by scenario
    inds=ind&dl$scenario==s
    # Acc tot by scenario
    di$AccTot[di$tag==uTag[i]&di$scenario==s]=mean(dl$door.open[inds],na.rm=T)
    # Acc first and last trials
    if (sum(inds,na.rm = T)>202)
    {
      # initial
      di$AccSt30[di$tag==uTag[i]&di$scenario==s]=mean(dl$door.open[inds&dl$trialNumber<31])
      di$AccSt50[di$tag==uTag[i]&di$scenario==s]=mean(dl$door.open[inds&dl$trialNumber<51])
      di$AccSt100[di$tag==uTag[i]&di$scenario==s]=mean(dl$door.open[inds&dl$trialNumber<101])
      # final
      di$AccFi30[di$tag==uTag[i]&di$scenario==s]=mean(dl$door.open[inds&dl$trialNumber>(sum(inds)-30)])
      di$AccFi50[di$tag==uTag[i]&di$scenario==s]=mean(dl$door.open[inds&dl$trialNumber>(sum(inds)-50)])
      di$AccFi100[di$tag==uTag[i]&di$scenario==s]=mean(dl$door.open[inds&dl$trialNumber>(sum(inds)-100)])
      #diff
      di$AccDi30[di$tag==uTag[i]&di$scenario==s]=mean(dl$door.open[inds&dl$trialNumber>(sum(inds)-30)])-mean(dl$door.open[inds&dl$trialNumber<31])
      di$AccDi50[di$tag==uTag[i]&di$scenario==s]=mean(dl$door.open[inds&dl$trialNumber>(sum(inds)-50)])-mean(dl$door.open[inds&dl$trialNumber<51])
      di$AccDi100[di$tag==uTag[i]&di$scenario==s]=mean(dl$door.open[inds&dl$trialNumber>(sum(inds)-100)])-mean(dl$door.open[inds&dl$trialNumber<101])
      # above chance
      if (s==30)
      {di$AboveChance100[di$tag==uTag[i]&di$scenario==s]=sum(dl$door.open[inds&dl$trialNumber>(sum(inds)-100)])>40}
      else
      {di$AboveChance100[di$tag==uTag[i]&di$scenario==s]=sum(dl$door.open[inds&dl$trialNumber>(sum(inds)-100)])>65}
      #chisq.test(rbind(c(7.5,22.5),c(15,15)))
      
    }
    # Acc final: last 2 day with more than 30 trials
    day30=unique(dd$dayDate[dd$tag==uTag[i]&dd$scenario==s&dd$nbt>30])#day with more than 30 trials
    day30=sort(day30)
    if (length(day30)>4){# performance on the last 2 days with more than 30 trials
      di$AccFin[di$tag==uTag[i]&di$scenario==s]=mean(dl$door.open[inds&dl$dayDate %in% c(day30[(length(day30)-1):(length(day30)-0)])])
      di$AccIni[di$tag==uTag[i]&di$scenario==s]=mean(dl$door.open[inds&dl$dayDate %in% c(day30[1:2])])
      di$AccDif[di$tag==uTag[i]&di$scenario==s]=mean(dl$door.open[inds&dl$dayDate %in% c(day30[(length(day30)-1):(length(day30)-0)])])-mean(dl$door.open[inds&dl$dayDate %in% c(day30[1:2])])
    }
    
    # Acc last day with more than 30 trials
    if (s==30) {chance=0.25} else {chance=0.5}
    
    if (length(day30)>0){# pr
      di$AccLastDay[di$tag==uTag[i]&di$scenario==s]=mean(dl$door.open[inds&dl$dayDate %in% c(day30[length(day30)])])
      nbtLast=length(dl$door.open[inds&dl$dayDate %in% c(day30[length(day30)])])
      propLast=table(dl$door.open[inds&dl$dayDate %in% c(day30[length(day30)])])
      propChance=c(round(nbtLast*(1-chance)),round(nbtLast*(chance)))
      chi=chisq.test(rbind(propLast,propChance))
      if (chi$p.value>0.05){
        di$LastDayAboveChance[di$tag==uTag[i]&di$scenario==s]="Chance"
      } else if (chi$p.value<0.05&mean(dl$door.open[inds&dl$dayDate %in% c(day30[length(day30)])])<chance){
        di$LastDayAboveChance[di$tag==uTag[i]&di$scenario==s]="BelowChance"  
      } else {
        di$LastDayAboveChance[di$tag==uTag[i]&di$scenario==s]="AboveChance" 
      }
    }
    
    # 1st Day above chance and 1st day vs last day with 30 trials
    #---------------------------------------
    di$firstDayAboveChance[di$tag==uTag[i]&di$scenario==s]=NA
    
    di$firstVSlastDayMore30trialsTest[di$tag==uTag[i]&di$scenario==s]=NA
    di$firstVSlastDayMore30trialsES[di$tag==uTag[i]&di$scenario==s]=NA
    di$firstVSlastDayMore30trialsFirstDay[di$tag==uTag[i]&di$scenario==s]=NA
    di$firstVSlastDayMore30trialsLastDay[di$tag==uTag[i]&di$scenario==s]=NA
    
    if (sum(dd$tag==uTag[i]&dd$scenario==s&dd$nbt>30)>0)# at least one day with more than 30 trials
    {DT <- as.data.table(dl[ind,])
    DT=DT[scenario==s,.(ACC =mean(door.open),nbtrial = .N,error=sum(door.open==0),
                        correct=sum(door.open==1),p=fisher.test(rbind(c(sum(door.open==0),sum(door.open==1)),c(length(door.open)*(1-chance),length(door.open)*chance)))[[1]]),by="day"]
    
    dday=as.data.frame(DT)
    dday$dayDate=as.Date(dday$day,"%d/%m/%y")
    # 1st day above chance
    dday=dday[order(dday$dayDate),]# sort
    dday$sig="Chance"
    dday$sig[dday$p<0.05&dday$ACC>chance]="AboveChance"
    dday$sig[dday$p<0.05&dday$ACC<chance]="BelowChance"
    
    if(sum(dday$sig=="AboveChance"&dday$nbtrial>30)>0)
    {di$firstDayAboveChance[di$tag==uTag[i]&di$scenario==s]=min(which(dday$sig=="AboveChance"&dday$nbtrial>30))}
    else
    {di$firstDayAboveChance[di$tag==uTag[i]&di$scenario==s]=NA}
    
    # 1st day v last day
    first=min(which(dday$nbtrial>30))
    last=max(which(dday$nbtrial>30))
    di$firstVSlastDayMore30trialsTest[di$tag==uTag[i]&di$scenario==s]=fisher.test(rbind(c(dday$error[first],dday$correct[first]),   
                                                                                        c(dday$error[last],dday$correct[last])))[[1]]
    di$firstVSlastDayMore30trialsES[di$tag==uTag[i]&di$scenario==s]=dday$ACC[last]-dday$ACC[first]
    di$firstVSlastDayMore30trialsFirstDay[di$tag==uTag[i]&di$scenario==s]=first
    di$firstVSlastDayMore30trialsLastDay[di$tag==uTag[i]&di$scenario==s]=last
    
    }
    
    
    #chisq.test(rbind(c(64,24),c(88*0.25,88*0.75)))[3]
    # TTC
    #----
    # loop over learning criterium
    crit=c(9/10,17/20,24/30)
    crit30=c(8/10,13/20,20/30)
    wind=c(10,20,30)
    TTC=c()
    AccPostTTC=c()
    AccPost50=c()
    AccPre50=c()
    dayTC=c()
    for (ler in 1:length(crit)){
      # learning criterium
      lcrit=crit[ler]
      if (s==30){lcrit=crit30[ler]} 
      # di$TTC[di$tag==uTag[i]&di$scenario==s]=NA
      # di$AccPostTTC[di$tag==uTag[i]&di$scenario==s]=NA
      TTC[ler]=NA
      AccPostTTC[ler]=NA
      AccPost50[ler]=NA
      AccPre50[ler]=NA
      dayTC[ler]=NA
      if (sum(inds)>100){# at least 30 trials in this scenario
        lc=slideFunct(dl$door.open[inds],wind[ler],1)
        # first time reach criterium
        if (sum(lc>=lcrit)>0) {
          #di$TTC[di$tag==uTag[i]&di$scenario==s]=min(which(lc>=lcrit)) 
          # day to criterium: d$dayDate[which(inds)[min(which(lc>=lcrit))+10]]
          #di$AccPostTTC[di$tag==uTag[i]&di$scenario==s]=mean(dl$door.open[which(inds)[min(which(lc>=lcrit)):sum(inds)]])
          ttc=min(which(lc>=lcrit)) 
          TTC[ler]=ttc
          AccPostTTC[ler]=mean(dl$door.open[which(inds)[ttc:sum(inds)]])
          AccPost50[ler]=mean(dl$door.open[which(inds)[ttc:(ttc+50)]])
          if ((ttc-50-wind[ler])>0){
            AccPre50[ler]=mean(dl$door.open[which(inds)[(ttc-50-wind[ler]):(ttc-wind[ler])]])
          }
          else
          {
            AccPre50[ler]=NA#mean(dl$door.open[which(inds)[1:(ttc-wind[ler])]])
          }
          dayTC[ler]=as.Date(dl$dayDate[which(inds)[ttc+wind[ler]]],"%Y-%m-%d")-min(as.Date(dl$dayDate[inds],"%Y-%m-%d"))
        }
      }
    }
    # store for each time windonw
    di$TTC10[di$tag==uTag[i]&di$scenario==s]=TTC[1]
    di$AccPostTTC10[di$tag==uTag[i]&di$scenario==s]=AccPostTTC[1]
    di$AccPost50_10[di$tag==uTag[i]&di$scenario==s]=AccPost50[1]
    di$AccPre50_10[di$tag==uTag[i]&di$scenario==s]=AccPre50[1]
    di$dayTC_10[di$tag==uTag[i]&di$scenario==s]=dayTC[1]
    
    di$TTC20[di$tag==uTag[i]&di$scenario==s]=TTC[2]
    di$AccPostTTC20[di$tag==uTag[i]&di$scenario==s]=AccPostTTC[2]
    di$AccPost50_20[di$tag==uTag[i]&di$scenario==s]=AccPost50[2]
    di$AccPre50_20[di$tag==uTag[i]&di$scenario==s]=AccPre50[2]
    di$dayTC_20[di$tag==uTag[i]&di$scenario==s]=dayTC[2]
    
    di$TTC30[di$tag==uTag[i]&di$scenario==s]=TTC[3]
    di$AccPostTTC30[di$tag==uTag[i]&di$scenario==s]=AccPostTTC[3]
    di$AccPost50_30[di$tag==uTag[i]&di$scenario==s]=AccPost50[3]
    di$AccPre50_30[di$tag==uTag[i]&di$scenario==s]=AccPre50[3]
    di$dayTC_30[di$tag==uTag[i]&di$scenario==s]=dayTC[3]
    
    # find exact trial number to find dday and time
  }
}
#save
write.table(di,paste(out_f,"alldata.csv",sep=""))



