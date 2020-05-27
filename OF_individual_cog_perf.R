# compute individual descritptive statistics
#-------------------------------------------
# We quantified individual performance in learning in 10 different manners. 
# We first computed binary measures of learning using 5 different approaches:
# 1-Did the bird reach at least once the learning criteria of 9 correct trials out of 10 consecutive trials (TTC)?
# 2-Was the bird above chance on its last day with more than 30 trials?
# 3-Was the bird above chance on its last 100 trials?
# 4-Was there a significant effect of trial number on the probability to perform a correct trials assessed using a binomial GAM for each bird.
# 5-Was the number of good responses given in the last 30 trials significantly greater than those performed on the first 30 trials.
# We then computed XX quantitative measure of individual performance:
# 6-Number of trials to reach a learning criterium of 9 correct trials out of 10 consecutive trials (TTC)
# 7-Accuracy (proportion of good response) on the last day with more than 30 trials
# 8-Accuracy on last 100 trials
# 9-Difference between the number of correct responses in last 30 trials and number of correct responses in first 30 trials.


# load precomputed data
#d=read.table('/Users/maximecauchoix/Documents/openfeeder/data/StudySite_2018_19.csv',h=T)

# load bart data
dFit30=read.csv2('/Users/maximecauchoix/Dropbox/wild_cog_OF/results/2018-2019/remodelfits/learning_modelfit_exp30.csv',h=T,sep=",",na.strings = "NaN",dec=".")
dFit30$scenario=30
dFit31=read.csv2('/Users/maximecauchoix/Dropbox/wild_cog_OF/results/2018-2019/remodelfits/learning_modelfit_exp31.csv',h=T,sep=",",na.strings = "NaN",dec=".")
dFit31$scenario=31
dFit32=read.csv2('/Users/maximecauchoix/Dropbox/wild_cog_OF/results/2018-2019/remodelfits/learning_modelfit_exp32.csv',h=T,sep=",",na.strings = "NaN",dec=".")
dFit32$scenario=32

dFit=rbind(dFit30,dFit31,dFit32)


# initial table with nbtrial by scenario
DT <- as.data.table(d)
DT=DT[,list(nbt =.N,species=unique(species),bandNumber=unique(bandNumber),sex=unique(sex),age=unique(age),site=unique(site_folder),elevation=unique(elevation)), by="scenario,dayDate,tag"]
dd=as.data.frame(DT)

# nbtm= mean number of trial per day,nbts=total number of trial by scenario,nbd= number of days,
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
 
  # total number of trials
  #-----------------------
  di$nbtt[di$tag==uTag[i]]=sum(di$nbts[di$tag==uTag[i]])
  for (j in c(30:32)){# by scenario
    inds=ind&dl$scenario==j
    # Acc tot by scenario
    #---------------------
    di$AccTot[di$tag==uTag[i]&di$scenario==j]=mean(dl$door.open[inds],na.rm=T)
    # GAM and GLM
    #--------------------
    di$gamPval[di$tag==uTag[i]&di$scenario==j]=NA
    di$glmPval[di$tag==uTag[i]&di$scenario==j]=NA
    di$glmEstim[di$tag==uTag[i]&di$scenario==j]=NA
    if (sum(inds,na.rm = T)>10){
      m=summary(gam(door.open~s(trialNumber),data=dl,subset=inds,family ="binomial" ))
      di$gamPval[di$tag==uTag[i]&di$scenario==j]=m$s.pv
      
      m2=summary(glm(door.open~trialNumber,data=dl,subset=inds,family ="binomial" ))
      di$glmPval[di$tag==uTag[i]&di$scenario==j]=coef(m2)[2,4]
      di$glmEstim[di$tag==uTag[i]&di$scenario==j]=coef(m2)[2,1]
    }
    
    # Acc first and last trials, diff and contengency table
    #--------------------------
    if (sum(inds,na.rm = T)>101)
    {
      # initial
      di$AccSt30[di$tag==uTag[i]&di$scenario==j]=mean(dl$door.open[inds&dl$trialNumber<31])
      di$AccSt50[di$tag==uTag[i]&di$scenario==j]=mean(dl$door.open[inds&dl$trialNumber<51])
      di$AccSt100[di$tag==uTag[i]&di$scenario==j]=mean(dl$door.open[inds&dl$trialNumber<101])
      # final
      di$AccFi30[di$tag==uTag[i]&di$scenario==j]=mean(dl$door.open[inds&dl$trialNumber>(sum(inds)-30)])
      di$AccFi50[di$tag==uTag[i]&di$scenario==j]=mean(dl$door.open[inds&dl$trialNumber>(sum(inds)-50)])
      di$AccFi100[di$tag==uTag[i]&di$scenario==j]=mean(dl$door.open[inds&dl$trialNumber>(sum(inds)-100)])
      #diff
      di$AccDi30[di$tag==uTag[i]&di$scenario==j]=mean(dl$door.open[inds&dl$trialNumber>(sum(inds)-30)])-mean(dl$door.open[inds&dl$trialNumber<31])
      di$AccDi50[di$tag==uTag[i]&di$scenario==j]=mean(dl$door.open[inds&dl$trialNumber>(sum(inds)-50)])-mean(dl$door.open[inds&dl$trialNumber<51])
      di$AccDi100[di$tag==uTag[i]&di$scenario==j]=mean(dl$door.open[inds&dl$trialNumber>(sum(inds)-100)])-mean(dl$door.open[inds&dl$trialNumber<101])
      #STATS 30 1st vs LAST  above chance
      contTable=rbind(c(sum(dl$door.open[inds&dl$trialNumber<31]),sum(dl$door.open[inds&dl$trialNumber>(sum(inds)-30)])),
                      c(length(dl$door.open[inds&dl$trialNumber<31]),length(dl$door.open[inds&dl$trialNumber>(sum(inds)-30)])))
      chi=fisher.test(contTable)
      di$firstVSlast_30trials[di$tag==uTag[i]&di$scenario==j]=chi$p.value
      #STATS 50 1st vs LAST  above chance
      contTable=rbind(c(sum(dl$door.open[inds&dl$trialNumber<51]),sum(dl$door.open[inds&dl$trialNumber>(sum(inds)-50)])),
                      c(length(dl$door.open[inds&dl$trialNumber<51]),length(dl$door.open[inds&dl$trialNumber>(sum(inds)-50)])))
      chi=fisher.test(contTable)
      di$firstVSlast_50trials[di$tag==uTag[i]&di$scenario==j]=chi$p.value
      #STATS 100 1st vs LAST  above chance
      contTable=rbind(c(sum(dl$door.open[inds&dl$trialNumber<101]),sum(dl$door.open[inds&dl$trialNumber>(sum(inds)-100)])),
                      c(length(dl$door.open[inds&dl$trialNumber<101]),length(dl$door.open[inds&dl$trialNumber>(sum(inds)-100)])))
      chi=fisher.test(contTable)
      di$firstVSlast_100trials[di$tag==uTag[i]&di$scenario==j]=chi$p.value
      
      # Above chance on the last 100 trials
      #------------------------------------
      if (j==30)
      {di$AboveChance100[di$tag==uTag[i]&di$scenario==j]=sum(dl$door.open[inds&dl$trialNumber>(sum(inds)-100)])>40}
      else
      {di$AboveChance100[di$tag==uTag[i]&di$scenario==j]=sum(dl$door.open[inds&dl$trialNumber>(sum(inds)-100)])>65}
      #chisq.test(rbind(c(7.5,22.5),c(15,15)))
      
    }
    # # Acc final: last 2 day with more than 30 trials
    # day30=unique(dd$dayDate[dd$tag==uTag[i]&dd$scenario==j&dd$nbt>30])#day with more than 30 trials
    # day30=sort(day30)
    # if (length(day30)>4){# performance on the last 2 days with more than 30 trials
    #   di$AccFin[di$tag==uTag[i]&di$scenario==j]=mean(dl$door.open[inds&dl$dayDate %in% c(day30[(length(day30)-1):(length(day30)-0)])])
    #   di$AccIni[di$tag==uTag[i]&di$scenario==j]=mean(dl$door.open[inds&dl$dayDate %in% c(day30[1:2])])
    #   di$AccDif[di$tag==uTag[i]&di$scenario==j]=mean(dl$door.open[inds&dl$dayDate %in% c(day30[(length(day30)-1):(length(day30)-0)])])-mean(dl$door.open[inds&dl$dayDate %in% c(day30[1:2])])
    # }
    
    # Accuary on Last and first day with more than 30 trials, difference and stat
    #-----------------------------
    day30=unique(dd$dayDate[dd$tag==uTag[i]&dd$scenario==j&dd$nbt>30])#day with more than 30 trials
    day30=sort(day30)
    if (length(day30)>3){# performance on the last 2 days with more than 30 trials
      di$AccFin[di$tag==uTag[i]&di$scenario==j]=mean(dl$door.open[inds&dl$dayDate %in% c(day30[length(day30)])])
      di$AccIni[di$tag==uTag[i]&di$scenario==j]=mean(dl$door.open[inds&dl$dayDate %in% c(day30[1])])
      di$AccDif[di$tag==uTag[i]&di$scenario==j]=mean(dl$door.open[inds&dl$dayDate %in% c(day30[length(day30)])])-mean(dl$door.open[inds&dl$dayDate %in% c(day30[1])])
      #STATS 1st vs LAST day above chance
      contTable=rbind(c(sum(dl$door.open[inds&dl$dayDate %in% c(day30[1])]),sum(dl$door.open[inds&dl$dayDate %in% c(day30[length(day30)])])),
                      c(length(dl$door.open[inds&dl$dayDate %in% c(day30[1])]),length(dl$door.open[inds&dl$dayDate %in% c(day30[length(day30)])])))
      chi=fisher.test(contTable)
      di$firstVSlastDay[di$tag==uTag[i]&di$scenario==j]=chi$p.value
    }
    
    
    # Above chance on last day with more than 30 trials
    #--------------------------
    if (j==30) {chance=0.25} else {chance=0.5}
    
    if (length(day30)>0){# pr
      di$AccLastDay[di$tag==uTag[i]&di$scenario==j]=mean(dl$door.open[inds&dl$dayDate %in% c(day30[length(day30)])])
      nbtLast=length(dl$door.open[inds&dl$dayDate %in% c(day30[length(day30)])])
      propLast=table(dl$door.open[inds&dl$dayDate %in% c(day30[length(day30)])])
      propChance=c(round(nbtLast*(1-chance)),round(nbtLast*(chance)))
      chi=chisq.test(rbind(propLast,propChance))
      if (chi$p.value>0.05){
        di$LastDayAboveChance[di$tag==uTag[i]&di$scenario==j]="Chance"
      } else if (chi$p.value<0.05&mean(dl$door.open[inds&dl$dayDate %in% c(day30[length(day30)])])<chance){
        di$LastDayAboveChance[di$tag==uTag[i]&di$scenario==j]="BelowChance"  
      } else {
        di$LastDayAboveChance[di$tag==uTag[i]&di$scenario==j]="AboveChance" 
      }
    }
    
    # 1st Day above chance and 1st day vs last day with 30 trials
    #---------------------------------------
    di$firstDayAboveChance[di$tag==uTag[i]&di$scenario==j]=NA
    di$bellowChanceOnFirstDay[di$tag==uTag[i]&di$scenario==j]=NA
    
    # di$firstVSlastDayMore30trialsTest[di$tag==uTag[i]&di$scenario==j]=NA
    # di$firstVSlastDayMore30trialsES[di$tag==uTag[i]&di$scenario==j]=NA
    # di$firstVSlastDayMore30trialsFirstDay[di$tag==uTag[i]&di$scenario==j]=NA
    # di$firstVSlastDayMore30trialsLastDay[di$tag==uTag[i]&di$scenario==j]=NA
    
    if (sum(dd$tag==uTag[i]&dd$scenario==j&dd$nbt>30)>0)# at least one day with more than 30 trials
    {DT <- as.data.table(dl[ind,])
    DT=DT[scenario==j,.(ACC =mean(door.open),nbtrial = .N,error=sum(door.open==0),
                        correct=sum(door.open==1),p=fisher.test(rbind(c(sum(door.open==0),sum(door.open==1)),c(length(door.open)*(1-chance),length(door.open)*chance)))[[1]]),by="day"]
    
    dday=as.data.frame(DT)
    dday$dayDate=as.Date(dday$day,"%d/%m/%y")
    
    # Nb day with more than 30 trials before 1st day above chance
    #-----------------------------------
    dday=dday[order(dday$dayDate),]# sort
    dday$sig="Chance"
    dday$sig[dday$p<0.05&dday$ACC>chance]="AboveChance"
    dday$sig[dday$p<0.05&dday$ACC<chance]="BelowChance"
    
    if(sum(dday$sig=="AboveChance"&dday$nbtrial>30)>0)
    {di$firstDayAboveChance[di$tag==uTag[i]&di$scenario==j]=min(which(dday$sig=="AboveChance"&dday$nbtrial>30))}
    else
    {di$firstDayAboveChance[di$tag==uTag[i]&di$scenario==j]=NA}
    
    # Bellow chance on first day
    #------------------
    di$bellowChanceOnFirstDay[di$tag==uTag[i]&di$scenario==j]=dday$sig[min(which(dday$nbtrial>30))]
    
    
    # # 1st day v last day
    # first=min(which(dday$nbtrial>30))
    # last=max(which(dday$nbtrial>30))
    # di$firstVSlastDayMore30trialsTest[di$tag==uTag[i]&di$scenario==j]=fisher.test(rbind(c(dday$error[first],dday$correct[first]),   
    #                                                                                     c(dday$error[last],dday$correct[last])))[[1]]
    # di$firstVSlastDayMore30trialsES[di$tag==uTag[i]&di$scenario==j]=dday$ACC[last]-dday$ACC[first]
    # di$firstVSlastDayMore30trialsFirstDay[di$tag==uTag[i]&di$scenario==j]=first
    # di$firstVSlastDayMore30trialsLastDay[di$tag==uTag[i]&di$scenario==j]=last
    # 
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
      if (j==30){lcrit=crit30[ler]} 
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
    di$TTC10[di$tag==uTag[i]&di$scenario==j]=TTC[1]
    di$AccPostTTC10[di$tag==uTag[i]&di$scenario==j]=AccPostTTC[1]
    di$AccPost50_10[di$tag==uTag[i]&di$scenario==j]=AccPost50[1]
    di$AccPre50_10[di$tag==uTag[i]&di$scenario==j]=AccPre50[1]
    di$dayTC_10[di$tag==uTag[i]&di$scenario==j]=dayTC[1]
    
    di$TTC20[di$tag==uTag[i]&di$scenario==j]=TTC[2]
    di$AccPostTTC20[di$tag==uTag[i]&di$scenario==j]=AccPostTTC[2]
    di$AccPost50_20[di$tag==uTag[i]&di$scenario==j]=AccPost50[2]
    di$AccPre50_20[di$tag==uTag[i]&di$scenario==j]=AccPre50[2]
    di$dayTC_20[di$tag==uTag[i]&di$scenario==j]=dayTC[2]
    
    di$TTC30[di$tag==uTag[i]&di$scenario==j]=TTC[3]
    di$AccPostTTC30[di$tag==uTag[i]&di$scenario==j]=AccPostTTC[3]
    di$AccPost50_30[di$tag==uTag[i]&di$scenario==j]=AccPost50[3]
    di$AccPre50_30[di$tag==uTag[i]&di$scenario==j]=AccPre50[3]
    di$dayTC_30[di$tag==uTag[i]&di$scenario==j]=dayTC[3]
    
    # find exact trial number to find dday and time
  }
}
#save
write.table(di,paste(out_local,"alldata.csv",sep=""))

#---------------------
# Merge with bart data
#---------------------
dFit$code=paste(dFit$id_individual,dFit$scenario)

di$code=paste(di$tag,di$scenario)

# not in dFit
setdiff(di$code[di$scenario>3],dFit$code)

# # correct 
# p=dfinal$glmEstim
# e=dfinal$glmPval
# 
# dfinal$glmPval=p
# dfinal$glmEstim=e

dfinal=merge(di,dFit,by="code", all.x=T,sort=F)#

write.csv2(dfinal,paste(out_local,"all_ind_stats_bart.csv",sep=""),row.names = F)

