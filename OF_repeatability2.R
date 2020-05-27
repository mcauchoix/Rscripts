# Evaluate repeatability of cognitive variables from "di", on both learning (L_L) and all task (L_R)
# for all species or by species
# Maxime Cauchoix 
# created: 2/05/2019`
# adjusted for new 14 binary and 27 continuous variables 
# 11/11/2019
#----------------------------------------------------------------
library("forestplot")
# load indivual cognitive variables
di=read.csv2(paste(out_local,"all_ind_stats_afterCorelation.csv",sep=""),h=T)
#di=di[di$nbdsup>10,]
di$scenario=di$scenario.x
#-----------------
# Variables
#-----------------

# Cognitive variables name
#-------------------------
# binary variables
binaryVar=names(di)[125:138]
# continuous variables
contVar=c("nbts","AccTot","AccFi30","AccFi50","AccFi100",
          "AccDi30","AccDi50","AccDi100",
          "AccLastDay","firstDayAboveChance",
          "TTC10","TTC20","TTC30",
          "dayTC_10","dayTC_20","dayTC_30",
          "AccPostTTC10","AccPostTTC20","AccPostTTC30",
          "exptial_beginperf","exptial_endperf",  
          "exptial_endtime","sigmoid_beginperf",
          "sigmoid_endperf","sigmoid_inflect",  
          "sigmoid_begintime", "sigmoid_endtime")
#bartV=names(di)[116:123]

# concatanate
varnames=c(binaryVar,contVar)

# Plot distribution of transformation to decide link function/ transformation in repeatability analysis
#------------------
# for (i in 1:length(contVar)){
#   pdf(paste0(out_dist,contVar[i],".pdf"), height=5, width=15)
#   
#   par(mfrow=c(1,3))
#   hist(di[[contVar[i]]],main=contVar[i],xlab =contVar[i])
#   hist(log(di[[contVar[i]]]+1),main=paste("Log",contVar[i]),xlab =paste("Log",contVar[i]))
#   hist(sqrt(di[[contVar[i]]]+1),main=paste("sqrt",contVar[i]),xlab =paste("sqrt",contVar[i]))
#   
#   dev.off()
#   }

#-----------------------
# Repeatability analysis
#-----------------------
#  generic parameters
#-------------------------
nboot=10
npermut=1


resList=list()
for (r in c(1:10,13,15:length(varnames))){

  
  # default
  #---------
  varname=varnames[r]
  var=di[[varname]]
  distrib="Gaussian"
  transformation="No"

  # tranform or adapt distribution
  #--------------------------------
  # transformation or poisson
  if (varname %in% binaryVar){distrib="Binary"}
  if (varname %in% contVar[c(1,10:13,15,25,27)]){distrib="Poisson"}
  if (varname %in% contVar[c(3:4,6:9,14,22)]){di[[varname]]=log(var+1);  transformation="Log"}
  if (varname %in% contVar[c(1,5,16,26)]){di[[varname]]=sqrt(var);  transformation="Sqrt"}
  
  # print
  print("-------------------------------")
  print(varname)
  print(distrib)
  print(transformation)
  print("-------------------------------")
  
  
  
  # all species
  #-------------
  # formula for all species
  form <- as.formula(paste(varnames[r],"species+scenario+(1|tag)",sep="~"))
  
  # 2 learning only
  rep=rpt(form, grname = c("tag"), data = di[di$scenario %in% c(30,31),], datatype = distrib,nboot = nboot, npermut = npermut) 
  # Validity
  simulationOutput <- simulateResiduals(fittedModel = rep$mod, n = 1000)
  Unif=testUniformity(simulationOutput = simulationOutput)
  
  # Store
  if (distrib=="Gaussian")
  {
  res=data.frame(species='All',
                 R=round(rep$R[[1]],2),
                 lowCI=round(rep$CI_emp[[1]],2),
                 highCI=round(rep$CI_emp[[2]],2),
                 SS=length(unique(di$tag[di$scenario %in% c(30,31)&!is.na(var)])),
                 task='L_L',
                 variable=varnames[r],
                 uniformity=round(Unif$p.value,3))
  }
  else
  {
    res=data.frame(species='All',
                   R=round(rep$R[[2,1]],3),
                   lowCI=round(rep$CI_emp$CI_link[[1]],3),
                   highCI=round(rep$CI_emp$CI_link[[2]],3),
                   SS=length(unique(di$tag[di$scenario %in% c(30,31)&!is.na(var)])),
                   task='L_L',
                   variable=varnames[r],
                   uniformity=round(Unif$p.value,3))
  }
  # check convergence
  res$mod_conv_R=1
  if (length(rep$mod@optinfo$conv$lme4)>0) {res$mod_conv_R=0}
  res$distrib=distrib
  res$transfo=transformation
  
  # # 2 Learning and reversal
  # rep=rpt(form, grname = c("tag"), data = di, datatype = distrib,nboot = nboot, npermut = npermut) 
  # # Validity
  # simulationOutput <- simulateResiduals(fittedModel = rep$mod, n = 1000)
  # Unif=testUniformity(simulationOutput = simulationOutput)
  # # Store
  # temp=data.frame(species='All',
  #                 R=round(rep$R[[1]],2),
  #                 lowCI=round(rep$CI_emp[[1]],2),
  #                 highCI=round(rep$CI_emp[[2]],2),
  #                 SS=length(unique(di$tag)),
  #                 task='L_L_R',
  #                 variable=varnames[r],
  #                 uniformity=round(Unif$p.value,3))
  # 
  # # merge
  # res = rbind(res,temp)
  
  # by species
  #-------------
  # formula for each species
  form <- as.formula(paste(varnames[r],"scenario+(1|tag)",sep="~"))

  uSpe=unique(di$species)
  for (s in 1:length(uSpe)){
   # length(var[di$scenario=="30"&di$species==uSpe[s]])
    
    if (sum(var[di$scenario=="31"&di$species==uSpe[s]],na.rm = T)>3&sum(var[di$scenario=="30"&di$species==uSpe[s]],na.rm = T)>0)# cant do repeat when all 0
    {
    # 2 learning only
    ind=di$scenario %in% c(30,31)&di$species==uSpe[s]&!is.na(di[,varnames[r]])
    rep=rpt(form, grname = c("tag"), data = di[ind,], datatype = distrib,nboot = nboot, npermut = npermut) 
    # Validity
    simulationOutput <- simulateResiduals(fittedModel = rep$mod, n = 1000)
    Unif=testUniformity(simulationOutput = simulationOutput)
    # Store
    if (distrib=="Gaussian")
    {
      temp=data.frame(species=uSpe[s],
                     R=round(rep$R[[1]],2),
                     lowCI=round(rep$CI_emp[[1]],2),
                     highCI=round(rep$CI_emp[[2]],2),
                     SS=length(unique(di$tag[di$species==uSpe[s]&di$scenario %in% c(30,31)&!is.na(var)])),
                     task='L_L',
                     variable=varnames[r],
                     uniformity=round(Unif$p.value,3))}
      else
      {
        temp=data.frame(species=uSpe[s],
                       R=round(rep$R[[2,1]],3),
                       lowCI=round(rep$CI_emp$CI_link[[1]],3),
                       highCI=round(rep$CI_emp$CI_link[[2]],3),
                       SS=length(unique(di$tag[di$species==uSpe[s]&di$scenario %in% c(30,31)&!is.na(var)])),
                       task='L_L',
                       variable=varnames[r],
                       uniformity=round(Unif$p.value,3))
      }
      # check convergence
      temp$mod_conv_R=1
      if (length(rep$mod@optinfo$conv$lme4)>0) {temp$mod_conv_R=0}
      temp$distrib=distrib
      temp$transfo=transformation
    
      res = rbind(res,temp)
    }
      
    # # 2 Learning and reversal
    # rep=rpt(form, grname = c("tag"), data = di[di$species==uSpe[s],], datatype = distrib,nboot = nboot, npermut = npermut) 
    # # Validity
    # simulationOutput <- simulateResiduals(fittedModel = rep$mod, n = 1000)
    # Unif=testUniformity(simulationOutput = simulationOutput)
    # # Store
    # temp=data.frame(species=uSpe[s],
    #                 R=round(rep$R[[1]],2),
    #                 lowCI=round(rep$CI_emp[[1]],2),
    #                 highCI=round(rep$CI_emp[[2]],2),
    #                 SS=length(unique(di$tag[di$species==uSpe[s]])),
    #                 task='L_L_R',
    #                 variable=varnames[r],
    #                 uniformity=round(Unif$p.value,3))
    # res = rbind(res,temp)
  }
  
  
  # plot Correlation
  #------------------
  #name
  repName=paste0('R=',res$R[1],' [',res$lowCI[1],' ',res$highCI[1],']')
  
  cor30=di[di$scenario==30,c("tag",varnames[r],"species")]
  cor31=di[di$scenario==31,c("tag",varnames[r],"species")]
  co=merge(cor30,cor31,by="tag", all.x=T,sort=F)
  names(co)=c("tag","Trait30","species30","Trait31","species31")#rename
  # color
  co$species=as.factor(co$species30)
  col=c()
  col[co$species=="Marsh"]="brown"
  col[co$species=="Great"]="gold"
  col[co$species=="Blue"]="blue"
  
  pdf(paste(out_rep,varnames[r],'_Correlation plot Learning only.pdf'))
  plot(co$Trait30,co$Trait31,col=col,xlab = "ON/OFF learning",ylab="Left/Right learning",pch=19,main=repName)
  abline(h=0.5, col = "gray60",lty=2)
  abline(v=0.25, col = "gray60",lty=2)
  legend(0.8,0.55,legend = c("Great","Blue","Marsh"),col=c("gold","blue","brown"),pch=19,cex=1,y.intersp=1,bty="n")
  dev.off()
  # store
  resList[[r]]=res
}

restot=rbindlist(resList)
write.table(restot,file=paste0(out_rep,"RepeatabilityData.csv"),sep = ";",row.names = F)

# Export
pdf(paste0(out_rep,"All_variables_Repeatability table.pdf"), height=40, width=10)
grid.table(restot,rows =c())
dev.off()

# Export
pdf(paste0(out_rep,"AccFin100_Repeatability table.pdf"), height=1.5, width=3)
grid.table(restot[restot$variable=="AccFi100"&restot$task=="L_L",c(1:5)],rows =c())
dev.off()


pdf(paste0(out_rep,"Significant_Repeatability table.pdf"), height=1.5, width=3)
grid.table(restot[restot$lowCI>0&restot$uniformity>0.05,],rows =c())
dev.off()

# plot
#-----
ind=restot$task=="L_L"&restot$species=="All"
x=restot[ind,]
x=x[order(x$R,decreasing = T),]


pdf(file=paste0(out_rep,"Best LL.pdf"), height=6, width=4)
# prepare text table
#tabletext=cbind(as.character(x$species),as.character(x$variable),x$SS)
#tabletext=rbind(c('Species','Measure', 'SS'),tabletext)
tabletext=cbind(as.character(x$variable))
tabletext=rbind(c('Measure'),tabletext)
# prepare data
mean  = c(NA,x$R)# add NA for title
lower = c(NA,x$lowCI)
upper = c(NA,x$highCI)
# plot
forestplot(tabletext,mean,lower,upper,new_page = F,is.summary=c(TRUE,rep(FALSE,dim(tabletext)[1]-1)),txt_gp = fpTxtGp(label = list(gpar(fontface = 1),gpar(fontface = 3),gpar(fontface = 1),gpar(fontface = 1)),cex=0.6, ticks = gpar(cex=1),xlab = gpar(cex=1)),boxsize = .25,grid=T,xlab="R")
dev.off()

