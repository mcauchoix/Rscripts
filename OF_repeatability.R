# Evaluate repeatability of cognitive variables from "di", on both learning (L_L) and all task (L_R)
# for all species or by species
# Maxime Cauchoix 
# 2/05/2019
#----------------------------------------------------------------
library("forestplot")
# load indivual cognitive variables
di=read.table(paste(out_f,"alldata.csv",sep=""),h=T)
#di=di[di$nbdsup>10,]
#-----------------
# Repeatability
#-----------------
#normalise
di$logTTC10=log(di$TTC10)
di$logTTC20=log(di$TTC20)
di$logTTC30=log(di$TTC30)
di$lognbts=log(di$nbts)

# visualise effectdi
#ggpairs(di[c(1,7,8,9,11,13,15,29,30,21)])

# loop on all variables
varnames=names(di)[c(13:37,39:45)+1]# +1 because I added bandNumber to check
nboot=10
npermut=10

resList=list()
for (r in 1:length(varnames)){
  
  # all species
  #-------------
  # formula for all species
  form <- as.formula(paste(varnames[r],"species+scenario+site+(1|tag)",sep="~"))
  # 2 learning only
  rep=rpt(form, grname = c("tag"), data = di[di$scenario %in% c(30,31),], datatype = "Gaussian",nboot = nboot, npermut = npermut) 
  # Validity
  simulationOutput <- simulateResiduals(fittedModel = rep$mod, n = 1000)
  Unif=testUniformity(simulationOutput = simulationOutput)
  # Store
  res=data.frame(species='All',
                 R=round(rep$R[[1]],2),
                 lowCI=round(rep$CI_emp[[1]],2),
                 highCI=round(rep$CI_emp[[2]],2),
                 SS=length(unique(di$tag[di$scenario %in% c(30,31)])),
                 task='L_L',
                 variable=varnames[r],
                 uniformity=round(Unif$p.value,3))
  # 2 Learning and reversal
  rep=rpt(form, grname = c("tag"), data = di, datatype = "Gaussian",nboot = nboot, npermut = npermut) 
  # Validity
  simulationOutput <- simulateResiduals(fittedModel = rep$mod, n = 1000)
  Unif=testUniformity(simulationOutput = simulationOutput)
  # Store
  temp=data.frame(species='All',
                  R=round(rep$R[[1]],2),
                  lowCI=round(rep$CI_emp[[1]],2),
                  highCI=round(rep$CI_emp[[2]],2),
                  SS=length(unique(di$tag)),
                  task='L_L_R',
                  variable=varnames[r],
                  uniformity=round(Unif$p.value,3))
  res = rbind(res,temp)
  
  # by species
  #-------------
  # formula for each species
  form <- as.formula(paste(varnames[r],"scenario+site+(1|tag)",sep="~"))
  uSpe=unique(di$species)
  for (s in 1:length(uSpe)){
    # 2 learning only
    ind=di$scenario %in% c(30,31)&di$species==uSpe[s]&!is.na(di[,varnames[r]])
    rep=rpt(form, grname = c("tag"), data = di[ind,], datatype = "Gaussian",nboot = nboot, npermut = npermut) 
    # Validity
    simulationOutput <- simulateResiduals(fittedModel = rep$mod, n = 1000)
    Unif=testUniformity(simulationOutput = simulationOutput)
    # Store
     temp=data.frame(species=uSpe[s],
                    R=round(rep$R[[1]],2),
                    lowCI=round(rep$CI_emp[[1]],2),
                    highCI=round(rep$CI_emp[[2]],2),
                    SS=length(unique(di$tag[di$species==uSpe[s]&di$scenario %in% c(30,31)])),
                    task='L_L',
                    variable=varnames[r],
                    uniformity=round(Unif$p.value,3))
    res = rbind(res,temp)
    
    # 2 Learning and reversal
    rep=rpt(form, grname = c("tag"), data = di[di$species==uSpe[s],], datatype = "Gaussian",nboot = nboot, npermut = npermut) 
    # Validity
    simulationOutput <- simulateResiduals(fittedModel = rep$mod, n = 1000)
    Unif=testUniformity(simulationOutput = simulationOutput)
    # Store
    temp=data.frame(species=uSpe[s],
                    R=round(rep$R[[1]],2),
                    lowCI=round(rep$CI_emp[[1]],2),
                    highCI=round(rep$CI_emp[[2]],2),
                    SS=length(unique(di$tag[di$species==uSpe[s]])),
                    task='L_L_R',
                    variable=varnames[r],
                    uniformity=round(Unif$p.value,3))
    res = rbind(res,temp)
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
pdf(paste0(out_rep,"All_variables_Repeatability table.pdf"), height=50, width=6)
grid.table(restot,rows =c())
dev.off()

# Export
pdf(paste0(out_rep,"AccFin100_Repeatability table.pdf"), height=1.5, width=3)
grid.table(restot[restot$variable=="AccFi100"&restot$task=="L_L",c(1:5)],rows =c())
dev.off()

# Export
pdf(paste0(out_rep,"AccFin_Repeatability table.pdf"), height=1.5, width=3)
grid.table(restot[restot$variable=="AccFin"&restot$task=="L_L",c(1:5)],rows =c())
dev.off()

pdf(paste0(out_rep,"Significant_Repeatability table.pdf"), height=1.5, width=3)
grid.table(restot[restot$lowCI>0&restot$uniformity>0.05,],rows =c())
dev.off()

# plot
#-----
ind=restot$uniformity>0.01&restot$task=="L_L"&restot$species=="All"
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

