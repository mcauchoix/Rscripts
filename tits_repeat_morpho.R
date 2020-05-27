library("rptR")# repeatability computation
library("DHARMa")
library("data.table")


out="Dropbox/spring/Rscripts/Rscripts_Maxime/injury/"

# beack and tarsus repeat
d=read.csv2("/Users/maximecauchoix/Documents/wild_cog_OF/banding/Repetabilite.csv",header = T,dec=".")

varnames=names(d)[4:7]

resList=list()
for (r in 1:length(varnames)){
  
  # all species
  #-------------
  # formula for all species
  form <- as.formula(paste(varnames[r],"Species+(1|Code)",sep="~"))
  # 2 learning only
  rep=rpt(form, grname = c("Code"), data = d, datatype = "Gaussian",nboot = 10, npermut = 10) 
  # Validity
  simulationOutput <- simulateResiduals(fittedModel = rep$mod, n = 1000)
  Unif=testUniformity(simulationOutput = simulationOutput)
  # Store
  res=data.frame(species='All',
                 variable=varnames[r],
                 R=round(rep$R[[1]],2),
                 lowCI=round(rep$CI_emp[[1]],2),
                 highCI=round(rep$CI_emp[[2]],2),
                 SS=length(unique(d$Code)),
                 uniformity=round(Unif$p.value,3))
  
  # formula for each species
  form <- as.formula(paste(varnames[r],"(1|Code)",sep="~"))
  uSpe=unique(d$Species)
  for (s in 1:length(uSpe)){
    # 2 learning only
    ind=d$Species==uSpe[s]&!is.na(d[,varnames[r]])
    if(sum(ind)>2)
    {
    rep=rpt(form, grname = c("Code"), data = d[ind,], datatype = "Gaussian",nboot = 10, npermut = 10) 
    # Validity
    simulationOutput <- simulateResiduals(fittedModel = rep$mod, n = 1000)
    Unif=testUniformity(simulationOutput = simulationOutput)
    # Store
    temp=data.frame(species=uSpe[s],
                    variable=varnames[r],
                    R=round(rep$R[[1]],2),
                    lowCI=round(rep$CI_emp[[1]],2),
                    highCI=round(rep$CI_emp[[2]],2),
                    SS=length(unique(d$Code[d$Species==uSpe[s]])),
                    uniformity=round(Unif$p.value,3))
    res = rbind(res,temp)
    }
    
  }
  resList[[r]]=res
  
}

restot=rbindlist(resList)
write.table(restot,file=paste0(out_rep,"RepeatabilityData.csv"),sep = ";",row.names = F)

# Export
pdf(paste0(out_rep,"Repeatability morpho.pdf"), height=6, width=6)
grid.table(restot,rows =c())
dev.off()

  