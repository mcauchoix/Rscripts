d=read.csv2("/Users/maximecauchoix/Dropbox/spring/Raw Data/Banding/Bander comparison gajan 16-08-19.csv",header = T)

# R by trait
tr=unique(d$measure)
resList=list()
for (i in 1:length(tr))
{

  rep=rpt(value~spe+(1|ID), grname = c("ID"), data = d[d$measure==tr[i],], datatype = "Gaussian",nboot = 100, npermut = 0) 
  # Validity
  simulationOutput <- simulateResiduals(fittedModel = rep$mod, n = 1000)
  Unif=testUniformity(simulationOutput = simulationOutput)
  # Store
  resList[[i]]=data.frame(variable=tr[i],
                  R=round(rep$R[[1]],2),
                  lowCI=round(rep$CI_emp[[1]],2),
                  highCI=round(rep$CI_emp[[2]],2))
}

res=rbindlist(resList)
print(res)

# correlation
cor.test(d$value[d$obs=="nor"],d$value[d$obs=="max"])
cor.test(d$value[d$obs=="nor"],d$value[d$obs=="mar"])
cor.test(d$value[d$obs=="max"],d$value[d$obs=="mar"])

# difference between observer
mean(abs(d$value[d$obs=="nor"]-d$value[d$obs=="max"]))
mean(abs(d$value[d$obs=="max"]-d$value[d$obs=="mar"]))
mean(abs(d$value[d$obs=="nor"]-d$value[d$obs=="mar"]))


