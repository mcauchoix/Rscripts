# Test biological/ecological value  of cognitive variables from "di", 
#
# Maxime Cauchoix 
# created: 2/05/2019`
# adjusted for new 14 binary and 27 continuous variables 
# 11/11/2019
#----------------------------------------------------------------
library("forestplot")
# load indivual cognitive variables
di=read.csv2(paste(out_local,"all_ind_stats_afterCorelation.csv",sep=""),h=T)
#di=di[di$nbdsup>10,]
di$scenario=as.factor(di$scenario.x)
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


##########################
# PARTICIPATION RATE
##########################
# OF paper
#----------
# Variables selected 
OFvar=c("test_TTC10","test_gam","test_chance_lastDay","test_chance_100tr","test_FvsL_100tr")
QUANTvar=c("AccTot","AccFi100", "AccLastDay","AccDi100","TTC10")


#all
l_ss=matrix(nrow=2,ncol=length(OFvar))
for (i in 1:length(OFvar)){
  x=table(di[[OFvar[i]]])
  l_ss[,i]=x
}
l_ss=data.frame(l_ss)
names(l_ss)=OFvar
l_ss[3,]=round((l_ss[2,]/colSums(l_ss))*100)
l_ss[4,]=round(l_ss[2,]+l_ss[1,])
row.names(l_ss)=c("No learning","learning","(%) learning ","Total")

# store
pdf(paste0(out_bioVal,"Learning or not all.pdf"), height=2, width=10)
grid.table(l_ss)
dev.off()

# keep % 
all=l_ss[3,]
all_tot=l_ss[4,] 

#by scenario and species
n=2
uSpe=c("Great","Blue","Marsh")
for (j in 30:31){
  for (k in 1:length(uSpe)){
    ts=di[di$scenario==j&di$species==uSpe[k],]
    l_ss=matrix(nrow=2,ncol=length(OFvar))
    for (i in 1:length(OFvar)){
      x=table(ts[[OFvar[i]]])
      l_ss[,i]=x
    }
    l_ss=data.frame(l_ss)
    names(l_ss)=OFvar
    l_ss[3,]=round((l_ss[2,]/colSums(l_ss))*100)
    row.names(l_ss)=c("No learning","learning","(%) learning ")
    l_ss[4,]=round(l_ss[2,]+l_ss[1,])
    row.names(l_ss)=c("No learning","learning","(%) learning ","Total")

    # write
    pdf(paste0(out_bioVal,"Learning or not ", j ,"_", uSpe[k], ".pdf"), height=2, width=10)
    grid.table(l_ss)
    dev.off()
    # store %
    all[n,]=l_ss[3,] 
    all_tot[n,]=l_ss[4,] 
    n=n+1
  }
}


row.names(all)=c("All","Great\nON/OFF","Blue\nON/OFF","Marsh\nON/OFF",
                 "Great\nLEFT/RIGHT","Blue\nLEFT/RIGHT","Marsh\nLEFT/RIGHT")
pdf(paste0(out_bioVal,"Learning or not by senario.pdf"), height=10, width=10)
grid.table(all)
dev.off()

row.names(all_tot)=c("All","Great\nON/OFF","Blue\nON/OFF","Marsh\nON/OFF",
                 "Great\nLEFT/RIGHT","Blue\nLEFT/RIGHT","Marsh\nLEFT/RIGHT")
pdf(paste0(out_bioVal,"Learning or not by senario_total.pdf"), height=10, width=10)
grid.table(all_tot)
dev.off()


# Learning style paper
#----------

OFvar=c("test_TTC10","test_gam","test_chance_lastDay","test_chance_100tr","test_FvsL_100tr")
QUANTvar=c("AccTot","AccFi100", "AccLastDay","AccDi100","TTC10")


#all
l_ss=matrix(nrow=2,ncol=length(OFvar))
for (i in 1:length(OFvar)){
  x=table(di[[OFvar[i]]])
  l_ss[,i]=x
}
l_ss=data.frame(l_ss)
names(l_ss)=OFvar
l_ss[3,]=round((l_ss[2,]/colSums(l_ss))*100)
l_ss[4,]=round(l_ss[2,]+l_ss[1,])
row.names(l_ss)=c("No learning","learning","(%) learning ","Total")

# store
pdf(paste0(out_bioVal,"Learning or not all.pdf"), height=2, width=10)
grid.table(l_ss)
dev.off()

# keep % 
all=l_ss[3,]
all_tot=l_ss[4,] 

#by scenario and species
n=2
uSpe=c("Great","Blue","Marsh")
for (j in 30:31){
  for (k in 1:length(uSpe)){
    ts=di[di$scenario==j&di$species==uSpe[k],]
    l_ss=matrix(nrow=2,ncol=length(OFvar))
    for (i in 1:length(OFvar)){
      x=table(ts[[OFvar[i]]])
      l_ss[,i]=x
    }
    l_ss=data.frame(l_ss)
    names(l_ss)=OFvar
    l_ss[3,]=round((l_ss[2,]/colSums(l_ss))*100)
    row.names(l_ss)=c("No learning","learning","(%) learning ")
    l_ss[4,]=round(l_ss[2,]+l_ss[1,])
    row.names(l_ss)=c("No learning","learning","(%) learning ","Total")
    
    # write
    pdf(paste0(out_bioVal,"Learning or not ", j ,"_", uSpe[k], ".pdf"), height=2, width=10)
    grid.table(l_ss)
    dev.off()
    # store %
    all[n,]=l_ss[3,] 
    all_tot[n,]=l_ss[4,] 
    n=n+1
  }
}


row.names(all)=c("All","Great\nON/OFF","Blue\nON/OFF","Marsh\nON/OFF",
                 "Great\nLEFT/RIGHT","Blue\nLEFT/RIGHT","Marsh\nLEFT/RIGHT")
pdf(paste0(out_bioVal,"Learning or not by senario.pdf"), height=10, width=10)
grid.table(all)
dev.off()

row.names(all_tot)=c("All","Great\nON/OFF","Blue\nON/OFF","Marsh\nON/OFF",
                     "Great\nLEFT/RIGHT","Blue\nLEFT/RIGHT","Marsh\nLEFT/RIGHT")
pdf(paste0(out_bioVal,"Learning or not by senario_total.pdf"), height=10, width=10)
grid.table(all_tot)
dev.off()


##########################
# SPECIES, ELEVATION,SEX,AGE EFFECT
##########################

# OF paper
#----------

# Learning style paper
#----------
binaryVar=binaryVar[c(1,2,13,14)]
res=list()
for (i in 1:length(binaryVar)){
  F1 <- as.formula(paste(binaryVar[i],"elevation+species+scenario+(1|tag)",sep="~"))
  F1spe <- as.formula(paste(binaryVar[i],"elevation+scenario+(1|tag)",sep="~"))
  F1sce <- as.formula(paste(binaryVar[i],"elevation+species+(1|tag)",sep="~"))
  F1ele <- as.formula(paste(binaryVar[i],"species+scenario+(1|tag)",sep="~"))
  
  M1=glmer(F1,data=di,family="binomial")
  M1spe=glmer(F1spe,data=di,family="binomial")
  M1sce=glmer(F1sce,data=di,family="binomial")
  M1ele=glmer(F1ele,data=di,family="binomial")
  
  S1spe=anova(M1,M1spe)
  S1sce=anova(M1,M1sce)
  S1ele=anova(M1,M1ele)
  
  temp=data.frame(variable=binaryVar[i],species="all", 
                  species.effect=S1spe$`Pr(>Chisq)`[2]<0.05,
                  scenario.effect=S1sce$`Pr(>Chisq)`[2]<0.05,
                  elevation.effect=S1ele$`Pr(>Chisq)`[2]<0.05)

  res[[i]]=temp
  
}
restot=rbindlist(res)


pdf(paste0(out_bioVal,"Species_elevation_scenario_all.pdf"), height=8, width=8)
grid.table(restot,rows =c())
dev.off()

  
  
  ##########################
# FITNESS et SOCIAL NETWORK effect
##########################

# OF paper
#----------

# Learning style paper
#----------


