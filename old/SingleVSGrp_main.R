# Single vs group 2016-2017- cecile experiment
#--------------------------------------------------------------------
# library
library('Matrix')
library('lme4')
library('lmerTest')


# read data
#-----------
 d=read.csv('/Users/maximecauchoix/Dropbox/cognitive repeatability/tits/data/tits_spatial_color_reversal_single_cauchoix.csv',header=T,na.strings=c("No Value",'NaN'))

# Nb by group
#------------------
table(d$IDmart,d$Nb)

# performance by group
#--------------------
pdf(paste(file="/Users/maximecauchoix/Dropbox/SingleVsGroup.pdf",sep=""))
boxplot(d$AccByPhase~d$Nb,data=d,ylab="Accuracy",xlab ="Groupe size")
dev.off()
#stats
#--------------------
summary(lmer(AccByPhase~Nb+Sex+Age+phase+(phase|IDmart),data=d,subset=Sex!='?'))
summary(glmer(log(nbTrialNoMissedByPhase)~Nb+Sex+Age+phase+(phase|IDmart),data=d,subset=Sex!='?'))

