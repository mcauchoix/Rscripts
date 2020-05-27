# Look for cognition link with other variables
#-------------------
# load
#-------
# cognition
di=read.table(paste(out_f,"alldata.csv",sep=""),h=T)
#normalise
di$logTTC10=log(di$TTC10)
di$logTTC20=log(di$TTC20)
di$logTTC30=log(di$TTC30)
di$lognbts=log(di$nbts)
# loop on all variables
varnames=names(di)[c(13:37,39:45)+1]# +1 because I added bandNumber to check


# fitness
fi=read.csv2(paste0(out_local,"Nest_LHT_Data_AllYears_compilation.csv"),h=T,sep=";",na.strings = c("?","","NA"))
print(table(fi$Year))
# number of attempt by nest
x=data.table(table(fi$Year,fi$Nest))
fi$eggORnot=0
fi$eggORnot[fi$LayDate!=""]=1
# initial table with nbtrial by scenario
DT <- as.data.table(fi)
DT=DT[,list(nb.year.occupied =sum(eggORnot),nb.year.present=.N), by="Nest"]
dd=as.data.frame(DT)
write.csv2(dd,paste0(out_local,"nestOccupied.csv"),row.names = F)


# merge male and female
mFit=fi[fi$M_Band!='',]
mFit$sex="M"
mFit$bandNumber=mFit$M_Band

fFit=fi[fi$F_Band!='',]
fFit$sex="F"
fFit$bandNumber=fFit$F_Band
fiB=rbind(mFit,fFit)
print(paste("Nb of bird in fitness database",length(unique(fiB$bandNumber))))
print(paste(sum(unique(di$bandNumber) %in% unique(fiB$bandNumber)),"with fitness data"))


# Overlap cog and fitness 2019
print(paste(sum(unique(di$bandNumber[di$species=="Great"]) %in% unique(fiB$bandNumber[fiB$Year==2019])),"Great with fitness data in 2019"))
print(paste(sum(unique(di$bandNumber[di$species=="Blue"]) %in% unique(fiB$bandNumber[fiB$Year==2019])),"Blue with fitness data in 2019"))

#  Overlap cog and fitness 2018
print(paste(sum(unique(di$bandNumber[di$species=="Great"]) %in% unique(fiB$bandNumber[fiB$Year==2018])),"Great with fitness data in 2018"))
print(paste(sum(unique(di$bandNumber[di$species=="Blue"]) %in% unique(fiB$bandNumber[fiB$Year==2018])),"Blue with fitness data in 2018"))

#  Overlap cog and fitness before 2018
print(paste(sum(unique(di$bandNumber[di$species=="Great"]) %in% unique(fiB$bandNumber[fiB$Year<2019])),"Great with fitness data before 2018"))
print(paste(sum(unique(di$bandNumber[di$species=="Blue"]) %in% unique(fiB$bandNumber[fiB$Year<2019])),"Blue with fitness data before 2018"))



# # mean by birds
# DT <- as.data.table(fiB)
# DT=DT[,list(nbY =.N,firstYear=min(Year),lastYear=max(Year),
#             Species=unique(Species),sex=unique(sex),
#             FledgeNb=mean(FledgeNb,na.rm=T),
#             EggNb=mean(EggNb,na.rm=T)), by="bandNumber"]
# f=as.data.frame(DT)

f=fiB[which(fiB$Year==2019),]

# merge cog and fit
all=merge(di,f,by="bandNumber",all.x=F)

# new variables
all$learn="Learning"
all$learn[is.na(all$TTC10)]="Nolearning"

# models
#-------
m=lmer(FledgeNb~AccFi100+sex.y+age+elevation+(1|Nest),data=all,subset = all$species=="Great")
summary(m)
# trial effect of all cognitive variables
#----------------------------------------

stat=list()
for (r in 1:length(varnames)){
  ind=all$species=="Blue"&all$scenario=="30"
  
  formF <- as.formula(paste0("FledgeNb~",varnames[r]))
  formE <- as.formula(paste0("EggNb~",varnames[r]))
  
  
  e=summary(lm(formF,data=all,subset=ind))
  f=summary(lm(formE,data=all,subset=ind))
  #store
  statTemp=data.frame(variable=varnames[r],
                      pvalFledglings=round(f$coefficients[2,4],2),
                      pvalEggs=round(e$coefficients[2,4],2))
  stat[[r]]=statTemp
}

statFinal=rbindlist(stat)




# exploration plots
#--------------------
# Nb fledgings learning or not
ind=all$species=="Blue"&all$scenario=="30"

estim<- dabest(
  all[!is.na(all$FledgeNb)&ind,],learn,FledgeNb,idx=c("Learning","Nolearning"),
  paired = FALSE
)
pdf(paste0(out_fit,"NbFledge_Learning Or not_blueTits_30_2019.pdf"),height = 4,width = 4)
plot(estim,rawplot.ylabel="Nb fledging")
dev.off()


# Plot NbFledge_AccFi100
pdf(paste0(out_fit,"NbFledge_AccFi100_blueTits_30_2019.pdf"))
ggplot(subset(all,ind), aes(y=FledgeNb, x=AccFi100)) + 
  geom_point(aes(colour=sex.y),size=6) +  stat_smooth(method="lm", formula=y~x^2,aes(colour=sex.y))+  theme_bw()+
  theme(text = element_text(size=18))+
  ggtitle("Learning ON/OFF") +
  ylab("Nb fledglings") + xlab("Accuracy last 100 trials")
dev.off() 

# Plot AccPost50_10
pdf(paste0(out_fit,"NbFledge_AccPost50_10_blueTits_30.pdf"))
ggplot(subset(all,ind), aes(y=FledgeNb, x=AccPre50_10)) + 
  geom_point(aes(colour=sex.y),size=6) +  stat_smooth(method="lm", formula=y~x^2)+  theme_bw()+
  theme(text = element_text(size=18))+
  ggtitle("Learning ON/OFF") +
  ylab("Nb fledglings") + xlab("Accuracy post TTC")
dev.off() 


# Plot AccFi30 nb egg
pdf(paste0(out_fit,"NbEgg_AccFi30_blueTits_30.pdf"))
ggplot(subset(all,ind), aes(y=EggNb, x=AccFi30)) + 
  geom_point(aes(colour=sex.y),size=6) +  stat_smooth(method="lm", formula=y~x^2)+  theme_bw()+
  theme(text = element_text(size=18))+
  ggtitle("Learning ON/OFF") +
  ylab("Nb eggs") + xlab("AccFi30")
dev.off() 

