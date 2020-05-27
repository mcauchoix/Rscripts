# Look for overlap between cognition in winter 2018_2019 and fitness 2019 
#-------------------
library('dplyr')

# load
#-------
# cognition
di=read.table(paste(out_f,"alldata.csv",sep=""),h=T)
# #normalise
# di$logTTC10=log(di$TTC10)
# di$logTTC20=log(di$TTC20)
# di$logTTC30=log(di$TTC30)
# di$lognbts=log(di$nbts)
# # loop on all variables
# varnames=names(di)[c(13:37,39:45)+1]# +1 because I added bandNumber to check


# fitness
fi=read.csv2(paste0(out_local,"Timeline_allsites_2019_11_05.csv"),h=T,sep=";")
# remove # and put in maj
fi$Female.RFID=as.character(fi$Female.RFID)
fi$Male.RFID=as.character(fi$Male.RFID)
# remove #
fi$Female.RFID<-toupper(gsub("#",'',fi$Female.RFID)) 
fi$Male.RFID<-toupper(gsub("#",'',fi$Male.RFID)) 
# removes space
fi$Female.RFID<-toupper(gsub(" ",'',fi$Female.RFID)) 
fi$Male.RFID<-toupper(gsub(" ",'',fi$Male.RFID)) 

# nchar
fi$male.nchar=nchar(fi$Male.RFID)
fi$female.nchar=nchar(fi$Female.RFID)

# incorrect line
fi[fi$female.nchar==9,]


# merge male and female
mFit=fi[fi$male.nchar==10,]
mFit$sex="M"
mFit$tag=mFit$Male.RFID

fFit=fi[fi$female.nchar==10,]
fFit$sex="F"
fFit$tag=fFit$Female.RFID

fiB=rbind(mFit,fFit)

print(paste("Nb of bird in fitness database",length(unique(fiB$tag))))
print(paste(sum(unique(di$tag) %in% unique(fiB$tag)),"with fitness data"))

print(paste(sum(unique(di$tag[di$species=="Great"]) %in% unique(fiB$tag)),"Great with fitness data"))
print(paste(sum(unique(di$tag[di$species=="Blue"]) %in% unique(fiB$tag)),"Blue with fitness data"))
print(paste(sum(unique(di$tag[di$species=="Marsh"]) %in% unique(fiB$tag)),"Marsh with fitness data"))

print(paste(sum(unique(di$tag[di$species=="Great"]) %in% unique(fiB$tag)),"Male Great with fitness data"))
print(paste(sum(unique(di$tag[di$species=="Blue"]) %in% unique(fiB$tag)),"Blue Great with fitness data"))


# merge cog and fit
all=merge(fiB,di,by="tag",all.x=F)

# count all
length(unique(all$tag))

# ON/OFF
length(unique(all$tag[all$species=="Great"&all$scenario==30]))
length(unique(all$tag[all$species=="Blue"&all$scenario==30]))
length(unique(all$tag[all$species=="Marsh"&all$scenario==30]))

# Male
length(unique(all$tag[all$species=="Blue"&all$sex.x=="M"]))
length(unique(all$tag[all$species=="Great"&all$sex.x=="M"]))
length(unique(all$tag[all$species=="Marsh"&all$sex.x=="M"]))
# nest
length(unique(all$NestNb[all$species=="Blue"]))
length(unique(all$NestNb[all$species=="Great"]))
length(unique(all$NestNb[all$species=="Marsh"]))
  
  # site
length(unique(all$NestNb[all$site=="C1"]))
length(unique(all$NestNb[all$site=="C4"]))
length(unique(all$NestNb[all$site=="M1"]))
length(unique(all$NestNb[all$site=="BA"]))

# nest to analyse
nt=data.frame(unique(all$NestNb[!all$VidDate %in% c('','NA')]))
nt=all[!all$VidDate %in% c('','NA')&all$scenario==30,]
write.csv2(nt,paste0(out_local,"NestToAnalysePriority.csv"))


# count by task
table(all$scenario,all$species)

# new variables
all$learn="Learning"
all$learn[is.na(all$TTC10)]="Nolearning"

# exploration plots
#--------------------
# Nb fledgings learning or not
ind=all$species=="Blue"&all$scenario=="30"&all$sex.y=="M"
all$EggNb=as.numeric(all$EggNb)

estim<- dabest(
  all[!is.na(all$EggNb)&ind,],learn,EggNb,idx=c("Learning","Nolearning"),
  paired = FALSE
)
pdf(paste0(out_fit,"NbFledge_Learning Or not_blueTits_30.pdf"),height = 4,width = 4)
plot(estim,rawplot.ylabel="Nb fledging")
dev.off()


# Plot NbFledge_AccFi100
pdf(paste0(out_fit,"NbFledge_AccFi100_blueTits_30.pdf"))
ggplot(subset(all,ind), aes(y=EggNb, x=AccFi100)) + 
  geom_point(aes(colour=sex.y),size=6) +  stat_smooth(method="lm", formula=y~x^2,aes(colour=sex.y))+  theme_bw()+
  theme(text = element_text(size=18))+
  ggtitle("Learning ON/OFF") +
  ylab("Nb fledglings") + xlab("Accuracy last 100 trials")
dev.off() 