# give a main result on fitness
#--------------------------------------
out_local='~/Documents/openfeeder/data/'


out="/Users/maximecauchoix/Dropbox/spring/Results/fitness/"
  
# read data
#----------
fi=read.csv2(paste0(out_local,"Nest_LHT_Data_AllYears_compilation_MC.csv"),h=T,sep=";",
             na.strings=c("?","","NA"))

# read nestbox information
#----------------
n=read.csv2(paste0(out_local,"NestBoxInformation_MB.csv"),h=T,sep=";")
n=n[,1:9]

# read vegetation
#----------------
v=read.csv2(paste0(out_local,"Compilation_Vegetation.csv"),h=T,sep=";",
            na.strings=c("?","","NA","No Value"))
v=v[order(v$Years,decreasing =T),]
v=v[!duplicated(v$Nest_Nb),]


#---------------
# 1- Clean data
#---------------

# clean Nest
#----------------
nestNameLength=nchar(as.character(fi$Nest))
table(nestNameLength)
table(nestNameLength,fi$Year)

#fi$Nest[nestNameLength==2] #s1 s2 N6 N8
#fi$Nest[nestNameLength==3]#N10 N11 N14 N10 N15
fi$Nest[nestNameLength==5] # MXXXb ... !!! check the one with same 1st pattern
#fi$Nest[nestNameLength==6] #Lizard C022-2 C098-2 C124-2 C145-2 C153-2 C156-2 !!! find in database
#fi$Nest[nestNameLength==7] #G163Bis L008Bis M111BIS

# clean brutforce
fi$NestCorrected=substr(fi$Nest,1,4)

# to understand nest movemennt
nestYear_fi=table(fi$Nest,fi$Year)# nest present by year in the fitness file
write.csv2(nestYear_fi,paste0(out,"nestPresentByYear_LHT.csv"))

n$name_both=paste(n$BoxName,n$Raw_Box_Name)# nest present by year in the nestinformation file
nestYear_n=table(n$name_both,n$Year)
write.csv2(nestYear_n,paste0(out,"nestPresentByYear_NestBoxinformation.csv"))




# nest in NestBoxinformation and not in LHT
sink(paste0(out,"Nest in NestBoxinformation and not in LHT.txt"))# store erros
print(setdiff(n$BoxName,fi$Nest))
sink()

sink(paste0(out,"Nest in LHT and not in NestBoxinformation.txt"))# store erros
print(setdiff(fi$Nest,n$Raw_Box_Name))
sink()

sink(paste0(out,"Nest in LHT and not in NestBoxinformation_Raw_Box_Name.txt"))# store erros
print(setdiff(fi$Nest,n$Raw_Box_Name))
sink()


# nest in  in LHT and not in NestBoxinformation



#write.table(b[bad_tag_ind,],paste0('~/Documents/wild_cog_OF/banding/',"tag_errors.csv"),sep=";",row.names = F)

# TO DO nestNameLength==5
# if alone this year= just remove last letter
# if 2 and 1 with no species= keep the one with something
# if 2 and species in both check data

# clean Site
#-----------
#dd$Site[dd$Site %in% c('C1','C3','C2','C4','C5')]="Cescau"
fi$SiteCorrect="Not a study site"
fi$SiteCorrect[grep("H",fi$Nest)]="Castera"
fi$SiteCorrect[grep("C",fi$Nest)]="Cescau"
fi$SiteCorrect[grep("B",fi$Nest)]="Balacet"
fi$SiteCorrect[grep("L",fi$Nest)]="Lab"
fi$SiteCorrect[grep("M",fi$Nest)]="Moulis"
fi$SiteCorrect[grep("G",fi$Nest)]="Galey"

# clean Year
#-----------
table(fi$Year)
setdiff(fi$Nest[fi$Year=="2018"],fi$Nest[fi$Year=="2019"])
setdiff(fi$Nest[fi$Year=="2019"],fi$Nest[fi$Year=="2018"])


# clean LayDate
#-----------
# nb character in date 
fi$LayDateIni=fi$LayDate #keep inital data
fi$LayDate=as.character(fi$LayDate)
fi$LayDateLength=nchar(as.character(fi$LayDate))
# different kind of dat
table(fi$LayDateLength)
# 8= missing 0 in day and month
ind=fi$LayDateLength==8&substr(fi$LayDate,2,2)=="/"
fi$LayDate[which(ind)]=paste0('0',fi$LayDate[which(ind)])
fi$LayDateLength=nchar(as.character(fi$LayDate))
# 9= missing 0 in month
fi$LayDate[which(fi$LayDateLength==9)]=paste0(substr(fi$LayDate[which(fi$LayDateLength==9)],1,3),'0',substr(fi$LayDate[which(fi$LayDateLength==9)],4,9))
fi$LayDateLength=nchar(as.character(fi$LayDate))
# date as a date dd/mm/yy
fi$LayDateCorrected=as.Date(fi$LayDate,format = "%d/%m/%y")
# if dd/mm/yyyy (TO DO:DEAL WITH dd/M/yyyy)
fi$LayDateCorrected[which(fi$LayDateLength==10)]=as.Date(fi$LayDate[which(fi$LayDateLength==10)],format = "%d/%m/%Y")
cbind(as.character(fi$LayDateIni),as.character(fi$LayDateCorrected)) #TO check

# clean hatchdate
#-----------
# nb character in date
fi$HatchDateIni=fi$HatchDate# keep inital data
fi$HatchDate=as.character(fi$HatchDate)
fi$HatchDateLength=nchar(as.character(fi$HatchDate))
# different kind of dat
table(fi$HatchDateLength)
# 8= missing 0 in day and month
ind=fi$HatchDateLength==8&substr(fi$HatchDate,2,2)=="/"
fi$HatchDate[which(ind)]=paste0('0',fi$HatchDate[which(ind)])
fi$HatchDateLength=nchar(as.character(fi$HatchDate))
# 9= missing 0 in month
fi$HatchDate[which(fi$HatchDateLength==9)]=paste0(substr(fi$HatchDate[which(fi$HatchDateLength==9)],1,3),'0',substr(fi$HatchDate[which(fi$HatchDateLength==9)],4,9))
fi$HatchDateLength=nchar(as.character(fi$HatchDate))
# date as a date dd/mm/yy
fi$HatchDateCorrected=as.Date(fi$HatchDate,format = "%d/%m/%y")
# if dd/mm/yyyy (TO DO:DEAL WITH dd/M/yyyy)
fi$HatchDateCorrected[which(fi$HatchDateLength==10)]=as.Date(fi$HatchDate[which(fi$HatchDateLength==10)],format = "%d/%m/%Y")
#cbind(fi$HatchDateIni,as.character(fi$HatchDateCorrected)) #TO check

# clean fledgedate
#-----------
# nb character in date
fi$FledgeDateIni=fi$FledgeDate
fi$FledgeDate=as.character(fi$FledgeDate)
fi$FledgeDateLength=nchar(as.character(fi$FledgeDate))
# different kind of dat
table(fi$FledgeDateLength)
# 8= missing 0 in day and month
ind=fi$FledgeDateLength==8&substr(fi$FledgeDate,2,2)=="/"
fi$FledgeDate[which(ind)]=paste0('0',fi$FledgeDate[which(ind)])
fi$FledgeDateLength=nchar(as.character(fi$FledgeDate))
# # 9= missing 0 in month
# fi$LayDate[which(fi$dateLength==9)]=paste0(substr(fi$LayDate[which(fi$dateLength==9)],1,3),'0',substr(fi$LayDate[which(fi$dateLength==9)],4,9))
# fi$dateLength=nchar(as.character(fi$LayDate))
# date as a date dd/mm/yy
fi$FledgeDateCorrected=as.Date(fi$FledgeDate,format = "%d/%m/%y")
# if dd/mm/yyyy (TO DO:DEAL WITH dd/M/yyyy)
fi$FledgeDateCorrected[which(fi$FledgeDateLength==10)]=as.Date(fi$FledgeDate[which(fi$FledgeDateLength==10)],format = "%d/%m/%Y")
cbind(fi$FledgeDateIni,as.character(fi$FledgeDateCorrected)) #TO check

# clean species
#--------------
fi$Species[fi$Species=="CR"]="Crested"
fi$Species=as.factor(fi$Species)
#----------------------
# Merge with elevation
#-----------------------
n$NestYear=paste0(n$BoxName,n$Year)
# remove nest with 2 lines from nestboxinformation
n$duplicated=duplicated(n$NestYear)
n=n[!n$duplicated,]

fi$NestYear=paste0(fi$NestCorrected,fi$Year)
setdiff(n$NestYear,fi$NestYear)
setdiff(fi$NestYear,n$NestYear)
fi$unique=c(1:dim(fi)[1])
fi=merge(fi,n,by="NestYear",sort=F,all.x=T)
#a=data.table(table(fi2$unique))
#a$V1[a$N>1]
fi$Elevation=as.numeric(as.character(fi$Elevation))

#----------------------
# Merge with vegetation
#-----------------------
v$Nest=v$Nest_Nb
fi=merge(fi,v,by="Nest",sort=F,all.x=T)
unique(fi$Habitat)
fi$HabiatClean[fi$Habitat %in% c("Clairiere","Edge","edge")]="Clairiere"
fi$HabiatClean[fi$Habitat=="Meadow"]="Prairie"
fi$HabiatClean[fi$Habitat=="Roadside"]="BordRoute"
fi$HabiatClean[fi$Habitat=="Thin"|fi$Habitat=="Small"]="ForetPetitTronc"
fi$HabiatClean[fi$Habitat=="Medium"]="ForetMoyenTronc"
fi$HabiatClean[fi$Habitat=="Big"]="ForetGrosTronc"
fi$HabiatClean[fi$Habitat=="Bog"]="Tourbiere"


#----------------------
# save for student project
#-----------------------
d=data.frame(nichoir=fi$NestCorrected,
             site=fi$Site.y,
             zone=fi$Woodlot,
             altitude=fi$Elevation,
             milieu=fi$HabiatClean,
             annee=fi$Year.x,
             espece=fi$Species,
             #nbOccupation=fi$nest_attempt.0.1.2.,
             datePonte=fi$LayDateCorrected,
             nbOeuf=fi$EggNb,
             dateEclosion=fi$HatchDateCorrected,
             nbPoussinEclos=fi$HatchNb,
             dateEnvol=fi$FledgeDateCorrected,
             nbPoussinEnvol=fi$FledgeNb
             )
d=d[order(d$annee),]


#i=d$espece=="B"&d$annee=="2019"
#plot(d$altitude[i],d$nbPoussinEnvol[i])
write.csv2(d,paste0(out_local,"THV_2013-19.csv"),row.names = F)

#----------------------
# Simple data analyis
#-----------------------
fi$Year=fi$Year.x

# nest occupancy
#----------
# all species
fi$eggORnot=0
fi$eggORnot[fi$LayDate!=""]=1
fi$eggORnot[fi$Year==2015&fi$FledgeNb!=""]=1

DT <- as.data.table(fi)
DT=DT[,list(nb.year.occupied =sum(eggORnot),nb.year.present=.N, last.date=max(Year,rm.na=T)), by="Nest,SiteCorrect"]
dd=as.data.frame(DT)
write.csv2(dd,paste0(out_local,"nestOccupency.csv"),row.names = F)
write.csv2(dd[which(dd$nb.year.present>2 &dd$last.date==2019&dd$nb.year.occupied==0&dd$SiteCorrect!="Galey"), ],paste0(out_local,"BadnestOccupency.csv"),row.names = F)

# Blue and great only
fi$eggORnot_BG=0
fi$eggORnot_BG[fi$LayDate!=""&fi$Species %in% c('G','B')]=1
fi$eggORnot_BG[fi$Year==2015&fi$FledgeNb!=""&fi$Species %in% c('G','B')]=1

DT <- as.data.table(fi)
DT=DT[,list(nb.year.occupied =sum(eggORnot_BG),nb.year.present=.N, last.date=max(Year,rm.na=T)), by="Nest,SiteCorrect"]
dd=as.data.frame(DT)
write.csv2(dd,paste0(out,"nestOccupency_BGonly.csv"),row.names = F)
write.csv2(dd[which(dd$nb.year.present>2 &dd$last.date==2019&dd$nb.year.occupied==0), ],paste0(out_local,"BadnestOccupency_BGonly.csv"),row.names = F)

# Nest ordered by laying date for each year
#----------------------
layOrder=data.frame(matrix(NA, nrow = 700, ncol = 7))
fi$LayDate_Day_Month <- format(as.Date(fi$LayDateCorrected), "%m-%d")


uYear=unique(fi$Year.x)
uYear=uYear[!is.na(uYear)]

for (i in 1:length(uYear))
{
  nestOcc=fi$Nest[fi$Year==uYear[i]&fi$eggORnot_BG==1&fi$LayDate_Day_Month<"05-01"]
  layDate=fi$LayDateCorrected[fi$Year==uYear[i]&fi$eggORnot_BG==1&fi$LayDate_Day_Month<"05-01"]
   nestOrder=nestOcc[order(layDate)]
  layOrder[1:length(nestOrder),i]=as.character(nestOrder)
}
names(layOrder)=c(2013:2019)
write.csv2(layOrder,paste0(out,"NestSortedByLayOrder.csv"))

# score of earliness laydate
#---------------------------------------

e=data.frame(nest=unique(fi$Nest[fi$Year=="2019"]))
for (i in 1:dim(e)[1]){
  nb=c()
  earl=c()
  n=1
  for (j in 1:dim(layOrder)[2]){
    nb[j]=sum(e$nest[i] %in% layOrder[[j]])
    if (nb[j]>0){
    earl[n]=which(e$nest[i]==layOrder[[j]])[1]
    n=n+1
    }
  }
  e$nbYearOccupied[i]=sum(nb)
  e$meanEarliness[i]=round(mean(earl))
}
e=e[order(e$meanEarliness),]
write.csv2(e,paste0(out,"NestSortedByEarliness.csv"))

e[e$nbYearOccupied>1&e$nest %in% fi$Nest[fi$Woodlot=="C4"],]


# % nest fledging after 11 May
#-----------------------------
#for (i in 1:length(uYear)){
fi$fledgeDate_Day_Month <- format(as.Date(fi$FledgeDateCorrected), "%m-%d")
fi$hatchDate_Day_Month <- format(as.Date(fi$HatchDateCorrected), "%m-%d")

totalWithFledgling=table(fi$SiteCorrect[fi$FledgeNb>1&!is.na(fi$FledgeDateCorrected)])#&fi$Year==uYear[i]
totalAfter11May=table(fi$SiteCorrect[fi$FledgeNb>1&fi$fledgeDate_Day_Month>"05-11"&!is.na(fi$FledgeDateCorrected)])
percAfetr11May=round(totalAfter11May/totalWithFledgling*100)
x=rbind(totalWithFledgling,totalAfter11May,percAfetr11May)
write.csv2(x,paste0(out,"FledgeAfter11May.csv"))

sum(fi$fledgeDate_Day_Month>"05-11",na.rm=T)/sum(fi$fledgeDate_Day_Month>"03-11",na.rm=T)
sum(fi$fledgeDate_Day_Month>"05-18",na.rm=T)/sum(fi$fledgeDate_Day_Month>"03-11",na.rm=T)

# nb of nest with chicks at more that 10 by day
#------------------------------------
# Moulis
ind=fi$Site.x=="Moulis"&fi$Species %in% c("G","B")
mat=c()  
uYear=c(2013:2019)
for (y in 1:length(uYear)){
  ref=seq(as.Date(paste0(as.character(uYear[y]),"-04-25")), as.Date(paste0(as.character(uYear[y]),"-06-20")), by="days")
  nb=c()
  for (i in 1:length(ref)){
    age=ref[i]-fi$HatchDateCorrected[fi$Year.x==uYear[y]&ind]
    nb[i]=sum(age>9&age<16,na.rm=T)
  }
  mat=cbind(mat,nb)
}
mat=data.frame(mat)
names(mat)=uYear
ref_m_d=format(as.Date(ref,format="%Y-%m-%d"), format = "%m-%d")
row.names(mat)=ref_m_d
mat$mean2016_2019=rowMeans(mat[,4:7])
write.csv2(mat,paste0(out,"Nb nests with G or B tits with more than 10 days in Moulis.csv"))

# mean for all sites
allSite=data.frame(M1=mat$mean2016_2019)
row.names(allSite)=ref_m_d


# C1
ind=fi$Woodlot=="C1"&fi$Species %in% c("G","B")
mat=c()  
uYear=c(2013:2019)
for (y in 1:length(uYear)){
  ref=seq(as.Date(paste0(as.character(uYear[y]),"-04-25")), as.Date(paste0(as.character(uYear[y]),"-06-20")), by="days")
  nb=c()
  for (i in 1:length(ref)){
    age=ref[i]-fi$HatchDateCorrected[fi$Year.x==uYear[y]&ind]
    nb[i]=sum(age>9&age<16,na.rm=T)
  }
  mat=cbind(mat,nb)
}
mat=data.frame(mat)
names(mat)=uYear
ref_m_d=format(as.Date(ref,format="%Y-%m-%d"), format = "%m-%d")
row.names(mat)=ref_m_d
mat$mean2016_2019=rowMeans(mat[,4:7])
write.csv2(mat,paste0(out,"Nb nests with G or B tits with more than 10 days in C1.csv"))

# mean for all sites
allSite$C1=mat$mean2016_2019

# C4
ind=fi$Woodlot=="C4"&fi$Species %in% c("G","B")
mat=c()  
uYear=c(2013:2019)
for (y in 1:length(uYear)){
  ref=seq(as.Date(paste0(as.character(uYear[y]),"-04-25")), as.Date(paste0(as.character(uYear[y]),"-06-20")), by="days")
  nb=c()
  for (i in 1:length(ref)){
    age=ref[i]-fi$HatchDateCorrected[fi$Year.x==uYear[y]&ind]
    nb[i]=sum(age>9&age<16,na.rm=T)
  }
  mat=cbind(mat,nb)
}
mat=data.frame(mat)
names(mat)=uYear
ref_m_d=format(as.Date(ref,format="%Y-%m-%d"), format = "%m-%d")
row.names(mat)=ref_m_d
mat$mean2016_2019=rowMeans(mat[,4:7])
write.csv2(mat,paste0(out,"Nb nests with G or B tits with more than 10 days in C4.csv"))

# mean for all sites
allSite$C4=mat$mean2016_2019

write.csv2(allSite,paste0(out,"Mean over 2016-2019 Nb nests with G or B tits with more than 10 days.csv"))

# nb couv??es by site
#-------------------
ind=fi$Site.x=="Moulis"&fi$Species %in% c("G","B")&fi$FledgeNb>0
M=data.frame(table(fi$Year.x[ind]))
names(M)=c('year','M')

ind=fi$Woodlot=="C1"&fi$Species %in% c("G","B")&fi$FledgeNb>0
C1=data.frame(table(fi$Year.x[ind]))

ind=fi$Woodlot=="C4"&fi$Species %in% c("G","B")&fi$FledgeNb>0
C4=data.frame(table(fi$Year.x[ind]))

M$C1=C1$Freq
M$C4=C4$Freq

write.csv2(M,paste0(out,"Nb of nestboxes with fledglings.csv"))



# fitness by individual
#----------------------
# remove # and put in maj
fi$F_RFID=as.character(fi$F_RFID)
fi$M_RFID=as.character(fi$M_RFID)
# remove #
fi$F_RFID<-toupper(gsub("#",'',fi$F_RFID)) 
fi$M_RFID<-toupper(gsub("#",'',fi$M_RFID)) 
# removes space
fi$F_RFID<-toupper(gsub(" ",'',fi$F_RFID)) 
fi$M_RFID<-toupper(gsub(" ",'',fi$M_RFID)) 
# nchar
fi$male.nchar=nchar(fi$M_RFID)
fi$female.nchar=nchar(fi$F_RFID)
# incorrect line
fi[fi$female.nchar==9,]
fi[fi$female.nchar==2,]

# # merge male and female with correct RIFD
# mFit=fi[fi$male.nchar==10,]
# mFit$sex="M"
# mFit$tag=mFit$M_RFID
# 
# fFit=fi[fi$female.nchar==10,]
# fFit$sex="F"
# fFit$tag=fFit$F_RFID
# 
# fiB=rbind(mFit,fFit)

# merge male and female with bandNumber
mFit=fi[fi$M_Band!='',]
mFit$sex="M"
mFit$bandNumber=mFit$M_Band

fFit=fi[fi$F_Band!='',]
fFit$sex="F"
fFit$bandNumber=fFit$F_Band
fiB=rbind(mFit,fFit)

# rename speceis
fiB$Species=as.character(fiB$Species)
fiB$Species[fiB$Species=="G"]="Great"
fiB$Species[fiB$Species=="B"]="Blue"
fiB$Species[fiB$Species=="M"]="Marsh"

# Coefficant selection on layDate 
#--------------------------------
# 0- deal with 2nd attempt
# TO DO
# 1- compute relative fitness by year by species
uYear=unique(fi$Year.x)
uSpe=c("B","C","M","G")
fi$relativeFit=NA

for(i in 1:length(uYear)){
  for (j in 1:length(uSpe)){
    ind=fi$Year.x==uYear[i]&fi$Species==uSpe[j]&!is.na(fi$FledgeNb)
    print(paste(uYear[i],uSpe[j],sum(ind,na.rm = T)))
    if (sum(ind,na.rm = T)>2){
      fi$relativeFit[which(ind)]=scale(fi$FledgeNb[which(ind)])
      # laying date from 1 of Mars
      
      # relative laying date
      fi$relativeLayDate[which(ind)]=scale(fi$FledgeNb[which(ind)])
    }
  }
}
# number of individuals with fledglings
ind=!is.na(fi$FledgeNb)&fi$Species %in% c("B","C","M","G")
nestWithFledgling=table(fi$Year.x[ind],factor(fi$Species[ind]))
write.csv2(nestWithFledgling,paste0(out,"nestWithFledgling.csv"))
# number of individuals with eggs   
ind=!is.na(fi$EggNb)&fi$Species %in% c("B","C","M","G")
nestWithEgg=table(fi$Year.x[ind],factor(fi$Species[ind]))
write.csv2(nestWithEgg,paste0(out,"nestWithEgg.csv"))

# check differcence in number of egg and number of fledglings
nestWithEgg[row.names(nestWithEgg)=="2013",]-
nestWithFledgling[row.names(nestWithFledgling)=="2013",]
  

