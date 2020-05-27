# clean banding data
# Mcauchoixxx July 2018
# -----------------------------------------
# Package
#-------------
library('dplyr')
library('varhandle')
# Function
#--------------

# Path
#--------------

# Load 31/12/2017 CRBPO data
#---------------------------
b=read.csv('~/Documents//wild_cog_OF/banding/All_Banding_Tits_29102019.csv',sep=';',na.strings = c("NA","No Value","?","Undetermined","",'No','/'),dec=',',colClasses = "character")
b=b[,4:32]
# clean
#b=b[!is.na(b$BandNumber),]
#b$BandNumber[!check.numeric(b$BandNumber)]

# # Load 2018 data till September
# #---------------------------
# bs=read.csv('~/Dropbox/wild_cog_OF/banding/1_01_2018TOSeptember2018.csv',sep=';',na.strings = c("NA","No Value","?","Undetermined","",'No','/'),dec=',',colClasses = "character")
# 
# # Load ongoing banding data
# #---------------------------
# bo=read.csv('~/Dropbox/wild_cog_OF/banding/Winter2018-2019.csv',sep=';',na.strings = c("NA","No Value","?","Undetermined",""),colClasses = "character")
# bo=bo[1:27]


#megre
#b=bind_rows(b,bo)
#b=rbind(b[c('BandNumber','RFID.','Species','Recapture','Site','Bander','Sex','Age','Wing.chord','Tarsus','Date','Time')],bs[c('BandNumber','RFID.','Species','Recapture','Site','Bander','Sex','Age','Wing.chord','Tarsus','Date','Time')],bo[c('BandNumber','RFID.','Species','Recapture','Site','Bander','Sex','Age','Wing.chord','Tarsus','Date','Time')])
#b=b[c('BandNumber','Color','RFID.','Species','Recapture','Site','Bander','Sex','Age','Wing.chord','Tarsus','Date','Time')]
#---------------------------------------------------------------------
# Clean data
#---------------------------------------------------------------------


#Species
b$Species[b$Species=='coal']="Coal"
b$Species[b$Species=='Marshtit'|b$Species=='Marsh '|b$Species=='MT']="Marsh"
b$Species[b$Species=='great'|b$Species=='GT '|b$Species=='GT']="Great"
b$Species[b$Species=='blue'|b$Species=='Blue tit']="Blue"
b$Species[b$Species=='crested']="Crested"
b$Species=factor(b$Species)
table(b$Species)

#Bander
table(b$Bander)
b$Bander[b$Bander %in% c('Ab')]='AB'
b$Bander[b$Bander %in% c('Bm')]='BM'
b$Bander[b$Bander %in% c('Cm')]='CM'
b$Bander[b$Bander %in% c('Es')]='ES'
b$Bander[b$Bander %in% c('Joel WHITE')]='JW'
b$Bander[b$Bander %in% c('Mc')]='MC'
b$Bander[b$Bander %in% c('Mb')]='MB'


#Site
b$Site=as.character(b$Site)
b$Site[b$Site %in% c('CES1','Ces1','Cescau 1','Cescau')]='C1'
b$Site[b$Site %in% c('CES4','Ces4','CES4','Cesc 4','Cescau haut')]='C4'
b$Site[b$Site %in% c('CES3','Ces3')]='C3'
b$Site[b$Site %in% c('L1_Moulis','Lab','Moulis lab')]='L1'
b$Site[b$Site %in% c('MOU5','MOU5\n')]='M5'
b$Site[b$Site %in% c('Mou1','MOU1','Moulis')]='M1'
b$Site[b$Site %in% c('MOU2','MOU2\n')]='M2'
b$Site[b$Site %in% c('Arrech','Arrech north')]='H4'
b$Site[b$Site %in% c('Castera bas')]='H1'
b$Site[b$Site %in% c('Afr garden','GaleyAFR','galey','Galey','Andy','Galey ')]='G1'
b$Site[b$Site %in% c('Cap de sour','Cap sour','Capsour')]='CS'
b$Site[b$Site %in% c('JJTL','JJap')]='JJ'
b$Site[b$Site %in% c('JPla','JarPlaTlse')]='JP'
b$Site[b$Site %in% c('Grotte Aliou','Grotte aliou')]='Grotte aliou'
b$Site[b$Site %in% c('Nest','nest')]='Nest'
b$Site[b$Site %in% c('Gajan','GJ')]='GJ'
b$Site[b$Site=='G1'&b$Bander=='MC']='GJ'
b$Site[b$Site %in% c('BA','Balacet')]='BA'

b$Site=factor(b$Site)
table(b$Site)

#Sex
table(b$Sex)
b$Sex[b$Sex %in% c('f','f ')]='F'
b$Sex[b$Sex %in% c('m')]='M'
b$Sex[!(b$Sex %in% c('M','F'))]=NA
b$Sex=factor(b$Sex)

#Age
table(b$Age)
b$Age[b$Age %in% c(' +1a','+1A','+1a.','>1A')]='+1a'
b$Age[b$Age %in% c('1A','+1A','+1a.')]='1a'
b$Age[!(b$Age %in% c('+1a','1a','PUL'))]=NA
b$Age=factor(b$Age)

# recapture
b$Recapture2='New Bird'
b$Recapture2[b$Recapture %in% c('Recapture','Recap','recap','Yes','recapture','yes')]='Recapture'
b$duplicat=duplicated(b$BandNumber)

# wing chord
b$Wing.chord.num=as.numeric(gsub(b$Wing.chord, pattern=',', replacement='.'))
# b$Wing.chord[!check.numeric(b$Wing.chord)]
# which(!check.numeric(b$Wing.chord))
# b=b[which(b$Wing.chord<96),]

# head
#b$Head.num=as.numeric(lapply(b$Head, gsub, pattern=',', replacement='.'))
b$Head.num=as.numeric(gsub(b$Head, pattern=',', replacement='.'))
#b$Head[which(b$Head.num>60)]
# tarsus
b$Tarsus.num=as.numeric(gsub(b$Tarsus, pattern=',', replacement='.'))
b$Tarsus[which(b$Tarsus.num>30)]


# color
b$Color=toupper(b$Color)
#b$Color2=gsub(b$Color, pattern=' ', replacement='')
sum(nchar(b$Color)==5,na.rm=T)
# blood
b$bloodCorrect="Yes"
b$bloodCorrect[b$Blood %in% c('NA','None','0','no-value','No  ','No ','NO','no ','no','No_Value',NA)]="No"
#-------------------------
# Clean RFID and detect by RFID to correct
#------------------------
# RFID as character
b$RFID.=as.character(b$RFID.)

# remove #
b$RFID. <- lapply(b$RFID., gsub, pattern='#', replacement='')

# tag length
b$TagLength=nchar(b$RFID.)

# all in majuscule
b$RFID. <- lapply(b$RFID., toupper)
# convert back to factor

# convert back to factor
b$RFID.=as.character(b$RFID.)
b$RFID.=as.factor(b$RFID.)

# tag with issue
bad_tag_ind=b$TagLength<10&b$TagLength>0|b$TagLength>10
bad_tag_ind=b$TagLength!=10&b$RFID.!="None"&b$RFID.!="NONE"&b$RFID.!="NONE "&b$RFID.!="OK"&!is.na(b$RFID.)
# investigate
print("------RFID to correct-----")
print(unique(b$RFID.[bad_tag_ind]))
print("--------------------------")
#save in a file
write.table(b[bad_tag_ind,],paste('~/Documents/wild_cog_OF/banding/',"tag_errors.csv"),sep=";",row.names = F)



#---------------------------------------------------------------------
# Recode variables 
#---------------------------------------------------------------------
#Environement: urban, low or high elevation
b$environement[b$Site %in% c('Brienne','ENFA','JJ','JP')]="Urban"
b$environement[b$Site %in% c('Montegut','MOU3','M5','M2','Ledar','M1','L1','GJ','C1','Aubert','Asc garden','Montjoie')]="low"
b$environement[b$Site %in% c('H4','H1','G1','CS','C4','BA')]="high"
b$environement=factor(b$environement)
#date
# b$dayDate[which(b$sizeDate==10)]=as.Date(b$Date[which(b$sizeDate==10)],"%d/%m/%Y")# convert date
# b$dayDate[which(b$sizeDate==8)]=as.Date(b$Date[which(b$sizeDate==8)],"%d/%m/%y")# convert date

# convert date
#-----------------------------
# nb character in date
b$dateLength=nchar(as.character(b$Date))
# different kind of dat
table(b$dateLength)
# 9= missing 0 in month
b$Date[which(b$dateLength==9)]=paste0(substr(b$Date[which(b$dateLength==9)],1,3),'0',substr(b$Date[which(b$dateLength==9)],4,9))
b$dateLength=nchar(as.character(b$Date))


# date as a date dd/mm/yy
b$dateCorrected=as.Date(b$Date,format = "%d/%m/%y")
# if dd/mm/yyyy (TO DO:DEAL WITH dd/M/yyyy)
b$dateCorrected[which(b$dateLength==10)]=as.Date(b$Date[which(b$dateLength==10)],format = "%d/%m/%Y")

# 7 month without 0/day/year
b$dateCorrected[which(b$dateLength==7)]=as.Date(paste0("0",b$Date[which(b$dateLength==7)]),format = "%m/%d/%y")

# 11 = spring 2013
b$dateCorrected[which(b$dateLength==11)]=as.Date("01/05/13",format = "%d/%m/%y")
#write.xlsx(b,'/Users/maximecauchoix/IAST Dropbox/maxime cauchoix/wild_cog_OF/banding/All_Banding_Tits_30_12_2018_After automated clean.csv',sep=";")


