# read banding files and summarise by pitTag
# mcauchoixxx@gmail.com, 12/12/17
#------------------------------------
# to do
#-------
# Add banding 31/12/17 and september 2019
# add other variables (wing chord, tarsus etc...)
# correct bandnumber and species (put only 1 value! as for the rest)

# libraries
#-----------
# read xlsx
#library('rJava')
#library('xlsxjars')
#library('xlsx')
# merge data frame
library('data.table')
library('dplyr')
# folders
#-------------
out="~/Dropbox/wild_cog_OF/banding/"

#-------------------------------------
# A- Merge and Clean banding (ADD NUMERIC VARIABLE)
#-------------------------------------
source('~/IAST Dropbox/maxime cauchoix/wild_cog_OF/Rscripts/band_clean_2018_19.R')

#-------------------------------------
# C- Summarise 1 ligne by tag
#-------------------------------------
#OF_clean_summarise_banding <- function(b){
# keep only 1 line by bird
#-------------------------
# 1- validate RFID
#------------------------
# RFID as character
b$RFID.=as.character(b$RFID.)

# remove #
b$RFID. <- lapply(b$RFID., gsub, pattern='#', replacement='')

# tag length
b$TagLength=nchar(b$RFID.)

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
sink(paste(out,"tag_errors.txt"))# store erros
print(b[bad_tag_ind,])
sink()

#------------------------------------------
# 2- keep only 1 summary line by indidvidual 
#------------------------------------------
# remove entire row with NA
#b[rowSums(is.na(b))!=ncol(b), ]



# tag
#----
# all in majuscule
b$RFID. <- lapply(b$RFID., toupper)
# convert back to factor
b$RFID.=as.character(b$RFID.)
b$RFID.=as.factor(b$RFID.)
# keep unique GOOD TAG in bu
bu= data.frame(unique(b$RFID.[b$TagLength==10&!is.na(b$RFID.)]))
names(bu)='tag'


# date
#-----------------------------
# nb character in date
b$dateLength=nchar(as.character(b$Date))
# date as a date dd/mm/yy
b$dateCorrected=as.Date(b$Date,format = "%d/%m/%y")
# if dd/mm/yyyy
b$dateCorrected[which(b$dateLength==10)]=as.Date(b$Date[which(b$dateLength==10)],format = "%d/%m/%Y")

#order by date (assuming all dates are correct)
#-----------------------------
b=b[order(b$dateCorrected),]

# Open error file
#----------------
sink(paste(out,"banding_errors.txt"))# store erros
#---------------------------------------------------------------------
# add rest of banding variables of interest
#---------------------------------------------------------------------

for (i in 1: dim(bu)[1]) {
	# indeces of bu tag in all banding b
		ind_tag= which(b$RFID.==bu$tag[i]) #'0700EE2478'#b[ind_tag,]

	# BandNumber: take last band with 7 digits
	#------------
	# should be 7
	ind_tag_c=intersect(ind_tag,which(nchar(b$BandNumber)==7))
	x=unique(as.character(b$BandNumber[ind_tag_c]))
	# latest with 7
	latest_ind_tag=ind_tag_c[b$dateCorrected[ind_tag_c]==max(b$dateCorrected[ind_tag_c])]
	bu$bandNumber[i]=b$BandNumber[latest_ind_tag]
	# check if there is 2 different band number for same tag
	if (length(unique(b$BandNumber[ind_tag]))>1)
	{
	print('--------------------------')
	print(paste(bu$tag[i],' several BANDNUMBER for same tag ind',i,sep=":"))
	print('--------------------------')
	print(b[ind_tag,])
	}
	
	# Color combo: take last band with 4 digits
	#---------
	# should be 4
	ind_tag_c=intersect(ind_tag,which(nchar(b$Color)==4))
	x=unique(as.character(b$Color[ind_tag_c]))
	# latest with 4
	latest_ind_tag=ind_tag_c[b$dateCorrected[ind_tag_c]==max(b$dateCorrected[ind_tag_c])]
	bu$Color[i]=b$Color[latest_ind_tag]
	# check if there is 2 different color for same tag
	if (length(unique(b$Color[ind_tag]))>1)
	{
	  print('--------------------------')
	  print(paste(bu$tag[i],' several Color combo for same tag ind',i,sep=":"))
	  print('--------------------------')
	  print(b[ind_tag,])

	}

	
	# Species: should be only 1 correct Banding DATA
	#---------
	x=unique(as.character(b$Species[ind_tag]))
	bu$species[i]=x
	bu$more.than1.species=length(x)
	# check if there is 2 different band number for same tag
	if (length(x)>1)
	{
	print('--------------------------')
	print(paste(bu$tag[i],' several SPECIES for same tag ind',i,sep=":"))
	print('--------------------------')
	print(b[ind_tag,])
	}
	# Sex: take the most represented
	#---------
	x=unique(as.character(b$Sex[ind_tag]))	
	# check if there is 2 different band number for same tag
	if (length(x)==1)
	{bu$sex[i]=x} else 
	{
	print('--------------------------')
	print(paste(bu$tag[i],' several SEX for same tag ind',i,sep=":"))
	print('--------------------------')
	print(b[ind_tag,])
	# number of M
	m=sum(b$Sex[ind_tag]=="M"|b$Sex[ind_tag]=="M?",na.rm ="T")
	# number of F
	f=sum(b$Sex[ind_tag]=="F"|b$Sex[ind_tag]=="F?",na.rm ="T")
	# take the most between both
	if (m>f) {
		bu$sex[i]="M"
		print('More M')
		} 
		else if (f>m) {
			bu$sex[i]="F"
			print('More F')
			}
	#
	}
	
	# Age: take last 
	#---------
	correctAgeInd=which(b$Age=="1a"|b$Age=="+1a") # not necessary when data cleaned
	ind_tag_c=intersect(ind_tag, correctAgeInd)
	x=unique(as.character(b$Age[ind_tag_c]))	
	# check if there is 2 different band number for same tag
	if (length(x)==1)
	{bu$age[i]=x} else if (length(x)>1)
	{
	print('--------------------------')
	print(paste(bu$tag[i],' several AGE for same tag ind',i,sep=":"))
	print('--------------------------')
	print(b[ind_tag,])
	# Take last
	bu$age[i]=tail(as.character(b$Age[ind_tag_c]),n=1)
	print(paste('Take Last',tail(b$Age[ind_tag],n=1),sep=":"))
	
	}	
	
	# Wing: take last, check differences
	#---------
	x=unique(as.character(b$Wing.chord.num[ind_tag]))
	#take last one
	bu$color[i]=toupper(x[length(x)])
	
	# Tarsus: take last, check differences
	#---------
	
	# 1st and Last capture
	#--------------------
}
sink()

# nb captures
#------------
br=data.frame(table(b$BandNumber))
names(br)=c('bandNumber','nbCapt')

# merge (becarefull loose individual with not band)
#ball <- merge(bu,br,by="bandNumber", all.x=T)
ball=bu
# Save
#-----
write.table(ball,paste(out,'summary_tag_banding.csv',sep=""))