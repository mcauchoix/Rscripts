# summarise banding "b" by unique pitTag "Utag" save banding issue in working directory
# 
# mcauchoixxx@gmail.com, 12/12/17
#------------------------------------
# to do
#  for bird capture on nest add a variable with nest number and year
#-------
OF_summarise_banding<-function(Utag,b){

# libraries
#-----------
# read xlsx
# library('rJava')
# library('xlsxjars')
# library('xlsx')
# merge data frame
library('data.table')
library('dplyr')
# folders
#-------------
out="/Users/maximecauchoix/IAST Dropbox/maxime cauchoix/wild_cog_OF/banding/"

#-------------------------------------
# A- Merge and Clean banding (ADD NUMERIC VARIABLE)
#-------------------------------------
#source('~/IAST Dropbox/maxime cauchoix/wild_cog_OF/Rscripts/band_clean_2018_19.R')

#------------------------------------------
# 2- keep only 1 summary line by indidvidual 
#------------------------------------------
# remove entire row with NA
#b[rowSums(is.na(b))!=ncol(b), ]


bu= data.frame(Utag)
names(bu)='tag'



#order by date (assuming all dates are correct)
#-----------------------------
#b=b[order(b$dateCorrected),]

# Open error file
#----------------
sink(paste("banding_errors.txt"))# store erros
#---------------------------------------------------------------------
# add rest of banding variables of interest
#---------------------------------------------------------------------

for (i in 1: dim(bu)[1]) {
	# indeces of bu tag in all banding b
		ind_tag= which(as.character(b$RFID.)==as.character(bu$tag[i])) #'0700EE2478'#b[ind_tag,]

	# BandNumber: take last band with 7 digits
	#------------
	# should be 7
	ind_tag_c=intersect(ind_tag,which(nchar(b$BandNumber)==7))
	x=unique(as.character(b$BandNumber[ind_tag_c]))
	if (length(x)>0){
	# latest with 7
	latest_ind_tag=ind_tag_c[which(b$dateCorrected[ind_tag_c]==max(b$dateCorrected[ind_tag_c],na.rm=T))]
	bu$bandNumber[i]=b$BandNumber[latest_ind_tag[1]]
	}
	else
	{bu$bandNumber[i]='uncorrectband'}
	
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
	if (length(x)>0){
	# latest with 4
	  latest_ind_tag=ind_tag_c[which(b$dateCorrected[ind_tag_c]==max(b$dateCorrected[ind_tag_c],na.rm=T))]
	  bu$Color[i]=b$Color[latest_ind_tag[1]]
	}
	else
	{bu$Color[i]='uncorrectColor'}
	# check if there is 2 different color for same tag
	if (length(unique(b$Color[ind_tag]))>1)
	{
	  print('--------------------------')
	  print(paste(bu$tag[i],' several Color combo for same tag ind',i,sep=":"))
	  print('--------------------------')
	  print(b[ind_tag,])

	}
	
	# should be 'Great','Blue','Marsh','Crested','Coal','Nuthatch'
	ind_tag_c=intersect(ind_tag,which(b$Species %in% c('Great','Blue','Marsh','Crested','Coal','Nuthatch')))
	x=unique(as.character(b$Species[ind_tag_c]))
	# latest 
	if (length(x)>0){
	  latest_ind_tag=ind_tag_c[which(b$dateCorrected[ind_tag_c]==max(b$dateCorrected[ind_tag_c],na.rm=T))]
	  bu$species[i]=as.character(b$Species[latest_ind_tag[1]])
	}
	else
	{bu$species[i]=b$Species[ind_tag]}
	
	# check if there is 2 different Species for same tag
	if (length(unique(b$Species[ind_tag]))>1)
	{
	print('--------------------------')
	print(paste(bu$tag[i],' several SPECIES for same tag ind',i,sep=":"))
	print('--------------------------')
	print(b[ind_tag,])
	}
	# Sex: take the most represented
	#---------
	ind_tag_c=intersect(ind_tag,which(!is.na(b$Sex)))
	x=unique(as.character(b$Sex[ind_tag_c]))
	# check if there is 2 different band number for same tag
	if (length(x)==1)
	{bu$sex[i]=x}
	else if (length(x)==0){bu$sex[i]=NA}
	else
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
	# date first capture
	first_ind_tag=ind_tag[which(b$dateCorrected[ind_tag]==min(b$dateCorrected[ind_tag],na.rm=T))]
	
	if (b$dateCorrected[first_ind_tag[1]]<"2017-08-01") {bu$age[i]="+1a"}# if capture before august then adults
	else {# else take last age
	  ind_tag_c=intersect(ind_tag,which(!is.na(b$Age)))
	  x=unique(as.character(b$Age[ind_tag_c]))
	  if (length(x)>0){
	    # latest 
	    latest_ind_tag=ind_tag_c[which(b$dateCorrected[ind_tag_c]==max(b$dateCorrected[ind_tag_c],na.rm=T))]
	    bu$age[i]=as.character(b$Age[latest_ind_tag[1]])
	   # print(paste(as.character(b$Age[latest_ind_tag[1]]),i))
	  }
	  else
	  {	  bu$age[i]=NA}
	  
	}
	# Report if 2 different Age for same tag
	if (length(x)>1)
	{
	  print('--------------------------')
	  print(paste(bu$tag[i],' several AGE for same tag ind',i,sep=":"))
	  print('--------------------------')
	  print(b[ind_tag,])
	}	
	
	# Wing: take last, check differences
	#---------
	ind_tag_c=intersect(ind_tag,which(!is.na(b$Wing.chord.num)))#intersect(ind_tag,which(b$Wing.chord.num>50&b$Wing.chord.num<90))
	x=unique(as.character(b$Wing.chord.num[ind_tag_c]))
	if (length(x)>0){
	  # latest 
	  latest_ind_tag=ind_tag_c[which(b$dateCorrected[ind_tag_c]==max(b$dateCorrected[ind_tag_c],na.rm=T))]
	  bu$Wing.chord.num[i]=b$Wing.chord.num[latest_ind_tag[1]] 
	}
	else
	{	bu$Wing.chord.num[i]=NA}


	# Site: take last, check differences
	#---------
	ind_tag_c=ind_tag
	x=unique(as.character(b$Site[ind_tag_c]))
	# latest 
	latest_ind_tag=ind_tag_c[which(b$dateCorrected[ind_tag_c]==max(b$dateCorrected[ind_tag_c],na.rm=T))]
	bu$Site[i]=as.character(b$Site[latest_ind_tag[1]])
	# captureSite: take first
	#---------
	ind_tag_c=ind_tag
	x=unique(as.character(b$Site[ind_tag_c]))
	# latest 
	latest_ind_tag=ind_tag_c[which(b$dateCorrected[ind_tag_c]==min(b$dateCorrected[ind_tag_c],na.rm=T))]
	bu$captureSite[i]=as.character(b$Site[latest_ind_tag[1]])
	
	# # nest number
	# nestName=paste(unique(b$Nest.Number[ind_tag]))
	# if (!is.na(nestName)){
	# bu$nest[i]=as.character(nestName)
	# bu$nestingYear[i]=as.character(paste(unique(b$Year[ind_tag])))
	# }
	
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
# Save
#-----
write.table(bu,paste('summary_tag_banding.csv',sep=""),sep=";",row.names = F)
return(bu)
}