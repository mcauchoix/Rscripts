# Read OpenFeeder logfiles and merge with subfolder name
# mcauchoixxx@gmail.com, 1/12/17
#--------------------------------------------------------
#to do
# remove XXXXXXX and ???????
#--------------------------------------------------------


# libraries
#install.packages('data.table',type="mac.binary.mavericks")
library('data.table')

# variables
path='/Users/maximecauchoix/Dropbox/wild_cog_2017-18/data/'
out='/Users/maximecauchoix/Dropbox/wild_cog_2017-18/results/'

# list all data files
allLogFiles=list.files(path,all.files=FALSE,pattern="201*",recursive=TRUE)# must be from 2017
allLogFiles=allLogFiles[c(grep("*gonogo*",allLogFiles))]#,grep("*eight*",allLogFiles))]#,grep("*eight*",allLogFiles)
# put data in a list and fields
dataList <- list()
badFile=0
badFileName=c()
obsNb=0
for (i in 1:length(allLogFiles)) {
	# read each file
	dt=read.csv(paste(path,allLogFiles[i], sep = ""))
	if (dim(dt)[1]>0&dim(dt)[2]==13){
	print(allLogFiles[i])
	# add row name
			names(dt)=c("day","hour","site","OF","scenario","tag","denied","reward","red","green","blue","door_status_at_arrival","visit_duration")
	# get delimitaion of subfolders
	pos = unlist(gregexpr('/', allLogFiles[i]))
	# add OF
	dt$site_folder=substr(allLogFiles[i], 1, 2)
	# add task
	dt$task=substr(allLogFiles[i], pos[1]+1, pos[2]-1)
	# add file
	dt$file=basename(allLogFiles[i])
	# add path
	dt$path=allLogFiles[i]	
	# store in a list
	dataList[[i]]=dt
	# check
	obsNb= obsNb+dim(dt)[1]
	}
	else 
	{
badFile=badFile+1
badFileName[badFile]=allLogFiles[i]
	}
	
}
# print
print(paste(badFile,'corrupted file'))

# merge
d=rbindlist(dataList)

#save
write.table(d,paste(out,"alldata.csv",sep=""))



#erros
sink(paste(out,"corrupted_data.txt"))# store erros
print(badFileName)
sink()







#INTERESTING
# merge two data frames by ID
#total <- merge(data frameA,data frameB,by="ID")
# merge two data frames by ID and Country
#total <- merge(data frameA,data frameB,by=c("ID","Country"))
