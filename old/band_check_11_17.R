# Check banding #created 11/10/17
#-----------------


# to read an xlsx file
#install.packages('xlsx')
#install.packages('rJava')
#install.packages('xlsxjars')

library('rJava')
library('xlsxjars')
library('xlsx')
library('gridExtra')


# read 
b=read.xlsx('/Users/maximecauchoix/IAST Dropbox/maxime cauchoix/Tits DATA/Banding/banding subfiles/Winter2018-2019.xlsx',sheetName = 1)
#out folder
out='/Users/maximecauchoix/IAST Dropbox/maxime cauchoix/wild_cog_OF/results/2018-2019/'


# indeces study sites
iss=is.element(b$Site,c('M1','L1','C1','C4','H1','GJ','BA'))&!is.element(b$Species,c('Nuthatch',"Coal","Crested"))
bs=b[iss,]# keep only study site bird
bs$Site=factor(bs$Site)# to update factor
bs$Species =factor(bs$Species)
# total capture and recapture
#---------------------------
#all
sum(iss)
# by species
print('Total number of capture')
All=table(bs$Species,bs$Site)
print(All)
filename=paste0(out,Sys.time(),'_Nb total capture.pdf')
pdf(filename, height=5, width=5)
grid.table(All)
dev.off()


# nb individual by site
#---------------------------
# remove duplicate
bu=bs[!duplicated(bs$BandNumber)&bs$RFID.!="None",]
# nb 
A=table(bu$Species,bu$Site)
print('Total number of bird')
print(A)
# export data
#-----------------------
filename=paste0(out,Sys.time(),'_Nb bird tagged.pdf')
pdf(filename, height=5, width=5)
grid.table(A)
dev.off()


# recapture rate by session