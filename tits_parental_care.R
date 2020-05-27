# Analyse nest feeding video extracted from baba

Input="/Users/maximecauchoix/Dropbox/spring/Raw Data/Feeding Video/baba/"
file="feeder_annotation_summary.csv"

d=read.csv2(paste0(Input,file),sep=c(",",'|'),header = F)
names(d)<-c("video","start","end","label")
length(unique(d$video))

# recode variable
d$human=1
d$human[d$label=="bird_spotted"]=0
length(unique(d$video[d$human==1]))

# extract 
