# 
# load
#-------
co=read.csv2(paste(out_local,"all_ind_stats_afterCorelation.csv",sep=""),h=T)


# bird recorded at feeder at least once
coR=co[!duplicated(co$bandNumber),]

# bird with enough cog data
coco=co[co$nbts>200&co$scenario.x>2,]
coco=coco[!duplicated(coco$bandNumber),]