#make list for go no go One (1/4 of bird rewarded on each LED )
#MCauchoix Fall 2018############################################
# go no go ind
goI=dall$scenario=="30"
# high
highI=dall$site_folder %in% c('C4','H1')
highTag=unique(d$tag[which(highI&goI)])
#low
lowI=dall$site_folder=='C1'
lowTag=unique(d$tag[which(lowI&goI)])
#both
bothTag=intersect(highTag,lowTag)

#list high
#-----------
df_high=data.frame(highTag)
names(df_high)='tag'
d_high=merge(df_high,ball,by="tag", all.x=T,sort=F)#all.x=T
d_high$species[is.na(d_high$species)]="unknown"
u_spe=unique(d_high$species)
high1=as.character(c())
high2=as.character(c())
high3=as.character(c())
high4=as.character(c())
for (i in 1:length(u_spe))# put same number of each spe
{
  speI=which(d_high$species==u_spe[i])# all bird from 1 species
  print(paste("High", length(speI),u_spe[i]))#print
  nb=round(length(speI)/4)# divied in 4
  modulo=length(speI)%%4# not used but should be to avoid putting too much in list 4
  if (length(speI)>3){
    high1=c(high1,as.character(d_high$tag[speI[1:nb]])) 
    high2=c(high2,as.character(d_high$tag[speI[(nb+1):(2*nb)]])) 
    high3=c(high3,as.character(d_high$tag[speI[(2*nb+1):(3*nb)]])) 
    high4=c(high4,as.character(d_high$tag[speI[(3*nb+1):length(speI)]])) 
  }
  else if (length(speI)==3)
  {
    high1=c(high1,as.character(d_high$tag[speI[1]])) 
    high2=c(high2,as.character(d_high$tag[speI[2]])) 
    high3=c(high3,as.character(d_high$tag[speI[3]])) 
  }
  else if (length(speI)==2)
  {
    high1=c(high1,as.character(d_high$tag[speI[1]])) 
    high2=c(high2,as.character(d_high$tag[speI[2]])) 
  } 
  else if (length(speI)==1)
  {
    high1=c(high1,as.character(d_high$tag[speI[1]])) 
  } 
  
}
sink()
# ad tags to checks
high1=c(high1,"01101708BB")
high2=c(high2,"0110179E77")
high3=c(high3,"0700EE1A32")
high4=c(high4,"0700EE4029")
# print
outHigh='~/Dropbox/wild_cog_2017-18/config/3_gonogo_one/C4_H1/'
for (i in 1:4){
  sink(paste0(outHigh,paste0('PTONE',i,'.txt')))
  cat(as.character(eval(parse(text=paste0('high',i)))),sep="")
  sink()
}

#list low
#-----------
df_low=data.frame(lowTag)
names(df_low)='tag'
d_low=merge(df_low,ball,by="tag", all.x=T,sort=F)#all.x=T
d_low$species[is.na(d_low$species)]="unknown"
u_spe=unique(d_low$species)
low1=as.character(c())
low2=as.character(c())
low3=as.character(c())
low4=as.character(c())
for (i in 1:length(u_spe))# put same number of each spe
{
  speI=which(d_low$species==u_spe[i])# all bird from 1 species
  print(paste("low", length(speI),u_spe[i]))#print
  nb=round(length(speI)/4)# divied in 4
  modulo=length(speI)%%4# not used but should be to avoid putting too much in list 4
  if (length(speI)>3){
    low1=c(low1,as.character(d_low$tag[speI[1:nb]])) 
    low2=c(low2,as.character(d_low$tag[speI[(nb+1):(2*nb)]])) 
    low3=c(low3,as.character(d_low$tag[speI[(2*nb+1):(3*nb)]])) 
    low4=c(low4,as.character(d_low$tag[speI[(3*nb+1):length(speI)]])) 
  }
  else if (length(speI)==3)
  {
    low1=c(low1,as.character(d_low$tag[speI[1]])) 
    low2=c(low2,as.character(d_low$tag[speI[2]])) 
    low3=c(low3,as.character(d_low$tag[speI[3]])) 
  }
  else if (length(speI)==2)
  {
    low1=c(low1,as.character(d_low$tag[speI[1]])) 
    low2=c(low2,as.character(d_low$tag[speI[2]])) 
  } 
  else if (length(speI)==1)
  {
    low1=c(low1,as.character(d_low$tag[speI[1]])) 
  } 
  
}
# ad tags to checks
low1=c(low1,"01101708BB")
low2=c(low2,"0110179E77")
low3=c(low3,"0700EE1A32")
low4=c(low4,"0700EE4029")
# print
outlow='~/Dropbox/wild_cog_2017-18/config/3_gonogo_one/C1/'
for (i in 1:4){
  sink(paste0(outlow,paste0('PTONE',i,'.txt')))
  cat(as.character(eval(parse(text=paste0('low',i)))),sep="")
  sink()
}



#to check
#--------
# df_high=data.frame(HIGH1)
# names(df_high)='tag'
# d_high=merge(df_high,ball,by="tag", all.x=T,sort=F)#all.x=T
length(high1)
length(high2)
length(high3)
length(high4)
# intersect(high1,high2)
# intersect(high1,high3)
# intersect(high1,high4)
# intersect(high2,high4)
# intersect(high2,high3)
# intersect(high3,high4)

length(high1)
length(high2)
length(high3)
length(high4)

length(low1)
length(low2)
length(low3)
length(low4)



