# compute total number of visit, daily average and max by species (with and without reversal learning data)
# work direcly on d [without 1O and GJ]

#-----------
# by species
#-----------
# Duration, start and end of the experiment 
print(paste("Nb days total all sites confonded",length(unique(d$day))))
print(paste("Start:", format(min(as.Date(d$day,"%d/%m/%y")),"%d-%b-%Y")))
print(paste("End:", format(max(as.Date(d$day,"%d/%m/%y")),"%d-%b-%Y")))



#-----------
# by species
#-----------

# nb ind
dunique=d[!duplicated(d$tag),]
tab=data.table(table(dunique$species))
names(tab)=c('species','nb.ind')
# nb trials total
temp=table(d$species)
tab$total.trial=temp

# mean nb trial by day
# nb of visit by day, tag, species and site by species
DT <- as.data.table(d)
DT=DT[,list(nbtrial = .N), by="tag,day,species"]
dv=as.data.frame(DT)
# mean visit by day 
DT <- as.data.table(dv)
DT=DT[,list(mean.visit.daily=round(mean(nbtrial))), by="species"]
dt=as.data.frame(DT)
tab=merge(tab,dt,by="species", all.x=F,sort=F)
# max nb trial by day
# mean visit by day 
DT <- as.data.table(dv)
DT=DT[,list(max.visit.daily=max(nbtrial)), by="species"]
dt=as.data.frame(DT)
tab=merge(tab,dt,by="species", all.x=F,sort=F)
print(tab)
# All
tab2=data.table(species=factor("All"),nb.ind=sum(tab$nb.ind),
                total.trial=sum(tab$total.trial),
                mean.visit.daily=mean(tab$mean.visit.daily),
                max.visit.daily=max(tab$max.visit.daily))
# merge
tab$total.trial=as.numeric(tab$total.trial)
tab=rbind(tab,tab2)
tab$mean.visit.daily=round(tab$mean.visit.daily)

pdf(paste0(out_f,"DescritptiveStats_by Species all.pdf"), height=2, width=6)
grid.table(tab,rows =c())
dev.off()

# without reversal
#-----------------
dall=d
d=d[d$scenario<32,]

# nb ind
dunique=d[!duplicated(d$tag),]
tab=data.table(table(dunique$species))
names(tab)=c('species','nb.ind')
# nb trials total
temp=table(d$species)
tab$total.trial=temp

# mean nb trial by day
# nb of visit by day, tag, species and site by species
DT <- as.data.table(d)
DT=DT[,list(nbtrial = .N), by="tag,day,species"]
dv=as.data.frame(DT)
# mean visit by day 
DT <- as.data.table(dv)
DT=DT[,list(mean.visit.daily=round(mean(nbtrial))), by="species"]
dt=as.data.frame(DT)
tab=merge(tab,dt,by="species", all.x=F,sort=F)
# max nb trial by day
# mean visit by day 
DT <- as.data.table(dv)
DT=DT[,list(max.visit.daily=max(nbtrial)), by="species"]
dt=as.data.frame(DT)
tab=merge(tab,dt,by="species", all.x=F,sort=F)
print(tab)
# All
tab2=data.table(species=factor("All"),nb.ind=sum(tab$nb.ind),
                total.trial=sum(tab$total.trial),
                mean.visit.daily=mean(tab$mean.visit.daily),
                max.visit.daily=max(tab$max.visit.daily))
# merge
tab$total.trial=as.numeric(tab$total.trial)
tab=rbind(tab,tab2)
tab$mean.visit.daily=round(tab$mean.visit.daily)

pdf(paste0(out_f,"DescritptiveStats_by Species no reversal.pdf"), height=2, width=6)
grid.table(tab,rows =c())
dev.off()


d=dall