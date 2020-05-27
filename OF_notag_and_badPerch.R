#OF_notag_and_badperch: deal with XXXXXXXXXX: bird with no tag and ?????????: bad read (those close than 3s to the previous has been removed in OF_mergeLogFiles as they correspond to bird departure)
# count number of each by site, day and scenario
# mcauchoixxxx@gmail.Com  18 03 19
#-----------------------------------------
# no tag read
indNoTag=d$tag=="XXXXXXXXXX"
# count nb by:  scenario, day, site and OF 
DT <- as.data.table(d[indNoTag,])
DT=DT[,list(nb = .N), by="scenario,site_folder,OF,day"]
dv=as.data.frame(DT)
dv$dayDate=as.Date(dv$day,"%d/%m/%y")# convert date

m_no=lmer(log(nb)~scenario+OF+dayDate+(1|site_folder),data=dv)
summary(m_no)

# plot 
boxplot(nb~scenario,data=dv)
boxplot(nb~as.factor(OF),data=dv)


# bad read
indUnread=d$tag=="??????????"
# count nb by:  scenario, day, site and OF 
DT <- as.data.table(d[indUnread,])
DT=DT[,list(nb = .N), by="scenario,site_folder,OF,day"]
dv=as.data.frame(DT)
dv$dayDate=as.Date(dv$day,"%d/%m/%y")# convert date
m_br=glmer(nb~scenario+OF+dayDate+(1|site_folder),data=dv,family=poisson)
summary(m_br)

# plot 
boxplot(nb~scenario,data=dv)
boxplot(nb~as.factor(OF),data=dv)
