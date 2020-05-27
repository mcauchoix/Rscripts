# Identify tag with no banding info (high number of visit)
# Mcauchoix Fall 2017####################################

v=data.frame(table(dall$tag,dall$site_folder))
names(v)=c("tag","site","nbvisitTotal")
# remove when not recorded at a site
v=v[v$nbvisitTotal>0,]
# present in banding
v$IsInBanding <- is.element(v$tag,ball$tag)
# not in banding
vNot=v[!v$IsInBanding&v$nbvisitTotal>2,]
print('-------Recorded on OF but not in banding-----------')
print(vNot)
print(dim(vNot))
print('---------------------------------------------------')


#write.table(vNot,"~/Dropbox/wild_cog_OF/banding/badPitTagInBandingFile.csv")