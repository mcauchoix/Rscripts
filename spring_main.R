# direct analysis of nestchecks
#--------------------------------
path_in="/Users/maximecauchoix/Dropbox/spring/Raw Data/Spring2020/CSV/"
list.files(path_in)
fileName="Balacet_Maxi_06052020.csv"

# read

d=read.csv2(paste0(path_in,fileName))
length(unique(d$NestNb))

table(d$Nest_state)
table(d$ChickAge[d$Species!="Marsh"])