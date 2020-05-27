# to check
#---------
# NO LAYDATE AND EGGNUMBER IN 2015
# NO NOT OCCUPIED NEST IN CASTERA IN 2018

# to do
#-------
# compile 2019

# load fitness
fi=read.csv2(paste0(out_local,"Nest_LHT_Data_AllYears_compilation.csv"),h=T,sep=";")

# new variables
#--------------
# nest with egg (!!!!! NO LAYDATE AND EGGNUMBER IN 2015, CHECK LAST FILE)
fi$eggORnot=0
fi$eggORnot[fi$LayDate!=""]=1
# hatch or not
fi$HatchOrNot=0
fi$HatchOrNot[fi$HatchDate!=""]=1
# site name
fi$Site[fi$Site=="Laboratoire"]="Labo"
fi$Site=factor(fi$Site)
# parent banding or not
fi$ParentBandOrNot=0
fi$ParentBandOrNot[fi$F_BandDate!=""|fi$M_BandDate!=""]=1
# number of chicks numeric
fi$HatchNb_num=fi$HatchNb
fi$HatchNb_num[fi$HatchNb %in% c("","?")]=NA
fi$HatchNb_num=as.numeric(as.character(fi$HatchNb_num))
mean(fi$HatchNb_num[fi$HatchNb_num>0],na.rm=T)

# number of attempt by nest
#---------------------------
x=data.table(table(fi$Year,fi$Nest))



# nb year occupied for each nest
#-------------------------------
DT <- as.data.table(fi)
DT=DT[,list(nb.year.occupied =sum(eggORnot),nb.year.present=.N), by="Nest"]
dd=as.data.frame(DT)
write.csv2(dd,paste0(out_local,"nestOccupied.csv"),row.names = F)

# nb of nest with parent banded each year
#------------------------
table(fi$ParentBandOrNot,fi$Year)

# nb nest occupied by year
#------------------------
table(fi$Year,fi$eggORnot)

# % of occupation by site
#------------------------
#all nest
n=table(fi$Site,fi$Year)
#  nest with egg
n_egg=table(fi$Site[fi$eggORnot==1],fi$Year[fi$eggORnot==1])
#  nest with chick hatching
n_chick_hatch=table(fi$Site[fi$HatchOrNot==1],fi$Year[fi$HatchOrNot==1])
#  nest with chick banded
n_chick_band=table(fi$Site[fi$Chick_Bands=="Yes"],fi$Year[fi$Chick_Bands=="Yes"])
#  nest with chick banded and parent banded
n_chick_band_parent_band=table(fi$Site[fi$Chick_Bands=="Yes"&fi$ParentBandOrNot==1],fi$Year[fi$Chick_Bands=="Yes"&fi$ParentBandOrNot==1])

#  nest with chick banded and parent NOT banded
n_chick_band_parentNOTband=table(fi$Site[fi$Chick_Bands=="Yes"&fi$ParentBandOrNot==0],fi$Year[fi$Chick_Bands=="Yes"&fi$ParentBandOrNot==0])


# % with chick
n_chick_band/n
# % with egg
n_egg/n[,c(1,2,4:6)]

# %with chick band compare to with egg
n_chick_band[,c(1,2,4:6)]/n_egg

# %with chick band compare to number of hatch
n_chick_band[,c(1,2,4:6)]/n_chick_hatch

# %with chick band compare to number of hatch
colSums(n_chick_hatch-n_chick_band[,c(1,2,4:6)])
colSums(n_egg)

# % nest with chicks banded
colSums(n_chick_band)[c(1,2,4:6)]/colSums(n_egg)


#-------------------------
# banding
#-------------------------
source('~/Dropbox/wild_cog_OF/Rscripts/band_clean_2018_19.R')
# nb ind by year
b$capt="Adult Nest"
b$capt[is.na(b$Nest.Number)]='Adult Missnet'
b$capt[b$Age=='PUL']='PUL'
#b$capt[b$Site!="M1"]=NA
#b$capt[!b$Species %in% c("Blue","Great")]=NA
table(b$Year,b$capt)
#code blood
b$Blood_correct="No"
b$Blood_correct[b$Blood %in% c("1","Large-immuNA & DNA","Metabollom","Metabolom","SMALL DNA","Small-DNA","Small-Dna","Telomere","small DNA","small dna","small-dna","Yes")]="Yes"
table(b$Year[b$capt!="PUL"],b$Blood_correct[b$capt!="PUL"])