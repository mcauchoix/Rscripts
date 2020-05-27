# evaluate learning with models and estimation plots

# load
di=read.table(paste(out_f,"alldata.csv",sep=""),h=T)

#------------------
# Models
#-----------------
# Trial number
#-------------
# mtrial=glmer(door.open~scale(trialNumber)*scale(scenario)*elevation*species+(1|tag),data=d,subset =d$scenario>29&d$trialNumber<100,family = binomial(link = "logit"))#(1|site_folder)+(1|site_folder:tag)
# summary(mtrial)
# 
# coeffs <- coef(summary(mtrial)) # get estimates, etc...
# # Export
# pdf(paste0(out_f,"Table Single trial learning test.pdf"), height=5, width=10)
# grid.table(round(coeffs,3))
# dev.off()

#------------------
# Estimation plot
#-----------------
a=data.frame(learn=c(rep("First",dim(di)[1]),rep("Last",dim(di)[1])),acc30=c(di$AccSt30,di$AccFi30),acc100=c(di$AccSt100,di$AccFi100),accDay=c(di$AccIni,di$AccFin),scenario=c(di$scenario,di$scenario),
             ID=c(di$tag,di$tag),species=c(di$species,di$species))


# Scenario 30
estim<- dabest(
  a[a$scenario==30&!is.na(a$acc100),],learn,acc100,idx=c("First","Last"),
  paired = TRUE,id.column = ID
)
pdf(paste0(out_f,"Estimation_learning_Accuracy 100 trials_Scenario30.pdf"), height=4, width=4)
plot(estim,rawplot.ylabel="Accuracy")
dev.off()

# Scenario 31
estim<- dabest(
  a[a$scenario==31&!is.na(a$acc100),],learn,acc100,idx=c("First","Last"),
  paired = TRUE,id.column = ID
)
pdf(paste0(out_f,"Estimation_learning_Accuracy 100 trials_Scenario31.pdf"), height=4, width=4)
plot(estim,rawplot.ylabel="Accuracy")
dev.off()

# Scenario 32
estim<- dabest(
  a[a$scenario==32&!is.na(a$acc100),],learn,acc100,idx=c("First","Last"),
  paired = TRUE,id.column = ID
)
pdf(paste0(out_f,"Estimation_learning_Accuracy 100 trials_Scenario32.pdf"), height=4, width=4)
plot(estim,rawplot.ylabel="Accuracy")
dev.off()




# Difference by species
estim<- dabest(
  di[di$scenario==30&!is.na(di$AccFi100),],species,AccFi100,idx=c("Great","Blue","Marsh"),
  paired = FALSE
)
pdf(paste0(out_f,"Estimation_learning_Accuracy 100 trials_Scenario30_Species effect.pdf"), height=4, width=8)
plot(estim,rawplot.ylabel="Accuracy")
dev.off()

# Difference by elevation
unpaired_median_diff      <- dabest(
  di[di$scenario==30&di$species=="Great"&!is.na(di$AccFi100),],elevation,AccFi100,idx=c("high","low"),
  paired = FALSE
)
plot(unpaired_median_diff)


# Nb of individuals reaching criterium
#---------------------------------
dlearn=di[!is.na(di$TTC),]
table(dlearn$species,dlearn$scenario)
#sum(!is.na(di$TTC[di$scenario==30&d$species]))
