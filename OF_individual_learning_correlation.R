# Name, organise and look for correlation between cognitive measure
# 1/ Learning or nor
# 2/ Cognitive performance
#-------------------------
#specific librayr
library(reshape2)
library(corrplot)
# read data
#----------
tall=read.csv2('~/Documents/openfeeder/data/all_ind_stats_bart.csv',sep=";",h=T)
t=tall[tall$scenario.x>3,]

#----------------
# 1/ Learning or not : variables
#----------------
t$test_gam=t$gamPval<0.05&t$glmEstim>0
t$test_glm=t$glmPval<0.05&t$glmEstim>0

t$test_FvsL_30tr=t$firstVSlast_30trial<0.05&t$AccDi30>0
t$test_FvsL_50tr=t$firstVSlast_50trial<0.05&t$AccDi50>0
t$test_FvsL_100tr=t$firstVSlast_100trial<0.05&t$AccDi100>0
t$test_FvsL_day=t$firstVSlastDay<0.05&t$AccDif>0

t$test_chance_100tr=t$AboveChance100==TRUE
t$test_chance_lastDay=t$LastDayAboveChance=="AboveChance"

t$test_TTC10=!is.na(t$TTC10)
t$test_TTC20=!is.na(t$TTC20)
t$test_TTC30=!is.na(t$TTC30)
t$test_1dayAboveChance=!is.na(t$firstDayAboveChance)

t$test_mod_cst=t$reject_mod_cst==1
t$test_mod_exp=t$reject_mod_exp==1&t$reject_mod_cst==1

learningTest=names(t)[125:138]
#----------------
# Cog perf: variables
#----------------
bartV=names(t)[116:123]

cogPerf=c("nbts","AccTot","AccFi30","AccFi50","AccFi100",
          "AccDi30","AccDi50","AccDi100",
          "AccLastDay","firstDayAboveChance",
          "TTC10","TTC20","TTC30",
          "dayTC_10","dayTC_20","dayTC_30",
          "AccPostTTC10","AccPostTTC20","AccPostTTC30",bartV)

#----------------
# Learning or not : sample size
#----------------
#all
l_ss=matrix(nrow=2,ncol=length(learningTest))
for (i in 1:length(learningTest)){
  x=table(t[[learningTest[i]]])
  l_ss[,i]=x
}
l_ss=data.frame(l_ss)
names(l_ss)=learningTest
l_ss[3,]=round((l_ss[2,]/colSums(l_ss))*100)
row.names(l_ss)=c("No learning","learning","(%) learning ")

# sort by %
l_ss=l_ss[,order(l_ss[3,],decreasing = T)]

# store
pdf(paste0(out_ils,"Learning or not all.pdf"), height=2, width=20)
grid.table(l_ss)
dev.off()

# keep % 
all=l_ss[3,] 

#by scenario
n=2
for (j in 30:32){
  ts=t[t$scenario.x==j,]
  l_ss=matrix(nrow=2,ncol=length(learningTest))
  for (i in 1:length(learningTest)){
    x=table(ts[[learningTest[i]]])
    l_ss[,i]=x
  }
  l_ss=data.frame(l_ss)
  names(l_ss)=learningTest
  l_ss[3,]=round((l_ss[2,]/colSums(l_ss))*100)
  row.names(l_ss)=c("No learning","learning","(%) learning ")
  # sort by %
  l_ss=l_ss[,order(l_ss[3,],decreasing = T)]
  # write
  pdf(paste0(out_ils,"Learning or not ", j, ".pdf"), height=2, width=20)
  grid.table(l_ss)
  dev.off()
  # store %
  all[n,]=l_ss[3,] 
  n=n+1
}

# save
all=all[c(2:4,1),]
# sort by ON/OFF
all=all[,order(all[1,],decreasing = T)]

row.names(all)=c("ON/OFF","LEFT/RIGHT","RIGHT/LEFT","ALL")
pdf(paste0(out_ils,"Learning or not by senario.pdf"), height=2, width=20)
grid.table(all)
dev.off()




#----------------
# Learning or not : overlap
#----------------
overlapnb=matrix(nrow=length(learningTest),ncol=length(learningTest))
overlapPerc=matrix(nrow=length(learningTest),ncol=length(learningTest))

for (i in 1:length(learningTest)){
  for(j in 1:length(learningTest)){
    overlapnb[i,j]=sum(!is.na(intersect(t$code[t[[learningTest[i]]]],t$code[t[[learningTest[j]]]])))
    overlapPerc[i,j]=round(sum(!is.na(intersect(t$code[t[[learningTest[i]]]],t$code[t[[learningTest[j]]]])))/
                             (sum(!is.na(unique(c(as.character(t$code[t[[learningTest[i]]]]),
                                                  as.character(t$code[t[[learningTest[j]]]]))))))*100)
    
  }
}
#overlapPerc=overlapnb
# row and col names
rownames(overlapPerc)<-learningTest
colnames(overlapPerc)<-learningTest

# get upper part only
overlapPerc[lower.tri(overlapPerc)]<- NA

# remove diag
diag(overlapPerc)<- NA

melted_cormat <- melt(overlapPerc)
head(melted_cormat)

# plot overlap
heatmapOverlap=ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(na.value ="white",low = "white", high = "red",midpoint=30,
                       limit = c(0,max(overlapPerc,na.rm=T)), space = "Lab",
                       name="nb of bird\n learning\n overlap (%)") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")

pdf(paste(out_ils,'Overlap learning bird.pdf'))
print(heatmapOverlap)
dev.off()

#----------------
# 2/Cog perf: correlation
#----------------
cogPerfM=t[,cogPerf]
M<-cor(cogPerfM,use = "pairwise.complete.obs")
head(round(M,2))

#Compute p valud

mat <- as.matrix(cogPerfM)
n <- ncol(mat)
p.mat<- matrix(NA, n, n)
diag(p.mat) <- 0
for (i in 1:(n - 1)) {
  for (j in (i + 1):n) {
    # test for pairwise value
    x=rowSums(cbind(is.na(mat[, i]), is.na(mat[, j])))
    if (sum(x==0)>10){
      # test
      tmp <- cor.test(mat[, i], mat[, j],method = "spearman")
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
}
colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
p.mat

# # Matrice de p-value de la corr??lation
# p.mat <- cor.mtest(cogPerfM)
# head(p.mat[, 1:5])

pdf(paste(out_ils,'Correlation cog perf.pdf'),height = 15,width = 15)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", #order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corr??lation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativit??
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # Cacher les coefficients de corr??lation sur la diagonale
         diag=FALSE 
)
dev.off()

# write
write.csv2(t,paste(out_local,"all_ind_stats_afterCorelation.csv",sep=""),row.names = F)


