# to do
#-----
# correlation nb observation and degree

# packages
#---------
library(chron) #deal with temporal data
library(asnipe) # gmm to group individuals and make adgency matrix
library(sna) # social network  metrics
library(igraph)# graphical network

#load data
#---------
data=d[d$scenario<3,]


#Transfrom temporal data as needed for asnipe
#---------------------------------------------

options(chron.origin = c(month=09, day=1, year=2018))

#time in second by day
data$seconds<-as.numeric(substr(data$hour,1,2))*3600+as.numeric(substr(data$hour,4,5))*60+as.numeric(substr(data$hour,7,8))


#julian day
#data$julian<-julian(as.numeric(substr(data$day,1,2)),as.numeric(substr(data$day,4,5)),as.numeric(paste("20",substr(data$day,7,8),sep="")),origin.=)

# do loop over site here
uSite=unique(data$site_folder)

adjMatrixlist = list()
nodeList=list()
gbiList=list()
eventsList=list()
gbiNbvisitList=list()
for (s in 1:length(uSite)){
  # Gaussian Mixture Model
  #-----------------------
  #subset by site, scenario and date
  print('-----------------------------------------------------')
  print(uSite[s])
  print('-----------------------------------------------------')
  dataS=data[data$site_folder==uSite[s],]
  #dataS=dataS[dataS$dayDate>max(unique(dataS$dayDate))-1,]# take only the X last days
  
  # Get event of foraging and indivuals present
  gmm_data<-gmmevents(time=dataS$seconds,
                      identity=dataS$tag,
                      location=dataS$day)
  # store row data
  gbiList[[s]]=gmm_data$gbi
  eventsList[[s]]=gmm_data$metadata
  gbiNbvisitList[[s]]=gmm_data$B
  
  #events <- gmm_data$metadata
  #observations_per_event <- gmm_data$B
  
  # adjacency matrix
  #-----------------
  adjMatrix<-get_network(gmm_data$gbi, data_format = "GBI",association_index = "SRI")
  adjMatrixlist[[s]]<-adjMatrix
  
  # node and network metrics
  species<-as.character(data$species[match(row.names(adjMatrix),data$tag)])
  nodeMetrics<-data.frame(site=uSite[s],ID=row.names(adjMatrix),species=species,
                          degree=sna::degree(adjMatrix,gmode="graph"),
                          degree_unweighted=sna::degree(adjMatrix,gmode="graph",ignore.eval = TRUE),
                          betweenness=sna::betweenness(adjMatrix,gmode="graph"),
                          closeness=sna::closeness(adjMatrix,gmode="graph"),
                          eigenvector=sna::evcent(adjMatrix,gmode="graph"),
                          density_weighed=gden(adjMatrix,ignore.eval=F),
                          density_unweighed=gden(adjMatrix,ignore.eval=T))
  
  # group size
  gs=rowSums(gmm_data$gbi)
  for (id in 1:dim(nodeMetrics)[1]){
    nodeMetrics$gs[id]=mean(gs[gmm_data$gbi[,id]==1])
  }
  # nb days
  nodeMetrics$nbday=length(unique(dataS$day))
  
  # store
  nodeList[[s]]=nodeMetrics
  
  # Stat test of non random association
  #-------------------------------------
  #coef of variation
  coefVar<-sd(adjMatrix)/mean(adjMatrix)
  permut<-1000
  #performing the permutation
  network_perm<-network_permutation(association_data=gmm_data$gbi,
                                    data_format="GBI",
                                    permutations=permut,
                                    association_index="SRI",
                                    association_matrix=adjMatrix,
                                    days=gmm_data$metadata$Location,
                                    within_day=T)
  # compute permutted coef of variation
  coefVarPerm<-numeric(0)
  for(i in 1:dim(network_perm)[1]){
    coefVarPerm[i]<-sd(network_perm[i,,])/mean(network_perm[i,,])
  }
  #plot the distribution of permutations with the original data overlaid
  pdf(paste0(out_net,uSite[s],"NonRandom association.pdf"), height=10, width=10)
  hist(coefVarPerm,breaks=100,main=paste("P= ",sum(coefVar<coefVarPerm)/length(coefVarPerm)),xlab="Coefficient of Variation",ylab="Probability")
  abline(v=mean(coefVar),col='red')
  dev.off()
  
  # draw network
  #-------------
  #fonction pour la couleur
  bird.col<-function(species){
    for(i in 1:length(species)){
      if(species[i]=="Great"){
        species[i]<-"yellow"
      }else if(species[i]=="Blue"){
        species[i]<-"blue"
      }else if(species[i]=="Marshtit"){
        species[i]<-"chocolate4"
      }else if (species[i]=="Coal"){
        species[i]<-"black"
      }else if (species[i]=="Crested"){
        species[i]<-"grey"
      }else(species[i]<-"pink")
    }
    return(species)
  }
  
  colors<-bird.col(as.character(nodeMetrics$species))
  
  #Avec igraph
  g<-graph.adjacency(adjMatrix,mode="undirected",weighted=T,diag=F)
  
  threshold<-0.1
  E(g)[weight<threshold]$lco<-"grey"
  E(g)[weight>=threshold]$lco<-"black"
  E(g)[weight<threshold]$lty<-0
  E(g)[weight>=threshold]$lty<-1
  
  pdf(paste0(out_net,uSite[s],"network.pdf"), height=10, width=10)
  par(mai=c(0,0,0,0))
  lay<-layout.fruchterman.reingold.grid(g)
  plot.igraph(g,layout=lay,vertex.color=colors,vertex.frame.color=NA,vertex.label.cex=0.8,vertex.label.color="green",vertex.label=substr(nodeMetrics$ID,7,10),edge.lty=as.numeric(!E(g)$lty),edge.width=E(g)$weight*10,edge.color=E(g)$lco)
  plot.igraph(g,layout=lay,vertex.color=colors,vertex.frame.color=NA,vertex.label.cex=0.8,vertex.label.color="green",vertex.label=substr(nodeMetrics$ID,7,10),edge.lty=E(g)$lty,edge.width=E(g)$weight*10,edge.color=E(g)$lco,add=T)
  dev.off()
  
}# loop over sites


# tkplot(g,vertex.color=colors,edge.width=E(g)$weight*10,edge.lty=E(g)$lty)
# coords<-tkplot.getcoords(4)

# #correlation avec le nombre d'observations
# seuil<-0
# par(mar=c(5, 4, 2, 2) + 0.1)
# plot(indSummary$totalCount[indSummary$totalCount>seuil],indSummary$degree[indSummary$totalCount>seuil],ylim=c(0,8),xlab="Nombre d'apparition",ylab="Degree centrality")
# cor.test(indSummary$totalCount[indSummary$totalCount>seuil],indSummary$degree[indSummary$totalCount>seuil])



# # if 
# for (i in 1:5){
# adjMatrix=adjMatrixlist[[s]]
# 
# species<-as.character(data$species[match(row.names(adjMatrix),data$tag)])
# nodeMetrics<-data.frame(site=uSite[s],ID=row.names(adjMatrix),species=species,degree=sna::degree(adjMatrix,gmode="graph"),betweenness=sna::betweenness(adjMatrix,gmode="graph"),closeness=sna::closeness(adjMatrix,gmode="graph"),eigenvector=sna::evcent(adjMatrix,gmode="graph"),density_weighed=gden(adjMatrix,ignore.eval=F),density_unweighed=gden(adjMatrix,ignore.eval=T))
# nodeMetricslist[[s]]<-nodeMetrics
# }
# 
# big_data = do.call(rbind,nodeMetricslist)


# save data
#----------
#adjencyMatrix
names(adjMatrixlist)<-uSite
save(adjMatrixlist,file=paste0(out_local,"Network_adjencyMatrix.Rdata"))
# nodMertics
big_data=rbindlist(nodeList)
write.table(big_data,paste0(out_local,"Network_nodeMetrics.csv"))
# row event data
save(gbiList,file=paste0(out_local,"Network_gbi.Rdata"))
save(gbiNbvisitList,file=paste0(out_local,"Network_gbiNbvisit.Rdata"))


events=rbindlist(eventsList)
write.table(events,paste0(out_local,"Network_events.csv"))


# recompute individual metrics
#-------------------------------
adjMatrixlist = list()
nodeList=list()


data=d[d$scenario<3,]
uSite=unique(data$site_folder)

load(paste0(out_local,"Network_gbi.Rdata"))
for (s in 1:length(gbiList))
{
  # group by individual matrix
  gbi=gbiList[[s]]
  # remove individual with not enough visit
  nbVisit=colSums(gbi)
  gbi=gbi[,nbVisit>100]
  nbVisit=colSums(gbi)
  # adjacency matrix
  #-----------------
  adjMatrix<-get_network(gbi, data_format = "GBI",association_index = "SRI")
  #adjMatrixlist[[s]]<-adjMatrix
  
  # node and network metrics
  species<-as.character(data$species[match(row.names(adjMatrix),data$tag)])
  nodeMetrics<-data.frame(site=uSite[s],ID=row.names(adjMatrix),species=species,
                          degree=sna::degree(adjMatrix,gmode="graph"),
                          degree_unweighted=sna::degree(adjMatrix,gmode="graph",ignore.eval = TRUE),
                          betweenness=sna::betweenness(adjMatrix,gmode="graph"),
                          closeness=sna::closeness(adjMatrix,gmode="graph"),
                          eigenvector=sna::evcent(adjMatrix,gmode="graph"),
                          density_weighed=gden(adjMatrix,ignore.eval=F),
                          density_unweighed=gden(adjMatrix,ignore.eval=T))
  
  # group size
  gs=rowSums(gbi)
  for (id in 1:dim(nodeMetrics)[1]){
    nodeMetrics$gs[id]=mean(gs[gbi[,id]==1])
  }
  # nb days
  #nodeMetrics$nbday=length(unique(data$day))
  # store
  nodeList[[s]]=nodeMetrics
  
  
  # draw network
  #-------------
  #fonction pour la couleur
  bird.col<-function(species){
    for(i in 1:length(species)){
      if(species[i]=="Great"){
        species[i]<-"yellow"
      }else if(species[i]=="Blue"){
        species[i]<-"blue"
      }else if(species[i]=="Marshtit"){
        species[i]<-"chocolate4"
      }else if (species[i]=="Coal"){
        species[i]<-"black"
      }else if (species[i]=="Crested"){
        species[i]<-"grey"
      }else(species[i]<-"pink")
    }
    return(species)
  }
  
  colors<-bird.col(as.character(nodeMetrics$species))
  
  #Avec igraph
  g<-graph.adjacency(adjMatrix,mode="undirected",weighted=T,diag=F)
  
  threshold<-0.1
  E(g)[weight<threshold]$lco<-"grey"
  E(g)[weight>=threshold]$lco<-"black"
  E(g)[weight<threshold]$lty<-0
  E(g)[weight>=threshold]$lty<-1
  
  pdf(paste0(out_net,uSite[s],"network.pdf"), height=10, width=10)
  par(mai=c(0,0,0,0))
  lay<-layout_with_fr(g)
  plot.igraph(g,layout=lay,vertex.color=colors,vertex.frame.color=NA,vertex.label.cex=0.8,vertex.label.color="green",vertex.label=substr(nodeMetrics$ID,7,10),edge.lty=as.numeric(!E(g)$lty),edge.width=E(g)$weight*10,edge.color=E(g)$lco)
  plot.igraph(g,layout=lay,vertex.color=colors,vertex.frame.color=NA,vertex.label.cex=0.8,vertex.label.color="green",vertex.label=substr(nodeMetrics$ID,7,10),edge.lty=E(g)$lty,edge.width=E(g)$weight*10,edge.color=E(g)$lco,add=T)
  dev.off()
  
  
  pdf(paste0(out_net,uSite[s],"nbvisit vs degree unwaited.pdf"), height=10, width=10)
  
  plot(nbVisit,nodeMetrics$degree_unweighted,xlab="Nombre d'apparition",ylab="Degree centrality")

  dev.off()
}

# nodMertics
big_data=rbindlist(nodeList)
write.table(big_data,paste0(out_local,"Network_nodeMetrics.csv"))