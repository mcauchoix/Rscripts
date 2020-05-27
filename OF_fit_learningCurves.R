# plot individuals learning curves by task and try to fit sigmoids
#--------------------------------------------------------------
# sliding window function
#-----------------------
source('~/Dropbox/wild_cog_OF/Rscripts/slideFunct_time.R')
# packages
#---------
library("sicegar")# fit sigmoid
library("cowplot")# for plots

# Sliding window
wind=30

# Select learning data (S:31,32)
d30=d[d$scenario>29,]

# Time
time=strptime(d30$fullTime,"%d/%m/%y %H:%M:%S")

# simulate sigmoidal data
time <- seq(3, 24, 0.5)

noise_parameter <- 0.1
intensity_noise <- runif(n = length(time), min = 0, max = 1) * noise_parameter
intensity <- sigmoidalFitFormula(time, maximum = 4, slope = 1, midPoint = 8)
intensity <- intensity + intensity_noise
dataInputSigmoidal <- data.frame(intensity = intensity, time = time)




# Loop over individuals [ADD DAY ON SLIDING]
#----------------------
uTag=unique(d30$tag)
uSc=unique(d30$scenario)


res=list()
n=1
for (i in 1:length(uTag)){# loop over individuals
  iInd=d30$tag==uTag[i]
  print(paste(uTag[i],i,length(uTag)))
  
  for (s in 1:length(uSc)){# loop over scenari
    iInds=iInd&d30$scenario==uSc[s]
    if (sum(iInds)>wind){
      name=paste0(uTag[i],'_',uSc[s])
      # compute sliding average
      #-----------------------
      # # temporal data
      # dat=data.frame(time=time[iInds]-min(time[iInds]),
      #                rep=d30$door.open[iInds])
      # trial data
      dat=data.frame(time=1:sum(iInds),
                     rep=d30$door.open[iInds])
      
      dataInput=slideFunct_time(dat,wind,1)
      
      # fit sigmoid
      #-----------------------
      # Generic function
      fitObj <- fitAndCategorize(dataInput = dataInput)#dataInputSigmoidal
      
      
      # By step
      #--------
      # normalizedInput <- normalizeData(dataInput = dataInput)
      # sigmoidalModel <- multipleFitFunction(dataInput = normalizedInput,
      #                                       model = "sigmoidal",
      #                                       n_runs_min = 20,
      #                                       n_runs_max = 500,
      #                                       showDetails = FALSE)
      # sigmoidalModel <- parameterCalculation(sigmoidalModel)
      # 
      
      if (fitObj$decisionProcess$decision %in% c("sigmoidal","ambiguous"))
      {      
        #Plot
        #----
        fig_a <-ggplot(dataInput, aes(time, intensity)) + geom_line() + theme_bw()
        fig_b <- figureModelCurves(dataInput = fitObj$normalizedInput,
                                   sigmoidalFitVector = fitObj$sigmoidalModel,
                                   showParameterRelatedLines = TRUE) 
        # save
        pdf(paste0(out_sig,name,".pdf"), height=4, width=8,onefile = FALSE)
        grid.arrange(fig_a, fig_b, ncol = 2)
        dev.off()
        
        #Parameters
        #----------
        decision=fitObj$decisionProcess$decision 
        R2=fitObj$sigmoidalModel$residual_Sum_of_Squares
        max=fitObj$sigmoidalModel$maximum_Estimate
        midpoint=fitObj$sigmoidalModel$midPoint_Estimate
        slope=fitObj$sigmoidalModel$slopeParam_Estimate
        
        
        
        
      } 
      else
      {
        fig_a <-ggplot(dataInput, aes(time, intensity)) + geom_line() + theme_bw()
        
        pdf(paste0(out_sig,name,".pdf"), height=4, width=4)
        grid.arrange(fig_a, ncol = 1)
        dev.off()
        
        #Parameters
        #----------
        decision=fitObj$decisionProcess$decision 
        R2=NA
        max=NA
        midpoint=NA
        slope=NA
      }
      
      # save parameters
      #-----------------------
      
      res[[n]]=data.frame(id=uTag[i],scenario=uSc[s],
                          decision=decision, 
                          R2=R2,
                          max=max,
                          midpoint=midpoint,
                          slope=slope)
      n=n+1
    }
    
  }
}
r=rbindlist(res)


# #####################################@
# ## individuals of interest for bart
# #----------------------------------
# #d=read.table('YOUR PATH/StudySite_2018_19.csv',h=T)
# # Select learning data (S:31,32)
# d30=d[d$scenario>29,]
# # sliding window
# wind=30
# # out folder to save figure
# outBart=paste0(out_f,'forBart')
# dir.create(outBart)
# 
# # individuals to select
# #Exp
# #011017A4E5_30
# #011016F56B_32
# #Sig
# #011016F56B_30
# #01101703AC_31
# #01101703AC_30
# # Sig start
# #0700EDB54C_30
# #01103F3A75_30
# # Constant
# #011016F46D_30
# # Linear increase
# #0700EDB54C_31
# indForBart=data.frame(id=c('011017A4E5','011016F56B','011016F56B','01101703AC','01101703AC',
#                            '0700EDB54C','01103F3A75','011016F46D','0700EDB54C'),
#                       scenario=c('30',"32",'30','31','30',
#                                  '30','30','30','31'))
# indForBart$id=as.character(indForBart$id)
# 
# res=list()
# data=list()
# n=1
# for (i in 1:dim(indForBart)[1]){# loop over individuals
#   iInds=d30$tag==indForBart$id[i]&d30$scenario==indForBart$scenario[i]
#  
#   name=paste0(indForBart$id[i],'_',indForBart$scenario[i])
#   print(name)
#       # compute sliding average
#       #-----------------------
#       dat=data.frame(time=1:sum(iInds),
#                      rep=d30$door.open[iInds])
#   dat$id=indForBart$id[i]
#        data[[i]]=dat
#       # dataInput=slideFunct_time(dat,wind,1)
#       # 
#       # # fit sigmoid
#       # #-----------------------
#       # # Generic function
#       # fitObj <- fitAndCategorize(dataInput = dataInput)#dataInputSigmoidal
#       # 
#       # if (fitObj$decisionProcess$decision %in% c("sigmoidal","ambiguous"))
#       # {      
#       #   #Plot
#       #   #----
#       #   fig_a <-ggplot(dataInput, aes(time, intensity)) + geom_line() + theme_bw()
#       #   fig_b <- figureModelCurves(dataInput = fitObj$normalizedInput,
#       #                              sigmoidalFitVector = fitObj$sigmoidalModel,
#       #                              showParameterRelatedLines = TRUE) 
#       #   # save
#       #   pdf(paste0(outBart,name,".pdf"), height=4, width=8,onefile = FALSE)
#       #   grid.arrange(fig_a, fig_b, ncol = 2)
#       #   dev.off()
#       #   
#       #   #Parameters
#       #   #----------
#       #   decision=fitObj$decisionProcess$decision 
#       #   R2=fitObj$sigmoidalModel$residual_Sum_of_Squares
#       #   max=fitObj$sigmoidalModel$maximum_Estimate
#       #   midpoint=fitObj$sigmoidalModel$midPoint_Estimate
#       #   slope=fitObj$sigmoidalModel$slopeParam_Estimate
#       #   
#       #   
#       #   
#       #   
#       # } 
#       # else
#       # {
#       #   fig_a <-ggplot(dataInput, aes(time, intensity)) + geom_line() + theme_bw()
#       #   
#       #   pdf(paste0(out_sig,name,".pdf"), height=4, width=4)
#       #   grid.arrange(fig_a, ncol = 1)
#       #   dev.off()
#       #   
#       #   #Parameters
#       #   #----------
#       #   decision=fitObj$decisionProcess$decision 
#       #   R2=NA
#       #   max=NA
#       #   midpoint=NA
#       #   slope=NA
#       # }
#       # 
#       # # save parameters
#       # #-----------------------
#       # 
#       # res[[i]]=data.frame(id=uTag[i],scenario=uSc[s],
#       #                     decision=decision, 
#       #                     R2=R2,
#       #                     max=max,
#       #                     midpoint=midpoint,
#       #                     slope=slope)
#       # 
#     
# }
# r=rbindlist(res)
# dataF=rbindlist(data)
# write.csv2(dataF,paste0(outBart,'data.csv'),row.names = F)
# data=read.csv2(paste0(outBart,'data.csv'),h=T)
# 
# #unique ID
# uID=unique(data$id)
# d1=data[data$id==uID[1],]
# 
# 
# save(data,file=paste0(outBart,'data.RDdata'))
# 
# load(paste0(outBart,'data.RDdata'))
