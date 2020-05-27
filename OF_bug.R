# Analyse OF bug
# mcauchoixxx@gmail.com, 13/03/19
#--------------------------------------------------------
#to do
#-------

#--------------------------------------------------------
rm(list = ls())
# library
#--------
library("ggplot2")
library('rJava')
library('xlsxjars')
library('xlsx')
library("gridExtra")# export table
#devtools::install_github("mastoffel/rptR", build_vignettes = F)
library("rptR")# repeatability computation
library("data.table")
library("barcode")
library("lme4")
#library("lmtest") # 
library("lmerTest") # 
# function
#---------
# To compute learning curve
source('~/IAST Dropbox/maxime cauchoix/wild_cog_OF/Rscripts/slideFunct.R')
source('~/IAST Dropbox/maxime cauchoix/wild_cog_OF/Rscripts/OF_mergeLogFiles.R')
#files
#---------
path='/Users/maximecauchoix/IAST Dropbox/maxime cauchoix/wild_cog_OF/'

# IN
#data prob
p=read.xlsx('/Users/maximecauchoix/IAST Dropbox/maxime cauchoix/wild_cog_OF/logistique/OF data problem.xlsx',sheetName = 1,header = T)

# nb probl??mes
#-------------
print(paste("nb problem",dim(p)[1]))
print(paste("% problem",round(100*dim(p)[1]/(13*3*4*4))))
table(p$probl.c3..a8.me)
print(paste("% problem sans graine",round(100*(dim(p)[1]-sum(p$probl.c3..a8.me=='graines'))/(13*3*4*4))))
# prob graines
(sum(p$probl.c3..a8.me=='graines')/dim(p)[1])*100
