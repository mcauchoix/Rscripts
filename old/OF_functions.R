# all fonction for Openfeeder data analysis
# mcauchoixxx@gmail.com, 5/12/17
#--------------------------------------------------------

#----------------------------
# check data
#----------------------------
# input: data frame, merged OF data (OF_merged_files)
# output: data frame (tag,nb visit not rewarded, nb visit rewarded)

OF_check_data <- function(d){
#nb trial rewarded by bird
r=table(d$tag,d$reward)
# histograme of % of not rewarded trial
hist(r[,1]/(r[,1]+r[,2])*100)
return(r)
}




#----------------------------
# match with banding
#----------------------------

OF_match_banding <- function(d){
#nb vist not read 
sum(d$tag=='??????????'&d$reward==1)

#  

return(object)
}