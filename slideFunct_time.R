# compute sliding average and return time and average
slideFunct_time <- function(data, window, step){
  total <- dim(data)[1]
  spots <- seq(from=1, to=(total-window), by=step)
  result <- vector(length = length(spots))
  for(i in 1:length(spots)){
    result[i] <- mean(data[spots[i]:(spots[i]+window-1),2])
  }
  res=data.frame(time=data[window:(dim(data)[1]-1),1],intensity=result)
  return(res)
}