.DF.N.get <- function(data, model_list){   
  temp <- vapply(model_list,length, 0)
  
  DF <-sum(temp)-length(temp)
  N <- rep(NA,length(temp))
  
  for (i in 1:length(temp)){
    if (i==1) N[1] <- sum(data[1:temp[1]])
    else N[i] <- sum(data[(sum(temp[1:(i-1)])+1):sum(temp[1:i])])
  }
  return(list(DF,N))
}

sat_model <- function(model_list, data){
  temp.branch <- sapply(model_list,length)
  NNN <- rep(.DF.N.get(data,model_list)[[2]], temp.branch)
  temp <- data * log(data/NNN)
  temp[data == 0] <- 0
  llk <- sum(temp)
  return(-llk)
}