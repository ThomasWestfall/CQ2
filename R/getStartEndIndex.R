getStartEndIndex <- function(data_index, minimum_period){

  ind.d <- data_index[2:NROW(data_index)] - data_index[1:(NROW(data_index)-1)]
  if(data_index[1] == TRUE){
    ind.start <- c(checkmate::wf(data_index,TRUE), (which(ind.d == 1)+1))
  }else{
    ind.start <- which(ind.d == 1)+1
  }
  if(data_index[NROW(data_index)] == TRUE){
    ind.end <- c(which(ind.d == -1), checkmate::wl(data_index, TRUE)) # + end
  }else{
    ind.end <- which(ind.d == -1)
  }
  delta <- cbind(ind.start,ind.end)

  #re-compute index, if observations are less than minimum_period
  if(any(delta[,2] - delta[,1] < minimum_period)){
    delta <- delta[-which(delta[,2] - delta[,1] < minimum_period),]
  }

  return(delta)
}

