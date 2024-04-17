GaussLiklihoodBV1 <- function(params, conc, Pred, conc.date){

  if(NCOL(params) >1){
    rho <- params[1,]/10
    sig <- params[2,]
  }else{
    rho <- params[1]/10
    sig <- params[2]
  }

  #re-compute conc.date, if observed days are less than p+1 (2), then remove
  if(any(conc.date[,2] - conc.date[,1] < 3)){
    conc.date <- conc.date[-which(conc.date[,2] - conc.date[,1] < 3),]
  }

  # get error vector for only finite values
  error <- log(conc) - log(Pred)

  ### Scenario 2 - varying mean bias
  # if we want mu_e to vary, assume error from first "missing" timesteps of continous period= mu_e
  # error[conc.date[2:NROW(conc.date),1]-1,] = matrix(mu_e, NROW(conc.date)-1, NCOL(Pred), byrow = TRUE)

  # initiate empty vectors for only finite values
  # inside <- matrix(NA, NROW(conc), NCOL(Pred))

  ### Scenario 1 - mean bias equals zero
  # assume mean error is constant at zero
  mu_e = rep(0, NCOL(Pred))


  # Calculate the correlation of the error from previous time step
  if (NCOL(params)>1) {
    # inside_list <- vector(mode = 'list', length = NROW(conc.date))
    inside_list <- sapply(1:NROW(conc.date), function(i) (sweep(error[(conc.date[i,1]+1):(conc.date[i,2]),], MARGIN = 2, mu_e,'-') - sweep(sweep(error[conc.date[i,1]:(conc.date[i,2]-1),], MARGIN = 2, mu_e, '-'),MARGIN = 2, rho,'*')))
    # inside[2:NROW(inside),] <- sweep(error[2:NROW(inside),],MARGIN = 2,mu_e,'-') - sweep(sweep(error[1:(NROW(inside)-1),],MARGIN = 2,mu_e,'-'),MARGIN = 2,rho,'*')
  }else{
    inside_list <- sapply(1:NROW(conc.date), function(i) error[(conc.date[i,1]+1):(conc.date[i,2])] - mu_e - rho*(error[conc.date[i,1]:(conc.date[i,2]-1)] - mu_e))
  }

  # get count of values in each slice, transpose to be in same rows as conc.date, and columns as pred/error
  if (NCOL(params)>1){
    ind_list <- t(sapply(1:length(inside_list), function(i) rep(NROW(inside_list[[i]]),NCOL(error))))
  }else{
    ind_list <- sapply(1:length(inside_list), function(i) rep(NROW(inside_list[[i]]),NCOL(error)))
  }

  # calculate the sum of squares, transpose to get in same rows as conc.date and columns as pred/error
  if (NCOL(params)>1){
    inside.sum <- t(sapply(1:length(inside_list), function(i) colSums('^'(inside_list[[i]],2))))
  }else{
    inside.sum <- sapply(1:length(inside_list), function(i) sum('^'(inside_list[[i]],2)))
  }
  # Calculate the log-likelihood
  # neg-loglikelihood equation
  if (NCOL(params)>1){
    L1 <- sweep(-ind_list/2, MARGIN = 2,log(2*pi*((sig)^2)),'*')
    L2 <- (1/2)*log(1-(rho)^2)
    L3 <- sweep(t(sapply(1:NROW(conc.date), function(i) (1-(rho)^2)*(error[conc.date[i,1],]-mu_e)^2 + inside.sum[i,])),MARGIN = 2, -1/(2*(sig)^2), '*')

    negLL <- -1*(sweep(L1,MARGIN = 2, L2,'+') + L3)
    negLL <- colSums(negLL)

  }else{
    L1 <- -ind_list/2*(log(2*pi*((sig)^2)))
    L2 <- (1/2)*log(1-(rho)^2)
    L3 <- -1/(2*(sig)^2)*sapply(1:NROW(conc.date), function(i) (1-(rho)^2)*(error[conc.date[i,1]]-mu_e)^2 + inside.sum[i])

    negLL <- -1*(L1 + L2 + L3)
    negLL <- sum(negLL)
  }

  # GaussLiklihood without mu_e or rho
  # L <- (-ind_sum/2)*log(2*pi*((sig)^2))+(-1/(2*(sig)^2)*(error.sum))

  #
  #

  negLL[is.na(negLL)] = Inf

  if(any(negLL == Inf)){
    message(paste(sum(!is.finite(negLL)),"negLL == Inf"))
  }



  return(negLL)
}
