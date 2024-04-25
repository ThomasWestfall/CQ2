GaussLiklihoodBV1 <- function(params, conc, Pred, conc.date){

  # AR1 Gauss Log-Likelihood from likelihood function (B7.1.6, pg. 277) in Rainfall-Runoff modelling: the primer by Keith Beven (2012)
  # should be identical results to "GaussLiklihoodAR1"

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

  # assume mean error bias is constant at zero
  mu_e = rep(0, NCOL(Pred))


  # Calculate the correlation of the error from previous time step
  if (NCOL(params)>1) {
    inside_list <- sapply(1:NROW(conc.date), function(i) (sweep(error[(conc.date[i,1]+1):(conc.date[i,2]),], MARGIN = 2, mu_e,'-') - sweep(sweep(error[conc.date[i,1]:(conc.date[i,2]-1),], MARGIN = 2, mu_e, '-'),MARGIN = 2, rho,'*')))
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


  negLL[is.na(negLL)] = Inf

  if(any(negLL == Inf)){
    message(paste(sum(!is.finite(negLL)),"negLL == Inf"))
  }



  return(negLL)
}
