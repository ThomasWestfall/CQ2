GaussLiklihoodAR1 <- function(params, conc, Pred, conc.date){ # Hamilton (1994)


  if(NCOL(params) >1){
    p <- 1
    rho <- params[1,]/10
    sig <- params[2,]/10

  }else{
    p <- 1
    rho <- params[1]/10
    sig <- params[2]/10

  }

  #re-compute conc.date, if observed days are less than p+2 (2), then remove
  if(any(conc.date[,2] - conc.date[,1] < 3)){
    conc.date <- conc.date[-which(conc.date[,2] - conc.date[,1] < 3),]
  }

  # get error vector
  error <- log(conc) - log(Pred)

  c <- 0 #assume mean error bias = 0
  mu_p <- rep(0,p)

  # Calculate the correlation of the error from previous time steps, i.e. sum of squares
  if (NCOL(params)>1) {

    inside_list <- sapply(1:NROW(conc.date), function(i) sweep(error[(conc.date[i,1]+1):(conc.date[i,2]),],MARGIN = 2,c,'-') -  sweep(error[(conc.date[i,1]):(conc.date[i,2]-1),], MARGIN = 2,rho,'*')) #default at p = 1, even if p = 0, need to ignore first error value
    inside_list <- sapply(1:NROW(conc.date), function(i) sweep('^'(inside_list[[i]],2), MARGIN = 2, (2*sig^2), '/'))
    inside.sum <- t(sapply(1:NROW(conc.date), function(i) colSums(inside_list[[i]])))

  }else{

    inside_list <- sapply(1:NROW(conc.date), function(i) error[(conc.date[i,1]+1):(conc.date[i,2])] - c -  error[(conc.date[i,1]):(conc.date[i,2]-1)]*rho) #default at p = 1, even if p = 0, need to ignore first error value
    inside_list <- sapply(1:NROW(conc.date), function(i) '^'(inside_list[[i]],2)/(2*sig^2))
    inside.sum <- sapply(1:NROW(conc.date), function(i) sum(inside_list[[i]]))

  }

  # get covariance matrix, no need to use Covmatrix function for AR1
  if(NCOL(params) > 1){

      Vp_list <- 1-rho^2

  }else{

      Vp_list <- 1-rho^2
  }

  # neg-loglikelihood equation
  if(NCOL(params) > 1){

      L1 <- -p/2*(log(2*pi)) - p/2*log(sig^2) - p/2*log(Vp_list)
      L2 <- lapply(1:NROW(conc.date), function(i) - (error[conc.date[i,1],] - c/(1-rho))^2/(2*sig^2/Vp_list) - ((length(conc.date[i,1]:conc.date[i,2])-p)/2)*log(2*pi) - ((length(conc.date[i,1]:conc.date[i,2])-p)/2)*log(sig^2))
      L3 <- -inside.sum

      negLL <- t(sapply(1:NROW(conc.date), function(i) L2[[i]] + L3[i,]))
      negLL <- -1*(colSums(negLL) + L1)


  }else{

      L1 <- -p/2*(log(2*pi)) - p/2*log(sig^2) - p/2*log(Vp_list)
      L2 <- lapply(1:NROW(conc.date), function(i) -(error[conc.date[i,1]] - c/(1-rho))^2/(2*sig^2/Vp_list) - ((length(conc.date[i,1]:conc.date[i,2])-p)/2)*log(2*pi) - ((length(conc.date[i,1]:conc.date[i,2])-p)/2)*log(sig^2))
      L3 <- -inside.sum

      negLL <- unlist(L2) + L3
      negLL <- -1*(sum(negLL) + L1)

  }

  # if any negLL are NA, then set to infinity, remember we are minimizing the negative log-likelihood
  # so the optimizer will steer away from results giving positive infinity
  negLL[is.na(negLL)] = Inf

  # output message if this is the case..
  if(any(negLL == Inf)){
    message(paste(sum(!is.finite(negLL)),"negLL == Inf"))
  }


  return(negLL)
}
