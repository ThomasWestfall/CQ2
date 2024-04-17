GaussLiklihoodAR1 <- function(params, conc, Pred, conc.date){


  if(NCOL(params) >1){
    p <- 1
    # rho1 <- params[1,]/5
    # rho2 <- params[2,]/5
    # rho3 <- params[3,]/5
    # rho <- rbind(rho1,rho2,rho3)
    ## calibrate p, assuming rho is same for all p's
    # p <- round(params[1,],0)
    # rho <- sapply(1:length(p), function(h) rep(params[2,h]/10,p[h]))
    rho <- params[1,]/10
    sig <- params[2,]/10
  }else{
    p <- 1
    # rho1 <- params[1]/5
    # rho2 <- params[2]/5
    # rho3 <- params[3]/5
    # rho <- rbind(rho1,rho2,rho3)
    # p <- round(params[1],0)
    # rho <- sapply(1:length(p), function(h) rep(params[2]/10,p[h]))
    rho <- params[1]/10
    sig <- params[2]/10
  }

  #re-compute conc.date, if observed days are less than p+2 (2), then remove
  if(any(conc.date[,2] - conc.date[,1] < 3)){
    conc.date <- conc.date[-which(conc.date[,2] - conc.date[,1] < 3),]
  }

  # get error vector
  error <- log(conc) - log(Pred)

  c <- 0 #assume mean error = 0
  mu_p <- rep(0,p) #sapply(1:p, function(i) (1-rho[i])

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

  # get covariance matrix
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

      L1 <- -p/2*(log(2*pi)) - p/2*log(sig^2) - p/2*log(Vp_list)  # equals zero when p and rho = 0
      L2 <- lapply(1:NROW(conc.date), function(i) -(error[conc.date[i,1]] - c/(1-rho))^2/(2*sig^2/Vp_list) - ((length(conc.date[i,1]:conc.date[i,2])-p)/2)*log(2*pi) - ((length(conc.date[i,1]:conc.date[i,2])-p)/2)*log(sig^2)) #equals 57.55....
      L3 <- -inside.sum # same as Gaussian Likelihood funciton AR0 when p and rho = 0

      #-(error[conc.date[i,1]] - c/(1-rho))^2/(2*sig^2/Vp_list) this result is the only difference between AR0 and AR1... because this term reduces to just the first error squared e_1^2/2sig^2... but I am still considering all error terms in sum of squares...
      # this term ^ needs to reduce to zero when p and rho = 0 OR the sum of squares must always ignore the first term in the AR1 likelihood.. the latter approach is more correct...

      negLL <- unlist(L2) + L3
      negLL <- -1*(sum(negLL) + L1)

  }

  negLL[is.na(negLL)] = Inf

  if(any(negLL == Inf)){
    message(paste(sum(!is.finite(negLL)),"negLL == Inf"))
  }


  return(negLL)
}
