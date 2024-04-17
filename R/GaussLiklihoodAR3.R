GaussLiklihoodAR3 <- function(params, conc, Pred, conc.date){


  if(NCOL(params) >1){
    p <- 3
    rho1 <- params[1,]/10
    rho2 <- params[2,]/10
    rho3 <- params[3,]/10
    rho <- rbind(rho1,rho2,rho3)
    sig <- params[4,]
  }else{
    p <- 3
    rho1 <- params[1]/10
    rho2 <- params[2]/10
    rho3 <- params[3]/10
    rho <- rbind(rho1,rho2,rho3)
    sig <- params[4]
  }

  #re-compute conc.date, if observed days are less than p+2 (2), then remove
  if(any(conc.date[,2] - conc.date[,1] < 3)){
    conc.date <- conc.date[-which(conc.date[,2] - conc.date[,1] < 3),]
  }

  # make observed values into a matrix
  # conc <- matrix(conc, NROW(conc), NCOL(Pred))

  # only run likelihood for observed C values that are finite
  # ind <- is.finite(conc)

  #sum of index that are finite for each run in population
  # ind_sum <- colSums(ind)

  # get error vector for only finite values
  # error <- matrix(NA, NROW(conc), NCOL(Pred))
  error <- log(conc) - log(Pred)

  # # get first vector of e_p
  # error_p <- error[which(ind == TRUE)[1:p],] #sapply(1:length(p), function(h) error[which(ind == TRUE)[1:p[h]],h]) # rep(params[2]/10,p[h])) error[which(ind == TRUE)[1:p],]
  #
  # # get vector of mu, mean, assume zero for now
  # c <- 0
  # mu_p <- error_p
  # # mu_p[lengths(mu_p) == 0] <- 0
  # # mu_p <- replace(mu_p, index(mu_p), 0) # matrix(0,p,NCOL(Pred))
  # # mu_p[1:p,] <- sapply(1:length(rho), function(k) c/(1-rho[k]) sweep(error[1:(NROW(inside)-z),],MARGIN = 2,rho[z,],'*')) c/(1-rho[p])
  #
  # ### Scenario 2 - varying mean bias
  # # if we want mu_e to vary, assume error from first "missing" timesteps of continous period= mu_e
  # # error[conc.date[2:NROW(conc.date),1]-1,] = matrix(mu_e, NROW(conc.date)-1, NCOL(Pred), byrow = TRUE)
  #
  # # initiate empty vectors for only finite values
  # inside <- matrix(NA, NROW(conc), NCOL(Pred))
  #
  # # Calculate the correlation of the error from previous time step
  # # if (NCOL(params)>1) {
  # #
  # #     inside[(p+1):NROW(inside),] <- error[(p+1):NROW(inside),] - sum(sapply(1:length(rho), function(z) sweep(error[1:(NROW(inside)-z),],MARGIN = 2,rho[z,],'*')))
  # # }else{
  #
  #     inside[(p+1):NROW(inside)] <- error[(p+1):NROW(inside)] - sum(sapply(1:length(rho), function(z) rho[z]*(error[1:(NROW(inside)-z)])))
  # # }
  # get vector of mu, mean, assume zero for now
  c <- 0
  mu_p <- rep(0,p) #sapply(1:p, function(i) (1-rho[i])

  # Calculate the correlation of the error from previous time steps, i.e. sum of squares
  if (NCOL(params)>1) {

    if(p>1){

    inside_list <- sapply(1:NROW(conc.date), function(i) sweep(error[(conc.date[i,1]+3):(conc.date[i,2]),],MARGIN = 2,c,'-') - Reduce("+",lapply(1:3, function(z) sweep(error[(conc.date[i,1]+3-z):(conc.date[i,2]-z),], MARGIN = 2,rho[z,],'*'))))
    inside_list <- sapply(1:NROW(conc.date), function(i) sweep('^'(inside_list[[i]],2), MARGIN = 2, (2*sig^2), '/'))
    inside.sum <- t(sapply(1:NROW(conc.date), function(i) colSums(inside_list[[i]])))

    }else{

    inside_list <- sapply(1:NROW(conc.date), function(i) sweep(error[(conc.date[i,1]+1):(conc.date[i,2]),],MARGIN = 2,c,'-') -  sweep(error[(conc.date[i,1]):(conc.date[i,2]-1),], MARGIN = 2,rho,'*'))
    inside_list <- sapply(1:NROW(conc.date), function(i) sweep('^'(inside_list[[i]],2), MARGIN = 2, (2*sig^2), '/'))
    inside.sum <- t(sapply(1:NROW(conc.date), function(i) colSums(inside_list[[i]])))

    }
  }else{

    if(p>1){

      inside_list <- sapply(1:NROW(conc.date), function(i) error[(conc.date[i,1]+3):(conc.date[i,2])] - c - Reduce("+",lapply(1:3, function(z) error[(conc.date[i,1]+3-z):(conc.date[i,2]-z)]*rho[z])))
      inside_list <- sapply(1:NROW(conc.date), function(i) '^'(inside_list[[i]],2)/(2*sig^2))
      inside.sum <- sapply(1:NROW(conc.date), function(i) sum(inside_list[[i]]))

    }else{

      inside_list <- sapply(1:NROW(conc.date), function(i) error[(conc.date[i,1]+1):(conc.date[i,2])] - c -  error[(conc.date[i,1]):(conc.date[i,2]-1)]*rho)
      inside_list <- sapply(1:NROW(conc.date), function(i) '^'(inside_list[[i]],2)/(2*sig^2))
      inside.sum <- sapply(1:NROW(conc.date), function(i) sum(inside_list[[i]]))
    }
  }

  # get covariance matrix
  if(NCOL(params) > 1){

    if(p>1){
      Vp_list <- lapply(1:NCOL(Pred), function(i) Covmatrix(p,rho[,i]))
    }else{
      Vp_list <- 1-rho^2
    }

  }else{

    if(p>1){
      Vp_list <- Covmatrix(p,rho)
    }else{
      Vp_list <- 1-rho^2
    }
  }
  # neg-loglikelihood equation
  if(NCOL(params) > 1){

    if(p>1){
      L1 <- -p/2*(log(2*pi)) - p/2*log(sig^2) + sapply(1:NCOL(Pred), function(i) 1/2*log(det(Vp_list[[i]])))
      L2 <- lapply(1:NROW(conc.date), function(i) -1/(2*sig^2)* sapply(1:NCOL(Pred), function(z) t(as.matrix(error[conc.date[i,1]:(conc.date[i,1]+p-1),z]) - mu_p)%*%Vp_list[[z]]%*%as.matrix(as.matrix(error[conc.date[i,1]:(conc.date[i,1]+p-1),z]) - mu_p)))
      L3 <- -inside.sum

      negLL <- t(sapply(1:NROW(conc.date), function(i) L2[[i]] + L3[i,]))
      negLL <- -1*(colSums(negLL) + L1)
    }else{
      L1 <- -1/2*(log(2*pi)) - 1/2*log(sig^2) - 1/2*log(Vp_list)
      L2 <- lapply(1:NROW(conc.date), function(i) - (error[conc.date[i,1],] - c/(1-rho))^2/(2*sig^2/Vp_list) - ((length(conc.date[i,1]:conc.date[i,2])-1)/2)*log(2*pi) - ((length(conc.date[i,1]:conc.date[i,2])-1)/2)*log(sig^2))
      L3 <- -inside.sum

      negLL <- t(sapply(1:NROW(conc.date), function(i) L2[[i]] + L3[i,]))
      negLL <- -1*(colSums(negLL) + L1)

    }
  }else{
    if(p>1){
      L1 <- -p/2*(log(2*pi)) - p/2*log(sig^2) + 1/2*log(det(Vp_list))
      L2 <- lapply(1:NROW(conc.date), function(i) -1/(2*sig^2)* t(as.matrix(error[conc.date[i,1]:(conc.date[i,1]+p-1)]) - mu_p)%*%Vp_list%*%(as.matrix(error[conc.date[i,1]:(conc.date[i,1]+p-1)]) - mu_p))
      L3 <- -inside.sum

      negLL <- unlist(L2) + L3
      negLL <- -1*(sum(negLL) + L1)

    }else{
      L1 <- -p/2*(log(2*pi)) - p/2*log(sig^2) - 1/2*log(Vp_list)
      L2 <- lapply(1:NROW(conc.date), function(i) -(error[conc.date[i,1]] - c/(1-rho))^2/(2*sig^2/Vp_list) - ((length(conc.date[i,1]:conc.date[i,2])-1)/2)*log(2*pi) - ((length(conc.date[i,1]:conc.date[i,2])-1)/2)*log(sig^2))
      L3 <- -inside.sum

      negLL <- unlist(L2) + L3
      negLL <- -1*(sum(negLL) + L1)
    }
  }



  # Scenario 1
  # Make the first values (t-1) at the start of each continuous record equal e_t
  # TEMP FIX, only works if mu_e = 0
  # inside[conc.date[,1],] = error[conc.date[,1],]

  # update count, sum of values that don't equal NA, are not missing or reoved from AR1
  # ind_sum <- colSums(!is.na(inside =='NA'))

  # Assuming data gaps have zero error (so they do not contribute to the summing of error)
  # inside[is.na(inside =='NA')] <- 0 # if missing data (gaps), just make zero
  # inside.sum <- colSums('^'(inside,2)) # if any Nan in error values, because negative Prediction, the total error is not calculated

  # Calculate the log-likelihood
  # neg-loglikelihood equation
  # L1 <- -p/2*(log(2*pi)) - p/2*log(sig^2) + 1/2*log(det(V_p))
  # L2 <- -1/(2*sig^2)*t(error_p - mu_p)*V_p*(error_p-mu_p)
  # L3 <- -((ind_sum-p)/2)*log(2*pi) - ((ind_sum-p)/2)*log((sig)^2)
  # L4 <- -inside.sum  #-1/(2*(sig)^2)*((1-(rho)^2)*(error[which(ind == TRUE)[1],]-mu_e)^2 + inside.sum)
  #
  # negLL <- -1*(L1 + L2+ L3 +L4)
  #
  # # GaussLiklihood without mu_e or rho
  # # L <- (-ind_sum/2)*log(2*pi*((sig)^2))+(-1/(2*(sig)^2)*(error.sum))
  #
  # #
  # negLL[is.na(negLL)] = Inf
  # #
  # # if(ncol(params)>1){
  # #   negLL.rerun <- objective_function1(as.matrix(params[,27]),Q,C,cl)
  # #
  # #   if(isFALSE(all.equal(negLL[27],negLL.rerun))){
  # #     message(paste("negLL[27] does not equal re-run of params[,27] = ",negLL[27],"  ",negLL.rerun))
  # #   }
  # # }
  # if(any(negLL == Inf)){
  #   message(paste(sum(!is.finite(negLL)),"negLL == Inf"))
  # }

  negLL[is.na(negLL)] = Inf
  #
  # if(ncol(params)>1){
  #   negLL.rerun <- objective_function1(as.matrix(params[,27]),Q,C,cl)
  #
  #   if(isFALSE(all.equal(negLL[27],negLL.rerun))){
  #     message(paste("negLL[27] does not equal re-run of params[,27] = ",negLL[27],"  ",negLL.rerun))
  #   }
  # }

  if(p>1){
  #Find roots, check if OUTSIDE unit circle, is the AR model stable?
    if(NCOL(params)>1){



      if(any(is.finite(negLL))){
        # get roots
        AR.polynomial.roots = lapply(1:NCOL(Pred), function(i) polyroot(as.vector(rho[,i])))

        negLL <- sapply(1:NCOL(Pred), function(i) ifelse(any(abs(AR.polynomial.roots[[i]])<=1), Inf, negLL[i]))
        # # If unit circle, the return false for AR being non-stationary
        # if (any(abs(AR.polynomial.roots)<=1)){
        #   #return(FALSE) OR make negLL Inf
        #   negLL[is.na(negLL)] = Inf
        # }
       }
      }else{
      if(any(is.finite(negLL))){
        # get roots
        AR.polynomial.roots = polyroot(as.vector(rho))

        negLL <- ifelse(any(abs(AR.polynomial.roots)<=1), Inf, negLL)

      }
    }
  }

  if(any(negLL == Inf)){
    message(paste(sum(!is.finite(negLL)),"negLL == Inf"))
  }


  return(negLL)
}
