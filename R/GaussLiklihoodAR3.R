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

  error <- log(conc) - log(Pred)

  # get vector of mu, mean, assume zero for now
  c <- 0
  mu_p <- rep(0,p)

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

  negLL[is.na(negLL)] = Inf

  if(p>1){
  #Find roots, check if OUTSIDE unit circle, is the AR model stable?
    if(NCOL(params)>1){

      if(any(is.finite(negLL))){
        # get roots
        AR.polynomial.roots = lapply(1:NCOL(Pred), function(i) polyroot(as.vector(rho[,i])))

        negLL <- sapply(1:NCOL(Pred), function(i) ifelse(any(abs(AR.polynomial.roots[[i]])<=1), Inf, negLL[i]))
        # # If unit circle, the return false for AR being non-stationary
        if (any(abs(AR.polynomial.roots)<=1)){
          #make negLL Inf
          negLL[is.na(negLL)] = Inf
        }
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
