Covmatrix <- function(p,rho){ # this is only used for AR1 and AR3 likelihood

  Vp = matrix(NA,p,p)
  if(p > 1){
    # input diag values
    diag(Vp) = (1-rho[p]^2)

    # index values that need to be calculated
    Vp.ind <- which(upper.tri(Vp) == TRUE, arr.ind=TRUE)

    # calculate the upper right triangle values following Hamilton and Galbraith and Galbraith (1974, equation 16, p. 70)
    left <- sapply(1:NROW(Vp.ind), function(r) sapply(0:(Vp.ind[r,1]-1), function(vl) ifelse(vl == 0,-1,rho[vl]) * ifelse(vl+Vp.ind[r,2]-Vp.ind[r,1] == 0,-1,rho[vl+Vp.ind[r,2]-Vp.ind[r,1]])))
    right <- sapply(1:NROW(Vp.ind), function(s) sapply((p+1-Vp.ind[s,2]):(p+Vp.ind[s,1]-Vp.ind[s,2]), function(vr) rho[vr] * rho[vr+Vp.ind[s,2]-Vp.ind[s,1]]))

    #sum if multiple values for higher order covariance matrix
    left <- sapply(left, sum)
    right <- sapply(right, sum)

    #now together
    Vp[Vp.ind] <- left - right

    # since matrix is symmetrical, populate lower portion with values
    Vp[lower.tri(Vp)] = t(Vp)[lower.tri(Vp)]

  } else { # if AR1, just do this... 1-rho^2
    Vp[p] = (1-rho[p]^2)
  }

return(Vp)
}
