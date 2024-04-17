bhat1 <- function(params, flow, conc) { # # power law between C ~ Q

  # IF using observed C and Q with calibrated "a", what is "b"?
  # how does it vary overtime?

  if(NCOL(params)>1){
    a <- 10^params[1,]

    Pred <- log(sweep(conc, MARGIN = 2,a,"/")) / log(flow)

  } else {

    a <- 10^params[1]

    Pred <- log(conc/a)/log(flow)

  }
  return(Pred)
}

