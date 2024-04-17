ahat1 <- function(params, flow, conc) { # # power law between C ~ Q

  # IF using observed C and Q with calibrated "b", what is "a"?
  # how does it vary overtime?

  if(NCOL(params)>1){

    b <- params[1,]

    Pred <- conc / sweep(flow, MARGIN=2,b, `^`)

  } else {

    b <- params[1]

    Pred <- conc / flow^b

  }
  return(Pred)
}
