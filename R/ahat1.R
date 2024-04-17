ahat1 <- function(params, flow, conc) { # # power law between C ~ Q

  if(NCOL(params)>1){
    b <- params[1,]
    # Pred <- a * flow^b

    Pred <- conc / sweep(flow, MARGIN=2,b, `^`)
  } else {
  b <- params[1]
  Pred <- conc / flow^b
  }
  return(Pred)
}
