bhat1 <- function(params, flow, conc) { # # power law between C ~ Q

  if(NCOL(params)>1){
    a <- params[1,]^5
    # Pred <- a * flow^b

    Pred <- log(sweep(conc, MARGIN = 2,a,"/")) / log(flow)
  } else {
  a <- params[1]^5
  Pred <- log(conc/a)/log(flow)
  }
  return(Pred)
}

# conc/a = flow^b
#log(conc/a) = b*log(flow)
#log(conc/a)/log(flow) = b
