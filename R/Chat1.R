Chat1 <- function(params, flow) { # # power law between C ~ Q

  if(NCOL(params)>1){
    a <- 10^params[1,]
    b <- params[2,]
    # Pred <- a * flow^b

    Pred <- sweep(sweep(flow, MARGIN=2,b, `^`), MARGIN=2,a,`*`)

    # Pred <- sweep(ifelse(flow == 0,1,sweep(flow, MARGIN=2,b, `^`)), MARGIN=2,a,`*`)

  } else {

  a <- 10^params[1]
  b <- params[2]
  Pred <- a * flow^b

  # Pred <- a * ifelse(flow==0,1,flow^b)

  }
  return(Pred)
}
