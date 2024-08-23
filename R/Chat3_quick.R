Chat3_quick <- function(params, flow, flow.s, flow.date) { #quick-slow CQ of Gunnerson (1967)

  #flow.s is baseflow (from BhatEck) either as is or in matrix form with duplicates in each column

  # calc quick-flow
  flow.q <- flow - flow.s

  if(NCOL(params)>1){

    # initiate parameters
    as <- 10^params[1,]
    bs <- params[2,]
    aq <- 10^params[3,]
    bq <- params[4,]

    Pred <- sweep(sweep(flow.s, MARGIN=2,bs, `^`), MARGIN=2,as,`*`) +
      ifelse(flow.q>0,sweep(sweep(flow.q, MARGIN=2,bq, `^`), MARGIN=2,aq,`*`),0)

    # Note: default quick-flow term to zero if flow.q = 0 else 0^-bq is undefined

  }else{
    as <- 10^params[1]
    bs <- params[2]
    aq <- 10^params[3]
    bq <- params[4]

    Pred <- ifelse(flow.q>0, (aq)*(flow.q)^bq, 0)

    # Note: default quick-flow term to zero if flow.q = 0 else 0^-bq is undefined

  }

  return(Pred)
}
