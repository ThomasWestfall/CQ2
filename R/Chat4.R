Chat4 <- function(params, flow, flow.s, flow.date) { #quick-slow CQ of Steele (1968)

  # calc quick-flow
  flow.q <- flow - flow.s

  if(NCOL(params)>1){

    # initiate parameters
    as <- 10^params[1,]
    bs <- params[2,]
    aq <- 10^params[3,]
    bq <- params[4,]


    Pred <- (1/flow)*(sweep(sweep(flow.s, MARGIN=2,bs, `^`), MARGIN=2,as,`*`)*(flow.s) +
      ifelse(flow.q>0,sweep(sweep(flow.q, MARGIN=2,bq, `^`), MARGIN=2,aq,`*`)*flow.q, + matrix(0,nrow(flow.q),ncol(flow.q),byrow=TRUE)))

    # Note: default quick-flow term to zero if flow.q = 0 else 0^-bq is undefined

  }else{

    # initiate parameters
    as <- 10^params[1]
    bs <- params[2]
    aq <- 10^params[3]
    bq <- params[4]

    Pred <- (1/flow)*((as)*(flow.s)^bs*(flow.s) + aq*ifelse(flow.q>0,(flow.q)^bq*(flow.q),0))

    # Note: default quick-flow term to zero if flow.q = 0 else 0^-bq is undefined
  }

  return(Pred)
}
