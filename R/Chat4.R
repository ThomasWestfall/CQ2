Chat4 <- function(params, flow, flow.s, flow.date) { #C ~ Qs,Qf, /Q for both Steele 1968

  # calc fast-flow
  flow.q <- flow - flow.s

  if(NCOL(params)>1){

    # initiate parameters
    as <- 10^params[1,]
    bs <- params[2,]
    aq <- 10^params[3,]
    bf <- params[4,]


    Pred <- (1/flow)*(sweep(sweep(flow.s, MARGIN=2,bs, `^`), MARGIN=2,as,`*`)*(flow.s) +
      ifelse(flow.q>0,sweep(sweep(flow.q, MARGIN=2,bf, `^`), MARGIN=2,aq,`*`)*flow.q, + matrix(0,nrow(flow.q),ncol(flow.q),byrow=TRUE)))


  }else{

    # initiate parameters
    as <- 10^params[1]
    bs <- params[2]
    aq <- 10^params[3]
    bf <- params[4]

    Pred <- (1/flow)*((as)*(flow.s)^bs*(flow.s) + aq*ifelse(flow.q>0,(flow.q)^bf*(flow.q),0))
  }

  return(Pred)
}
