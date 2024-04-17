Chat8 <- function(params, flow, flow.s, flow.date) {  #quick-slow CQ variant,
  #source load varies as slow and quick flow vary

  # calc quick-flow
  flow.q <- flow - flow.s

  if(NCOL(params)>1){

    b <- params[1,]
    Cq <- 10^params[2,]
    Cs <- 10^params[3,]

    Pred <- (sweep(flow.s, MARGIN=2, Cs,`*`) + sweep(flow.q, MARGIN=2, Cq,`*`))*sweep(flow,MARGIN=2,b,`^`)

  }else{

    b <- params[1]
    Cq <- 10^params[2]
    Cs <- 10^params[3]

    Pred <- (Cs*(flow.s) + Cq*(flow.q))*(flow^b)

  }


  return(Pred)
}
