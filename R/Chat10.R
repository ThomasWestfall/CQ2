Chat10 <- function(params, flow, flow.s, flow.date) { #quick-slow CQ variant,
  #source load varies as slow and quick flow vary
  #two solute responses

  # calc quick-flow
  flow.q <- flow - flow.s

  if(NCOL(params)>1){

    bs <- params[1,]
    Cq <- 10^params[2,]
    bq <- params[3,]
    Cs <- 10^params[4,]

    Pred <- (sweep(flow.s, MARGIN=2, Cs,`*`) + sweep(flow.q, MARGIN=2, Cq,`*`))*sweep(flow.s,MARGIN=2,bs,`^`)*ifelse(flow.q>0,sweep(flow.q,MARGIN=2,bq,`^`),1)

    # Note: default quick-flow term to 1 if flow.q = 0 else 0^-bq is undefined

  }else{

    bs <- params[1]
    Cq <- 10^params[2]
    bq <- params[3]
    Cs <- 10^params[4]

    Pred <- (Cs*(flow.s) + Cq*(flow.q))*(flow.s^bs)*(ifelse(flow.q>0,flow.q^bq,1))

    # Note: default quick-flow term to 1 if flow.q = 0 else 0^-bq is undefined

  }


  return(Pred)
}
