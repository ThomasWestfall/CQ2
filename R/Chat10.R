Chat10 <- function(params, flow, flow.s, flow.date) {


  # calc fast-flow
  flow.q <- flow - flow.s

  if(NCOL(params)>1){

    bs <- params[1,]
    Cq <- 10^params[2,]
    bq <- params[3,]
    Cs <- 10^params[4,]

    Pred <- (sweep(flow.s, MARGIN=2, Cs,`*`) + sweep(flow.q, MARGIN=2, Cq,`*`))*sweep(flow.s,MARGIN=2,bs,`^`)*ifelse(flow.q>0,sweep(flow.q,MARGIN=2,bq,`^`),1)

    # if(any(Cq)>Cs)
    # Pred[,Cq>Cs] <- matrix(0,nrow = nrow(Pred),1)


  }else{

    bs <- params[1]
    Cq <- 10^params[2]
    bq <- params[3]
    Cs <- 10^params[4]

    # if(Cq>Cs){ # make prediction 0 if quick flow conc is greater than slow flow conc
    #
    #   Pred <- rep(0,length(flow))
    #
    # }else{

      Pred <- (Cs*(flow.s) + Cq*(flow.q))*(flow.s^bs)*(ifelse(flow.q>0,flow.q^bq,1))

    # }


  }


  return(Pred)
}
