Chat8 <- function(params, flow, flow.s, flow.date) {


  # calc fast-flow
  flow.q <- flow - flow.s

  #calc groundwater end member
  # Cs <- max(conc[which(flow> 0)], na.rm = TRUE)

  if(NCOL(params)>1){

    b <- params[1,]
    Cq <- 10^params[2,]
    Cs <- 10^params[3,]

    Pred <- (sweep(flow.s, MARGIN=2, Cs,`*`) + sweep(flow.q, MARGIN=2, Cq,`*`))*sweep(flow,MARGIN=2,b,`^`)

    # if(any(Cq)>Cs)
    # Pred[,Cq>Cs] <- matrix(0,nrow = nrow(Pred),1)


  }else{

    b <- params[1]
    Cq <- 10^params[2]
    Cs <- 10^params[3]

    # if(Cq>Cs){ # make prediction 0 if quick flow conc is greater than slow flow conc
    #
    #   Pred <- rep(0,length(flow))
    #
    # }else{

      Pred <- (Cs*(flow.s) + Cq*(flow.q))*(flow^b)

    # }


  }


  return(Pred)
}
