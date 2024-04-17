Chat16 <- function(params, flow, flow.s, conc, flow.date) { #Johnson 1969 with two-flow components

  flow.q = flow - flow.s

  if(NCOL(params)>1){
    Bs <- 1/(params[1,]/10)
    Bq <- 10^params[3,]
    Co <- 10^params[4,]
    Cs <- 10^params[5,]
    Cq <- 10^params[6,]
    b <- params[7,]

    Pred <-  (sweep(sweep(sweep(sweep(flow.s, MARGIN =2,b,"^"), MARGIN = 2, Bs, '*'), MARGIN = 2, Cs, '*') + sweep(sweep(flow.q, MARGIN = 2, Bq, '*'), MARGIN = 2, Cq, '*'), MARGIN = 2, Co,'+')) /
      (1 + sweep(sweep(flow.s, MARGIN =2,b,"^"), MARGIN = 2, Bs, '*') + sweep(flow.q, MARGIN = 2, Bq, '*'))

    # if(any(Cq)>Cs)
    # Pred[,Cq>Cs] <- matrix(0,nrow = nrow(Pred),1)


  } else {
    Bs <- 1/(params[1]/10)
    Bq <- 10^params[3]
    Co <- 10^params[4]
    Cs <- 10^params[5]
    Cq <- 10^params[6]
    b <- params[7]

    # if(Cs>Cq){

      Pred <- (Co + Cs*Bs*flow.s^b + Cq*Bq*flow.q) / (1 + Bs*flow.s^b + Bq*flow.q)

    # }else{
    #
    #   Pred <- rep(0,length(flow))
    # }


  }


  return(Pred)
}
