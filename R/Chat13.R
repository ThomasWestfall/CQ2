Chat13 <- function(params, flow, flow.s, conc, flow.date) { #Johnson 1969, with two flow components

  flow.q = flow - flow.s

  if(NCOL(params)>1){
    Bs <- 1/(params[1,]/10)

    Bq <- 10^params[3,] #gdg
    L0 <- 10^params[4,]
    Cs <- 10^params[5,]
    CqBq <- 10^params[6,]
    V0 <- 10^params[7,]


    Pred <-  sweep(sweep(sweep(flow.s, MARGIN = 2, Bs, '*'), MARGIN = 2, Cs, '*') + sweep(flow.q, MARGIN = 2, CqBq, '*'),MARGIN = 2, L0,'+') /
      (sweep(sweep(flow.s, MARGIN = 2, Bs, '*') + sweep(flow.q, MARGIN = 2, Bq, '*'),MARGIN=2,V0,'+'))


  } else {

    Bs <- 1/(params[1]/10)

    Bq <- 10^params[3]
    L0 <- 10^params[4]
    Cs <- 10^params[5]
    CqBq <- 10^params[6]
    V0 <- 10^params[7]


    Pred <- (L0 + Cs*Bs*flow.s + CqBq*flow.q) / (V0 + Bs*flow.s + Bq*flow.q)
  }


  return(Pred)
}
