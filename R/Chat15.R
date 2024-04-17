Chat15 <- function(params, flow, flow.s, flow.date) { #Hall 1970 "model 5" with two flow components

  flow.q = flow - flow.s

  if(NCOL(params)>1){
    Bs <- 10^params[1,]
    Bq <- 10^params[2,]
    C0 <- 10^params[3,]                   #ifelse(is.na(mean(conc[which(flow == 0)], na.rm = TRUE)), mean(conc[which(flow < 0.005)], na.rm = TRUE),mean(conc[which(flow == 0)], na.rm = TRUE)) #10^params[3,] #10^params[3,] #i
    CsBs <- 10^params[4,]
    CqBq <- 10^params[5,]
    ns <- 10^params[6,]
    nq <- 10^params[7,]

    Pred <-  sweep(sweep(sweep(flow.s,MARGIN =2,(1/ns),'^'), MARGIN = 2, CsBs, '*') + sweep(sweep(flow.q,MARGIN = 2,(1/nq),'^'), MARGIN = 2, CqBq, '*'),MARGIN = 2, C0,'+') /
      (1 + sweep(sweep(flow.s,MARGIN =2,(1/ns),'^'), MARGIN = 2, Bs, '*') + sweep(sweep(flow.q,MARGIN = 2,(1/nq),'^'), MARGIN = 2, Bq, '*'))

  } else {
    Bs <- 10^params[1]
    Bq <- 10^params[2]
    C0 <- 10^params[3] #ifelse(is.na(mean(conc[which(flow == 0)], na.rm = TRUE)), mean(conc[which(flow < 0.005)], na.rm = TRUE),mean(conc[which(flow == 0)], na.rm = TRUE)) #10^params[3] #10^params[3] #
    CsBs <- 10^params[4]
    CqBq <- 10^params[5]
    ns <- 10^params[6]
    nq <- 10^params[7]

    Pred <- (C0 + CsBs*flow.s^(1/ns) + CqBq*flow.q^(1/nq)) / (1 + Bs*flow.s^(1/ns) + Bq*flow.q^(1/nq))


  }


  return(Pred)
}
