Chat19 <- function(params, flow, flow.s, conc, flow.date) { #Assume two constant loads following Hall's work

  flow.q = flow - flow.s

  if(NCOL(params)>1){
    Bs <- 1/(params[1,]/10)

    Bq <- 10^params[3,]
    C0 <- 10^params[4,] #ifelse(is.na(mean(conc[which(flow == 0)], na.rm = TRUE)), mean(conc[which(flow < quantile(flow,0.10, na.rm = TRUE))], na.rm = TRUE),mean(conc[which(flow == 0)], na.rm = TRUE)) #10^params[3,] #
    Cs <- 10^params[5,]
    Cq <- 10^params[6,]
    V0 <- 10^params[7,]


    # Pred <-  sweep(sweep(sweep(flow.s, MARGIN = 2, Bs, '*'), MARGIN = 2, Cs, '*') + sweep(sweep(flow.q, MARGIN = 2, Bq, '*'), MARGIN = 2, Cq, '*'),MARGIN = 2, C0,'+') /
    #   (1 + sweep(flow.s, MARGIN = 2, Bs, '*') + sweep(flow.q, MARGIN = 2, Bq, '*'))

    Pred <-  sweep(sweep(sweep(flow.s, MARGIN = 2, Bs, '*'), MARGIN = 2, Cs, '*') + sweep(sweep(flow.q, MARGIN = 2, Bq, '*'), MARGIN = 2, Cq, '*'),MARGIN = 2, C0*V0,'+') /
      (sweep(sweep(flow.s, MARGIN = 2, Bs, '*') + sweep(flow.q, MARGIN = 2, Bq, '*'),MARGIN=2,V0,'+'))

    # Pred[,Cq>Cs | Cs>C0] <- matrix(0,nrow = nrow(Pred),1)

  } else {
    # Bs <- 10^params[1]
    # Bq <- 10^params[2]
    # C0 <- 10^params[3]              #ifelse(is.na(mean(conc[which(flow == 0)], na.rm = TRUE)), mean(conc[which(flow < quantile(flow,0.10, na.rm = TRUE))], na.rm = TRUE),mean(conc[which(flow == 0)], na.rm = TRUE)) #10^params[3] #
    # Cs <- 10^params[4]
    # Cq <- 10^params[5]

    Bs <- 1/(params[1]/10)

    Bq <- 10^params[3]
    C0 <- 10^params[4] #ifelse(is.na(mean(conc[which(flow == 0)], na.rm = TRUE)), mean(conc[which(flow < quantile(flow,0.10, na.rm = TRUE))], na.rm = TRUE),mean(conc[which(flow == 0)], na.rm = TRUE)) #10^params[3,] #
    Cs <- 10^params[5]
    Cq <- 10^params[6]
    V0 <- 10^params[7]



    # if(Cq>Cs | Cs>C0){
    #
    #   Pred <- rep(0,length(flow))
    #
    # }else{

      Pred <- (C0*V0 + Cs*Bs*flow.s + Cq*Bq*flow.q) / (V0 + Bs*flow.s + Bq*flow.q)

    # }


  }


  return(Pred)
}
