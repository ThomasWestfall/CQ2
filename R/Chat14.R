Chat14 <- function(params, flow, conc) { # # Hall 1970 "model 5"

  if(NCOL(params)>1){
    B <- 10^params[1,]
    Cq <- 10^params[2,]
    n <- 10^params[3,]


    C0 <- 10^params[4,] #ifelse(is.na(mean(conc[which(flow == 0)], na.rm = TRUE)), mean(conc[which(flow < quantile(flow,0.10, na.rm = TRUE))], na.rm = TRUE),mean(conc[which(flow == 0)], na.rm = TRUE))
    Cd <- C0 - Cq


    Pred <- sweep(sweep((1/(1 + sweep(sweep(flow,MARGIN=2, (1/n),'^'), MARGIN=2,B, `*`))),MARGIN = 2, Cd,'*'), MARGIN = 2, Cq, '+')

    # if(any(Cq)>Cs)
    # Pred[,Cq>C0] <- matrix(0,nrow = nrow(Pred),1)



  } else {
    B <- 10^params[1]
    Cq <- 10^params[2]
    n <- 10^params[3]

    C0 <- 10^params[4] #ifelse(is.na(mean(conc[which(flow == 0)], na.rm = TRUE)), mean(conc[which(flow < quantile(flow,0.10, na.rm = TRUE))], na.rm = TRUE),mean(conc[which(flow == 0)], na.rm = TRUE))
    Cd <- C0 - Cq


    # if(Cq>C0){
    #
    #   Pred <- rep(0,length(flow))
    #
    # }else{

    Pred <- Cd/(1 + B * Q^(1/n)) + Cq

    # }
  }
  return(Pred)
}
