Chat14 <- function(params, flow, conc) { # # Hall (1970) "model 5"

  if(NCOL(params)>1){
    B <- 10^params[1,]
    Cq <- 10^params[2,]
    n <- 10^params[3,]
    C0 <- 10^params[4,]
    Cd <- C0 - Cq

    Pred <- sweep(sweep((1/(1 + sweep(sweep(flow,MARGIN=2, (1/n),'^'), MARGIN=2,B, `*`))),MARGIN = 2, Cd,'*'), MARGIN = 2, Cq, '+')

  } else {

    B <- 10^params[1]
    Cq <- 10^params[2]
    n <- 10^params[3]
    C0 <- 10^params[4]
    Cd <- C0 - Cq

    Pred <- Cd/(1 + B * Q^(1/n)) + Cq

  }
  return(Pred)
}
