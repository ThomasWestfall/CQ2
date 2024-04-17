Chat12 <- function(params, flow, conc) { # Hubbard Brook Working Model - Johnson (1969)

  if(NCOL(params)>1){
    B <- 10^params[1,]
    Cq <- 10^params[2,]
    C0 <-  10^params[3,]
    Cd <- C0 - Cq

    Pred <- sweep(sweep((1/(1 + sweep(flow, MARGIN=2,B, `*`))),MARGIN = 2, Cd,'*'), MARGIN = 2, Cq, '+')

  } else {
    B <- 10^params[1]
    Cq <- 10^params[2]
    C0 <-  10^params[3]
    Cd <- C0 - Cq

    Pred <-  (Cd)/(1 + B * Q) + Cq

  }
  return(Pred)
}
