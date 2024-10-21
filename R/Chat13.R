Chat13 <- function(params, flow, flow.s, conc, flow.date) { # Hubbard Brook Working Model with two flow component

  flow.q = flow - flow.s

  if(NCOL(params)>1){
    ys <- 1/(params[1,]/10) # NOTE: "ys" is the inverse of the recession constant from BhatEck (Eckhardt) baseflow filter
    yq <- 10^params[3,]
    L0 <- 10^params[4,] # NOTE: "L0" = "C0*V0", "C0" can be calculated post analysis using "V0" and "LO"
    Cs <- 10^params[5,]
    Cqyq <- 10^params[6,] # NOTE: "Cqyq" are calibrated as one constant, "Cq" can be back-calculated post analysis using "yq"
    V0 <- 10^params[7,]



    Pred <-  sweep(sweep(sweep(flow.s, MARGIN = 2, ys, '*'), MARGIN = 2, Cs, '*') + sweep(flow.q, MARGIN = 2, Cqyq, '*'),MARGIN = 2, L0,'+') /
      (sweep(sweep(flow.s, MARGIN = 2, ys, '*') + sweep(flow.q, MARGIN = 2, yq, '*'),MARGIN=2,V0,'+'))



  } else {

    ys <- 1/(params[1]/10)
    yq <- 10^params[3]
    L0 <- 10^params[4]
    Cs <- 10^params[5]
    Cqyq <- 10^params[6]
    V0 <- 10^params[7]

    Pred <- (L0 + Cs*ys*flow.s + Cqyq*flow.q) / (V0 + ys*flow.s + yq*flow.q)

  }


  return(Pred)
}
