Chat2.3 <- function(params, flow, flow.s,flow.date) {


  # calc fast-flow
  flow.q <- flow - flow.s

  if(NCOL(params)>1){
    FC <- params[1,]^7
    E <- params[2,]^5
    K <- params[3,]*5
    n <- params[4,]*5

    Pred <-  sweep(sweep(log(sweep(sweep(flow, MARGIN = 2, 1/n, '^'), MARGIN = 2, K^(-1/n), '*')),MARGIN = 2,E, '*'), MARGIN = 2, FC, '-')

  } else {
    FC <- params[1]^7
    E <- params[2]^5
    K <- params[3]*5
    n <- params[4]*5

    Pred <- FC - E*log(Q^(1/n)*K^(-1/n))

  }


  return(Pred)
}
