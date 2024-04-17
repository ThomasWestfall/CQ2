Chat6 <- function(params, flow, flow.s, flow.date) { #quick-slow CQ variant,
  #source load varies as slow flow varies


  # calc quick-flow
  flow.q <- flow - flow.s

  if(NCOL(params)>1){

    b <- params[1,]
    Cs <-  10^params[2,] #Cs is the objectivly calibrated

    Pred <- sweep(flow.s,MARGIN = 2, Cs,'*')*sweep(flow,MARGIN=2,b,`^`)

  }else{

    b <- params[1]
    Cs <-  10^params[2]


    Pred <- (flow.s) * Cs * flow^b


  }


  return(Pred)
}
