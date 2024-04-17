Chat5 <- function(params, flow, flow.s,flow.date) {


  # calc fast-flow
  flow.q <- flow - flow.s

  if(NCOL(params)>1){

    a <- 10^params[1,]
    bs <- params[2,]
    bf <- params[3,]

    Pred <- sweep(sweep(flow.s,MARGIN=2,bs,`^`),MARGIN = 2, a,`*`) * ifelse(flow.q>0,sweep(flow.q,MARGIN=2, bf,`^`),1)

  }else{

    a <- 10^params[1]
    bs <- params[2]
    bf <- params[3]


    Pred <- a * flow.s^bs * ifelse(flow.q>0,flow.q^bf,1)


  }


  return(Pred)
}
