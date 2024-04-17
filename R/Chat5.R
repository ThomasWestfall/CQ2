Chat5 <- function(params, flow, flow.s,flow.date) { #quick-slow CQ of Minaudo et al. (2019)

  # calc quick-flow
  flow.q <- flow - flow.s

  if(NCOL(params)>1){

    a <- 10^params[1,]
    bs <- params[2,]
    bf <- params[3,]

    Pred <- sweep(sweep(flow.s,MARGIN=2,bs,`^`),MARGIN = 2, a,`*`) * ifelse(flow.q>0,sweep(flow.q,MARGIN=2, bf,`^`),1)

    # Note: default quick-flow term to 1 if flow.q = 0 else 0^-bq is undefined

  }else{

    a <- 10^params[1]
    bs <- params[2]
    bf <- params[3]


    Pred <- a * flow.s^bs * ifelse(flow.q>0,flow.q^bf,1)

    # Note: default quick-flow term to 1 if flow.q = 0 else 0^-bq is undefined


  }


  return(Pred)
}
