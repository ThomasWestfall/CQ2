Chat3 <- function(params, flow, flow.s, flow.date) { #C ~ Qs, Qf Gunnerson

  # calc fast-flow
  flow.q <- flow - flow.s

  if(NCOL(params)>1){
    # initiate parameters
    as <- 10^params[1,]
    bs <- params[2,]
    aq <- 10^params[3,]
    bq <- params[4,]

    Pred <- sweep(sweep(flow.s, MARGIN=2,bs, `^`), MARGIN=2,as,`*`) +
      ifelse(flow.q>0,sweep(sweep(flow.q, MARGIN=2,bq, `^`), MARGIN=2,aq,`*`),0)

    # Pred <- sweep(ifelse(flow.s ==0,1,sweep(flow.s, MARGIN=2,bs, `^`)), MARGIN=2,as,`*`) +
    #   ifelse(flow.q>0,sweep(sweep(flow.q, MARGIN=2,bq, `^`), MARGIN=2,aq,`*`),0)

  }else{
    as <- 10^params[1]
    bs <- params[2]
    aq <- 10^params[3]
    bq <- params[4]

    Pred <- (as)*(flow.s)^bs + ifelse(flow.q>0, (aq)*(flow.q)^bq, 0)
    # Pred <- as * ifelse(flow.s == 0, 1,flow.s^bs) + ifelse(flow.q>0, aq*flow.q^bq, 0)
    #
  }

  return(Pred)
}
