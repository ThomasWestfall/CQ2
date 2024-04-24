Chat6_hys <- function(params, flow, flow.s, flow.date, conc) { #C ~ Qs, Qs/Q

  # calc fast-flow
  flow.q <- flow - flow.s

  if(NCOL(params)>1){
    # initiate parameters
    as <- (params[1,])^5
    bs <- params[2,]

    #three point difference dQf/dt = (Qf+t - Qf-t)/(2*t) where t = 1
    flow.q.d <- flow.q
    flow.q.d[2:(nrow(flow.q)-1),] <- (flow.q[3:nrow(flow.q),] - flow.q[1:(nrow(flow.q)-2),])/(2*1)
    flow.q.d[1,] <- 0
    flow.q.d[nrow(flow.q),] <- 0

    # make first and last delta 0 since NAs from previous gap in the subtraction create NAs on first time step
    flow.q.d[flow.date[,1],] <- 0
    flow.q.d[flow.date[,2],] <- 0

    Pred <- (conc - sweep(sweep(flow.s, MARGIN=2,bs, `^`), MARGIN=2,as,`*`))/flow.q.d


  }else{
    # initiate parameters
    as <- (params[1])^5
    bs <- params[2]

    flow.q.d <- flow.q
    flow.q.d[2:(length(flow.q)-1)] <- (flow.q[3:length(flow.q)] - flow.q[1:(length(flow.q)-2)])/(2*1)
    flow.q.d[1] <- 0
    flow.q.d[length(flow.q)] <- 0

    # make first delta 0 since NAs from previous gap in the subtraction create NAs on first time step
    flow.q.d[flow.date[,1]] <- 0
    flow.q.d[flow.date[,2]] <- 0

    Pred <- (conc - (as)*(flow.s)^bs)/flow.q.d

  }

  return(Pred)
}
