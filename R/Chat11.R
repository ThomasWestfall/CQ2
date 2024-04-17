Chat11 <- function(params, flow, flow.s, flow.date) { #quick-slow CQ variant,
  #source load varies as slow and quick flow vary
  #two solute responses
  #with hysteresis term

  # calc quick-flow
  flow.q <- flow - flow.s

  if(NCOL(params)>1){

    bs <- params[1,]
    Cq <- 10^params[2,]
    bq <- params[3,]
    Cs <- 10^params[4,]
    c <- params[5,]^5

    #three point difference dQf/dt = (Qf+t - Qf-t)/(2*t) where t = 1
    flow.d <- flow
    flow.d[2:(nrow(flow)-1),] <- (flow[3:nrow(flow),] - flow[1:(nrow(flow)-2),])/(2*1)

    # forward and backward difference
    flow.d[1,] <- (flow[2,] -flow[1,])/1
    flow.d[nrow(flow),] <- (flow[nrow(flow),] - flow[(nrow(flow)-1),])/1

    # forward and backward difference
    flow.d[flow.date[,1],] <- (flow[(flow.date[,1]+1),] -flow[flow.date[,1],])/1
    flow.d[flow.date[,2],] <- (flow[flow.date[,2],] - flow[(flow.date[,2]-1),])/1# if(any(Cq)>Cs)

    Pred <- (sweep(flow.s, MARGIN=2, Cs,`*`) + sweep(flow.q, MARGIN=2, Cq,`*`))*sweep(flow.s,MARGIN=2,bs,`^`)*ifelse(flow.q>0,sweep(flow.q,MARGIN=2,bq,`^`),1) +
      sweep(flow.d,MARGIN=2,c,`*`)



  }else{

    bs <- params[1]
    Cq <- 10^params[2]
    bq <- params[3]
    Cs <- 10^params[4]
    c <- params[5]^5

    #three point difference dQf/dt = (Qf+t - Qf-t)/(2*t) where t = 1
    flow.d <- flow
    flow.d[2:(length(flow)-1)] <- (flow[3:length(flow)] - flow[1:(length(flow)-2)])/(2*1)

    # forward and backward difference
    flow.d[1] <- (flow[2] -flow[1])/1
    flow.d[length(flow)] <- (flow[length(flow)] - flow[(length(flow)-1)])/1

    # forward and backward difference
    flow.d[flow.date[,1]] <- (flow[(flow.date[,1]+1)] -flow[flow.date[,1]])/1
    flow.d[flow.date[,2]] <- (flow[flow.date[,2]] - flow[(flow.date[,2]-1)])/1

    Pred <- (Cs*(flow.s) + Cq*(flow.q))*(flow.s^bs)*(ifelse(flow.q>0,flow.q^bq,1)) + c*flow.d

  }


  return(Pred)
}
