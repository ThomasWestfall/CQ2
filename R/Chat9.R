Chat9 <- function(params, flow, flow.s, flow.date) {


  # calc fast-flow
  flow.q <- flow - flow.s

  if(NCOL(params)>1){

    b <- params[1,]
    Cq <- 10^params[2,]
    Cs <- 10^params[3,]
    c <- params[4,]^5

    #three point difference dQf/dt = (Qf+t - Qf-t)/(2*t) where t = 1
    flow.d <- flow
    flow.d[2:(nrow(flow)-1),] <- (flow[3:nrow(flow),] - flow[1:(nrow(flow)-2),])/(2*1)

    # forward and backward difference
    flow.d[1,] <- (flow[2,] -flow[1,])/1
    flow.d[nrow(flow),] <- (flow[nrow(flow),] - flow[(nrow(flow)-1),])/1

    # forward and backward difference
    flow.d[flow.date[,1],] <- (flow[(flow.date[,1]+1),] -flow[flow.date[,1],])/1
    flow.d[flow.date[,2],] <- (flow[flow.date[,2],] - flow[(flow.date[,2]-1),])/1# if(any(Cq)>Cs)
    # Pred[,Cq>Cs] <- matrix(0,nrow = nrow(Pred),1)

    Pred <- (sweep(flow.s, MARGIN=2, Cs,`*`) + sweep(flow.q, MARGIN=2, Cq,`*`))*sweep(flow,MARGIN=2,b,`^`) +
      sweep(flow.d,MARGIN=2,c,`*`)



  }else{

    b <- params[1]
    Cq <- 10^params[2]
    Cs <- 10^params[3]
    c <- params[4]^5


    flow.d <- flow
    flow.d[2:(length(flow)-1)] <- (flow[3:length(flow)] - flow[1:(length(flow)-2)])/(2*1)
    # forward and backward difference
    flow.d[1] <- (flow[2] -flow[1])/1
    flow.d[length(flow)] <- (flow[length(flow)] - flow[(length(flow)-1)])/1

    # forward and backward difference
    flow.d[flow.date[,1]] <- (flow[(flow.date[,1]+1)] -flow[flow.date[,1]])/1
    flow.d[flow.date[,2]] <- (flow[flow.date[,2]] - flow[(flow.date[,2]-1)])/1

      Pred <- (Cs*(flow.s) + Cq*(flow.q))*(flow^b) + c*flow.d




  }


  return(Pred)
}
