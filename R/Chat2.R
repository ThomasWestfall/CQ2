Chat2 <- function(params, flow, flow.date) { # simple C-Q with hysteresis term, explanations, see: Krueger et al. 2009; Minaudo et al. 2017; Musolff et al. 2021)

  #flow.date is a matrix with two columns, each row is an index of the start and end of a continuous flow period

  if(NCOL(params)>1){
    # initiate parameters
    a <- 10^params[1,]
    b <- params[2,]
    c <- params[3,]^5

    #three point difference dQf/dt = (Qf+t - Qf-t)/(2*t) where t = 1
    flow.d <- flow
    flow.d[2:(nrow(flow)-1),] <- (flow[3:nrow(flow),] - flow[1:(nrow(flow)-2),])/(2*1)

    # forward and backward difference for start and end of record
    flow.d[1,] <- (flow[2,] -flow[1,])/1
    flow.d[nrow(flow),] <- (flow[nrow(flow),] - flow[(nrow(flow)-1),])/1

    # forward and backward difference for start and end of continuous periods
    flow.d[flow.date[,1],] <- (flow[(flow.date[,1]+1),] -flow[flow.date[,1],])/1
    flow.d[flow.date[,2],] <- (flow[flow.date[,2],] - flow[(flow.date[,2]-1),])/1

    Pred <- sweep(sweep(flow, MARGIN=2,b, `^`), MARGIN = 2,a,'*') +
      sweep(flow.d,MARGIN=2,c,`*`)

  }else{
    # initiate parameters
    a <- 10^params[1]
    b <- params[2]
    c <- params[3]^5

    #three point difference dQf/dt = (Qf+t - Qf-t)/(2*t) where t = 1
    flow.d <- flow
    flow.d[2:(length(flow)-1)] <- (flow[3:length(flow)] - flow[1:(length(flow)-2)])/(2*1)

    # forward and backward difference for start and end of record
    flow.d[1] <- (flow[2] -flow[1])/1
    flow.d[length(flow)] <- (flow[length(flow)] - flow[(length(flow)-1)])/1

    # forward and backward difference for start and end of continuous periods
    flow.d[flow.date[,1]] <- (flow[(flow.date[,1]+1)] -flow[flow.date[,1]])/1
    flow.d[flow.date[,2]] <- (flow[flow.date[,2]] - flow[(flow.date[,2]-1)])/1

    Pred <-   a * flow^b + c*flow.d

  }

  return(Pred)
}
