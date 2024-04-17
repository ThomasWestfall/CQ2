Chat2 <- function(params, flow, flow.date) { #C ~ Qs, Qs/Q


  if(NCOL(params)>1){
    # initiate parameters
    a <- 10^params[1,]
    b <- params[2,]
    c <- params[3,]^5

    # allow c to range from larger negative and positive values
    # c <- ifelse(c<0,-1*(10^(-1*c)),10^c)


    #three point difference dQf/dt = (Qf+t - Qf-t)/(2*t) where t = 1
    flow.d <- flow
    flow.d[2:(nrow(flow)-1),] <- (flow[3:nrow(flow),] - flow[1:(nrow(flow)-2),])/(2*1)

    # forward and backward difference
    flow.d[1,] <- (flow[2,] -flow[1,])/1
    flow.d[nrow(flow),] <- (flow[nrow(flow),] - flow[(nrow(flow)-1),])/1

    # forward and backward difference
    flow.d[flow.date[,1],] <- (flow[(flow.date[,1]+1),] -flow[flow.date[,1],])/1
    flow.d[flow.date[,2],] <- (flow[flow.date[,2],] - flow[(flow.date[,2]-1),])/1

    Pred <- sweep(sweep(flow, MARGIN=2,b, `^`), MARGIN = 2,a,'*') +
      sweep(flow.d,MARGIN=2,c,`*`)

    # Pred <- sweep(ifelse(flow == 0,1,sweep(flow, MARGIN=2,b, `^`)), MARGIN = 2,a,'*') +
    #   sweep(flow.d,MARGIN=2,c,`*`)


  }else{
    # initiate parameters
    a <- 10^params[1]
    b <- params[2]
    c <- params[3]^5

    # c <- ifelse(c<0,-1*(10^(-1*c)),10^c)

    flow.d <- flow
    flow.d[2:(length(flow)-1)] <- (flow[3:length(flow)] - flow[1:(length(flow)-2)])/(2*1)
    # forward and backward difference
    flow.d[1] <- (flow[2] -flow[1])/1
    flow.d[length(flow)] <- (flow[length(flow)] - flow[(length(flow)-1)])/1

    # forward and backward difference
    flow.d[flow.date[,1]] <- (flow[(flow.date[,1]+1)] -flow[flow.date[,1]])/1
    flow.d[flow.date[,2]] <- (flow[flow.date[,2]] - flow[(flow.date[,2]-1)])/1

    Pred <-   a * flow^b + c*flow.d

    # Pred <-   a * ifelse(flow==0,1,flow^b) + c*flow.d
  }

  return(Pred)
}
