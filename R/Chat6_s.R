Chat6_s <- function(params, flow, flow.s, flow.date, dec.time) { #C ~ Qs, Qs/Q

  # calc fast-flow
  flow.q <- flow - flow.s

  if(NCOL(params)>1){
    # initiate parameters
    # a - as sin function
    as_amp <- params[1,]^5
    as_phase <- params[2,]
    as_disp <- params[3,]^5

    as <- sweep(sweep(sin(sweep(2*pi*dec.time,MARGIN = 2,as_phase,'+')),MARGIN = 2, as_amp, '*',),MARGIN = 2, as_disp,"+")

    # b is just b
    bs <- params[4,]

    # c - as sin function
    # c_amp <- params[5,]^5
    # c_phase <- params[6,]
    # c_disp <- params[7,]^5

    c <- params[5,]^5 #sweep(sweep(sin(sweep(2*pi*dec.time,MARGIN = 2,c_phase,'+')),MARGIN = 2, c_amp, '*',),MARGIN = 2, c_disp,"+")

    #three point difference dQf/dt = (Qf+t - Qf-t)/(2*t) where t = 1
    flow.q.d <- flow.q
    flow.q.d[2:(nrow(flow.q)-1),] <- (flow.q[3:nrow(flow.q),] - flow.q[1:(nrow(flow.q)-2),])/(2*1)
    flow.q.d[1,] <- 0
    flow.q.d[nrow(flow.q),] <- 0

    # make first and last delta 0 since NAs from previous gap in the subtraction create NAs on first time step
    flow.q.d[flow.date[,1],] <- 0
    flow.q.d[flow.date[,2],] <- 0

    Pred <- as*sweep(flow.s, MARGIN=2,bs, `^`) +  flow.q.d*c

  }else{
    # initiate parameters
    # a - as sin function
    as_amp <- params[1]^5
    as_phase <- params[2]
    as_disp <- params[3]^5

    as <- as_amp*sin(as_phase+2*pi*dec.time)+as_disp

    # b is just b
    bs <- params[4]

    # c - as sin function
    # c_amp <- params[5]^5
    # c_phase <- params[6]
    # c_disp <- params[7]^5

    c <- params[5]^5 #c_amp*sin(c_phase+2*pi*dec.time)+c_disp

    #three point difference dQf/dt = (Qf+t - Qf-t)/(2*t) where t = 1
    flow.q.d <- flow.q
    flow.q.d[2:(length(flow.q)-1)] <- (flow.q[3:length(flow.q)] - flow.q[1:(length(flow.q)-2)])/(2*1)
    flow.q.d[1] <- 0
    flow.q.d[length(flow.q)] <- 0

    # make first delta 0 since NAs from previous gap in the subtraction create NAs on first time step
    flow.q.d[flow.date[,1]] <- 0
    flow.q.d[flow.date[,2]] <- 0

    Pred <-   as*(flow.s)^bs + c*flow.q.d
  }

  return(Pred)
}
