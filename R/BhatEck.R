#' @export BhatEck
BhatEck <- function(params, flow, flow.date) { # # # Su (2016)/ Eckhardt (2005) baseflow


  flow.pred = flow

    A <- params[1]/10 # recession constant
    BFI <- params[2]/10

    for(j in 1:nrow(flow.date)){

      # Get continuous periods
      # take difference by index for next for loop
      diff_d <- flow.date[j,2]-flow.date[j,1]
      start_ind <- flow.date[j,1]
      end_ind <- start_ind + diff_d

      # initiate start values of each continuous period with stream flow
      flow.pred[start_ind] <- flow[start_ind]*0.5

      # only run baseflow filter if more than one day of streamflow
      if(length(start_ind:end_ind) > 1){
        for(i in (start_ind+1):end_ind){
          #Eck filter
          flow.pred[i] <- min(flow[i], (((A*(1-BFI))/(1-A*BFI))*(flow.pred[i-1]) + (((1-A)*BFI)/(1-A*BFI))*flow[i]))

        }
      }
    }

  return(flow.pred)
}
