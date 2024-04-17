BhatEck <- function(params, flow, flow.date) { # # # Su (2016)/ Eckhardt (2005) baseflow


  flow.pred = flow

    A <- params[1]/10
    BFI <- params[2]/10

    for(j in 1:nrow(flow.date)){

      #take difference by index for next for loop
      diff_d <- flow.date[j,2]-flow.date[j,1]
      start_ind <- flow.date[j,1]
      end_ind <- start_ind + diff_d


      flow.pred[start_ind] <- flow[start_ind]
      # set starting values, each start begins at previous base flow value or flow of start
      # if(j >1){
      # if(flow[start_ind] == 0){
      #   flow.pred[start_ind] <- 0
      # }else{
      #   flow.pred[start_ind] <- flow[start_ind]/1.5
      # }
      # mquantile(flow[start_ind:end_ind],0.33, na.rm = TRUE)
      #   flow.pred[start_ind] <- max(ifelse(min(flow[start_ind:end_ind], na.rm=TRUE) == 0,flow[start_ind ifelse(quantile(flow[start_ind:end_ind],0.33, na.rm = TRUE) == 0, flow[start_ind]/1.5)
      # }else{
        # flow.pred[start_ind] <- ifelse(flow[start_ind] == 0, 0, flow[start_ind]/1.5) #max(min(flow,na.rm=TRUE),quantile(flow,0.10,na.rm = TRUE)) #min(flow[start_ind:end_ind], na.rm = TRUE)  # assume first timestep of baseflow is minimum of total flow
      # }

      if(length(start_ind:end_ind) > 1){ # only enter if more than one day of data
        for(i in (start_ind+1):end_ind){
          #Eck filter
          flow.pred[i] <- min(flow[i], (((A*(1-BFI))/(1-A*BFI))*(flow.pred[i-1]) + (((1-A)*BFI)/(1-A*BFI))*flow[i]))

        }
      }
    }
  # }
  return(flow.pred)
}
