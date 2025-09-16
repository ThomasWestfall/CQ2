#' @export interpQ
interpQ <- function(data,period){
  #linerally interoplate streamflow if missing gaps are less than or equal to 5 days
  data$Q_interp = NA

  # Index of flow observations
  flow.date = getIndexQ("ChatModel.homo.normal.linear.baseflow", data)

  for(i in 1:(NROW(flow.date)-1)){

    if(flow.date[i+1,1] - flow.date[i,2] <= period){

      interp_ind = c(flow.date[i,2]:flow.date[i+1,1])

      data$flow[interp_ind] = zoo::na.approx(data$flow[interp_ind])

      data$Q_interp[(interp_ind[1]+1):(interp_ind[length(interp_ind)]-1)] = 1

    }
  }

  return(data)

}
