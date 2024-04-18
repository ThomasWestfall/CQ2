#' \code{Chat1} simple CQ model
#'
#' This is a model function. It predicts in-stream concentration.
#'
#' @return
#' A vector of concentration predictions for each time-step
#'
#' @keywords model
#'
#' @export

Chat1 <- function(params, flow) { # # power law between C ~ Q
  #flow = observed stream flow data either as is or in matrix form as duplicates in each column

  if(NCOL(params)>1){
    # First section of IF statement is for calibrating model given a vector of parameters

    a <- 10^params[1,]
    b <- params[2,]

    # sweep is a useful function to apply a column of parameters to a vector of data
    Pred <- sweep(sweep(flow, MARGIN=2,b, `^`), MARGIN=2,a,`*`)

  } else {
    # run model with one set of parameters
    # used with every iteration of the calibration to verify the model above is programmed correctly
    # and for back-calculating results

    a <- 10^params[1]
    b <- params[2]

    Pred <- a * flow^b

  }
  return(Pred)
}
