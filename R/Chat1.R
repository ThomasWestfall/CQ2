#' Runs simple C-Q model
#'
#' \code{Chat1} get simple CQ model
#'
#' Chat1 runs the simple CQ model: $C=aQ\^b$. This predicts in-stream concentration $C$ based on observed streamflow $Q$ data.
#'
#' @details
#' The input requires observed streamflow data on a daily timescale and two parameters, $a$ and $b$.
#'
#' @param params are two values for setting parameters $a$ and $b$. The parameters can be a single value or a vector. The calibration procedure runs the model in vector form, calling Chat1 with a vector of guesses for each parameter. The bounds and initial guess are set using \code{getBounds}
#' Note: $a = 10^params[1]$ and $b = params[2]$
#'
#' @param flow streamflow dataframe. Vector or array with duplicates in each column for model calibration
#'
#'
#' @return
#' A $Pred$ data frame with predicted concentrations for each time-step. If called in calibration mode, array returned with predictions in each column.
#'
#' @keywords Chat1 simple C-Q
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
