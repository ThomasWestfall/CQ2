BhatCMB <- function(flow, conc) { # # # CMB


    # set end members, Cgw is max C when flow is > 0

    Cgw <- max(conc[which(flow> 0)], na.rm = TRUE)
    Csw <- 1.5

    flow.pred = flow*(conc-Csw)/(Cgw-Csw)

  return(flow.pred)
}
