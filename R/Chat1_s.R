Chat1_s <- function(params, flow, dec.time) { # # power law between C ~ Q

  if(NCOL(params)>1){
    a_amp <- params[1,]^5
    a_phase <- params[2,]
    a_disp <- params[3,]^5

    a <- sweep(sweep(sin(sweep(2*pi*dec.time,MARGIN = 2,a_phase,'+')),MARGIN = 2, a_amp, '*',),MARGIN = 2, a_disp,"+")

    b <- params[4,]

    Pred <- a*sweep(flow, MARGIN=2,b, `^`)

  } else {

    a_amp <- params[1]^5
    a_phase <- params[2]
    a_disp <- params[3]^5

    a <- a_amp*sin(a_phase+2*pi*dec.time)+a_disp

    b <- params[4]

    Pred <- a * flow^b
  }
  return(Pred)
}
