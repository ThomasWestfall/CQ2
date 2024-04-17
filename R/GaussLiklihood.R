GaussLiklihood <- function(params, conc, Pred, conc.date){

  #conc.date is a matrix with two columns, each row is an index of the start and end of a continuous concentration periods

  if(length(params) >1){
    sig <- params/10
  }else{
    sig <- params/10
  }

  # Only run if more than three days of observations (can be removed since not assuming auto-correlation)
  # re-compute conc.date, if observed days are less than p+2 (2), then remove
  if(any(conc.date[,2] - conc.date[,1] < 3)){
    conc.date <- conc.date[-which(conc.date[,2] - conc.date[,1] < 3),]
  }

  error <- log(conc) - log(Pred)

  #  i.e. sum of squares
  if(length(params)>1) {

    inside_list <- sapply(1:NROW(conc.date), function(i) error[(conc.date[i,1]):(conc.date[i,2]),])
    inside_list <- sapply(1:NROW(conc.date), function(i) '^'(inside_list[[i]],2))
    inside.sum <- t(sapply(1:NROW(conc.date), function(i) colSums(inside_list[[i]])))

  }else{

    inside_list <- sapply(1:NROW(conc.date), function(i) error[(conc.date[i,1]):(conc.date[i,2])])
    inside_list <- sapply(1:NROW(conc.date), function(i) '^'(inside_list[[i]],2))
    inside.sum <- sapply(1:NROW(conc.date), function(i) sum(inside_list[[i]]))
    }

  # neg-loglikelihood equation
  if(length(params) > 1){

    L <- t(sapply(1:NROW(conc.date), function(i) (-length((conc.date[i,1]):(conc.date[i,2]))/2)*log(2*pi*((sig)^2))+(-1/(2*(sig)^2)*(inside.sum[i,]))))

    negLL <- -1*(colSums(L))

  }else{

    L <- sapply(1:NROW(conc.date), function(i) (-length((conc.date[i,1]):(conc.date[i,2]))/2)*log(2*pi*((sig)^2))+(-1/(2*(sig)^2)*(inside.sum[i])))

    negLL <- -1*(sum(L))

  }

  # if any negLL are NA, then set to infinity, remember we are minimizing the negative log-likelihood
  # so the optimizer will steer away from results giving positive infinity
  negLL[is.na(negLL)] = Inf

  # output message if this is the case..
  if(any(negLL == Inf)){
    message(paste(sum(!is.finite(negLL)),"negLL == Inf"))
  }


  return(negLL)
}
