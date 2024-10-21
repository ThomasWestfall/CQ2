objective_function_factory <- function(flow, conc, flow.date, conc.date, dec.time, cl, Chat.model.names, Bhat.model.name, Likelihood.name) {

  force(flow)
  force(conc)
  force(flow.date)
  force(conc.date)
  force(dec.time)
  force(cl)
  force(Chat.model.names)
  force(Bhat.model.name)
  force(Likelihood.name)

  objective_function <- function(params){

  #make params a matrix
  params = as.matrix(params)

  #get predicted Chat
  if (NCOL(params)>1) {

    parlen <- nrow(params)

    # # # export parameters 1 and 2 into a 6 list for each core, keep them in a column
    splt = rep(1:length(cl), each = ceiling(ncol(params)/length(cl)), length.out = ncol(params))
    newgrid = lapply(as.list(1:length(cl)), function(w) (params[1:parlen, splt == w]))

    # # # exported the number of simulations required for each core, each list in newgrid
    sims_perCore = as.data.frame(table(splt))
    sims_perCore = sims_perCore$Freq

    ########## single flow component models

    if(noquote(Chat.model.names) %in%  c("C1")){

      parallel::clusterExport(cl,c("flow","sims_perCore","newgrid",noquote("Chat1")),envir = environment())

      Pred <- parallel::parLapply(cl, 1:length(cl), function(u) Chat1(params = newgrid[[u]][1:parlen,], matrix(flow,length(flow),ncol= sims_perCore[u])))

    }else if(noquote(Chat.model.names) %in% c("C12")){

      parallel::clusterExport(cl,c("flow","conc", "sims_perCore","newgrid","flow.date",noquote("Chat12")),envir = environment())

      Pred <- parallel::parLapply(cl, 1:length(cl), function(u) Chat12(params = newgrid[[u]][1:parlen,], matrix(flow,length(flow),ncol= sims_perCore[u]),conc))

    }else if(noquote(Chat.model.names) == "C14"){

      parallel::clusterExport(cl,c("flow","conc", "sims_perCore","newgrid","flow.date",noquote("Chat14")),envir = environment())

      Pred <- parallel::parLapply(cl, 1:length(cl), function(u) Chat14(params = newgrid[[u]][1:parlen,], matrix(flow,length(flow),ncol= sims_perCore[u]),conc))

    }else if(noquote(Chat.model.names) == "C2"){

      parallel::clusterExport(cl,c("flow","sims_perCore","newgrid","flow.date",noquote("Chat2")),envir = environment())

      Pred <- parallel::parLapply(cl, 1:length(cl), function(u) Chat2(params = newgrid[[u]][1:parlen,], matrix(flow,length(flow),ncol= sims_perCore[u]),flow.date))

    }else if(noquote(Chat.model.names) == "C1_s"){

      parallel::clusterExport(cl,c("flow","sims_perCore","newgrid","dec.time",noquote("Chat1_s")),envir = environment())

      Pred <- parallel::parLapply(cl, 1:length(cl), function(u) Chat1_s(params = newgrid[[u]][1:parlen,], matrix(flow,length(flow),ncol= sims_perCore[u]),dec.time))

      ########## all other Chat models with multiple flow components

    }else if(noquote(Chat.model.names) %in%  c("C3","C4", "C5","C6","C7","C8","C9","C10","C11","C15")){

      temp.name = sub('.','Chat',Chat.model.names)

      parallel::clusterExport(cl,c("flow","flow.date","sims_perCore","newgrid",noquote(Bhat.model.name)),envir = environment())

      #two lapply functions to cycle through the numeric parameters in the list of cores
      flow.s <- parallel::parLapply(cl, 1:length(cl), function(u) lapply(1:sims_perCore[u], function(k) match.fun(Bhat.model.name)(params = c(newgrid[[u]][[1,k]], newgrid[[u]][[2,k]]), flow, flow.date))) #assign params in Bhat

      parallel::clusterExport(cl,c("flow","flow.s","flow.date","sims_perCore","newgrid",noquote(temp.name)),envir = environment())

      Pred <- parallel::parLapply(cl, 1:length(cl), function(u) match.fun(temp.name)(params = newgrid[[u]][3:parlen,], matrix(flow,length(flow),ncol= sims_perCore[u]), matrix(unlist(flow.s[u]),ncol=sims_perCore[u]),flow.date))

    }else if(noquote(Chat.model.names) == "C13"){ #Chat 13 is unique because we are using the recession constant from Bhat as a parameter in Chat...

      parallel::clusterExport(cl,c("flow","flow.date","sims_perCore","newgrid",noquote(Bhat.model.name)),envir = environment())

      flow.s <- parallel::parLapply(cl, 1:length(cl), function(u) lapply(1:sims_perCore[u], function(k) match.fun(Bhat.model.name)(params = c(newgrid[[u]][[1,k]], newgrid[[u]][[2,k]]), flow, flow.date)))

      parallel::clusterExport(cl,c("flow","flow.s","conc","flow.date","sims_perCore","newgrid",noquote("Chat13")),envir = environment())

      Pred <- parallel::parLapply(cl, 1:length(cl), function(u) Chat13(params = newgrid[[u]][1:parlen,], matrix(flow,length(flow),ncol= sims_perCore[u]), matrix(unlist(flow.s[u]),ncol=sims_perCore[u]),conc,flow.date))

    }else if(endsWith(noquote(Chat.model.names), '_s')){  # pass dec.time to two flow component chat models with seasonal funciton

      parallel::clusterExport(cl,c("flow","flow.date","sims_perCore","newgrid",noquote(Bhat.model.name)),envir = environment())

      flow.s <- parallel::parLapply(cl, 1:length(cl), function(u) lapply(1:sims_perCore[u], function(k) match.fun(Bhat.model.name)(params = c(newgrid[[u]][[1,k]], newgrid[[u]][[2,k]]), flow, flow.date)))

      parallel::clusterExport(cl,c("flow","flow.s","flow.date","sims_perCore","newgrid","dec.time",noquote(Chat.model.names)),envir = environment())

      Pred <- parallel::parLapply(cl, 1:length(cl), function(u) match.fun(Chat.model.names)(params = newgrid[[u]][3:parlen,], matrix(flow,length(flow),ncol= sims_perCore[u]), matrix(unlist(flow.s[u]),ncol=sims_perCore[u]),flow.date,dec.time))

    }

  }else{
    parlen <- length(params)

    if(noquote(Chat.model.names) %in%  c("C1")){

      Pred = Chat1(params = params[1:parlen], flow)

    }else if(noquote(Chat.model.names) %in%  c("C12")){

      Pred = Chat12(params = params[1:parlen], flow, conc)

    }else if(noquote(Chat.model.names) %in%  c("C14")){

      Pred = Chat14(params = params[1:parlen], flow, conc)

    }else if(noquote(Chat.model.names) == "C2"){

      Pred = Chat2(params = params[1:parlen], flow, flow.date)

    }else if(noquote(Chat.model.names) == "C1_s"){

      Pred = Chat1_s(params = params[1:parlen], flow, dec.time)

    }else if(noquote(Chat.model.names) %in%  c("C3","C4", "C5","C6","C7","C8","C9","C10","C11","C15")){

      temp.name = sub('.','Chat',Chat.model.names)

      flow.s = match.fun(Bhat.model.name)(params = params[1:2],flow, flow.date)
      Pred = match.fun(temp.name)(params = params[3:parlen], flow, flow.s, flow.date)

    }else if(noquote(Chat.model.names) == "C13"){
      flow.s = match.fun(Bhat.model.name)(params = params[1:2],flow, flow.date)
      Pred = Chat13(params = params[1:parlen], flow, flow.s,conc, flow.date)
    }

  }

  #negLL
  if (NCOL(params)>1) {

    if(noquote(Likelihood.name) == "GaussLiklihood"){
      parallel::clusterExport(cl,c("conc","Pred",noquote(Likelihood.name),"newgrid","conc.date"),envir = environment())
      #get neg log liklihood
      negLL <- parallel::parLapply(cl, 1:length(cl), function(v) match.fun(Likelihood.name)(params = newgrid[[v]][parlen,], matrix(conc, NROW(conc), ncol= sims_perCore[v]), matrix(unlist(Pred[v]), ncol = sims_perCore[v]),conc.date))
      negLL <- matrix(unlist(negLL), ncol = ncol(params))

    }else if(noquote(Likelihood.name) == "GaussLiklihoodAR1"){
      parallel::clusterExport(cl,c("conc","Pred",noquote(Likelihood.name),"newgrid","conc.date"),envir = environment())
      #get neg log liklihood
      negLL <- parallel::parLapply(cl, 1:length(cl), function(v) match.fun(Likelihood.name)(params = newgrid[[v]][(parlen-1):parlen,], matrix(conc, NROW(conc), ncol= sims_perCore[v]), matrix(unlist(Pred[v]), ncol = sims_perCore[v]),conc.date))
      negLL <- matrix(unlist(negLL), ncol = ncol(params))

    }else if(noquote(Likelihood.name) == "GaussLiklihoodBV1"){
      parallel::clusterExport(cl,c("conc","Pred",noquote(Likelihood.name),"newgrid","conc.date"),envir = environment())
      #get neg log liklihood
      negLL <- parallel::parLapply(cl, 1:length(cl), function(v) match.fun(Likelihood.name)(params = newgrid[[v]][(parlen-1):parlen,], matrix(conc, NROW(conc), ncol= sims_perCore[v]), matrix(unlist(Pred[v]), ncol = sims_perCore[v]),conc.date))
      negLL <- matrix(unlist(negLL), ncol = ncol(params))

    }else if(noquote(Likelihood.name) == "GaussLiklihoodAR3"){
      parallel::clusterExport(cl,c("conc","Pred",noquote(Likelihood.name),"Covmatrix","newgrid","conc.date"),envir = environment())
      #get neg log liklihood
      negLL <- parallel::parLapply(cl, 1:length(cl), function(v) match.fun(Likelihood.name)(params = newgrid[[v]][(parlen-3):parlen,], matrix(conc, NROW(conc), ncol= sims_perCore[v]), matrix(unlist(Pred[v]), ncol = sims_perCore[v]),conc.date))
      negLL <- matrix(unlist(negLL), ncol = ncol(params))
    }

  } else {

    if(noquote(Likelihood.name) == "GaussLiklihood"){
      #get neg log liklihood
      negLL <- match.fun(Likelihood.name)(params = params[parlen,], conc, Pred,conc.date)

    }else if(noquote(Likelihood.name) == "GaussLiklihoodAR1"){
      #get neg log liklihood
      negLL <- match.fun(Likelihood.name)(params = params[(parlen-1):parlen,], conc, Pred,conc.date)

    }else if(noquote(Likelihood.name) == "GaussLiklihoodBV1"){
      #get neg log liklihood
      negLL <- match.fun(Likelihood.name)(params = params[(parlen-1):parlen,], conc, Pred,conc.date)

    }else if(noquote(Likelihood.name) == "GaussLiklihoodAR3"){
      #get neg log liklihood
      negLL <- match.fun(Likelihood.name)(params = params[(parlen-3):parlen,], conc, Pred,conc.date)
    }
  }

  ##  Check to make sure baseflow does not get stuck at lower and upper bounds
  if (NCOL(params)>1) {
    if(noquote(Chat.model.names) %nin%  c("C1","C1_s", "C2", "C12", "C14")){ # do not run on one-flow component models

      # BFI <- flow.s/flow

      BFI <- lapply(1:length(cl), function(i) colSums(matrix(unlist(flow.s[i]),ncol=sims_perCore[i]),na.rm = TRUE)/sum(flow, na.rm = TRUE))

      BFI <- unlist(BFI[1:length(cl)])

      negLL <- ifelse((BFI < 0.05 | BFI > 0.95),Inf,negLL)

      if(length(BFI)-sum(BFI < 0.05 | BFI > 0.95) > 0){
        # message(paste(length(BFI)-sum(BFI < 0.05 | BFI > 0.95),"negLL == Inf because BFI is < 0.05 or BFI > 0.95"))
      }

    }

  }else{

    if(noquote(Chat.model.names) %nin%  c("C1","C1_s", "C2", "C12", "C14")){ # do not run on one-flow component models

      BFI <- sum(flow.s, na.rm = TRUE)/sum(flow, na.rm = TRUE)

      negLL <- ifelse((BFI < 0.05 | BFI > 0.95)==TRUE,Inf,negLL)
    }

  }

  # Check to make sure Cs > Cq in Chat 8 and Chat 9 ... if not, return Infinite negLL
  if(noquote(Chat.model.names) %in% c("C8","C9")){
    if (NCOL(params)>1) {

      negLL[which(params[4,]>params[5,])] = Inf

    }else{
      negLL <- ifelse(params[4]>params[5], Inf, negLL)
    }
  }

  # Check to make sure Cs > Cq in Chat 10 and Chat11 ... if not, return Infinite negLL
  if(noquote(Chat.model.names) %in% c("C10","C11")){
    if (NCOL(params)>1) {

      negLL[which(params[4,]>params[6,])] = Inf

    }else{
      negLL <- ifelse(params[4]>params[6], Inf, negLL)
    }
  }

  # Check to make sure Cs > Cq in Chat 13 ... if not, return Infinite negLL
  if(noquote(Chat.model.names) == "C13"){
    if (NCOL(params)>1) {

      negLL[which((10^params[6,]/10^params[3,])>(10^params[5,]))] = Inf

    }else{
      negLL <- ifelse((10^params[6]/10^params[3])>(10^params[5]), Inf, negLL)
    }
  }

  # Check to make sure Cs > Cq in Chat 15
  if(noquote(Chat.model.names) == "C15"){
    if (NCOL(params)>1) {

      negLL[which((10^params[7,]/10^params[4,])>(10^params[6,]/10^params[3,]))] = Inf

    }else{
      negLL <- ifelse((10^params[7]/10^params[4])>(10^params[6]/10^params[3]), Inf, negLL)
    }
  }

  # Check to make sure vectorized parallel Chat is working by running with parameter value
  if(NCOL(params)>1){

    params = as.matrix(params[,27])

    # force(params)

    negLL.rerun <- objective_function(params)

    # if(isFALSE(all.equal(negLL[27],negLL.rerun))){
    if(isFALSE(negLL[27] == negLL.rerun)){
      # message(paste("negLL[27] does not equal re-run of params[,27] = ",negLL[27],"  ",negLL.rerun))
    }
  }

    return(negLL)

  }
  return(objective_function)
}
