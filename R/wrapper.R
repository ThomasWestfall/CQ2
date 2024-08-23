#' Set models
#'
#' \code{setModels}
#'
#' @description
#' Set-up C-Q models for comparison
#'
#' @details
#' Sets-up C-Q models from the selection of Chat1-15. Default sets-up simple C-Q model (Chat1) and quick-slow Hubbard Brook model (Chat13)
#'
#' @param Chat.model.names character string vector with a 'Chat#' model name from provided models (i.e. Chat1-Chat15). Chat1 and Chat13 default
#' @param input.data dataframe of daily runoff and concentration. colnames = c("year", "month", "day", "C", "flow_mm_d")
#' @param Qthresh numeric low-flow streamflow threshold, models only fitted to observations with same day streamflow above this threshold.
#' @param Likelihood.name character string with name of likelihood function ("GaussLiklihood","GaussLiklihoodAR1", or "GaussLiklihoodAR3")
#' @param site.id character string with identifier of gauge or catchment
#' @param site.name character string with name of gauge or catchment
#'
#' @return
#' C-Q models ready for runModels
#'
#' @keywords setModels
#'
#'
#' @export setModels
#'

setModels <- function(Chat.model.names = c('Chat1','Chat13'),
                      input.data = data_all,
                      Qthresh = 0,
                      Likelihood.name = "GaussLiklihood",
                      site.id = '',
                      site.name = ''){

  if(is.null(input.data)){stop('please provide a data.frame of input.data')}

  if(is.null(Qthresh)){

    Qthresh = 0

    message('Lower Qthresh is assumed 0')
  }

  if(is.null(Likelihood.name)){

    Likelihood.name = "GaussLiklihood"

    message('Gaussian Likelihood function selected')
  }

  if(is.null(Chat.model.names) & !is.null(input.data) & !is.null(Qthresh)  & !is.null(Likelihood.name)){

    Chat.model.names = c('Chat1','Chat13')

  }


  # Initiate variables
  T.dtime = NA
  year = data_all$year
  month = data_all$month
  day = data_all$day
  C = data_all$C
  Q = data_all$Q

  # only run likelihood for observed C values that are finite and above Q threshold
  indC <- !is.na(C)
  indQ <- (!is.na(Q))&(Q>=Qthresh)
  ind <- ifelse(indC + indQ == 2, TRUE,FALSE) #(TRUE + TRUE = 2)

  # get index of start and end periods in two columns
  deltaC = getStartEndIndex(ind,0)
  rm(ind)

  # only run baseflow filter for observed Q greater than zero on continuous periods greater than or equal to 3 days
  indQ <- (!is.na(Q))&(Q>0)

  # get index of start and end periods in two columns
  deltaQ = getStartEndIndex(indQ,3)

  # insert name of base flow filter function, only choose 1
  Bhat.model.name <- "BhatEck"

  if(Likelihood.name %nin% c("GaussLiklihood","GaussLiklihoodAR1","GaussLiklihoodAR3")){
    stop('Likelihood.name is not one of the provided likelihood functions')
  }

  # list data
  model.setup = list(reference.data = list(year = year, month = month, day = day,  C = C, Q = Q, T.dtime = T.dtime, deltaC = deltaC, deltaQ = deltaQ), models = list(Chat.model.names = Chat.model.names, Bhat.model.name = Bhat.model.name, Likelihood.name = Likelihood.name), site.info = list(site.id = site.id, site.name = site.name))

  return(model.setup)
}


#' Run models
#'
#' \code{runModels}
#'
#' @description
#' Run C-Q models for comparison
#'
#' @details
#' Runs C-Q models from the selection of Chat1-15. Default runs simple C-Q model (Chat1) and quick-slow Hubbard Brook model (Chat13)
#'
#' @param model.setup lists of details about data, model, and site from setModels()
#'
#' @return
#' fitted C-Q models
#'
#' @keywords runModels
#'
#'
#' @export runModels
#'

runModels <- function(model.setup = list()){

    # list of results for each model
    cmaes.results = list()
    # for loop for each model
    for(k in 1:length(model.setup$models$Chat.model.names)){

      # check models to run
      if(model.setup$models$Chat.model.names[k] %nin%  c("Chat1","Chat2","Chat3","Chat4","Chat5","Chat6","Chat7","Chat8","Chat9","Chat10","Chat11","Chat12","Chat13","Chat14","Chat15")){
        stop(model.setup$models$Chat.model.names[k], ' is not one of the provided Chat models')
      }


      # Prepare to calibrate
      # edit getBounds to include lower/upper bounds and start values
      # insert start value and bounds of each Chat model
      boundsList <- getBounds(model.setup$models$Chat.model.names[k], model.setup$models$Bhat.model.name, model.setup$models$Likelihood.name)

      # assign CMAES Variables
      lamda.cmaes <- 8^3 # lamda must be greater or equal to double the number of cores
      sigma.cmaes <- 3.33
      Nrestart <- 2
      restart.multi <- 2

      #start cluster
      nclus = parallel::detectCores()/2
      clus <- parallel::makeCluster(nclus)

      # Create objective functions
      objective_function_to_run <- smoof_objective_function(objective_function_factory(model.setup$reference.data$Q, model.setup$reference.data$C, model.setup$reference.data$deltaQ, model.setup$reference.data$deltaC, model.setup$reference.data$T.dtime, clus, model.setup$models$Chat.model.names[k], model.setup$models$Bhat.model.name, model.setup$models$Likelihood.name), model.setup$models$Chat.model.names[k], boundsList)

      # RUN CMAES
      cmaes.results[[k]] <- RunCMAES(sigma.cmaes, lamda.cmaes, Nrestart, restart.multi, objective_function_to_run, model.setup$models$Chat.model.names[k], boundsList)

      #stop cluster after model runs
      parallel::stopCluster(clus)

      print(paste(model.setup$models$Chat.model.names[k]," is complete.",sep=""))

    }

    names(cmaes.results) = model.setup$models$Chat.model.names

    return(cmaes.results)

}


#' Get Results
#'
#' \code{getResults}
#'
#' @description
#' getResults of fitted C-Q models
#'
#' @details
#' exported predicted concentration and baseflow of fitted models
#'
#' @param cmaes.results list of cmaes.results from fitted models
#' @param model.setup lists of details about data, model, and site from setModels()
#'
#'
#' @return
#' output original data_all dataframe with predicted concentration and baseflow of fitted models
#'
#' @keywords getResults
#'
#'
#' @export getResults
#'



getResults<- function(cmaes.results = list(), model.setup = model.setup){


  deltaC = model.setup$reference.data$deltaC
  deltaQ = model.setup$reference.data$deltaQ

  C = model.setup$reference.data$C
  Q = model.setup$reference.data$Q

  data_all = data.frame(cbind(model.setup$reference.data$year, model.setup$reference.data$month, model.setup$reference.data$day, model.setup$reference.data$C, model.setup$reference.data$Q))

  names(data_all) = c("year", "month", "day", "C", "Q")

  #Only run output on data that was used to fit the model, (avoid running when Q < Qthresh)
  indC.deltaC <-sapply(1:NROW(deltaC), function(i) deltaC[i,1]:deltaC[i,2])
  indC = rep(FALSE,NROW(C))
  indC[unlist(indC.deltaC)] <- TRUE

  indQ.deltaQ <-sapply(1:NROW(deltaQ), function(i) deltaQ[i,1]:deltaQ[i,2])
  indQ = rep(FALSE,NROW(Q))
  indQ[unlist(indQ.deltaQ)] <- TRUE

  # for loop for each model
  for(k in 1:length(model.setup$models$Chat.model.names)){

    # short name of models to run
    models <- sub('....', '', model.setup$models$Chat.model.names[k])

    # simulated base flow and concentration, and error
    if(model.setup$models$Chat.model.names[k] == "Chat1"){
      data_all[[paste("C",models,sep="")]][indC] <- match.fun(model.setup$models$Chat.model.names[k])(params = cmaes.results[[k]]$best.param[1:length(cmaes.results[[k]]$best.param)],Q)[indC]

    }else if(model.setup$models$Chat.model.names[k] == "Chat2"){
      data_all[[paste("C",models,sep="")]][indC] <- match.fun(model.setup$models$Chat.model.names[k])(params = cmaes.results[[k]]$best.param[1:length(cmaes.results[[k]]$best.param)],Q,deltaQ)[indC]

    }else if(model.setup$models$Chat.model.names[k] %in%  c("Chat12","Chat14")){
      data_all[[paste("C",models,sep="")]][indC] <- match.fun(model.setup$models$Chat.model.names[k])(params = cmaes.results[[k]]$best.param[1:length(cmaes.results[[k]]$best.param)],Q,C)[indC]

    }else if(model.setup$models$Chat.model.names[k] == "Chat1_s"){
      data_all[[paste("C",models,sep="")]][indC] <- match.fun(model.setup$models$Chat.model.names[k])(params = cmaes.results[[k]]$best.param[1:length(cmaes.results[[k]]$best.param)],Q,T.dtime)[indC]

    }else if(model.setup$models$Chat.model.names[k]  %in%  c("Chat3","Chat4", "Chat5","Chat6","Chat7","Chat8","Chat9","Chat10","Chat11","Chat15")){
      data_all[[paste("Q",models,sep="")]][indQ] <- match.fun(model.setup$models$Bhat.model.name)(params = cmaes.results[[k]]$best.param[1:2],Q,deltaQ)[indQ]
      data_all[[paste("C",models,sep="")]][indC] <- match.fun(model.setup$models$Chat.model.names[k])(params = cmaes.results[[k]]$best.param[3:length(cmaes.results[[k]]$best.param)],Q,data_all[[paste("Q",models,sep="")]],deltaQ)[indC]

    }else if(model.setup$models$Chat.model.names[k] == "Chat13"){
      data_all[[paste("Q",models,sep="")]][indQ] <- match.fun(model.setup$models$Bhat.model.name)(params = cmaes.results[[k]]$best.param[1:2],Q,deltaQ)[indQ]
      data_all[[paste("C",models,sep="")]][indC] <- match.fun(model.setup$models$Chat.model.names[k])(params = cmaes.results[[k]]$best.param[1:length(cmaes.results[[k]]$best.param)],Q,data_all[[paste("Q",models,sep="")]],C,deltaQ)[indC]

    }else if(endsWith(noquote(model.setup$models$Chat.model.names[k]), '_s')){
      data_all[[paste("Q",models,sep="")]][indQ] <- match.fun(model.setup$models$Bhat.model.name)(params = cmaes.results[[k]]$best.param[1:2],Q,deltaQ)[indQ]
      data_all[[paste("C",models,sep="")]][indC] <- match.fun(model.setup$models$Chat.model.names[k])(params = cmaes.results[[k]]$best.param[3:length(cmaes.results[[k]]$best.param)],Q,data_all[[paste("Q",models,sep="")]],deltaQ, d.Time)[indC]
    }
    # error
    data_all[[paste("eL",models,sep="")]][indC] <- log(data_all[[paste("C",models,sep="")]])[indC] - log(C)[indC]

  }

  return(data_all)

}


#' Get Stats
#'
#' \code{getStats}
#'
#' @description
#' getStats calculates the statistics for each fitted C-Q model
#'
#' @details
#' exported summary table with negLL, AIC, NSE, RMSE, and BFI
#'
#' @param Chat.model.names character string vector with a 'Chat#' model name from provided models (i.e. Chat1-Chat15). Chat1 and Chat13 default
#' @param input.data dataframe of daily runoff and concentration AND predictions of concentration and baseflow
#' @param cmaes.results list of cmaes.results from fitted models
#' @param model.setup lists of details about data, model, and site from setModels()
#'
#'
#' @return
#' output summary dataframe with statistics (negLL, AIC, NSE, RMSE, and BFI)
#'
#' @keywords getStats
#'
#'
#' @export getStats
#'

getStats <- function(Chat.model.names = c('Chat1','Chat13'),
                     input.data = data_all,
                     cmaes.results = models,
                     model.setup = model.setup){

  if(length(Chat.model.names)>1){

    if(any(Chat.model.names) %nin% model.setup$models$Chat.model.names){
      stop('please only choose a Chat model that has been fitted')
    }

  }else{

    if(Chat.model.names %nin% model.setup$models$Chat.model.names){
      stop('please only choose a Chat model that has been fitted')
    }
  }

  deltaC = model.setup$reference.data$deltaC
  deltaQ = model.setup$reference.data$deltaQ

  C = model.setup$reference.data$C
  Q = model.setup$reference.data$Q

  #Only run output on data that was used to fit the model, (avoid running when Q < Qthresh)
  indC.deltaC <-sapply(1:NROW(deltaC), function(i) deltaC[i,1]:deltaC[i,2])
  indC = rep(FALSE,NROW(C))
  indC[unlist(indC.deltaC)] <- TRUE

  indQ.deltaQ <-sapply(1:NROW(deltaQ), function(i) deltaQ[i,1]:deltaQ[i,2])
  indQ = rep(FALSE,NROW(Q))
  indQ[unlist(indQ.deltaQ)] <- TRUE

  # initiate table for output
  output.summary <- matrix(NA,nrow = length(Chat.model.names),ncol = 7)
  colnames(output.summary) <- c("ID","model","negLL","AIC","NSE","RMSE","BFI")

  # for loop for each model
  for(k in 1:length(Chat.model.names)){

    # short name of models to run
    models <- sub('....', '', Chat.model.names[k])

    # AIC = 2*(np+nll)
    AIC <- 2*(length(cmaes.results[[k]]$best.param)+(cmaes.results[[k]]$best.fitness))

    #NSE
    NSE = 1 - (sum((data_all$C[indC] - data_all[[paste("C",noquote(models),sep="")]][indC])^2,na.rm = TRUE) / sum((data_all$C[indC] - mean(data_all[[paste("C",noquote(models),sep="")]][indC],na.rm=TRUE))^2,na.rm=TRUE))

    #RMSE
    RMSE <- sqrt(sum((data_all$C[indC] - data_all[[paste("C",noquote(models),sep="")]][indC])^2)/sum(indC))

    if(Chat.model.names %in%  c("Chat1","Chat1_s", "Chat2", "Chat12", "Chat14")){
      BFI <- NA
    }else{
      BFI <- sum(data_all[[paste("Q",models,sep="")]][indQ])/sum(Q[indQ])
    }

    output.summary[k,1] <- model.setup$site.info$site.id
    output.summary[k,2] <- Chat.model.names
    output.summary[k,3] <- cmaes.results[[k]]$best.fitness #negLL
    output.summary[k,4] <- AIC
    output.summary[k,5] <- NSE
    output.summary[k,6] <- RMSE
    output.summary[k,7] <- BFI

  }

  return(as.data.frame(output.summary))

}


#' Get Param
#'
#' \code{getParam}
#'
#' @description
#' getParam outputs the parameters for each fitted C-Q model
#'
#' @details
#' exported datafame with parameters from fitted C-Q models
#'
#' @param Chat.model.names character string vector with a 'Chat#' model name from provided models (i.e. Chat1-Chat15). Chat1 and Chat13 default
#' @param input.data dataframe of daily runoff and concentration AND predictions of concentration and baseflow
#' @param cmaes.results list of cmaes.results from fitted models
#' @param model.setup lists of details about data, model, and site from setModels()
#'
#'
#' @return
#' output summary dataframe with statistics (negLL, AIC, NSE, RMSE, and BFI)
#'
#' @keywords getParam
#'
#'
#' @export getParam
#'

getParam <- function(Chat.model.names = c('Chat1','Chat13'),
                     input.data = data_all,
                     cmaes.results = model,
                    model.setup = model.setup){

  if(is.null(model.setup)){
    site.id = ''
    site.name = ''
  }else{
    site.id = model.setup$site.info$site.id
  }

  if(length(Chat.model.names)>1){

    if(any(Chat.model.names) %nin% model.setup$models$Chat.model.names){
      stop('please only choose a Chat model that has been fitted')
    }

  }else{

    if(Chat.model.names %nin% model.setup$models$Chat.model.names){
      stop('please only choose a Chat model that has been fitted')
    }
  }

  para.list <- rep(list(rep(list()), length(Chat.model.names)),1)

  # for loop for each model
  for(k in 1:length(Chat.model.names)){

    # boundsList <- getBounds(Chat.model.names[k], Bhat.model.name, Likelihood.name)

    ##################### POST-RUN ANALYSIS ###########################
    # export parameters of each model for each rds_list
    para.list[[k]] <- cmaes.results[[k]]$best.param
    names(para.list[[k]]) <- Chat.model.names[k]

    para.summary <- matrix(NA,nrow = length(Chat.model.names[k]),ncol = length(cmaes.results[[k]]$best.param)+3)
    para.summary[,1] <- site.id
    para.summary[,2] <- Chat.model.names[k]
    # para.summary[1,3] <- "upper"
    para.summary[1,3] <- "est"
    # para.summary[3,3] <- "lower"

    # para.summary[1,4:(length(cmaes.results[[k]]$best.param)+3)] <- boundsList[[noquote(Chat.model.names)]]$upper - cmaes.results[[k]]$best.param
    para.summary[1,4:(length(cmaes.results[[k]]$best.param)+3)] <- cmaes.results[[k]]$best.param
    # para.summary[3,4:(length(cmaes.results[[k]]$best.param)+3)] <- cmaes.results[[k]]$best.param - boundsList[[noquote(Chat.model.names)]]$lower

  }

  return(as.data.frame(para.summary))

}


#' plot Results
#'
#' \code{plotResults}
#'
#' @description
#' plotResults plots predictions from each fitted C-Q model
#'
#' @details
#' plots with parameters from fitted C-Q models
#'
#' @param Chat.model.names character string vector with a 'Chat#' model name from provided models (i.e. Chat1-Chat15). Chat1 and Chat13 default
#' @param input.data dataframe of daily runoff and concentration AND predictions of concentration and baseflow
#' @param model.setup lists of details about data, model, and site from setModels()
#'
#' @return
#' annual timeseries plot comparing predictions from two models with observations, streamflow and baseflow; C-Q scatter plots of each model
#'
#' @keywords plotResults
#'
#'
#' @export plotResults
#'

plotResults <- function(Chat.model.names = c('Chat1','Chat13'),
                        input.data = data.frame(),
                        model.setup = model.setup){


  # short name of models to run
  models <- sub('....', '', Chat.model.names)

  if(is.null(model.setup)){
    site.id = ''
    site.name = ''
  }

  plot_scatter = plot_CQ_scatter_compare(model.setup$site.info$site.id, model.setup$site.info$site.name, Chat.model.names[1], Chat.model.names[2] ,models[1],models[2],input.data)

  plot_yearly = plot_yearly_Chat_model_compare(model.setup$site.info$site.id, Chat.model.names[1], Chat.model.names[2] ,models[1],models[2], input.data)


  return(list(plot_scatter,plot_yearly))

}
