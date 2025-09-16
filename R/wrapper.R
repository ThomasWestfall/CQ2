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
#' @param input.data dataframe of daily runoff and concentration. colnames = c("year", "month", "day", "C", "Q")
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
#' @import Hmisc
#'
#' @export setModels
#'

setModels <- function(Chat.model.names = character(),
                      input.data = data.frame(year = c(), month = c(), day = c(), C = c(), Q = c()),
                      Qthresh = 0,
                      Likelihood.name = "GaussLiklihood",
                      site.id = character(),
                      site.name = character()){

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

    Chat.model.names = c('C1','C13')

  }


  # Initiate variables
  T.dtime = NA
  year = input.data$year
  month = input.data$month
  day = input.data$day
  C = input.data$C
  Q = input.data$Q

  # only run likelihood for observed C values that are finite and above Q threshold
  indC <- !is.na(C)
  indQ <- (!is.na(Q))&(Q>=Qthresh)
  ind <- ifelse(indC + indQ == 2, TRUE,FALSE) #(TRUE + TRUE = 2)

  # get index of start and end periods in two columns, must have two days of data at least in each period...
  deltaC = getStartEndIndex(ind,1)
  rm(ind)

  # only run baseflow filter for observed Q greater than zero on continuous periods greater than or equal to 3 days
  indQ <- (!is.na(Q))

  # get index of start and end periods in two columns
  deltaQ = getStartEndIndex(indQ,3)

  # insert name of base flow filter function, only choose 1
  Bhat.model.name <- "BhatEck"

  if(Likelihood.name %nin% c("GaussLiklihood","GaussLiklihoodAR1","GaussLiklihoodAR3")){
    stop('Likelihood.name is not one of the provided likelihood functions')
  }

  # list data
  model.setup = list(reference.data = list(year = year, month = month, day = day,  C = C, Q = Q, T.dtime = T.dtime, deltaC = deltaC, deltaQ = deltaQ),
                     model.names = list(Chat.model.names = Chat.model.names, Bhat.model.name = Bhat.model.name, Likelihood.name = Likelihood.name),
                     site.info = list(site.id = site.id, site.name = site.name),
                     model.output = list(Chat.model.names))

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
#' @import Hmisc
#' @import cmaesr
#'
#' @export runModels
#'

runModels <- function(model.setup = list()){

    # list of results for each model
    # cmaes.results = list()
    # for loop for each model
    for(k in 1:length(model.setup$model.names$Chat.model.names)){

      # check models to run
      if(model.setup$model.names$Chat.model.names[k]  %nin%  c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15")){
        stop(model.setup$model.names$Chat.model.names[k], ' is not one of the provided Chat models')
      }


      # Prepare to calibrate
      # edit getBounds to include lower/upper bounds and start values
      # insert start value and bounds of each Chat model
      boundsList <- getBounds(model.setup$model.names$Chat.model.names[k], model.setup$model.names$Bhat.model.name, model.setup$model.names$Likelihood.name)

      # assign CMAES Variables
      lamda.cmaes <- 8^3 # lamda must be greater or equal to double the number of cores
      sigma.cmaes <- 3.33
      Nrestart <- 2
      restart.multi <- 2

      #start cluster
      nclus = parallel::detectCores()/2
      clus <- parallel::makeCluster(nclus)

      # Create objective functions
      objective_function_to_run <- smoof_objective_function(objective_function_factory(model.setup$reference.data$Q, model.setup$reference.data$C, model.setup$reference.data$deltaQ, model.setup$reference.data$deltaC, model.setup$reference.data$T.dtime, clus, model.setup$model.names$Chat.model.names[k], model.setup$model.names$Bhat.model.name, model.setup$model.names$Likelihood.name), model.setup$model.names$Chat.model.names[k], boundsList)

      # RUN CMAES
      model.setup$model.output[[k]] <- RunCMAES(sigma.cmaes, lamda.cmaes, Nrestart, restart.multi, objective_function_to_run, model.setup$model.names$Chat.model.names[k], boundsList)

      #stop cluster after model runs
      parallel::stopCluster(clus)

      print(paste(model.setup$model.names$Chat.model.names[k]," is complete.",sep=""))

    }

    names(model.setup$model.output) = model.setup$model.names$Chat.model.names

    return(model.setup)

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



getResults<- function(model.setup = list()){


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
  for(k in 1:length(model.setup$model.output)){

    # just get number of model 1,2,3 etc.
    models <- sub('.', '', names(model.setup$model.output[k]))

    # simulated base flow and concentration, and error
    if(names(model.setup$model.output[k]) == "C1"){
      data_all[[names(model.setup$model.output[k])]][indC] <- Chat1(params = model.setup$model.output[[k]]$best.param[1:length(model.setup$model.output[[k]]$best.param)],Q)[indC]

    }else if(names(model.setup$model.output[k]) == "C2"){
      data_all[[names(model.setup$model.output[k])]][indC] <- Chat2(params = model.setup$model.output[[k]]$best.param[1:length(model.setup$model.output[[k]]$best.param)],Q,deltaQ)[indC]

    }else if(names(model.setup$model.output[k]) == "C12"){
      data_all[[names(model.setup$model.output[k])]][indC] <- Chat12(params = model.setup$model.output[[k]]$best.param[1:length(model.setup$model.output[[k]]$best.param)],Q,C)[indC]

    }else if(names(model.setup$model.output[k]) == "C14"){
      data_all[[names(model.setup$model.output[k])]][indC] <- Chat14(params = model.setup$model.output[[k]]$best.param[1:length(model.setup$model.output[[k]]$best.param)],Q,C)[indC]

    }else if(names(model.setup$model.output[k]) == "C1_s"){
      data_all[[names(model.setup$model.output[k])]][indC] <- Chat1_s(params = model.setup$model.output[[k]]$best.param[1:length(model.setup$model.output[[k]]$best.param)],Q,T.dtime)[indC]

    }else if(names(model.setup$model.output[k])  == "C3"){
      data_all[[paste("Q",models,sep="")]][indQ] <- BhatEck(params = model.setup$model.output[[k]]$best.param[1:2],Q,deltaQ)[indQ]
      data_all[[names(model.setup$model.output[k])]][indC] <- Chat3(params = model.setup$model.output[[k]]$best.param[3:length(model.setup$model.output[[k]]$best.param)],Q,data_all[[paste("Q",models,sep="")]],deltaQ)[indC]

    }else if(names(model.setup$model.output[k])  == "C4"){
      data_all[[paste("Q",models,sep="")]][indQ] <- BhatEck(params = model.setup$model.output[[k]]$best.param[1:2],Q,deltaQ)[indQ]
      data_all[[names(model.setup$model.output[k])]][indC] <- Chat4(params = model.setup$model.output[[k]]$best.param[3:length(model.setup$model.output[[k]]$best.param)],Q,data_all[[paste("Q",models,sep="")]],deltaQ)[indC]

    }else if(names(model.setup$model.output[k])  ==  "C5"){
      data_all[[paste("Q",models,sep="")]][indQ] <- BhatEck(params = model.setup$model.output[[k]]$best.param[1:2],Q,deltaQ)[indQ]
      data_all[[names(model.setup$model.output[k])]][indC] <- Chat5(params = model.setup$model.output[[k]]$best.param[3:length(model.setup$model.output[[k]]$best.param)],Q,data_all[[paste("Q",models,sep="")]],deltaQ)[indC]

    }else if(names(model.setup$model.output[k])  ==  "C6"){
      data_all[[paste("Q",models,sep="")]][indQ] <- BhatEck(params = model.setup$model.output[[k]]$best.param[1:2],Q,deltaQ)[indQ]
      data_all[[names(model.setup$model.output[k])]][indC] <- Chat6(params = model.setup$model.output[[k]]$best.param[3:length(model.setup$model.output[[k]]$best.param)],Q,data_all[[paste("Q",models,sep="")]],deltaQ)[indC]

    }else if(names(model.setup$model.output[k])  ==  "C7"){
      data_all[[paste("Q",models,sep="")]][indQ] <- BhatEck(params = model.setup$model.output[[k]]$best.param[1:2],Q,deltaQ)[indQ]
      data_all[[names(model.setup$model.output[k])]][indC] <- Chat7(params = model.setup$model.output[[k]]$best.param[3:length(model.setup$model.output[[k]]$best.param)],Q,data_all[[paste("Q",models,sep="")]],deltaQ)[indC]

    }else if(names(model.setup$model.output[k])  == "C8"){
      data_all[[paste("Q",models,sep="")]][indQ] <- BhatEck(params = model.setup$model.output[[k]]$best.param[1:2],Q,deltaQ)[indQ]
      data_all[[names(model.setup$model.output[k])]][indC] <- Chat8(params = model.setup$model.output[[k]]$best.param[3:length(model.setup$model.output[[k]]$best.param)],Q,data_all[[paste("Q",models,sep="")]],deltaQ)[indC]

    }else if(names(model.setup$model.output[k])  ==  "C9"){
      data_all[[paste("Q",models,sep="")]][indQ] <- BhatEck(params = model.setup$model.output[[k]]$best.param[1:2],Q,deltaQ)[indQ]
      data_all[[names(model.setup$model.output[k])]][indC] <- Chat9(params = model.setup$model.output[[k]]$best.param[3:length(model.setup$model.output[[k]]$best.param)],Q,data_all[[paste("Q",models,sep="")]],deltaQ)[indC]

    }else if(names(model.setup$model.output[k])  == "C10"){
      data_all[[paste("Q",models,sep="")]][indQ] <- BhatEck(params = model.setup$model.output[[k]]$best.param[1:2],Q,deltaQ)[indQ]
      data_all[[names(model.setup$model.output[k])]][indC] <- Chat10(params = model.setup$model.output[[k]]$best.param[3:length(model.setup$model.output[[k]]$best.param)],Q,data_all[[paste("Q",models,sep="")]],deltaQ)[indC]

    }else if(names(model.setup$model.output[k])  ==  "C11"){
      data_all[[paste("Q",models,sep="")]][indQ] <- BhatEck(params = model.setup$model.output[[k]]$best.param[1:2],Q,deltaQ)[indQ]
      data_all[[names(model.setup$model.output[k])]][indC] <- Chat11(params = model.setup$model.output[[k]]$best.param[3:length(model.setup$model.output[[k]]$best.param)],Q,data_all[[paste("Q",models,sep="")]],deltaQ)[indC]

    }else if(names(model.setup$model.output[k])  ==  "C15"){
      data_all[[paste("Q",models,sep="")]][indQ] <- BhatEck(params = model.setup$model.output[[k]]$best.param[1:2],Q,deltaQ)[indQ]
      data_all[[names(model.setup$model.output[k])]][indC] <- Chat15(params = model.setup$model.output[[k]]$best.param[3:length(model.setup$model.output[[k]]$best.param)],Q,data_all[[paste("Q",models,sep="")]],deltaQ)[indC]

    }else if(names(model.setup$model.output[k]) == "C13"){
      data_all[[paste("Q",models,sep="")]][indQ] <- BhatEck(params = model.setup$model.output[[k]]$best.param[1:2],Q,deltaQ)[indQ]
      data_all[[names(model.setup$model.output[k])]][indC] <- Chat13(params = model.setup$model.output[[k]]$best.param[1:length(model.setup$model.output[[k]]$best.param)],Q,data_all[[paste("Q",models,sep="")]],C,deltaQ)[indC]

    }
    # error
    data_all[[paste("eL_",names(model.setup$model.output[k]),sep="")]][indC] <- log(data_all[[names(model.setup$model.output[k])]])[indC] - log(C)[indC]

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
#' @param model.setup lists of details about data, model, and site from \code{setModels}
#' @param output.data dataframe of daily concentration and baseflow predictions from \code{getResults}
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

getStats <- function(model.setup = list(),
                     output.data = dataframe()){

  # if(length(model.setup$model.output)>1){
  #
  #   if(any(model.setup$model.names$Chat.model.names) %nin% model.setup$model.output){
  #     stop('please run all  model that has been fitted')
  #   }
  #
  # }else{
  #
  #   if(model.setup$model.names$Chat.model.names %in% model.setup$model.output){
  #     stop('please only choose a Chat model that has been fitted')
  #   }
  # }

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
  output.summary <- matrix(NA,nrow = length(model.setup$model.output),ncol = 7)
  colnames(output.summary) <- c("ID","model","negLL","AIC","NSE","RMSE","BFI")

  # for loop for each model
  for(k in 1:length(model.setup$model.output)){

    # short name of models to run
    models <- sub('.', '', names(model.setup$model.output[k]))

    # AIC = 2*(np+nll)
    AIC <- 2*(length(model.setup$model.output[[k]]$best.param)+(model.setup$model.output[[k]]$best.fitness))

    #NSE
    NSE = 1 - (sum((output.data$C[indC] - output.data[[names(model.setup$model.output[k])]][indC])^2,na.rm = TRUE) / sum((output.data$C[indC] - mean(output.data[[names(model.setup$model.output[k])]][indC],na.rm=TRUE))^2,na.rm=TRUE))

    #RMSE
    RMSE <- sqrt(sum((output.data$C[indC] - output.data[[names(model.setup$model.output[k])]][indC])^2, na.rm = TRUE)/sum(indC))

    if(names(model.setup$model.output[k]) %in%  c("C1","C1_s", "C2", "C12", "C14")){
      BFI <- NA
    }else{
      BFI <- sum(output.data[[paste("Q",models,sep="")]][indQ])/sum(Q[indQ])
    }

    output.summary[k,1] <- model.setup$site.info$site.id
    output.summary[k,2] <- names(model.setup$model.output[k])
    output.summary[k,3] <- model.setup$model.output[[k]]$best.fitness #negLL
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
#' @param model.setup lists of details about data, model, and site from \code{setModels}
#' @param output.data dataframe of daily concentration and baseflow predictions from \code{getResults}
#'
#'
#' @return
#' output summary dataframe with parameters for fitted models
#'
#' @keywords getParam
#'
#'
#' @export getParam
#'

getParam <- function(model.setup = list(),
                     output.data = dataframe()){

  if(is.null(model.setup)){
    site.id = ''
    site.name = ''
  }else{
    site.id = model.setup$site.info$site.id
  }

  # if(length(names(model.setup$model.output))>1){
  #
  #   if(any(Chat.model.names) %nin% model.setup$models$Chat.model.names){
  #     stop('please only choose a Chat model that has been fitted')
  #   }
  #
  # }else{
  #
  #   if(Chat.model.names %nin% model.setup$models$Chat.model.names){
  #     stop('please only choose a Chat model that has been fitted')
  #   }
  # }

  para.list <- rep(list(rep(list()), length(names(model.setup$model.output))),1)

  # for loop for each model
  for(k in 1:length(names(model.setup$model.output))){

    # boundsList <- getBounds(Chat.model.names[k], Bhat.model.name, Likelihood.name)

    ##################### POST-RUN ANALYSIS ###########################
    # export parameters of each model for each rds_list
    para.list[[k]] <- model.setup$model.output[[k]]$best.param
    names(para.list[[k]]) <- names(model.setup$model.output[k])

    para.summary <- matrix(NA,nrow = length(names(model.setup$model.output[k])),ncol = length(model.setup$model.output[[k]]$best.param)+3)
    para.summary[,1] <- site.id
    para.summary[,2] <- names(model.setup$model.output[k])
    # para.summary[1,3] <- "upper"
    para.summary[1,3] <- "est"
    # para.summary[3,3] <- "lower"

    # para.summary[1,4:(length(cmaes.results[[k]]$best.param)+3)] <- boundsList[[noquote(Chat.model.names)]]$upper - cmaes.results[[k]]$best.param
    para.summary[1,4:(length(model.setup$model.output[[k]]$best.param)+3)] <- model.setup$model.output[[k]]$best.param
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
#' plots C-Q scatter plots and annual timeseries with predictions from fitted C-Q models
#'
#' @param model.setup lists of details about data, model, and site from setModels()
#' @param output.data dataframe of daily concentration and baseflow predictions from \code{getResults}
#' @param plot.models character string vector with a 'C#' model name from provided models (i.e. C1-C15). Ch1 and C13 default. Two model limit.
#' @param plot.type character string of either "scatter" or "timeseries" to view results. Note, 'timeseries' exports plot as a pdf in the working directory
#'
#'
#'
#' @return
#' annual timeseries plot comparing predictions from two models with observations, streamflow and baseflow; C-Q scatter plots of each model
#'
#' @keywords plotResults
#'
#' @import padr
#'
#' @export plotResults
#'

plotResults <- function(model.setup = list(),
                        output.data = data.frame(),
                        plot.models = character(c()),
                        plot.type = "scatter"
                        ){

  op <- par(no.readonly = T)
  on.exit(par(op))

  if(length(plot.models)>2){
    stop("plot.models > 2, only 2 models can be plotted at once")
  }

  if(length(plot.type)>1){
    stop("only one plot.type at a time, either 'scatter' or 'timeseries' permitted")
  }

  if(is.null(model.setup)){
    site.id = ''
    site.name = ''
  }

  # short name of models to run
  models <- sub('.', '', names(model.setup$model.output))

  if(length(plot.models) == 2){

    if(plot.models[1] %in% c("C3","C4","C5","C6","C7","C8","C9","C10","C11","C13","C15")){
      stop("plot.models[1] must be a single flow component model (C1, C2, C12, C14)")
    }

    if(plot.type == "scatter" || is.null(plot.type)){

      plot.output = plot_CQ_scatter_compare(model.setup$site.info$site.id, model.setup$site.info$site.name, plot.models[1], plot.models[2],output.data)

    }else if(plot.type == "timeseries"){

      plot.output = plot_yearly_Chat_model_compare(model.setup$site.info$site.id, plot.models[1], plot.models[2], output.data)
      message("Timeseries plot exported as a PDF in working directory")

    }

  }else{

    if(plot.type == "scatter" || is.null(plot.type)){

    plot.output = plot_CQ_scatter(model.setup$site.info$site.id, model.setup$site.info$site.name, plot.models ,output.data)

    }else if(plot.type == "timeseries"){

    plot.output = plot_yearly_Chat_model(model.setup$site.info$site.id, plot.models, output.data)
    message("Timeseries plot exported as a PDF in working directory")

    }

  }




  return(plot.output)

}
