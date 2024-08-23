#######################################################################
rm(list = setdiff(ls(), lsf.str()))
# Install and load the cmaes package
# install.packages("cmaes")
# devtools::install_github("jakobbossek/cmaesr")
library(cmaesr)
library(colorspace)
library(padr)

library(ggplot2)
library(svglite)

savepath <- #"/home/tpet008/Westfall/VICWQplots"
  "C:/Users/twes0006/OneDrive - Monash University/CQ2Results/Special"
SENS_SITE <- read.csv(paste("C:/Users/twes0006/OneDrive - Monash University/CQ2Results/FinalResults","/","SENS_SITE_info3.csv",sep=""))

#################################################################
# models.to.run = c("Chat1","Chat2","Chat3","Chat4","Chat5","Chat6","Chat7","Chat8","Chat9","Chat10","Chat11")
# models.to.run = c("Chat1","Chat2","Chat8","Chat9")
# models.to.run = c("Chat1","Chat2","Chat3","Chat4","Chat5","Chat6","Chat7","Chat8","Chat9","Chat10","Chat11","Chat12","Chat13","Chat14","Chat15")
models.to.run = c("Chat1","Chat13")

Batch = "Chat1_13_20240731"

output.summary <- matrix(NA,nrow = nrow(SENS_SITE)*length(models.to.run),ncol = 7)
colnames(output.summary) <- c("ID","model","negLL","AIC","NSE","RMSE","BFI")

para.list <- rep(list(rep(list(list()), length(models.to.run))),nrow(SENS_SITE))
names(para.list) <- SENS_SITE$V1

# to get files from .txt
foldername <- paste("C:/Users/twes0006/OneDrive - Monash University/Data/HourlyVicData/hrly TS data/hrly TS data",sep="")

m<-1
for(m in 1:nrow(SENS_SITE)){ #nrow(SENS_SITE)){

  # Extract one catchment from site list
  gaugeID <- SENS_SITE$V1[m]

  # Get raw data hourly, reformat from .txt
  # raw_data = getRawDataHourly(foldername,savepath,gaugeID,0.6)


  # set folder to get sitefiles, already computed
  # foldername <-  #paste("/home/tpet008/Westfall/SiteFiles","/",SENS_SITE$V1[m],sep="")
    # paste("C:/Users/twes0006/OneDrive - Monash University/VICWQPLOTS/SiteFiles","/",SENS_SITE$V1[m],sep="")

  # # # GET OBSERVED DATA # # #
  # get a list of .rds files in the Mother folder,
  # rds_files <- list.files(foldername, pattern = paste0("^",prefix = paste0(SENS_SITE$V1[m],"_EC_TDS_",timevalue, sep =""), ".*\\.rds$", sep =""), full.names = TRUE)
  rds_files <- list.files(paste("C:/Users/twes0006/OneDrive - Monash University/VICWQPLOTS/SiteFiles/",gaugeID,sep=""), pattern = ".rds", full.names = TRUE)
  # Read the .rds files and store them in a list
  rds_list <- lapply(rds_files, readRDS)
  # get first rds file, should be observed data on a daily timestep
  working_daily <- rds_list[[1]]

  # stand in to review if EC/TDS values are less than zero, manually adjust based on reviewing
  wd_ind <- is.finite(working_daily$TDS)
  if(any(working_daily$TDS[wd_ind] <= 0)){
    print(which(working_daily$TDS <= 0))
    #
    # working_daily$TDS[6614] = 82.48618
  }



  # Convert to format for hydroState
  data_all <- data.frame(year = working_daily$year, month=working_daily$month, day = as.numeric(working_daily$day), C=working_daily$TDS, flow_mm_d=working_daily$FLOW_mm_d, dtime = working_daily$DecYear)
  # data_all <- data_all[data_all$year == 2000, ]

  #unique years & drought index
  uniqueyears <- unique(data_all$year)

  ###for plotting
  data_all$year_range <- cut(data_all$year, breaks = c(uniqueyears[1]-1, 1997, 2009, uniqueyears[length(uniqueyears)]), labels = c("Pre-drought", "Drought","Post-drought"))
  # working_daily$year_range <- cut(working_daily$year, breaks = c(uniqueyears[1]-1, 1997, 2009, uniqueyears[length(uniqueyears)]), labels = c("Pre-drought", "Drought","Post-drought"))

  # Initiate variables
  nrow_data <- nrow(data_all)
  T.dtime <- data_all$dtime - data_all$year
  C <- data_all$C
  C.mean <- mean(data_all$C)
  Q <- data_all$flow_mm_d
  Qthresh <- 0.00

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

  # future work allows switching between hourly and daily timesteps... daily for now
  # timevalue <- "daily" #or 'hourly'

  for(k in 1:length(models.to.run)){
    # insert name of Chat models to run
    Chat.model.names <- c(models.to.run[k])
    # Chat.model.names <- "Chat10"
    # insert name of base flow filter function, only choose 1
    Bhat.model.name <- "BhatEck"

    # insert name of likelihood function, only choose 1
    # (GaussLiklihood, GaussLiklihoodAR1, or GaussLiklihoodAR3)
    Likelihood.name <- "GaussLiklihood"

    ## For Calibration... edit getBounds to include lower/upper bounds and start values
    # insert start value and bounds of each Chat model
    boundsList <- getBounds(Chat.model.names, Bhat.model.name, Likelihood.name)

    # short name of models to run
    models <- sub('....', '', Chat.model.names)


    #new folder to save output
    foldername1 <- paste(savepath,"/",SENS_SITE$V1[m],"/",Batch,sep="")
    foldername2 <- paste(foldername1,"/",Chat.model.names,sep="")
    lapply(foldername1,dir.create,recursive = TRUE)
    lapply(foldername2,dir.create,recursive = TRUE)

    #reset wd
    setwd(foldername2)
    getwd()

    # assign CMAES Variables
    lamda.cmaes <- 8^3 # lamda must be greater or equal to double the number of cores
    sigma.cmaes <- 3.33
    Nrestart <- 2
    restart.multi <- 2

    #start cluster
    nclus = parallel::detectCores()/2
    clus <- parallel::makeCluster(nclus)

    # Create objective functions
    objective_function_to_run <- smoof_objective_function(objective_function_factory(Q, C, deltaQ, deltaC, T.dtime, clus, Chat.model.names, Bhat.model.name, Likelihood.name),Chat.model.names, boundsList)

    # RUN CMAES
    cmaes.results <- RunCMAES(sigma.cmaes, lamda.cmaes, Nrestarts, restart.multi, objective_function_to_run,Chat.model.names, boundsList)

    #stop cluster after model runs
    parallel::stopCluster(clus)
  ###################################################################################


    #Only run output on data that was used to fit the model, (avoid running when Q < Qthresh)
    indC.deltaC <-sapply(1:NROW(deltaC), function(i) deltaC[i,1]:deltaC[i,2])
    indC = rep(FALSE,NROW(C))
    indC[unlist(indC.deltaC)] <- TRUE

    indQ.deltaQ <-sapply(1:NROW(deltaQ), function(i) deltaQ[i,1]:deltaQ[i,2])
    indQ = rep(FALSE,NROW(Q))
    indQ[unlist(indQ.deltaQ)] <- TRUE

    # simulated base flow and concentration, and error
    if(Chat.model.names == "Chat1"){
      data_all[[paste("C",models,sep="")]][indC] <- match.fun(Chat.model.names)(params = cmaes.results$best.param[1:length(cmaes.results$best.param)],Q)[indC]

    }else if(Chat.model.names == "Chat2"){
      data_all[[paste("C",models,sep="")]][indC] <- match.fun(Chat.model.names)(params = cmaes.results$best.param[1:length(cmaes.results$best.param)],Q,deltaQ)[indC]

    }else if(Chat.model.names %in%  c("Chat12","Chat14")){
      data_all[[paste("C",models,sep="")]][indC] <- match.fun(Chat.model.names)(params = cmaes.results$best.param[1:length(cmaes.results$best.param)],Q,C)[indC]

    }else if(Chat.model.names == "Chat1_s"){
      data_all[[paste("C",models,sep="")]][indC] <- match.fun(Chat.model.names)(params = cmaes.results$best.param[1:length(cmaes.results$best.param)],Q,T.dtime)[indC]

    }else if(Chat.model.names  %in%  c("Chat3","Chat4", "Chat5","Chat6","Chat7","Chat8","Chat9","Chat10","Chat11","Chat15")){
      data_all[[paste("Q",models,sep="")]][indQ] <- match.fun(Bhat.model.name)(params = cmaes.results$best.param[1:2],Q,deltaQ)[indQ]
      data_all[[paste("C",models,sep="")]][indC] <- match.fun(Chat.model.names)(params = cmaes.results$best.param[3:length(cmaes.results$best.param)],Q,data_all[[paste("Q",models,sep="")]],deltaQ)[indC]

    }else if(Chat.model.names == "Chat13"){
      data_all[[paste("Q",models,sep="")]][indQ] <- match.fun(Bhat.model.name)(params = cmaes.results$best.param[1:2],Q,deltaQ)[indQ]
      data_all[[paste("C",models,sep="")]][indC] <- match.fun(Chat.model.names)(params = cmaes.results$best.param[1:length(cmaes.results$best.param)],Q,data_all[[paste("Q",models,sep="")]],C,deltaQ)[indC]

    }else if(endsWith(noquote(Chat.model.names), '_s')){
      data_all[[paste("Q",models,sep="")]][indQ] <- match.fun(Bhat.model.name)(params = cmaes.results$best.param[1:2],Q,deltaQ)[indQ]
      data_all[[paste("C",models,sep="")]][indC] <- match.fun(Chat.model.names)(params = cmaes.results$best.param[3:length(cmaes.results$best.param)],Q,data_all[[paste("Q",models,sep="")]],deltaQ, d.Time)[indC]
    }
    # error
    data_all[[paste("eL",models,sep="")]][indC] <- log(C)[indC] - log(data_all[[paste("C",models,sep="")]])[indC]

    ##################### POST-RUN ANALYSIS ###########################
    # export parameters of each model for each rds_list
    para.list[[m]][[k]] <- cmaes.results$best.param
    names(para.list[[m]]) <- models.to.run

    para.summary <- matrix(NA,nrow = 3,ncol = length(cmaes.results$best.param)+3)
    para.summary[,1] <- gaugeID
    para.summary[,2] <- Chat.model.names
    para.summary[1,3] <- "upper"
    para.summary[2,3] <- "est"
    para.summary[3,3] <- "lower"

    para.summary[1,4:(length(cmaes.results$best.param)+3)] <- boundsList[[noquote(Chat.model.names)]]$upper - cmaes.results$best.param
    para.summary[2,4:(length(cmaes.results$best.param)+3)] <- cmaes.results$best.param
    para.summary[3,4:(length(cmaes.results$best.param)+3)] <- cmaes.results$best.param - boundsList[[noquote(Chat.model.names)]]$lower


    # AIC = 2*(np+nll) # from hydrostate.R, line 386
    AIC <- 2*(length(cmaes.results$best.param)+(cmaes.results$best.fitness))

    #NSE
    NSE = 1 - (sum((data_all$C[indC] - data_all[[paste("C",noquote(models),sep="")]][indC])^2,na.rm = TRUE) / sum((data_all$C[indC] - mean(data_all[[paste("C",noquote(models),sep="")]][indC],na.rm=TRUE))^2,na.rm=TRUE))

    #RMSE
    RMSE <- sqrt(sum((data_all$C[indC] - data_all[[paste("C",noquote(models),sep="")]][indC])^2)/sum(indC))

    if(Chat.model.names %in%  c("Chat1","Chat1_s", "Chat2", "Chat12", "Chat14")){
      BFI <- NA
    }else{
      BFI <- sum(data_all[[paste("Q",models,sep="")]][indQ])/sum(Q[indQ])
    }

    output.summary[(m-1)*length(models.to.run)+k,1] <- gaugeID
    output.summary[(m-1)*length(models.to.run)+k,2] <- Chat.model.names
    output.summary[(m-1)*length(models.to.run)+k,3] <- cmaes.results$best.fitness #negLL
    output.summary[(m-1)*length(models.to.run)+k,4] <- AIC
    output.summary[(m-1)*length(models.to.run)+k,5] <- NSE
    output.summary[(m-1)*length(models.to.run)+k,6] <- RMSE
    output.summary[(m-1)*length(models.to.run)+k,7] <- BFI

    # # Export
    # write.csv(output.summary,file = paste0(SENS_SITE$V1[m],"_",Chat.model.names,"_output.summary",".csv", sep = ""), row.names = FALSE)
    # capture.output(para.list, file = paste0(SENS_SITE$V1[m],"_",Chat.model.names,"_parameters",".csv", sep = ""))

    #save current R data
    # save.image(file=paste(foldername2,"/",SENS_SITE$V1[m],".RData",sep=""))

    ##Plots for each model, autocorrelationerror
    pdf(paste(SENS_SITE$V1[m],"_",Chat.model.names,"_C_auto_error",".pdf",sep=""), width = 5, height = 5)

    plot_error_Chat(site.id = SENS_SITE$V1[m], model.name = Chat.model.names, model.short = models, Chat.residuals = data_all[[paste("eL",models,sep="")]])

    dev.off()

    ## compare obs with simulated
    png(filename = paste(SENS_SITE$V1[m],"_",Chat.model.names,"_C_compare",".png",sep=""), width = 5, height = 5, units = "in", res = 300)

    plot_error_Chat_compare(site.id = SENS_SITE$V1[m],site.name =SENS_SITE$SHORTNAME[m],  model.name = Chat.model.names, model.short = models, data = data_all, index.C = indC)

    dev.off()

    # residual timeseries
    pdf(paste(SENS_SITE$V1[m],"_",Chat.model.names,"_yearly_residuals.pdf",sep=""), width = 14, height = 8)

    NSE.list = list()
    for(p in 1:length(uniqueyears)){

      data <- data_all[data_all$year == uniqueyears[p], ]

      plot_yearly_error(site.id = SENS_SITE$V1[m], model.name = Chat.model.names, model.short = models, data = data)

      #NSE
      NSE = 1 - (sum((data$C[indC] - data[[paste("C",noquote(models),sep="")]][indC])^2,na.rm = TRUE) / sum((data$C[indC] - mean(data[[paste("C",noquote(models),sep="")]][indC],na.rm=TRUE))^2,na.rm=TRUE))

      NSE.list[[p]] = c( uniqueyears[p],NSE)
      # Chat_ress <- data_all[[paste("eL",models,sep="")]]
    }

    dev.off()

  }
    ####################### COMPARISON PLOTS ######################################################
  setwd(foldername1)

    # open pdf printer and create Chat plots, one year on each page
    pdf(paste(SENS_SITE$V1[m],"_","Chat1_v_Chat13_Qlog10","_yearly.pdf",sep=""), width = 14, height = 8)

    for(p in 1:length(uniqueyears)){

      data <- data_all[data_all$year == uniqueyears[p], ]

      svglite(paste(SENS_SITE$V1[m],"_","Chat1_",uniqueyears[p],".svg",sep=""), width = 9, height = 6, bg = "transparent", pointsize = 8)

      # plot_yearly_Chat_model_compare(site.id = SENS_SITE$V1[m], model.name1 = "Chat1", model.name2 = "Chat13", model.short1 = 1, model.short2 = 13, data = data)

      plot_yearly_Chat(site.id = SENS_SITE$V1[m], model.name = "Chat1", model.short= 1, data=data)

      # Chat_ress <- data_all[[paste("eL",models,sep="")]]
      dev.off()
    }

    dev.off()

  ### ADD CQ scatter
  png(filename = paste(SENS_SITE$V1[m],"_","Chat1_v_Chat13","_CQ_scatter_mmDay",".png",sep=""), width = 5, height = 5, units = "in", res = 300)
  plot_CQ_scatter_compare(site.id =SENS_SITE$V1[m], site.name =SENS_SITE$SHORTNAME[m], model.name1 = "Chat1", model.name2 = "Chat13", model.short1 = 1, model.short2 = 13,data = data_all,index.C = indC)
  dev.off()

  # save output
  filename <- paste0(SENS_SITE$V1[m],"_data_all.rds", sep="")
  saveRDS(data_all, file = file.path(foldername1, filename))
  write.csv(data_all, file = paste0(SENS_SITE$V1[m],"_data_all",".csv", sep = ""), row.names = FALSE)

  filename <- paste0(SENS_SITE$V1[m],"_output.summary.rds", sep="")
  saveRDS(output.summary, file = file.path(foldername1, filename))
  write.csv(output.summary,file = paste0(SENS_SITE$V1[m],"_output.summary",".csv", sep = ""), row.names = FALSE)

  capture.output(para.list, file = paste0(SENS_SITE$V1[m],"_parameters",".csv", sep = ""))

  #save current R data
  save.image(file=paste(foldername1,"/",SENS_SITE$V1[m],".RData",sep=""))
}

setwd(savepath)
filename <- paste0("_output.summary.rds", sep="")
saveRDS(output.summary, file = file.path(foldername1, filename))
write.csv(output.summary,file = paste0("ALL_output.summary_open",".csv", sep = ""), row.names = FALSE)

capture.output(para.list, file = paste0("ALL_parameters",".csv", sep = ""))





########### boxplots of error...











# capture.output(para.list, file = paste0(SENS_SITE$V1[m],"A_parameters",".csv", sep = ""))

    # #error plot
    # pframe <- data.frame(datetime,Cp)
    # pframe <- pad(pframe,interval = "day")
    # plot(pframe, type='l',col='darkgrey', lwd=3, ylim = ylim_, xlim = xlim_,xlab= paste(uniqueyears[p]), ylab='Salinity (mg/L)',xaxt='n', cex.lab = 2, cex.axis = 2)
    # axis(cex.axis = 2,side = 1, at = month_lab, labels = format(month_lab, "%b"))
    # abline(v = month_lab, col = "lightgray", lty = "dotted")
    #
    # # add Chat line graph log
    # par(new=TRUE)
    # pframe <- data.frame(datetime,data[[paste("C",noquote(models),sep="")]])
    # pframe <- pad(pframe,interval = "day")
    # plot(pframe, type="l", lty = 1, col="gold", axes = FALSE, ylim = ylim_,xlim=xlim_, ylab="", xlab="", lwd=3)
    # mtext(paste(modelsEQ[[1]]), side =3,cex = 1.2)
    #
    # #add error
    # ylim_3 <- c(min(data[[paste("eL",noquote(models),sep="")]],na.rm = TRUE),max(data[[paste("eL",noquote(models),sep="")]],na.rm = TRUE))
    # ylim_3[1] <- ifelse(is.finite(ylim_3[1]),ylim_3[1],-2)
    # ylim_3[2] <- ifelse(is.finite(ylim_3[2]),ylim_3[2],2)
    # par(new=TRUE)
    # pframe <- data.frame(datetime,data[[paste("eL",noquote(models),sep="")]])
    # pframe <- pad(pframe,interval = "day")
    # plot(pframe, pch =19, col="#D24E71", xlim = xlim_, ylim = ylim_3, axes = FALSE, xlab="",ylab = "", lwd=3, cex.axis = 2,cex.lab = 2, las = 1)
    # mtext("Residuals-ln", cex = 2, side = 4, line = 5)
    # axis(side = 4, cex.axis = 2, las=1, labels=TRUE)
    # abline(h = 0, col = "black", lty = 2, lwd = 2)
    # #
    # legend(x = "top", y = par("usr")[4] + 0.1, legend = c("Obs. C",paste("C",noquote(models),sep=""),"Residuals"),
    #        col = c("darkgray","gold","#D24E71"),lty = c(1,1,0),pch = c(NA,NA,19),lwd=3, pt.cex = 1,cex = 1.2,
    #        ncol=3, inset=c(0, -0.0), xpd=TRUE)
    # title(paste(SENS_SITE$V1[m]," ",Chat.model.names,sep=""))



  # }

