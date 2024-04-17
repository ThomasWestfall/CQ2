library(colorspace)
library(padr)
library(ggplot2)
library(svglite)


#######################################################################
savepath.new <- #"/home/tpet008/Westfall/VICWQplots"
  "C:/Users/twes0006/OneDrive - Monash University/CQ2Results/FinalResults"
SENS_SITE.new <- read.csv(paste(savepath.new,"/","SENS_SITE_info3.csv",sep=""))

#################################################################
# models.to.run = c("Chat1","Chat2","Chat3","Chat4","Chat5","Chat6","Chat7","Chat8","Chat9","Chat10","Chat11")
models.to.run.now = c("Chat1","Chat2","Chat12","Chat13","Chat14")
# models.to.run.now = c("Chat3","Chat4","Chat5","Chat6","Chat7","Chat8","Chat9","Chat10","Chat11","Chat15")
models.now <- sub('....', '', models.to.run.now)

Batch.new = "Test_20240316_AR0"

# output.summary <- matrix(NA,nrow = nrow(SENS_SITE)*length(models.to.run),ncol = 7)
# colnames(output.summary) <- c("ID","model","negLL","AIC","NSE","RMSE","BFI")


m<-1
for(m in 1:nrow(SENS_SITE.new)){ #nrow(SENS_SITE)){

  # Extract one catchment from site list
  # gaugeID <- SENS_SITE.new$V1[m]

  # set Mother folder
  foldername.new <- paste(savepath.new,"/",SENS_SITE.new$V1[m],"/", Batch.new,sep="")

  # subfolders <- list.dirs(foldername.new, recursive = FALSE)


  love<-1
  for(love in 1:length(models.to.run.now)){

    foldername.new1 <-  paste(foldername.new,"/",models.to.run.now[love],sep="")



    # folder_index <- which(subfolders == paste(savepath,"/",SENS_SITE['V1'][1,], sep =""))

    setwd(foldername.new1)
    # setwd(file.path(foldername.new1))
    #
    # folder_index <- which(subfolders == paste(savepath,"/",SENS_SITE['V1'][i,], sep =""))
    # sub_subfolder <- list.dirs(subfolders[folder_index], recursive = FALSE)
    # subsub_folderindex <- which(sub_subfolder == paste(subfolders[folder_index],"/",SENS_SITE['V1'][i,], "_CQ_CQsQf_test_thresh", sep =""))
    #
    # setwd(sub_subfolder[subsub_folderindex])

    load(paste(SENS_SITE.new$V1[m],".RData",sep=""))


    getwd()


    # png(filename = paste(SENS_SITE.new$V1[m],"_",models.to.run.now[love],"_CQ_scatter_C",".png",sep=""), width = 5, height = 5, units = "in", res = 300)
    svglite(paste(SENS_SITE.new$V1[m],"_",models.to.run.now[love],"_CQ_scatter_C",".svg",sep=""), width = 5, height = 5, bg = "transparent", pointsize = 8)
    plot_CQ_scatter(site.id =SENS_SITE.new$V1[m], site.name =SENS_SITE.new$SHORTNAME[m], model.name = models.to.run.now[1],model.short = models.now[1],data = data_all,index.C = indC)
    dev.off()


    #plot time-sereis with log on Q

    # open pdf printer and create Chat plots, one year on each page
    # pdf(paste(SENS_SITE.new$V1[m],"_",models.to.run.now[love],"_yearly_log.pdf",sep=""), width = 14, height = 8)
    # setEPS()
    # postscript(file=paste(SENS_SITE.new$V1[m],"_",models.to.run.now[love],"_yearly.eps",sep=""),width=14, height=8,pointsize=8, paper='special',colormodel='rgb')

    for(p in 1:length(uniqueyears)){

      data <- data_all[data_all$year == uniqueyears[p], ]

      svglite(paste(SENS_SITE.new$V1[m],"_",models.to.run.now[love],"_yearly_",uniqueyears[p],".svg",sep=""), width = 7, height = 4, bg = "transparent", pointsize = 8)

      plot_yearly_Chat_dot(site.id = SENS_SITE.new$V1[m], model.name = models.to.run.now[love], model.short = models.now[love], data = data)

      dev.off()
      # ggsave(file=paste(SENS_SITE.new$V1[m],"_",models.to.run.now[love],"_yearly_",uniqueyears[p],".svg",sep=""), plot=plotyearly, width=14, height=8)
    }

    # dev.off()

    # png(filename = paste(SENS_SITE.new$V1[m],"_",models.to.run.now[love],"_C_compare",".png",sep=""), width = 5, height = 5, units = "in", res = 300)

    svglite(paste(SENS_SITE.new$V1[m],"_",models.to.run.now[love],"_C_compare",".svg",sep=""), width = 5, height = 5, bg = "transparent", pointsize = 8)

    plot_error_Chat_compare(site.id =SENS_SITE.new$V1[m], site.name =SENS_SITE.new$SHORTNAME[m], model.name = models.to.run.now[love],model.short = models.now[love],data = data_all,index.C = indC)
    #
    dev.off()


    # rm(list = setdiff(ls(models.now,models.to.run.now,savepath,SENS_SITE,Batch), lsf.str()))
  }
}



    #############
    #####

    # output.summary <- c(SENS_SITE$V1[m], Chat.model.names, cmaes.results$best.fitness, AIC, NSE, RMSE, BFI)

    output.summary[(m-1)*length(models.to.run)+k,1] <- SENS_SITE$V1[m]
    output.summary[(m-1)*length(models.to.run)+k,2] <- Chat.model.names
    output.summary[(m-1)*length(models.to.run)+k,3] <- cmaes.results$best.fitness #negLL
    output.summary[(m-1)*length(models.to.run)+k,4] <- AIC
    output.summary[(m-1)*length(models.to.run)+k,5] <- NSE
    output.summary[(m-1)*length(models.to.run)+k,6] <- RMSE
    output.summary[(m-1)*length(models.to.run)+k,7] <- BFI


    # # Export
    write.csv(output.summary,file = paste0(SENS_SITE$V1[m],"_",Chat.model.names,"_output.summary",".csv", sep = ""), row.names = FALSE)
    # capture.output(para.list, file = paste0(SENS_SITE$V1[m],"_",Chat.model.names,"_parameters",".csv", sep = ""))

    #save current R data
    save.image(file=paste(foldername2,"/",SENS_SITE$V1[m],".RData",sep=""))
  }
}
    ####################### PLOTS ######################################################
    modelsEQ <- list(paste("C",noquote(models),":  negLL= ",round(cmaes.results$best.fitness,2),sep=""))

    # open pdf printer and create Chat plots, one year on each page
    pdf(paste(SENS_SITE$V1[m],"_",Chat.model.names,"_yearly.pdf",sep=""), width = 14, height = 8)

    for(p in 1:length(uniqueyears)){

      data <- data_all[data_all$year == uniqueyears[p], ]

      plot_yearly_Chat(site.id = SENS_SITE$V1[m], model.name = Chat.model.names, model.short = models, data = data)

      # Chat_ress <- data_all[[paste("eL",models,sep="")]]
    }
    plot_error_Chat(site.id = SENS_SITE$V1[m], model.name = Chat.model.names, model.short = models, Chat.residuals = data_all[[paste("eL",models,sep="")]])

    dev.off()

    ### ADD CQ scatter
    png(filename = paste(SENS_SITE$V1[m],"_",Chat.model.names,"_CQ_scatter_mmDay",".png",sep=""), width = 5, height = 5, units = "in", res = 300)

    plot_CQ_scatter(site.id =SENS_SITE$V1[m], site.name =SENS_SITE$SHORTNAME[m], model.name = Chat.model.names,model.short = models,data = data_all,index.C = indC)

    dev.off()

  ### Parameter plots
  # open pdf printer and create Chat/parameter plots, one year on each page
  # select parameter
  # param.name = "intercept"
  #
  # if(Chat.model.names == "Chat1_s"){
  #   if(param.name == "intercept"){
  #     data_all$intercept = para[[1]][1]^5*sin(para[[1]][2]+2*pi*(data_all$dtime-data_all$year))+para[[1]][3]^5
  #   }
  #
  # }else if(Chat.model.names == "Chat6_s"){
  #   if(param.name == "intercept"){
  #     data_all$intercept = para[[1]][3]^5*sin(para[[1]][4]+2*pi*(data_all$dtime-data_all$year))+para[[1]][5]^5
  #   }else if(param.name == "hysteresis"){
  #     data_all$hysteresis = para[[1]][7]^5*sin(para[[1]][8]+2*pi*(data_all$dtime-data_all$year))+para[[1]][9]^5
  #   }
  # }

    # pdf(paste(SENS_SITE$V1[m],"_",Chat.model.names,"_",param.name,"_yearly.pdf",sep=""), width = 14, height = 8)
    # for(p in 1:length(uniqueyears)){
    #
    #   data <- data_all[data_all$year == uniqueyears[p], ]
    #
    #   plot_yearly_Param(site.id = SENS_SITE$V1[m], model.name = Chat.model.names, model.short = models, param.name = param.name, data = data)
    #
    # }
    # dev.off()
    write.csv(para.summary, file = paste0(SENS_SITE$V1[m],"_",Chat.model.names,"_para.summary",".csv", sep = ""), row.names = FALSE)

    #save current R data
    save.image(file=paste(foldername2,"/",SENS_SITE$V1[m],".RData",sep=""))
  }


# save data
filename <- paste0(SENS_SITE$V1[m],"_data_all.rds", sep="")
saveRDS(data_all, file = file.path(foldername1, filename))
write.csv(data_all, file = paste0(SENS_SITE$V1[m],"_data_all",".csv", sep = ""), row.names = FALSE)

filename <- paste0(SENS_SITE$V1[m],"_output.summary.rds", sep="")
saveRDS(output.summary, file = file.path(foldername1, filename))
write.csv(output.summary,file = paste0(SENS_SITE$V1[m],"_output.summary",".csv", sep = ""), row.names = FALSE)

capture.output(para.list, file = paste0(SENS_SITE$V1[m],"_parameters",".csv", sep = ""))

}
setwd(savepath)
filename <- paste0("_output.summary.rds", sep="")
saveRDS(output.summary, file = file.path(foldername1, filename))
write.csv(output.summary,file = paste0("ALL_output.summary_open",".csv", sep = ""), row.names = FALSE)

capture.output(para.list, file = paste0("ALL_parameters",".csv", sep = ""))

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

