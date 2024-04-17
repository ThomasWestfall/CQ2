plot_yearly_Param <- function(site.id,model.name,model.short,param.name,data){

  #unique years & drought index
  # uniqueyears <- unique(data.all$year)

  #open pdf printer and create plots
  # pdf(paste(site.id,"_",model.name,"_yearly.pdf",sep=""), width = 14, height = 8)
    par(mar = c(6, 7,8, 8)) #margin
  #1 plot on each page
  # layout_mat <- matrix(c(1,2), nrow = 2, ncol = 1,byrow = TRUE)
  # my_lay <- layout(mat = layout_mat, heights = c(4,4),widths = c(5,5), respect =TRUE)

  # for(p in 1:length(uniqueyears)){
    # data <- data.all[data.all$year == uniqueyears[p], ]
    Qp <- data$flow_mm_d
    Cp <- data$C
    if(all(is.na(Cp))){
      return()} #skip if all observed data in the year is na


    #set up axis and limits
    datetime <- as.Date(ISOdate(data$year,data$month,data$day))
    xlim.date = as.Date(c(paste(uniqueyears[p],"-01-01", sep = ""),paste(uniqueyears[p],"-12-31",sep = "")))
    ylim.C = c(min(na.omit(Cp)), max(na.omit(Cp)))
    ylim.3 = c(min(na.omit(data[[param.name]][is.finite(data[[param.name]])])),max(na.omit(data[[param.name]][is.finite(data[[param.name]])])))
    ylim.Q = c(min((na.omit((Qp))))-0.2, max((na.omit((Qp))))+0.2)
    if(ylim.Q[1] == -Inf){ylim.Q[1] <- -4}
    month_lab <- seq(xlim.date[1], xlim.date[2], by = "month")

    # plot Obs C, pframe used due to gaps in the data
    pframe <- data.frame(datetime,Cp)
    pframe <- pad(pframe,interval = "day")
    output.plot <- plot(pframe, type='l',col='darkgrey', lwd=3, ylim = ylim.C, xlim = xlim.date, xlab= paste(uniqueyears[p]), ylab='',xaxt='n', cex.lab = 1.8, cex.axis = 1.8)
    axis(cex.axis = 2,side = 1, at = month_lab, labels = format(month_lab, "%b"))
    abline(v = month_lab, col = "lightgray", lty = "dotted")

    # plot flow, Q, on same plot
    par(new=TRUE)
    pframe <- data.frame(datetime,(Qp))
    pframe <- pad(pframe,interval = "day")
    plot(pframe, type="l", col="#0072B4", xlim = xlim.date, ylim = ylim.Q, axes = FALSE, xlab="",ylab = "", lwd=2.5, cex.axis = 1.8,cex.lab = 1.8, las = 1)
    mtext("Discharge (mm/day)", cex = 1.8, side = 4, line = 5)
    axis(side = 4, cex.axis = 2, las=1, labels=TRUE)

    # plot Chat1 Pred from model
    par(new=TRUE)
    pframe <- data.frame(datetime,data[[paste("C",model.short,sep="")]])
    pframe <- pad(pframe,interval = "day")
    plot(pframe, type="l", lty = 1,col="#8856a7", axes = FALSE, ylim = ylim.C, xlim=xlim.date, ylab="", xlab="", lwd=3)
    # mtext(paste(model.name, side =3,cex = 1.5, padj = -2.5))

    # plot parameter
    par(new=TRUE)
    pframe <- data.frame(datetime,data[[param.name]])
    pframe <- pad(pframe,interval = "day")
    plot(pframe, type="l", lty = 2,col="#1a9850", axes = FALSE, ylim = ylim.3, xlim=xlim.date, ylab="", xlab="", lwd=3)
    mtext(param.name, cex = 2, side = 3, line = 1, col = "#1a9850", padj = -1)
    axis(side = 2, cex.axis = 1, las=1, labels=TRUE, line = 3, col = "#1a9850", col.axis = "#1a9850")
    # mtext(paste(model.name, side =3,cex = 1.5, padj = -2.5))



    if(model.name ==  "Chat1" || model.name == "Chat1_s"){

      legend(x = "top", y = par("usr")[4] + 0.1, legend = c("Obs. C",model.name,"Q",param.name),
             col = c("darkgray","#8856a7","#0072B4","#1a9850"),lty = c(1,1,1),lwd=3, pt.cex = 0.1, pch = 16,cex = 1.2,
             ncol=4, inset=c(0, -0.0), xpd=TRUE)

    }else{

      # plot Chat# Pred from model
      par(new=TRUE)
      pframe <- data.frame(datetime,data[[paste("C",model.short,sep="")]])
      pframe <- pad(pframe,interval = "day")
      plot(pframe, type="l", lty = 1,col="#fc8d59", axes = FALSE, ylim = ylim.C, xlim=xlim.date, ylab="", xlab="", lwd=3)
      # mtext(model.name, side =3,cex = 1.5, padj = -2.5)

      # add baseflow to plot
      par(new=TRUE)
      pframe <- data.frame(datetime,data[[paste("Q",model.short,sep="")]])
      pframe <- pad(pframe,interval = "day")
      plot(pframe, type="l", col="#C3DBFD", axes = FALSE, ylim = ylim.Q ,xlim=xlim.date, ylab="", xlab="", lwd=2.5)


      legend(x = "top", y = par("usr")[4] + 0.1, legend = c("Obs. C","Chat1",model.name,"Q",paste("Q",model.short,sep=""),param.name),
             col = c("darkgray","#8856a7","#fc8d59","#0072B4","#C3DBFD", "#1a9850"),lty = c(1,1,1,1,1,1),lwd=3, pt.cex = 0.1, pch = 16,cex = 1.2,
             ncol=6, inset=c(0, -0.0), xpd=TRUE)
    }

    title(paste(site.id," ",model.name,sep=""))


    # Get input grapics settings
    op <- par(no.readonly = T)
    # reset graphics
    par(op)

  return(output.plot)
}

    # #error plot
    # pframe <- data.frame(datetime,Cp)
    # pframe <- pad(pframe,interval = "day")
    # plot(pframe, type='l',col='darkgrey', lwd=3, ylim = ylim_, xlim = xlim_,xlab= paste(uniqueyears[p]), ylab='Salinity (mg/L)',xaxt='n', cex.lab = 2, cex.axis = 2)
    # axis(cex.axis = 2,side = 1, at = month_lab, labels = format(month_lab, "%b"))
    # abline(v = month_lab, col = "lightgray", lty = "dotted")
    #
    # # add Chat line graph log
    # par(new=TRUE)
    # pframe <- data.frame(datetime,data[[paste("C",models[k],sep="")]])
    # pframe <- pad(pframe,interval = "day")
    # plot(pframe, type="l", lty = 1, col="gold", axes = FALSE, ylim = ylim_,xlim=xlim_, ylab="", xlab="", lwd=3)
    # mtext(paste(modelsEQ[[k]]), side =3,cex = 1.2)
    #
    # #add error
    # ylim_3 <- c(min(data[[paste("eL",models[k],sep="")]],na.rm = TRUE),max(data[[paste("eL",models[k],sep="")]],na.rm = TRUE))
    # ylim_3[1] <- ifelse(is.finite(ylim_3[1]),ylim_3[1],-2)
    # ylim_3[2] <- ifelse(is.finite(ylim_3[2]),ylim_3[2],2)
    # par(new=TRUE)
    # pframe <- data.frame(datetime,data[[paste("eL",models[k],sep="")]])
    # pframe <- pad(pframe,interval = "day")
    # plot(pframe, pch =19, col="#D24E71", xlim = xlim_, ylim = ylim_3, axes = FALSE, xlab="",ylab = "", lwd=3, cex.axis = 2,cex.lab = 2, las = 1)
    # mtext("Residuals-ln", cex = 2, side = 4, line = 5)
    # axis(side = 4, cex.axis = 2, las=1, labels=TRUE)
    # abline(h = 0, col = "black", lty = 2, lwd = 2)
    # #
    # legend(x = "top", y = par("usr")[4] + 0.1, legend = c("Obs. C",paste("C",models[k],sep=""),"Residuals"),
    #        col = c("darkgray","gold","#D24E71"),lty = c(1,1,0),pch = c(NA,NA,19),lwd=3, pt.cex = 1,cex = 1.2,
    #        ncol=3, inset=c(0, -0.0), xpd=TRUE)
    # title(paste(SENS_SITE$V1[m]," Chat",models[k],sep=""))



