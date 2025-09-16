plot_yearly_Chat_model <- function(site.id,model.name1,data_all){

  op <- par(no.readonly = T)
  on.exit(par(op))

  #unique years & drought index
  uniqueyears <- unique(data_all$year)

  #open pdf printer and create plots
  pdf(paste(site.id,"_",model.name1,"_yearly.pdf",sep=""), width = 11, height = 8)
  par(mar = c(6, 7,8, 8)) #margin
  #1 plot on each page
  # layout_mat <- matrix(c(1,2), nrow = 2, ncol = 1,byrow = TRUE)
  # my_lay <- layout(mat = layout_mat, heights = c(4,4),widths = c(5,5), respect =TRUE)

  for(p in 1:length(uniqueyears)){
    data <- data_all[data_all$year == uniqueyears[p], ]
    Qp <- data$Q
    Cp <- data$C
    if(all(is.na(Cp))){
      return() #skip if all observed data in the year is na
    }

    #set up axis and limits
    datetime <- as.Date(ISOdate(data$year,data$month,data$day))
    xlim.date = as.Date(c(paste(uniqueyears[p],"-01-01", sep = ""),paste(uniqueyears[p],"-12-31",sep = "")))
    ylim.C = c(min(na.omit(Cp)), max(na.omit(Cp)))
    # ylim.3 = c(min(na.omit(data[[noquote(model.name)]])),max(na.omit(data[[noquote(model.name)]])))
    ylim.Q = c((min((na.omit((Qp))))), (max((na.omit((Qp))))))
    if(ylim.Q[1] == -Inf){ylim.Q[1] <- -4}
    month_lab <- seq(xlim.date[1], xlim.date[2], by = "month")

    # add baseflow to plot
    # par(new=TRUE)
    if(model.name1 %in% c("C3","C4","C5","C6","C7","C8","C9","C10","C11","C13","C15")){
      pframe <- data.frame(datetime,(data[[paste("Q",sub('.','', model.name1),sep="")]]))
      pframe <- pad(pframe,interval = "day")
      plot(pframe, type="l", col="#9ecae1", axes = FALSE, ylim = ylim.Q ,xlim=xlim.date, ylab="", xlab="", lwd=2)
      polygon(x = c(min(as.Date(pframe$datetime), na.rm = TRUE), as.Date(pframe$datetime,na.rm = TRUE),max(as.Date(pframe$datetime), na.rm = TRUE)),
              y = c(-4,pframe[[2]],-4),
              border = NA, col = adjustcolor("#9ecae1", alpha.f=0.7))

      par(new=TRUE)
    }
    # plot Obs C, pframe used due to gaps in the data
    pframe <- data.frame(datetime,Cp)
    pframe <- pad(pframe,interval = "day")
    output.plot <- plot(pframe, type='l',col='darkgrey', lwd=1.5, ylim = ylim.C, xlim = xlim.date, xlab= paste(uniqueyears[p]), ylab='Salinity (mg/L)',xaxt='n', cex.lab = 1, cex.axis = 1)
    axis(cex.axis = 1,side = 1, at = month_lab, labels = format(month_lab, "%b"))
    abline(v = month_lab, col = "lightgray", lty = "dotted")

    # plot flow, Q, on same plot
    par(new=TRUE)
    pframe <- data.frame(datetime,(Qp))
    pframe <- pad(pframe,interval = "day")
    plot(pframe, type="l", col="#0072B4", xlim = xlim.date, ylim = ylim.Q, axes = FALSE, xlab="",ylab = "", lwd=2, cex.axis = 1,cex.lab = 1, las = 1)
    mtext("Discharge (mm/day)", cex = 1, side = 4, line = 5)
    axis(side = 4, cex.axis = 1, las=1, labels=TRUE)

    # plot model one
    par(new=TRUE)
    pframe <- data.frame(datetime,data[[model.name1]])
    pframe <- pad(pframe,interval = "day")
    plot(pframe, type="l", lty = 1,col="#8856a7", axes = FALSE, ylim = ylim.C, xlim=xlim.date, ylab="", xlab="", lwd=2)
    # mtext(paste(model.name1, side =3,cex = 1.5, padj = -2.5))

    if(model.name1 %in% c("C3","C4","C5","C6","C7","C8","C9","C10","C11","C13","C15")){

      legend(x = "top", y = par("usr")[4] - 0.1, legend = c("Obs. C",model.name1,"Q",paste("Q",sub('.','', model.name1),sep="")),
           col = c("darkgray","#8856a7","#0072B4","#9ecae1"),lty = c(1,1,1,1),lwd=1, pt.cex = 0.1, pch = 16,cex = 1,
           ncol=4, inset=c(0, -.1), xpd=TRUE)
    }else{

      legend(x = "top", y = par("usr")[4] - 0.1, legend = c("Obs. C",model.name1,"Q"),
             col = c("darkgray","#8856a7","#0072B4"),lty = c(1,1,1),lwd=1, pt.cex = 0.1, pch = 16,cex = 1,
             ncol=3, inset=c(0, -.1), xpd=TRUE)

    }


    title(paste(site.id," ",model.name1,sep=""))





  }
  dev.off()

  return(output.plot)
}
