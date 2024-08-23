plot_error_Chat <- function(site.id,model.name, model.short, Chat.residuals, flow){

  if(is.finite(max(Chat.residuals, na.rm = TRUE)) & is.finite(min(Chat.residuals, na.rm = TRUE))){
  # error plots
  # Chat.residuals <- ifelse(is.finite(Chat.residuals),Chat.residuals,0) # sometimes starting value is inf, if flow is zero

  svglite(paste(site.id,"_",model.name,"_Correlation",".svg",sep=""), width = 3, height = 3, bg = "transparent", pointsize = 8)

    # par(mar = c(0.5, 0.5,0.5, 0.5))
  plot.correlation <- plot(Chat.residuals[2:NROW(Chat.residuals)],Chat.residuals[1:(NROW(Chat.residuals)-1)], xlab = "ln(error) t", ylab = "ln(error) t-1")
  mtext(paste(site.id," ", model.name,sep=""),padj = -1) #,inset=c(0, -0.35))
  dev.off()

  # ACF
  svglite(paste(site.id,"_",model.name,"_ACF",".svg",sep=""), width = 3, height = 3, bg = "transparent", pointsize = 8)

  # par(mar = c(0.5, 0.5,0.5, 0.5))
  plot.acf = acf(Chat.residuals, na.action = na.pass, col ="black")
  title(main = NULL)
  # axis(side = 1, cex.axis = 2, las=1, labels=TRUE)
  # axis(side = 2, cex.axis = 2, las=1, labels=TRUE)
  mtext(paste(site.id," ", model.name,sep=""),padj = -1)
  dev.off()

  #qq
  svglite(paste(site.id,"_",model.name,"_QQ",".svg",sep=""),  width = 3, height = 3, bg = "transparent", pointsize = 8)

  # par(mar = c(0.5, 0.5,0.5, 0.5))
  plot.qq <- qqnorm(Chat.residuals,ylim = c(-1.5,2), frame = TRUE)
  qqline(Chat.residuals, col = "steelblue", lwd = 2)
  mtext(paste(site.id," ", model.name,sep=""),padj = -1)
  dev.off()

  svglite(paste(site.id,"_",model.name,"_RQ",".svg",sep=""),  width = 3, height = 3, bg = "transparent", pointsize = 8)

  # par(mar = c(0.5, 0.5,0.5, 0.5))
  x.limit = c(-2, 2)
  y.limit = c(-1.5, 2)
  plot.RQ = plot(log10(flow), Chat.residuals, xlim = x.limit, ylim = y.limit, col = "black", pch = 20, ylab = "Residuals", xlab = "Discharge (log10 - mm/day)", cex =.5)
  abline(h=0, col = "#d7191c", lty = 2)
  dev.off()

  svglite(paste(site.id,"_",model.name,"_RQ",".svg",sep=""),  width = 3, height = 3, bg = "transparent", pointsize = 8)

  # par(mar = c(0.5, 0.5,0.5, 0.5))
  x.limit = c(-2, 2)
  y.limit = c(-1.5, 2)
  plot.RQ = plot(log10(flow), Chat.residuals, xlim = x.limit, ylim = y.limit, col = "black", pch = 20, ylab = "Residuals", xlab = "Discharge (log10 - mm/day)", cex =.5)
  abline(h=0, col = "#d7191c", lty = 2)
  dev.off()


  svglite(paste(site.id,"_",model.name,"_histogram",".svg",sep=""),  width = 3, height = 3, bg = "transparent", pointsize = 8)

  # par(mar = c(0.5, 0.5,0.5, 0.5))
  plot.hist <- hist(Chat.residuals)
  mtext(paste(site.id," ", model.name,sep=""),padj = -1)
  dev.off()

  return(list(plot.correlation,plot.acf,plot.qq,plot.RQ,plot.hist))

  dev.off()

  }else{
    return(print("non-finite residuals"))
  }
}
