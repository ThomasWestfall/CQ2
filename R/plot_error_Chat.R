plot_error_Chat <- function(site.id,model.name, model.short, Chat.residuals){

  if(is.finite(max(Chat.residuals, na.rm = TRUE)) & is.finite(min(Chat.residuals, na.rm = TRUE))){
  # error plots
  # Chat.residuals <- ifelse(is.finite(Chat.residuals),Chat.residuals,0) # sometimes starting value is inf, if flow is zero
  plot.correlation <- plot(Chat.residuals[2:NROW(data)],Chat.residuals[1:(NROW(data)-1)], xlab = "ln(error) t", ylab = "ln(error) t-1")
  mtext(paste(site.id," ", model.name,sep=""),padj = -1) #,inset=c(0, -0.35))

  #qq
  plot.qq <- qqnorm(Chat.residuals, pch = 1, frame = FALSE)
  qqline(Chat.residuals, col = "steelblue", lwd = 2)
  mtext(paste(site.id," ", model.name,sep=""),padj = -1)

  plot.hist <- hist(Chat.residuals)
  mtext(paste(site.id," ", model.name,sep=""),padj = -1)

  return(list(plot.correlation,plot.qq,plot.hist))

  dev.off()

  }else{
    return(print("non-finite residuals"))
  }
}
