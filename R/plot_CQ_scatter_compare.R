plot_CQ_scatter_compare <- function(site.id,site.name, model.name1,model.name2,data){
  #if pdf...
  # par(mar = c(2, 2,2, 2))

  ylimC= c(min(log10(data$C),na.rm = TRUE), max(log10(data$C),na.rm = TRUE))
  xlimQ = c(-5, max(log10(data$Q),na.rm = TRUE))

  plot(log10(data$Q),log10(data$C),
       xlab="Discharge log10(mm/day) ", ylab="Salinity log10(mg/L)", pch=19, cex = .5, col = "darkgrey", xaxt ="n", yaxt = "n", cex.lab = 1.5, xlim = xlimQ, ylim = ylimC)


  axis(side = 1, cex.axis = 1.5)
  axis(side = 2, cex.axis = 1.5)
  mtext(paste(site.id," ",site.name,sep=""), side =3,cex = 1.2, padj = -.5)
  # # # "#238b45","#66c2a4","#b2e2e2
  par(new =TRUE)
  plot(log10(data$Q),log10(data[[model.name2]]), pch=19, cex = .5, col = "#238b45", xaxt ="n", yaxt = "n", xlab =" ", ylab = "",xlim = xlimQ, ylim = ylimC)
       # xlab="Observed log10(mg/L) ", ylab="Estimated log10(mg/L)")
  ##67a9cf
  ##ef8a62
  # par(new = TRUE)
  # plot(log10(data$Q),log10(data[[model.name1]]), pch=19, cex = .5, col = "black", xaxt ="n", yaxt = "n", xlab ="", ylab = "",xlim = xlimQ, ylim = ylimC)

  # # obs vs. predicted
  # plot(log10(data$C),log10(data[[model.name1]]), pch=19, cex = .5, col = "black", xaxt ="n", yaxt = "n", xlab ="", ylab = "",xlim = ylimC, ylim = ylimC)
  #
  # par(new = TRUE)
  # plot(log10(data$C),log10(data[[model.name2]]), pch=19, cex = .5, col = "#b2e2e2", xlab ="Observed log10(mg/L) ", ylab = "Estimated log10(mg/L)",xlim = ylimC, ylim = ylimC)
  #
  # abline(coef = c(0,1), lty =1, lwd = 2)
  # legend("bottomleft", legend = c("Obs. C", model.name1, model.name2),
         # col = c("darkgrey","#8856a7","#fc8d59"), pch = 19, cex = 1)


  # return(plot_CQ)

  # dev.off()

}
