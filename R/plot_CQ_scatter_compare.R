plot_CQ_scatter_compare <- function(site.id,site.name, model.name1,model.name2,data){
  #if pdf...
  # par(mar = c(2, 2,2, 2))

  ylimC= c(min(log10(data$C),na.rm = TRUE), max(log10(data$C),na.rm = TRUE))
  xlimQ = c(-3, max(log10(data$Q),na.rm = TRUE))

  plot_CQ <- plot(log10(data$Q),log10(data$C),
       xlab="Discharge log10(mm/day) ", ylab="Salinity log10(mg/L)", pch=19, cex = .5, col = "darkgrey", xaxt ="n", yaxt = "n", cex.lab = 1.5, xlim = xlimQ, ylim = ylimC)

  #   ylim_2 = c(min((na.omit((Qp)))), max((na.omit((Qp))))+0.2)
  axis(side = 1, cex.axis = 1.5)
  axis(side = 2, cex.axis = 1.5)
  mtext(paste(site.id," ",site.name,sep=""), side =3,cex = 1.2, padj = -.5)

  par(new =TRUE)
  plot(log10(data$Q),log10(data[[model.name2]]), pch=19, cex = .5, col = "#fc8d59", xaxt ="n", yaxt = "n", xlab ="", ylab = "",xlim = xlimQ, ylim = ylimC)

  par(new = TRUE)
  plot(log10(data$Q),log10(data[[model.name1]]), pch=19, cex = .5, col = "#8856a7", xaxt ="n", yaxt = "n", xlab ="", ylab = "",xlim = xlimQ, ylim = ylimC)

  legend("bottomleft", legend = c("Obs. C", model.name1, model.name2),
         col = c("darkgrey","#8856a7","#fc8d59"), pch = 19, cex = 1)


  return(plot_CQ)

  dev.off()

}
