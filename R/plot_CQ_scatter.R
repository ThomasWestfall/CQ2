plot_CQ_scatter <- function(site.id,site.name, model.name,model.short,data,index.C){
  #if pdf...
  # par(mar = c(2, 2,2, 2))

  ylimC= c(min(log10(data$C[index.C])), max(log10(data$C[index.C])))
  xlimQ = c(min(log10(data$flow_mm_d[index.C])), max(log10(data$flow_mm_d[index.C])))

  plot_CQ <- plot(log10(data$flow_mm_d[index.C]),log10(C[index.C]),
       xlab="", ylab="", pch=19, cex = .5, col = "darkgrey", xaxt ="n", yaxt = "n", cex.lab = 1.5, xlim = xlimQ, ylim = ylimC)

  #   ylim_2 = c(min((na.omit((Qp)))), max((na.omit((Qp))))+0.2)
  axis(side = 1, cex.axis = 1.5)
  axis(side = 2, cex.axis = 1.5)
  mtext(paste(site.id," ",site.name,sep=""), side =3,cex = 1.2, padj = -.5)


  if(model.name == "Chat1"){
    par(new =TRUE)
    plot(log10(data$flow_mm_d[index.C]),log10(data[[paste("C",model.short,sep="")]])[index.C], pch=19, cex = .5, col = "#8856a7", xaxt ="n", yaxt = "n", xlab ="", ylab = "",xlim = xlimQ, ylim = ylimC)
    # abline(C.Q.lm, col="purple", lwd = 2)
  }else{
    par(new =TRUE)
    plot(log10(data$flow_mm_d[index.C]),log10(data[[paste("C",model.short,sep="")]])[index.C], pch=19, cex = .5, col = "#fc8d59", xaxt ="n", yaxt = "n", xlab ="", ylab = "",xlim = xlimQ, ylim = ylimC)

    par(new = TRUE)
    plot(log10(data$flow_mm_d[index.C]),log10(data[[paste("C1",sep="")]])[index.C], pch=19, cex = .5, col = "#8856a7", xaxt ="n", yaxt = "n", xlab ="", ylab = "",xlim = xlimQ, ylim = ylimC)

  }
  return(plot_CQ)

  dev.off()

}
