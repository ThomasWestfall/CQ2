plot_error_Chat_compare <-function(site.id,site.name, model.name,model.short,data,index.C){

  #plot compare
  ylimC= c(min(log10(data[[paste("C",model.short,sep="")]])[index.C],log10(data$C[index.C]), na.rm = TRUE),
          max(log10(data[[paste("C",model.short,sep="")]])[index.C], na.rm = TRUE))

  if(model.name == "Chat1"){
    color_label = "#8856a7"
  }else{
    color_label = "#fc8d59"
  }

  plot_compare <- plot(log10(data$C[index.C]),log10(data[[paste("C",model.short,sep="")]])[index.C],
                  xlab="Observed (log10 - mg/L)", ylab="Simulated (log10 - mg/L)", pch=19, cex = .5, col = color_label, xaxt ="n", yaxt = "n", cex.lab = 1.5, xlim = ylimC, ylim = ylimC)


    axis(side = 1, cex.axis = 1.5)
    axis(side = 2, cex.axis = 1.5)
    mtext(paste(site.id," ",site.name,sep=""), side =3,cex = 1.2, padj = -.5)

    # r_squared <- round(cor(data$C[index.C],data[[paste("C",model.short,sep="")]][index.C])^2,2)

    RMSE <- round(sqrt(sum((data$C[index.C] - data[[paste("C",model.short,sep="")]][index.C])^2)/sum(index.C)),2)



    abline(a=0, b=1, col = "black", cex = 1,lty = 5)
    text(x = ylimC[2]-0.2, y = ylimC[1]+0.1 , labels = paste("RMSE: ", RMSE,sep=""),cex = 1.5,col = color_label)
    text(x = ylimC[1]+0.1, y = ylimC[2]-0.1 , labels = model.name,cex = 1.5,col = color_label)

    print(RMSE)

  return(plot_compare)

  dev.off()

}
