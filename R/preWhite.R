preWhite <- function(input_data, acf_target, site.id, period){

  pdf(paste(site.id,"_WhiteningPlots","_",period,".pdf",sep=""))
  par(mfrow = c(2, 2))

  # plot initial
  plot(input_data[2:length(input_data)],input_data[1:(length(input_data)-1)], xlab = "ln(error) t", ylab = "ln(error) t-1")
  mtext(paste(site.id," ",period,sep=""),padj = -1) #,inset=c(0, -0.35))

  # test initial auto-correlation structure
  input_data_acf = acf(input_data, na.action = na.pass, plot = T)

  # AR1 coefficient
  acf_level = input_data_acf$acf[2]


  # if AR1 coefficient > acf_target, then iterate through whitining
  if(acf_level > acf_target){

    pre_white_data = input_data

    niter = 0
    while(acf_level > acf_target){

      niter = niter + 1

      if(niter == 1){

        data_acf = acf(pre_white_data, na.action = na.pass,plot = F)

        acf_level = data_acf$acf[2]
      }

      # pre white
      pre_white_data = pre_white_data[2:(length(pre_white_data))] - acf_level * pre_white_data[1:(length(pre_white_data)-1)]

      data_acf = acf(pre_white_data, na.action = na.pass,plot = F)

      acf_level = data_acf$acf[2]

    }

    } else{
      niter = 0
      pre_white_data = input_data
    }

  # plot final
  plot(pre_white_data[2:length(pre_white_data)],pre_white_data[1:(length(pre_white_data)-1)], xlab = "ln(error) t", ylab = "ln(error) t-1")
  # mtext(paste(site.id," ",period,sep=""),padj = -1) #,inset=c(0, -0.35))
  mtext(paste("AR",niter,"  ","final lag-1 coeff: ",round(acf_level,3),sep=""),padj = 0)

  acf(pre_white_data, na.action = na.pass,plot = T)

  dev.off()

  return(list(pre_white_data,niter,acf_level))
}
