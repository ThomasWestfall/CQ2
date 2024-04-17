getBounds <- function(Chat.model.name, Bhat.model.name, Likelihood.name){

  # This function gets bounds (upper and lower) and start value for CMAES
  # based on selection of models...

  boundsList1 <- sapply("lower", function(x) vector(mode ='list'))
  boundsList2 <- sapply("upper", function(x) vector(mode ='list'))
  startsList <- sapply("start.value", function(x) vector(mode ='list'))
  boundsList <- c(startsList,boundsList1,boundsList2)
  boundsList <- sapply(Chat.model.name, function(x) list(boundsList))

  ################# one flow component models

  if(Chat.model.name == "Chat1" & Likelihood.name == "GaussLiklihood"){
    boundsList[[1]]$start.value = c(2,-.2,.1)
    boundsList[[1]]$lower = c(-5,-5,1e-7)
    boundsList[[1]]$upper = c(5,5,10)
  }

  if(Chat.model.name == "Chat1" & Likelihood.name == "GaussLiklihoodAR1"){
    boundsList[[1]]$start.value = c(3,-.2,9,.1)
    boundsList[[1]]$lower = c(-5,-5,1e-7,1e-7)
    boundsList[[1]]$upper = c(5,5,9.9999,10)
  }

  if(Chat.model.name == "Chat1" & Likelihood.name == "GaussLiklihoodBV1"){
    boundsList[[1]]$start.value = c(3,-.2,9,.1)
    boundsList[[1]]$lower = c(-5,-5,1e-7,1e-7)
    boundsList[[1]]$upper = c(5,5,9.9999,10)
  }

  if(Chat.model.name == "Chat1" & Likelihood.name == "GaussLiklihoodAR3"){
    boundsList[[1]]$start.value = c(1515,-.2,9,9,1e-6,.1)
    boundsList[[1]]$lower = c(1510,-5,1e-7,1e-7,1e-7,1e-7)
    boundsList[[1]]$upper = c(1520,5,10,10,1e-5,5)
  }

  # with seasonal terms...
  # if(Chat.model.name == "Chat1_s" & Likelihood.name == "GaussLiklihood"){
  #   boundsList[[1]]$start.value = c(2,-.2,3,-1,.1)
  #   boundsList[[1]]$lower = c(-7,-5,-5,-5,1e-7)
  #   boundsList[[1]]$upper = c(5,5,5,5,5)
  # }
  #
  # if(Chat.model.name == "Chat1_s" & Likelihood.name == "GaussLiklihoodAR1"){
  #   boundsList[[1]]$start.value = c(2,-.2,3,-1,9,.1)
  #   boundsList[[1]]$lower = c(-7,-5,-5,-5,1e-7,1e-7)
  #   boundsList[[1]]$upper = c(5,5,5,5,10,5)
  # }

  # with hystersis

  if(Chat.model.name == "Chat2" & Likelihood.name == "GaussLiklihood"){
    boundsList[[1]]$start.value = c(2,-0.3,4,0.5)
    boundsList[[1]]$lower = c(-5,-5,-5,1e-7)
    boundsList[[1]]$upper = c(5,5,5,10)
  }

  if(Chat.model.name == "Chat2" & Likelihood.name == "GaussLiklihoodAR1"){
    boundsList[[1]]$start.value = c(2,-0.3,4,9,0.5)
    boundsList[[1]]$lower = c(-5,-5,-5,1e-7,1e-7)
    boundsList[[1]]$upper = c(5,5,5,9.9999,10)
  }

  if(Chat.model.name == "Chat2" & Likelihood.name == "GaussLiklihoodBV1"){
    boundsList[[1]]$start.value = c(2,-0.3,4,9,0.5)
    boundsList[[1]]$lower = c(-5,-5,-5,1e-7,1e-7)
    boundsList[[1]]$upper = c(5,5,5,9.9999,10)
  }

 ############ two flow component models from the literature

  if(Chat.model.name == "Chat3" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihood"){
    boundsList[[1]]$start.value = c(9,8,2,-0.3,2,-1,0.5)
    boundsList[[1]]$lower = c(1e-7,1e-7,-6,-5,-6,-5,1e-7)
    boundsList[[1]]$upper = c(9.9999,9.9999,4,5,4,5,10)
  }

  if(Chat.model.name == "Chat3" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihoodAR1"){
    boundsList[[1]]$start.value = c(9,8,2,-0.3,2,-1,9,0.5)
    boundsList[[1]]$lower = c(1e-7,1e-7,-6,-5,-6,-5,1e-7,1e-7)
    boundsList[[1]]$upper = c(9.9999,9.9999,4,5,4,5,9.9999,10)
  }

  if(Chat.model.name == "Chat4" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihood"){
    boundsList[[1]]$start.value = c(9,8,2,-0.3,2,-1,0.5)
    boundsList[[1]]$lower = c(1e-7,1e-7,-6,-5,-6,-5,1e-7)
    boundsList[[1]]$upper = c(9.9999,9.9999,4,5,4,5,10)
  }

  if(Chat.model.name == "Chat4" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihoodAR1"){
    boundsList[[1]]$start.value = c(9,8,2,-0.3,2,-1,9,0.5)
    boundsList[[1]]$lower = c(1e-7,1e-7,-6,-5,-6,-5,1e-7,1e-7)
    boundsList[[1]]$upper = c(9.9999,9.9999,4,5,4,5,9.9999,10)
  }

  if(Chat.model.name == "Chat5" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihood"){
    boundsList[[1]]$start.value = c(9,8,3,-0.3,-1,0.5)
    boundsList[[1]]$lower = c(1e-7,1e-7,-6,-5,-5,1e-7)
    boundsList[[1]]$upper = c(9.9999,9.9999,4,5,5,10)
  }

  if(Chat.model.name == "Chat5" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihoodAR1"){
    boundsList[[1]]$start.value = c(9,8,3,-0.3,-1,9,0.5)
    boundsList[[1]]$lower = c(1e-7,1e-7,-6,-5,-5,1e-7,1e-7)
    boundsList[[1]]$upper = c(9.9999,9.9999,4,5,5,9.9999,10)
  }

################ two flow component variants derived for this study based on "simple CQ"

  if(Chat.model.name == "Chat6" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihood"){
    boundsList[[1]]$start.value = c(9,8,-0.3,2,0.5)
    boundsList[[1]]$lower = c(1e-7,1e-7,-5,-7,1e-7)
    boundsList[[1]]$upper = c(9.9999,9.9999,5,4,10)
  }

  if(Chat.model.name == "Chat6" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihoodAR1"){
    boundsList[[1]]$start.value = c(9,8,-0.3,2,9,0.5)
    boundsList[[1]]$lower = c(1e-7,1e-7,-5,-5,1e-7,1e-7)
    boundsList[[1]]$upper = c(9.9999,9.9999,5,5,9.9999,10)
  }

  if(Chat.model.name == "Chat7" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihood"){
    boundsList[[1]]$start.value = c(9,8,-0.3,2,2,0.5)
    boundsList[[1]]$lower = c(1e-7,1e-7,-5,-5,-5,1e-7)
    boundsList[[1]]$upper = c(9.9999,9.9999,5,5,5,10)
  }

  if(Chat.model.name == "Chat7" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihoodAR1"){
    boundsList[[1]]$start.value = c(9,8,-0.3,2,2,9,0.5)
    boundsList[[1]]$lower = c(1e-7,1e-7,-5,-5,-5,1e-7,1e-7)
    boundsList[[1]]$upper = c(9.9999,9.9999,5,5,5,9.9999,10)
  }

  if(Chat.model.name == "Chat8" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihood"){
    boundsList[[1]]$start.value = c(9,8,-0.3,2,2,0.5)
    boundsList[[1]]$lower = c(1e-7,1e-7,-5,-5,-5,1e-7)
    boundsList[[1]]$upper = c(9.9999,9.9999,5,5,5,10)
  }

  if(Chat.model.name == "Chat8" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihoodAR1"){
    boundsList[[1]]$start.value = c(9,8,-0.3,2,2,9,0.5)
    boundsList[[1]]$lower = c(1e-7,1e-7,-5,-5,-5,1e-7,1e-7)
    boundsList[[1]]$upper = c(9.9999,9.9999,5,5,5,9.9999,10)
  }

  if(Chat.model.name == "Chat9" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihood"){
    boundsList[[1]]$start.value = c(9,8,-0.3,2,2,2,0.5)
    boundsList[[1]]$lower = c(1e-7,1e-7,-5,-5,-5,-5,1e-7)
    boundsList[[1]]$upper = c(9.9999,9.9999,5,5,5,5,10)
  }

  if(Chat.model.name == "Chat9" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihoodAR1"){
    boundsList[[1]]$start.value = c(9,8,-0.3,2,2,2,9,0.5)
    boundsList[[1]]$lower = c(1e-7,1e-7,-5,-5,-5,-5,1e-7,1e-7)
    boundsList[[1]]$upper = c(9.9999,9.9999,5,5,5,5,9.9999,10)
  }

  if(Chat.model.name == "Chat10" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihood"){
    boundsList[[1]]$start.value = c(9,8,-0.3,4,2,1,0.5)
    boundsList[[1]]$lower = c(1e-7,1e-7,-5,-5,-5,-5,1e-7)
    boundsList[[1]]$upper = c(9.9999,9.9999,5,5,5,5,10)
  }

  if(Chat.model.name == "Chat10" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihoodAR1"){
    boundsList[[1]]$start.value = c(9,8,-0.3,4,2,1,9,0.5)
    boundsList[[1]]$lower = c(1e-7,1e-7,-5,-5,-5,-5,1e-7,1e-7)
    boundsList[[1]]$upper = c(9.9999,9.9999,5,5,5,5,9.9999,10)
  }

  if(Chat.model.name == "Chat11" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihood"){
    boundsList[[1]]$start.value = c(9,8,2,4,2,1,2,0.5)
    boundsList[[1]]$lower = c(1e-7,1e-7,-5,-5,-5,-5,-5,1e-7)
    boundsList[[1]]$upper = c(9.9999,9.9999,5,5,5,5,5,10)
  }

  if(Chat.model.name == "Chat11" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihoodAR1"){
    boundsList[[1]]$start.value = c(9,8,-0.3,4,2,1,2,9,0.5)
    boundsList[[1]]$lower = c(1e-7,1e-7,-5,-5,-5,-5,-5,1e-7,1e-7)
    boundsList[[1]]$upper = c(9.9999,9.9999,5,5,5,5,5,9.9999,10)
  }


############## Johnson 1969 Models

  if(Chat.model.name == "Chat12" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihood"){
    boundsList[[1]]$start.value = c(-0.3,1,1,0.5)
    boundsList[[1]]$lower = c(-5,-5,-5,1e-6)
    boundsList[[1]]$upper = c(5,5,5,10)
  }

  if(Chat.model.name == "Chat12" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihoodAR1"){
    boundsList[[1]]$start.value = c(-0.3,1,1,9,0.5)
    boundsList[[1]]$lower = c(-5,-5,-5,1e-7,1e-6)
    boundsList[[1]]$upper = c(5,5,5,9.9999,10)
  }

# with two flow components derived for this study

  if(Chat.model.name == "Chat13" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihood"){
    boundsList[[1]]$start.value = c(9,8,-0.3,1,1,1,1,0.5)
    boundsList[[1]]$lower = c(1e-7,1e-7,-5,-5,-5,-5,-5,1e-6)
    boundsList[[1]]$upper = c(9.9999,9.9999,5,5,5,5,5,10)
  }

  if(Chat.model.name == "Chat13" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihoodAR1"){
    boundsList[[1]]$start.value = c(9,8,-0.3,1,1,1,1,9,0.5)
    boundsList[[1]]$lower = c(1e-7,1e-6,-5,-5,-5,-5,-5,1e-7,1e-6)
    boundsList[[1]]$upper = c(9.9999,9.9999,5,5,5,5,5,9.9999,10)
  }

############# Hall 1970 Models ("model 5")

  if(Chat.model.name == "Chat14" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihood"){
    boundsList[[1]]$start.value = c(-0.3,1,1,1,0.5)
    boundsList[[1]]$lower = c(-5,-5,-5,-5,1e-7)
    boundsList[[1]]$upper = c(5,5,5,5,10)
  }

  if(Chat.model.name == "Chat14" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihoodAR1"){
    boundsList[[1]]$start.value = c(-0.3,1,1,1,9,0.5)
    boundsList[[1]]$lower = c(-5,-5,-5,-5,1e-7,1e-7)
    boundsList[[1]]$upper = c(5,5,5,5,9.9999,10)
  }

  # with two flow components derived for this study

  if(Chat.model.name == "Chat15" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihood"){
    boundsList[[1]]$start.value = c(9,8,-0.3,4,1,1,1,1,1,0.5)
    boundsList[[1]]$lower = c(1e-7,1e-7,-5,-5,-4,-4,-4,-5,-5,1e-7)
    boundsList[[1]]$upper = c(9.9999,9.9999,5,5,6,6,6,5,5,10)
  }

  if(Chat.model.name == "Chat15" & Bhat.model.name == "BhatEck" & Likelihood.name == "GaussLiklihoodAR1"){
    boundsList[[1]]$start.value = c(9,8,-0.3,1,1,1,1,1,1,9,0.5)
    boundsList[[1]]$lower = c(1e-7,1e-7,-5,-5,-4,-4,-4,-5,-5,1e-7,1e-7)
    boundsList[[1]]$upper = c(9.9999,9.9999,5,5,6,6,6,5,5,9.9999,10)
  }

  return(boundsList)
}
