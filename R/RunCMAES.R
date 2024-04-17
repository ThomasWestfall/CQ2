RunCMAES <- function(sigma.cmaes, lamda.cmaes, Nrestarts, restart.multi, objective_function, Chat.model.names, bounds){

  ## Description of myStopOnTolx... changed from cmaesr package, because cmaesr "tol" only accepts integers... shared by Lucas Pamminger ##
  myStopOnTolX = function(tol = 1e-12) {
    assertNumber(tol, na.ok = FALSE) # this bit is changed
    force(tol)
    return(makeStoppingCondition(
      name = "tolX",
      message = sprintf("Standard deviation below tolerance in all coordinates."),
      stop.fun = function(envir = parent.frame()) {
        return(all(envir$D <tol) && all((envir$sigma * envir$p.c) < tol))
      }
    ))
  }


  # set control list for cmaes
  control.cmaes = list(
    sigma = sigma.cmaes,
    lambda = lamda.cmaes,
    restart.triggers = c("tolX", "indefCovMat"), #"noEffectAxis", "noEffectCoord"), #), #  "indefCovMat", # "conditionCov","tolX"),
    restart.multiplier = restart.multi,
    max.restarts = Nrestart,
    stop.ons = c(
      list(
        stopOnMaxIters(100000L)),
        getDefaultStoppingConditions()
        # myStopOnTolX(tol=1e-12),
        # stopOnNoEffectAxis(),
        # stopOnNoEffectCoord(),
        # stopOnCondCov()) #,getDefaultStoppingConditions())
  ))

  cmaes.result <- cmaes(objective_function,
                        start.point = bounds[[noquote(Chat.model.names)]]$start.value,
                        control = control.cmaes)

  return(cmaes.result)

}
