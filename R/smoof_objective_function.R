smoof_objective_function <- function(objective_function_factory,Chat.model.names, bounds){

  # force(Chat.model.name)

  smoof_function <- smoof::makeSingleObjectiveFunction(name = "PLACEHOLDER",
                              description = "PLACEHOLDER",
                              fn = objective_function_factory,
                              vectorize = TRUE,
                              par.set = ParamHelpers::makeNumericParamSet(
                                lower = bounds[[noquote(Chat.model.names)]]$lower,
                                upper = bounds[[noquote(Chat.model.names)]]$upper))

  return(smoof_function)
}
