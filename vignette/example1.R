# Compare two C-Q models

# Install and load the cmaesr package
# devtools::install_github("jakobbossek/cmaesr")
library(cmaesr)
library(padr)

# load data
CQ.daily = readRDS('data/234201B_daily.rds')

# set-up models
models = setModels(Chat.model.names = c('C13'),
                        input.data = CQ.daily,
                        Likelihood.name = "GaussLiklihood",
                        site.id = "234201B",
                        site.name = "Woady Yaloak")

# Fit models
models = runModels(model.setup = models)

# get predictions from the fitted models
model.output = getResults(model.setup = models)

# plot timeseries and C-Q scatter plots from two models
output.plots = plotResults(model.setup = models,
                          output.data = model.output,
                          plot.models = c('C13'),
                          plot.type = "scatter")

# export summary of statistics as a dataframe
output_summary = getStats(model.setup = models,
                          output.data = model.output)

# export parameters from each model
parameter_summary = getParam(model.setup = models,
                             output.data = model.output)
