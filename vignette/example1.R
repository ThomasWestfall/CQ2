# Compare two C-Q models

# Install and load packages
# install CQ2
# devtools::install_github("ThomasWestfall/CQ2")
# install cmaesr
# devtools::install_github("jakobbossek/cmaesr")
library(cmaesr)
library(padr)
library(Hmisc)

# load data
CQ.daily = readRDS('data/234201B_daily.rds')

# set-up models
models = setModels(Chat.model.names = c('C1','C3'),
                   input.data = CQ.daily,
                   Likelihood.name = "GaussLiklihood",
                   Qthresh = 0.005,
                   site.id = "234201B",
                   site.name = "Woady Yaloak")

# Fit models
models = runModels(model.setup = models)

# get estimates from the fitted models
model.output = getResults(model.setup = models)

# plot timeseries and C-Q scatter plots from two models
output.plots = plotResults(model.setup = models,
                          output.data = model.output,
                          plot.models = c('C1','C13'),
                          plot.type = "scatter")

# export summary of statistics as a dataframe
output_summary = getStats(model.setup = models,
                          output.data = model.output)

# export parameters from each model
parameter_summary = getParam(model.setup = models,
                             output.data = model.output)

save("temp.Rdata")

