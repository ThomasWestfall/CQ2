# Compare two C-Q models

# Install and load the cmaesr package
# devtools::install_github("jakobbossek/cmaesr")
library(cmaesr)
library(padr)

# load data
data_all = readRDS("data/234201B_daily.rds")

# re-format as data.frame with the following columns
data_all = data.frame(year = data_all$year, month=data_all$month, day = data_all$day, C=data_all$C, Q=data_all$flow_mm_d)

# set-up models
model.setup = setModels(Chat.model.names = c('Chat1'),
                        input.data = data_all,
                        Qthresh = 0,
                        Likelihood.name = "GaussLiklihood",
                        site.id = "234201B",
                        site.name = "Woady Yaloak")

# select and fit models
models = runModels(model.setup = model.setup)

# get predictions from the fitted models
data_all = getResults(cmaes.results = models, model.setup = model.setup)

# plot timeseries and C-Q scatter plots from two models
model.plots = plotResults(Chat.model.names = c('Chat1','Chat1'), input.data = data_all,
                          model.setup = model.setup)

# export summary of statistics as a dataframe table
output_summary = getStats(Chat.model.names = c('Chat1'),
                          input.data = data_all,
                          cmaes.results = models,
                          model.setup = model.setup)

# export parameters from each model
parameter_summary = getParam(Chat.model.names = c('Chat1'),
                             input.data = data_all,
                             cmaes.results = models,
                             model.setup = model.setup)
