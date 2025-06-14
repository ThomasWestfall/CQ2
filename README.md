# CQ2
 _CQ2_ is an R-package that calibrates concentration-discharge (C-Q) models with slow and quick flow components. The package was developed to better explain the temporal variability in long-term daily records of electrical conductivity (EC), a surrogate for salinity. The package requires daily measurements of streamflow and either EC or salinity. Gaps in the dataset are permitted as the package calibrates models through global maximum likelihood parameter estimation on only periods of observations. Parameters from the C-Q models and baseflow filter (Eckhardt) are jointly calibrated to provide an objective baseflow estimate. A total of 15 models are provided with options to compare each. A systematic evaluation of all models identified "C13" a quick-slow variant of the Hubbard Brook model, as the best performing in 14/23 catchments in Victoria, Australia, and often tied for the highest ranked model at the other sites. For details on the equations, and the solute response, see:

Westfall, T. G., Peterson, T. J., Lintern, A., & Western, A. W. (2025). Slow and quick flow models explain the temporal dynamics of daily salinity in streams. Water Resources Research, 61, e2024WR039103. https://doi.org/10.1029/2024WR039103

Below is an example for building, fitting, and evaluating the simple C-Q model (C1) and the quick-slow Hubbard Brook model (C13). The original analysis was performed only on observations above a low flow streamflow threshold of (0.005 mm/d); however, "C13" has proven capable and informative when calibrated to lower streamflows that approach zero. The example is also available within the vignette folder. 

# Example 1. Simple CQ vs. quick-slow Hubbard Brook

This _CQ2_ example sets-up and calibrates two models, then plots their results.

```r
# Install and load the CQ2 package
devtools::install_github("ThomasWestfall/CQ2")
```

## Load additional packages and data
```r
#----------------
# Install and load the cmaesr and padr package
devtools::install_github("jakobbossek/cmaesr")
library(cmaesr)
library(padr)

# load data
CQ.daily = readRDS('data/234201B_daily.rds')

# Validate column names as shown below
head(CQ.daily)
#>  year month day        C           Q
#> 1 1993     5  14 3961.851 0.007035374
#> 2 1993     5  15 4101.576 0.007100299
#> 3 1993     5  16 4317.657 0.007723581
#> 4 1993     5  17 4232.229 0.007870385
#> 5 1993     5  18 4340.684 0.007946491
#> 6 1993     5  19 4773.975 0.008211242
```
## Set-up models
Only the `Chat.model.names` as a character vector of model names (C1-C15) and `input.data`as a data.frame of daily C-Q observations are required. The other selections are shown below as defaults. The Gaussian likelihood function assumes normal distribution and no auto-correlation within the residuals of the model. The models are fitted to observations that occur when streamflow is only above the `Qthresh`. We assumed the catchment response is negligible below this threshold. `site.id` and `site.name` are not required, but recommend.

```r
# set-up models 
models = setModels(Chat.model.names = c('C1','C13'),
                   input.data = CQ.daily,
                   Likelihood.name = "GaussLiklihood",
                   Qthresh = 0.005,
                   site.id = "234201B",
                   site.name = "Woady Yaloak")
```

## Fit models
Calibrate the set-up models using `runModels` as shown below. This calibration optimizes parameters using a global search algorithm within the [cmaesr](https://github.com/jakobbossek/cmaesr) R-package. The fit takes several minutes to several hours depending on the model and amount of data. It takes 1.5 hours for model _C13_ to calibrate to 30 years of daily data. Every iteration is printed to the console where _y_ is the value of the negative log-likelihood that is being minimized in the objective function. After fitting the models, `getResults` re-runs the model with the best set of parameters in order to output predictions. 
```r 
# Fit models
models = runModels(model.setup = models)

#> Starting optimization.
#> Iteration    1: x1:    +3.9098   x2:    -0.0655   x3:    +5.5986, y = +31367.2920
#> Iteration    2: x1:    +2.2868   x2:    -0.4143   x3:    +4.7704, y = +20013.2967
#> Iteration    3: x1:    +2.2868   x2:    -0.4143   x3:    +4.7704, y = +20013.2967
#> Iteration    4: x1:    +3.6778   x2:    +0.0172   x3:    +8.1810, y = +10634.3370
#> Iteration    5: x1:    +3.6778   x2:    +0.0172   x3:    +8.1810, y = +10634.3370
#> ...
#> Iteration   40: x1:    +3.0389   x2:    -0.2203   x3:    +3.8253, y = +4836.3509
#> Iteration   41: x1:    +3.0389   x2:    -0.2203   x3:    +3.8253, y = +4836.3509
#> Optimization terminated.
#> [1] "C1 is complete."

# get predictions from the fitted models in a data frame
model.output = getResults(model.setup = models)

```

## Plot results
`plotResults` allows predictions from any two models to be plotted alongside observations. A CQ _scatter_ plot and _timeseries_ plot are available. When _timeseries_ is selected, a PDF will be exported to the working directory with a timeseries of each year on a separate page. 

```r
# plot C-Q scatter plots from two models
output.plots = plotResults(model.setup = models,
                          output.data = model.output,
                          plot.models = c('C1','C13'),
                          plot.type = "scatter")
```

![CQ observations and predictions from both models](https://github.com/ThomasWestfall/CQ2/blob/main/data/CQ_plot_234201B.png?raw=true){width=50%}

```
# plot C-Q timeseries from two models

# Set working directory to output PDF plot with timeseries
setwd("C:/Users/twes0006/OneDrive - Monash University/Git/CQ2/data")

# plot timeseries
output.plots = plotResults(model.setup = models,
                          output.data = model.output,
                          plot.models = c('C1','C13'),
                          plot.type = "timeseries")
                          
```

![Example of timeseries figure on one-page of PDF exported](https://github.com/ThomasWestfall/CQ2/blob/main/data/CQ_timeseries_234201B_1994.png?raw=true){width=70%}


## Export statistics and parameters
A data frame is exported with the negative log-likelihood, AIC, NSE, and RMSE for each model that is calculated on the fitted data. The calculated BFI is also given for quick-slow models.
```r
# export summary of statistics as a dataframe
output_summary = getStats(model.setup = models,
                          output.data = model.output)

head(output_summary)
#>        ID model             negLL               AIC               NSE             RMSE             BFI
#> 1 234201B    C1  4836.35090303965  9678.70180607931 0.232530854346575 1408.56860983028            <NA>
#> 2 234201B   C13 -23.2978663965841 -30.5957327931683 0.735433480449069 824.072331651912 0.1161062944671

# export parameters from each model
parameter_summary = getParam(model.setup = models,
                             output.data = model.output)

```
