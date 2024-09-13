\name{CQ2-package}

\alias{CQ2-package}

\alias{CQ2}

\docType{package}

\title{Overview of methods and procedures}

\description{

\code{CQ2} fits and compares C-Q models with single flow components and slow-quick flow components on daily streamflow and concentration observations. The slow flow component in the multiple flow component C-Q models is estimated as baseflow using the Eckhardt (2005) baseflow filter, and the filter parameters are objectivly calibrated along with model parameters using global optimization with the cmaesr (R-package). You have the option to run and evaluate any or all of the 15 models that are provided (Chat1 - Chat15), but the default analysis will compare Chat1, the simple C-Q model \eqn{C=aQ^b} and Chat13, a slow-quick version of the Hubbard Brook working model, that was the best performing at explaining the varation in C-Q plots. See the article when published: Westfall T.G., Peterson T.J., Lintern A., Western, A.W (2024), Slow and quick flow models explain the temporal dynamics of daily salinity in streams (IN PREP)

The \code{CQ2} package operates by first setting up the data and models, \code{setModels}, then fitting the models, \code{runModels}. Be prepared to run models over lunch or overnight as the computation time for the slow-quick models can take several hours for 20+ year records of daily data. After computation, output the predictions from the models into a dataframe with \code{getResults}. Then the results can be evaluated through comparing the statistics (i.e. AIC, NSE, RMSE, BFI), \code{getStats}, parameters, \code{getParam}, or plot the predictions in C-Q scatter plots and annual timeseries along with observations and the objectivly estimated baseflow, \code{plotResults}!


-----------------------------------------------------------------------------------
}
  \section{I. Set-Up}{

    \tabular{ll}{
      \code{\link{setModels}}\tab set-up data and C-Q models\cr
      ------------------------\tab------------------------------------------------
  }
  }
   \section{II. Fit}{

    \tabular{ll}{
      \code{\link{runModels}}\tab fit C-Q models\cr
      ------------------------\tab------------------------------------------------
  }
  }
  \section{III. Review}{

    \tabular{ll}{
      \code{\link{getResults}}\tab retrieve predictions from C-Q models\cr
      \code{\link{plotResults}}\tab plot predictions from C-Q models\cr
      \code{\link{getStats}}\tab retrieve performance of each C-Q model\cr
      \code{\link{getParam}}\tab retrieve parameters from C-Q models\cr
      ------------------------\tab------------------------------------------------
  }
  }
\section{Authors}{
Except where indicated otherwise, the methods and functions in this package were written by Thomas Westfall.
}

\section{Acknowledgments}{
Immense graditude to Tim Peterson, Anna Lintern, Andrew W. Western, and Lucas Pamminger for their assistance, patience, and support.

}


\keyword{ package }
\keyword{ slow-quick }
\keyword{ water quality }

