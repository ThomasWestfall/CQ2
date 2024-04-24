# This R scrpt details the step required to build the CRAN .tar.gz file for submission to CRAN and how to build the manual PDF.
#------------------------------------------------

# RE-write NAMESPACE
roxygen2::roxygenise()

# Build PDF. If CQ2.pdf already exists, then delete before running.
path <- "C:/Users/twes0006/OneDrive - Monash University/Git/CQ2"  # Note could also use: find.package("CQ2")
system(paste(shQuote(file.path(R.home("bin"), "R")),"CMD", "Rd2pdf", shQuote(path)))

# NOTE, to build pdf manual. For Windows install: install.packages("tinytex")
# NOTE, you might also need MikTeX...

# This is not working because R is looking for pdflatex in the wrong location...

#returns path where R-Studio is trying to find pdflatex.exe in windows...
Sys.getenv("PATH")

#if not at this path.. then set path..
Sys.setenv(PATH=paste(Sys.getenv("PATH"),"C:/Users/twes0006/AppData/Local/Programs/MiKTeX/miktex/bin/x64/",sep=";"))

# This should not work...
system(paste(shQuote(file.path(R.home("bin"), "R")),"CMD", "Rd2pdf", shQuote(path)))

# For later....
# Pre-build the vignettes to avod their execuation during package installation.
# NOTE: Must manually move image files from vignettes/figure/ to vignettes/ after knit
#       and remove figure/ from .Rmd
# library(knitr)
# knitr::knit("vignettes/Point_rainfall.Rmd.orig", output = "vignettes/Point_rainfall.Rmd")
# knitr::knit("vignettes/Catchment_avg_ET_rainfall.Rmd.orig", output = "vignettes/Catchment_avg_ET_rainfall.Rmd")
# browseVignettes("AWAPer")

# Build the package for CRAN
devtools::build(".")
