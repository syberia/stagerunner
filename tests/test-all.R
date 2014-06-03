#if (!'testthat' %in% .packages())
#  install.packages('testthat', repos = c(CRAN="http://cran.rstudio.com"))
library(testthat)
test_package("stagerunner")
