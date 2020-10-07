
library(tidyverse, quietly = TRUE)
library(rstan, quietly = TRUE)
library(stringr, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(gridExtra, quietly = TRUE)
library(pbapply, quietly = TRUE)
library(parallel, quietly = TRUE)
library(boot, quietly = TRUE)
library(lqmm, quietly = TRUE) 
library(gridExtra, quietly = TRUE)
library(ggrepel, quietly = TRUE)

setwd("/home/rgiordan/Documents/git_repos/us-potus-model")
rstan_options(auto_write = TRUE)

# extracting results ----
# The 2012 results seem broken
# RUN_DATE <- ymd("2012-11-06")
# out <- read_rds(sprintf('models/backtest_2012/stan_model_%s.rds', RUN_DATE))

RUN_DATE <- ymd("2008-11-03")
out <- read_rds(sprintf('models/backtest_2008/stan_model_%s.rds', RUN_DATE))

class(out)
out@model_pars

#############################################################
# Diagnostics.  What do we need to save from a bootstrap?
