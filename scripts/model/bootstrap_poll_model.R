
## Desc
# Refactored version run file

library(tidyverse, quietly = TRUE)
library(rstan, quietly = TRUE)
library(stringr, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(pbapply, quietly = TRUE)
library(parallel, quietly = TRUE)
# library(boot, quietly = TRUE)
# library(lqmm, quietly = TRUE)
library(gridExtra, quietly = TRUE)
# library(ggrepel, quietly = TRUE)

setwd("/home/rgiordan/Documents/git_repos/us-potus-model")
rstan_options(auto_write = TRUE)

# This fixes the random walks.
set.seed(42)

## Setup
#rm(list = ls())
options(mc.cores = 6)
n_chains <- 6
n_cores <- 6
n_sampling <- 500
n_warmup <- 500
n_refresh <- n_sampling*0.1


## Master variables
RUN_DATE <- ymd("2016-11-08")

# election_day <- ymd("2016-11-08")
# start_date <- as.Date("2016-03-01") # Keeping all polls after March 1, 2016

####################

load(file=sprintf('models/stan_data_%s.Rdata', RUN_DATE))

model <- rstan::stan_model("scripts/model/poll_model_2020.stan")
sampling_time <- Sys.time()
out <- rstan::sampling(model, data = data,
                       refresh = n_refresh,
                       chains  = n_chains,
                       iter = 500,
                       warmup = 250
)
sampling_time <- Sys.time() - sampling_time


###################

predicted_score <- rstan::extract(out, pars = "predicted_score")[[1]]

# First compute the draws
pct_clinton_natl <- pblapply(1:dim(predicted_score)[1],
                             function(x){
                                 # each row is a day for a particular draw
                                 temp <- predicted_score[x,,] %>% as.data.frame()
                                 names(temp) <- colnames(state_correlation_polling)
                                 
                                 # for each row, get weigted natl vote
                                 tibble(natl_vote = apply(temp, MARGIN = 1,
                                                          function(y) {
                                                              weighted.mean(y, state_weights) }
                                 )
                                 ) %>%
                                     mutate(t = row_number() + min(df$begin)) %>%
                                     mutate(draw = x)
                             }) %>% do.call('bind_rows',.)

# ...then compute summary statistics
pct_clinton_natl_summary <- pct_clinton_natl %>%
    group_by(t) %>%
    summarise(low = quantile(natl_vote,0.025),
              high = quantile(natl_vote,0.975),
              mean = mean(natl_vote),
              prob = mean(natl_vote > 0.5)) %>%
    mutate(state = '--')