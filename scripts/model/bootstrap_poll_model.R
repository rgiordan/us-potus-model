#!/usr/bin/env Rscript
#
# Example usage:
# $ ....

library(tidyverse, quietly = TRUE)
library(rstan, quietly = TRUE)
library(stringr, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(pbapply, quietly = TRUE)
library(parallel, quietly = TRUE)
library(optparse, quietly = TRUE)
library(doParallel)

rstan_options(auto_write = TRUE)


option_list <- list(
    make_option(c("--base_dir"),
                default="/home/rgiordan/Documents/git_repos/InfinitesimalJackknifeWorkbench/src/bayes",
                help="The base directory"),
    make_option(c("--save_filename"),
                help="Optional filename and path relative to base_dir for the output"),
    make_option(c("--force"),
                action="store_true",
                default=FALSE,
                help="If set, overwrite existing results"),
    make_option(c("--num_cores"),
                default=1,
                help="Number of cores for parallel processing"),
    make_option(c("--num_mcmc_samples"),
                default=500,
                help="Default number of MCMC samples"),
    make_option(c("--num_boots"),
                default=50,
                help="Default number of bootstrap samples"),
    make_option(c("--num_mcmc_chains"),
                default=6,
                help="Number of MCMC chains"),
    make_option(c("--seed"),
                default=42,
                help="Random seed")
)

# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults,
opt <- parse_args(OptionParser(option_list=option_list))
print("===================")
print("Options:")
print(opt)
print("===================")

base_dir <- opt$base_dir

if (is.null(opt$save_filename)) {
    run_year <- 2016
    save_filename <- file.path(
        base_dir, "output",
        sprintf("election_%d_bootstrap_mcmc.Rdata", run_year))
} else {
    save_filename <- opt$save_filename
}

if (file.exists(save_filename)) {
    if (opt$force) {
        cat("Overwriting\n", save_filename, "\n", sep="")
    } else {
        cat("File\n", save_filename, "\n", "already exists, terminating.", sep="")
        quit()
    }
}

set.seed(opt$seed)
registerDoParallel(cores=opt$num_cores)
options(mc.cores=1) # don't do parallel within parallel

model_dir <- file.path(base_dir, "example-models/us-potus-model")
#setwd("/home/rgiordan/Documents/git_repos/us-potus-model")

## Setup
#rm(list = ls())
# options(mc.cores = opt$num_cores)
# n_chains <- 6
# n_cores <- 6
# n_sampling <- 500
# n_warmup <- 500
# n_refresh <- n_sampling*0.1


####################

run_date <- ymd("2016-11-08")

data_filename <- file.path(model_dir, sprintf("/stan_data_%s.Rdata", run_date))
load(file=data_filename)
model <- rstan::stan_model(file.path(model_dir, "poll_model_2020.stan"))

###################

GetDrawsToSave <- function(out) { 
    predicted_score <- rstan::extract(out, pars = "predicted_score")[[1]]

    num_dates <- dim(predicted_score)[2]
    pred_dates <- 1:num_dates + ymd(start_date)
    election_row <- pred_dates == election_day
    
    predicted_score_election <- predicted_score[, election_row, , drop=TRUE]
    state_weights_array <- array(state_weights, c(1, length(state_weights)))
    
    # Sweep is just doing a broadcast.
    predicted_nation_score_election <-
        sweep(predicted_score_election, MARGIN=2, FUN="*", state_weights) %>%
        apply(FUN=sum, MARGIN=1)
    
    pct_clinton_natl <-
        tibble(natl_vote=predicted_nation_score_election,
               draw=1:length(predicted_nation_score_election),
               t=ymd(start_date))
    
    pct_clinton_state <-
        as_tibble(predicted_score_election) %>%
        mutate(draw=1:n()) %>%
        pivot_longer(cols=-draw) %>%
        mutate(state_num=as.numeric(str_replace(name, "^V", "")),
               state=colnames(state_correlation_polling)[state_num]) %>%
        select(-name)
    
    return(list(pct_clinton_state=pct_clinton_state,
                pct_clinton_natl=pct_clinton_natl))
}


####################
# Run the bootstraps

boot_results <- foreach(b=1:num_boots) %dopar% {
    options(mc.cores=1) # Don't do parallel within parallel

    data_boot <- data
    data_boot$n_democrat_state <-
        with(data_boot,
             rbinom(size=length(n_democrat_state),
                    n=n_two_share_state,
                    p=n_democrat_state / n_two_share_state)
        )
    data_boot$n_democrat_national <-
        with(data_boot,
             rbinom(size=length(n_democrat_national),
                    n=n_two_share_national,
                    p=n_democrat_national / n_two_share_national)
        )
    
    sampling_time <- Sys.time()
    out_boot <- rstan::sampling(model,
                                data = data_boot,
                                chains  = opt$num_mcmc_chains,
                                iter = opt$num_mcmc_samples,
                                warmup = opt$num_mcmc_samples
    )
    sampling_time <- Sys.time() - sampling_time
    
    result_list <- GetDrawsToSave(out_boot)
    result_list$sampling_time <- sampling_time
    result_list$n_democrat_state <- data_boot$n_democrat_state
    result_list$n_democrat_national <- data_boot$n_democrat_national
    
    return(result_list)
}


save(boot_results,
     run_date,
     data_filename,
     file=save_filename)





##########################
# Sanity checks

if (FALSE) {
    # 
    # predicted_score_election <- predicted_score[,election_row,,drop=TRUE]
    # state_weights_array <- array(state_weights, c(1, length(state_weights)))
    # 
    # # Oh R you are not easy to read
    # predicted_nation_score_election <-
    #     sweep(predicted_score_election, MARGIN=2, FUN="*", state_weights) %>%
    #     apply(FUN=sum, MARGIN=1)
    # 
    # pct_clinton_natl <-
    #     tibble(natl_vote=predicted_nation_score_election,
    #            draw=1:length(predicted_nation_score_election),
    #            t=ymd(start_date))
    
    # save(pct_clinton_natl, file="/tmp/pct_clinton.Rdata")
    # 15k, great
    
    predicted_nation_score_election2 <-
        predicted_score_election * rep(state_weights, each=dim(predicted_score_election)[1])
    predicted_nation_score_election2 <- apply(predicted_nation_score_election2, FUN=sum, MARGIN=1)
    
    stopifnot(max(abs(predicted_nation_score_election2 - predicted_nation_score_election)) < 1e-12)
    
    # First compute the draws
    pct_clinton_natl <- pblapply(1:dim(predicted_score)[1],
                                 function(x){
                                     # each row is a day for a particular draw
                                     temp <- predicted_score[x,,] %>% as.data.frame()
                                     # temp <- predicted_score[x,,]
                                     # temp <- temp[election_row, , drop=FALSE]
                                     # temp <- as.data.frame(temp)
                                     names(temp) <- colnames(state_correlation_polling)
                                     
                                     # for each row, get weigted natl vote
                                     tibble(natl_vote = apply(temp, MARGIN = 1,
                                                              function(y) {
                                                                  weighted.mean(y, state_weights) }
                                     )
                                     ) %>%
                                         mutate(t = row_number() + ymd(start_date)) %>%
                                         mutate(draw = x)
                                 }) %>% do.call('bind_rows',.)
    
    pct_clinton_natl <- filter(pct_clinton_natl, t==election_day)
    stopifnot(max(abs(pct_clinton_natl$natl_vote - predicted_nation_score_election)) < 1e-12)

    # Optionally compute summary statistics
    pct_clinton_natl_summary <- pct_clinton_natl %>%
        group_by(t) %>%
        summarise(low = quantile(natl_vote,0.025),
                  high = quantile(natl_vote,0.975),
                  mean = mean(natl_vote),
                  var = var(natl_vote),
                  prob = mean(natl_vote > 0.5)) %>%
        mutate(state = '--')
    
    #model <- rstan::stan_model("scripts/model/poll_model_2020.stan")
    
    # run_init <- FALSE
    # 
    # if (run_init) {
    #     sampling_time <- Sys.time()
    #     out <- rstan::sampling(model, data = data,
    #                            refresh = n_refresh,
    #                            chains  = n_chains,
    #                            iter = 500,
    #                            warmup = 250
    #     )
    #     sampling_time <- Sys.time() - sampling_time
    #     print(sampling_time)
    # } else {
    #     out <- read_rds(sprintf('models/stan_model_%s.rds',run_date))
    #     predicted_score <- rstan::extract(out, pars = "predicted_score")[[1]]
    #     rm(out)
    # }
    
}
