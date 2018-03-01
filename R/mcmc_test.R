# Let's script up a test of the model that uses MCMC rather than VB
# This will take a while so it needs to run in a screen.

library(magrittr)
library(coda)
library(rstan)
library(stringr)
library(tidyverse)

load("~/ayush/outputs/peak_counts.RData")


stan_model_code = '
data {
int<lower=0> N; // number of regions
int case_counts[N];
int ctrl_counts[N];

}
parameters {
vector<lower=0, upper=1>[N] case_p;
vector<lower=0, upper=1>[N] ctrl_p;
real<lower=0> ctrl_a;
real<lower=0> ctrl_b;
real<lower=0> case_a;
real<lower=0> case_b;
}
model {
ctrl_a ~ gamma(.01, .01);
ctrl_b ~ gamma(.01, .01);
case_a ~ gamma(.01, .01);
case_b ~ gamma(.01, .01);
ctrl_p ~ beta(ctrl_a, ctrl_b);
case_p ~ beta(case_a, case_b);
case_counts ~ binomial(12, case_p);
ctrl_counts ~ binomial(12, ctrl_p);
}
generated quantities {
vector[N] case_ctrl_diff;
case_ctrl_diff = case_p - ctrl_p;
}
'
stanDSO = stan_model(model_code = stan_model_code)

peak_counts %<>% 
  spread(subj_type, n_peaks) %>% 
  filter((case > 0 & ctrl > 0) | ((case + ctrl > 3))) #reduces number of regions by ~51%

sample_and_save = function(file_dat) {
  start_time = Sys.time()
  
  mcmc_res = sampling(stanDSO, 
                      data = list(N = nrow(file_dat), case_counts = file_dat$case, ctrl_counts = file_dat$ctrl),
                      chains = 3,
                      warmup = 500,
                      cores = 3,
                      open_progress = TRUE)
  save(mcmc_res,
       file = paste0('~/ayush/outputs/mcmc_results/', gsub('[.]txt', '',file_dat$file[1]), '_mcmc.RData')) # probably pretty big
  finish_time = Sys.time()
  
  return(finish_time - start_time)
}

peak_counts %>%
  group_by(file) %>%
  nest %>% 
  mutate(data = map2(data, file, ~mutate(.x, file = .y))) %>%
  mutate(mcmc_result_time = map(data, sample_and_save))
