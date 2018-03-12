library(magrittr)
library(coda)
library(rstan)
library(stringr)
library(tidyverse)

load("~/ayush/outputs/peak_counts.RData")

peak_counts %<>% 
  spread(subj_type, n_peaks) 

full_fits = list.files('~/ayush/outputs/mcmc_results', 
                       pattern = 'full',
                       full.names = TRUE)

hdi_fun = function(case_ctrl_diff_vec, prob = .99){
  list(HPDinterval(mcmc(case_ctrl_diff_vec), prob = prob))
}

#load(full_fits[3])

get_region_hdis = function(fit_file){
  load(fit_file)
  region_hdis = mcmc_res %>% 
    rstan::extract() %>% 
    .[[7]] %>% 
    as_tibble %>% 
    set_colnames(filter(peak_counts, grepl(fit_file %>% str_extract('E[0-9]+-'), file))$region) %>% 
    mutate(iter = 1:n()) %>% 
    gather(region, diff_val, -iter) %>% 
    group_by(region) %>% 
    summarise(hdi99 = hdi_fun(diff_val)) %>% # ~ 140s for E12 dataset up to this point
    mutate(is_functional = map_lgl(hdi99, ~!between(0, .x[1], .x[2])),
           file = str_extract(fit_file, 'E[0-9]+')) # this is instant
  
  region_hdis
}

all_region_hdis = full_fits %>% 
  map(get_region_hdis)
