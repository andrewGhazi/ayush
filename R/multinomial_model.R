# Let's try to run a multinomial model on the output space, simply counting the number of times n_case_peaks and n_ctrl_peaks co-occur

library(rstan)
library(tidyverse)

load("~/ayush/outputs/peak_counts.RData")

peak_counts %<>% 
  spread(subj_type, n_peaks) 

count_tables = peak_counts %>% group_by(file) %>% nest %>% mutate(count_table = map(data, ~t(table(select(.x, case, ctrl)))))

multi_model = '
data{
int<lower=0> n_case;
int<lower=0> n_ctrl;
int<lower=0> count_array[n_ctrl+1, n_case+1]; // if there are n samples, there are n+1 possible number of peak outcomes
}
parameters{
simplex[n_ctrl + 1] alpha;
real<lower=0,upper=1> p_array[n_ctrl+1, n_case+1];
}
model{
  for (i in 1:(n_case+1)) {
    p_array[,i] ~ dirichlet(alpha);
    count_array[,i] ~ multi(p_array[,i]);
  }
}
'

multi_stan = stan_model(model_code = multi_model)
