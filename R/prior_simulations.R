library(tidyverse)


g_params = data_frame(shape = c(.01, .1, 1, 10, 20, 100, 200),
                      rate = shape)

get_sim_df = function(shape, rate){
  data_frame(ctrl_a = rgamma(10000, shape = shape, rate = rate),
             ctrl_b = rgamma(10000, shape = shape, rate = rate),
             case_a = rgamma(10000, shape = shape, rate = rate),
             case_b = rgamma(10000, shape = shape, rate = rate),
             ctrl_p = map2_dbl(ctrl_a, ctrl_b, ~rbeta(1, shape1 = .x, shape2 = .y)),
             case_p = map2_dbl(ctrl_a, ctrl_b, ~rbeta(1, shape1 = .x, shape2 = .y)))
}

prior_sims = g_params %>% 
  mutate(sim_df = map2(shape, rate, get_sim_df)) %>% 
  unnest

prior_sims %>% 
  ggplot(aes(ctrl_p, case_p)) + geom_bin2d() + facet_wrap(factor('shape'))

prior_sims %>% 
  ggplot(aes(ctrl_p, case_p)) + geom_point(alpha = .1) + facet_wrap(factor('shape'))



prior_sim %>% ggplot(aes(ctrl_p, case_p)) + geom_point(alpha = .1)
