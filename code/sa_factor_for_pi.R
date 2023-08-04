library(tidyverse);library(here);library(broom);library(wesanderson)

# no-intervention data at time 150
dat <- 
  here("data/processed/joint_dat_1_time_150.csv") %>% 
  read_csv() %>% 
  filter(intervention == "none")

# general results

p_underestimate_coef <- 
  dat %>% 
  lm(p_underestimate ~ p_rewire*p_com*bc_threshold_mean*alpha*conf_mean*r_mean, data = .) %>% 
  tidy()
names(p_underestimate_coef) <- c("term", "estimate_p_underestimate",
                                 "std.error_p_underestimate", "statistic_p_underestimate",
                                 "p.value_p_underestimate")

p_inconsistent_coef <- 
  dat %>% 
  lm(p_inconsistent ~ p_rewire*p_com*bc_threshold_mean*alpha*conf_mean*r_mean, data = .) %>% 
  tidy() 
names(p_inconsistent_coef) <- c("term", "estimate_p_inconsistent",
                                 "std.error_p_inconsistent", "statistic_p_inconsistent",
                                 "p.value_p_inconsistent")

p_wwoh_coef <- 
  dat %>% 
  lm(p_wwoh ~ p_rewire*p_com*bc_threshold_mean*alpha*conf_mean*r_mean, data = .) %>% 
  tidy() 
names(p_wwoh_coef) <- c("term", "estimate_p_wwoh",
                        "std.error_p_wwoh", "statistic_p_wwoh",
                        "p.value_p_wwoh")

bind_cols(p_underestimate_coef, select(p_inconsistent_coef, -term), select(p_wwoh_coef, -term)) %>% 
  write_csv("data/processed/factor_for_pi_coef.csv")

# in all combinations of the four parameters 
# the final effects of p_com and r_mean are the same
dat %>% 
  select(-c(intervention, credibility_mean)) %>% 
  group_by(p_rewire, bc_threshold_mean, alpha, conf_mean) %>% 
  nest() %>% 
  mutate(
    lm1 = map(data, ~ lm(.x$p_underestimate ~ .x$p_com*.x$r_mean)),
    p_underestimate = map(lm1, tidy),
    lm2 = map(data, ~ lm(.x$p_inconsistent ~ .x$p_com*.x$r_mean)),
    p_inconsistent = map(lm2, tidy),
    lm3 = map(data, ~ lm(.x$p_wwoh ~ .x$p_com*.x$r_mean)),
    p_wwoh = map(lm3, tidy)
  ) %>% 
  unnest(c(p_underestimate, p_inconsistent, p_wwoh), names_sep = "_") %>% 
  select(-c(data,lm1,lm2,lm3)) %>% 
  write_csv("data/processed/sa_factor_for_pi.csv")
