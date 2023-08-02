library(tidyverse);library(here);library(broom)

# no-intervention data at time 150
dat <- 
  here("data/processed/joint_dat_1_time_150.csv") %>% 
  read_csv() %>% 
  filter(intervention == "none")

# get a long tibble of summary statistics
dat_sum <- dat %>% 
  filter(intervention == "none") %>% 
  group_by(p_rewire, p_com, bc_threshold_mean, alpha, conf_mean, r_mean) %>% 
  summarise(across(c(p_underestimate, p_wwoh), list(mean = mean, sd = sd),
                   .names = "{.col}.{.fn}"), .groups = "drop") 

# test for the rate of underestimation
t_test_underestimate <- 
  dat %>% 
  filter(intervention == "none") %>%
  # get rid of conditions where the outcome is a constant
  right_join(filter(dat_sum, p_underestimate.sd != 0),
             by = c("p_rewire", "p_com", "bc_threshold_mean", "alpha", "conf_mean", "r_mean")) %>% 
  select(intervention:r_mean, p_underestimate) %>% 
  group_by(p_rewire, p_com, bc_threshold_mean, alpha, conf_mean, r_mean) %>% 
  nest() %>% 
  mutate(
    t_test = map(data, ~ t.test(.x$p_underestimate, mu = 0.8)),
    p_underestimate = map(t_test, tidy),
    .group = "drop"
  ) %>% 
  unnest(p_underestimate, names_sep = ".") %>% 
  select(-c(data, t_test)) 


# test for the WWOH actions
t_test_wwoh <- 
  dat %>% 
  filter(intervention == "none") %>%
  # get rid of conditions where the outcome is a constant
  #right_join(filter(outcome_des, p_underestimate.sd != 0)) %>% 
  select(intervention:r_mean, p_wwoh) %>% 
  group_by(p_rewire, p_com, bc_threshold_mean, alpha, conf_mean, r_mean) %>% 
  nest() %>% 
  mutate(
    t_test = map(data, ~ t.test(.x$p_wwoh, mu = 0.8)),
    p_wwoh = map(t_test, tidy),
    .group = "drop"
  ) %>% 
  unnest(p_wwoh, names_sep = ".") %>% 
  select(-c(data, t_test)) 


filtered_underestimate <- 
  inner_join(
    dat_sum,
    filter(t_test_underestimate, 
           p_underestimate.conf.low <= 0.8 & p_underestimate.conf.high >= 0.8),
    by = c("p_rewire", "p_com", "bc_threshold_mean", "alpha", "conf_mean", "r_mean")
  ) 

left_join(
  filtered_underestimate, t_test_wwoh,
  by = c("p_rewire", "p_com", "bc_threshold_mean", "alpha", "conf_mean", "r_mean")
) %>% 
  mutate(
    p_underestimate.p.adjust = p.adjust(p_underestimate.p.value, "BH"),
    p_wwoh.p.adjust = p.adjust(p_wwoh.p.value, "BH")
    ) %>% 
write_csv("data/processed/p_underestimate_against_empirical.csv")

filtered_wwoh <- 
  inner_join(
    dat_sum,
    filter(t_test_wwoh, p_wwoh.conf.low <= 0.05 & p_wwoh.conf.high >= 0.05),
    by = c("p_rewire", "p_com", "bc_threshold_mean", "alpha", "conf_mean", "r_mean")
  ) 

left_join(
  filtered_wwoh, t_test_underestimate,
  by = c("p_rewire", "p_com", "bc_threshold_mean", "alpha", "conf_mean", "r_mean")
) %>% 
  mutate(
    p_underestimate.p.adjust = p.adjust(p_underestimate.p.value, "BH"),
    p_wwoh.p.adjust = p.adjust(p_wwoh.p.value, "BH")
  ) %>% 
    write_csv("data/processed/p_wwoh_against_empirical.csv")
