library(tidyverse);library(here)

dat_1 <- 
  here("data/raw/intervention-none-one.csv") %>% 
  read_delim(delim = ",", n_max = 2, skip = 6,
             col_select = c('[run number]', 'intervention-type', 'p-rewire', 'p-com', 'bc-threshold-mean', 
                            'alpha', 'conf-mean', 'r-mean', 'credibility-mean', '[step]',
                            '(count turtles with [ valuation-norm-mean <  (count turtles with [ valuation-wwoh >= 0.5 ]) / n-agents]) / n-agents',
                            'count turtles with [(valuation-wwoh >= 0.5 and behaviour-wwoh = 0) or (valuation-wwoh < 0.5 and behaviour-wwoh = 1)] / n-agents',
                            '(count turtles with [behaviour-wwoh = 1]) / n-agents',
                            'mean [ valuation-wwoh ] of turtles',
                            '(count turtles with [ valuation-wwoh >= 0.5 ]) / n-agents',
                            'mean [ valuation-norm-mean ] of turtles',
                            'mean [ valuation-norm-sd ] of turtles'))

## step

step_1 <- 
  here("data/raw/intervention-two-three.csv") %>% 
  #here("data/raw/intervention-none-one.csv") %>% 
  read_delim(delim = ",", skip = 6,
             col_select = '[step]')

id <- 1:nrow(step_1)

step_1 <- 
  step_1 %>% 
  rename(step = `[step]`) %>% 
  mutate(id = id)

#write_csv(step_1, file = "data/processed/step_1.csv")
write_csv(step_1, file = "data/processed/step_2.csv")

## p_underestimate

p_underestimate_1 <- 
  here("data/raw/intervention-two-three.csv") %>% 
  #here("data/raw/intervention-none-one.csv") %>% 
  read_delim(delim = ",", skip = 6,
             col_select = '(count turtles with [ valuation-norm-mean <  (count turtles with [ valuation-wwoh >= 0.5 ]) / n-agents]) / n-agents') %>% 
  rename(p_underestimate = `(count turtles with [ valuation-norm-mean <  (count turtles with [ valuation-wwoh >= 0.5 ]) / n-agents]) / n-agents`) %>% 
  mutate(id = id)

#write_csv(p_underestimate_1, file = "data/processed/p_underestimate_1.csv")
write_csv(p_underestimate_1, file = "data/processed/p_underestimate_2.csv")

# p_inconsistent

p_inconsistent_1 <- 
  here("data/raw/intervention-two-three.csv") %>% 
  #here("data/raw/intervention-none-one.csv") %>% 
  read_delim(delim = ",", skip = 6,
             col_select = 'count turtles with [(valuation-wwoh >= 0.5 and behaviour-wwoh = 0) or (valuation-wwoh < 0.5 and behaviour-wwoh = 1)] / n-agents') %>% 
  rename(p_inconsistent = `count turtles with [(valuation-wwoh >= 0.5 and behaviour-wwoh = 0) or (valuation-wwoh < 0.5 and behaviour-wwoh = 1)] / n-agents`) %>% 
  mutate(id = id)

#write_csv(p_inconsistent_1, file = "data/processed/p_inconsistent_1.csv")
write_csv(p_inconsistent_1, file = "data/processed/p_inconsistent_2.csv")

## p_wwoh

p_wwoh_1 <- 
  here("data/raw/intervention-two-three.csv") %>% 
  #here("data/raw/intervention-none-one.csv") %>% 
  read_delim(delim = ",", skip = 6,
             col_select = '(count turtles with [behaviour-wwoh = 1]) / n-agents') %>% 
  rename(p_wwoh = `(count turtles with [behaviour-wwoh = 1]) / n-agents`) %>% 
  mutate(id = id)

#write_csv(p_wwoh_1, file = "data/processed/p_wwoh_1.csv")
write_csv(p_wwoh_1, file = "data/processed/p_wwoh_2.csv")

## mean_belief

mean_belief_1 <- 
  here("data/raw/intervention-two-three.csv") %>% 
  #here("data/raw/intervention-none-one.csv") %>% 
  read_delim(delim = ",", skip = 6,
             col_select = 'mean [ valuation-wwoh ] of turtles') %>% 
  rename(mean_belief = `mean [ valuation-wwoh ] of turtles`) %>% 
  mutate(id = id)

#write_csv(mean_belief_1, file = "data/processed/mean_belief_1.csv")
write_csv(mean_belief_1, file = "data/processed/mean_belief_2.csv")

## p_supporter

p_supporter_1 <- 
  here("data/raw/intervention-two-three.csv") %>% 
  #here("data/raw/intervention-none-one.csv") %>% 
  read_delim(delim = ",", skip = 6,
             col_select = '(count turtles with [ valuation-wwoh >= 0.5 ]) / n-agents') %>% 
  rename(p_supporter = `(count turtles with [ valuation-wwoh >= 0.5 ]) / n-agents`) %>% 
  mutate(id = id)

#write_csv(p_supporter_1, file = "data/processed/p_supporter_1.csv")
write_csv(p_supporter_1, file = "data/processed/p_supporter_2.csv")

## mean_norm_mu

mean_norm_mu_1 <- 
  here("data/raw/intervention-two-three.csv") %>% 
  #here("data/raw/intervention-none-one.csv") %>% 
  read_delim(delim = ",", skip = 6,
             col_select = 'mean [ valuation-norm-mean ] of turtles') %>% 
  rename(mean_norm_mu = `mean [ valuation-norm-mean ] of turtles`) %>% 
  mutate(id = id)

#write_csv(mean_norm_mu_1, file = "data/processed/mean_norm_mu_1.csv")
write_csv(mean_norm_mu_1, file = "data/processed/mean_norm_mu_2.csv")

## mean_norm_sd

mean_norm_sd_1 <- 
  here("data/raw/intervention-two-three.csv") %>% 
  #here("data/raw/intervention-none-one.csv") %>% 
  read_delim(delim = ",", skip = 6,
             col_select = 'mean [ valuation-norm-sd ] of turtles') %>% 
  rename(mean_norm_sd = `mean [ valuation-norm-sd ] of turtles`) %>% 
  mutate(id = id)

#write_csv(mean_norm_sd_1, file = "data/processed/mean_norm_sd_1.csv")
write_csv(mean_norm_sd_1, file = "data/processed/mean_norm_sd_2.csv")

## rum_number

run_number_1 <- 
  here("data/raw/intervention-two-three.csv") %>% 
  #here("data/raw/intervention-none-one.csv") %>% 
  read_delim(delim = ",", skip = 6,
             col_select = '[run number]') %>% 
  rename(run_number = `[run number]`) %>% 
  mutate(id = id)

#write_csv(run_number_1, file = "data/processed/run_number_1.csv")
write_csv(run_number_1, file = "data/processed/run_number_2.csv")

## intervention

intervention_1 <- 
  here("data/raw/intervention-two-three.csv") %>% 
  #here("data/raw/intervention-none-one.csv") %>% 
  read_delim(delim = ",", skip = 6,
             col_select = 'intervention-type') %>% 
  rename(intervention = `intervention-type`) %>% 
  mutate(id = id)

#write_csv(intervention_1, file = "data/processed/intervention_1.csv")
write_csv(intervention_1, file = "data/processed/intervention_2.csv")

## p_rewire

p_rewire_1 <- 
  here("data/raw/intervention-two-three.csv") %>% 
  #here("data/raw/intervention-none-one.csv") %>% 
  read_delim(delim = ",", skip = 6,
             col_select = 'p-rewire') %>% 
  rename(p_rewire = `p-rewire`) %>% 
  mutate(id = id)

#write_csv(p_rewire_1, file = "data/processed/p_rewire_1.csv")
write_csv(p_rewire_1, file = "data/processed/p_rewire_2.csv")

## p_com

p_com_1 <- 
  here("data/raw/intervention-two-three.csv") %>% 
  #here("data/raw/intervention-none-one.csv") %>% 
  read_delim(delim = ",", skip = 6,
             col_select = 'p-com') %>% 
  rename(p_com = `p-com`) %>% 
  mutate(id = id)

#write_csv(p_com_1, file = "data/processed/p_com_1.csv")
write_csv(p_com_1, file = "data/processed/p_com_2.csv")

## bc_threshold_mean

bc_threshold_mean_1 <- 
  here("data/raw/intervention-two-three.csv") %>% 
  #here("data/raw/intervention-none-one.csv") %>% 
  read_delim(delim = ",", skip = 6,
             col_select = 'bc-threshold-mean') %>% 
  rename(bc_threshold_mean = `bc-threshold-mean`) %>% 
  mutate(id = id)

#write_csv(bc_threshold_mean_1, file = "data/processed/bc_threshold_mean_1.csv")
write_csv(bc_threshold_mean_1, file = "data/processed/bc_threshold_mean_2.csv")

## alpha

alpha_1 <- 
  here("data/raw/intervention-two-three.csv") %>% 
  #here("data/raw/intervention-none-one.csv") %>% 
  read_delim(delim = ",", skip = 6,
             col_select = 'alpha') %>% 
  rename(alpha = `alpha`) %>% 
  mutate(id = id)

#write_csv(alpha_1, file = "data/processed/alpha_1.csv")
write_csv(alpha_1, file = "data/processed/alpha_2.csv")

## conf_mean

conf_mean_1 <- 
  here("data/raw/intervention-two-three.csv") %>% 
  #here("data/raw/intervention-none-one.csv") %>% 
  read_delim(delim = ",", skip = 6,
             col_select = 'conf-mean') %>% 
  rename(conf_mean = `conf-mean`) %>% 
  mutate(id = id)

#write_csv(conf_mean_1, file = "data/processed/conf_mean_1.csv")
write_csv(conf_mean_1, file = "data/processed/conf_mean_2.csv")

## r_mean

r_mean_1 <- 
  here("data/raw/intervention-two-three.csv") %>% 
  #here("data/raw/intervention-none-one.csv") %>% 
  read_delim(delim = ",", skip = 6,
             col_select = 'r-mean') %>% 
  rename(r_mean = `r-mean`) %>% 
  mutate(id = id)

#write_csv(r_mean_1, file = "data/processed/r_mean_1.csv")
write_csv(r_mean_1, file = "data/processed/r_mean_2.csv")

## credibility_mean

credibility_mean_1 <- 
  here("data/raw/intervention-two-three.csv") %>% 
  #here("data/raw/intervention-none-one.csv") %>% 
  read_delim(delim = ",", skip = 6,
             col_select = 'credibility-mean') %>% 
  rename(credibility_mean = `credibility-mean`) %>% 
  mutate(id = id)

#write_csv(credibility_mean_1, file = "data/processed/credibility_mean_1.csv")
write_csv(credibility_mean_1, file = "data/processed/credibility_mean_2.csv")
