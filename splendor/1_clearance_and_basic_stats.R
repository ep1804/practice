library(el)
library(tidyverse)


# data clearance

# csv file is modifed from following source
# https://docs.google.com/spreadsheets/d/15ghp8rJ_vdVgxZIVJGawAYQXRMZSVHJYpZRfQUplAhE/htmlview
d1 = read.csv('card.csv') %>% as_tibble()

# zero fill NA values
# https://stackoverflow.com/a/41585689
d2 = d1 %>% map_dfc(function(x){x[is.na(x)] <- 0; x})

saveRDS(d2, 'card.rds')


# basic statistics

d3 = readRDS('card.rds') %>% as_tibble()

# add some of our interest
d4 = d3 %>%
  mutate(token_all = token_diamond + token_sapphire + token_emerald + token_ruby + token_onyx) %>%
  mutate(point_per_token = point / token_all) %>%
  mutate(dev_diamond = as.integer(develop == 'diamond')) %>%
  mutate(dev_sapphire = as.integer(develop == 'sapphire')) %>%
  mutate(dev_emerald = as.integer(develop == 'emerald')) %>%
  mutate(dev_ruby = as.integer(develop == 'ruby')) %>%
  mutate(dev_onyx = as.integer(develop == 'onyx')) %>%
  mutate(dev_all = dev_diamond + dev_sapphire + dev_emerald + dev_ruby + dev_onyx) %>%
  mutate(dev_per_token = dev_all / token_all)

d5 = d4 %>%
  group_by(level) %>%
  summarise(td_mu = mean(token_diamond),
            ts_mu = mean(token_sapphire),
            te_mu = mean(token_emerald),
            tr_mu = mean(token_ruby),
            to_mu = mean(token_onyx),
            td_sd = sd(token_diamond),
            ts_sd = sd(token_sapphire),
            te_sd = sd(token_emerald),
            tr_sd = sd(token_ruby),
            to_sd = sd(token_onyx),
            tall_mu = mean(token_all),
            tall_sd = sd(token_all),
            p_mu = mean(point),
            p_sd = sd(point),
            ppt_mu = mean(point_per_token),
            ppt_sd = sd(point_per_token),
            d_mu = mean(dev_all),
            dpt_mu = mean(dev_per_token),
            dpt_sd = sd(dev_per_token),
            count = n())

print(d5)

d6 = d4 %>%
  group_by(level) %>%
  summarise(t_mu = mean(token_diamond),
            tall_mu = mean(token_all),
            p_mu = mean(point),
            ppt_mu = mean(point_per_token),
            d_mu = mean(dev_all),
            dpt_mu = mean(dev_per_token),
            t_sd = sd(token_diamond),
            tall_sd = sd(token_all),
            p_sd = sd(point),
            ppt_sd = sd(point_per_token),
            dpt_sd = sd(dev_per_token),
            count = n())

print(d6)
View(d6)


# some interesting statstics

d7 = d6
d7$token_for_9_developments = 9 / d7$dpt_mu
d7$token_for_15_points = 15 / d7$ppt_mu
d7$turn_for_9_developments = d7$token_for_9_developments / 3
d7$turn_for_15_points = d7$token_for_15_points / 3
d7 %>% select(level, token_for_9_developments, token_for_15_points,
              turn_for_9_developments, turn_for_15_points) %>% print

# token correlation

select_token_dev = function(d) {
  d %>% select(token_diamond, token_sapphire, token_emerald, token_ruby, token_onyx,
               dev_diamond, dev_sapphire, dev_emerald, dev_ruby, dev_onyx)
}

d4 %>% select_token_dev %>% el.cor
d4 %>% filter(level == 1) %>% select_token_dev %>% el.cor
d4 %>% filter(level == 2) %>% select_token_dev %>% el.cor
d4 %>% filter(level == 3) %>% select_token_dev %>% el.cor


# token association (TBD)


