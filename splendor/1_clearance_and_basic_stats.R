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
  mutate(point_per_token = point / token_all)

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
            pt_mu = mean(point),
            pt_sd = sd(point),
            ptt_mu = mean(point_per_token),
            ptt_sd = sd(point_per_token),
            count = n())

print(d5)

d6 = d4 %>%
  group_by(level) %>%
  summarise(td_mu = mean(token_diamond),
            tall_mu = mean(token_all),
            pt_mu = mean(point),
            ptt_mu = mean(point_per_token),
            td_sd = sd(token_diamond),
            tall_sd = sd(token_all),
            pt_sd = sd(point),
            ptt_sd = sd(point_per_token),
            count = n())

print(d6)


# token correlation

select_token = function(d) {
  d %>% select(token_diamond, token_sapphire, token_emerald, token_ruby, token_onyx)
}

d4 %>% select_token %>% el.cor
d4 %>% filter(level == 1) %>% select_token %>% el.cor
d4 %>% filter(level == 2) %>% select_token %>% el.cor
d4 %>% filter(level == 3) %>% select_token %>% el.cor


# token association (TBD)


