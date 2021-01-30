library(tidyverse)
library(tidygraph)

d1 = readRDS('card.rds') %>% as_tibble()


# token - develop graph

d2 = d1 %>% select(-point)

