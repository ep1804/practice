library(igraph)
library(tidyverse)

d1 = readRDS('card.rds') %>% as_tibble()


# token - develop graph

d2 = d1 %>% select(-point)

plot_tok_to_dev = function(d){
  
  g1 = make_empty_graph(5) %>%
    set_vertex_attr('label', value = c('Ruby', 'Emerald', 'sapphire', 'Diamond', 'Onyx')) %>%
    set_vertex_attr('color', value = c('red', 'green', 'blue', 'gray', 'black'))
  
  for (index in 1:nrow(d)) {
    row = d[index,]
    
    dest = NULL
    if (row$develop == 'ruby') {
      dest = 1
    } else if (row$develop == 'emerald') {
      dest = 2
    } else if (row$develop == 'sapphire') {
      dest = 3
    } else if (row$develop == 'diamond') {
      dest = 4
    } else if (row$develop == 'onyx') {
      dest = 5
    }
    
    
    if (row$token_ruby > 0) {
      g1 = add_edges(g1, c(1, dest), color = 'red', weight = row$token_ruby, loop.angle = 0)
    }
    
    if (row$token_emerald > 0) {
      g1 = add_edges(g1, c(2, dest), color = 'green', weight = row$token_emerald, loop.angle = pi*0.4*4)
    }
    
    if (row$token_sapphire > 0) {
      g1 = add_edges(g1, c(3, dest), color = 'blue', weight = row$token_sapphire, loop.angle = pi*0.4*3)
    }
    
    if (row$token_diamond > 0) {
      g1 = add_edges(g1, c(4, dest), color = 'gray', weight = row$token_diamond, loop.angle = pi*0.4*2)
    }
    
    if (row$token_onyx > 0) {
      g1 = add_edges(g1, c(5, dest), color = 'black', weight = row$token_onyx, loop.angle = pi*0.4*1)
    }
  }
  
  plot(g1, 
       layout = layout_in_circle(g1),
       vertex.size=15,
       vertex.color=V(g1)$color,
       vertex.frame.color='white',
       vertex.label.color='black',
       vertex.label.cex=0.8,
       vertex.label.dist=2.5,
       edge.arrow.mode=2,
       edge.arrow.size=0.7,
       edge.width = E(g1)$weight * 1.7 + 1)
}

d1 %>% plot_tok_to_dev

d1 %>% filter(develop == 'diamond') %>% plot_tok_to_dev
d1 %>% filter(develop == 'diamond' & level == 1) %>% plot_tok_to_dev
d1 %>% filter(develop == 'diamond' & level == 2) %>% plot_tok_to_dev
d1 %>% filter(develop == 'diamond' & level == 3) %>% plot_tok_to_dev

d1 %>% filter(develop == 'sapphire') %>% plot_tok_to_dev
d1 %>% filter(develop == 'sapphire' & level == 1) %>% plot_tok_to_dev
d1 %>% filter(develop == 'sapphire' & level == 2) %>% plot_tok_to_dev
d1 %>% filter(develop == 'sapphire' & level == 3) %>% plot_tok_to_dev

d1 %>% filter(develop == 'emerald') %>% plot_tok_to_dev
d1 %>% filter(develop == 'emerald' & level == 1) %>% plot_tok_to_dev
d1 %>% filter(develop == 'emerald' & level == 2) %>% plot_tok_to_dev
d1 %>% filter(develop == 'emerald' & level == 3) %>% plot_tok_to_dev

d1 %>% filter(develop == 'ruby') %>% plot_tok_to_dev
d1 %>% filter(develop == 'ruby' & level == 1) %>% plot_tok_to_dev
d1 %>% filter(develop == 'ruby' & level == 2) %>% plot_tok_to_dev
d1 %>% filter(develop == 'ruby' & level == 3) %>% plot_tok_to_dev

d1 %>% filter(develop == 'onyx') %>% plot_tok_to_dev
d1 %>% filter(develop == 'onyx' & level == 1) %>% plot_tok_to_dev
d1 %>% filter(develop == 'onyx' & level == 2) %>% plot_tok_to_dev
d1 %>% filter(develop == 'onyx' & level == 3) %>% plot_tok_to_dev

# lets focus on color relation. do merge edges


d3 = d2 %>%
  group_by(level, develop) %>%
  summarise(token_diamond = sum(token_diamond),
            token_sapphire = sum(token_sapphire),
            token_emerald = sum(token_emerald),
            token_ruby = sum(token_ruby),
            token_onyx = sum(token_onyx))

print(d3)

d3 %>% filter(develop == 'diamond') %>% plot_tok_to_dev
d3 %>% filter(develop == 'diamond' & level == 1) %>% plot_tok_to_dev
d3 %>% filter(develop == 'diamond' & level == 2) %>% plot_tok_to_dev
d3 %>% filter(develop == 'diamond' & level == 3) %>% plot_tok_to_dev

d3 %>% filter(develop == 'sapphire') %>% plot_tok_to_dev
d3 %>% filter(develop == 'sapphire' & level == 1) %>% plot_tok_to_dev
d3 %>% filter(develop == 'sapphire' & level == 2) %>% plot_tok_to_dev
d3 %>% filter(develop == 'sapphire' & level == 3) %>% plot_tok_to_dev

d3 %>% filter(develop == 'emerald') %>% plot_tok_to_dev
d3 %>% filter(develop == 'emerald' & level == 1) %>% plot_tok_to_dev
d3 %>% filter(develop == 'emerald' & level == 2) %>% plot_tok_to_dev
d3 %>% filter(develop == 'emerald' & level == 3) %>% plot_tok_to_dev

d3 %>% filter(develop == 'ruby') %>% plot_tok_to_dev
d3 %>% filter(develop == 'ruby' & level == 1) %>% plot_tok_to_dev
d3 %>% filter(develop == 'ruby' & level == 2) %>% plot_tok_to_dev
d3 %>% filter(develop == 'ruby' & level == 3) %>% plot_tok_to_dev

d3 %>% filter(develop == 'onyx') %>% plot_tok_to_dev
d3 %>% filter(develop == 'onyx' & level == 1) %>% plot_tok_to_dev
d3 %>% filter(develop == 'onyx' & level == 2) %>% plot_tok_to_dev
d3 %>% filter(develop == 'onyx' & level == 3) %>% plot_tok_to_dev
