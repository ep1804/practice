library(igraph)
library(tidyverse)

d1 = readRDS('card.rds') %>% as_tibble()


# token - develop graph

d2 = d1 %>% select(-point)

graph_t2d = function(d) {
  
  g1 = make_empty_graph(5) %>%
    set_vertex_attr('label', value = c('Diamond', 'Sapphire', 'Emerald', 'Ruby', 'Onyx')) %>%
    set_vertex_attr('color', value = c('gray', 'blue', 'green', 'red', 'black'))
  
  for (index in 1:nrow(d)) {
    row = d[index,]
    
    dest = NULL
    if (row$develop == 'diamond') {
      dest = 1
    } else if (row$develop == 'sapphire') {
      dest = 2
    } else if (row$develop == 'emerald') {
      dest = 3
    } else if (row$develop == 'ruby') {
      dest = 4
    } else if (row$develop == 'onyx') {
      dest = 5
    }
    
    if (row$token_diamond > 0) {
      g1 = add_edges(g1, c(1, dest), color = 'gray', weight = row$token_diamond, loop.angle = 0)
    }
    
    if (row$token_sapphire > 0) {
      g1 = add_edges(g1, c(2, dest), color = 'blue', weight = row$token_sapphire, loop.angle = pi*0.4*4)
    }
    
    if (row$token_emerald > 0) {
      g1 = add_edges(g1, c(3, dest), color = 'green', weight = row$token_emerald, loop.angle = pi*0.4*3)
    }
    
    if (row$token_ruby > 0) {
      g1 = add_edges(g1, c(4, dest), color = 'red', weight = row$token_ruby, loop.angle = pi*0.4*2)
    }
    
    if (row$token_onyx > 0) {
      g1 = add_edges(g1, c(5, dest), color = 'black', weight = row$token_onyx, loop.angle = pi*0.4*1)
    }
  }
  
  g1
}

graph_v2l = function(v) {
  
}

plot_circle = function(g) {
  plot(g,
       layout = layout_in_circle(g),
       vertex.size=15,
       vertex.color=V(g)$color,
       vertex.frame.color='white',
       vertex.label.color='black',
       vertex.label.cex=0.8,
       vertex.label.dist=2.5,
       edge.arrow.mode=2,
       edge.arrow.size= sqrt(E(g)$weight * 0.4 + 0.5) * 0.8,
       edge.width = E(g)$weight * 0.6 + 0.6)
}

plot_line = function(g) {
  
  curve_multiple(g)
  
  g %>%
    set_edge_attr("curved", value=0) %>%
    set_edge_attr("loop.angle", E(.)[1%--%1, 2%--%2, 3%--%3, 4%--%4, 5%--%5], pi * 0.7) %>%
    set_edge_attr("curved", E(.)[1:5 %->% 1], 0.7) %>%
    set_edge_attr("curved", E(.)[1:5 %->% 2], 0.7) %>%
    set_edge_attr("curved", E(.)[1:5 %->% 3], 0.7) %>%
    set_edge_attr("curved", E(.)[1:5 %->% 4], 0.7) %>%
    set_edge_attr("curved", E(.)[1:5 %->% 5], 0.7) %>%
    plot(
      layout = layout_on_grid(g, width=5, height=1),
      vertex.size=15,
      vertex.color=V(g)$color,
      vertex.frame.color='white',
      vertex.label.color='black',
      vertex.label.cex=0.8,
      vertex.label.dist=2.5,
      edge.arrow.mode=2,
      edge.arrow.size= sqrt(E(g)$weight * 0.4 + 0.5) * 0.8,
      edge.width = E(g)$weight * 0.6 + 0.6)
}


d1 %>% graph_t2d %>% plot_circle

d1 %>% filter(develop == 'diamond') %>% graph_t2d %>% plot_circle
d1 %>% filter(develop == 'diamond' & level == 1) %>% graph_t2d %>% plot_circle
d1 %>% filter(develop == 'diamond' & level == 2) %>% graph_t2d %>% plot_circle
d1 %>% filter(develop == 'diamond' & level == 3) %>% graph_t2d %>% plot_circle

d1 %>% filter(develop == 'sapphire') %>% graph_t2d %>% plot_circle
d1 %>% filter(develop == 'sapphire' & level == 1) %>% graph_t2d %>% plot_circle
d1 %>% filter(develop == 'sapphire' & level == 2) %>% graph_t2d %>% plot_circle
d1 %>% filter(develop == 'sapphire' & level == 3) %>% graph_t2d %>% plot_circle

d1 %>% filter(develop == 'emerald') %>% graph_t2d %>% plot_circle
d1 %>% filter(develop == 'emerald' & level == 1) %>% graph_t2d %>% plot_circle
d1 %>% filter(develop == 'emerald' & level == 2) %>% graph_t2d %>% plot_circle
d1 %>% filter(develop == 'emerald' & level == 3) %>% graph_t2d %>% plot_circle

d1 %>% filter(develop == 'ruby') %>% graph_t2d %>% plot_circle
d1 %>% filter(develop == 'ruby' & level == 1) %>% graph_t2d %>% plot_circle
d1 %>% filter(develop == 'ruby' & level == 2) %>% graph_t2d %>% plot_circle
d1 %>% filter(develop == 'ruby' & level == 3) %>% graph_t2d %>% plot_circle

d1 %>% filter(develop == 'onyx') %>% graph_t2d %>% plot_circle
d1 %>% filter(develop == 'onyx' & level == 1) %>% graph_t2d %>% plot_circle
d1 %>% filter(develop == 'onyx' & level == 2) %>% graph_t2d %>% plot_circle
d1 %>% filter(develop == 'onyx' & level == 3) %>% graph_t2d %>% plot_circle

# lets focus on color relation. do merge edges


d3 = d2 %>%
  group_by(level, develop) %>%
  summarise(token_diamond = sum(token_diamond),
            token_sapphire = sum(token_sapphire),
            token_emerald = sum(token_emerald),
            token_ruby = sum(token_ruby),
            token_onyx = sum(token_onyx))

print(d3)

d3 %>% filter(develop == 'diamond') %>% graph_t2d %>% plot_circle
d3 %>% filter(develop == 'diamond' & level == 1) %>% graph_t2d %>% plot_circle
d3 %>% filter(develop == 'diamond' & level == 2) %>% graph_t2d %>% plot_circle
d3 %>% filter(develop == 'diamond' & level == 3) %>% graph_t2d %>% plot_circle

d3 %>% filter(develop == 'sapphire') %>% graph_t2d %>% plot_circle
d3 %>% filter(develop == 'sapphire' & level == 1) %>% graph_t2d %>% plot_circle
d3 %>% filter(develop == 'sapphire' & level == 2) %>% graph_t2d %>% plot_circle
d3 %>% filter(develop == 'sapphire' & level == 3) %>% graph_t2d %>% plot_circle

d3 %>% filter(develop == 'emerald') %>% graph_t2d %>% plot_circle
d3 %>% filter(develop == 'emerald' & level == 1) %>% graph_t2d %>% plot_circle
d3 %>% filter(develop == 'emerald' & level == 2) %>% graph_t2d %>% plot_circle
d3 %>% filter(develop == 'emerald' & level == 3) %>% graph_t2d %>% plot_circle

d3 %>% filter(develop == 'ruby') %>% graph_t2d %>% plot_circle
d3 %>% filter(develop == 'ruby' & level == 1) %>% graph_t2d %>% plot_circle
d3 %>% filter(develop == 'ruby' & level == 2) %>% graph_t2d %>% plot_circle
d3 %>% filter(develop == 'ruby' & level == 3) %>% graph_t2d %>% plot_circle

d3 %>% filter(develop == 'onyx') %>% graph_t2d %>% plot_circle
d3 %>% filter(develop == 'onyx' & level == 1) %>% graph_t2d %>% plot_circle
d3 %>% filter(develop == 'onyx' & level == 2) %>% graph_t2d %>% plot_circle
d3 %>% filter(develop == 'onyx' & level == 3) %>% graph_t2d %>% plot_circle

d3 %>% filter(level == 1) %>% graph_t2d %>% plot_circle
d3 %>% filter(level == 2) %>% graph_t2d %>% plot_circle
d3 %>% filter(level == 3) %>% graph_t2d %>% plot_circle


# leave MAX only
d4 = d3 %>% pmap_dfr(function(...){
  row = as.vector(tibble(...))
  toks = c(row$token_diamond, row$token_sapphire, row$token_emerald, row$token_ruby, row$token_onyx)
  cut = max(toks)
  tibble(level = row$level,
         develop = row$develop,
         token_diamond = if (row$token_diamond >= cut) row$token_diamond else 0,
         token_sapphire = if (row$token_sapphire >= cut) row$token_sapphire else 0,
         token_emerald = if (row$token_emerald >= cut) row$token_emerald else 0,
         token_ruby = if (row$token_ruby >= cut) row$token_ruby else 0,
         token_onyx = if (row$token_onyx >= cut) row$token_onyx else 0)
})

print(d4)

d4 %>% filter(develop == 'diamond' & level == 2) %>% graph_t2d %>% plot_circle
d4 %>% filter(develop == 'diamond' & level == 3) %>% graph_t2d %>% plot_circle

d4 %>% filter(develop == 'sapphire' & level == 2) %>% graph_t2d %>% plot_circle
d4 %>% filter(develop == 'sapphire' & level == 3) %>% graph_t2d %>% plot_circle

d4 %>% filter(develop == 'emerald' & level == 2) %>% graph_t2d %>% plot_circle
d4 %>% filter(develop == 'emerald' & level == 3) %>% graph_t2d %>% plot_circle

d4 %>% filter(develop == 'ruby' & level == 2) %>% graph_t2d %>% plot_circle
d4 %>% filter(develop == 'ruby' & level == 3) %>% graph_t2d %>% plot_circle

d4 %>% filter(develop == 'onyx' & level == 2) %>% graph_t2d %>% plot_circle
d4 %>% filter(develop == 'onyx' & level == 3) %>% graph_t2d %>% plot_circle

topo_lv2 = d4 %>% filter(level == 2) %>% graph_t2d %>% topo_sort(mode = c("out"))


# leave MAX and second to MAX
d5 = d3 %>% pmap_dfr(function(...){
  row = as.vector(tibble(...))
  toks = c(row$token_diamond, row$token_sapphire, row$token_emerald, row$token_ruby, row$token_ruby)
  cut = toks[order(toks)[4]]
  tibble(level = row$level,
         develop = row$develop,
         token_diamond = if (row$token_diamond >= cut) row$token_diamond else 0,
         token_sapphire = if (row$token_sapphire >= cut) row$token_sapphire else 0,
         token_emerald = if (row$token_emerald >= cut) row$token_emerald else 0,
         token_ruby = if (row$token_ruby >= cut) row$token_ruby else 0,
         token_onyx = if (row$token_onyx >= cut) row$token_onyx else 0)
})

print(d5)




