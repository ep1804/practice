library(igraph)
library(tidyverse)

# csv file is modifed from following source
# https://docs.google.com/spreadsheets/d/15ghp8rJ_vdVgxZIVJGawAYQXRMZSVHJYpZRfQUplAhE/htmlview

d1 = read.csv('card.csv') %>% as_tibble()

# https://stackoverflow.com/a/41585689

d2 = d1 %>% map_dfc(function(x){x[is.na(x)] <- 0; x})

g1 = make_empty_graph(5) %>%
  set_vertex_attr('label', value = c('Ruby', 'Emerald', 'Sappire', 'Diamond', 'Onyx')) %>%
  set_vertex_attr('color', value = c('red', 'green', 'blue', 'gray', 'black'))

plotTokenToDevelopGraph = function(d1, merge_edge = F){

  g1 = make_empty_graph(5) %>%
    set_vertex_attr('label', value = c('Ruby', 'Emerald', 'Sappire', 'Diamond', 'Onyx')) %>%
    set_vertex_attr('color', value = c('red', 'green', 'blue', 'gray', 'black'))

  for (index in 1:nrow(d1)) {
    row = d1[index,]
    
    dest = NULL
    if (row$develop == 'ruby') {
      dest = 1
    } else if (row$develop == 'emerald') {
      dest = 2
    } else if (row$develop == 'sappire') {
      dest = 3
    } else if (row$develop == 'diamond') {
      dest = 4
    } else if (row$develop == 'onyx') {
      dest = 5
    }
    
    if (merge_edge) {

      if (row$token_ruby > 0) {
        if (length(incident(g1, 1, c('out'))) == 0) {
          g1 = add_edges(g1, c(1, dest), color = 'red', weight = row$token_ruby, loop.angle = 0)
        } else {
          eg = incident(g1, 1, c('out'))[1]
          eg$
          g1 = add_edges(g1, c(1, dest), color = 'red', weight = row$token_ruby, loop.angle = 0)
        }
      }
      
    } else {

      if (row$token_ruby > 0) {
        g1 = add_edges(g1, c(1, dest), color = 'red', weight = row$token_ruby, loop.angle = 0)
      }
      
      if (row$token_emerald > 0) {
        g1 = add_edges(g1, c(2, dest), color = 'green', weight = row$token_emerald, loop.angle = pi*0.4*4)
      }
      
      if (row$token_sappire > 0) {
        g1 = add_edges(g1, c(3, dest), color = 'blue', weight = row$token_sappire, loop.angle = pi*0.4*3)
      }
      
      if (row$token_diamond > 0) {
        g1 = add_edges(g1, c(4, dest), color = 'gray', weight = row$token_diamond, loop.angle = pi*0.4*2)
      }
      
      if (row$token_onyx > 0) {
        g1 = add_edges(g1, c(5, dest), color = 'black', weight = row$token_onyx, loop.angle = pi*0.4*1)
      }
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

plotTokenToDevelopGraph(d2)

plotTokenToDevelopGraph(d2[d2$develop == 'ruby', ])
plotTokenToDevelopGraph(d2[d2$level == 1 & d2$develop == 'ruby', ])
plotTokenToDevelopGraph(d2[d2$level == 2 & d2$develop == 'ruby', ])
plotTokenToDevelopGraph(d2[d2$level == 3 & d2$develop == 'ruby', ])

plotTokenToDevelopGraph(d2[d2$develop == 'emerald', ])
plotTokenToDevelopGraph(d2[d2$level == 1 & d2$develop == 'emerald', ])
plotTokenToDevelopGraph(d2[d2$level == 2 & d2$develop == 'emerald', ])
plotTokenToDevelopGraph(d2[d2$level == 3 & d2$develop == 'emerald', ])

plotTokenToDevelopGraph(d2[d2$develop == 'sappire', ])
plotTokenToDevelopGraph(d2[d2$level == 1 & d2$develop == 'sappire', ])
plotTokenToDevelopGraph(d2[d2$level == 2 & d2$develop == 'sappire', ])
plotTokenToDevelopGraph(d2[d2$level == 3 & d2$develop == 'sappire', ])

plotTokenToDevelopGraph(d2[d2$develop == 'diamond', ])
plotTokenToDevelopGraph(d2[d2$level == 1 & d2$develop == 'diamond', ])
plotTokenToDevelopGraph(d2[d2$level == 2 & d2$develop == 'diamond', ])
plotTokenToDevelopGraph(d2[d2$level == 3 & d2$develop == 'diamond', ])

plotTokenToDevelopGraph(d2[d2$develop == 'onyx', ])
plotTokenToDevelopGraph(d2[d2$level == 1 & d2$develop == 'onyx', ])
plotTokenToDevelopGraph(d2[d2$level == 2 & d2$develop == 'onyx', ])
plotTokenToDevelopGraph(d2[d2$level == 3 & d2$develop == 'onyx', ])

