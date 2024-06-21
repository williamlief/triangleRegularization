
# Hook's law f = k*x
# weight = k
# edge = d
# relaxed state is d = 0 ?

df_edges <-
  tidyr::tribble(
    ~node1, ~node2, ~edge, ~weight, ~node1_weight, ~node2_weight,
    "a", "a1", 2, 1, 1, 1,
    "a1", "b1", 1, 1, 1, 1,
    "b1", "b", 2, 1, 1, 1,
  )

measure_spring <- function(edge, weight, node1_weight, node2_weight) {
  
  # this edge reduces by f
  f = edge *  weight
  
  # all other edges connected to node1 or node2 increase by w*f/2
  w1 = 1 / node1_weight # bigger weight means less adjustment
  w2 = 1 / node2_weight 
  w1_norm = w1 / (w1 + w2)
  w2_norm = w2 / (w1 + w2) # using 1-w1_norm messes up inherited names
  
  f1 = w1_norm * f
  f2 = w2_norm * f
  
  return(c(f, f1, f2))
  
}

learn_spring <- function(df_edges, alpha = 0.1, tolerance = 0.001) {

  converged = FALSE
  history <- list()
  iter <- 0
  
  while (!converged) {
    iter = iter+1
    current_edges <- df_edges$edge
    history[[iter]] <- current_edges
    
    # calculate how much each spring will move
    deltas <- apply(df_edges[c("edge", "weight", "node1_weight", "node2_weight")], 1, function(row) {
      measure_spring(row['edge'], 
                     row['weight'],
                     row['node1_weight'], 
                     row['node2_weight'])
    }) 
    deltas <- t(deltas * alpha)
    
    # Each spring contracts, and makes neighboring springs stretch
    for (i in 1:nrow(deltas)) {
      
      # edge in question shrinks by delta
      df_edges[i, "edge"] = df_edges[i, "edge"] - deltas[i,"edge"]
      
      # connected edges stretch by n1/n2 weighted share of delta
      n1 = unlist(df_edges[i, "node1"])
      n2 = unlist(df_edges[i, "node2"])
      d1 = deltas[i, "node1_weight"]
      d2 = deltas[i, "node2_weight"]
      
      # node 1 connections
      df_edges$edge = ifelse((df_edges$node1 == n1 & df_edges$node2 != n2) | 
                             (df_edges$node2 == n1 & df_edges$node1 != n2),
                             df_edges$edge + d1, 
                             df_edges$edge)
      
      # node2 connections
      df_edges$edge = ifelse((df_edges$node1 == n2 & df_edges$node2 != n1) | 
                             (df_edges$node2 == n2 & df_edges$node1 != n1),
                             df_edges$edge + d2, 
                             df_edges$edge)
    }

    converged = sum(abs(current_edges - df_edges$edge)) < tolerance
  }
  
  return(history)
}  

history <- learn_spring(df_edges)

library(tidyverse)
x <- do.call(rbind, lapply(history, function(x) unlist(x)))
x <- data.frame(x)
names(x) <- c("edge_a_a1", "edge_a1_b1", "edge_b1_b")
x <- x |> rownames_to_column()
x$rowname <- as.numeric(x$rowname)
x <- pivot_longer(data.frame(x), -rowname)

ggplot(data = x, aes(x = rowname, y = value, group = name, color = name)) +
  geom_line() +
  facet_wrap(~name)