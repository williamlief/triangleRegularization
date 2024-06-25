#' 
#'
#' @param edge 
#' @param weight 
#' @param node1_weight 
#' @param node2_weight 
#'
#' @return a vector with three elements, the total force, and the weight force split between node1 and node2
#' @export
#'
#' @examples
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

#' Title
#'
#' @param df_edges 
#' @param deltas 
#'
#' @return
#' @export
#'
#' @examples
adjust_spring <- function(df_edges, deltas) {
  # Each spring contracts, and makes neighboring springs stretch
  for (i in 1:nrow(deltas)) {
    
    # edge in question shrinks by delta
    df_edges[i, "edge"] = df_edges[i, "edge"] - deltas[i,"edge"]
    
    # get the nodes in question
    n1 = unlist(df_edges[i, "node1"])
    n2 = unlist(df_edges[i, "node2"])
    
    # get their weighted share
    d1 = deltas[i, "node1_weight"]
    d2 = deltas[i, "node2_weight"]
    
    # connected edges stretch by weighted share of delta UNLESS one node has no
    # connections, in which case the other node moves by the full amount. If we
    # treat an unconnected node as being free, we lose force from the system and
    # it leads to degenerate convergence to 0 lengths. 
    
    n1_links = which((df_edges$node1 == n1 & df_edges$node2 != n2) | 
                       (df_edges$node2 == n1 & df_edges$node1 != n2))
    
    n2_links = which((df_edges$node1 == n2 & df_edges$node2 != n1) | 
                       (df_edges$node2 == n2 & df_edges$node1 != n1))
    


    if(length(n2_links) == 0) d1 = d1 + d2
    if(length(n1_links) == 0) d2 = d2 + d1 
    if(length(n1_links) == 0 & length(n2_links) == 0) {
      warning(paste("nodes:",n1, n2, "have no other connections. Distance will converge to 0"))
    }
    
    # If a node is connected to multiple nodes, we need to split its delta among
    # them otherwise we add force to the system which also has problems for
    # convergence.
    if(length(n1_links) != 0) d1 = d1 / length(n1_links)
    if(length(n2_links) != 0) d2 = d2 / length(n2_links)
    
    # finally stretch the connected edges
    df_edges[n1_links, "edge"] = df_edges[n1_links, "edge"] + d1
    df_edges[n2_links, "edge"] = df_edges[n2_links, "edge"] + d2
  }
  
  return(df_edges)
}


#' Title
#'
#' @param df_edges 
#' @param alpha 
#' @param tolerance 
#'
#' @return
#' @export
#'
#' @examples
learn_spring <- function(df_edges, alpha = 0.1, tolerance = 0.001) {

  converged = FALSE
  history <- list()
  iter <- 0
  
  while (!converged) {
    iter = iter+1
    current_edges <- df_edges$edge
    history[[iter]] <- df_edges
    
    # calculate how much each spring will move
    deltas <- apply(df_edges[c("edge", "weight", "node1_weight", "node2_weight")], 1, function(row) {
      measure_spring(row['edge'], 
                     row['weight'],
                     row['node1_weight'], 
                     row['node2_weight'])
    }) 
    deltas <- t(deltas * alpha)
    
    df_edges <- adjust_spring(df_edges, deltas)

    converged = sum(abs(current_edges - df_edges$edge)) < tolerance
  }
  
  res <- list(
    df_edges = df_edges,
    history = history
  )
  
  return(res)
}  

# test <- learn_spring(df_edges)
# 
# library(tidyverse)
# x <- bind_rows(test$history, .id = "iter") |> 
#   mutate(
#     iter = as.numeric(iter),
#     edge_name = paste0(node1,"<->",node2)
#   )
# 
# ggplot(data = x,
#        aes(x = iter, y = edge, group = edge_name, color = edge_name)) +
#   geom_line() +
#   facet_wrap("edge_name")

