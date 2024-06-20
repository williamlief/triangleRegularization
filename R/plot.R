#' Create plot of triangles
#'
#' Distance between nodes is not scaled to distance values - it can't be since
#' we want to show invalid triangles! Invalid triangles are highlighted in
#' red.Since edges are shared, a valid triangle is one where at least one side
#' is grey.
#'
#' @inheritParams learn_polys
#'
#' @return a plot
#' @export
#'
#' @examples
#' df_edges <-
#' tidyr::tribble(
#'  ~node1, ~node2, ~edge,
#'  "a", "b", 1,
#'  "a", "c", 1,
#'  "b", "c", 3
#' )
#'
#' tris <- get_triangles(df_edges)
#' triangle_plot(df_edges, tris)
triangle_plot <- function(df_edges, polys, tolerance = 0.001) {

  bad_polys <- invalid_polys(df_edges, polys)
  
  df_edges$valid <- c()
  
  for(i in 1:length(polys)){
    df_edges[polys[[i]],"valid"] = bad_polys[i] <= tolerance
  }
  
  g <- igraph::graph_from_data_frame(df_edges, directed = FALSE)

  # TODO: highlight the edges that are too big
  edge_colors <- ifelse(igraph::E(g)$valid == TRUE, "grey", "red")

  plot(g,
       edge.color = edge_colors,
       edge.label = round(igraph::E(g)$edge, 2))

}
