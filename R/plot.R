#' Create plot of triangles
#'
#' Distance between nodes is not scaled to distance values - it can't be since
#' we want to show invalid triangles! Invalid triangles are highlighted in
#' red. Since edges are shared, a valid triangle may have all of its edges
#' showing as red if all neighboring triangles are invalid.
#'
#' @inheritParams learn_polys
#' @param layout Node layout is non-deterministic. See igraph package
#'   documentation for more details. You can save a layout using
#'   `save_layout` to have the same node layout in multiple graphs.
#' @param ... additional arguments passed to `plot`
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
#' # to have persistent layout
#' layout <- save_layout(df_edges)
#' triangle_plot(df_edges, tris, layout = layout)
#' triangle_plot(df_edges, tris, layout = layout)
triangle_plot <- function(df_edges, polys, layout = NULL, tolerance = 0.001, ...) {

  bad_polys <- invalid_polys(df_edges, polys)
  
  df_edges$valid <- c()
  
  for(i in 1:length(polys)){
    df_edges[polys[[i]],"invalid"] = bad_polys[i] > tolerance
  }
  
  g <- igraph::graph_from_data_frame(df_edges, directed = FALSE)

  edge_colors <- ifelse(igraph::E(g)$invalid == TRUE, "red", "grey")

  if (is.null(layout)) layout = igraph::layout_nicely(g)
  
  plot(g, 
       layout = layout,
       edge.color = edge_colors,
       edge.label = round(igraph::E(g)$edge, 2),
       ...)

}

#' Create a plot of springs
#'
#' @inheritParams learn_spring
#' @param layout Node layout is non-deterministic. See igraph package
#'   documentation for more details. You can save a layout using
#'   `save_layout` to have the same node layout in multiple graphs.
#' @param ... additional arguments passed to `plot`#' @param ... 
#'
#' @return a plot
#' @export
#'
#' @examples
#' df_edges <-
#' tidyr::tribble(
#'   ~node1, ~node2, ~edge, ~weight, ~node1_weight, ~node2_weight,
#'   "a", "a1", 2, 1, 1, 1,
#'   "a1", "b1", 1, 1, 1, 1,
#'   "b1", "b", 2, 1, 1, 1,
#' )
#' 
#' layout <- save_layout(df_edges)
#' spring_plot(df_edges, layout = layout, main = "input graph")
spring_plot <- function(df_edges, layout = NULL, ...) {
  
  g <- igraph::graph_from_data_frame(df_edges, directed = FALSE)
  if (is.null(layout)) layout = igraph::layout_nicely(g)
  
  plot(g,
       layout = layout, 
       edge.label = round(igraph::E(g)$edge, 2),
       ...)
  
}

#' Persistent layout of nodes
#'
#' @inheritParams learn_poly
#' @param layout a layout function from the igraph package
#'
#' @return A numeric matrix with two or three columns.
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
#' layout <- save_layout(df_edges)
#' triangle_plot(df_edges, tris, layout = layout)
#' triangle_plot(df_edges, tris, layout = layout)
save_layout <- function(df_edges, layout = igraph::layout_nicely) {
  
  g <- igraph::graph_from_data_frame(df_edges, directed = FALSE)
  
  return(layout(g))
  
}
