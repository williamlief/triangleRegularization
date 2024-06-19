#' Title
#'
#' @param df_edges
#' @param polys
#'
#' @return
#' @export
#'
#' @examples
triangle_plot <- function(df_edges, polys) {

  g <- graph_from_data_frame(df_edges, directed = FALSE)

  # TODO: highlight the edges that are too big
  edge_colors <- ifelse(E(g)$edge == 1, "red", "gray")

  plot(g,
       edge.color = edge_colors,
       edge.label = E(g)$edge)

}
