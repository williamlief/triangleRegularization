
#' Title
#'
#' @param df_edges
#'
#' @return
#' @export
#'
#' @examples
get_triangles <- function(df_edges) {

  g <- igraph::graph_from_data_frame(df_edges, directed = FALSE)
  triangles <- t(matrix(igraph::triangles(g), nrow = 3))

  # igraph::triangles gives us all the triangles in the graph, but listed as the node ids
  # need to convert back to the original rows in our df.
  # this is hacky and awful
  df_edges <- df_edges |> tibble::rownames_to_column() # this has to be below creation of g!
  triangle_index <- list(length = nrow(triangles))
  for (i in 1:nrow(triangles)) {

    triangle <- triangles[i,]
    node_combinations <- combn(triangle, 2)

    edges <- apply(node_combinations, 2, function(nodes) {
      df_edges |>
      dplyr::filter(
        (node1 == igraph::V(g)[nodes[1]]$name & node2 == igraph::V(g)[nodes[2]]$name) |
        (node1 == igraph::V(g)[nodes[2]]$name & node2 == igraph::V(g)[nodes[1]]$name)) |>
        dplyr::pull(rowname)
    })
    triangle_index[i] <- list(edges)

  }
  return(triangle_index)

}



