#' Find all triangles in a set of nodes and edges
#'
#' @inheritParams learn_polys
#'
#' @return a list of lists, where each element is the row indices from
#'   df_edges that make up a triangle. 
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
get_triangles <- function(df_edges) {

  g <- igraph::graph_from_data_frame(df_edges, directed = FALSE)
  triangles <- t(matrix(igraph::triangles(g), nrow = 3))

  # igraph::triangles gives us all the triangles in the graph, but listed as the
  # node ids need to convert back to the original rows in our df. 
  # This is hacky and awful
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



