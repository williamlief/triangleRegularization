#' Adjust distances between nodes so all polygons are valid
#'
#' @param df_edges a dataframe with columns node1, node2, edge.
#' @param polys a list of lists, where each element are the row indices from
#'   df_edges that make up a polygon
#' @param alpha learning rate
#' @param tolerance allowed error in checking polygon inequality
#'
#' @return a dataframe of the same structure as df_edges, but edge values have
#'   been adjusted so all polygons are valid.
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
#' learn_polys(df_edges, tris)
learn_polys <- function(df_edges, polys, alpha = 0.1, tolerance = 0.001) {
  
  checks <- invalid_polys(df_edges, polys)
  
  while (max(checks) > tolerance) {
    
    # check_poly returns how much the poly edges need to be moved, but we use a
    # learning rate to reduce the movement. If invalid polys share an edge, a
    # learning rate of 1 would be bad for convergence
    deltas <- checks * alpha
    
    for(i in 1:length(polys)){
      df_edges[polys[[i]],"edge"] = adjust_poly(unlist(df_edges[polys[[i]],"edge"]),
                                                deltas[[i]])
    }
    
    checks <- invalid_polys(df_edges, polys)
    
  }
  
  return(df_edges)
}



#' Checks polygon inequality rule, if invalid returns excess distance value
#'
#' For any polygon the longest side has to be shorter than the sum of all other
#' sides. This will return that distance, returns 0 for valid polygons.
#'
#' @paramInherits learn_polys
#'
#' @return vector of same length as polys containing 0 for valid polys or the
#'   excess distance
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
#' invalid_polys(df_edges, tris)
invalid_polys <- function(df_edges, polys) {
  
  checks <- vector(length = length(polys))
  
  for(i in 1:length(polys)){
    
    edges = unlist(df_edges[polys[[i]], "edge"])
    edges = sort(edges)
    check = edges[length(edges)] - sum(edges[-length(edges)])
    
    checks[i] = check
  }
  # negative values mean poly is valid and doesnt need to be adjusted
  checks <- ifelse(checks < 0, 0, checks)

  return(checks)
}



#' Adjusts edges to become a valid poly
#'
#' @param edges a vector of edges
#' @param delta total adjustment amount, will be divided by number of edges
#' @param weights null or a vector of same length as edges. Higher weights mean bigger adjustment
#'
#' @return a vector of edges adjusted to become valid
#' @export
#'
#' @examples
#' adjust_geom(c(1,2,4), 1)
#' adjust_geom(c(1,2,4), 1, weights = c(1,1,2))
adjust_poly <- function(edges, delta, weights = NULL) {

  n_edges = length(edges)

  adjust_factor = delta / n_edges

  if (!is.null(weights)) {
    if(length(weights) != length(edges)) stop("edges and weights differing lengths")
    norm_weights = weights / sum(weights)
    adjust_factor = adjust_factor * norm_weights
  }

  # big side gets smaller, small sides get bigger
  edges = ifelse(edges == max(edges),
                 edges - adjust_factor,
                 edges + adjust_factor)

  return(edges)
}



#' Partitions a set of edges into a list of polys with n sides
#' 
#' I haven't figured out how to do this, I am using the igraph package and creating 
#' triangles in `get_triangles` instead.
#'
#' @inheritParams learn_polys
#' @param n the number of nodes to include in each shape.
#'
#' @return a list of vectors, where each vector contains the row indices from
#'   the input df_edges that make up a poly
#' @export
#'
#' @examples
create_polys <- function(df_edges, n = 3) {
  # ToDO

  stop("This function hasn't been created - see get_triangles instead")
}






