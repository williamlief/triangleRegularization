#' Adjust distances between nodes so all polygons are valid
#'
#' @param df_edges a dataframe with columns `node1`, `node2`, `edge`, `weight`. weight
#'   col can be excluded. First two columns must be node1, node2 because of use
#'   of igraph:::graph_from_data_frame.
#' @param polys a list of lists, where each element is the row indices from
#'   df_edges that make up a polygon
#' @param use_weights logical
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
#'  ~node1, ~node2, ~edge, ~weight,
#'  "a", "b", 1, 1,
#'  "a", "c", 1, 1,
#'  "b", "c", 3, 1
#' )
#'
#' tris <- get_triangles(df_edges)
#' learn_polys(df_edges, tris)
learn_polys <- function(df_edges, polys, use_weights = FALSE, alpha = 0.1, tolerance = 0.001) {
  
  check_dim_df_edges(df_edges)
  
  checks <- invalid_polys(df_edges, polys)
  
  while (max(checks) > tolerance) {
    
    # check_poly returns how much the poly edges need to be moved, but we use a
    # learning rate to reduce the movement. If invalid polys share an edge, a
    # learning rate of 1 would be bad for convergence
    deltas <- checks * alpha
    
    
    
    for(i in 1:length(polys)){
      
      weights = if(use_weights) unlist(df_edges[polys[[i]], "weight"]) else NULL
      
      df_edges[polys[[i]],"edge"] = adjust_poly(edges = unlist(df_edges[polys[[i]],"edge"]),
                                                delta = deltas[[i]],
                                                weights = weights
      )
    }
    
    checks <- invalid_polys(df_edges, polys)
    
  }
  
  return(df_edges)
}

#' Checks that df_edges has correct dimensions
#'
#' @inheritParams learn_polys 
#'
#' @return error or true
#' @export
check_dim_df_edges <- function(df_edges, use_weights = FALSE) {
  
  col_names <- names(df_edges)
  
  if(!all(col_names[1:2] == c("node1", "node2"))) stop("First two columns in df_edges must be `node1`, `node2`")
  if(!("edge" %in% col_names)) stop("`edge` column must be present in df_edges")
  if(use_weights & !("weight" %in% col_names)) stop("weights param set to True, but column `weight` not found in df_edges")
  
  return(TRUE)
}

#' Checks polygon inequality rule, if invalid returns excess distance value
#'
#' For any polygon the longest side has to be shorter than the sum of all other
#' sides. This will return that distance, returns 0 for valid polygons.
#'
#' @inheritParams learn_polys
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
#' @inheritParams learn_polys
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



#' Partitions a set of edges into a list of polys with n sides (doesn't exist)
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
create_polys <- function(df_edges, n = 3) {
  # ToDO

  stop("This function hasn't been created - see get_triangles instead")
}






