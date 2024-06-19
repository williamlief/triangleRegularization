#' Checks polygon inequality rule and returns distance value
#'
#' For any polygon the longest side has to be shorter than the sum of all other
#' sides
#'
#' @param edges a vector of edges or sides
#'
#' @return the distance between the longest side and the sum of all other sides.
#'  Negative values are valid polys.
#' @export
#'
#' @examples
#' check_geom(c(1,2,3))
#' check_gem(c(1,2,5,1))
check_poly <- function(edges) {
  sort <- sort(edges)
  check = sort[length(sort)] - sum(sort[-length(sort)])

  return(check)
}

#' Creates a vector with 0 for a valid poly or the amount of excess distance
#'
#' @param df_edges
#' @param polys
#'
#' @return
#' @export
#'
#' @examples
invalid_polys <- function(df_edges, polys) {
  checks <- vector(length = length(polys))
  for(i in 1:length(polys)){
    checks[i] = check_poly(unlist(df_edges[polys[[i]],"edge"]))
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


#' Partitions a set of edges into a list of polys
#'
#' @param df_edges a dataframe with columns node1, node2, edge.
#' @param n the number of nodes to include in each shape.
#'
#' @return a list of vectors, where each vector contains the row indices from
#'   the input df_edges that make up a poly
#' @export
#'
#' @examples
create_polys <- function(df_edges, n = 3) {
  # ToDO

  # hack hardcoded placeholder for the example dfs
  if (length(df_edges) == 3) {
    poly =  list("abc" = c(1,2,3))
  }
  if (length(df_edges) == 6) {
    poly = list(
      "abc" = c(1,2,4),
      "abd" = c(1,3,5),
      "acd" = c(2,6,3),
      "bcd" = c(4,6,5)
    )
  }

  return(poly)
}


#' Title
#'
#' @param df_edges
#' @param polys
#' @param alpha
#' @param tolerance
#'
#' @return
#' @export
#'
#' @examples
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









