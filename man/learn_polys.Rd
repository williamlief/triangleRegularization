% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/polys.R
\name{learn_polys}
\alias{learn_polys}
\title{Adjust distances between nodes so all polygons are valid}
\usage{
learn_polys(
  df_edges,
  polys,
  use_weights = FALSE,
  alpha = 0.1,
  tolerance = 0.001
)
}
\arguments{
\item{df_edges}{a dataframe with columns `node1`, `node2`, `edge`, `weight`. weight
col can be excluded. First two columns must be node1, node2 because of use
of igraph:::graph_from_data_frame.}

\item{polys}{a list of lists, where each element is the row indices from
df_edges that make up a polygon}

\item{use_weights}{logical}

\item{alpha}{learning rate}

\item{tolerance}{allowed error in checking polygon inequality}
}
\value{
a dataframe of the same structure as df_edges, but edge values have
  been adjusted so all polygons are valid.
}
\description{
Adjust distances between nodes so all polygons are valid
}
\examples{
df_edges <-
tidyr::tribble(
 ~node1, ~node2, ~edge, ~weight,
 "a", "b", 1, 1,
 "a", "c", 1, 1,
 "b", "c", 3, 1
)

tris <- get_triangles(df_edges)
learn_polys(df_edges, tris)
}
