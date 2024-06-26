% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/polys.R
\name{check_dim_df_edges}
\alias{check_dim_df_edges}
\title{Checks that df_edges has correct dimensions}
\usage{
check_dim_df_edges(df_edges, use_weights = FALSE)
}
\arguments{
\item{df_edges}{a dataframe with columns `node1`, `node2`, `edge`, `weight`. weight
col can be excluded. First two columns must be node1, node2 because of use
of igraph:::graph_from_data_frame.}

\item{use_weights}{logical}
}
\value{
error or true
}
\description{
Checks that df_edges has correct dimensions
}
