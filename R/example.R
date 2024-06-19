df_edges <-
  tidyr::tribble(
    ~node1, ~node2, ~edge,
    "a", "b", 1,
    "a", "c", 1,
    "b", "c", 2
  )

tris <- get_triangles(df_edges)
triangle_plot(df_edges, tris)
learn_polys(df_edges, tris)


df_edges <-
  tidyr::tribble(
    ~node1, ~node2, ~edge,
    "a", "b", 1,
    "a", "c", 1,
    "b", "c", 3
  )

tris <- get_triangles(df_edges)
triangle_plot(df_edges, tris)
learn_polys(df_edges, tris)



df_edges <-
  tidyr::tribble(
    ~node1, ~node2, ~edge,
    "a", "b", 1,
    "a", "c", 1,
    "a", "d", 4,
    "b", "c", 3,
    "b", "d", 2,
    "c", "d", 2
  )

tris <- get_triangles(df_edges)
triangle_plot(df_edges, tris)
learn_polys(df_edges, tris)


df_edges <-
  tidyr::tribble(
    ~node1, ~node2, ~edge,
    "a", "b", 1,
    "a", "c", 1,
    "a", "d", 4,
    "b", "c", 3,
    "b", "d", 2,
    "c", "d", 2,
    "a", "e", 1,
    "b", "e", 5,
    "a", "f", 6,
    "e", "f", 4,
    "c", "g", 4,
    "d", "g", 1
  )

tris <- get_triangles(df_edges)
triangle_plot(df_edges, tris)
learn_polys(df_edges, tris)


