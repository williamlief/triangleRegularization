
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Triangle Regularization

Given a list of nodes and edge distances, updates edges so that for any
set of three connected nodes, a valid triangle can be formed using the
triangle inequality rule.

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the development version of Triangle Regularization from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("williamlief/triangleRegularization")
```

## Examples

### A single triangle

This is a basic example with a valid triangle, no updates are made. Note
that the edge length labels are NOT proportional to the graphed length -
this is necessary because we will be showing invalid triangles, that is,
cases where you can’t close the shape with the listed edge lengths.

``` r
library(triangleRegularization)

df_edges <-
  tidyr::tribble(
    ~node1, ~node2, ~edge,
    "a", "b", 1,
    "a", "c", 1,
    "b", "c", 2
  )

layout <- save_layout(df_edges)
tris <- get_triangles(df_edges)

triangle_plot(df_edges, tris, layout, main = "input graph")
update <- learn_polys(df_edges, tris)
triangle_plot(update, tris, layout, main = "updated graph")
```

<img src="man/figures/README-example_valid, figures-side-1.png" width="50%" /><img src="man/figures/README-example_valid, figures-side-2.png" width="50%" />

Now we have an invalid triangle and we update it to be valid. We are
implicitly using equal weights here, so the long side and the short
sides are all adjusted by the same amount.

``` r
df_edges <-
  tidyr::tribble(
    ~node1, ~node2, ~edge,
    "a", "b", 1,
    "a", "c", 1,
    "b", "c", 3
  )

layout <- save_layout(df_edges)
tris <- get_triangles(df_edges)

triangle_plot(df_edges, tris, layout, main = "input graph")
update <- learn_polys(df_edges, tris)
triangle_plot(update, tris, layout, main = "updated graph")
```

<img src="man/figures/README-example_invalid, figures-side-1.png" width="50%" /><img src="man/figures/README-example_invalid, figures-side-2.png" width="50%" />

### Multiple Triangles

Now we have a mix of valid and invalid triangles, with shared edges.

``` r
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

layout <- save_layout(df_edges)
tris <- get_triangles(df_edges)

triangle_plot(df_edges, tris, layout, main = "input graph")
update <- learn_polys(df_edges, tris)
triangle_plot(update, tris, layout, main = "updated graph")
```

<img src="man/figures/README-example_with_square, figures-side-1.png" width="50%" /><img src="man/figures/README-example_with_square, figures-side-2.png" width="50%" />

And finally here is a complex example

``` r

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

layout <- save_layout(df_edges)
tris <- get_triangles(df_edges)

triangle_plot(df_edges, tris, layout, main = "input graph")
update <- learn_polys(df_edges, tris)
triangle_plot(update, tris, layout, main = "updated graph")
```

<img src="man/figures/README-example_complex, figures-side-1.png" width="50%" /><img src="man/figures/README-example_complex, figures-side-2.png" width="50%" />

### Using Weights

You can also include a `weight` column in df_edges. The weights column
will be normalized within each triangle as `w = w/sum(w)`. Edges with
larger weights will be adjusted proportionally less than edges with
smaller weights.

``` r
df_edges <-
  tidyr::tribble(
    ~node1, ~node2, ~edge, ~weight,
    "a", "b", 1, 1,
    "a", "c", 1, 1,
    "b", "c", 3, 4
  )

layout <- save_layout(df_edges)
tris <- get_triangles(df_edges)

triangle_plot(df_edges, tris, layout, main = "input graph")
update <- learn_polys(df_edges, tris, use_weights = TRUE)
triangle_plot(update, tris, layout, main = "updated graph")
```

<img src="man/figures/README-example_weights, figures-side-1.png" width="50%" /><img src="man/figures/README-example_weights, figures-side-2.png" width="50%" />
