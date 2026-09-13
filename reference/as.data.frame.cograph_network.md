# Cograph Network as a Data Frame

The tidy accessor for a `cograph_network`: one row per edge (or per
node), with endpoints given as labels rather than internal indices, so
no caller has to reach into the object with `$` or translate integer ids
by hand.

## Usage

``` r
# S3 method for class 'cograph_network'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  what = c("edges", "nodes")
)
```

## Arguments

- x:

  A `cograph_network` object.

- row.names:

  `NULL` or a character vector of row names, as for
  [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html).

- optional:

  Logical, as for
  [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). Ignored;
  the column names of the returned table are always the documented ones.

- ...:

  Unused, for compatibility with the generic.

- what:

  Which table to return. `"edges"` (default) or `"nodes"`.

## Value

A base data frame. For `what = "edges"`, one row per edge with columns
`from` and `to` (node labels), `weight`, and any extra edge columns the
network carries (for example `session`). For `what = "nodes"`, one row
per node with the node metadata columns (`id`, `label`, layout
coordinates, and any custom columns).

## See also

[`to_df`](https://sonsoles.me/cograph/reference/to_data_frame.md),
[`get_edges`](https://sonsoles.me/cograph/reference/get_edges.md),
[`get_nodes`](https://sonsoles.me/cograph/reference/get_nodes.md)

## Examples

``` r
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
net <- as_cograph(adj)

as.data.frame(net)
#>   from to weight
#> 1    A  B    0.5
#> 2    A  C    0.8
#> 3    B  C    0.3
#> 4    B  D    0.6
#> 5    C  D    0.4
as.data.frame(net, what = "nodes")
#>   id label name  x  y
#> 1  1     A    A NA NA
#> 2  2     B    B NA NA
#> 3  3     C    C NA NA
#> 4  4     D    D NA NA
```
