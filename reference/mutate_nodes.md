# Add or Change Node Attributes

Evaluates expressions against the node table, with the same centrality
and structural vocabulary that
[`select_nodes()`](https://sonsoles.me/cograph/reference/select_nodes.md)
offers, and stores the results as node columns.

## Usage

``` r
mutate_nodes(x, ..., keep_format = FALSE, directed = NULL)
```

## Arguments

- x:

  Network input.

- ...:

  Named expressions, for example `hub = degree > 3` or
  `score = pagerank * 100`. Available names are the existing node
  columns plus every measure and predicate listed under
  [`select_nodes`](https://sonsoles.me/cograph/reference/select_nodes.md).

- keep_format:

  Logical. Return the input format when TRUE. Note that only igraph and
  cograph_network formats can carry node attributes; a matrix cannot,
  and the new columns are lost.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A `cograph_network` whose node table has the new columns, or the input
format when `keep_format = TRUE`.

## See also

[`mutate_edges`](https://sonsoles.me/cograph/reference/mutate_edges.md),
[`select_nodes`](https://sonsoles.me/cograph/reference/select_nodes.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md)

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1,
                1, 0, 1, 0,
                1, 1, 0, 0,
                1, 0, 0, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

as.data.frame(mutate_nodes(adj, deg = degree, hub = degree >= 3),
              what = "nodes")
#>   id label name  x  y deg   hub
#> 1  1     A    A NA NA   3  TRUE
#> 2  2     B    B NA NA   2 FALSE
#> 3  3     C    C NA NA   2 FALSE
#> 4  4     D    D NA NA   1 FALSE
```
