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
as.data.frame(mutate_nodes(regulation_net, deg = degree), what = "nodes")
#>    id      label       name  x  y deg
#> 1   1    Explore    Explore NA NA   6
#> 2   2       Plan       Plan NA NA   7
#> 3   3    Monitor    Monitor NA NA   8
#> 4   4      Adapt      Adapt NA NA   6
#> 5   5    Reflect    Reflect NA NA   6
#> 6   6    Discuss    Discuss NA NA   5
#> 7   7 Synthesize Synthesize NA NA   4
#> 8   8   Evaluate   Evaluate NA NA   5
#> 9   9     Create     Create NA NA   7
#> 10 10      Share      Share NA NA   6
```
