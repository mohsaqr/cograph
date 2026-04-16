# Generalized Closeness Centrality

Sum of alpha^d over all nodes. Generalization of decay centrality
matching tidygraph's implementation.

## Usage

``` r
centrality_generalized_closeness(x, mode = "all", decay_parameter = 0.5, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- mode:

  For directed networks: `"all"` (default), `"in"`, or `"out"`.

- decay_parameter:

  Numeric between 0 and 1 (the alpha parameter). Default 0.5.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector of generalized closeness values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_decay`](https://sonsoles.me/cograph/reference/centrality_decay.md)
(equivalent formulation).

## Examples

``` r
adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_generalized_closeness(adj)
#>    A    B    C 
#> 1.75 2.00 1.75 
```
