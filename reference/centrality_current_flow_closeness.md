# Current Flow Closeness Centrality

Information centrality based on electrical current flow through the
network. Uses the pseudoinverse of the Laplacian matrix. Requires a
connected graph.

## Usage

``` r
centrality_current_flow_closeness(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
  (e.g., `weighted`, `directed`).

## Value

Named numeric vector of current flow closeness values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md)
for the shortest-path variant.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_current_flow_closeness(adj)
#>   A   B   C 
#> 1.5 1.5 1.5 
```
