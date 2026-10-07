# Select the k-Core of a Network

The k-core is the maximal subgraph in which every node has degree at
least `k`, found by repeatedly removing nodes of degree below `k`.
Degree is the total degree, which is in-degree plus out-degree in a
directed network. A self-loop adds 2 to the degree of its node.

## Usage

``` r
select_k_core(x, k, keep_format = FALSE, directed = NULL)
```

## Arguments

- x:

  Network input.

- k:

  A single non-negative whole number. The core number.

- keep_format:

  Logical. If TRUE, a matrix, igraph, statnet network or tna input is
  returned in its own format. An edge-list data frame or a qgraph object
  is returned as a `cograph_network` with a
  `cograph_no_format_roundtrip` warning. Default FALSE returns a
  `cograph_network`.

- directed:

  Logical or NULL. Directedness used to read the input. NULL (default)
  detects it from the input.

## Value

A `cograph_network` holding the k-core, or the input format when
`keep_format = TRUE`. When no node reaches coreness `k`, the result is
an empty network and a warning is raised.

## References

Seidman, S. B. (1983). Network structure and minimum degree. *Social
Networks*, 5(3), 269–287.

## See also

[`select_nodes`](https://sonsoles.me/cograph/reference/select_nodes.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md)

## Examples

``` r
select_k_core(regulation_net, k = 2)
#> Cograph network: 10 nodes, 30 edges ( directed )
#> Source: matrix 
#>   Nodes (10): Explore, Plan, Monitor, Adapt, Reflect, Discuss, ... +4 more
#>   Edges: 30 / 90 (density: 33.3%)
#>   Weights: [0.050, 0.490]  |  mean: 0.265
#>   Strongest edges:
#>     Share -> Monitor  0.490
#>     Plan -> Evaluate  0.490
#>     Evaluate -> Adapt  0.430
#>     Synthesize -> Reflect  0.420
#>     Plan -> Discuss  0.400
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
