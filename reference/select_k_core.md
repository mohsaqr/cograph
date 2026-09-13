# Select the k-Core of a Network

The k-core is the maximal subgraph in which every node has degree at
least `k`, found by repeatedly removing nodes of degree below `k`.

## Usage

``` r
select_k_core(x, k, keep_format = FALSE, directed = NULL)
```

## Arguments

- x:

  Network input.

- k:

  Integer. The core number.

- keep_format:

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A `cograph_network` holding the k-core, or the input format when
`keep_format = TRUE`. An empty network when no node reaches coreness
`k`.

## References

Seidman, S. B. (1983). Network structure and minimum degree. *Social
Networks*, 5(3), 269–287.

## See also

[`select_nodes`](https://sonsoles.me/cograph/reference/select_nodes.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md)

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1,
                1, 0, 1, 0,
                1, 1, 0, 0,
                1, 0, 0, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

select_k_core(adj, k = 2)
#> Cograph network: 3 nodes, 3 edges ( undirected )
#> Source: matrix 
#>   Nodes (3): A, B, C
#>   Edges: 3 / 3 (density: 100.0%)
#>   Weights: [1.000, 1.000]  |  mean: 1.000
#>   Strongest edges:
#>     A -- B  1.000
#>     A -- C  1.000
#>     B -- C  1.000
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
