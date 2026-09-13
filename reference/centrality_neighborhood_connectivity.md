# Neighborhood Connectivity

Mean degree of a node's neighbours (Maslov & Sneppen 2002), the "average
neighbour degree" reported by Cytoscape: \$\$C\_{NC}(i) = \frac{1}{k_i}
\sum\_{j \in N(i)} k_j.\$\$ High values mark nodes attached to hubs.
Isolates score 0. Under `mode = "out"` the out-neighbours' out-degrees
are averaged, under `"in"` the in-neighbours' in-degrees.

## Usage

``` r
centrality_neighborhood_connectivity(x, mode = "all", ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- mode:

  For directed networks: `"all"` (default), `"out"` (distances along
  out-edges), or `"in"`.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector, one value per node.

## References

Maslov, S., & Sneppen, K. (2002). Specificity and stability in topology
of protein networks. Science, 296(5569), 910-913.

## See also

[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
and [`igraph::knn()`](https://r.igraph.org/reference/knn.html) for the
Barrat weighted generalisation.

## Examples

``` r
star5 <- matrix(0, 5, 5)
star5[1, 2:5] <- 1; star5[2:5, 1] <- 1
rownames(star5) <- colnames(star5) <- LETTERS[1:5]
centrality_neighborhood_connectivity(star5)
#> A B C D E 
#> 1 4 4 4 4 
```
