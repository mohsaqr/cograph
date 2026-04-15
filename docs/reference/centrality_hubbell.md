# Hubbell Centrality

Hubbell (1965) input-output centrality: \\C = (I - w W)^{-1}
\mathbf{1}\\, where \\W\\ is the (weighted) adjacency matrix and \\w\\
is a weight factor that must satisfy \\w \cdot \rho(W) \< 1\\ for the
system to be solvable.

## Usage

``` r
centrality_hubbell(x, hubbell_weight = 0.5, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- hubbell_weight:

  Attenuation factor \\w\\. Default 0.5. If \\w \cdot \rho(W) \ge 1\\,
  the function returns `NA` with a warning.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector of Hubbell centrality values (or `NA` if the system
is not solvable).

## Details

Bit-exact match against
[`centiserve::hubbell`](https://rdrr.io/pkg/centiserve/man/hubbell.html)
when edge weights are passed explicitly (cograph mirrors centiserve's
full-inverse LAPACK call path).

## Note on centiserve equivalence

`centiserve::hubbell(g, weights = NULL)` silently resets all edge
weights to 1, ignoring the graph's weight attribute. To reproduce
cograph's values with centiserve on a weighted graph, pass
`weights = igraph::E(g)$weight` explicitly.

## References

Hubbell, C. H. (1965). An input-output approach to clique
identification. *Sociometry*, 28(4), 377-399.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
[`centrality_katz`](https://sonsoles.me/cograph/reference/centrality_katz.md).

## Examples

``` r
# Small weighted path graph; spectral radius permits weightfactor = 0.5
adj <- matrix(0, 4, 4)
adj[1,2] <- adj[2,1] <- adj[2,3] <- adj[3,2] <- adj[3,4] <- adj[4,3] <- 0.3
rownames(adj) <- colnames(adj) <- LETTERS[1:4]
centrality_hubbell(adj, hubbell_weight = 0.5)
#>        A        B        C        D 
#> 1.208459 1.389728 1.389728 1.208459 
```
