# Global Structure Model Centrality

The global structure model (GSM; Ullah et al. 2021) multiplies a
coreness factor of the node by the coreness of all other nodes, each
discounted by its hop distance: \$\$GSM(i) =
\exp\left(\frac{k_s(i)}{N}\right) \sum\_{j \ne i}
\frac{k_s(j)}{d\_{ij}}.\$\$ Here \\k_s\\ is the core number and \\N\\
the number of nodes, isolated nodes included.

## Usage

``` r
centrality_global_structure(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure uses the simple undirected skeleton, so direction, weights,
loops and parallel edges are ignored, and `mode` has no effect. Only
reachable nodes contribute to the sum, which extends the measure to
disconnected networks. Other components still affect the scores through
\\N\\. An isolated node scores 0.

## References

Ullah, A., Wang, B., Sheng, J., Long, J., Khan, N., & Sun, Z. (2021).
Identification of nodes influence based on global structure model in
complex networks. Scientific Reports, 11, 6173.
[doi:10.1038/s41598-021-84684-x](https://doi.org/10.1038/s41598-021-84684-x)
.

## See also

[`centrality_hybrid_global_structure`](https://sonsoles.me/cograph/reference/centrality_hybrid_global_structure.md),
[`centrality_improved_global_structure`](https://sonsoles.me/cograph/reference/centrality_improved_global_structure.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_global_structure(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   41.77109   44.75474   47.73839   44.75474   41.77109   41.77109   38.78744 
#>   Evaluate     Create      Share 
#>   41.77109   44.75474   41.77109 
```
