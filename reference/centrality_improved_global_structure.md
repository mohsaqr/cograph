# Improved Global Structure Model Centrality

The improved global structure model (IGSM; Zhu and Wang 2022) replaces
the core numbers of
[`centrality_global_structure`](https://sonsoles.me/cograph/reference/centrality_global_structure.md)
with degrees and raises each distance to an exponent set by the mean
degree \\\bar{k}\\: \$\$IGSM(i) = \exp\left(\frac{k_i}{N}\right)
\sum\_{j \ne i} \frac{k_j}{d\_{ij}^{a}}, \qquad a = \lceil \log_2
\bar{k} \rceil.\$\$ The formula follows equation 5 of Mukhtar et al.
(2023).

## Usage

``` r
centrality_improved_global_structure(x, ...)
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
reachable nodes contribute to the sum, and \\N\\ and the mean degree
include every node of the network. When the mean degree is at most 1 the
exponent is zero or negative, and with a negative exponent distant nodes
contribute more than near ones. An isolated node scores 0, and so does
every node of a network without edges.

## References

Zhu, J.-C., & Wang, L.-W. (2022). An extended improved global structure
model for influential node identification in complex networks. Chinese
Physics B, 31, 068904.
[doi:10.1088/1674-1056/ac380d](https://doi.org/10.1088/1674-1056/ac380d)
.

Mukhtar, M. F., et al. (2023). Integrating local and global information
to identify influential nodes in complex networks. Scientific Reports,
13, 11411.
[doi:10.1038/s41598-023-37570-7](https://doi.org/10.1038/s41598-023-37570-7)
.

## See also

[`centrality_global_structure`](https://sonsoles.me/cograph/reference/centrality_global_structure.md),
[`centrality_hybrid_global_structure`](https://sonsoles.me/cograph/reference/centrality_hybrid_global_structure.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_improved_global_structure(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   49.04946   61.95204   77.02604   60.35769   47.60683   50.49209   40.65222 
#>   Evaluate     Create      Share 
#>   53.37735   63.54639   53.37735 
```
