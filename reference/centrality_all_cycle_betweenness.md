# All Cycle Betweenness

All cycle betweenness (Zhou et al. 2018) counts the simple cycles that
pass through a node, a longer cycle counting less: \$\$C_A(i) =
\sum\_{k=3}^{n} \alpha^{k-2} s_k(i),\$\$ where \\s_k(i)\\ is the number
of simple cycles of \\k\\ edges through \\i\\.

## Usage

``` r
centrality_all_cycle_betweenness(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `cycle_alpha` (the discount per edge, default 0.5)
  and `enumeration_limit`.

## Value

A named numeric vector with one score per node, in input node order, or
`NA` at every node when the search was cut short.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored. A
cycle is counted once, whatever its starting node and direction. A node
on no cycle scores 0. `cycle_alpha` is 0.5 by default, the value of the
worked example of the source; its experiments use 0.1, and it states no
single default. A `cycle_alpha` outside \\(0,1)\\ raises a
`cograph_bad_parameter` error.

The number of simple cycles grows exponentially with the size and
density of the network. The measure is therefore costly, computed only
when requested by name or through `include`, and it stops after
`enumeration_limit` search steps (default one million): every score is
then `NA` with a `cograph_undefined_measure` warning. The count is
exact, where the source estimates it on large networks.

## References

Zhou, X., Liang, X., Zhao, J. and Zhang, S. (2018). Cycle based network
centrality. Scientific Reports, 8, 11749.
[doi:10.1038/s41598-018-30249-4](https://doi.org/10.1038/s41598-018-30249-4)
.

## See also

[`centrality_transportation`](https://sonsoles.me/cograph/reference/centrality_transportation.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
ring <- matrix(0, 5, 5, dimnames = list(letters[1:5], letters[1:5]))
ring[cbind(1:5, c(2:5, 1))] <- 1
ring <- ring + t(ring)
centrality_all_cycle_betweenness(ring)
#>     a     b     c     d     e 
#> 0.125 0.125 0.125 0.125 0.125 
```
