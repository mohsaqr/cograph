# Exogenous Centrality

Exogenous centrality (Everett and Borgatti 2010) is the total change in
a base centrality of the other nodes when the node is deleted: \$\$E(i)
= \sum\_{j \ne i} \left\[C_G(j) - C\_{G-i}(j)\right\].\$\$ The default
base is reverse closeness, \\C_H(j) = \sum\_{k \ne j} \max(N - d_H(j,k),
0)\\, with \\N\\ the number of nodes of the original network. The other
bases are raw betweenness and degree.

## Usage

``` r
centrality_exogenous(
  x,
  mode = "all",
  exogenous_base = "reverse_closeness",
  ...
)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  Direction of the base measure: `"all"` (default), `"out"` or `"in"`.

- exogenous_base:

  Base centrality: `"reverse_closeness"` (default), `"betweenness"` or
  `"degree"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure uses the simple binary network, so weights, loops and
parallel edges are ignored. `mode = "all"` uses the undirected skeleton,
and `"out"` and `"in"` use directed paths and degrees. On an undirected
network the three modes agree and the degree base returns the degree. On
a directed network the out-degree base returns the in-degree and the
in-degree base the out-degree. Exogenous betweenness can be negative
when a deletion raises the betweenness of the remaining nodes. An
isolated node scores 0. `normalized = TRUE` divides by the largest
score.

## References

Everett, M. G., & Borgatti, S. P. (2010). Induced, endogenous and
exogenous centrality. Social Networks, 32(4), 339-344.
[doi:10.1016/j.socnet.2010.06.004](https://doi.org/10.1016/j.socnet.2010.06.004)
.

## See also

[`centrality_closeness_vitality`](https://sonsoles.me/cograph/reference/centrality_closeness_vitality.md),
[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_exogenous(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>         77         78         79         78         77         77         76 
#>   Evaluate     Create      Share 
#>         77         78         77 
```
