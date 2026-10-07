# Pairwise Disconnectivity

Pairwise disconnectivity (Potapov et al. 2008) is the share of ordered
reachable pairs that become unreachable when a node is removed:
\$\$PD(v) = \frac{\|P(G)\| - \|P(G - v)\|}{\|P(G)\|},\$\$ where
\\\|P(G)\|\\ is the number of ordered pairs \\(s, t)\\, \\s \ne t\\,
with a directed path from \\s\\ to \\t\\.

## Usage

``` r
centrality_pairwisedis(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure needs a directed network. On undirected input every score is
`NA` with a `cograph_undefined_measure` warning. Reachability uses hop
counts, so edge weights are ignored. The score lies between 0 and 1, and
a network without reachable pairs scores 0 everywhere. The values equal
[`centiserve::pairwisedis()`](https://rdrr.io/pkg/centiserve/man/pairwisedis.html).

## References

Potapov, A. P., Goemann, B., & Wingender, E. (2008). The pairwise
disconnectivity index as a new metric for the topological analysis of
regulatory networks. *BMC Bioinformatics*, 9, 227.
[doi:10.1186/1471-2105-9-227](https://doi.org/10.1186/1471-2105-9-227) .

## See also

[`centrality_prestige_domain`](https://sonsoles.me/cograph/reference/centrality_prestige_domain.md),
[`robustness`](https://sonsoles.me/cograph/reference/robustness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_pairwisedis(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.2000000  0.2000000  0.2000000  0.2888889  0.2000000  0.2000000  0.2000000 
#>   Evaluate     Create      Share 
#>  0.2000000  0.2000000  0.2000000 
```
