# HITS Authority and Hub Scores

The HITS algorithm (Kleinberg 1999) assigns each node an authority score
from the hubs that point to it and a hub score from the authorities it
points to: \$\$a = \lambda^{-1} A^{T} h, \qquad h = \lambda^{-1} A a
.\$\$ Authorities are the dominant eigenvector of \\A^{T} A\\ and hubs
the dominant eigenvector of \\A A^{T}\\.

## Usage

``` r
centrality_authority(x, ...)

centrality_hub(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `directed`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

\\A\\ holds the edge weights. Edge weights are always used, and
`weighted = FALSE` has no effect. Both scores are scaled to a maximum of
one, so they lie between 0 and 1. On an undirected network both equal
eigenvector centrality. A network without edges gives every node a score
of one. `centrality_hub()` returns the hub scores.

## References

Kleinberg, J. M. (1999). Authoritative sources in a hyperlinked
environment. Journal of the ACM, 46(5), 604-632.
[doi:10.1145/324133.324140](https://doi.org/10.1145/324133.324140) .

## See also

[`centrality_eigenvector`](https://sonsoles.me/cograph/reference/centrality_eigenvector.md),
[`centrality_pagerank`](https://sonsoles.me/cograph/reference/centrality_pagerank.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_authority(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.30736489 0.21141184 1.00000000 0.67930404 0.40566475 0.62132791 0.05601863 
#>   Evaluate     Create      Share 
#> 0.93786061 0.40032707 0.74285635 
centrality_hub(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.2889344  1.0000000  0.2166116  0.2588081  0.1394834  0.2448074  0.2223682 
#>   Evaluate     Create      Share 
#>  0.5486759  0.6323115  0.6742079 
```
