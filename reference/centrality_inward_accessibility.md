# Inward Accessibility

Inward accessibility (Travençolo and Costa 2008) measures how evenly a
node is reached by self-avoiding random walks of \\h\\ steps started at
the other nodes: \$\$IA_h(i) = \frac{1}{N - 1} \exp\Bigl(-\sum\_{j \ne
i} q\_{ji} \log q\_{ji}\Bigr), \qquad q\_{ji} = \frac{P_h(j \to i)}{N -
1},\$\$ with \\P_h(j \to i)\\ the probability that a walk started at
\\j\\ is at \\i\\ after \\h\\ steps and \\N\\ the number of nodes.

## Usage

``` r
centrality_inward_accessibility(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `ia_steps` (the number of steps, required),
  `ia_stuck` (`\"vanish\"` or `\"stay\"`) and `enumeration_limit`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored. A
self-avoiding walk steps to a neighbor it has not visited, each equally
likely, and stops when there is none. `ia_steps`, the number of steps
\\h\\, has no default, because the source gives none: without it the
measure warns (class `cograph_undefined_measure`) and returns `NA`. The
source leaves the treatment of a stopped walk to the application.
`ia_stuck = "vanish"`, the default, removes it from the analysis, as the
source's own application does; `"stay"` keeps it at its last node. The
probabilities are computed exactly by enumerating every walk, the limit
of the sampled walks of the source. Their number grows exponentially
with \\h\\; past `enumeration_limit` walk steps the measure warns and
returns `NA`. The logarithm is natural. A node that no walk reaches
scores \\1/(N - 1)\\, the floor of the measure, and 1 is the largest
score. On a one-node network the measure warns and returns `NA`. The
source defines the measure but computes no value of it; its Fig. 2
prints 18 diversity entropies of the same walks, which the walk
enumeration reproduces. With `ia_steps = 2` the score ranks nodes
closest to `nwrank` (mean Kendall correlation 0.89 on four test
networks) and `density_centrality` (0.86). A `ia_steps` that is not a
whole number of at least one, or a `ia_stuck` other than `"vanish"` and
`"stay"`, raises a `cograph_bad_parameter` error. `normalized = TRUE`
divides the scores by their maximum.

## References

Travençolo, B. A. N. and da F. Costa, L. (2008). Accessibility in
complex networks. Physics Letters A, 373(1), 89-95.
[doi:10.1016/j.physleta.2008.10.069](https://doi.org/10.1016/j.physleta.2008.10.069)
.

## See also

[`centrality_access_information`](https://sonsoles.me/cograph/reference/centrality_access_information.md),
[`centrality_random_walk`](https://sonsoles.me/cograph/reference/centrality_random_walk.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_inward_accessibility(regulation_net, ia_steps = 2)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.1745627  0.1832790  0.2046922  0.1822050  0.1691508  0.1753219  0.1612805 
#>   Evaluate     Create      Share 
#>  0.1752906  0.1872859  0.1763371 
centrality_inward_accessibility(regulation_net, ia_steps = 2, ia_stuck = "stay")
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.1745627  0.1832790  0.2046922  0.1822050  0.1691508  0.1753219  0.1612805 
#>   Evaluate     Create      Share 
#>  0.1752906  0.1872859  0.1763371 
```
