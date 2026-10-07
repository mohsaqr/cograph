# Length-Scaled, Delta and Ego Betweenness, and Delta Closeness

Length-scaled betweenness (Borgatti and Everett 2006; Brandes 2008)
weights each pair \\s,t\\ in the betweenness sum by \\1/d(s,t)\\. Delta
betweenness (Agneessens et al. 2017) uses the pair weight
\\(h(s,t)-1)^{-\delta}\\, where \\h(s,t)\\ is the number of edges on a
shortest path, so \\\delta = 0\\ gives ordinary betweenness. Ego
betweenness (Everett and Borgatti 2005) is the betweenness of a node
inside its own ego network. Delta closeness (Agneessens et al. 2017, eq.
2) is \$\$C\_\delta(i) = \frac{1}{n-1} \sum\_{j \ne i}
d\_{ij}^{-\delta}.\$\$

## Usage

``` r
centrality_length_scaled_betweenness(x, ...)

centrality_delta_betweenness(x, betweenness_delta = 1, ...)

centrality_ego_betweenness(x, ...)

centrality_delta_closeness(x, mode = "all", closeness_delta = 1, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The weighted measures use `weighted` (default `TRUE`),
  `invert_weights` (default `NULL`, which inverts for tna input only)
  and `alpha` (inversion exponent, default 1). Delta closeness also uses
  `cutoff` (default -1, no limit).

- betweenness_delta:

  Decay exponent \\\delta\\ of delta betweenness. Default 1.

- mode:

  Direction for delta closeness: `"all"` (default), `"out"` or `"in"`.

- closeness_delta:

  Distance exponent \\\delta\\ of delta closeness. Default 1.

## Value

A named numeric vector with one score per node, in input node order.

## Details

On a directed network the three betweenness measures count directed
paths. Length-scaled and delta betweenness and delta closeness read edge
weights as distances, and `invert_weights = TRUE` converts weights to
distances \\1/w^\alpha\\. In delta betweenness the weighted distances
decide which paths are shortest, and the pair weight \\(h -
1)^{-\delta}\\ uses the number of edges \\h\\ on a shortest path (the
fewest when several tie), so \\h - 1\\ is the number of intermediaries.
On a binary network \\h = d(s,t)\\. A pair joined by a one-edge shortest
path contributes nothing. `weighted = FALSE` uses hop counts. Ego
betweenness ignores weights, and a node with fewer than two neighbors
scores 0. Delta closeness follows `mode` and `cutoff`. On hop distances
\\\delta = 1\\ gives harmonic closeness divided by \\n-1\\ and \\\delta
= 0\\ gives the share of nodes reached. Bounded-distance betweenness,
listed in the Centrality Zoo as k-betweenness, is
`centrality_betweenness(x, cutoff = k)`.

## References

Borgatti, S. P., & Everett, M. G. (2006). A graph-theoretic perspective
on centrality. Social Networks, 28(4), 466-484.
[doi:10.1016/j.socnet.2005.11.005](https://doi.org/10.1016/j.socnet.2005.11.005)
.

Agneessens, F., Borgatti, S. P., & Everett, M. G. (2017). Geodesic based
centrality: Unifying the local and the global. Social Networks, 49,
12-26.

Brandes, U. (2008). On variants of shortest-path betweenness centrality
and their generic computation. Social Networks, 30(2), 136-145.

Everett, M., & Borgatti, S. P. (2005). Ego network betweenness. Social
Networks, 27(1), 31-38.

## See also

[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality_harmonic`](https://sonsoles.me/cograph/reference/centrality_harmonic.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_length_scaled_betweenness(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  11.060744  28.746387  38.946145  27.957440  28.003107   1.041667  15.379439 
#>   Evaluate     Create      Share 
#>   5.258329  27.863965  14.631523 
centrality_delta_betweenness(regulation_net, weighted = FALSE)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   4.883333   6.991667   8.458333  12.475000   3.000000   2.883333   3.533333 
#>   Evaluate     Create      Share 
#>   2.491667   7.116667   8.166667 
centrality_ego_betweenness(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>    8.00000    9.00000   11.83333   17.00000    4.00000    3.00000    7.00000 
#>   Evaluate     Create      Share 
#>    4.00000   13.16667   10.00000 
centrality_delta_closeness(regulation_net, closeness_delta = 2)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   67.51029   26.77763   49.56442   15.65708   82.11082   13.46055   44.62772 
#>   Evaluate     Create      Share 
#>   38.75376   27.73156   11.54804 
```
