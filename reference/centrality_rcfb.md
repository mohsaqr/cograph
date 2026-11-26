# Resized Current-Flow Betweenness

RCFB (Agryzkov et al. 2019) joins every node to a virtual target by a
link of conductance \\\varepsilon\\, lets one unit of current enter at
each node \\s\\ in turn, and credits the current of every edge to its
end of higher potential: \$\$b_i = \frac{1}{n} \sum\_{s} \sum\_{j:\\
u^{(s)}\_i \> u^{(s)}\_j} q\_{ij} \bigl(u^{(s)}\_i - u^{(s)}\_j\bigr),
\qquad u^{(s)} = (\varepsilon I + L)^{-1} e_s,\$\$ with \\q\_{ij}\\ the
conductance (weight) of the edge \\ij\\, \\L\\ the weighted Laplacian
and \\n\\ the number of nodes.

## Usage

``` r
centrality_rcfb(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `rcfb_epsilon` (default 0, the limit) and the edge
  weights (`weighted`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The source defines the measure on undirected networks with edge weights
as conductances; a weight is read as a conductance (for a resistance,
give its reciprocal) and loops are ignored. A network with an arc whose
reverse is missing or has another weight has no value: the measure warns
(class `cograph_undefined_measure`) and returns `NA`. The source bounds
\\\varepsilon\\ above by one over the sum of the edge resistances (its
equation 14) and gives no value. `rcfb_epsilon = 0`, the default, takes
the limit \\\varepsilon \to 0\\ (owner decision 2026-11-26), in which
the potential differences are those of the Laplacian pseudo-inverse; on
the Krackhardt kite \\\varepsilon = 10^{-6}\\ is within \\10^{-6}\\ of
it. A positive `rcfb_epsilon` at or above the bound raises a
`cograph_bad_parameter` error. The network need not be connected: the
current of a source stays in its component, and an isolated node scores
0.

The loop of the source's Algorithm 2 credits every edge, also those at
the source, so a source is credited with the current it sends out; that
is the second option of its equation (19), \\I_s = x_s - x_s/n\\, and it
is what is computed (a builder choice). The caption of Algorithm 2 names
the first option, \\I_s = 0\\; in the limit that reading subtracts
\\(1 - 1/n_c)/n\\ from every score, \\n_c\\ the size of the node's
component, so it ranks the nodes of one component in the same order.
Algorithm 2 also omits the conductance of equation (18), which matters
only on a weighted network; the current includes it here. The source
prints no score. The implementation reproduces the values a source audit
computed on the Krackhardt kite in the limit (10 of 10) and its
distances to the limit at \\\varepsilon = 10^{-4}\\ and \\10^{-6}\\; the
audit's ten values at \\\varepsilon = 1\\, which is above the bound, are
reproduced by the internal computation, which this function does not let
reach that value. On four test networks the score ranks nodes closest to
[`centrality_current_flow_betweenness`](https://sonsoles.me/cograph/reference/centrality_current_flow_betweenness.md)
(mean Kendall correlation 0.958, 0.931 at the least) and
[`centrality_godfather`](https://sonsoles.me/cograph/reference/centrality_truss.md)
(0.87); with
[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md)
the correlation is 0.83 to 0.87. `normalized = TRUE` divides the scores
by their maximum.

## References

Agryzkov, T., Tortosa, L. and Vicent, J. F. (2019). A variant of the
current flow betweenness centrality and its application in urban
networks. Applied Mathematics and Computation, 347, 600-615.
[doi:10.1016/j.amc.2018.11.032](https://doi.org/10.1016/j.amc.2018.11.032)
.

## See also

[`centrality_current_flow_betweenness`](https://sonsoles.me/cograph/reference/centrality_current_flow_betweenness.md),
[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_rcfb(to_undirected(regulation_net))
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.1318329  0.1491990  0.1432635  0.1681408  0.1516253  0.1464511  0.1026948 
#>   Evaluate     Create      Share 
#>  0.1376961  0.1216454  0.1430690 
centrality_rcfb(to_undirected(regulation_net), weighted = FALSE, rcfb_epsilon = 1e-4)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.1232380  0.1403001  0.1501619  0.1498074  0.1353150  0.1249504  0.1096086 
#>   Evaluate     Create      Share 
#>  0.1187143  0.1349811  0.1172644 
```
