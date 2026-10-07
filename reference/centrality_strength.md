# Strength Centrality

Strength (Barrat et al. 2004) is the sum of the weights of the edges
incident to a node, the weighted counterpart of degree. With
`mode = "in"` it sums incoming weights and with `mode = "out"` outgoing
weights. `centrality_instrength()` and `centrality_outstrength()` are
these two forms.

## Usage

``` r
centrality_strength(x, mode = "all", ...)

centrality_instrength(x, ...)

centrality_outstrength(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  Direction for directed networks: `"all"` (default), `"in"` or `"out"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `loops` (keep self-loops, default `TRUE`) and
  `normalized` (default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The stored weights are always summed, and `weighted = FALSE` has no
effect;
[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md)
counts edges. A self-loop counts twice on an undirected network and
under `mode = "all"`, and once under `"in"` or `"out"`; `loops = FALSE`
drops it. Negative weights are summed with their sign.
`normalized = TRUE` divides the scores by their maximum.

## References

Barrat, A., Barthelemy, M., Pastor-Satorras, R., & Vespignani, A.
(2004). The architecture of complex weighted networks. Proceedings of
the National Academy of Sciences, 101(11), 3747-3752.
[doi:10.1073/pnas.0400087101](https://doi.org/10.1073/pnas.0400087101) .

## See also

[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_strength(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>       1.39       1.90       1.87       1.77       1.39       1.53       0.77 
#>   Evaluate     Create      Share 
#>       1.71       1.64       1.95 
```
