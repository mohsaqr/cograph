# Eccentricity

The eccentricity of a node (Hage and Harary 1995) is its largest
shortest-path distance to a node it reaches: \$\$e(v) = \max\_{w:\\ d(v,
w) \< \infty} d(v, w).\$\$ Lower values mark more central nodes.

## Usage

``` r
centrality_eccentricity(x, mode = "all", ...)

centrality_ineccentricity(x, ...)

centrality_outeccentricity(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  For directed networks: `"all"` (default), `"out"` or `"in"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `directed` and `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are read as path lengths. The raw weights are always used,
so `weighted = FALSE` and `invert_weights` have no effect. On an
unweighted input the distances are hop counts. `mode = "out"` follows
paths leaving the node and `mode = "in"` paths arriving at it.
`centrality_outeccentricity()` and `centrality_ineccentricity()` are
these two forms. A node that reaches no other node scores 0.

## References

Hage, P., & Harary, F. (1995). Eccentricity and centrality in networks.
Social Networks, 17(1), 57-63.
[doi:10.1016/0378-8733(94)00248-9](https://doi.org/10.1016/0378-8733%2894%2900248-9)
.

## See also

[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md),
[`centrality_radiality`](https://sonsoles.me/cograph/reference/centrality_radiality.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_eccentricity(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>       0.33       0.35       0.34       0.39       0.33       0.40       0.38 
#>   Evaluate     Create      Share 
#>       0.40       0.33       0.39 
```
