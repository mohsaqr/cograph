# Within-Module Degree Z-Score

The within-module degree z-score (Guimera and Nunes Amaral 2005)
standardizes the number of ties a node has inside its own community
against the other members of that community: \$\$z_i = \frac{\kappa_i -
\bar{\kappa}\_{s_i}}{\sigma\_{\kappa\_{s_i}}},\$\$ where \\\kappa_i\\
counts the ties of \\i\\ to its community \\s_i\\. High values mark hubs
within their community.

## Usage

``` r
centrality_within_module_z(x, membership = NULL, mode = "all", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- membership:

  Community of each node, a vector with one entry per node in input node
  order (default `NULL`).

- mode:

  Direction for directed networks: `"all"` (default), `"out"` or `"in"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are ignored. `mode` selects the ties counted, and with
`mode = "all"` on a directed network a reciprocated tie counts twice.
The standard deviation is the sample value. A community with one member,
or whose members all have the same within-community degree, gives `NaN`.
Without `membership` every score is `NA` with a warning that carries no
condition class, and a `membership` of the wrong length raises an error.
On undirected networks the values equal
[`brainGraph::within_module_deg_z_score()`](https://rdrr.io/pkg/brainGraph/man/vertex_roles.html).

## References

Guimera, R., & Nunes Amaral, L. A. (2005). Functional cartography of
complex metabolic networks. Nature, 433(7028), 895-900.
[doi:10.1038/nature03288](https://doi.org/10.1038/nature03288) .

## See also

[`centrality_participation`](https://sonsoles.me/cograph/reference/centrality_participation.md),
[`centrality_gateway`](https://sonsoles.me/cograph/reference/centrality_gateway.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_within_module_z(regulation_net, membership = rep(1:2, each = 5))
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.6708204 -1.5652476  0.6708204 -0.4472136  0.6708204 -0.1825742 -1.0954451 
#>   Evaluate     Create      Share 
#> -0.1825742  1.6431677 -0.1825742 
```
