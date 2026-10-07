# Participation Coefficient

The participation coefficient (Guimera and Nunes Amaral 2005) measures
how evenly the ties of a node spread over communities: \$\$P_i = 1 -
\sum\_{s} \left(\frac{k\_{is}}{k_i}\right)^2,\$\$ where \\k\_{is}\\
counts the ties of node \\i\\ to community \\s\\ and \\k_i\\ is its
degree.

## Usage

``` r
centrality_participation(x, membership = NULL, mode = "all", ...)
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
`mode = "all"` on a directed network a reciprocated tie counts twice. A
node whose ties all stay in one community scores 0, as does an isolated
node, and the score is below 1. Without `membership` every score is `NA`
with a warning that carries no condition class, and a `membership` of
the wrong length raises an error. On undirected networks the values
equal
[`brainGraph::part_coeff()`](https://rdrr.io/pkg/brainGraph/man/vertex_roles.html).

## References

Guimera, R., & Nunes Amaral, L. A. (2005). Functional cartography of
complex metabolic networks. Nature, 433(7028), 895-900.
[doi:10.1038/nature03288](https://doi.org/10.1038/nature03288) .

## See also

[`centrality_within_module_z`](https://sonsoles.me/cograph/reference/centrality_within_module_z.md),
[`centrality_gateway`](https://sonsoles.me/cograph/reference/centrality_gateway.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_participation(regulation_net, membership = rep(1:2, each = 5))
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.5000000  0.2448980  0.4687500  0.4444444  0.5000000  0.3200000  0.0000000 
#>   Evaluate     Create      Share 
#>  0.3200000  0.4897959  0.2777778 
```
