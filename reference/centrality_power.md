# Bonacich Power Centrality

Bonacich (1987) power centrality with exponent \\\beta = 1\\ sums the
walks leaving a node, each further step weighted by \\\beta\\: \$\$c =
\gamma\\(I - A)^{-1} A \mathbf{1},\$\$ where \\A\\ is the binary
adjacency matrix and \\\gamma\\ scales the scores so that their squares
sum to \\n\\.

## Usage

``` r
centrality_power(x, mode = "all", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  For directed networks: `"all"` (default), `"out"` or `"in"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The exponent is fixed at 1. Edge weights and self-loops are ignored. On
a directed network `mode = "out"` sums over out-ties and equals
`igraph::power_centrality(exponent = 1)`, `mode = "in"` sums over
in-ties, and `mode = "all"` (default) uses the undirected skeleton, the
binary matrix with a tie wherever either direction has one. On an
undirected network the three modes agree. Where \\I - A\\ is singular,
as on a network that contains an isolated edge, a
`cograph_singular_system` error is raised, and a network without edges
gives `NaN`. The scores can be negative, as on `regulation_net`, and
`normalized = TRUE` leaves scores that are all negative unchanged.

## References

Bonacich, P. (1987). Power and centrality: A family of measures.
American Journal of Sociology, 92(5), 1170-1182.
[doi:10.1086/228631](https://doi.org/10.1086/228631) .

## See also

[`centrality_eigenvector`](https://sonsoles.me/cograph/reference/centrality_eigenvector.md),
[`centrality_katz`](https://sonsoles.me/cograph/reference/centrality_katz.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_power(regulation_net)
#>       Explore          Plan       Monitor         Adapt       Reflect 
#> -1.217161e+00 -6.756602e-17 -6.085806e-01 -1.217161e+00 -1.825742e+00 
#>       Discuss    Synthesize      Evaluate        Create         Share 
#> -1.217161e+00 -1.217161e+00 -6.085806e-01 -7.413494e-16 -5.733728e-16 
```
