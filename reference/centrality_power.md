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

  Accepted for a uniform interface; it has no effect on this measure
  (default `"all"`).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The exponent is fixed at 1, and the values equal
`igraph::power_centrality(exponent = 1)`. Edge weights and self-loops
are ignored. On a directed network the score sums over out-ties, and the
argument `mode` has no effect. Where \\I - A\\ is singular, as on a
network that contains an isolated edge, a `cograph_singular_system`
error is raised, and a network without edges gives `NaN`. The scores can
be negative, as on `regulation_net`, and `normalized = TRUE` leaves
scores that are all negative unchanged.

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
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> -0.7865422 -1.5805753 -0.8165057 -0.8239966 -0.4494527 -0.6517064 -1.1161409 
#>   Evaluate     Create      Share 
#> -0.3595622 -1.1461044 -1.4906848 
```
