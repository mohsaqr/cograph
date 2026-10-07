# Domain Prestige

Domain prestige (Wasserman and Faust 1994) counts the other nodes that
reach a node through a directed path: \$\$D(v) = \|\\u \ne v : u
\to^{\*} v\\\|.\$\$

## Usage

``` r
centrality_prestige_domain(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure needs a directed network. On undirected input every score is
`NA` with a `cograph_undefined_measure` warning. Edge weights are
ignored. The score is a whole number between 0 and \\n - 1\\. The values
equal `sna::prestige(cmode = "domain")`.

## References

Wasserman, S., & Faust, K. (1994). *Social Network Analysis: Methods and
Applications*. Cambridge University Press.

## See also

[`centrality_prestige_domain_proximity`](https://sonsoles.me/cograph/reference/centrality_prestige_domain_proximity.md),
[`centrality_reaching_local`](https://sonsoles.me/cograph/reference/centrality_reaching_local.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_prestige_domain(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          9          9          9          9          9          9          9 
#>   Evaluate     Create      Share 
#>          9          9          9 
```
