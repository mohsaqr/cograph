# Local Bridging Centrality

Local bridging centrality multiplies the bridging coefficient of a node
by its inverse degree: \$\$LB(v) = \frac{1}{k_v} \cdot
\frac{1/k_v}{\sum\_{u \in N(v)} 1/k_u}.\$\$ A node of low degree whose
neighbors have high degree scores high.

## Usage

``` r
centrality_local_bridging(x, ...)
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

Edge weights are ignored. On a directed network \\k\\ is the total
degree, in plus out, and a reciprocated tie counts twice. An isolated
node scores 0. The local bridging centrality of Nanda and Kotz, the
product of ego betweenness and the bridging coefficient, is
[`centrality_localized_bridging`](https://sonsoles.me/cograph/reference/centrality_localized_bridging.md).

## See also

[`centrality_bridging`](https://sonsoles.me/cograph/reference/centrality_bridging.md),
[`centrality_localized_bridging`](https://sonsoles.me/cograph/reference/centrality_localized_bridging.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_local_bridging(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.02751572 0.01631100 0.01133420 0.02506266 0.02506266 0.05090909 0.10396040 
#>   Evaluate     Create      Share 
#> 0.05376000 0.01812141 0.03131991 
```
