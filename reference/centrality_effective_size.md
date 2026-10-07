# Effective Size

Burt's effective size is the number of a node's contacts minus their
redundancy, the average number of ties each contact has to the other
contacts: \$\$ES(v) = k_v - \frac{1}{k_v} \sum\_{j \in N(v)} \|N(v) \cap
N(j)\|.\$\$ On an undirected network this is \\k_v - 2 t_v / k_v\\, with
\\t_v\\ the number of ties among the contacts.

## Usage

``` r
centrality_effective_size(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `directed` and `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are ignored. On a directed network each reciprocated
neighbor enters the neighbor list twice, so \\k_v\\ counts it twice and
the result differs from that of the undirected skeleton. An isolated
node scores 0.

## See also

[`centrality_constraint`](https://sonsoles.me/cograph/reference/centrality_constraint.md),
[`centrality_redundancy`](https://sonsoles.me/cograph/reference/centrality_heatmap.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_effective_size(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   4.166667   4.714286   4.875000   4.333333   4.833333   3.400000   2.500000 
#>   Evaluate     Create      Share 
#>   3.000000   4.285714   3.666667 
```
