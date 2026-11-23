# Effective Size

Burt's (1992) effective size is the number of a node's contacts minus
their redundancy: \$\$ES(v) = \sum\_{j \in N(v)} \left(1 - \sum\_{q}
p\_{vq} m\_{jq}\right),\$\$ where \\p\_{vq}\\ is v's share of its ties
going to q and \\m\_{jq}\\ j's tie to q over j's strongest tie, every
tie weighed by its mutual weight \\a\_{vq} + a\_{qv}\\. On an undirected
network this is Borgatti's (1997) \\k_v - 2 t_v / k_v\\, with \\t_v\\
the number of ties among the contacts.

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

Edge weights are ignored (each tie is 0/1). On a directed network
\\N(v)\\ holds the in- and out-neighbors, each once, and a reciprocated
tie has mutual weight 2, as in `networkx::effective_size()`; cograph
2.7.19 and earlier applied Borgatti's form with a reciprocated neighbor
counted twice. An isolated node scores 0.

## See also

[`centrality_constraint`](https://sonsoles.me/cograph/reference/centrality_constraint.md),
[`centrality_redundancy`](https://sonsoles.me/cograph/reference/centrality_heatmap.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_effective_size(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   3.583333   4.428571   4.625000   4.916667   4.083333   3.900000   3.125000 
#>   Evaluate     Create      Share 
#>   3.700000   4.142857   3.500000 
```
