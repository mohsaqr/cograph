# Localized bridging centrality from ego betweenness

Nanda and Kotz's localized bridging centrality is the product of a
node's unnormalized betweenness in its induced one-hop ego network and
its bridging coefficient. The coefficient is reciprocal focal degree
divided by the sum of reciprocal neighbor degrees, all measured in the
original graph. It is not computed from degrees truncated to the ego
network.

## Usage

``` r
centrality_localized_bridging(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  `normalized = TRUE` divides final scores by their maximum; all-zero
  scores remain zero. Ego betweenness is never scaled by ego size.

## Value

Named numeric vector in input node order.

## Details

Each unordered pair of other ego-network vertices contributes the
fraction of its shortest paths that pass through the focal vertex.
Endpoints are excluded. Uses a simple unweighted undirected skeleton:
either arc direction creates an edge, loops are removed and parallel
edges count once. Weights, mode, inversion and cutoff are ignored. This
projection is an explicit cograph convention, not a directed or weighted
generalization of LBC.

Isolates and leaves score zero; the isolate value extends the undefined
bridging coefficient by zero. Complete graphs score zero. Disconnected
components are evaluated independently before optional maximum scaling.
Empty graphs return no scores. The one-hop calculation uses the
Everett-Borgatti common-neighbor shortcut in each ego network, with
worst-case O(n to the fourth power) time and O(n squared) memory for
dense matrix multiplication across all nodes.

## References

Nanda, S. and Kotz, D. (2012). Localized Bridging Centrality. Handbook
of Optimization in Complex Networks, pp. 197-224, equations 7.7-7.8.
[doi:10.1007/978-1-4614-0857-4_7](https://doi.org/10.1007/978-1-4614-0857-4_7)
. This author chapter restates their 2008 LBC definition.

## See also

[`centrality_extended_local_bridging`](https://sonsoles.me/cograph/reference/centrality_extended_local_bridging.md)
for two-hop ego networks.
[`centrality_local_bridging`](https://sonsoles.me/cograph/reference/centrality_local_bridging.md)
retains the distinct legacy score, inverse degree times bridging
coefficient.

## Examples

``` r
centrality_localized_bridging(igraph::make_graph("Zachary"))
#>         1         2         3         4         5         6         7         8 
#> 1.0638369 0.7407407 1.4256278 0.3064699 0.2580645 0.4363636 0.4363636 0.0000000 
#>         9        10        11        12        13        14        15        16 
#> 1.2620415 3.1481481 0.2580645 0.0000000 0.0000000 1.6028810 0.0000000 0.0000000 
#>        17        18        19        20        21        22        23        24 
#> 0.0000000 0.0000000 0.0000000 2.8681898 0.0000000 0.0000000 0.0000000 1.0251256 
#>        25        26        27        28        29        30        31        32 
#> 0.8888889 0.9523810 0.0000000 1.8059490 2.0481928 0.2968568 1.6546503 1.5910478 
#>        33        34 
#> 0.6822368 0.9894594 
```
