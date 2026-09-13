# Extended local bridging centrality

Macker's two-hop localized bridging centrality multiplies betweenness of
the focal node in its induced closed two-hop neighborhood by its
bridging coefficient. Degrees for that coefficient come from the
original graph. The ego network includes every edge between the selected
vertices. Its shortest paths can be up to four edges long; this is not
global betweenness with a path-length cutoff of two. Betweenness uses
unordered pairs, excludes endpoints, and is not normalized by
ego-network size.

## Usage

``` r
centrality_extended_local_bridging(x, ...)
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

Uses the same simple undirected unweighted projection and zero
conventions as
[`centrality_localized_bridging`](https://sonsoles.me/cograph/reference/centrality_localized_bridging.md).
Macker's separate weighted model uses link quality for degree and costs
for paths; that model is outside this implementation. Native
breadth-first path counts cost O(sum over ego networks of n_ego times
(n_ego + m_ego)), at worst O(n to the fourth power), with O(n squared)
memory. This measure is marked costly and must be selected explicitly or
through `include`.

## References

Macker, J. P. (2016). An improved local bridging centrality model for
distributed network analytics. MILCOM, pp. 600-605, sections IV-V,
equation 5 and Table I.
[doi:10.1109/MILCOM.2016.7795393](https://doi.org/10.1109/MILCOM.2016.7795393)
.

## Examples

``` r
centrality_extended_local_bridging(igraph::make_graph("Zachary"))
#>          1          2          3          4          5          6          7 
#>  2.0189267  0.9933652  3.3244479  0.6045079  0.1720430  1.7090909  1.7090909 
#>          8          9         10         11         12         13         14 
#>  0.0000000  9.5411745  1.4091711  0.1720430  0.0000000  0.0000000  9.1625292 
#>         15         16         17         18         19         20         21 
#>  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000 22.6422477  0.0000000 
#>         22         23         24         25         26         27         28 
#>  0.0000000  0.0000000  2.0092462  0.5185185  0.6507937  0.0000000  3.8618643 
#>         29         30         31         32         33         34 
#>  0.9704532  0.4382172  4.2075394  9.3565470  1.1317356  1.1801784 
```
