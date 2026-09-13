# Exogenous centrality

Measures a node's contribution to the base centrality of all other
nodes, following Everett and Borgatti (2010): \\E(i)=\sum\_{j\ne
i}\[C_G(j)-C\_{G-i}(j)\]\\. The focal node's own base score is excluded.
Three base measures are supported, each calculated without normalization
before deletion:

- reverse_closeness:

  Default. For a graph H with m remaining nodes, \\C_H(j)=\sum\_{k\ne
  j}\max(N-d_H(j,k),0)\\, where N is the ORIGINAL input size, including
  isolates. Unreachable pairs contribute zero. This implements the
  fixed-size adjustment in section 3.3; it is distinct from ordinary
  reciprocal-farness closeness.

- betweenness:

  Raw shortest-path betweenness with endpoints excluded. Each unordered
  pair counts once on undirected graphs; directed pairs count
  separately. Exogenous contributions can be negative when removing a
  node increases the remaining nodes' betweenness.

- degree:

  Simple degree in the chosen base direction. On an undirected graph the
  exogenous result equals degree. On a directed graph, outgoing base
  degree produces incoming exogenous degree, and incoming base degree
  produces outgoing exogenous degree.

## Usage

``` r
centrality_exogenous(
  x,
  mode = "all",
  exogenous_base = "reverse_closeness",
  ...
)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  Direction of the base measure: all, out or in. Default all.

- exogenous_base:

  One of `"reverse_closeness"` (default), `"betweenness"` or `"degree"`.
  Exact names are required.

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector in input node order.

## Details

Uses the simple binary topology: parallel connections count once,
self-loops are removed and weights/path inversion are ignored. Mode
`"all"` projects onto the undirected skeleton; `"out"` and `"in"` use
directed paths when the input is directed. For undirected input all
three modes agree. Empty input returns an empty vector; isolates and
singletons score zero. Original size includes other components, so
adding an isolate can change reverse-closeness scores of connected nodes
even though the isolate's own contribution is zero.

`normalized = TRUE` applies cograph's final division by a positive
maximum, retaining negative values; it does not normalize the base
measure, nor apply the paper's theoretical normalization. If the maximum
is nonpositive, values remain unchanged. Arbitrary normalized base
scores, such as unit-length eigenvectors, are not supported.

Numerical verification uses independent NetworkX base scores, explicit
path enumeration and analytical graphs. Some numerical entries in the
paper's Florentine tables could not be reproduced from NetworkX's graph
plus the Pucci isolate; this implementation follows the stated
definition and does not claim complete published-table or UCINET parity.

Betweenness and reverse-closeness require repeated all-pairs distances,
with worst-case O(N^4) time using the current dense kernels. The measure
is therefore in the costly tier even when the degree base is selected.

## References

Everett, M. G., & Borgatti, S. P. (2010). Induced, endogenous and
exogenous centrality. Social Networks, 32(4), 339-344. Equations 3 and
8, sections 3.1-3.3.
[doi:10.1016/j.socnet.2010.06.004](https://doi.org/10.1016/j.socnet.2010.06.004)
.

## Examples

``` r
centrality_exogenous(igraph::make_ring(4), exogenous_base = "betweenness")
#>   1   2   3   4 
#> 0.5 0.5 0.5 0.5 
```
