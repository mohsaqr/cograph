# Degree Centrality

Degree centrality counts the edges incident to each node. With
`mode = "in"` it counts incoming edges and with `mode = "out"` outgoing
edges. On an undirected network the three modes agree.

## Usage

``` r
centrality_degree(x, mode = "all", ...)

centrality_indegree(x, ...)

centrality_outdegree(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  For directed networks: `"all"` (default), `"in"` or `"out"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are ignored;
[`centrality_strength`](https://sonsoles.me/cograph/reference/centrality_strength.md)
sums them instead. `normalized = TRUE` divides the scores by their
maximum. `centrality_indegree()` and `centrality_outdegree()` are the
`mode = "in"` and `mode = "out"` forms.

## References

Freeman, L. C. (1978). Centrality in social networks conceptual
clarification. Social Networks, 1(3), 215-239.
[doi:10.1016/0378-8733(78)90021-7](https://doi.org/10.1016/0378-8733%2878%2990021-7)
.

## See also

[`centrality_strength`](https://sonsoles.me/cograph/reference/centrality_strength.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_degree(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          6          7          8          6          6          5          4 
#>   Evaluate     Create      Share 
#>          5          7          6 
```
