# Transportation Centrality

Transportation centrality (Piraveenan and Saripada 2023) is a
betweenness in which traffic between two nodes spreads over all simple
paths, a longer path carrying less: \$\$TC(v) = \frac{1}{(N-1)(N-2)}
\sum\_{s \ne v \ne t} \frac{\sum\_{i \in P^v\_{s,t}} e^{-\beta C_i}}
{\sum\_{j \in P\_{s,t}} e^{-\beta C_j}},\$\$ where \\P\_{s,t}\\ holds
all simple paths from \\s\\ to \\t\\, \\P^v\_{s,t}\\ those through
\\v\\, and \\C\\ is the number of edges of a path.

## Usage

``` r
centrality_transportation(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `transport_beta` (the sensitivity to path length,
  default 0.1) and `enumeration_limit`.

## Value

A named numeric vector with one score per node, in input node order,
between 0 and 1, or `NA` at every node when the score was not computed.

## Details

Direction is used, and weights, loops and parallel edges are ignored.
The pairs \\(s, t)\\ are ordered. `transport_beta` is 0.1 by default,
the value the experiments of the source concentrate on; its worked
example uses 2, and it states no single default. As `transport_beta`
grows the measure tends to shortest-path betweenness divided by
\\(N-1)(N-2)\\, and at 0 every simple path counts alike. A pair without
a path adds nothing. A node with one tie scores 0. A negative
`transport_beta` raises a `cograph_bad_parameter` error, and a network
of fewer than three nodes gives `NA` with a `cograph_undefined_measure`
warning.

The number of simple paths grows exponentially with the size and density
of the network. The measure is therefore costly, computed only when
requested by name or through `include`, and it stops after
`enumeration_limit` search steps (default one million): every score is
then `NA` with a `cograph_undefined_measure` warning.

## References

Piraveenan, M. and Saripada, N. B. (2023). Transportation centrality:
quantifying the relative importance of nodes in transportation networks
based on traffic modeling. IEEE Access, 11, 142214-142234.
[doi:10.1109/ACCESS.2023.3339121](https://doi.org/10.1109/ACCESS.2023.3339121)
.

## See also

[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality_rsp_betweenness`](https://sonsoles.me/cograph/reference/centrality_rsp_betweenness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
paw <- matrix(0, 4, 4, dimnames = list(1:4, 1:4))
paw[rbind(c(1, 2), c(1, 3), c(2, 3), c(3, 4))] <- 1
paw <- paw + t(paw)
centrality_transportation(paw, transport_beta = 2)
#>          1          2          3          4 
#> 0.07946861 0.07946861 0.70640097 0.00000000 
```
