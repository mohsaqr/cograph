# Path-Transfer Centrality

Path-transfer centrality (Tutzauer 2007) is the entropy of where a flow
that starts at a node comes to rest. The flow never visits a node twice.
At a node with \\m\\ unvisited out-neighbors it stops with probability
\\1/(m+1)\\ and moves to each of them with the same probability. With
\\p\_{uv}\\ the probability that a flow from \\u\\ stops at \\v\\,
\$\$C_H(u) = -\sum\_{v} p\_{uv} \log_2 p\_{uv}.\$\$

## Usage

``` r
centrality_path_transfer(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `enumeration_limit` and `normalized`.

## Value

A named numeric vector with one score per node, in input node order, or
`NA` at every node when the search was cut short.

## Details

Direction is used, and weights, loops and parallel edges are ignored.
The score is in bits and lies between 0 and the base-2 logarithm of the
number of nodes. A node without outgoing ties scores 0, because its flow
can only stay. The definition follows its restatement by Oggier et al.
(2019); the 2007 article was not consulted.

The number of simple paths grows exponentially with the size and density
of the network. The measure is therefore costly, computed only when
requested by name or through `include`, and it stops after
`enumeration_limit` search steps (default one million): every score is
then `NA` with a `cograph_undefined_measure` warning.

## References

Tutzauer, F. (2007). Entropy as a measure of centrality in networks
characterized by path-transfer flow. Social Networks, 29(2), 249-265.
[doi:10.1016/j.socnet.2006.10.001](https://doi.org/10.1016/j.socnet.2006.10.001)
.

Oggier, F., Phetsouvanh, S. and Datta, A. (2019). A split-and-transfer
flow based entropic centrality. PeerJ Computer Science, 5, e220.
[doi:10.7717/peerj-cs.220](https://doi.org/10.7717/peerj-cs.220) .

## See also

[`centrality_transportation`](https://sonsoles.me/cograph/reference/centrality_transportation.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
flow <- matrix(0, 5, 5, dimnames = list(1:5, 1:5))
flow[rbind(c(1, 2), c(1, 3), c(1, 4), c(3, 5), c(4, 5))] <- 1
centrality_path_transfer(flow)
#>    1    2    3    4    5 
#> 2.25 0.00 1.00 1.00 0.00 
```
