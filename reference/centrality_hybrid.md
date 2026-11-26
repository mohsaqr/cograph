# Hybrid Centrality

Hybrid centrality (Christensen et al. 2018) combines eight rankings of
the nodes into one score: \$\$HC = \frac{BC^w + BC^u + LC^w + LC^u +
k^u + EC^w + EC^u + NS^w - 8} {8 (N - 1)},\$\$ where each term is the
rank of the node on one centrality: betweenness (\\BC\\), closeness
(\\LC\\) and eigenvector centrality (\\EC\\), each on the weighted
(\\w\\) and on the unweighted (\\u\\) network, the degree (\\k\\) and
the strength (\\NS\\).

## Usage

``` r
centrality_hybrid(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `weighted`.

## Value

A named numeric vector with one score per node, in input node order,
between 0 and 1, or `NA` at every node when the score is undefined.

## Details

The most central node holds the largest rank, and tied nodes share the
largest rank of their group. The score runs from 0, for a node last on
every centrality, to 1, for a node first on every one. Weights are read
as strengths of association, and the length of a path on the weighted
network is the sum of the reciprocal weights. On an unweighted network
the weighted and unweighted rankings coincide. Loops are ignored.

Betweenness is Freeman's shortest-path betweenness, as the source
states. The function `hybrid()` of the NetworkToolbox package, by the
first author, uses randomized shortest paths betweenness unless told
otherwise, and rounds every centrality to three decimals before ranking,
so its values can differ. Here every centrality is rounded to twelve
significant digits before ranking, so that scores equal in exact
arithmetic are tied.

The measure is defined for connected undirected networks of at least two
nodes. Otherwise every score is `NA` with a `cograph_undefined_measure`
warning.

## References

Christensen, A. P., Kenett, Y. N., Aste, T., Silvia, P. J. and Kwapil,
T. R. (2018). Network structure of the Wisconsin Schizotypy Scales-Short
Forms: examining psychometric network filtering approaches. Behavior
Research Methods, 50(6), 2531-2550.
[doi:10.3758/s13428-018-1032-9](https://doi.org/10.3758/s13428-018-1032-9)
.

## See also

[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md),
[`centrality_eigenvector`](https://sonsoles.me/cograph/reference/centrality_eigenvector.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
ring <- matrix(0, 5, 5, dimnames = list(letters[1:5], letters[1:5]))
ring[cbind(1:5, c(2:5, 1))] <- c(0.9, 0.2, 0.5, 0.7, 0.4)
ring <- ring + t(ring)
centrality_hybrid(ring)
#>       a       b       c       d       e 
#> 0.90625 0.71875 0.53125 0.81250 0.87500 
```
