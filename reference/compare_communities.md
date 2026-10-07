# Compare Community Structures

Compares two partitions of the same nodes with igraph's partition
comparison measures.

## Usage

``` r
compare_communities(
  comm1,
  comm2,
  method = c("vi", "nmi", "split.join", "rand", "adjusted.rand")
)
```

## Arguments

- comm1, comm2:

  Partitions to compare. Each is a `cograph_communities` data frame, an
  igraph `communities` object or a membership vector.

- method:

  Comparison measure, one of `"vi"` (default; variation of information),
  `"nmi"` (normalized mutual information), `"split.join"` (split-join
  distance), `"rand"` (Rand index) or `"adjusted.rand"` (adjusted Rand
  index).

## Value

A single numeric value. `"vi"` and `"split.join"` are distances (0 for
identical partitions); `"nmi"`, `"rand"` and `"adjusted.rand"` are
similarities (1 for identical partitions).

## Examples

``` r
walktrap <- community_walktrap(regulation_net)
fast_greedy <- community_fast_greedy(regulation_net)
compare_communities(walktrap, fast_greedy, method = "nmi")
#> [1] 1
```
