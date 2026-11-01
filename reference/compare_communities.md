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

## References

Meila, M. (2007). Comparing clusterings: An information based distance.
*Journal of Multivariate Analysis*, 98(5), 873–895.
[doi:10.1016/j.jmva.2006.11.013](https://doi.org/10.1016/j.jmva.2006.11.013)

Danon, L., Diaz-Guilera, A., Duch, J., & Arenas, A. (2005). Comparing
community structure identification. *Journal of Statistical Mechanics:
Theory and Experiment*, 2005(09), P09008.
[doi:10.1088/1742-5468/2005/09/P09008](https://doi.org/10.1088/1742-5468/2005/09/P09008)

Hubert, L., & Arabie, P. (1985). Comparing partitions. *Journal of
Classification*, 2(1), 193–218.
[doi:10.1007/BF01908075](https://doi.org/10.1007/BF01908075)

## Examples

``` r
walktrap <- community_walktrap(regulation_net)
fast_greedy <- community_fast_greedy(regulation_net)
compare_communities(walktrap, fast_greedy, method = "nmi")
#> [1] 1
```
