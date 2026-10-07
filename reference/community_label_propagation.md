# Label Propagation Community Detection

Label propagation community detection. Each node repeatedly adopts the
most frequent label among its neighbors.

## Usage

``` r
community_label_propagation(
  x,
  weights = NULL,
  mode = c("out", "in", "all"),
  initial = NULL,
  fixed = NULL,
  seed = NULL,
  ...
)

com_lp(
  x,
  weights = NULL,
  mode = c("out", "in", "all"),
  initial = NULL,
  fixed = NULL,
  seed = NULL,
  ...
)
```

## Arguments

- x:

  Network input.

- weights:

  Edge weights. `NULL` uses the network weights and `NA` runs
  unweighted. Negative weights are replaced by their absolute values.

- mode:

  Direction of label propagation in directed graphs, one of `"out"`
  (default), `"in"` or `"all"`.

- initial:

  Initial labels, an integer vector, or `NULL` for a unique label per
  node.

- fixed:

  Logical vector marking the nodes whose labels are fixed.

- seed:

  Random seed for reproducibility. Default NULL.

- ...:

  Passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md),
  whose only other argument is `directed`; anything else raises an
  "unused argument" error.

## Value

A `cograph_communities` data frame with columns `node` and `community`.
See
[`communities`](https://sonsoles.me/cograph/reference/communities.md)
for its attributes.

## References

Raghavan, U.N., Albert, R., & Kumara, S. (2007). Near linear time
algorithm to detect community structures in large-scale networks.
*Physical Review E*, 76, 036106.

## Examples

``` r
community_label_propagation(regulation_net, seed = 1)
#> Community structure (label_propagation)
#>   Nodes: 10  | Communities: 1  | Modularity: 0 
#>   Sizes: 10 
#> 
#>        node community
#>     Explore         1
#>        Plan         1
#>     Monitor         1
#>       Adapt         1
#>     Reflect         1
#>     Discuss         1
#>  Synthesize         1
#>    Evaluate         1
#>      Create         1
#>       Share         1
```
