# Add or Change Edge Attributes

Add or Change Edge Attributes

## Usage

``` r
mutate_edges(
  x,
  ...,
  community = "louvain",
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- ...:

  Named expressions evaluated against the edge table, with the same
  metrics and predicates
  [`select_edges()`](https://sonsoles.me/cograph/reference/select_edges.md)
  offers, for example `strong = abs_weight > 0.5` or
  `scaled = weight / max(weight)`.

- community:

  Community detection method used when an expression refers to
  `same_community`, `from_community` or `to_community`. Default
  `"louvain"`.

- keep_format:

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A `cograph_network` whose edge table has the new columns, or the input
format when `keep_format = TRUE`.

## See also

[`mutate_nodes`](https://sonsoles.me/cograph/reference/mutate_nodes.md),
[`select_edges`](https://sonsoles.me/cograph/reference/select_edges.md)

## Examples

``` r
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

as.data.frame(mutate_edges(adj, strong = weight > 0.5))
#>   from to weight strong
#> 1    A  B    0.5  FALSE
#> 2    A  C    0.8   TRUE
#> 3    B  C    0.3  FALSE
#> 4    B  D    0.6   TRUE
#> 5    C  D    0.4  FALSE
```
