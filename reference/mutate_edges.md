# Add or Change Edge Attributes

Evaluates expressions against the edge table and stores the results as
edge columns.

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
as.data.frame(mutate_edges(regulation_net, strong = weight > 0.2))
#>          from         to weight strong
#> 1       Adapt    Explore   0.28   TRUE
#> 2     Reflect    Explore   0.05  FALSE
#> 3     Discuss    Explore   0.30   TRUE
#> 4      Create    Explore   0.14  FALSE
#> 5  Synthesize       Plan   0.11  FALSE
#> 6       Share       Plan   0.21   TRUE
#> 7        Plan    Monitor   0.13  FALSE
#> 8     Reflect    Monitor   0.15  FALSE
#> 9  Synthesize    Monitor   0.07  FALSE
#> 10   Evaluate    Monitor   0.33   TRUE
#> 11     Create    Monitor   0.17  FALSE
#> 12      Share    Monitor   0.49   TRUE
#> 13    Monitor      Adapt   0.16  FALSE
#> 14   Evaluate      Adapt   0.43   TRUE
#> 15      Share      Adapt   0.39   TRUE
#> 16    Explore    Reflect   0.35   TRUE
#> 17    Discuss    Reflect   0.35   TRUE
#> 18 Synthesize    Reflect   0.42   TRUE
#> 19   Evaluate    Reflect   0.07  FALSE
#> 20       Plan    Discuss   0.40   TRUE
#> 21      Adapt    Discuss   0.34   TRUE
#> 22      Adapt Synthesize   0.17  FALSE
#> 23       Plan   Evaluate   0.49   TRUE
#> 24     Create   Evaluate   0.39   TRUE
#> 25       Plan     Create   0.20  FALSE
#> 26    Monitor     Create   0.37   TRUE
#> 27    Discuss     Create   0.14  FALSE
#> 28    Explore      Share   0.27   TRUE
#> 29       Plan      Share   0.36   TRUE
#> 30     Create      Share   0.23   TRUE
```
