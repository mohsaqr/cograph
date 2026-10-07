# Itinerant Brokerage

Itinerant brokerage (Gould and Fernandez 1989), also called the
consultant role, counts the open two-paths \\a \to v \to c\\ through
node \\v\\ in which \\a\\ and \\c\\ belong to the same group and \\v\\
to another group. A two-path is open when the network has no edge from
\\a\\ to \\c\\. This role is \\w_O\\ in the notation of the source.

## Usage

``` r
centrality_brokerage_itinerant(x, membership = NULL, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- membership:

  Group labels, one per node.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named integer vector with one count per node, in input node order.
`normalized = TRUE` returns a numeric vector.

## Details

The measure is defined for directed networks. On an undirected network
it raises an unclassed warning and returns `NA`, and the same happens
when `membership` is missing. A `membership` whose length differs from
the number of nodes raises an unclassed error. Group labels may be
numbers or strings. Edge weights and self-loops are ignored.

## References

Gould, R. V., & Fernandez, R. M. (1989). Structures of mediation: A
formal approach to brokerage in transaction networks. Sociological
Methodology, 19, 89-126.
[doi:10.2307/270949](https://doi.org/10.2307/270949) .

## See also

[`centrality_brokerage_coordinator`](https://sonsoles.me/cograph/reference/centrality_brokerage_coordinator.md),
[`centrality_brokerage_liaison`](https://sonsoles.me/cograph/reference/centrality_brokerage_liaison.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_brokerage_itinerant(regulation_net, membership = rep(1:2, each = 5))
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          1          7          3          4          0          3          3 
#>   Evaluate     Create      Share 
#>          2          2          4 
```
