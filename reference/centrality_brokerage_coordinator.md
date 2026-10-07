# Coordinator Brokerage

Coordinator brokerage (Gould and Fernandez 1989) counts the open
two-paths \\a \to v \to c\\ through node \\v\\ in which \\a\\, \\v\\ and
\\c\\ all belong to the same group. A two-path is open when the network
has no edge from \\a\\ to \\c\\. This role is \\w_I\\ in the notation of
the source.

## Usage

``` r
centrality_brokerage_coordinator(x, membership = NULL, ...)
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
it returns `NA` with a `cograph_undefined_measure` warning, and the same
happens when `membership` is missing, where the warning also has class
`cograph_bad_membership`. A `membership` whose length differs from the
number of nodes raises a `cograph_bad_membership` error. Group labels
may be numbers or strings. Edge weights and self-loops are ignored. The
other four roles are
[`centrality_brokerage_itinerant`](https://sonsoles.me/cograph/reference/centrality_brokerage_itinerant.md),
[`centrality_brokerage_representative`](https://sonsoles.me/cograph/reference/centrality_brokerage_representative.md),
[`centrality_brokerage_gatekeeper`](https://sonsoles.me/cograph/reference/centrality_brokerage_gatekeeper.md)
and
[`centrality_brokerage_liaison`](https://sonsoles.me/cograph/reference/centrality_brokerage_liaison.md).

## References

Gould, R. V., & Fernandez, R. M. (1989). Structures of mediation: A
formal approach to brokerage in transaction networks. Sociological
Methodology, 19, 89-126.
[doi:10.2307/270949](https://doi.org/10.2307/270949) .

## See also

[`centrality_brokerage_gatekeeper`](https://sonsoles.me/cograph/reference/centrality_brokerage_gatekeeper.md),
[`centrality_gateway`](https://sonsoles.me/cograph/reference/centrality_gateway.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_brokerage_coordinator(regulation_net, membership = rep(1:2, each = 5))
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          1          0          2          1          1          0          0 
#>   Evaluate     Create      Share 
#>          0          2          0 
```
