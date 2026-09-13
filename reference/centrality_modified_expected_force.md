# Modified Expected Force centrality

Multiplies the two-event Expected Force by the logarithm of alpha times
seed degree (Lawyer 2015, equation 2). Alpha defaults to two, as in the
paper, and must be finite and strictly greater than one. Directed input
uses outgoing degree, consistent with the outgoing transmission process.
An isolate scores zero without evaluating the logarithm of zero. All
graph, event-counting and zero-force conventions of
[`centrality_expected_force`](https://sonsoles.me/cograph/reference/centrality_expected_force.md)
apply. Native log addition avoids overflow when alpha times degree
cannot be represented.

## Usage

``` r
centrality_modified_expected_force(x, exf_alpha = 2, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- exf_alpha:

  Degree rescaling factor, default two, finite and greater than one. The
  paper motivates small values; larger finite values are permitted by
  the formula without a predictive-performance claim.

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  `normalized = TRUE` divides by the maximum score; all-zero results
  remain zero.

## Value

Named numeric vector in input node order.

## References

Lawyer, G. (2015). Understanding the influence of all nodes in a
network. Scientific Reports, 5, 8665, equation 2.
[doi:10.1038/srep08665](https://doi.org/10.1038/srep08665) .

## Examples

``` r
centrality_modified_expected_force(igraph::make_graph("Zachary"))
#>         1         2         3         4         5         6         7         8 
#> 19.634822 13.579674 14.822717 10.462639  5.717804  7.021545  7.021545  8.028485 
#>         9        10        11        12        13        14        15        16 
#>  9.853701  4.534737  5.717804  1.871167  4.251166  9.805879  4.636217  4.636217 
#>        17        18        19        20        21        22        23        24 
#>  2.539017  4.423686  4.636217  6.771364  4.636217  4.423686  4.636217  9.103169 
#>        25        26        27        28        29        30        31        32 
#>  4.730017  4.846594  4.192057  7.716535  6.364664  7.787481  8.118877 10.690913 
#>        33        34 
#> 16.382792 20.290653 
```
