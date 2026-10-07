# Modified Expected Force Centrality

The modified Expected Force (Lawyer 2015, equation 2) multiplies the
Expected Force of a node by the logarithm of its scaled degree \\\alpha
d_i\\: \$\$ExF^{\alpha}\_i = \log(\alpha d_i) \\ ExF_i.\$\$

## Usage

``` r
centrality_modified_expected_force(x, exf_alpha = 2, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- exf_alpha:

  Degree scaling factor \\\alpha\\, a finite number greater than one.
  Default 2, as in the paper.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized` (divide by the maximum, default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

On a directed network \\d_i\\ is the out-degree, in line with the
outgoing transmission process. An isolated node scores zero. The input
handling and zero conventions of
[`centrality_expected_force`](https://sonsoles.me/cograph/reference/centrality_expected_force.md)
apply. An `exf_alpha` that is not a finite number greater than one
raises an error.

## References

Lawyer, G. (2015). Understanding the influence of all nodes in a
network. Scientific Reports, 5, 8665.
[doi:10.1038/srep08665](https://doi.org/10.1038/srep08665) .

## See also

[`centrality_expected_force`](https://sonsoles.me/cograph/reference/centrality_expected_force.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_modified_expected_force(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   2.435907   8.103802   2.877173   4.687413   2.196127   4.630090   4.769520 
#>   Evaluate     Create      Share 
#>   4.560098   6.291283   4.818151 
```
