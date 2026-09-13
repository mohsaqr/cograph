# Improved global structure model centrality

The IGSM definition reproduced in Mukhtar et al. (2023), equation 5, is
\\IGSM(i)=\exp(k_i/N)\sum\_{j\ne i}k_j/d\_{ij}^{a}\\, with
\\a=\lceil\log_2(\overline{k})\rceil\\. The original method is
attributed to Zhu and Wang (2022); the exact equation used here was
checked in the later primary experimental paper, not its original full
text. IGSM uses simple degrees rather than GSM's core numbers, and its
distance exponent depends on global mean degree, including isolates.

## Usage

``` r
centrality_improved_global_structure(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector in input node order.

## Details

Topology, normalization and disconnected-graph conventions follow
[`centrality_global_structure`](https://sonsoles.me/cograph/reference/centrality_global_structure.md).
For a positive mean degree below one, the exponent may be zero or
negative; it is not clamped. With a negative exponent, more distant
reachable partners contribute more, an explicit consequence of extending
the equation to sparse disconnected inputs. Unreachable partners still
contribute zero. Edgeless graphs score zero by an explicit extension
because the logarithm of zero in the exponent is otherwise undefined.

This implements IGSM itself, without an additional nearest-neighbor
aggregation for the extended IGSM variant.

## References

Zhu, J.-C., & Wang, L.-W. (2022). An extended improved global structure
model for influential node identification in complex networks. Chinese
Physics B, 31, 068904.
[doi:10.1088/1674-1056/ac380d](https://doi.org/10.1088/1674-1056/ac380d)
. The implemented IGSM formula is reproduced as equation 5 in Mukhtar et
al. (2023),
[doi:10.1038/s41598-023-37570-7](https://doi.org/10.1038/s41598-023-37570-7)
.

## Examples

``` r
centrality_improved_global_structure(igraph::make_ring(4))
#>        1        2        3        4 
#> 8.243606 8.243606 8.243606 8.243606 
```
