# Local Information Dimensionality

Entropy-weighted local dimension (Wen & Deng 2020). With \\p_i(l) =
B_i(l) / N\\ the share of the network inside the box of \\l\\ hops
around \\i\\ (node included), the box information is \\I_i(l) = -p_i(l)
\ln p_i(l)\\ and \$\$D^I_i = -\frac{d I_i(l)}{d \ln l},\$\$ estimated as
minus the least-squares slope of \\I_i(l)\\ on \\\ln l\\ for \\l = 1,
\ldots, \lceil d\_{\max}(i) / 2 \rceil\\. **Higher values mark more
influential nodes.** When only one box size is available the discretized
derivative of the source paper, \\l (1 + \ln p_i(l))\\ n_i(l) / N\\, is
reported.

## Usage

``` r
centrality_local_information_dimension(x, mode = "all", ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- mode:

  For directed networks: `"all"` (default), `"out"` (distances along
  out-edges), or `"in"`.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector, one value per node. `NaN` for a node that reaches
no other node.

## Details

Distances are hop counts; edge weights are ignored.

## References

Wen, T., & Deng, Y. (2020). Identification of influencers in complex
networks by local information dimensionality. Information Sciences, 512,
549-562.

## See also

[`centrality_local_dimension`](https://sonsoles.me/cograph/reference/centrality_local_dimension.md).

## Examples

``` r
path5 <- matrix(0, 5, 5)
path5[cbind(1:4, 2:5)] <- 1; path5 <- path5 + t(path5)
rownames(path5) <- colnames(path5) <- LETTERS[1:5]
centrality_local_information_dimension(path5)
#>          A          B          C          D          E 
#> 0.08659188 0.18463688 0.19566975 0.18463688 0.08659188 
```
