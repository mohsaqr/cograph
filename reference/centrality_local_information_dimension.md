# Local Information Dimensionality

Local information dimensionality (Wen and Deng 2020) weights the local
dimension by information. With \\p_i(l) = B_i(l) / N\\ the share of the
network within \\l\\ hops of \\i\\, the node included, and box
information \\I_i(l) = -p_i(l) \ln p_i(l)\\, the measure is minus the
least-squares slope of \\I_i(l)\\ on \\\ln l\\ for \\l = 1, \ldots,
\lceil d\_{\max}(i) / 2 \rceil\\: \$\$D^I_i = -\frac{d I_i(l)}{d \ln
l}.\$\$

## Usage

``` r
centrality_local_information_dimension(x, mode = "all", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  For directed networks: `"all"` (default), `"out"` or `"in"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

Distances are hop counts, so edge weights are ignored. On a directed
network `mode` sets the direction of the paths. Higher values mark more
influential nodes. A node with a single box size returns the discretized
derivative of the source, \\l (1 + \ln p_i(l))\\ n_i(l) / N\\. A node
that reaches no other node returns `NaN`.

## References

Wen, T., & Deng, Y. (2020). Identification of influencers in complex
networks by local information dimensionality. Information Sciences, 512,
549-562.

## See also

[`centrality_local_dimension`](https://sonsoles.me/cograph/reference/centrality_local_dimension.md),
[`centrality_local_dimension_fixed`](https://sonsoles.me/cograph/reference/centrality_local_dimension_fixed.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_local_information_dimension(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.2445872  0.3859950  0.5437995  0.3859950  0.2445872  0.2445872  0.1227411 
#>   Evaluate     Create      Share 
#>  0.2445872  0.3859950  0.2445872 
```
