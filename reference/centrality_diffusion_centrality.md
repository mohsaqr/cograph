# Diffusion Centrality

Diffusion centrality (Banerjee et al. 2013) counts the walks of length 1
to \\T\\ that start at a node, each discounted by \\q\\ per step:
\$\$DC(A; q, T) = \sum\_{t=1}^{T} (qA)^t \mathbf{1}.\$\$ Walks may
revisit nodes and return to the source, so the score counts repeated
hearings of a message.

## Usage

``` r
centrality_diffusion_centrality(x, diffusion_q = 1, diffusion_steps = 3, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- diffusion_q:

  Discount \\q\\, between 0 and 1. Default 1.

- diffusion_steps:

  Horizon \\T\\, a nonnegative integer. Default 3.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `weighted` (default `TRUE`), `loops` (default `TRUE`)
  and `normalized` (default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

\\A\\ holds the edge weights, or ones with `weighted = FALSE`. Directed
edges carry information from source to target, so the score uses row
sums and `mode` has no effect. Transposing the input gives incoming
walks. Self-loops follow `loops`. Negative or non-finite weights raise
an error. \\T = 0\\ gives 0 and \\T = 1\\ gives \\q\\ times the
out-strength. When every entry of \\qA\\ lies between 0 and 1 the score
is the expected number of times the information is heard (Banerjee et
al. 2013), and it can exceed the number of nodes. The defaults \\q = 1\\
and \\T = 3\\ are package choices. This measure differs from
[`centrality_diffusion`](https://sonsoles.me/cograph/reference/centrality_diffusion.md).

## References

Banerjee, A., Chandrasekhar, A. G., Duflo, E., & Jackson, M. O. (2013).
The Diffusion of Microfinance. Science, 341, 1236498.
[doi:10.1126/science.1236498](https://doi.org/10.1126/science.1236498) .

Banerjee, A., Chandrasekhar, A. G., Duflo, E., & Jackson, M. O. (2019).
Using Gossips to Spread Information: Theory and Evidence from Two
Randomized Controlled Trials. Review of Economic Studies, 86, 2453-2490.
[doi:10.1093/restud/rdz008](https://doi.org/10.1093/restud/rdz008) .

## See also

[`centrality_dynamics_sensitive`](https://sonsoles.me/cograph/reference/centrality_dynamics_sensitive.md),
[`centrality_diffusion`](https://sonsoles.me/cograph/reference/centrality_diffusion.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_diffusion_centrality(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   1.265867   3.898775   1.365553   1.617645   0.399290   1.429347   1.124945 
#>   Evaluate     Create      Share 
#>   1.755606   2.225349   2.720083 
```
