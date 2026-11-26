# Improved Entropy-Based Centrality

The improved entropy-based centrality of Ni et al. (2020) adds a node's
direct influence, from the spread of its tie weights and of its
neighbors' degrees, and its indirect influence on the nodes two steps
away: \$\$I_i = \tfrac{1}{2} DI_i + \tfrac{1}{2} II_i, \qquad DI_i =
\tfrac{1}{2} H(p\_{i\cdot}) + \tfrac{1}{2} H(T\_{i\cdot}), \qquad
T\_{ij} = \frac{k_j^{\beta}}{\sum\_{l \in N(i)} k_l^{\beta}},\$\$ with
\\H\\ the entropy to base 10 over the neighbors, \\p\_{ij}\\ the share
of the weight of \\i\\ on its tie to \\j\\ and \\k\\ the degree.
\\II_i\\ averages, over the nodes two steps away, \\DI_i\\ times the
mean \\DI\\ of the neighbors they share with \\i\\.

## Usage

``` r
centrality_improved_entropy(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `ie_beta` (the exponent of the neighbor degree,
  default 2).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure uses the edge weights. With `weighted = FALSE`, or on an
unweighted network, \\H(p\_{i\cdot}) = \log\_{10} k_i\\. The source
defines it for undirected networks; when the weighted adjacency matrix
is not symmetric the measure warns (class `cograph_undefined_measure`)
and returns `NA`. Loops are ignored. `ie_beta = 2` and the mixing
weights of 1/2 are the source's basic values (its section 5); the mixing
weights are fixed. A node two steps away shares a neighbor with \\i\\
and is not adjacent to it. A node with no such node has indirect
influence 0, where the source leaves 0/0; an isolated node scores 0. The
source prints no value of the measure, so the implementation is checked
against the definition only. This is the measure the Centrality Zoo
describes as the improved entropy-based centrality; the Zoo's entry also
cites Peng et al. (2017), whose different measure is
[`centrality_entropy_influence`](https://sonsoles.me/cograph/reference/centrality_entropy_influence.md).
On four test networks the score ranks nodes closest to `ked` and `lnc`
(mean Kendall correlation 0.93); with the measure of Peng et al. it has
0.85 to 0.93. A `ie_beta` that is not a single finite number raises a
`cograph_bad_parameter` error, and negative weights a
`cograph_negative_weights` error. `normalized = TRUE` divides the scores
by their maximum.

## References

Ni, C., Yang, J. and Kong, D. (2020). Sequential seeding strategy for
social influence diffusion with improved entropy-based centrality.
Physica A: Statistical Mechanics and its Applications, 545, 123659.
[doi:10.1016/j.physa.2019.123659](https://doi.org/10.1016/j.physa.2019.123659)
.

## See also

[`centrality_entropy_influence`](https://sonsoles.me/cograph/reference/centrality_entropy_influence.md),
[`centrality_entropy`](https://sonsoles.me/cograph/reference/centrality_entropy.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_improved_entropy(to_undirected(regulation_net))
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.5873504  0.6203873  0.6868462  0.6334209  0.5547494  0.5900044  0.4747818 
#>   Evaluate     Create      Share 
#>  0.5794531  0.6493129  0.6005726 
centrality_improved_entropy(to_undirected(regulation_net), ie_beta = 0)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.5941110  0.6336524  0.6977712  0.6465333  0.5696051  0.5970275  0.4833657 
#>   Evaluate     Create      Share 
#>  0.5876161  0.6592209  0.6079290 
```
