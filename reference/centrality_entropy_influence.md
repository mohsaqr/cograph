# Entropy-Based Social Influence

The entropy-based influence of Peng et al. (2017) combines the direct
influence of a node on its friends with its indirect influence on the
friends of its friends: \$\$I_i = \omega\\DI_i + (1 - \omega)\\II_i,
\qquad DI_i = \alpha \log\_{10} n_i + (1 - \alpha)\\H_i.\$\$ Two nodes
are friends when each has interacted with the other, \\n_i\\ is the
number of friends of \\i\\, and \\H_i\\ is the entropy, to base 10, of
the shares of its outgoing interactions that go to each friend. \\II_i\\
averages, over the nodes two steps away, \\DI_i\\ times the mean \\DI\\
of the friends that connect the two.

## Usage

``` r
centrality_entropy_influence(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `ei_alpha` (the weight of the friend count in the
  direct influence, default 0.4) and `ei_omega` (the weight of the
  direct influence, default 0.6).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure reads the edge weights as numbers of interactions and uses
direction: a tie counts only when it runs both ways, and the entropy
uses the outgoing weights. On an undirected network every tie is mutual;
on a directed network without mutual ties every score is 0. With
`weighted = FALSE`, or on an unweighted network, \\H_i = \log\_{10}
n_i\\ and the direct influence is the logarithm of the degree. Loops are
ignored. `ei_alpha` is 0.4 and `ei_omega` 0.6 by default, the values of
the source's worked example. A node with no node two steps away has
indirect influence 0, and a node without friends scores 0; the source
leaves both cases open. The implementation reproduces the direct
influence of all 11 nodes of the source's worked example and the
indirect influence of 8; the other 3 printed values follow from slips in
its hand calculation, and the printed ranking is unchanged. The
Centrality Zoo's improved entropy-based centrality cites this source,
but its formula is that of Ni et al. (2020), which is
[`centrality_improved_entropy`](https://sonsoles.me/cograph/reference/centrality_improved_entropy.md);
this measure is not a Zoo label. An `ei_alpha` or `ei_omega` outside
\\\[0, 1\]\\ raises a `cograph_bad_parameter` error, and negative
weights a `cograph_negative_weights` error. `normalized = TRUE` divides
the scores by their maximum.

## References

Peng, S., Yang, A., Cao, L., Yu, S. and Xie, D. (2017). Social influence
modeling using information theory in mobile social networks. Information
Sciences, 379, 146-159.
[doi:10.1016/j.ins.2016.08.023](https://doi.org/10.1016/j.ins.2016.08.023)
.

## See also

[`centrality_entropy`](https://sonsoles.me/cograph/reference/centrality_entropy.md),
[`centrality_diversity`](https://sonsoles.me/cograph/reference/centrality_diversity.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_entropy_influence(to_undirected(regulation_net))
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.6109697  0.6502134  0.7117450  0.6662276  0.5822812  0.6121916  0.4872665 
#>   Evaluate     Create      Share 
#>  0.5988017  0.6747310  0.6219952 
```
