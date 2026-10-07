# Access and Hide Information

Access and hide information (Rosvall et al. 2005; Sneppen, Trusina and
Rosvall 2005) measure the number of bits a walker needs to follow a
shortest path without a map. The search information from \\i\\ to \\j\\
sums over all shortest paths \\p(i, j)\\, with \\k_i\\ the degree of the
source and \\k_l - 1\\ the choices left at each intermediate node:
\$\$S(i \to j) = -\log_2 \sum\_{p(i, j)} \frac{1}{k_i} \prod\_{l \in
p,\\ l \ne i, j} \frac{1}{k_l - 1}.\$\$ Access information \\A_i\\
averages \\S(i \to j)\\ over targets \\j\\, and hide information \\H_i\\
averages \\S(j \to i)\\ over sources.

## Usage

``` r
centrality_access_information(x, ...)

centrality_hide_information(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order, in
bits.

## Details

Distances are hop counts, so edge weights are ignored. On a directed
network every step uses the out-degree. The average runs over the nodes
a walker can reach, or be reached from, so values stay finite on a
disconnected network and equal the source's \\1/N\\ average on a
connected one. Hubs score high on access information and low on hide
information. On a star with five leaves the hub has access information
1.93 bits and hide information 0, and each leaf has access information
1.33 bits. The Centrality Zoo states the star case in reverse, and the
values above follow the formulas and the source papers.

## References

Rosvall, M., Trusina, A., Minnhagen, P., & Sneppen, K. (2005). Networks
and cities: An information perspective. Physical Review Letters, 94,
028701.

Sneppen, K., Trusina, A., & Rosvall, M. (2005). Hide-and-seek on complex
networks. Europhysics Letters, 69(5), 853-859.

## See also

[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_access_information(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   2.305054   2.340010   2.005376   2.253412   2.275489   2.167470   2.374502 
#>   Evaluate     Create      Share 
#>   2.275207   2.212248   2.211524 
centrality_hide_information(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   1.864995   2.779716   1.650978   1.729946   1.655327   2.627164   2.897916 
#>   Evaluate     Create      Share 
#>   3.194949   1.890976   2.128327 
```
