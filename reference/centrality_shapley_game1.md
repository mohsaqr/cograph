# Shapley Value Centrality

Shapley value centrality (Michalak et al. 2013) is the Shapley value of
each node in a coalition game whose worth \\v(C)\\ counts the nodes a
coalition \\C\\ covers. Each game has an exact closed form. In game 1 a
coalition covers its members and their neighbors, and \$\$SV(v) =
\sum\_{u \in \\v\\ \cup N(v)} \frac{1}{1 + k_u}.\$\$ In game 2 a
coalition covers its members and the nodes with at least \\k\\ neighbors
in it. In game 3 it covers the nodes within `shapley_cutoff` hops of it,
and \\N(v)\\ is replaced by the set of nodes within that distance.

## Usage

``` r
centrality_shapley_game1(x, ...)

centrality_shapley_game2(x, shapley_k = 2, ...)

centrality_shapley_game3(x, shapley_cutoff = 2, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

- shapley_k:

  Neighbor threshold \\k\\ for game 2 (default 2).

- shapley_cutoff:

  Hop cutoff for game 3 (default 2).

## Value

A named numeric vector with one Shapley value per node, in input node
order.

## Details

The values of every game sum to the number of nodes. Game 2 with \\k =
1\\ and game 3 with cutoff 1 equal game 1. Degrees exclude self-loops,
and edge weights are ignored. On a directed network coverage runs along
out-edges and the denominators use in-degrees, the extension the source
states. Distances in game 3 are hop counts.

## References

Michalak, T. P., Aadithya, K. V., Szczepanski, P. L., Ravindran, B., &
Jennings, N. R. (2013). Efficient computation of the Shapley value for
game-theoretic network centrality. Journal of Artificial Intelligence
Research, 46, 607-650.

## See also

[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_shapley_game1(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.6500000  1.6428571  0.6428571  1.2833333  0.5428571  0.9833333  1.1761905 
#>   Evaluate     Create      Share 
#>  0.9261905  1.1761905  0.9761905 
centrality_shapley_game2(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.7166667  1.4523810  0.6190476  0.8166667  0.6690476  1.1333333  1.4357143 
#>   Evaluate     Create      Share 
#>  1.1023810  1.1023810  0.9523810 
centrality_shapley_game3(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.7138889  1.1107143  1.0190476  1.1678571  0.6583333  0.8329365  1.3107143 
#>   Evaluate     Create      Share 
#>  1.0329365  0.9678571  1.1857143 
```
