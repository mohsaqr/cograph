# Access and Hide Information

Search-information centralities of Rosvall, Trusina, Minnhagen and
Sneppen (2005) and Sneppen, Trusina and Rosvall (2005). A walker who
knows only the shortest paths from \\i\\ to \\j\\ but has no map must be
told which link to take at each step; the number of bits needed is
\$\$S(i \to j) = -\log_2 \sum\_{p \in \\p(i, j)\\} \frac{1}{k_i}
\prod\_{l \in p,\\ l \ne i, j} \frac{1}{k_l - 1},\$\$ summed over all
shortest paths, with \\k_i\\ the degree of the source and \\k_l - 1\\
the choices left at each intermediate node (the link the walker arrived
on is excluded). Then \$\$A_i = \frac{1}{N} \sum_j S(i \to j), \qquad
H_i = \frac{1}{N} \sum_j S(j \to i),\$\$ with \\S(i \to i) = 0\\.

## Usage

``` r
centrality_access_information(x, ...)

centrality_hide_information(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector, one value per node, in bits.

## Details

**Access information** \\A_i\\: how many bits it costs, on average, to
reach the rest of the network from \\i\\. A low value means the node
reaches others with few decisions. Hubs score *high*: a walker leaving a
hub has many links to choose from (on a star with five leaves the hub
scores 1.93 bits, a leaf 1.33). **Hide information** \\H_i\\: how many
bits it costs the rest of the network to find \\i\\. High values mark
hidden, peripheral nodes; hubs score low (the star hub scores 0). The
encyclopedia's prose states the star case the other way round; the
formulas and the source papers give the values above.

On a directed graph every step uses the out-degree, \\1 / k^{out}\\. On
a disconnected graph the average runs over the nodes a walker can
actually reach (or be reached from), so values stay finite; on a
connected graph this is exactly the paper's \\1 / N\\. Distances are hop
counts; edge weights are ignored. Cost is \\O(N (N + M))\\ with an \\N
\times N\\ matrix in memory.

Validated against an independent enumeration of all shortest paths and
against the worked values in both papers (star and complete bipartite
graphs).

## References

Rosvall, M., Trusina, A., Minnhagen, P., & Sneppen, K. (2005). Networks
and cities: An information perspective. Physical Review Letters, 94,
028701.

Sneppen, K., Trusina, A., & Rosvall, M. (2005). Hide-and-seek on complex
networks. Europhysics Letters, 69(5), 853-859.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once.

## Examples

``` r
star5 <- matrix(0, 5, 5)
star5[1, 2:5] <- 1; star5[2:5, 1] <- 1
rownames(star5) <- colnames(star5) <- LETTERS[1:5]
centrality_access_information(star5)
#>         A         B         C         D         E 
#> 1.6000000 0.9509775 0.9509775 0.9509775 0.9509775 
centrality_hide_information(star5)
#>        A        B        C        D        E 
#> 0.000000 1.350978 1.350978 1.350978 1.350978 
```
