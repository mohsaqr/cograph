# Entropy Variation

Ai's (2017) vitality measure: the change in the Shannon entropy of a
node-level distribution when a node and its links are removed,
\$\$EnV_f(i) = I_f(G) - I_f(G - i), \qquad I_f(G) = -\sum_j p_j \log
p_j, \quad p_j = \frac{f(j)}{\sum_l f(l)},\$\$ with \\f\\ the degree
(`"entropy_variation_degree"`, in-, out- or total degree by `mode`) or
the betweenness (`"entropy_variation_betweenness"`). Natural logarithm,
as in the author's code. The difference is signed: a positive value
means the remaining network is less even without the node, a negative
value that removing it evens the distribution out. Higher = more
important.

## Usage

``` r
centrality_entropy_variation(
  x,
  of = c("degree", "betweenness"),
  mode = "all",
  ...
)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- of:

  Which distribution: `"degree"` (default) or `"betweenness"`.

- mode:

  For the degree variant on directed networks: `"all"` (default, in +
  out), `"out"`, or `"in"`.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector, one value per node, in nats.

## Details

The degree variant is computed in closed form. The betweenness variant
recomputes betweenness once per node and costs \\O(n \cdot nm)\\; it
ignores edge weights. Self-loops are counted as igraph counts them. When
a deletion leaves every \\f\\ at zero (for instance betweenness on a
clique) that entropy is taken as 0.

Validated against the author's own R code path (`iCalEnV()` from the
paper's repository) to \\10^{-15}\\ and against the quantiles of Table 2
of the paper on its 4234-node Snake Idioms network.

## References

Ai, X. (2017). Node importance ranking of complex networks with entropy
variation. Entropy, 19(7), 303.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once.

## Examples

``` r
star5 <- matrix(0, 5, 5)
star5[1, 2:5] <- 1; star5[2:5, 1] <- 1
rownames(star5) <- colnames(star5) <- LETTERS[1:5]
centrality_entropy_variation(star5)
#>        A        B        C        D        E 
#> 1.386294 0.143841 0.143841 0.143841 0.143841 
centrality_entropy_variation(star5, of = "betweenness")
#> A B C D E 
#> 0 0 0 0 0 
```
