# s-shell Index

Liu, Tang, Do and Hui's (2017) strength-based generalization of k-shell
for identifying spreaders. Each link is given an asymmetric weight from
the topology alone, \$\$w\_{ij} = 1 + (k_i \\ k^{out}\_j)^a,\$\$ where
\\k^{out}\_j\\ is the number of \\j\\'s neighbors that lie outside
\\i\\'s closed neighborhood (links that lead a spreading process to new
territory), and each node's strength is \\s_i = \sum\_{j \in N(i)}
w\_{ij}\\. The graph is then peeled like a k-shell but by strength: the
minimum remaining strength is the threshold, everything at or below it
is removed (neighbors lose the corresponding \\w\_{ji}\\), removals
cascade until the threshold holds, and the removed nodes receive the
next shell index. Higher index = more central. With \\a = 0\\ the shells
are the dense ranks of the k-core numbers.

## Usage

``` r
centrality_s_shell(x, s_shell_a = 0.5, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- s_shell_a:

  Exponent \\a\\ of the link weights. A single non-negative number;
  default 0.5. Anything else raises a `cograph_bad_parameter` error.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named integer vector of shell indices, one per node.

## Details

The index is an ordinal counter (1 = outermost shell), not a strength
value, so it is not comparable across graphs. Isolates form shell 1 on
their own, shifting every other shell up by one, as the paper's rule
implies. Direction, edge weights and self-loops are ignored. The paper's
robust default is \\a = 0.5\\.

Validated against the shell peeled at each threshold being exactly the
complement of the maximal subgraph in which every node keeps strength
above the threshold (brute force over all vertex subsets), and against
k-core dense ranks at \\a = 0\\.

## References

Liu, Y., Tang, M., Do, Y., & Hui, P. M. (2017). Accurate ranking of
influential spreaders in networks based on dynamically asymmetric link
weights. Physical Review E, 96(2), 022323.

## See also

[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md)
for the k-shell index.

## Examples

``` r
adj <- matrix(0, 6, 6)
adj[cbind(c(1, 2, 1, 3, 4, 5), c(2, 3, 3, 4, 5, 6))] <- 1
adj <- adj + t(adj)
rownames(adj) <- colnames(adj) <- LETTERS[1:6]
centrality_s_shell(adj)
#> A B C D E F 
#> 3 3 3 3 2 1 
```
