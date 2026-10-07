# KED Centrality

The KED method (Chen et al. 2014) combines a node's degree \\k_i\\ with
the number and the diversity of the local paths that leave it. The local
path number \\K_i = \sum\_{j \in N(i)} k_j\\ is the neighbors' degree
sum, and the path diversity \\H_i\\ is the entropy of the shares \\p_j =
k_j / K_i\\ divided by \\\log k_i\\. \$\$KED_i = k_i \\ (1 + H_i) \\
\exp(K_i / N), \qquad H_i = \frac{-\sum\_{j \in N(i)} p_j \log p_j}{\log
k_i}\$\$

## Usage

``` r
centrality_ked(x, ...)
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

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
The source's directed variant is not implemented. \\H_i\\ lies in \\\[0,
1\]\\, and it is set to zero for a node with one neighbor, where it is
\\0/0\\. Isolates score zero. \\N\\ is the number of nodes in the whole
network, so adding a disconnected component changes every score and can
change the ranking. The source's stated range \\1 \le \exp(K_i/N) \le
e\\ holds only when \\K_i \le N\\, and an overflow of the exponential
raises an error. The Centrality Zoo (section 2.215) adds exponents,
drops the \\1 +\\ term and divides by the largest \\K_l\\. That form
does not reproduce the scores printed in the source.

## References

Chen, D.-B., Xiao, R., Zeng, A. and Zhang, Y.-C. (2014). Path diversity
improves the identification of influential spreaders. Europhysics
Letters, 104(6), 68006.
[doi:10.1209/0295-5075/104/68006](https://doi.org/10.1209/0295-5075/104/68006)
.

## See also

[`centrality_lnc`](https://sonsoles.me/cograph/reference/centrality_lnc.md),
[`centrality_entropy`](https://sonsoles.me/cograph/reference/centrality_entropy.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_ked(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   148.6091   293.1288   564.9522   265.3054   133.9247   164.2483    87.9635 
#>   Evaluate     Create      Share 
#>   200.5071   324.5153   200.5071 
```
