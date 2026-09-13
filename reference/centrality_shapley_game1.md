# Shapley Value Centrality (Games 1, 2 and 3)

Game-theoretic centrality of Michalak, Aadithya, Szczepanski, Ravindran
and Jennings (2013): the Shapley value of each node in a coalition game
whose worth \\v(C)\\ is the number of nodes a coalition \\C\\ "covers".
Each game has a closed form, so the values are exact and cost linear
time.

## Usage

``` r
centrality_shapley_game1(x, ...)

centrality_shapley_game2(x, shapley_k = 2, ...)

centrality_shapley_game3(x, shapley_cutoff = 2, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- shapley_k:

  Neighbor threshold \\k\\ for game 2. Default 2.

- shapley_cutoff:

  Hop cutoff for game 3. Default 2.

## Value

Named numeric vector, one Shapley value per node.

## Details

- Game 1 (`shapley_game1`):

  \\v(C)\\ = nodes in \\C\\ or adjacent to it. \\SV(v) = \sum\_{u \in
  \\v\\ \cup N(v)} 1 / (1 + k_u)\\.

- Game 2 (`shapley_game2`):

  \\v(C)\\ = nodes in \\C\\ or with at least \\k\\ neighbors in \\C\\.
  \\SV(v) = \min(1, k / (1 + k_v)) + \sum\_{u \in N(v)} \max(0, (k_u -
  k + 1) / (k_u (1 + k_u)))\\. With \\k = 1\\ this is game 1. Threshold
  via `shapley_k` (default 2).

- Game 3 (`shapley_game3`):

  \\v(C)\\ = nodes within `shapley_cutoff` hops of \\C\\ (default 2).
  \\SV(v) = \sum\_{u \in \\v\\ \cup N_d(v)} 1 / (1 + \|N_d(u)\|)\\,
  where \\N_d(u)\\ is the set of nodes within \\d\\ hops of \\u\\. With
  cutoff 1 this is game 1.

Values in every game sum to the number of nodes (efficiency). Higher
values mark nodes whose presence adds more coverage to a typical
coalition. Degrees exclude self-loops, as in the paper. On a directed
graph the coverage runs along out-edges and the denominators use
in-degrees (the paper's stated extension); distances for game 3 are hop
counts, so edge weights are ignored.

Validated against exact Shapley values obtained by enumerating every
coalition on random graphs of up to eight nodes, including graphs with
isolates, self-loops and several components.

## References

Michalak, T. P., Aadithya, K. V., Szczepanski, P. L., Ravindran, B., &
Jennings, N. R. (2013). Efficient computation of the Shapley value for
game-theoretic network centrality. Journal of Artificial Intelligence
Research, 46, 607-650.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once.

## Examples

``` r
star5 <- matrix(0, 5, 5)
star5[1, 2:5] <- 1; star5[2:5, 1] <- 1
rownames(star5) <- colnames(star5) <- LETTERS[1:5]
centrality_shapley_game1(star5)
#>   A   B   C   D   E 
#> 2.2 0.7 0.7 0.7 0.7 
centrality_shapley_game2(star5, shapley_k = 2)
#>    A    B    C    D    E 
#> 0.40 1.15 1.15 1.15 1.15 
centrality_shapley_game3(star5, shapley_cutoff = 1)
#>   A   B   C   D   E 
#> 2.2 0.7 0.7 0.7 0.7 
```
