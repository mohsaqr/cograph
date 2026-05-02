# Diffusion Centrality

Sum of scaled degrees of a node and its neighbors, measuring the node's
potential for spreading information through the network.

## Usage

``` r
centrality_diffusion(x, mode = "all", lambda = 1, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- mode:

  For directed networks: `"all"` (default), `"in"`, or `"out"`. Only
  used when `diffusion_method = "kandhway_kuri"` (the default for
  non-tna inputs); ignored under `"power_series"`, which always treats
  the matrix as the row transition operator.

- lambda:

  Scaling factor for neighbor contributions. Default 1. Only used when
  `diffusion_method = "kandhway_kuri"`.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
  (e.g., `diffusion_method`, `loops`, `weighted`, `directed`).

## Value

Named numeric vector of diffusion centrality values.

## Details

Two methods are supported. `"kandhway_kuri"` (Kandhway & Kuri, 2014)
computes the 1-hop binary-degree neighborhood sum and is the default for
raw matrices, igraph objects, and other non-tna inputs. `"power_series"`
computes \\\mathrm{rowSums}(P + P^2 + \ldots + P^n)\\ on the weighted
matrix (with `diag(P) := 0` when `loops = FALSE`) and matches
`tna::centralities(., measures = "Diffusion")` byte-for-byte. For tna
inputs, the default switches to `"power_series"` to match user
expectation; pass `diffusion_method = "kandhway_kuri"` to force the
binary-degree formula.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_diffusion(adj)
#> A B C 
#> 6 6 6 
```
