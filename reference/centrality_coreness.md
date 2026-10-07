# Coreness

Coreness (Seidman 1983) is the largest \\k\\ for which a node belongs to
the \\k\\-core, the maximal subnetwork in which every node has degree at
least \\k\\. It is found by repeatedly removing the nodes of lowest
degree.

## Usage

``` r
centrality_coreness(x, mode = "all", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  For directed networks: `"all"` (default), `"out"` or `"in"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `loops` and `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are ignored. `mode = "in"` and `mode = "out"` peel by
in-degree and out-degree, and `mode = "all"` by total degree, in which a
reciprocated tie counts twice. A self-loop adds a fixed amount to the
degree of its node throughout the peeling. The values match
[`igraph::coreness()`](https://r.igraph.org/reference/coreness.html).

## References

Seidman, S. B. (1983). Network structure and minimum degree. Social
Networks, 5(3), 269-287.
[doi:10.1016/0378-8733(83)90028-X](https://doi.org/10.1016/0378-8733%2883%2990028-X)
.

## See also

[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality_s_core`](https://sonsoles.me/cograph/reference/centrality_local_efficiency.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_coreness(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          5          5          5          5          5          5          4 
#>   Evaluate     Create      Share 
#>          5          5          5 
```
