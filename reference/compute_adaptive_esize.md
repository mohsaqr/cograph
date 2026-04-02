# Compute Adaptive Base Edge Size

Calculates the maximum edge width that decreases with more nodes.
Inspired by qgraph but scaled for line widths (not pixels).

## Usage

``` r
compute_adaptive_esize(n_nodes, directed = FALSE)
```

## Arguments

- n_nodes:

  Number of nodes in the network.

- directed:

  Whether the network is directed (directed networks use thinner edges).

## Value

Numeric maximum edge width (suitable for lwd parameter).

## Details

The formula produces reasonable line widths:

- 3 nodes: ~5

- 10 nodes: ~4.5

- 50 nodes: ~3

- 100 nodes: ~2

- 200 nodes: ~1.2

For directed networks, the size is reduced by 30% (minimum 1).
