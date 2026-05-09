# qgraph Default Edge Size

Calculates the default maximum edge width using qgraph's exact formula.
For weighted networks, uses `15 * exp(-n/90) + 1` (halved for directed
networks, minimum 1). For unweighted networks, returns 2.

## Usage

``` r
qgraph_default_esize(n_nodes, weighted = TRUE, directed = FALSE)
```

## Arguments

- n_nodes:

  Number of nodes in the network.

- weighted:

  Logical: is the network weighted?

- directed:

  Logical: is the network directed?

## Value

Default esize value.
