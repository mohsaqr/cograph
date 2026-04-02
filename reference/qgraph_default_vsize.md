# qgraph Default Node Size

Calculates the default node size using qgraph's exact formula. Formula:
8 \* exp(-n/80) + 1

## Usage

``` r
qgraph_default_vsize(n_nodes)
```

## Arguments

- n_nodes:

  Number of nodes in the network.

## Value

Default vsize value (before scale factor conversion).
