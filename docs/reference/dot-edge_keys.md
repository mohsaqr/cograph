# Canonical edge keys for grouping/deduplication. Undirected: sorts endpoints so A-B == B-A. Directed: preserves order.

Canonical edge keys for grouping/deduplication. Undirected: sorts
endpoints so A-B == B-A. Directed: preserves order.

## Usage

``` r
.edge_keys(from, to, directed = FALSE)
```
