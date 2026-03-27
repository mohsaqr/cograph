# Detect Duplicate Edges in Undirected Network

Identifies edges that appear multiple times between the same pair of
nodes. For undirected networks, edges A`->`B and B`->`A are considered
duplicates. For directed networks, only identical from`->`to pairs are
duplicates.

## Usage

``` r
detect_duplicate_edges(edges)
```

## Arguments

- edges:

  Data frame with `from` and `to` columns (and optionally `weight`).

## Value

A list with two components:

- has_duplicates:

  Logical indicating whether any duplicates were found.

- info:

  A list of duplicate details, where each element contains: `nodes` (the
  node pair), `count` (number of edges), and `weights` (vector of
  weights if available).

## Details

This function is useful for cleaning network data before visualization.
Duplicate edges can arise from:

- Data collection errors (same edge recorded twice)

- Combining multiple data sources

- Converting from formats that allow multi-edges

- Edge lists that include both A`->`B and B`->`A for undirected networks

The function creates canonical keys by sorting node pairs (lower index
first), so edges 1`->`2 and 2`->`1 map to the same key "1-2" in
undirected mode.

## See also

[`aggregate_duplicate_edges`](http://sonsoles.me/cograph/reference/aggregate_duplicate_edges.md)
for combining duplicates into single edges
