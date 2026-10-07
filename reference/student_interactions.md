# Student Interaction Edge List

An edge list of observed interactions between 34 students during
collaborative learning sessions. Each row represents one observed
interaction between two students. The same pair may appear multiple
times, reflecting repeated interactions.

## Usage

``` r
student_interactions
```

## Format

A data frame with 389 rows and 2 columns:

- from:

  Character. Anonymized two-letter student code (e.g., "Ac", "Bd")

- to:

  Character. Anonymized two-letter student code (e.g., "Ce", "Df")

## Source

Anonymized collaborative learning interaction data.

## Value

A data frame with 389 rows and 2 columns:

- from:

  Character. Anonymized two-letter student code.

- to:

  Character. Anonymized two-letter student code.

## Details

The dataset includes self-loops (34 rows where `from == to`). These can
be removed with `subset(student_interactions, from != to)`.

The 389 rows contain 226 distinct ordered pairs. Because interactions
repeat, the edge list forms a multigraph when loaded into igraph with
[`igraph::graph_from_data_frame()`](https://r.igraph.org/reference/graph_from_data_frame.html).

## Examples

``` r
as_cograph(student_interactions)
#> Cograph network: 34 nodes, 389 edges ( directed )
#> Source: edgelist 
#> Data: data.frame (389 x 2) 
#>   Nodes (34): Ac, Ad, Fi, Ik, Vx, Rt, ... +28 more
#> Weights: 1 (all equal)
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
