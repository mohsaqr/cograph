# Convert Network to Adjacency Matrix

Converts any supported network format to an adjacency matrix.

## Usage

``` r
to_matrix(x, directed = NULL)
```

## Arguments

- x:

  Network input: matrix, cograph_network, igraph, network, tna, etc.

- directed:

  Logical or NULL. If NULL (default), auto-detect from input.

## Value

A square numeric adjacency matrix, preserving row/column names when
available.

## See also

[`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md),
[`to_df`](https://sonsoles.me/cograph/reference/to_data_frame.md),
[`as_cograph`](https://sonsoles.me/cograph/reference/as_cograph.md),
[`to_network`](https://sonsoles.me/cograph/reference/to_network.md)

## Examples

``` r
to_matrix(student_interactions)
#>    Ac Ad Fi Ik Vx Rt Km Gj Bd Ce Oq Ya Mo Hj Tv Eg Pr Qs Xz Np Dg Hk Wy Jl Fh
#> Ac  1  1  1  0  0  1  0  0  0  0  0  0  1  0  0  0  1  0  0  0  0  1  0  0  1
#> Ad  1  0  1  0  1  0  0  0  1  0  0  0  0  0  0  0  0  1  0  0  0  1  0  0  1
#> Fi  1  1  1  0  1  0  1  1  0  0  0  1  0  0  0  0  0  0  0  0  0  1  0  0  1
#> Ik  1  1  1  0  0  1  0  0  0  0  0  0  0  0  0  0  0  1  0  0  0  1  0  0  0
#> Vx  1  1  1  1  1  0  0  1  0  0  0  0  0  1  0  0  0  0  0  0  0  0  0  1  0
#> Rt  1  1  0  0  1  0  0  1  0  0  0  0  0  1  0  0  0  0  1  0  0  1  0  1  1
#> Km  1  0  1  1  1  0  0  1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#> Gj  1  0  0  1  0  0  1  1  1  0  0  0  0  0  0  1  1  0  0  0  0  0  0  0  1
#> Bd  1  1  0  0  1  0  1  1  0  0  0  1  0  0  0  0  0  0  0  0  0  1  0  0  0
#> Ce  0  0  0  0  1  1  0  0  1  0  0  0  1  1  0  0  0  0  0  0  0  0  0  0  0
#> Oq  1  1  0  0  0  1  0  0  0  1  0  1  0  1  0  0  0  1  1  0  0  0  0  0  0
#> Ya  1  0  1  0  0  1  0  1  0  0  1  0  0  0  0  0  0  1  0  0  0  0  0  0  0
#> Mo  1  1  0  0  1  0  0  0  0  0  1  1  0  0  0  0  0  0  0  0  0  0  0  0  1
#> Hj  1  0  1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  1  0  0
#> Tv  0  0  0  1  1  1  0  0  1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  1  0
#> Eg  0  0  1  0  1  0  0  0  0  0  0  0  1  0  0  0  0  0  0  0  1  0  0  1  1
#> Pr  1  0  0  0  1  0  0  0  0  0  0  0  0  1  1  1  0  0  0  0  1  0  0  0  0
#> Qs  0  0  0  1  0  1  1  1  0  0  0  0  1  0  0  1  1  0  1  0  0  0  0  0  0
#> Xz  1  0  1  0  0  0  0  0  0  0  0  1  0  1  1  0  0  1  1  0  0  0  0  0  0
#> Np  1  1  1  1  0  0  0  0  0  1  0  0  0  0  0  0  0  0  0  0  0  1  0  0  0
#> Dg  1  1  1  0  1  1  0  0  0  1  1  1  0  0  0  0  0  0  1  0  0  0  0  0  0
#> Hk  1  0  1  0  0  0  0  0  0  0  0  0  1  1  1  0  0  1  0  0  0  0  0  0  0
#> Wy  1  0  0  0  0  0  0  0  0  1  0  0  0  0  0  0  0  0  0  0  0  1  0  1  0
#> Jl  1  0  0  0  0  1  1  0  0  1  1  1  0  0  0  0  0  0  0  1  1  0  0  0  0
#> Fh  1  0  0  0  0  1  1  0  0  0  1  0  0  0  0  0  1  0  0  1  0  0  0  1  1
#> Zb  1  0  0  0  0  0  0  1  0  0  0  0  0  0  0  1  0  0  0  0  0  0  1  0  1
#> Eh  1  1  0  0  1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#> Be  1  0  0  0  1  1  0  0  0  0  0  0  0  1  1  0  1  0  0  0  1  0  1  0  1
#> Df  0  1  1  0  1  0  0  1  1  0  1  0  0  0  0  0  0  0  0  0  0  1  0  0  0
#> Cf  1  0  0  1  0  0  0  0  0  0  0  0  0  0  0  0  0  1  0  0  0  1  0  0  1
#> Su  0  1  1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  1  0  0  0  1  0  0
#> Ln  0  0  0  1  1  0  0  0  0  0  0  0  0  0  0  0  0  0  1  0  0  0  0  1  1
#> Gi  0  0  0  0  1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#> Uw  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  1  0  0
#>    Zb Eh Be Df Cf Su Ln Gi Uw
#> Ac  0  0  0  0  0  0  0  0  0
#> Ad  0  0  0  0  0  0  0  0  0
#> Fi  0  0  0  0  0  0  0  0  0
#> Ik  0  0  0  0  0  0  0  0  0
#> Vx  0  0  0  0  1  0  0  0  0
#> Rt  0  0  0  0  0  0  0  0  0
#> Km  0  0  0  0  0  0  0  0  0
#> Gj  0  0  1  0  0  0  0  0  0
#> Bd  0  0  0  0  0  0  0  0  0
#> Ce  0  0  0  0  0  0  0  0  0
#> Oq  0  0  0  0  0  0  0  0  0
#> Ya  0  0  0  0  0  0  0  0  0
#> Mo  0  0  0  0  1  0  0  0  0
#> Hj  0  0  0  0  1  0  0  0  0
#> Tv  0  0  0  0  0  0  1  0  0
#> Eg  0  0  0  0  0  0  0  0  0
#> Pr  0  0  0  0  0  0  0  0  0
#> Qs  0  0  0  0  0  0  0  0  0
#> Xz  0  0  0  0  0  0  0  0  0
#> Np  0  0  0  0  0  0  0  0  0
#> Dg  0  0  0  0  0  0  0  0  0
#> Hk  0  0  0  0  0  0  0  0  0
#> Wy  0  0  1  0  1  0  0  0  0
#> Jl  0  0  0  0  0  0  0  0  0
#> Fh  0  0  0  0  0  0  1  0  0
#> Zb  0  1  0  0  0  0  0  0  0
#> Eh  1  0  0  0  0  0  0  0  0
#> Be  0  0  0  0  0  1  0  0  0
#> Df  0  0  0  0  0  0  0  0  0
#> Cf  0  1  1  0  0  0  0  0  0
#> Su  0  0  0  1  0  0  0  0  0
#> Ln  0  0  0  0  0  0  0  0  0
#> Gi  0  0  0  0  1  0  0  0  0
#> Uw  0  1  1  0  0  0  0  1  0
```
