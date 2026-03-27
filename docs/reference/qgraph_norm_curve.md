# qgraph Curve Normalization Factor

Calculates the normalization factor for edge curvature to maintain
consistent visual appearance across different plot sizes. Formula:
sqrt(sum(pin^2)) / sqrt(7^2 + 7^2)

## Usage

``` r
qgraph_norm_curve()
```

## Value

Numeric normalization factor.
