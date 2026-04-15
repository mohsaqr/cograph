# Print method for cograph_degree_fit

Displays the comparison table of fitted distributions sorted by AIC.

## Usage

``` r
# S3 method for class 'cograph_degree_fit'
print(x, digits = 4, ...)
```

## Arguments

- x:

  A `cograph_degree_fit` object from
  [`fit_degree_distribution`](https://sonsoles.me/cograph/reference/fit_degree_distribution.md).

- digits:

  Number of decimal places. Default 4.

- ...:

  Additional arguments passed to `print.data.frame`.

## Value

Invisible `x`.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 0, 0,
                1, 0, 1, 1, 0,
                1, 1, 0, 1, 1,
                0, 1, 1, 0, 1,
                0, 0, 1, 1, 0), 5, 5, byrow = TRUE)
fit <- cograph::fit_degree_distribution(adj,
  distributions = c("exponential", "poisson"))
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
print(fit)
#> Degree Distribution Fit
#> =======================
#> N degrees: 5 
#> Best fit:  poisson 
#> 
#> Comparison (sorted by AIC):
#>  distribution     aic     bic ks_stat   ks_p
#>       poisson 17.4664 17.0758  0.4695 0.2205
#>   exponential 22.2962 21.9056  0.5105 0.1476
#> 
#> Fitted parameters:
#>   exponential: lambda = 0.3571
#>   poisson: lambda = 2.8000
```
