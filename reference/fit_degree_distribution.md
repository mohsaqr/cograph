# Fit Statistical Distributions to Degree Sequence

Fits one or more statistical distributions to the degree sequence of a
network by maximum likelihood and evaluates goodness of fit with
Kolmogorov-Smirnov statistics. The comparison table is sorted by AIC.

## Usage

``` r
fit_degree_distribution(
  x,
  distributions = NULL,
  mode = "all",
  directed = NULL,
  xmin = NULL,
  ...
)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna
  object. Without igraph installed, only a numeric matrix is accepted.

- distributions:

  Character vector of distributions to fit. Options: `"power_law"`,
  `"exponential"`, `"poisson"`, `"geometric"`. Default `NULL` fits all
  four. An unknown name raises an error.

- mode:

  For directed networks: `"all"`, `"in"`, or `"out"`. Determines which
  degree to extract. Default `"all"`.

- directed:

  Logical or NULL. If NULL (default), auto-detect from matrix symmetry.
  Set TRUE to force directed, FALSE to force undirected.

- xmin:

  Minimum degree to include in fitting. For the power law, NULL triggers
  automatic estimation (Clauset et al. 2009 via igraph). For the other
  distributions, NULL defaults to 1. Each fit requires at least two
  degrees at or above `xmin` and raises an error otherwise.

- ...:

  Additional arguments (currently unused).

## Value

An object of class `"cograph_degree_fit"`, a list containing:

- fits:

  Named list, one entry per distribution, each with `distribution`,
  `parameters` (named list of fitted parameters), `loglik`, `aic`,
  `bic`, `ks_stat` and `ks_p`. The parameters are `alpha` and `xmin` for
  the power law, `lambda` for the exponential and Poisson fits, and `p`
  for the geometric fit.

- comparison:

  Data frame sorted by AIC with columns `distribution`, `aic`, `bic`,
  `ks_stat`, `ks_p`.

- best:

  Name of the best-fitting distribution (lowest AIC).

- degree:

  The named degree vector used for fitting.

## Details

The power-law model (Pareto type I) is \\P(k) \sim k^{-\alpha}\\. With
igraph available and `xmin = NULL`, it is fitted with
[`igraph::fit_power_law()`](https://r.igraph.org/reference/fit_power_law.html),
which implements the Clauset et al. (2009) method. Otherwise the simple
MLE \\\alpha = 1 + n / \sum \log(k / k\_{min})\\ is computed, with
\\k\_{min}\\ equal to the supplied `xmin` or, without igraph, to the
smallest degree (at least 1).

The exponential model is \\P(k) \sim e^{-\lambda k}\\, with MLE
\\\lambda = 1 / \bar{k}\\.

The Poisson model is \\P(k) \sim \lambda^k e^{-\lambda} / k!\\, with MLE
\\\lambda = \bar{k}\\.

The geometric model is \\P(k) \sim (1-p)^k p\\, with MLE \\p = 1 / (1 +
\bar{k})\\.

`ks_p` comes from
[`stats::ks.test()`](https://rdrr.io/r/stats/ks.test.html) for the
exponential and Poisson fits and is `NA` for the power-law and geometric
fits. The p-values are approximate because the test assumes a continuous
distribution. For the exponential fit
[`stats::ks.test()`](https://rdrr.io/r/stats/ks.test.html) warns when
degrees are tied. A power-law fit to degrees that all equal `xmin` is
degenerate and returns `NA` for `alpha`, the likelihood and every
statistic.

Each likelihood is computed over the degrees at or above the `xmin` of
that fit. With `xmin = NULL`, the power-law fit uses the estimated
`xmin` and the other fits use 1, so their AIC values are computed on
different subsets of the degrees. AIC and BIC count one free parameter
per distribution, so the power-law `xmin` is not penalized.

## Printing and plotting

Printing the result shows the best-fitting distribution, the
`comparison` table and the fitted parameters of each distribution.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the result
is documented in
[`plot-results`](https://sonsoles.me/cograph/reference/plot-results.md).

## References

Clauset, A., Shalizi, C. R., & Newman, M. E. J. (2009). Power-law
distributions in empirical data. *SIAM Review*, 51(4), 661–703.

## See also

[`degree_distribution`](https://sonsoles.me/cograph/reference/degree_distribution.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md)

## Examples

``` r
fit_degree_distribution(regulation_net,
  distributions = c("poisson", "geometric"))
#> Degree Distribution Fit
#> =======================
#> N degrees: 10 
#> Best fit:  poisson 
#> 
#> Comparison (sorted by AIC):
#>  distribution     aic     bic ks_stat   ks_p
#>       poisson 40.4388 40.7414  0.3457 0.1831
#>     geometric 59.4163 59.7189  0.5373     NA
#> 
#> Fitted parameters:
#>   poisson: lambda = 6.0000
#>   geometric: p = 0.1429
```
