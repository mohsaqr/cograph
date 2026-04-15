# Fit Statistical Distributions to Degree Sequence

Fits one or more statistical distributions to the degree sequence of a
network via maximum likelihood estimation and evaluates goodness-of-fit
using Kolmogorov-Smirnov tests. Returns a comparison table sorted by
AIC.

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
  object.

- distributions:

  Character vector of distributions to fit. Options: `"power_law"`,
  `"exponential"`, `"poisson"`, `"geometric"`. Default `NULL` fits all
  four.

- mode:

  For directed networks: `"all"`, `"in"`, or `"out"`. Determines which
  degree to extract. Default `"all"`.

- directed:

  Logical or NULL. If NULL (default), auto-detect from matrix symmetry.
  Set TRUE to force directed, FALSE to force undirected.

- xmin:

  Minimum degree to include in fitting. For power-law, NULL triggers
  automatic estimation (Clauset et al. 2009 via igraph). For other
  distributions, NULL defaults to 1.

- ...:

  Additional arguments (currently unused).

## Value

An object of class `"cograph_degree_fit"` containing:

- fits:

  Named list, one entry per distribution, each with: `distribution`,
  `parameters` (named list of fitted params), `loglik`, `aic`, `bic`,
  `ks_stat`, `ks_p`.

- comparison:

  Data frame sorted by AIC with columns: `distribution`, `aic`, `bic`,
  `ks_stat`, `ks_p`.

- best:

  Name of the best-fitting distribution (lowest AIC).

- degree:

  The degree vector used for fitting.

## Details

**Power-law** (Pareto Type I): \\P(k) \sim k^{-\alpha}\\. When igraph is
available, uses
[`igraph::fit_power_law()`](https://r.igraph.org/reference/fit_power_law.html)
implementing the Clauset et al. (2009) method. Otherwise, computes the
simple MLE: \\\alpha = 1 + n / \sum \log(k / k\_{min})\\.

**Exponential**: \\P(k) \sim e^{-\lambda k}\\. MLE: \\\lambda = 1 /
\bar{k}\\.

**Poisson**: \\P(k) \sim \lambda^k e^{-\lambda} / k!\\. MLE: \\\lambda =
\bar{k}\\. Note: the KS test uses a continuous approximation for a
discrete distribution; p-values are approximate.

**Geometric**: \\P(k) \sim (1-p)^k p\\. MLE: \\p = 1 / (1 + \bar{k})\\.

## References

Clauset, A., Shalizi, C. R., & Newman, M. E. J. (2009). Power-law
distributions in empirical data. *SIAM Review*, 51(4), 661–703.

## See also

[`degree_distribution`](https://sonsoles.me/cograph/reference/degree_distribution.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md)

## Examples

``` r
adj <- matrix(c(0, 1, 1, 0, 0,
                1, 0, 1, 1, 0,
                1, 1, 0, 1, 1,
                0, 1, 1, 0, 1,
                0, 0, 1, 1, 0), 5, 5, byrow = TRUE)
rownames(adj) <- colnames(adj) <- LETTERS[1:5]
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
