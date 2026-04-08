#' Fit Statistical Distributions to Degree Sequence
#'
#' Fits one or more statistical distributions to the degree sequence of a
#' network via maximum likelihood estimation and evaluates goodness-of-fit
#' using Kolmogorov-Smirnov tests. Returns a comparison table sorted by AIC.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna
#'   object.
#' @param distributions Character vector of distributions to fit. Options:
#'   \code{"power_law"}, \code{"exponential"}, \code{"poisson"},
#'   \code{"geometric"}. Default \code{NULL} fits all four.
#' @param mode For directed networks: \code{"all"}, \code{"in"}, or
#'   \code{"out"}. Determines which degree to extract. Default \code{"all"}.
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#' @param xmin Minimum degree to include in fitting. For power-law, NULL
#'   triggers automatic estimation (Clauset et al. 2009 via igraph). For other
#'   distributions, NULL defaults to 1.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class \code{"cograph_degree_fit"} containing:
#'   \describe{
#'     \item{fits}{Named list, one entry per distribution, each with:
#'       \code{distribution}, \code{parameters} (named list of fitted params),
#'       \code{loglik}, \code{aic}, \code{bic}, \code{ks_stat}, \code{ks_p}.}
#'     \item{comparison}{Data frame sorted by AIC with columns:
#'       \code{distribution}, \code{aic}, \code{bic}, \code{ks_stat},
#'       \code{ks_p}.}
#'     \item{best}{Name of the best-fitting distribution (lowest AIC).}
#'     \item{degree}{The degree vector used for fitting.}
#'   }
#'
#' @details
#' **Power-law** (Pareto Type I): \eqn{P(k) \sim k^{-\alpha}}. When igraph is
#' available, uses \code{igraph::fit_power_law()} implementing the Clauset
#' et al. (2009) method. Otherwise, computes the simple MLE:
#' \eqn{\alpha = 1 + n / \sum \log(k / k_{min})}.
#'
#' **Exponential**: \eqn{P(k) \sim e^{-\lambda k}}. MLE:
#' \eqn{\lambda = 1 / \bar{k}}.
#'
#' **Poisson**: \eqn{P(k) \sim \lambda^k e^{-\lambda} / k!}. MLE:
#' \eqn{\lambda = \bar{k}}. Note: the KS test uses a continuous approximation
#' for a discrete distribution; p-values are approximate.
#'
#' **Geometric**: \eqn{P(k) \sim (1-p)^k p}. MLE:
#' \eqn{p = 1 / (1 + \bar{k})}.
#'
#' @references
#' Clauset, A., Shalizi, C. R., & Newman, M. E. J. (2009). Power-law
#' distributions in empirical data. \emph{SIAM Review}, 51(4), 661--703.
#'
#' @seealso \code{\link{degree_distribution}}, \code{\link{centrality}}
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 0, 0,
#'                 1, 0, 1, 1, 0,
#'                 1, 1, 0, 1, 1,
#'                 0, 1, 1, 0, 1,
#'                 0, 0, 1, 1, 0), 5, 5, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:5]
#' fit <- cograph::fit_degree_distribution(adj,
#'   distributions = c("exponential", "poisson"))
#' print(fit)
fit_degree_distribution <- function(x,
                                    distributions = NULL,
                                    mode = "all",
                                    directed = NULL,
                                    xmin = NULL,
                                    ...) {

  mode <- match.arg(mode, c("all", "in", "out"))

  all_dists <- c("power_law", "exponential", "poisson", "geometric")

  if (is.null(distributions)) {
    distributions <- all_dists
  } else {
    stopifnot(is.character(distributions), length(distributions) >= 1L)
    bad <- setdiff(distributions, all_dists)
    if (length(bad) > 0L) {
      stop("Unknown distribution(s): ", paste(bad, collapse = ", "),
           ". Choose from: ", paste(all_dists, collapse = ", "),
           call. = FALSE)
    }
  }

  # Convert to igraph and extract degree
  has_igraph <- requireNamespace("igraph", quietly = TRUE)

  if (has_igraph) {
    g <- to_igraph(x, directed = directed)
    deg <- igraph::degree(g, mode = mode)
  } else {
    # Fallback: accept only a numeric matrix
    stopifnot(is.matrix(x), is.numeric(x))
    is_sym <- isSymmetric(unname(x))
    deg <- if (mode == "in") {
      colSums(x > 0)
    } else if (mode == "out") {
      rowSums(x > 0)
    } else if (is_sym) {
      rowSums(x > 0)  # undirected: don't double-count
    } else {
      rowSums(x > 0) + colSums(x > 0)
    }
  }

  # Fit each requested distribution
  fits <- lapply(distributions, function(d) {
    switch(d,
      power_law  = .fit_power_law(deg, xmin, has_igraph),
      exponential = .fit_exponential(deg, xmin),
      poisson     = .fit_poisson(deg, xmin),
      geometric   = .fit_geometric(deg, xmin)
    )
  })
  names(fits) <- distributions

  # Build comparison table
  comparison <- data.frame(
    distribution = vapply(fits, `[[`, character(1), "distribution"),
    aic          = vapply(fits, `[[`, numeric(1), "aic"),
    bic          = vapply(fits, `[[`, numeric(1), "bic"),
    ks_stat      = vapply(fits, `[[`, numeric(1), "ks_stat"),
    ks_p         = vapply(fits, `[[`, numeric(1), "ks_p"),
    stringsAsFactors = FALSE
  )
  comparison <- comparison[order(comparison$aic), ]
  rownames(comparison) <- NULL

  best <- comparison$distribution[1L]

  result <- list(
    fits       = fits,
    comparison = comparison,
    best       = best,
    degree     = deg
  )
  class(result) <- "cograph_degree_fit"
  result
}


# ============================================================================
# Internal fitters
# ============================================================================

#' Build standardized fit result with AIC/BIC
#' @noRd
.make_fit_result <- function(distribution, parameters, loglik, n,
                             n_params = 1L, ks_stat = NA_real_,
                             ks_p = NA_real_) {
  list(
    distribution = distribution,
    parameters = parameters,
    loglik = loglik,
    aic = -2 * loglik + 2 * n_params,
    bic = -2 * loglik + n_params * log(n),
    ks_stat = ks_stat,
    ks_p = ks_p
  )
}

#' Fit power-law distribution
#' @noRd
.fit_power_law <- function(deg, xmin, has_igraph) {
  # Filter to xmin
  if (has_igraph && is.null(xmin)) {
    # Clauset et al. automatic xmin estimation
    pl <- igraph::fit_power_law(deg)
    alpha   <- pl$alpha
    xmin_used <- pl$xmin
    ks_stat <- pl$KS.stat
    ks_p    <- if (!is.null(pl$KS.p)) pl$KS.p else NA_real_
    k <- deg[deg >= xmin_used]
    n <- length(k)
    loglik <- if (n > 0L) {
      n * log(alpha - 1) + n * (alpha - 1) * log(xmin_used) -
        alpha * sum(log(k))
    } else {
      NA_real_
    }
  } else {
    # Manual MLE
    xmin_used <- if (is.null(xmin)) max(1L, min(deg)) else xmin
    k <- deg[deg >= xmin_used]
    n <- length(k)
    stopifnot(n > 1L)
    log_sum <- sum(log(k / xmin_used))
    if (log_sum == 0) {
      # All degrees equal xmin — power-law MLE is degenerate
      return(list(distribution = "power_law",
                  parameters = list(alpha = NA_real_, xmin = xmin_used),
                  loglik = NA_real_, aic = NA_real_, bic = NA_real_,
                  ks_stat = NA_real_, ks_p = NA_real_))
    }
    alpha <- 1 + n / log_sum
    loglik <- n * log(alpha - 1) + n * (alpha - 1) * log(xmin_used) -
      alpha * sum(log(k))
    # KS test against theoretical CDF: F(x) = 1 - (xmin/x)^(alpha-1)
    empirical <- sort(k)
    theoretical_cdf <- 1 - (xmin_used / empirical) ^ (alpha - 1)
    ecdf_vals <- seq_len(n) / n
    ecdf_left <- c(0, ecdf_vals[-n])
    ks_stat <- max(max(ecdf_vals - theoretical_cdf),
                   max(theoretical_cdf - ecdf_left))
    ks_p <- NA_real_
  }

  .make_fit_result("power_law", list(alpha = alpha, xmin = xmin_used),
                   loglik, n, ks_stat = ks_stat, ks_p = ks_p)
}


#' Fit exponential distribution
#' @noRd
.fit_exponential <- function(deg, xmin) {
  xmin_used <- if (is.null(xmin)) 1L else xmin
  k <- deg[deg >= xmin_used]
  n <- length(k)
  stopifnot(n > 1L)

  lambda <- 1 / mean(k)

  # Log-likelihood: sum(log(lambda * exp(-lambda * k)))
  loglik <- n * log(lambda) - lambda * sum(k)

  # KS test
  ks <- tryCatch(
    stats::ks.test(k, "pexp", rate = lambda),
    error = function(e) NULL
  )

  .make_fit_result("exponential", list(lambda = lambda), loglik, n,
                   ks_stat = if (!is.null(ks)) unname(ks$statistic) else NA_real_,
                   ks_p = if (!is.null(ks)) ks$p.value else NA_real_)
}


#' Fit Poisson distribution
#' @noRd
.fit_poisson <- function(deg, xmin) {
  xmin_used <- if (is.null(xmin)) 1L else xmin
  k <- deg[deg >= xmin_used]
  n <- length(k)
  stopifnot(n > 1L)

  lambda <- mean(k)

  # Log-likelihood: sum(k * log(lambda) - lambda - log(k!))
  loglik <- sum(k * log(lambda) - lambda - lgamma(k + 1))

  # KS test (note: approximate for discrete distribution)
  ks <- tryCatch({
    suppressWarnings(stats::ks.test(k, "ppois", lambda = lambda))
  }, error = function(e) NULL)

  .make_fit_result("poisson", list(lambda = lambda), loglik, n,
                   ks_stat = if (!is.null(ks)) unname(ks$statistic) else NA_real_,
                   ks_p = if (!is.null(ks)) ks$p.value else NA_real_)
}


#' Fit geometric distribution
#' @noRd
.fit_geometric <- function(deg, xmin) {
  xmin_used <- if (is.null(xmin)) 1L else xmin
  k <- deg[deg >= xmin_used]
  n <- length(k)
  stopifnot(n > 1L)

  p <- 1 / (1 + mean(k))

  # Log-likelihood: sum(log(p) + k * log(1 - p))
  loglik <- n * log(p) + sum(k) * log(1 - p)

  # KS test via custom CDF: F(k) = 1 - (1-p)^(k+1)
  k_sorted <- sort(k)
  theoretical_cdf <- 1 - (1 - p) ^ (k_sorted + 1)
  ecdf_vals <- seq_len(n) / n
  ecdf_left <- c(0, ecdf_vals[-n])
  ks_stat <- max(max(ecdf_vals - theoretical_cdf),
                 max(theoretical_cdf - ecdf_left))

  .make_fit_result("geometric", list(p = p), loglik, n,
                   ks_stat = ks_stat)
}


# ============================================================================
# Print and plot methods
# ============================================================================

#' Print method for cograph_degree_fit
#'
#' Displays the comparison table of fitted distributions sorted by AIC.
#'
#' @param x A \code{cograph_degree_fit} object from
#'   \code{\link{fit_degree_distribution}}.
#' @param digits Number of decimal places. Default 4.
#' @param ... Additional arguments passed to \code{print.data.frame}.
#'
#' @return Invisible \code{x}.
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 0, 0,
#'                 1, 0, 1, 1, 0,
#'                 1, 1, 0, 1, 1,
#'                 0, 1, 1, 0, 1,
#'                 0, 0, 1, 1, 0), 5, 5, byrow = TRUE)
#' fit <- cograph::fit_degree_distribution(adj,
#'   distributions = c("exponential", "poisson"))
#' print(fit)
print.cograph_degree_fit <- function(x, digits = 4, ...) {
  cat("Degree Distribution Fit\n")
  cat("=======================\n")
  cat("N degrees:", length(x$degree), "\n")
  cat("Best fit: ", x$best, "\n\n")
  cat("Comparison (sorted by AIC):\n")

  comp <- x$comparison
  numeric_cols <- c("aic", "bic", "ks_stat", "ks_p")
  comp[numeric_cols] <- lapply(comp[numeric_cols], round, digits = digits)
  print(comp, row.names = FALSE, ...)

  cat("\nFitted parameters:\n")
  invisible(lapply(names(x$fits), function(nm) {
    fit <- x$fits[[nm]]
    params <- paste(
      names(fit$parameters),
      vapply(fit$parameters, function(v) format(round(v, digits), nsmall = digits),
             character(1)),
      sep = " = ",
      collapse = ", "
    )
    cat(sprintf("  %s: %s\n", nm, params))
  }))

  invisible(x)
}


#' Plot method for cograph_degree_fit
#'
#' Overlays fitted distribution curves on a histogram of observed degrees.
#'
#' @param x A \code{cograph_degree_fit} object from
#'   \code{\link{fit_degree_distribution}}.
#' @param which Character vector of distribution names to display. Default
#'   \code{NULL} shows all fitted distributions.
#' @param log Character string for log-scale axes: \code{""} (default),
#'   \code{"x"}, \code{"y"}, or \code{"xy"}.
#' @param cols Named or unnamed character vector of colors for distribution
#'   curves. Default uses a built-in palette.
#' @param lwd Line width for fitted curves. Default 2.
#' @param main Plot title. Default \code{"Degree Distribution Fit"}.
#' @param ... Additional arguments passed to \code{\link[graphics]{hist}}.
#'
#' @return Invisible \code{NULL}.
#' @export
#' @examples
#' \dontrun{
#' adj <- matrix(c(0, 1, 1, 0, 0,
#'                 1, 0, 1, 1, 0,
#'                 1, 1, 0, 1, 1,
#'                 0, 1, 1, 0, 1,
#'                 0, 0, 1, 1, 0), 5, 5, byrow = TRUE)
#' fit <- cograph::fit_degree_distribution(adj)
#' plot(fit)
#' plot(fit, which = c("exponential", "poisson"), log = "y")
#' }
plot.cograph_degree_fit <- function(x,
                                    which = NULL,
                                    log = "",
                                    cols = NULL,
                                    lwd = 2,
                                    main = "Degree Distribution Fit",
                                    ...) {

  stopifnot(log %in% c("", "x", "y", "xy"))

  deg <- x$degree
  fits <- x$fits

  if (!is.null(which)) {
    stopifnot(is.character(which))
    bad <- setdiff(which, names(fits))
    if (length(bad) > 0L) {
      stop("Distributions not fitted: ", paste(bad, collapse = ", "),
           call. = FALSE)
    }
    fits <- fits[which]
  }

  n_fits <- length(fits)
  dist_names <- names(fits)

  # Default color palette
  default_palette <- c(
    power_law   = "#E41A1C",
    exponential = "#377EB8",
    poisson     = "#4DAF4A",
    geometric   = "#984EA3"
  )

  if (is.null(cols)) {
    cols <- default_palette[dist_names]
  } else if (is.null(names(cols))) {
    # Unnamed vector: recycle to match number of fits
    cols <- rep_len(cols, n_fits)
    names(cols) <- dist_names
  }

  # Degree histogram (probability density)
  use_log <- log
  h <- graphics::hist(deg, plot = FALSE)
  ylab <- "Density"

  graphics::hist(deg,
                 freq = FALSE,
                 main = main,
                 xlab = "Degree",
                 ylab = ylab,
                 col = grDevices::adjustcolor("gray80", 0.6),
                 border = "white",
                 log = if (use_log %in% c("y", "xy")) "y" else "",
                 ...)

  # Overlay fitted density curves
  k_seq <- seq(max(1, min(deg)), max(deg), length.out = 200)

  invisible(lapply(dist_names, function(d) {
    fit <- fits[[d]]
    density_vals <- .degree_density(d, k_seq, fit$parameters)
    # Skip if all NA or zero
    if (all(is.na(density_vals) | density_vals <= 0)) return(NULL)
    if (use_log %in% c("x", "xy")) {
      keep <- k_seq > 0 & density_vals > 0 & !is.na(density_vals)
    } else {
      keep <- density_vals > 0 & !is.na(density_vals)
    }
    graphics::lines(k_seq[keep], density_vals[keep],
                    col = cols[d], lwd = lwd)
  }))

  # Legend
  graphics::legend("topright",
                   legend = dist_names,
                   col = cols[dist_names],
                   lwd = lwd,
                   bty = "n",
                   cex = 0.9)

  invisible(NULL)
}


#' Compute density values for a fitted distribution
#' @noRd
.degree_density <- function(dist, k, params) {
  switch(dist,
    power_law = {
      alpha <- params$alpha
      xmin  <- params$xmin
      # Pareto Type I density: (alpha - 1) / xmin * (k / xmin)^(-alpha)
      out <- ifelse(k >= xmin,
                    (alpha - 1) / xmin * (k / xmin) ^ (-alpha),
                    0)
      out
    },
    exponential = {
      lambda <- params$lambda
      stats::dexp(k, rate = lambda)
    },
    poisson = {
      lambda <- params$lambda
      stats::dpois(round(k), lambda = lambda)
    },
    geometric = {
      p <- params$p
      stats::dgeom(round(k), prob = p)
    },
    rep(NA_real_, length(k))
  )
}
