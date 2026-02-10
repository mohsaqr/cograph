#' @title Centrality Stability Analysis
#' @name stability
NULL

#' Estimate Centrality Stability (CS-Coefficient)
#'
#' @param x A tna object.
#' @param loops Include self-loops? Default FALSE.
#' @param normalize Normalize centrality? Default FALSE.
#' @param measures Centrality measures. Default c("InStrength", "OutStrength", "Betweenness").
#' @param iter Iterations per drop proportion. Default 1000.
#' @param method Correlation method. Default "pearson".
#' @param drop_prop Drop proportions to test. Default seq(0.1, 0.9, by = 0.1).
#' @param threshold Correlation threshold. Default 0.7.
#' @param certainty Certainty level. Default 0.95.
#' @param progressbar Show progress? Default FALSE.
#' @param ... Additional arguments.
#'
#' @return A tna_stability object.
#' @export
estimate_cs <- function(x, loops = FALSE, normalize = FALSE,
                        measures = c("InStrength", "OutStrength", "Betweenness"),
                        iter = 1000, method = "pearson",
                        drop_prop = seq(0.1, 0.9, by = 0.1),
                        threshold = 0.7, certainty = 0.95,
                        progressbar = FALSE, ...) {
  UseMethod("estimate_cs")
}

#' @rdname estimate_cs
#' @export
estimate_centrality_stability <- estimate_cs

#' @export
estimate_cs.default <- function(x, ...) {
  stop(
    "estimate_cs() requires a tna object with raw data.\n",
    "CS-coefficient measures how stable centrality is when dropping cases.",
    call. = FALSE
  )
}

#' @export
estimate_cs.tna <- function(x, loops = FALSE, normalize = FALSE,
                            measures = c("InStrength", "OutStrength", "Betweenness"),
                            iter = 1000, method = "pearson",
                            drop_prop = seq(0.1, 0.9, by = 0.1),
                            threshold = 0.7, certainty = 0.95,
                            progressbar = FALSE, ...) {
  if (is.null(x$data)) stop("TNA object needs $data for stability analysis.", call. = FALSE)

  d <- x$data
  type <- attr(x, "type") %||% "relative"
  scaling <- attr(x, "scaling") %||% "none"
  params <- attr(x, "params")

  model <- tna:::initialize_model(d, type, scaling, params, transitions = TRUE)
  trans <- model$trans
  a <- dim(trans)[2]
  n <- nrow(d)
  n_seq <- seq_len(n)
  n_prop <- length(drop_prop)

  # Original centralities
  cent_orig <- tna:::centralities_(x = x$weights, loops = loops, normalize = normalize, measures = measures)

  # Check which measures have variance
  sds_orig <- sapply(measures, function(m) stats::sd(cent_orig[[m]], na.rm = TRUE))
  valid <- which(sds_orig > 0)
  cent_orig <- cent_orig[, 1 + valid, drop = FALSE]
  measures <- measures[valid]
  n_measures <- length(measures)

  if (n_measures == 0) {
    warning("No measures with variance. Returning empty result.")
    return(structure(list(), class = "tna_stability", drop_prop = drop_prop, threshold = threshold))
  }

  # Pre-allocate stability matrices
  stability <- lapply(measures, function(m) matrix(NA, nrow = iter, ncol = n_prop))
  names(stability) <- measures

  # Case-dropping loop
  for (i in seq_len(n_prop)) {
    prop <- drop_prop[i]
    n_drop <- floor(n * prop)
    if (n_drop == 0) next

    for (j in seq_len(iter)) {
      keep <- sample(n_seq, n - n_drop, replace = FALSE)
      trans_sub <- trans[keep, , , drop = FALSE]
      weight_sub <- tna:::compute_weights(trans_sub, type, scaling, a)

      cent_sub <- tna:::centralities_(x = weight_sub, loops = loops, normalize = normalize, measures = measures)

      for (k in seq_len(n_measures)) {
        m <- measures[k]
        sd_sub <- stats::sd(cent_sub[[m]], na.rm = TRUE)
        if (sd_sub > 0) {
          stability[[m]][j, i] <- stats::cor(cent_sub[[m]], cent_orig[[m]],
                                             method = method, use = "complete.obs")
        }
      }

      if (progressbar && j %% 100 == 0) {
        cat(sprintf("\rdrop=%.1f iter=%d/%d", prop, j, iter))
      }
    }
  }
  if (progressbar) cat("\n")

  # Calculate CS-coefficients
  out <- list()
  for (m in measures) {
    corr_mat <- stability[[m]]
    prop_above <- apply(corr_mat, 2, function(x) mean(x >= threshold, na.rm = TRUE))
    valid_idx <- which(prop_above >= certainty)
    cs_coef <- if (length(valid_idx) > 0) drop_prop[max(valid_idx)] else 0

    out[[m]] <- list(cs_coefficient = cs_coef, correlations = corr_mat)
  }

  structure(out, class = "tna_stability", drop_prop = drop_prop, threshold = threshold)
}

#' @export
estimate_cs.group_tna <- function(x, loops = FALSE, normalize = FALSE,
                                  measures = c("InStrength", "OutStrength", "Betweenness"),
                                  iter = 1000, method = "pearson",
                                  drop_prop = seq(0.1, 0.9, by = 0.1),
                                  threshold = 0.7, certainty = 0.95,
                                  progressbar = FALSE, i = 1, ...) {
  estimate_cs.tna(x[[i]], loops, normalize, measures, iter, method, drop_prop, threshold, certainty, progressbar, ...)
}

#' @export
estimate_cs.cograph_network <- function(x, loops = FALSE, normalize = FALSE,
                                        measures = c("InStrength", "OutStrength", "Betweenness"),
                                        iter = 1000, method = "pearson",
                                        drop_prop = seq(0.1, 0.9, by = 0.1),
                                        threshold = 0.7, certainty = 0.95,
                                        progressbar = FALSE, ...) {
  if (is_tna_network(x)) {
    return(estimate_cs.tna(x$tna$model, loops, normalize, measures, iter, method, drop_prop, threshold, certainty, progressbar, ...))
  }
  stop(
    "Centrality stability requires raw case-level data to resample.\n",
    "Options:\n",
    "
- Use set_raw_data(net, data, estimator) to attach raw data\n",
    "
- Compute centrality directly with igraph or other packages\n",
    "See ?set_raw_data for details.",
    call. = FALSE
  )
}

#' @export
print.tna_stability <- function(x, ...) {
  cat("Centrality Stability (CS-Coefficients)\n")
  measures <- setdiff(names(x), c("drop_prop", "threshold"))
  for (m in measures) {
    cs <- x[[m]]$cs_coefficient
    cat(sprintf("  %s: %.2f\n", m, cs))
  }
  invisible(x)
}

#' @export
plot.tna_stability <- function(x, ...) {
  drop_prop <- attr(x, "drop_prop")
  threshold <- attr(x, "threshold")
  measures <- setdiff(names(x), c("drop_prop", "threshold"))
  colors <- c(InStrength = "#2196F3", OutStrength = "#4CAF50", Betweenness = "#FF9800")

  plot(NULL, xlim = c(0, max(drop_prop)), ylim = c(0, 1),
       xlab = "Proportion dropped", ylab = "Correlation",
       main = "Centrality Stability")
  abline(h = threshold, lty = 2, col = "gray50")

  for (m in measures) {
    mean_cor <- colMeans(x[[m]]$correlations, na.rm = TRUE)
    col <- colors[m] %||% "black"
    lines(drop_prop, mean_cor, col = col, lwd = 2)
    points(x[[m]]$cs_coefficient, threshold, pch = 19, col = col, cex = 1.5)
  }

  legend("bottomleft",
         legend = paste0(measures, " (", sprintf("%.2f", sapply(measures, function(m) x[[m]]$cs_coefficient)), ")"),
         col = colors[measures], lwd = 2, bty = "n")
}

`%||%` <- function(a, b) if (is.null(a)) b else a
