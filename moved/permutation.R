#' @title Permutation Test for Network Comparison
#' @name permutation
NULL

#' Permutation Test
#'
#' @param x First tna object.
#' @param y Second tna object.
#' @param adjust P-value adjustment method. Default "none".
#' @param iter Number of iterations. Default 1000.
#' @param paired Logical. Paired test? Default FALSE.
#' @param level Significance level. Default 0.05.
#' @param measures Centrality measures to compare.
#' @param ... Additional arguments.
#'
#' @return A tna_permutation object.
#' @export
permutation_test <- function(x, y, adjust = "none", iter = 1000, paired = FALSE,
                             level = 0.05, measures = character(0), ...) {
  UseMethod("permutation_test")
}

#' @export
permutation_test.default <- function(x, ...) {
  stop(
    "permutation_test() requires tna objects with raw data.\n",
    "For visual comparison without p-values, use plot_compare().",
    call. = FALSE
  )
}
#' @export
permutation_test.tna <- function(x, y, adjust = "none", iter = 1000, paired = FALSE,
                                 level = 0.05, measures = character(0), ...) {
  if (!inherits(y, "tna")) stop("y must be a tna object.", call. = FALSE)
  if (is.null(x$data) || is.null(y$data)) stop("Both objects need $data.", call. = FALSE)

  data_x <- x$data
  data_y <- y$data
  n_x <- nrow(data_x)
  n_y <- nrow(data_y)

  if (paired && n_x != n_y) stop("Paired test requires equal sample sizes.", call. = FALSE)

  alph_x <- attr(data_x, "alphabet")
  alph_y <- attr(data_y, "alphabet")
  a <- length(alph_x)

  if (a != length(alph_y)) stop("Networks must have same number of states.", call. = FALSE)
  if (!all(alph_x == alph_y)) stop("State labels must match.", call. = FALSE)

  # Combine data
  combined_data <- rbind(data_x, data_y)
  attr(combined_data, "alphabet") <- alph_x
  attr(combined_data, "labels") <- attr(data_x, "labels")
  n_xy <- n_x + n_y

  weights_x <- x$weights
  weights_y <- y$weights
  type <- attr(x, "type") %||% "relative"
  scaling <- attr(x, "scaling") %||% "none"
  params <- attr(x, "params")

  # Centralities
  n_measures <- length(measures)
  include_cent <- n_measures > 0
  if (include_cent) {
    cent_x <- tna::centralities(x, measures = measures, ...)
    cent_y <- tna::centralities(y, measures = measures, ...)
    cent_diffs_true <- as.matrix(cent_x[, -1] - cent_y[, -1])
    cent_diffs_true_abs <- abs(cent_diffs_true)
  }

  edge_diffs_true <- weights_x - weights_y
  edge_diffs_true_abs <- abs(edge_diffs_true)

  idx_x <- seq_len(n_x)
  idx_y <- seq(n_x + 1, n_xy)

  # Get combined transitions
  combined_model <- tna:::initialize_model(combined_data, type, scaling, params, transitions = TRUE)
  combined_trans <- combined_model$trans

  # Pre-allocate
  edge_diffs_perm <- array(0, dim = c(iter, a, a))
  cent_diffs_perm <- if (include_cent) array(0, dim = c(iter, a, n_measures)) else NULL
  edge_p_values <- matrix(0L, a, a)
  cent_p_values <- if (include_cent) matrix(0L, a, n_measures) else NULL

  # Permutation loop
  for (i in seq_len(iter)) {
    if (paired) {
      pair_idx <- matrix(seq_len(n_xy), ncol = 2)
      perm_idx <- c(t(apply(pair_idx, 1, sample)))
    } else {
      perm_idx <- sample(n_xy)
    }

    trans_perm_x <- combined_trans[perm_idx[idx_x], , , drop = FALSE]
    trans_perm_y <- combined_trans[perm_idx[idx_y], , , drop = FALSE]

    weights_perm_x <- tna:::compute_weights(trans_perm_x, type, scaling, a)
    weights_perm_y <- tna:::compute_weights(trans_perm_y, type, scaling, a)

    if (include_cent) {
      cent_perm_x <- tna::centralities(weights_perm_x, measures = measures, ...)
      cent_perm_y <- tna::centralities(weights_perm_y, measures = measures, ...)
      cent_diffs_perm[i, , ] <- as.matrix(cent_perm_x[, -1] - cent_perm_y[, -1])
      cent_p_values <- cent_p_values + 1L * (abs(cent_diffs_perm[i, , ]) >= cent_diffs_true_abs)
    }

    edge_diffs_perm[i, , ] <- weights_perm_x - weights_perm_y
    edge_p_values <- edge_p_values + 1L * (abs(edge_diffs_perm[i, , ]) >= edge_diffs_true_abs)
  }

  # P-values
  edge_p_values <- (edge_p_values + 1) / (iter + 1)
  edge_p_values[, ] <- stats::p.adjust(edge_p_values, method = adjust)

  edge_diffs_sd <- apply(edge_diffs_perm, c(2, 3), stats::sd)
  edge_diffs_sig <- edge_diffs_true * (edge_p_values < level)

  edge_names <- expand.grid(from = rownames(weights_x), to = colnames(weights_y))
  edge_names <- paste0(edge_names$from, " -> ", edge_names$to)

  edge_stats <- data.frame(
    edge_name = edge_names,
    diff_true = c(edge_diffs_true),
    effect_size = c(edge_diffs_true) / c(edge_diffs_sd),
    p_value = c(edge_p_values)
  )

  out <- list(
    edges = list(stats = edge_stats, diffs_true = edge_diffs_true, diffs_sig = edge_diffs_sig)
  )

  if (include_cent) {
    cent_p_values <- (cent_p_values + 1) / (iter + 1)
    cent_p_values[, ] <- stats::p.adjust(cent_p_values, method = adjust)
    cent_diffs_sd <- apply(cent_diffs_perm, c(2, 3), stats::sd)
    cent_diffs_sig <- cent_diffs_true * (cent_p_values < level)

    cent_stats <- expand.grid(state = cent_x$state, centrality = measures)
    cent_stats$diff_true <- c(cent_diffs_true)
    cent_stats$effect_size <- c(cent_diffs_true) / c(cent_diffs_sd)
    cent_stats$p_value <- c(cent_p_values)

    out$centralities <- list(
      stats = cent_stats,
      diffs_true = cbind(data.frame(state = cent_x$state), as.data.frame(cent_diffs_true)),
      diffs_sig = cbind(data.frame(state = cent_x$state), as.data.frame(cent_diffs_sig))
    )
  }

  structure(out, labels = x$labels, level = level, class = "tna_permutation")
}

#' @export
permutation_test.group_tna <- function(x, y = NULL, adjust = "none", iter = 1000,
                                       paired = FALSE, level = 0.05,
                                       measures = character(0), i = 1, j = 2, ...) {
  if (is.null(y)) {
    tna_x <- x[[i]]
    tna_y <- x[[j]]
  } else {
    tna_x <- x[[1]]
    tna_y <- if (inherits(y, "group_tna")) y[[1]] else y
  }
  permutation_test.tna(tna_x, tna_y, adjust, iter, paired, level, measures, ...)
}

#' @export
permutation_test.cograph_network <- function(x, y, adjust = "none", iter = 1000,
                                             paired = FALSE, level = 0.05,
                                             measures = character(0), ...) {
  if (is_tna_network(x) && is_tna_network(y)) {
    return(permutation_test.tna(x$tna$model, y$tna$model, adjust, iter, paired, level, measures, ...))
  }
  stop(
    "Permutation test requires raw case-level data to resample.\n",
    "Options:\n",
    "
- Use set_raw_data(net, data, estimator) to attach raw data\n",
    "
- Use plot_compare() for visual comparison without p-values\n",
    "See ?set_raw_data for details.",
    call. = FALSE
  )
}

#' @export
print.tna_permutation <- function(x, ...) {
  cat("Permutation Test\n")
  n_sig <- sum(x$edges$diffs_sig != 0)
  n_total <- sum(x$edges$diffs_true != 0)
  cat("Significant edge differences:", n_sig, "/", n_total, "\n")
  invisible(x)
}

#' @export
plot.tna_permutation <- function(x, ...) {
  splot(x$edges$diffs_sig, ...)
}

#' @export
splot.tna_permutation <- function(x, show = c("styled", "significant", "full"),
                                  edge_style_sig = 1, edge_style_nonsig = 2,
                                  alpha_nonsig = 0.3, ...) {
  show <- match.arg(show)
  level <- attr(x, "level") %||% 0.05

  if (show == "significant") {
    splot(x$edges$diffs_sig, ...)
  } else if (show == "full") {
    splot(x$edges$diffs_true, ...)
  } else {
    diffs <- x$edges$diffs_true
    sig_mask <- x$edges$diffs_sig != 0
    a <- nrow(diffs)

    edge_styles <- matrix(edge_style_nonsig, a, a)
    edge_styles[sig_mask] <- edge_style_sig
    edge_alphas <- matrix(alpha_nonsig, a, a)
    edge_alphas[sig_mask] <- 1

    edge_idx <- which(diffs != 0, arr.ind = TRUE)
    if (nrow(edge_idx) > 0) {
      args <- list(...)
      args$edge_style <- edge_styles[edge_idx]
      args$edge_alpha <- edge_alphas[edge_idx]
      do.call(splot, c(list(x = diffs), args))
    } else {
      splot(diffs, ...)
    }
  }
}

`%||%` <- function(a, b) if (is.null(a)) b else a
