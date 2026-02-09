# ============================================================================
# WTNA: Window-based Transition Network Analysis
#
# Builds network models from one-hot encoded sequential data.
# Fully vectorized - no R loops. Compatible with tna/cograph ecosystem.
#
# Usage: source("experimental_ona.R")
# ============================================================================

library(dplyr)

#' Window-based Transition Network Analysis
#'
#' Builds a transition network from one-hot encoded data where multiple
#' codes can be active per window. Uses vectorized matrix operations.
#'
#' @param data Data frame with one-hot encoded columns (0/1 values)
#' @param method Network type:
#'   - "transition": directed edges from window t to t+1 (ONA-style)
#'   - "cooccurrence": undirected edges within same window (ENA-style)
#'   - "both": returns list with both networks
#' @param type Weight normalization:
#'   - "frequency": raw counts
#'   - "relative": row-normalized (transition probabilities)
#' @param codes Column names to use (auto-detected if NULL)
#' @param window_size Aggregate this many rows per window (default 1)
#' @param actor Column name for grouping by actor/unit (optional
#'
#' @return A tna-compatible object with $weights, $inits, $labels, $data
#'
#' @examples
#' # One-hot data
#' data <- data.frame(
#'   A = c(1, 1, 0, 0),
#'   B = c(0, 1, 1, 0),
#'   C = c(0, 0, 1, 1)
#' )
#'
#' # Transition network
#' wtna(data, method = "transition")
#'
#' # Co-occurrence network
#' wtna(data, method = "cooccurrence")
#'
#' # Both at once
#' wtna(data, method = "both")
#'
#' @export
wtna <- function(data,
                 method = c("transition", "cooccurrence", "both"),
                 type = c("frequency", "relative"),
                 codes = NULL,
                 window_size = 1L,
                 actor = NULL) {

  method <- match.arg(method)
  type <- match.arg(type)

  # Prepare data
  df <- as.data.frame(data)
  codes <- codes %||% .auto_detect_codes(df)
  n_codes <- length(codes)

  # Build weight matrix/matrices
  if (is.null(actor)) {
    X <- .to_matrix(df, codes, window_size)
    weights <- .compute_weights(X, method)
  } else {
    weights <- .compute_by_actor(df, codes, window_size, actor, method)
  }

  # Handle "both" method
  if (method == "both") {
    return(list(
      transition = .finalize(weights$transition, type, codes, data, "transition"),
      cooccurrence = .finalize(weights$cooccurrence, type, codes, data, "cooccurrence")
    ))
  }

  .finalize(weights, type, codes, data, method)
}

# ============================================================================
# VECTORIZED CORE COMPUTATIONS
# ============================================================================

# Transition: t(X[1:n-1,]) %*% X[2:n,]
.transitions <- function(X) {
  n <- nrow(X)
  if (n < 2L) return(matrix(0, ncol(X), ncol(X)))
  crossprod(X[-n, , drop = FALSE], X[-1L, , drop = FALSE])
}

# Co-occurrence: t(X) %*% X
.cooccurrence <- function(X) {
  crossprod(X)
}

# Dispatch by method
.compute_weights <- function(X, method) {
  switch(method,
    transition = .transitions(X),
    cooccurrence = .cooccurrence(X),
    both = list(transition = .transitions(X), cooccurrence = .cooccurrence(X))
  )
}

# ============================================================================
# DATA PREPARATION (VECTORIZED)
# ============================================================================

# Auto-detect one-hot columns
.auto_detect_codes <- function(df) {
  is_onehot <- vapply(df, function(x) {
    is.numeric(x) && all(x %in% c(0L, 1L, NA))
  }, logical(1L))

  codes <- names(df)[is_onehot]
  if (length(codes) == 0L) stop("No one-hot columns found. Specify 'codes'.")
  codes
}
# Convert to matrix with optional window aggregation
.to_matrix <- function(df, codes, window_size) {
  if (window_size > 1L) {
    df <- .aggregate_windows(df, codes, window_size)
  }
  as.matrix(df[, codes, drop = FALSE])
}

# Aggregate windows (vectorized via dplyr)
.aggregate_windows <- function(df, codes, window_size) {
  n <- nrow(df)
  wid <- (seq_len(n) - 1L) %/% window_size

  df |>
    mutate(..w.. = wid) |>
    summarise(across(all_of(codes), ~ as.integer(any(.x == 1L))), .by = ..w..) |>
    select(-..w..) |>
    as.data.frame()
}

# Compute by actor (vectorized aggregation)
.compute_by_actor <- function(df, codes, window_size, actor, method) {
  n_codes <- length(codes)
  init <- matrix(0, n_codes, n_codes)

  groups <- split(df, df[[actor]])

  matrices <- lapply(groups, function(g) {
    X <- .to_matrix(g, codes, window_size)
    .compute_weights(X, method)
  })

  if (method == "both") {
    list(
      transition = Reduce(`+`, lapply(matrices, `[[`, "transition"), init),
      cooccurrence = Reduce(`+`, lapply(matrices, `[[`, "cooccurrence"), init)
    )
  } else {
    Reduce(`+`, matrices, init)
  }
}

# ============================================================================
# OUTPUT
# ============================================================================

# Normalize and build tna object
.finalize <- function(weights, type, codes, data, method) {
  # Row-normalize if relative
  if (type == "relative") {
    rs <- rowSums(weights)
    rs[rs == 0] <- 1
    weights <- weights / rs
  }

  dimnames(weights) <- list(codes, codes)

  # Initial probabilities from first row
  X <- as.matrix(as.data.frame(data)[1L, codes, drop = FALSE])
  inits <- if (sum(X) > 0) as.numeric(X / sum(X)) else rep(1 / length(codes), length(codes))
  names(inits) <- codes

  # Transition = directed, Cooccurrence = undirected
  directed <- method == "transition"

  structure(
    list(weights = weights, inits = inits, labels = codes, data = data, directed = directed),
    class = "tna",
    type = type,
    directed = directed
  )
}

# Null coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a

# ============================================================================
# TESTS
# ============================================================================

cat("\n========== WTNA TESTS ==========\n\n")

test <- data.frame(
  A = c(1, 1, 0, 0),
  B = c(0, 1, 1, 0),
  C = c(0, 0, 1, 1)
)

