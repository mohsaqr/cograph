# Numerical kernels for the first parameter-candidate expansion.

#' Local brokerage and supported relationships on a simple graph
#' @param b Symmetric binary adjacency matrix with zero diagonal.
#' @param measure One of bridging_coefficient, godfather, support.
#' @return Numeric vector in vertex order.
#' @keywords internal
#' @noRd
.cg_local_candidates <- function(b, measure) {
  deg <- rowSums(b)
  if (measure == "bridging_coefficient") {
    inv <- numeric(length(deg))
    inv[deg > 0] <- 1 / deg[deg > 0]
    denom <- as.numeric(b %*% inv)
    out <- numeric(length(deg))
    out[denom > 0] <- inv[denom > 0] / denom[denom > 0]
    return(out)
  }
  common <- b %*% b
  if (measure == "support") return(as.numeric(rowSums(b != 0 & common > 0)))
  # Each edge between two neighbours participates in one focal triangle.
  as.numeric(deg * (deg - 1) / 2 - rowSums(b * common) / 2)
}

#' Node truss number with the k-minus-two triangle convention
#' @param b Symmetric binary adjacency matrix with zero diagonal.
#' @return Numeric vector; isolates have value zero, tree vertices two.
#' @keywords internal
#' @noRd
.cg_truss <- function(b) {
  out <- numeric(nrow(b))
  k <- 2L
  while (any(b != 0)) {
    repeat {
      drop <- b != 0 & (b %*% b) < k - 2L
      if (!any(drop)) break
      b[drop] <- 0
    }
    alive <- rowSums(b) > 0
    out[alive] <- k
    k <- k + 1L
  }
  out
}

#' Mixed-degree shell decomposition of Zeng and Zhang
#' @param b Symmetric binary adjacency matrix with zero diagonal.
#' @param lambda Exhausted-degree weight in the closed interval zero to one.
#' @return Numeric shell thresholds; these need not be integers.
#' @keywords internal
#' @noRd
.cg_mdd <- function(b, lambda = 0.7) {
  if (!is.numeric(lambda) || length(lambda) != 1L ||
        !is.finite(lambda) || lambda < 0 || lambda > 1) {
    stop("mdd_lambda must be a finite number between 0 and 1", call. = FALSE)
  }
  n <- nrow(b)
  out <- numeric(n)
  active <- rep(TRUE, n)
  original <- rowSums(b)
  residual <- original
  while (any(active)) {
    score <- lambda * original + (1 - lambda) * residual
    level <- min(score[active])
    repeat {
      batch <- which(active & score <= level)
      if (!length(batch)) break
      out[batch] <- level
      active[batch] <- FALSE
      residual <- residual - rowSums(b[, batch, drop = FALSE])
      score <- lambda * original + (1 - lambda) * residual
    }
  }
  out
}
