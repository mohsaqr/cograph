#' Clustering degree algorithm of Wang et al. (2018)
#' @param w Symmetric finite nonnegative weights, zero diagonal.
#' @param alpha Convex weight on degree versus strength.
#' @return Clustering degree plus weighted neighbor contributions.
#' @keywords internal
#' @noRd
.cg_cda <- function(w, alpha = 0.5) {
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) ||
        alpha < 0 || alpha > 1) {
    stop("cda_alpha must be a finite number between 0 and 1", call. = FALSE)
  }
  n <- nrow(w)
  if (!n || !any(w > 0)) return(numeric(n))
  b <- (w > 0) * 1
  degree <- rowSums(b)
  strength <- rowSums(w)
  if (any(!is.finite(strength))) {
    stop("cda strength exceeds finite double precision", call. = FALSE)
  }
  # Normalize rows for clustering to avoid multiplying two large weights.
  shares <- w / ifelse(strength > 0, strength, 1)
  clustering <- numeric(n)
  active <- degree > 1
  clustering[active] <- rowSums(shares * (b %*% b))[active] /
    (degree[active] - 1)
  cd <- (alpha * degree + (1 - alpha) * strength) * stats::plogis(clustering)
  score <- cd + as.numeric((w / max(w)) %*% cd)
  if (any(!is.finite(score))) {
    stop("cda score exceeds finite double precision", call. = FALSE)
  }
  score
}
