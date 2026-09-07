#' Dynamics-sensitive centrality with finite spreading and recovery
#' @param b Symmetric binary zero-diagonal adjacency matrix.
#' @param beta Spreading rate in the closed unit interval.
#' @param mu Recovery rate in the closed unit interval.
#' @param steps Nonnegative integer time horizon.
#' @return Sum of beta A (beta A + (1-mu) I)^r 1 for r=0,...,steps-1.
#' @keywords internal
#' @noRd
.cg_dynamics_sensitive <- function(b, beta = 0.1, mu = 1, steps = 5) {
  for (parameter in c("beta", "mu")) {
    value <- if (parameter == "beta") beta else mu
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
          value < 0 || value > 1) {
      stop("ds_", parameter, " must be a finite number between 0 and 1",
           call. = FALSE)
    }
  }
  if (!is.numeric(steps) || length(steps) != 1L || !is.finite(steps) ||
        steps < 0 || steps != floor(steps) || steps > .Machine$integer.max) {
    stop("ds_steps must be an integer in [0, .Machine$integer.max]",
         call. = FALSE)
  }
  out <- numeric(nrow(b))
  if (!length(out) || steps == 0 || beta == 0) return(out)
  term <- beta * rowSums(b)
  for (r in seq_len(steps)) {
    out <- out + term
    if (any(!is.finite(out))) {
      stop("dynamics_sensitive calculation exceeds finite double precision",
           call. = FALSE)
    }
    if (!any(term != 0)) break
    if (r < steps) term <- as.numeric(beta * (b %*% term) + (1 - mu) * term)
  }
  out
}

#' Malatya degree-ratio centrality
#' @param b Symmetric binary zero-diagonal adjacency matrix.
#' @return Sum of focal-to-neighbour degree ratios; isolates score zero.
#' @keywords internal
#' @noRd
.cg_malatya <- function(b) {
  degree <- rowSums(b)
  inverse <- numeric(length(degree))
  inverse[degree > 0] <- 1 / degree[degree > 0]
  as.numeric(degree * (b %*% inverse))
}
