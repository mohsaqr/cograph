#' Finite weighted walk sum of Banerjee et al.
#' @param a Finite nonnegative weighted adjacency matrix.
#' @param q Multiplier in the closed unit interval.
#' @param steps Nonnegative integer horizon.
#' @return Row sums of the powers of qA, excluding the zeroth power.
#' @keywords internal
#' @noRd
.cg_finite_diffusion <- function(a, q = 1, steps = 3) {
  if (!is.numeric(q) || length(q) != 1L || !is.finite(q) || q < 0 || q > 1) {
    stop("diffusion_q must be a finite number between 0 and 1", call. = FALSE)
  }
  if (!is.numeric(steps) || length(steps) != 1L || !is.finite(steps) ||
        steps < 0 || steps != floor(steps) || steps > .Machine$integer.max) {
    stop("diffusion_steps must be an integer in [0, .Machine$integer.max]",
         call. = FALSE)
  }
  n <- nrow(a)
  out <- numeric(n)
  if (!n || steps == 0 || q == 0) return(out)
  term <- rep(1, n)
  transition <- q * a
  for (t in seq_len(steps)) {
    term <- as.numeric(transition %*% term)
    out <- out + term
    if (any(!is.finite(out))) {
      stop("diffusion_centrality exceeds finite double precision",
           call. = FALSE)
    }
    if (!any(term != 0)) break
  }
  out
}

#' Spectral radius of a nonnegative zero-diagonal adjacency
#' @param a Finite nonnegative zero-diagonal matrix.
#' @return Largest component spectral radius; zero for acyclic graphs.
#' @keywords internal
#' @noRd
.cg_candidate_radius <- function(a) {
  components <- .cg_strong_components(a != 0)
  radius <- 0
  for (comp in components) {
    if (length(comp) < 2L) next
    block <- a[comp, comp, drop = FALSE]
    values <- eigen(block, symmetric = isSymmetric(block),
                    only.values = TRUE)$values
    if (any(!is.finite(values))) {
      stop("dynamical_importance eigenvalues exceed finite double precision",
           call. = FALSE)
    }
    radius <- max(radius, Mod(values))
  }
  radius
}

#' Relative spectral loss on vertex deletion
#' @param a Finite nonnegative zero-diagonal adjacency matrix.
#' @return Relative loss in the unit interval; NaN for a zero original radius.
#' @keywords internal
#' @noRd
.cg_dynamical_importance <- function(a) {
  n <- nrow(a)
  if (n == 0L) return(numeric(0))
  radius <- .cg_candidate_radius(a)
  if (radius == 0) return(rep(NaN, n))
  out <- vapply(seq_len(n), function(i) {
    after <- .cg_candidate_radius(a[-i, -i, drop = FALSE])
    (radius - after) / radius
  }, numeric(1))
  pmax(0, pmin(1, out))
}
