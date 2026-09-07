#' Node resistance curvature from grounded electrical systems
#' @param a Symmetric nonnegative conductance matrix, with zero diagonal.
#' @return One minus half the incident relative resistance sum.
#' @keywords internal
#' @noRd
.cg_resistance_curvature <- function(a) {
  n <- nrow(a)
  out <- rep(1, n)
  components <- .cg_component_labels(a > 0)
  for (label in unique(components)) {
    nodes <- which(components == label)
    size <- length(nodes)
    if (size == 1L) next
    w <- a[nodes, nodes, drop = FALSE]
    # Curvature is unchanged by uniform conductance scaling in a component.
    scaled <- w / max(w)
    if (any(w > 0 & scaled == 0)) {
      stop("resistance_curvature conductance range exceeds double precision",
           call. = FALSE)
    }
    w <- scaled
    edges <- which(upper.tri(w) & w > 0, arr.ind = TRUE)
    laplacian <- diag(rowSums(w), size) - w
    keep <- seq_len(size - 1L)
    factor <- tryCatch(chol(laplacian[keep, keep, drop = FALSE]),
                       error = function(e) NULL)
    if (is.null(factor)) {
      stop("resistance_curvature grounded Laplacian is numerically singular",
           call. = FALSE)
    }
    incidence <- matrix(0, size, nrow(edges))
    incidence[cbind(edges[, 1L], seq_len(nrow(edges)))] <- 1
    incidence[cbind(edges[, 2L], seq_len(nrow(edges)))] <- -1
    potentials <- forwardsolve(t(factor), incidence[keep, , drop = FALSE])
    relative <- w[edges] * colSums(potentials^2)
    if (any(!is.finite(relative))) {
      stop("resistance_curvature calculation exceeds finite double precision",
           call. = FALSE)
    }
    out[nodes] <- 1 - as.numeric(abs(incidence) %*% relative) / 2
  }
  out
}
