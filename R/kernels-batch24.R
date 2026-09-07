#' Reciprocal diagonal of the regularized Laplacian inverse
#' @param a Symmetric nonnegative adjacency matrix with zero diagonal.
#' @param gamma Finite nonnegative regularization strength.
#' @return Graph regularization centrality in vertex order.
#' @keywords internal
#' @noRd
.cg_graph_regularization <- function(a, gamma = 1) {
  if (!is.numeric(gamma) || length(gamma) != 1L || !is.finite(gamma) ||
        gamma < 0) {
    stop("grc_gamma must be a finite nonnegative number", call. = FALSE)
  }
  out <- rep(1, nrow(a))
  if (!nrow(a) || gamma == 0) return(out)
  components <- .cg_component_labels(a > 0)
  for (label in unique(components)) {
    nodes <- which(components == label)
    size <- length(nodes)
    if (size == 1L) next
    w <- a[nodes, nodes, drop = FALSE]
    scale <- max(w)
    scaled <- w / scale
    if (any(w > 0 & scaled == 0)) {
      stop("graph_regularization weight range exceeds double precision",
           call. = FALSE)
    }
    laplacian <- diag(rowSums(scaled), size) - scaled
    # Separate the exact constant eigenvector: its inverse contribution
    # remains 1/size even when gamma times a weight would overflow.
    basis <- stats::contr.helmert(size)
    basis <- sweep(basis, 2L, sqrt(colSums(basis^2)), "/")
    reduced <- crossprod(basis, laplacian %*% basis)
    spectral <- eigen(reduced, symmetric = TRUE)
    threshold <- 64 * .Machine$double.eps * max(spectral$values)
    if (any(!is.finite(spectral$values)) ||
          any(spectral$values <= threshold)) {
      stop("graph_regularization Laplacian is numerically singular",
           call. = FALSE)
    }
    vectors <- basis %*% spectral$vectors
    attenuation <- stats::plogis(
      -(log(gamma) + log(scale) + log(spectral$values))
    )
    diagonal <- 1 / size + as.numeric(vectors^2 %*% attenuation)
    out[nodes] <- 1 / diagonal
  }
  out
}
