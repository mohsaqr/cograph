# ===========================================================================
# Batch 7 kernels — Centrality Zoo comparison batch
#
# Pure base-R kernels on matrices. Chosen from the Centrality Zoo correlation
# study (Shvydun 2025, 349 measures on 648 ICON networks) as the measures with
# the lowest rank correlation to anything cograph already computed:
#
#   distance_entropy             tau <= 0.30 vs any existing measure
#   local_dimension              tau <= 0.50
#   local_information_dimension  tau <= 0.38
#   modularity_vitality          tau <= 0.40
#   neighborhood_connectivity    tau <= 0.64
#
# Distance-scaling kernels take a hop-distance matrix `d` (0 on the diagonal,
# Inf where unreachable, integer hops elsewhere). Community kernels take a
# weight matrix `m`. Nothing here touches igraph.
# ===========================================================================

#' Ring counts: how many nodes sit at each exact hop distance from a node
#'
#' @param d_row One row of a hop-distance matrix.
#' @param self Index of the row's own node (excluded from the counts).
#' @return Integer vector; element `r` is the number of nodes at distance
#'   exactly `r`. Length is the node's eccentricity over reachable nodes;
#'   zero-length when nothing is reachable.
#' @keywords internal
#' @noRd
.cg_ring_counts <- function(d_row, self) {
  reach <- d_row[-self]
  reach <- reach[is.finite(reach) & reach > 0]
  if (length(reach) == 0L) return(integer(0))
  tabulate(as.integer(round(reach)), nbins = max(as.integer(round(reach))))
}

#' Ordinary least-squares slope of `y` on `x`
#'
#' @param x,y Numeric vectors of equal length (at least 2).
#' @return The slope; `NaN` when `x` has no spread.
#' @keywords internal
#' @noRd
.cg_ols_slope <- function(x, y) {
  xc <- x - mean(x)
  sxx <- sum(xc^2)
  if (sxx < sqrt(.Machine$double.eps)) return(NaN)
  sum(xc * (y - mean(y))) / sxx
}

#' Distance entropy (Stella & De Domenico 2018)
#'
#' Shannon entropy of the distribution of hop distances from a node to the
#' nodes it can reach, normalised to \[0, 1\] by the entropy of a uniform
#' distribution over the `M - m + 1` distance values between the node's
#' minimum and maximum distance. The printed formula in the source divides
#' by `log(M - m)`, which is undefined when only two distinct distances
#' occur; `log(M - m + 1)` is the normaliser that actually bounds the index
#' at 1 for a uniform distribution.
#'
#' @param d Hop-distance matrix.
#' @return Numeric vector in \[0, 1\]; 0 when every reachable node sits at
#'   the same distance; `NaN` for a node that reaches nothing.
#' @keywords internal
#' @noRd
.cg_distance_entropy <- function(d) {
  n <- nrow(d)
  if (is.null(n) || n == 0L) return(numeric(0))
  vapply(seq_len(n), function(i) {
    rings <- .cg_ring_counts(d[i, ], i)
    if (length(rings) == 0L) return(NaN)
    observed <- rings[rings > 0]
    if (length(observed) == 1L) return(0)
    # Distinct distance values span from the first non-empty ring to the
    # last one; the uniform distribution over that span has the maximum
    # entropy, so it is the normaliser.
    span <- length(rings) - which(rings > 0)[1L] + 1L
    p <- observed / sum(observed)
    -sum(p * log(p)) / log(span)
  }, numeric(1L))
}

#' Local dimension (Silva & Costa 2013; Pu et al. 2014)
#'
#' The ball `B_i(r)` around node `i` counts the node itself plus every node
#' within `r` hops. Its growth exponent, the slope of `ln B_i(r)` on
#' `ln r` for \code{r = 1, ..., d_max(i)}, is the local dimension. Calibrated
#' against the worked example in Wen & Deng (2019), which reports 0.9231
#' for ring sizes 4, 5, 4, 4 only when the centre is counted in the ball.
#'
#' With a single radius the regression is undefined, so the discretised
#' derivative from the same paper, \code{r * n_i(r) / B_i(r)} at \code{r = 1}, is
#' reported instead.
#'
#' @param d Hop-distance matrix.
#' @return Numeric vector; lower values mark more influential nodes.
#'   `NaN` for a node that reaches nothing.
#' @keywords internal
#' @noRd
.cg_local_dimension <- function(d) {
  n <- nrow(d)
  if (is.null(n) || n == 0L) return(numeric(0))
  vapply(seq_len(n), function(i) {
    rings <- .cg_ring_counts(d[i, ], i)
    if (length(rings) == 0L) return(NaN)
    ball <- 1 + cumsum(rings)
    if (length(ball) == 1L) return(rings[1L] / ball[1L])
    .cg_ols_slope(log(seq_along(ball)), log(ball))
  }, numeric(1L))
}

#' Local information dimensionality (Wen & Deng 2020)
#'
#' Replaces the ball count of local dimension with its Shannon information
#' `I_i(l) = -p ln p`, `p = B_i(l) / N`, and grows the box only to half the
#' node's eccentricity, `l = 1, ..., ceil(d_max(i) / 2)`. The measure is
#' minus the slope of `I_i(l)` on `ln l`. With a single box size the
#' discretised derivative of the paper (its eq. 15),
#' `l * (1 + ln p) * n_i(l) / N`, is reported instead.
#'
#' @param d Hop-distance matrix.
#' @param n_total Total node count `N` (the probability denominator).
#' @return Numeric vector; higher values mark more influential nodes.
#'   `NaN` for a node that reaches nothing.
#' @keywords internal
#' @noRd
.cg_local_information_dimension <- function(d, n_total = nrow(d)) {
  n <- nrow(d)
  if (is.null(n) || n == 0L) return(numeric(0))
  vapply(seq_len(n), function(i) {
    rings <- .cg_ring_counts(d[i, ], i)
    if (length(rings) == 0L) return(NaN)
    l_max <- ceiling(length(rings) / 2)
    ball <- (1 + cumsum(rings))[seq_len(l_max)]
    p <- ball / n_total
    if (l_max == 1L) return((1 + log(p[1L])) * rings[1L] / n_total)
    -.cg_ols_slope(log(seq_len(l_max)), -p * log(p))
  }, numeric(1L))
}

#' Newman modularity of a partition from a weight matrix
#'
#' `Q = (1 / W) sum_ij (m_ij - k_i^out k_j^in / W) delta(c_i, c_j)` with
#' `W = sum(m)`. On a symmetric matrix this is the classical undirected
#' formula (`W = 2m`); on an asymmetric one it is the Leicht-Newman
#' directed generalisation, which is also what igraph computes.
#'
#' @param m Weight matrix.
#' @param membership Community labels, one per node.
#' @return A single number; `NaN` when the matrix has no weight.
#' @keywords internal
#' @noRd
.cg_modularity <- function(m, membership) {
  total <- sum(m)
  if (!is.finite(total) || total <= 0) return(NaN)
  same <- outer(membership, membership, "==")
  sum((m - outer(rowSums(m), colSums(m)) / total) * same) / total
}

#' Modularity vitality (Magelinski, Bartulovic & Carley 2021)
#'
#' `Q(G, C) - Q(G - i, C \ i)`: how much modularity drops when node `i`
#' is deleted and the partition is kept. Positive values mark community
#' hubs, negative values mark bridges. Computed in closed form: deleting
#' `i` changes each community's internal weight and strength sums by the
#' weight `i` exchanges with that community, which is one column of
#' `m %*% indicator`, so all `n` vitalities cost one matrix product.
#'
#' @param m Weight matrix.
#' @param membership Community labels, one per node.
#' @return Numeric vector; `NaN` where removing the node leaves no weight.
#' @keywords internal
#' @noRd
.cg_modularity_vitality <- function(m, membership) {
  n <- nrow(m)
  if (is.null(n) || n == 0L) return(numeric(0))
  comm <- as.integer(factor(membership))
  k <- max(comm)
  own <- matrix(0, n, k)
  own[cbind(seq_len(n), comm)] <- 1
  total <- sum(m)
  k_out <- rowSums(m)
  k_in <- colSums(m)
  loop <- diag(m)
  out_c <- m %*% own        # weight from i into each community
  in_c <- crossprod(m, own) # weight from each community into i
  e_c <- colSums(own * out_c)
  s_out <- colSums(own * k_out)
  s_in <- colSums(own * k_in)
  q_full <- if (total > 0) {
    sum(e_c) / total - sum(s_out * s_in) / total^2
  } else {
    NaN
  }
  total_i <- total - k_out - k_in + loop
  e_i <- matrix(e_c, n, k, byrow = TRUE) - own * (out_c + in_c - loop)
  s_out_i <- matrix(s_out, n, k, byrow = TRUE) - in_c - own * (k_out - loop)
  s_in_i <- matrix(s_in, n, k, byrow = TRUE) - out_c - own * (k_in - loop)
  q_i <- ifelse(total_i > 0,
                rowSums(e_i) / total_i - rowSums(s_out_i * s_in_i) / total_i^2,
                NaN)
  as.numeric(q_full - q_i)
}

#' Neighborhood connectivity: mean degree of a node's neighbours
#'
#' Also called average neighbour degree (Maslov & Sneppen 2002). Under
#' `mode = "out"` the out-neighbours' out-degrees are averaged, under
#' `"in"` the in-neighbours' in-degrees, and under `"all"` direction is
#' ignored. Isolates score 0, following the Zoo convention.
#'
#' @param b Adjacency matrix (any non-zero entry is an edge).
#' @param mode One of `"all"`, `"out"`, `"in"`.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_neighborhood_connectivity <- function(b, mode = c("all", "out", "in")) {
  mode <- match.arg(mode)
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  nb <- switch(mode,
               all = (b + t(b)) != 0,
               out = b != 0,
               "in" = t(b) != 0)
  storage.mode(nb) <- "numeric"
  deg <- rowSums(nb)
  ifelse(deg > 0, as.numeric(nb %*% deg) / deg, 0)
}


#' Average nearest-neighbour degree (Barrat et al. 2004), igraph semantics
#'
#' `knn_i = sum_e w_e k_{j(e)} / sum_e w_e` over the edge incidences `e` of
#' node `i` under `mode`, where `k_j` is the *unweighted* degree of the
#' neighbour under `neighbor_mode` (what `igraph::knn()` computes with
#' `weights`; with unit weights it is the plain mean neighbour degree).
#' Conventions pinned against `igraph::knn()`:
#'
#' * A self-loop is an incidence like any other, and it appears twice when
#'   both of its endpoints are counted -- undirected graphs and directed
#'   `mode = "all"` -- but once under `"out"` or `"in"`. The neighbour it
#'   contributes is the node itself.
#' * An undirected graph ignores `mode` and `neighbor_mode`.
#' * A node with no incidence at all scores `NaN` (0 / 0), as igraph does.
#'
#' Differs from `.cg_neighborhood_connectivity()`, which scores isolates 0
#' and reads every non-zero entry as an unweighted edge.
#'
#' @param b Binary adjacency matrix (any non-zero entry is an edge).
#' @param w Weight matrix in the same layout; pass `b` for the unweighted
#'   average.
#' @param directed Whether the graph is directed.
#' @param mode Which incidences of `i` define its neighbours: `"all"`,
#'   `"out"` or `"in"`.
#' @param neighbor_mode Which degree of the neighbour is averaged; igraph's
#'   default is `"all"`.
#' @return Numeric vector, one value per node.
#' @references Barrat, A., Barthelemy, M., Pastor-Satorras, R., &
#'   Vespignani, A. (2004). The architecture of complex weighted networks.
#'   *PNAS*, 101(11), 3747-3752.
#' @keywords internal
#' @noRd
.cg_knn <- function(b, w, directed, mode = c("all", "out", "in"),
                    neighbor_mode = c("all", "out", "in")) {
  mode <- match.arg(mode)
  neighbor_mode <- match.arg(neighbor_mode)
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  k <- .cg_degree(b, directed, neighbor_mode)
  loop <- diag(w)
  if (!directed) {
    # Symmetric layout: each partner once, the loop a second time.
    num <- as.numeric(w %*% k) + loop * k
    den <- rowSums(w) + loop
  } else {
    num <- switch(mode,
      out = as.numeric(w %*% k),
      `in` = as.numeric(crossprod(w, k)),
      all = as.numeric(w %*% k) + as.numeric(crossprod(w, k)))
    den <- switch(mode,
      out = rowSums(w),
      `in` = colSums(w),
      all = rowSums(w) + colSums(w))
  }
  num / den
}
