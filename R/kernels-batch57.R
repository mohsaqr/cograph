#' Global importance of nodes (Zhao, Wang and Deng 2020)
#'
#' Zhao, Wang and Deng (2020), *Chaos, Solitons and Fractals* 133:109637.
#' The original was not read; the equation is the one two later papers
#' reproduce in the same form (Yu et al. 2022, *Entropy* 24(2):275,
#' equation 8, and a second open reproduction that keeps the parameters):
#'
#' ```
#' GIN(i) = exp(alpha k_i / n) * beta * sum_{j != i} k_j / d_ij,
#' ```
#'
#' zero for a node of degree zero, with `alpha = beta = 1`. `beta` rescales
#' every score alike and is not exposed. A node that cannot be reached
#' contributes nothing to the sum, a convention the sources do not state.
#' `n` is the number of nodes of the whole network.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param alpha Weight of the node's own degree in the exponent, a single
#'   finite nonnegative number.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_gin <- function(b, alpha = 1) {
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) ||
        alpha < 0) {
    .cg_stop_bad_parameter(
      "`gin_alpha` must be a single finite nonnegative number")
  }
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  degree <- rowSums(b)
  hops <- .cg_hop_matrix(b > 0, n)
  closeness <- ifelse(is.finite(hops) & hops > 0, 1 / hops, 0)
  ifelse(degree > 0, exp(alpha * degree / n), 0) *
    as.numeric(closeness %*% degree)
}

#' Icentr (Mussone, Viseh and Notari 2022)
#'
#' Equations (9)-(13) of Mussone, Viseh and Notari (2022), "Novel
#' centrality measures and applications to underground networks",
#' *Physica A* 589:126595. From
#' the focal node every node gets a level, its hop distance. An edge
#' between two levels has the level of its farther end (9); an edge inside
#' one level has that level plus one (10). On an unweighted network every
#' edge of the focal node's component contributes `2^-(lev(e) - 1)`
#' (11)-(12), and the score is the sum (13).
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector; isolates score zero.
#' @keywords internal
#' @noRd
.cg_icentr <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  edges <- which(b != 0 & upper.tri(b), arr.ind = TRUE)
  if (!nrow(edges)) return(numeric(n))
  hops <- .cg_hop_matrix(b > 0, n)
  vapply(seq_len(n), \(focal) {
    near <- hops[focal, edges[, 1L]]
    far <- hops[focal, edges[, 2L]]
    level <- ifelse(near == far, near + 1, pmax(near, far))
    sum(2^-(level[is.finite(level)] - 1))
  }, numeric(1))
}

#' Global and local information (GLI) and its improved k-shell
#'
#' Yang, Hu and Huang (2020), "Influential nodes identification in complex
#' networks based on global and local information", *Chinese Physics B*
#' 29(8):088903, equations (1), (4) and (5):
#'
#' ```
#' Iks(i) = ks(i) + nit(i)
#' I_i = exp[(Iks(i) + DC(i)) / sum_l (Iks(l) + DC(l))]
#'       * sum_{j : 1 <= d_ij <= r} (Iks(j) + DC(j)) / d_ij
#' ```
#'
#' with `DC` the degree, `ks` the k-shell index and `nit(i)` the round of
#' the k-shell decomposition in which `i` is removed. Table 1 of the paper
#' fixes `nit`: rounds are counted from one through the whole
#' decomposition and every node at or below the current degree leaves in
#' the same round. The paper sets `r = 3`.
#'
#' The paper assumes no isolated node. Here an isolate is left out of the
#' decomposition, has `Iks = 0` and scores zero.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param radius The `r` of equation (5), a single whole number of at
#'   least one.
#' @return A `data.frame` with the columns `shell`, `round`, `iks` and
#'   `gli`.
#' @keywords internal
#' @noRd
.cg_gli_terms <- function(b, radius = 3) {
  if (!is.numeric(radius) || length(radius) != 1L || !is.finite(radius) ||
        radius < 1 || radius != trunc(radius)) {
    .cg_stop_bad_parameter(
      "`gli_radius` must be a single whole number of at least one")
  }
  n <- nrow(b)
  if (is.null(n) || n == 0L) {
    return(data.frame(shell = numeric(0), round = numeric(0),
                      iks = numeric(0), gli = numeric(0)))
  }
  degree <- rowSums(b)
  linked <- degree > 0
  shell <- .cg_mdd(b, lambda = 0)
  round <- numeric(n)
  round[linked] <- .cg_onion(b[linked, linked, drop = FALSE], FALSE)
  mass <- shell + round + degree
  hops <- .cg_hop_matrix(b > 0, n)
  inside <- ifelse(is.finite(hops) & hops >= 1 & hops <= radius, 1 / hops, 0)
  total <- sum(mass)
  gli <- if (total > 0) exp(mass / total) * as.numeric(inside %*% mass) else {
    numeric(n)
  }
  data.frame(shell = shell, round = round, iks = shell + round,
             gli = ifelse(linked, gli, 0), row.names = NULL)
}
