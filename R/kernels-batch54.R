#' Nonbacktracking matrix of a simple undirected graph
#'
#' Equation (2.1) of Torres, Chan, Tong and Eliassi-Rad (2021): over the
#' `2m` directed edges, `B[k->l, i->j] = 1` when `j = k` and `i != l`.
#'
#' @param a Binary symmetric adjacency matrix with a zero diagonal.
#' @return A list with `b` (the `2m x 2m` matrix) and the integer vectors
#'   `from` and `to` naming the directed edge of each row and column.
#' @keywords internal
#' @noRd
.cg_nb_matrix <- function(a) {
  arcs <- which(a != 0, arr.ind = TRUE)
  from <- unname(arcs[, 1L])
  to <- unname(arcs[, 2L])
  b <- outer(from, to, "==") & !outer(to, from, "==")
  list(b = b * 1, from = from, to = to)
}

#' X-nonbacktracking centrality of one node
#'
#' Proposition 3.7 / equation (3.11), pages 660-661, of Torres, Chan, Tong
#' and Eliassi-Rad (2021), *SIAM Journal on Mathematics of Data Science*
#' 3(2):656-675:
#'
#' ```
#' alpha_1 = v_1' P X v_1 = (sum_i a_ci v_1^i)^2 - sum_i a_ci (v_1^i)^2
#' ```
#'
#' "where `v_1^i` is the NB-centrality of node `i` in the graph after
#' removal", `v_1^i = sum_j a_ij v_{j->i}` (equation 2.2), `v_1` is the
#' principal right eigenvector of the nonbacktracking matrix `B'` of
#' `G - c`, and "the principal eigenvector is normalized by
#' `u_1' v_1 = v_1' P v_1 = 1`, i.e., it does not have unit length". `P`
#' reverses every directed edge. The quotient form used here,
#' `alpha = (closed form on any scaling of v) / (v' P v)`, is the same
#' number and needs no rescaling.
#'
#' **The normalization is per node and cannot be dropped.** `B'` is a
#' different matrix for every focal node, so `v' P v` is a different factor
#' for every node. The Centrality Zoo's entry 2.408 omits it. The authors'
#' program defaults to an approximation that takes the eigenvector of the
#' whole graph (section 4 of the paper), a different quantity that ranks the
#' karate club differently; its exact branch does not run as published.
#'
#' **When the score has no value.** The definition needs a principal
#' eigenvector, that is a simple largest eigenvalue of `B'`. Three cases
#' have none, and the paper discusses none of them:
#' * `G - c` is a forest. `B'` is nilpotent and has no eigenvalue but zero.
#' * Every component of the 2-core of `G - c` is a single cycle. A
#'   nonbacktracking walk cannot turn round on a cycle, so the eigenvalue
#'   one occurs once per orientation.
#' * Two components of the 2-core share the largest eigenvalue.
#' The cases are decided on the structure of the 2-core, where the
#' nonbacktracking matrix is nonsingular (its determinant is the product
#' of the degrees minus one), and not on the computed spectrum of `B'`,
#' whose zero eigenvalues are defective and numerically unreliable.
#'
#' @param a Binary symmetric adjacency matrix with a zero diagonal.
#' @param focal Index of the node `c`.
#' @return A list with `value` (the score or `NA`) and `status`, one of
#'   `"ok"`, `"forest"`, `"cycles"` or `"tied"`.
#' @keywords internal
#' @noRd
.cg_xnb_node <- function(a, focal) {
  undefined <- function(status) list(value = NA_real_, status = status)
  rest <- a[-focal, -focal, drop = FALSE]
  neighbors <- which(a[focal, -focal] != 0)
  if (!nrow(rest)) return(undefined("forest"))
  in_core <- .cg_mdd(rest, lambda = 0) >= 2
  if (!any(in_core)) return(undefined("forest"))

  # Largest nonbacktracking eigenvalue of every component of the 2-core.
  core_nodes <- which(in_core)
  core <- rest[core_nodes, core_nodes, drop = FALSE]
  labels <- .cg_component_labels(core)
  radius <- vapply(unique(labels), \(label) {
    part <- core[labels == label, labels == label, drop = FALSE]
    if (all(rowSums(part) == 2)) return(1)
    max(Re(eigen(.cg_nb_matrix(part)$b, only.values = TRUE)$values))
  }, numeric(1))
  top <- max(radius)
  if (top <= 1) return(undefined("cycles"))
  if (sum(radius >= top * (1 - 1e-9)) > 1L) return(undefined("tied"))

  # The eigenvector lives on the component of G - c that holds that core
  # component; it is zero on every other component.
  seed <- core_nodes[labels == unique(labels)[which.max(radius)]][1L]
  whole <- .cg_component_labels(rest)
  members <- which(whole == whole[seed])
  nb <- .cg_nb_matrix(rest[members, members, drop = FALSE])
  spectrum <- eigen(nb$b)
  principal <- Re(spectrum$vectors[, which.min(Mod(spectrum$values - top))])

  # Equation (2.2): the entries on the edges that point into each node.
  inflow <- rowsum(principal, nb$to)
  centrality <- numeric(nrow(rest))
  centrality[members[as.integer(rownames(inflow))]] <- inflow[, 1L]
  reverse <- match(paste(nb$to, nb$from), paste(nb$from, nb$to))
  scale <- sum(principal * principal[reverse])

  at <- centrality[neighbors]
  list(value = (sum(at)^2 - sum(at^2)) / scale, status = "ok")
}

#' X-nonbacktracking centrality of every node
#' @param a Binary symmetric adjacency matrix with a zero diagonal.
#' @return A `data.frame` with one row per node and the columns `value` and
#'   `status`.
#' @keywords internal
#' @noRd
.cg_x_nonbacktracking <- function(a) {
  n <- nrow(a)
  if (is.null(n) || n == 0L) {
    return(data.frame(value = numeric(0), status = character(0)))
  }
  nodes <- lapply(seq_len(n), \(focal) .cg_xnb_node(a, focal))
  data.frame(value = vapply(nodes, \(x) x$value, numeric(1)),
             status = vapply(nodes, \(x) x$status, character(1)))
}
