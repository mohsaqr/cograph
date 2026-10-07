#' Calculate global structure models on the simple skeleton
#' @keywords internal
#' @noRd
calculate_global_structure <- function(cg, model = "gsm", normalized = FALSE) {
  b <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(b) <- 0
  core <- if (model == "igsm") numeric(nrow(b)) else .cg_mdd(b, 0)
  .cg_global_structure(b, core, model, normalized)
}

#' Global Structure Model Centrality
#'
#' The global structure model (GSM; Ullah et al. 2021) multiplies a
#' coreness factor of the node by the coreness of all other nodes, each
#' discounted by its hop distance:
#' \deqn{GSM(i) = \exp\left(\frac{k_s(i)}{N}\right) \sum_{j \ne i}
#'   \frac{k_s(j)}{d_{ij}}.}{
#'   GSM(i) = exp(k_s(i) / N) sum_{j != i} k_s(j) / d_ij.}
#' Here \eqn{k_s}{k_s} is the core number and \eqn{N} the number of nodes,
#' isolated nodes included.
#'
#' @details
#' The measure uses the simple undirected skeleton, so direction, weights,
#' loops and parallel edges are ignored, and \code{mode} has no effect.
#' Only reachable nodes contribute to the sum, which extends the measure to
#' disconnected networks. Other components still affect the scores through
#' \eqn{N}. An isolated node scores 0.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Ullah, A., Wang, B., Sheng, J., Long, J., Khan, N., & Sun, Z. (2021).
#'   Identification of nodes influence based on global structure model in
#'   complex networks. Scientific Reports, 11, 6173.
#'   \doi{10.1038/s41598-021-84684-x}.
#' @seealso \code{\link{centrality_hybrid_global_structure}},
#'   \code{\link{centrality_improved_global_structure}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_global_structure(regulation_net)
centrality_global_structure <- function(x, ...) {
  df <- centrality(x, measures = "global_structure", ...)
  stats::setNames(df$global_structure, df$node)
}

#' Hybrid Global Structure Model Centrality
#'
#' The hybrid global structure model (H-GSM; Mukhtar et al. 2023) combines
#' a self-influence
#' \eqn{s_i = \exp(k_s(i)\, k_i / N)}{s_i = exp(k_s(i) k_i / N)}
#' with a distance exponent \eqn{a} computed from the mean self-influence:
#' \deqn{HGSM(i) = s_i \sum_{j \ne i} \frac{s_j}{d_{ij}^{a}}, \qquad
#'   a = \left\lceil \log_2 \frac{1}{N} \sum_l s_l \right\rceil.}{
#'   HGSM(i) = s_i sum_{j != i} s_j / d_ij^a,
#'   a = ceiling(log2(sum_l s_l / N)).}
#' Here \eqn{k_s(i)} is the core number, \eqn{k_i} the degree and \eqn{N}
#' the number of nodes, isolated nodes included.
#'
#' @details
#' The measure uses the simple undirected skeleton, so direction, weights,
#' loops and parallel edges are ignored, and \code{mode} has no effect.
#' Only reachable nodes contribute to the sum. Other components affect the
#' scores through \eqn{N} and the mean self-influence, to which each
#' isolated node adds 1. An isolated node scores 0. Raw scores beyond
#' double precision raise an error. With \code{normalized = TRUE} the
#' scores are computed on the log scale and remain available in that case.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Mukhtar, M. F., et al. (2023). Integrating local and global information to
#'   identify influential nodes in complex networks. Scientific Reports, 13,
#'   11411. \doi{10.1038/s41598-023-37570-7}.
#' @seealso \code{\link{centrality_global_structure}},
#'   \code{\link{centrality_improved_global_structure}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_hybrid_global_structure(regulation_net)
centrality_hybrid_global_structure <- function(x, ...) { # nolint: object_length_linter
  df <- centrality(x, measures = "hybrid_global_structure", ...)
  stats::setNames(df$hybrid_global_structure, df$node)
}

#' Improved Global Structure Model Centrality
#'
#' The improved global structure model (IGSM; Zhu and Wang 2022) replaces
#' the core numbers of \code{\link{centrality_global_structure}} with
#' degrees and raises each distance to an exponent set by the mean degree
#' \eqn{\bar{k}}{k_bar}:
#' \deqn{IGSM(i) = \exp\left(\frac{k_i}{N}\right) \sum_{j \ne i}
#'   \frac{k_j}{d_{ij}^{a}}, \qquad a = \lceil \log_2 \bar{k} \rceil.}{
#'   IGSM(i) = exp(k_i / N) sum_{j != i} k_j / d_ij^a,
#'   a = ceiling(log2(k_bar)).}
#' The formula follows equation 5 of Mukhtar et al. (2023).
#'
#' @details
#' The measure uses the simple undirected skeleton, so direction, weights,
#' loops and parallel edges are ignored, and \code{mode} has no effect.
#' Only reachable nodes contribute to the sum, and \eqn{N} and the mean
#' degree include every node of the network. When the mean degree is at
#' most 1 the exponent is zero or negative, and with a negative exponent
#' distant nodes contribute more than near ones. An isolated node scores
#' 0, and so does every node of a network without edges.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Zhu, J.-C., & Wang, L.-W. (2022). An extended improved global structure
#'   model for influential node identification in complex networks. Chinese
#'   Physics B, 31, 068904. \doi{10.1088/1674-1056/ac380d}.
#'
#' Mukhtar, M. F., et al. (2023). Integrating local and global information to
#'   identify influential nodes in complex networks. Scientific Reports, 13,
#'   11411. \doi{10.1038/s41598-023-37570-7}.
#' @seealso \code{\link{centrality_global_structure}},
#'   \code{\link{centrality_hybrid_global_structure}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_improved_global_structure(regulation_net)
centrality_improved_global_structure <- function(x, ...) { # nolint: object_length_linter
  df <- centrality(x, measures = "improved_global_structure", ...)
  stats::setNames(df$improved_global_structure, df$node)
}
