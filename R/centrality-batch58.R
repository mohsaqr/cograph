#' Simple undirected skeleton of a graph context
#' @param cg Graph context.
#' @return Binary symmetric adjacency matrix with a zero diagonal.
#' @keywords internal
#' @noRd
.cg_skeleton <- function(cg) {
  b <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(b) <- 0
  b
}

#' Calculate the batch 58 measures
#' @keywords internal
#' @noRd
calculate_niem <- function(cg) .cg_niem(.cg_skeleton(cg))

#' @keywords internal
#' @noRd
calculate_iksd <- function(cg) .cg_iksd(.cg_skeleton(cg))

#' @keywords internal
#' @noRd
calculate_density_centrality <- function(cg, radius = 3) {
  .cg_density_centrality(.cg_skeleton(cg), radius)
}

#' @keywords internal
#' @noRd
calculate_wksdn <- function(cg) .cg_wksdn(.cg_skeleton(cg))

#' @keywords internal
#' @noRd
calculate_mu_pci <- function(cg, mu = 1) .cg_mu_pci(.cg_skeleton(cg), mu)

#' Node Importance Evaluation Matrix Centrality
#'
#' The node importance evaluation matrix (NIEM) method (Zhou et al. 2012)
#' multiplies the efficiency of a node by the importance its neighbors
#' contribute:
#' \deqn{C_i = I_i \sum_{j \in N(i)} \frac{D_j I_j}{\langle k \rangle^2},
#'   \qquad I_k = \frac{1}{n} \sum_{i \ne k} \frac{1}{d_{ki}},}{
#'   C_i = I_i sum_{j in N(i)} D_j I_j / <k>^2,
#'   I_k = (1/n) sum_{i != k} 1 / d_ki,}
#' with \eqn{D} the degree, \eqn{\langle k \rangle}{<k>} the mean degree
#' and \eqn{d} the shortest-path distance.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored. A
#' pair of nodes without a path adds nothing to the efficiency, as the
#' source defines. The mean degree and \eqn{n} are those of the whole
#' network. Isolated nodes score 0, and so does every node of a network
#' without edges. \code{normalized = TRUE} divides the scores by their
#' maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Zhou, X., Zhang, F.-M., Li, K.-W., Hui, X.-B. and Wu, H.-S. (2012).
#'   Finding vital node by node importance evaluation matrix in complex
#'   networks. Acta Physica Sinica, 61(5), 050201.
#'   \doi{10.7498/aps.61.050201}.
#' @seealso \code{\link{centrality_harmonic}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_niem(regulation_net)
centrality_niem <- function(x, ...) {
  df <- centrality(x, measures = "niem", ...)
  stats::setNames(df$niem, df$node)
}

#' Improved k-shell Decomposition
#'
#' The improved k-shell decomposition (IKSD; Liu et al. 2015) removes, in
#' each iteration, the nodes whose current degree is the lowest in what
#' remains of the network, and gives them the number of the iteration. A
#' node removed late lies deep in the network.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' Only the nodes at the lowest degree leave in an iteration. Nodes that
#' the removal brings down to that degree wait for the next iteration,
#' which separates nodes that share a k-shell. The decomposition runs over
#' the whole network at once, so isolated nodes form the first iteration
#' and raise every other label by one.
#' \code{normalized = TRUE} divides the labels by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one iteration number per node, in
#'   input node order.
#' @references
#' Liu, Z., Jiang, C., Wang, J. and Yu, H. (2015). The node importance in
#'   actual complex networks based on a multi-attribute ranking method.
#'   Knowledge-Based Systems, 84, 56-66.
#'   \doi{10.1016/j.knosys.2015.03.026}.
#' @seealso \code{\link{centrality_coreness}},
#'   \code{\link{centrality_gli}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_iksd(regulation_net)
centrality_iksd <- function(x, ...) {
  df <- centrality(x, measures = "iksd", ...)
  stats::setNames(df$iksd, df$node)
}

#' Density Centrality
#'
#' Density centrality (Ibnoulouafi and El Haziti 2018) treats the degree
#' of a node as a mass spread over discs whose radii are the distances to
#' the nodes around it:
#' \deqn{DNC(i) = \sum_{j:\,1 \le d_{ij} \le r} \frac{k_i}{\pi d_{ij}^2},}{
#'   DNC(i) = sum_{j: 1 <= d_ij <= r} k_i / (pi d_ij^2),}
#' with \eqn{k_i} the degree of the node and \eqn{d_{ij}}{d_ij} the
#' shortest-path distance.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' \code{density_radius} is 3 by default, the setting the Centrality Zoo
#' reports for the source; \code{Inf} sums over every node that can be
#' reached. The formula was taken from Ullah et al. (2021), who reproduce
#' it; the 2018 article was not consulted, so the default radius is not
#' confirmed against it. Isolated nodes score 0. A \code{density_radius}
#' that is neither a whole number of at least one nor \code{Inf} raises a
#' \code{cograph_bad_parameter} error. \code{normalized = TRUE} divides the
#' scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{density_radius} (the largest distance summed over, default 3).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Ibnoulouafi, A. and El Haziti, M. (2018). Density centrality: identifying
#'   influential nodes based on area density formula. Chaos, Solitons and
#'   Fractals, 114, 69-80. \doi{10.1016/j.chaos.2018.06.022}.
#'
#' Ullah, A., Wang, B., Sheng, J., Long, J., Khan, N. and Sun, Z. (2021).
#'   Identification of nodes influence based on global structure model in
#'   complex networks. Scientific Reports, 11, 6173.
#'   \doi{10.1038/s41598-021-84684-x}.
#' @seealso \code{\link{centrality_gravity}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_density_centrality(regulation_net)
centrality_density_centrality <- function(x, ...) {
  df <- centrality(x, measures = "density_centrality", ...)
  stats::setNames(df$density_centrality, df$node)
}

#' Weighted k-shell Degree Neighborhood Centrality
#'
#' The weighted k-shell degree neighborhood method (WKSDN; Maji 2020)
#' gives every tie a weight built from the k-shell indices and degrees of
#' its two ends, and scores a node by the weights of its ties:
#' \deqn{WKSDN(i) = \sum_{j \in N(i)} \bigl[(ks_i + ks_j) +
#'   \lambda (k_i + k_j)\bigr], \qquad
#'   \lambda = \frac{\sum_l ks_l}{\sum_l k_l}.}{
#'   WKSDN(i) = sum_{j in N(i)} [(ks_i + ks_j) + lambda (k_i + k_j)],
#'   lambda = sum(ks) / sum(k).}
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' \eqn{\lambda}{lambda} is taken over the whole network, so adding a
#' component can change every score. Isolated nodes score 0. The equations
#' were taken from a later paper that reproduces them and from the
#' Centrality Zoo; the 2020 article was not consulted, and no published
#' value was available to check against. \code{normalized = TRUE} divides
#' the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Maji, G. (2020). Influential spreaders identification in complex networks
#'   with potential edge weight based k-shell degree neighborhood method.
#'   Journal of Computational Science, 39, 101055.
#'   \doi{10.1016/j.jocs.2019.101055}.
#' @seealso \code{\link{centrality_ink}},
#'   \code{\link{centrality_coreness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_wksdn(regulation_net)
centrality_wksdn <- function(x, ...) {
  df <- centrality(x, measures = "wksdn", ...)
  stats::setNames(df$wksdn, df$node)
}

#' Mu-Power Community Index
#'
#' The \eqn{\mu}{mu}-power community index (Papadimitriou et al. 2010) of
#' a node is the largest \eqn{k} such that at least \eqn{\mu k}{mu k}
#' nodes within \eqn{\mu}{mu} steps of it have a degree of at least
#' \eqn{k}.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The node itself is not counted. \code{pci_mu} is 1 by default, the
#' value the source computes and recommends. At that value the index is
#' the h-index of the neighbors' degrees, the same number as
#' \code{\link{centrality_lobby}}. Isolated nodes score 0. A
#' \code{pci_mu} that is not a whole number of at least one raises a
#' \code{cograph_bad_parameter} error. \code{normalized = TRUE} divides the
#' scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{pci_mu} (the number of steps and the multiplier, default 1).
#' @return A named numeric vector with one whole-number score per node, in
#'   input node order.
#' @references
#' Papadimitriou, A., Katsaros, D. and Manolopoulos, Y. (2010). Social
#'   network analysis and its applications in wireless sensor and vehicular
#'   networks. In Lecture Notes of the Institute for Computer Sciences,
#'   Social Informatics and Telecommunications Engineering, 411-420.
#'   \doi{10.1007/978-3-642-11631-5_37}.
#' @seealso \code{\link{centrality_lobby}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_mu_pci(regulation_net)
centrality_mu_pci <- function(x, ...) {
  df <- centrality(x, measures = "mu_pci", ...)
  stats::setNames(df$mu_pci, df$node)
}
