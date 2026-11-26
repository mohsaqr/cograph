#' Calculate the batch 64 measures
#' @keywords internal
#' @noRd
calculate_wd <- function(cg) .cg_rmd_weighted_degree(.cg_skeleton(cg))

#' @keywords internal
#' @noRd
calculate_ewd <- function(cg) {
  .cg_rmd_weighted_degree(.cg_skeleton(cg), extended = TRUE)
}

#' @keywords internal
#' @noRd
calculate_mlc <- function(cg) .cg_mlc(.cg_skeleton(cg))

#' @keywords internal
#' @noRd
calculate_spreading_probability <- function(cg, radius = 3) {
  .cg_spreading_probability(.cg_skeleton(cg), radius)
}

#' @keywords internal
#' @noRd
calculate_profitleader <- function(cg) .cg_profitleader(.cg_skeleton(cg))

#' @keywords internal
#' @noRd
calculate_hks <- function(cg) .cg_hks(.cg_skeleton(cg))

#' @keywords internal
#' @noRd
calculate_niccm <- function(cg, layers = 2) .cg_niccm(.cg_skeleton(cg), layers)

#' @keywords internal
#' @noRd
calculate_correlation_centrality <- function(cg, alpha = 3) {
  .cg_correlation_centrality(.cg_skeleton(cg), alpha)
}

#' RMD-Weighted Degree
#'
#' The RMD-weighted degree (WD; Yang et al. 2018) sums, over the neighbors
#' of a node, the round in which each neighbor is removed by the
#' remaining-minimum-degree decomposition, as a share of the number of
#' rounds:
#' \deqn{WD(v) = \sum_{u \in N(v)} \frac{Iter(u)}{MaxIter}.}{WD(v) = sum_{u in N(v)} Iter(u) / MaxIter.}
#' The decomposition removes, in each round, every node whose current
#' degree equals the lowest current degree. A neighbor removed late lies
#' deep in the network and adds more to the score.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' \eqn{Iter} is the index \code{\link{centrality_iksd}} returns. Rounds
#' are counted over the whole network, so the components of a disconnected
#' network share them. Isolated nodes score 0. The implementation
#' reproduces the worked value and the ranking the source prints for its
#' 15-node example. \code{normalized = TRUE} divides the scores by their
#' maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Yang, F., Li, X., Xu, Y., Liu, X., Wang, J., Zhang, Y., Zhang, R. and
#' Yao, Y. (2018). Ranking the spreading influence of nodes in complex
#' networks: An extended weighted degree centrality based on a remaining
#' minimum degree decomposition. Physics Letters A, 382(34), 2361-2371.
#' \doi{10.1016/j.physleta.2018.05.032}.
#' @seealso \code{\link{centrality_ewd}}, \code{\link{centrality_iksd}},
#' \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_wd(regulation_net)
centrality_wd <- function(x, ...) {
  df <- centrality(x, measures = "wd", ...)
  stats::setNames(df$wd, df$node)
}

#' Extended RMD-Weighted Degree
#'
#' The extended RMD-weighted degree (EWD; Yang et al. 2018) sums the
#' RMD-weighted degree of the neighbors of a node:
#' \deqn{EWD(v) = \sum_{u \in N(v)} WD(u),}{EWD(v) = sum_{u in N(v)} WD(u),}
#' with \eqn{WD} the score \code{\link{centrality_wd}} returns. It extends
#' that score from the neighbors of a node to the nodes two steps away.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The sum for a neighbor includes the node itself, as the worked value in
#' the source requires, so a node is counted once for each of its
#' neighbors. Isolated nodes score 0. The implementation reproduces the
#' worked value and the ranking the source prints for its 15-node example.
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Yang, F., Li, X., Xu, Y., Liu, X., Wang, J., Zhang, Y., Zhang, R. and
#' Yao, Y. (2018). Ranking the spreading influence of nodes in complex
#' networks: An extended weighted degree centrality based on a remaining
#' minimum degree decomposition. Physics Letters A, 382(34), 2361-2371.
#' \doi{10.1016/j.physleta.2018.05.032}.
#' @seealso \code{\link{centrality_wd}}, \code{\link{centrality_iksd}},
#' \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_ewd(regulation_net)
centrality_ewd <- function(x, ...) {
  df <- centrality(x, measures = "ewd", ...)
  stats::setNames(df$ewd, df$node)
}

#' Modified Local Centrality
#'
#' The modified local centrality (MLC; Ma and Ma 2017) is the semi-local
#' centrality of a node less twice the sum of the degrees of its neighbors:
#' \deqn{MLC(v) = \sum_{u \in N(v)} \sum_{w \in N(u)} N_2(w) - 2 \sum_{u \in N(v)} k_u,}{MLC(v) = sum_{u in N(v)} sum_{w in N(u)} N2(w) - 2 sum_{u in N(v)} k_u,}
#' where \eqn{N_2(w)}{N2(w)} is the number of nodes within two steps of
#' \eqn{w} and \eqn{k_u} the degree of \eqn{u}. The subtraction removes the
#' part of the semi-local score that the node and its neighbors contribute
#' themselves.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The first term is \code{\link{centrality_semilocal}}, and the two scores
#' rank nodes almost alike (Kendall correlation of 0.99 on four test
#' networks). The score can be negative on a small component: both ends of
#' an isolated edge score -1. Isolated nodes score 0. The source prints no
#' value of the measure, so the implementation is checked against the
#' definition only. \code{normalized = TRUE} divides the scores by their
#' maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Ma, Q. and Ma, J. (2017). Identifying and ranking influential spreaders
#' in complex networks with consideration of spreading probability. Physica
#' A: Statistical Mechanics and its Applications, 465, 312-330.
#' \doi{10.1016/j.physa.2016.08.041}.
#' @seealso \code{\link{centrality_semilocal}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_mlc(regulation_net)
centrality_mlc <- function(x, ...) {
  df <- centrality(x, measures = "mlc", ...)
  stats::setNames(df$mlc, df$node)
}

#' Spreading Probability Centrality
#'
#' Spreading probability centrality (SP; Bao et al. 2017) sums, over the
#' nodes within a radius of a node, the number of shortest paths to each,
#' discounted by the mean degree of the network once per step:
#' \deqn{SP(i) = \sum_{j:\,1 \le d_{ij} \le r} \sigma_{ij} \langle k \rangle^{-d_{ij}},}{SP(i) = sum_{j: 1 <= d_ij <= r} sigma_ij <k>^(-d_ij),}
#' with \eqn{\sigma_{ij}}{sigma_ij} the number of shortest paths between
#' \eqn{i} and \eqn{j}, \eqn{d_{ij}}{d_ij} their distance and \eqn{\langle
#' k \rangle}{<k>} the mean degree. The reciprocal of the mean degree
#' stands for the probability that an infection crosses one tie.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' \code{sp_radius} is 3 by default, the value the source uses; \code{Inf}
#' sums over every node that can be reached. With \code{sp_radius = 1} the
#' score is the degree divided by the mean degree. The node itself is not
#' counted. Isolated nodes score 0, and so does every node of a network
#' without ties. The source prints no value of the measure on a small
#' network, so the implementation is checked against the definition only. A
#' \code{sp_radius} that is neither a whole number of at least one nor
#' \code{Inf} raises a \code{cograph_bad_parameter} error. \code{normalized
#' = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{sp_radius} (the largest distance summed over, default 3).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Bao, Z.-K., Ma, C., Xiang, B.-B. and Zhang, H.-F. (2017). Identification
#' of influential nodes in complex networks: Method from spreading
#' probability viewpoint. Physica A: Statistical Mechanics and its
#' Applications, 468, 391-397. \doi{10.1016/j.physa.2016.10.086}.
#' @seealso \code{\link{centrality_kpath}},
#' \code{\link{centrality_semilocal}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_spreading_probability(regulation_net)
#' centrality_spreading_probability(regulation_net, sp_radius = 2)
centrality_spreading_probability <- function(x, ...) {
  df <- centrality(x, measures = "spreading_probability", ...)
  stats::setNames(df$spreading_probability, df$node)
}

#' ProfitLeader
#'
#' ProfitLeader (Yu et al. 2019) scores a node by the resource it can share
#' with its neighbors. For a neighbor \eqn{v} of \eqn{u}, the available
#' resource is one more than the degree of \eqn{u} plus the degrees of the
#' neighbors of \eqn{u} that are neither \eqn{v} nor neighbors of \eqn{v},
#' and the sharing probability is the Jaccard similarity of the two closed
#' neighborhoods:
#' \deqn{PC(u) = \sum_{v \in N(u)} \Bigl(k_u + 1 + \sum_{s \in N(u) \setminus N[v]} k_s\Bigr) \frac{|N[u] \cap N[v]|}{|N[u] \cup N[v]|}.}{PC(u) = sum_{v in N(u)} (k_u + 1 + sum_{s in N(u) minus N[v]} k_s) |N[u] and N[v]| / |N[u] or N[v]|.}
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' \eqn{N[u]} is the neighborhood of \eqn{u} with \eqn{u} added. The one
#' added to the degree is that of the code the authors released: with it
#' the implementation reproduces all 40 positions of the top ten nodes the
#' source lists in its Table 5 for the karate club, dolphin, adjective-noun
#' and Les Miserables networks, and without it 36. The article itself is
#' not consistent on this point: its equation (2), and the closed forms
#' printed with its Fig. 3, have no added one. Isolated nodes score 0.
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Yu, Z., Shao, J., Yang, Q. and Sun, Z. (2019). ProfitLeader: identifying
#' leaders in networks with profit capacity. World Wide Web, 22(2),
#' 533-553. \doi{10.1007/s11280-018-0537-6}.
#' @seealso \code{\link{centrality_degree}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_profitleader(regulation_net)
centrality_profitleader <- function(x, ...) {
  df <- centrality(x, measures = "profitleader", ...)
  stats::setNames(df$profitleader, df$node)
}

#' Hierarchical k-Shell Centrality
#'
#' Hierarchical k-shell centrality (HKS; Zareie and Sheikhahmadi 2018)
#' combines the degree of a node with two positions in a peeling of the
#' network, and sums the result over two steps:
#' \deqn{S(i) = \sum_{j \in N(i)} k_j (b_j + f_j), \qquad HKS(i) = \sum_{j \in N(i)} S(j).}{S(i) = sum_{j in N(i)} k_j (b_j + f_j),  HKS(i) = sum_{j in N(i)} S(j).}
#' \eqn{b_j} is the round in which node \eqn{j} is removed when the nodes
#' of lowest degree are peeled off round by round, and \eqn{f_j = \max_u
#' (b_u - d_{ju})}{f_j = max_u (b_u - d_ju)} measures how close \eqn{j}
#' lies to the nodes removed last.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The peeling starts at degree 1, removes in each round every remaining
#' node whose degree does not exceed the current level, and raises the
#' level only when a round removes nothing. The maximum that defines
#' \eqn{f} runs over the nodes that can be reached, so each component is
#' scored on its own. Isolated nodes score 0. The implementation reproduces
#' the monotonicity value the source prints for the karate club network
#' (0.9542); the source prints no node scores on a network whose ties it
#' lists. \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Zareie, A. and Sheikhahmadi, A. (2018). A hierarchical approach for
#' influential node ranking in complex social networks. Expert Systems with
#' Applications, 93, 200-211. \doi{10.1016/j.eswa.2017.10.018}.
#' @seealso \code{\link{centrality_iksd}},
#' \code{\link{centrality_coreness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_hks(regulation_net)
centrality_hks <- function(x, ...) {
  df <- centrality(x, measures = "hks", ...)
  stats::setNames(df$hks, df$node)
}

#' Node Importance by the Contribution Correlation Matrix
#'
#' The node importance contribution correlation matrix method (NICCM; Hu et
#' al. 2015) lets each node hand its efficiency to the nodes around it, in
#' proportion to how much each of them depends on it:
#' \deqn{H_j = I_j \sum_{i:\,1 \le d_{ij} \le n_l} \frac{\delta_{ij} I_i}{d_{ij}},}{H_j = I_j sum_{i: 1 <= d_ij <= nl} delta_ij I_i / d_ij,}
#' where \eqn{I_i = \frac{1}{n}\sum_{p \ne i} 1/d_{ip}}{I_i = (1/n) sum_{p
#' != i} 1/d_ip} is the efficiency of node \eqn{i} and
#' \eqn{\delta_{ij}}{delta_ij} is the efficiency node \eqn{j} loses when
#' \eqn{i} is deleted, as a share of the loss of all nodes at the same
#' distance from \eqn{i}.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' \code{niccm_layers} is the largest distance \eqn{n_l}{nl} a node
#' contributes over, 2 by default as in the source; \code{Inf} uses every
#' distance. The efficiency after a deletion is still divided by the
#' original number of nodes, which is what the tables of the source
#' require. Unreachable pairs add nothing to an efficiency. Isolated nodes
#' score 0. The implementation reproduces the values the source prints for
#' its 10-node example and for the ARPA network. The measure deletes each
#' node in turn and recomputes all distances, so it is in the costly tier:
#' it is computed when named in \code{measures} and left out of the default
#' set. A \code{niccm_layers} that is neither a whole number of at least
#' one nor \code{Inf} raises a \code{cograph_bad_parameter} error.
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{niccm_layers} (the number of layers a node contributes over,
#' default 2).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Hu, P., Fan, W. and Mei, S. (2015). Identifying node importance in
#' complex networks. Physica A: Statistical Mechanics and its Applications,
#' 429, 169-176. \doi{10.1016/j.physa.2015.02.002}.
#' @seealso \code{\link{centrality_niem}},
#' \code{\link{centrality_harmonic}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_niccm(regulation_net)
centrality_niccm <- function(x, ...) {
  df <- centrality(x, measures = "niccm", ...)
  stats::setNames(df$niccm, df$node)
}

#' Correlation Centrality
#'
#' Correlation centrality (Fan et al. 2013) sums the efficiency of the
#' other nodes, each weighted by the number of shortest paths to it and
#' discounted by a power of the distance:
#' \deqn{CoC(i) = \frac{1}{n} \sum_{j \ne i} EC(j) \frac{\sigma_{ij}}{d_{ij}^{\alpha}}, \qquad EC(j) = \frac{1}{n} \sum_{k \ne j} \frac{1}{d_{jk}},}{CoC(i) = (1/n) sum_{j != i} EC(j) sigma_ij / d_ij^alpha,  EC(j) = (1/n) sum_{k != j} 1/d_jk,}
#' with \eqn{\sigma_{ij}}{sigma_ij} the number of shortest paths between
#' \eqn{i} and \eqn{j} and \eqn{d_{ij}}{d_ij} their distance.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' \code{coc_alpha} is 3 by default, the value the source selects. With
#' that value the direct neighbors carry most of the score. A node that
#' cannot be reached contributes 0, and isolated nodes score 0. The
#' implementation reproduces the 21 values the source prints for the ARPA
#' network, on an edge list recovered from the other columns of the same
#' table. A \code{coc_alpha} that is not a single finite nonnegative number
#' raises a \code{cograph_bad_parameter} error. \code{normalized = TRUE}
#' divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{coc_alpha} (the exponent of the distance, default 3).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Fan, W., Liu, Z. and Hu, P. (2013). Identifying node importance based on
#' information entropy in complex networks. Physica Scripta, 88(6), 065201.
#' \doi{10.1088/0031-8949/88/06/065201}.
#' @seealso \code{\link{centrality_harmonic}},
#' \code{\link{centrality_niccm}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_correlation_centrality(regulation_net)
centrality_correlation_centrality <- function(x, ...) {
  df <- centrality(x, measures = "correlation_centrality", ...)
  stats::setNames(df$correlation_centrality, df$node)
}
