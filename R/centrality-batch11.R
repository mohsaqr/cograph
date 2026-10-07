# ===========================================================================
# Batch 11 — parameterized members of families cograph already had
#
# igraph-facing calculators over R/kernels-batch11.R and the exported verbs.
# ===========================================================================

#' @keywords internal
#' @noRd
calculate_length_scaled_betweenness <- function(cg, weights = NULL) {
  n <- cg$n
  if (n == 0L) return(numeric(0))
  directed <- cg$directed
  w <- .cg_mode_weights(.cg_path_matrix(cg, weights),
                        if (directed) "out" else "all")
  .cg_length_scaled_betweenness(w, n, directed)
}

#' @keywords internal
#' @noRd
calculate_delta_betweenness <- function(cg, weights = NULL, delta = 1) {
  n <- cg$n
  if (n == 0L) return(numeric(0))
  directed <- cg$directed
  w <- .cg_mode_weights(.cg_path_matrix(cg, weights),
                        if (directed) "out" else "all")
  .cg_delta_betweenness(w, n, directed, delta = delta)
}

#' @keywords internal
#' @noRd
calculate_ego_betweenness <- function(cg) {
  if (cg$n == 0L) return(numeric(0))
  .cg_ego_betweenness(.cg_edge_indicator(.cg_path_matrix(cg, NULL)),
                      cg$directed)
}

#' @keywords internal
#' @noRd
calculate_delta_closeness <- function(cg, mode = "all", delta = 1,
                                      dist_mat = NULL, weights = NULL) {
  if (cg$n == 0L) return(numeric(0))
  d <- dist_mat %||% .cg_distances(.cg_path_matrix(cg, weights), mode)
  .cg_delta_closeness(d, delta = delta)
}

#' Mass vectors for the gravity family
#' @keywords internal
#' @noRd
.cg_gravity_mass <- function(cg, mass, mode) {
  b <- .cg_path_matrix(cg, NULL)
  deg <- .cg_degree(b, cg$directed, mode)
  ks <- .cg_loop_coreness(b, cg$n, cg$directed, mode)
  switch(mass,
         degree = list(i = deg, j = deg),
         kshell = list(i = ks, j = ks),
         # cograph's pre-2.4.8 form: no mass on the focal node, and the
         # product of degree and k-shell on its partners. No published
         # source; kept so earlier results stay reproducible.
         legacy = list(i = rep(1, cg$n), j = deg * ks))
}

#' @keywords internal
#' @noRd
calculate_gravity <- function(cg, mode = "all", mass = "kshell",
                              radius = 3, exponent = 2) {
  cg <- .cg_context(cg)
  n <- cg$n
  if (n == 0L) return(numeric(0))
  if (n == 1L) return(0)
  d <- .cg_hop_distances(cg, mode)
  if (identical(radius, "auto")) radius <- .cg_gravity_auto_radius(d)
  m <- .cg_gravity_mass(cg, mass, mode)
  .cg_gravity(d, m$i, m$j, radius = radius, exponent = exponent)
}

# ---------------------------------------------------------------------------
# Exported verbs
# ---------------------------------------------------------------------------

#' Length-Scaled, Delta and Ego Betweenness, and Delta Closeness
#'
#' Length-scaled betweenness (Borgatti and Everett 2006; Brandes 2008)
#' weights each pair \eqn{s,t} in the betweenness sum by \eqn{1/d(s,t)}.
#' Delta betweenness (Agneessens et al. 2017) uses the pair weight
#' \eqn{(h(s,t)-1)^{-\delta}}{(h(s,t) - 1)^(-delta)}, where \eqn{h(s,t)} is
#' the number of edges on a shortest path, so \eqn{\delta = 0}{delta = 0}
#' gives ordinary betweenness. Ego betweenness (Everett and Borgatti 2005)
#' is the betweenness of a node inside its own ego network. Delta closeness
#' (Agneessens et al. 2017, eq. 2) is
#' \deqn{C_\delta(i) = \frac{1}{n-1} \sum_{j \ne i} d_{ij}^{-\delta}.}{
#'   C_delta(i) = sum_{j != i} d_ij^(-delta) / (n - 1).}
#'
#' @details
#' On a directed network the three betweenness measures count directed
#' paths. Length-scaled and delta betweenness and delta closeness read edge
#' weights as distances, and \code{invert_weights = TRUE} converts weights
#' to distances \eqn{1/w^\alpha}{1/w^alpha}. In delta betweenness the
#' weighted distances decide which paths are shortest, and the pair weight
#' \eqn{(h - 1)^{-\delta}}{(h - 1)^(-delta)} uses the number of edges
#' \eqn{h} on a shortest path (the fewest when several tie), so
#' \eqn{h - 1} is the number of intermediaries. On a binary network
#' \eqn{h = d(s,t)}{h = d(s,t)}. A pair joined by a one-edge shortest path
#' contributes nothing. \code{weighted = FALSE} uses hop counts. Ego betweenness ignores weights, and a node with fewer
#' than two neighbors scores 0. Delta closeness follows \code{mode} and
#' \code{cutoff}. On hop distances \eqn{\delta = 1}{delta = 1} gives harmonic
#' closeness divided by \eqn{n-1} and \eqn{\delta = 0}{delta = 0} gives the share of
#' nodes reached. Bounded-distance betweenness, listed in the Centrality
#' Zoo as k-betweenness, is \code{centrality_betweenness(x, cutoff = k)}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The weighted
#'   measures use \code{weighted} (default \code{TRUE}),
#'   \code{invert_weights} (default \code{NULL}, which inverts for tna input
#'   only) and \code{alpha} (inversion exponent, default 1). Delta
#'   closeness also uses \code{cutoff} (default -1, no limit).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Borgatti, S. P., & Everett, M. G. (2006). A graph-theoretic perspective
#'   on centrality. Social Networks, 28(4), 466-484.
#'   \doi{10.1016/j.socnet.2005.11.005}.
#'
#' Agneessens, F., Borgatti, S. P., & Everett, M. G. (2017). Geodesic based
#'   centrality: Unifying the local and the global. Social Networks, 49,
#'   12-26.
#'
#' Brandes, U. (2008). On variants of shortest-path betweenness centrality
#'   and their generic computation. Social Networks, 30(2), 136-145.
#'
#' Everett, M., & Borgatti, S. P. (2005). Ego network betweenness. Social
#'   Networks, 27(1), 31-38.
#' @seealso \code{\link{centrality_betweenness}},
#'   \code{\link{centrality_harmonic}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_length_scaled_betweenness(regulation_net)
#' centrality_delta_betweenness(regulation_net, weighted = FALSE)
#' centrality_ego_betweenness(regulation_net)
#' centrality_delta_closeness(regulation_net, closeness_delta = 2)
centrality_length_scaled_betweenness <- function(x, ...) {
  df <- centrality(x, measures = "length_scaled_betweenness", ...)
  stats::setNames(df$length_scaled_betweenness, df$node)
}

#' @rdname centrality_length_scaled_betweenness
#' @param betweenness_delta Decay exponent \eqn{\delta}{delta} of delta
#'   betweenness. Default 1.
#' @export
centrality_delta_betweenness <- function(x, betweenness_delta = 1, ...) {
  df <- centrality(x, measures = "delta_betweenness",
                   betweenness_delta = betweenness_delta, ...)
  stats::setNames(df$delta_betweenness, df$node)
}

#' @rdname centrality_length_scaled_betweenness
#' @export
centrality_ego_betweenness <- function(x, ...) {
  df <- centrality(x, measures = "ego_betweenness", ...)
  stats::setNames(df$ego_betweenness, df$node)
}

#' @rdname centrality_length_scaled_betweenness
#' @param mode Direction for delta closeness: \code{"all"} (default),
#'   \code{"out"} or \code{"in"}.
#' @param closeness_delta Distance exponent \eqn{\delta}{delta} of delta
#'   closeness. Default 1.
#' @export
centrality_delta_closeness <- function(x, mode = "all", closeness_delta = 1,
                                       ...) {
  df <- centrality(x, measures = "delta_closeness", mode = mode,
                   closeness_delta = closeness_delta, ...)
  stats::setNames(df[[paste0("delta_closeness_", mode)]], df$node)
}

#' Gravity Centrality
#'
#' Gravity centrality (Ma et al. 2016) treats node masses as attracting
#' each other with a force that falls with the squared hop distance, summed
#' over the nodes within a radius \eqn{r}:
#' \deqn{G(i) = \sum_{j:\, 0 < d_{ij} \le r} \frac{m_i m_j}{d_{ij}^2}.}{
#'   G(i) = sum_{j: 0 < d_ij <= r} m_i m_j / d_ij^2.}
#' The default uses the k-shell index as mass and \eqn{r = 3} (Ma et al.
#' 2016). Degree mass without truncation is the gravity model of Li et al.
#' (2019, eq. 1), and degree mass with \code{gravity_radius = "auto"} is
#' their local gravity model (eq. 2).
#'
#' @details
#' Distances are hop counts, so edge weights are ignored. \code{mode} sets
#' the direction of both the distances and the degree or k-shell masses,
#' and \code{mode = "all"} treats edges as undirected. The \code{"auto"}
#' radius is half the mean finite positive distance, rounded to the nearest
#' integer with a minimum of 1 (Li et al. 2019, eq. 5). A radius below 1
#' gives a score of 0 for every node. \code{gravity_mass = "legacy"} with
#' \code{gravity_radius = NULL} computes
#' \eqn{\sum_j k_j s_j / d_{ij}^2}{sum_j k_j s_j / d_ij^2}, with \eqn{k_j}
#' the degree, \eqn{s_j} the k-shell index and no mass on the focal node.
#' This form differs from the formula of Li et al. (2019).
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode Direction for directed networks: \code{"all"} (default),
#'   \code{"out"} or \code{"in"}.
#' @param gravity_mass Node mass: \code{"kshell"} (default),
#'   \code{"degree"} or \code{"legacy"}.
#' @param gravity_radius Largest hop distance included: a number (default
#'   3), \code{"auto"}, or \code{NULL} for the whole network.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Ma, L.-L., Ma, C., Zhang, H.-F., & Wang, B.-H. (2016). Identifying
#'   influential spreaders in complex networks based on gravity formula.
#'   Physica A, 451, 205-212.
#'
#' Li, Z., Ren, T., Ma, X., Liu, S., Zhang, Y., & Zhou, T. (2019).
#'   Identifying influential spreaders by gravity model. Scientific
#'   Reports, 9, 8387.
#' @seealso \code{\link{centrality_extended_gravity}},
#'   \code{\link{centrality_coreness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_gravity(regulation_net)
centrality_gravity <- function(x, mode = "all", gravity_mass = "kshell",
                               gravity_radius = 3, ...) {
  df <- centrality(x, measures = "gravity", mode = mode,
                   gravity_mass = gravity_mass,
                   gravity_radius = gravity_radius, ...)
  stats::setNames(df[[paste0("gravity_", mode)]], df$node)
}
