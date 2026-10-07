#' Recorded and unrecorded flow for map equation centrality
#' @keywords internal
#' @noRd
.cg_map_flow <- function(a, model, damping, directed) {
  n <- nrow(a)
  zero <- list(node = numeric(n), link = a * 0)
  if (!n) return(zero)
  if (any(a > 0)) {
    positive <- a > 0
    a <- a / max(a)
    if (any(a[positive] == 0)) {
      stop("map_equation weight range exceeds double precision", call. = FALSE)
    }
  }
  strength <- rowSums(a)
  if (model == "unrecorded" && !any(strength > 0)) return(zero)
  if (model == "unrecorded" && !directed) {
    flux <- a / sum(a)
    return(list(node = colSums(flux), link = flux))
  }
  transition <- a / ifelse(strength > 0, strength, 1)
  preference <- if (model == "recorded") {
    rep(1 / n, n)
  } else {
    strength / sum(strength)
  }
  # Normalize the resolvent solution. This also accounts for dangling
  # rows, which teleport to the same preference vector on their next step.
  mass <- tryCatch(as.numeric(solve(diag(n) - damping * t(transition),
                                    preference)), error = function(e) NULL)
  if (is.null(mass) || any(!is.finite(mass)) || any(mass < 0) ||
        sum(mass) <= 0) {
    stop("map_equation flow solve failed; reduce damping or weight range",
         call. = FALSE)
  }
  mass <- mass / sum(mass)
  if (model == "unrecorded") {
    flux <- transition * mass
    total <- sum(flux)
    if (!is.finite(total) || total <= 0) {
      stop("map_equation has no representable recorded link flow",
           call. = FALSE)
    }
    flux <- flux / total
    return(list(node = colSums(flux), link = flux))
  }
  jump <- (1 - damping) + damping * (strength == 0)
  flux <- (damping * transition + outer(jump, preference)) * mass
  list(node = mass, link = flux)
}

#' Map equation centrality for a supplied leaf-module partition
#' @keywords internal
#' @noRd
calculate_map_equation <- function(cg, weights = NULL, membership = NULL,
                                   damping = .85, map_flow = "unrecorded",
                                   map_convention = "paper") {
  if (!is.character(map_flow) || length(map_flow) != 1L ||
        is.na(map_flow) || !map_flow %in% c("unrecorded", "recorded")) {
    .cg_stop_bad_parameter("map_flow must be 'unrecorded' or 'recorded'")
  }
  if (!is.character(map_convention) || length(map_convention) != 1L ||
        is.na(map_convention) || !map_convention %in% c("paper", "infomap")) {
    .cg_stop_bad_parameter("map_convention must be 'paper' or 'infomap'")
  }
  if (!is.numeric(damping) || length(damping) != 1L ||
        !is.finite(damping) || damping < 0 || damping >= 1) {
    .cg_stop_bad_parameter("map_equation damping must be finite and in [0, 1)")
  }
  n <- cg$n
  if (is.null(membership)) membership <- rep(1L, n)
  if (!is.atomic(membership) || !is.null(dim(membership)) ||
        length(membership) != n || anyNA(membership)) {
    stop(errorCondition(
      "map_equation membership must give one nonmissing label per node",
      class = "cograph_bad_membership", call = NULL))
  }
  if (!is.null(names(membership))) {
    labels <- cg$labels
    if (anyDuplicated(names(membership)) ||
          !setequal(names(membership), labels)) {
      stop(errorCondition(
        "map_equation membership names must match node names exactly",
        class = "cograph_bad_membership", call = NULL))
    }
    membership <- membership[match(labels, names(membership))]
  }
  a <- .cg_candidate_adjacency(cg, weights, "map_equation")
  diag(a) <- 0
  flow <- .cg_map_flow(a, map_flow, damping, cg$directed)
  score <- numeric(n)
  groups <- split(seq_len(n), as.character(membership))
  for (ids in groups) {
    exit_rate <- if (map_convention == "paper") {
      sum(flow$link[ids, setdiff(seq_len(n), ids), drop = FALSE])
    } else {
      0
    }
    for (i in ids) {
      # Sum remaining symbols directly to avoid subtracting nearly equal
      # module and focal masses. Evaluate the logarithm without p/rest overflow.
      rest <- sum(flow$node[setdiff(ids, i)]) + exit_rate
      own <- flow$node[i]
      if (rest > 0 && own > 0) {
        ratio_log <- if (own < rest) {
          log1p(own / rest)
        } else {
          log(own + rest) - log(rest)
        }
        score[i] <- rest * ratio_log / log(2)
      }
    }
  }
  score
}

#' Map Equation Centrality
#'
#' Map equation centrality (Blocker et al. 2022) is the reduction in
#' codelength, in bits, when a node's codeword is removed from its module
#' codebook and the codebook is redesigned. With \eqn{p}{p} the visit rate
#' of the node and \eqn{s}{s} the use rate of its module codebook,
#' \deqn{MEC_i = -(s - p) \log_2 \frac{s - p}{s}.}{
#'   MEC_i = -(s - p) log2((s - p) / s).}
#'
#' @details
#' The partition is held fixed and is given by \code{membership}, and
#' \code{NULL} places every node in one module. With
#' \code{map_convention = "paper"} the rate \eqn{s}{s} includes module
#' exits (equations 2 and 9-11), and with \code{"infomap"} it includes node
#' visits only, which reproduces Infomap 2.15.1 and Table 1 of the paper.
#' With \code{map_flow = "unrecorded"} (Lambiotte and Rosvall 2012) the
#' walk teleports in proportion to out-strength and only link steps are
#' recorded, so on an undirected network the visit rates are proportional
#' to strength for every \code{damping}. With \code{"recorded"} the walk
#' teleports uniformly and teleportation steps are recorded. Direction is
#' kept, loops are removed and edge weights must be finite and
#' nonnegative. Under unrecorded flow an isolated node scores zero. An
#' invalid \code{membership}, \code{map_flow} or \code{map_convention}, or
#' a \code{damping} outside \eqn{[0, 1)}{[0, 1)}, raises an error.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param membership One module label per node. \code{NULL} (default)
#'   gives one module. A named vector is matched to the node names.
#' @param map_flow \code{"unrecorded"} (default) for unrecorded link
#'   teleportation or \code{"recorded"} for recorded uniform teleportation.
#' @param map_convention \code{"paper"} (default) includes module exits in
#'   the codebook rate. \code{"infomap"} uses node visits only.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (use edge weights, default \code{TRUE}) and
#'   \code{damping} (probability of following a link, default 0.85).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references Blocker, C., Nieves, J. C. and Rosvall, M. (2022). Map equation
#'   centrality: community-aware centrality based on the map equation. Applied
#'   Network Science, 7, 56. \doi{10.1007/s41109-022-00477-9}.
#'
#' Lambiotte, R. and Rosvall, M. (2012). Ranking and clustering of nodes in
#'   networks with smart teleportation. Physical Review E, 85, 056107.
#'   \doi{10.1103/PhysRevE.85.056107}.
#' @seealso \code{\link{centrality_community_based}},
#'   \code{\link{centrality_participation}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_map_equation(regulation_net, membership = rep(1:2, each = 5))
centrality_map_equation <- function(x, membership = NULL,
                                    map_flow = "unrecorded",
                                    map_convention = "paper", ...) {
  df <- centrality(x, measures = "map_equation", membership = membership,
                   map_flow = map_flow, map_convention = map_convention, ...)
  stats::setNames(df$map_equation, df$node)
}
