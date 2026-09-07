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
calculate_map_equation <- function(g, weights = NULL, membership = NULL,
                                   damping = .85, map_flow = "unrecorded",
                                   map_convention = "paper") {
  if (!is.character(map_flow) || length(map_flow) != 1L ||
        is.na(map_flow) || !map_flow %in% c("unrecorded", "recorded")) {
    stop("map_flow must be 'unrecorded' or 'recorded'", call. = FALSE)
  }
  if (!is.character(map_convention) || length(map_convention) != 1L ||
        is.na(map_convention) || !map_convention %in% c("paper", "infomap")) {
    stop("map_convention must be 'paper' or 'infomap'", call. = FALSE)
  }
  if (!is.numeric(damping) || length(damping) != 1L ||
        !is.finite(damping) || damping < 0 || damping >= 1) {
    stop("map_equation damping must be finite and in [0, 1)", call. = FALSE)
  }
  n <- igraph::vcount(g)
  if (is.null(membership)) membership <- rep(1L, n)
  if (!is.atomic(membership) || !is.null(dim(membership)) ||
        length(membership) != n || anyNA(membership)) {
    stop("map_equation membership must give one nonmissing label per node",
         call. = FALSE)
  }
  if (!is.null(names(membership))) {
    labels <- igraph::V(g)$name %||% as.character(seq_len(n))
    if (anyDuplicated(names(membership)) ||
          !setequal(names(membership), labels)) {
      stop("map_equation membership names must match node names exactly",
           call. = FALSE)
    }
    membership <- membership[match(labels, names(membership))]
  }
  a <- .cg_candidate_adjacency(g, weights, "map_equation")
  diag(a) <- 0
  flow <- .cg_map_flow(a, map_flow, damping, igraph::is_directed(g))
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

#' Map equation centrality with explicit coding and flow conventions
#'
#' Measures the reduction in codelength when a node is silenced, comparing
#' the original codebook used without that node's codeword to a redesigned
#' codebook. It does not remove the node or recompute the network partition.
#' The score in bits is -(s-p) log2((s-p)/s), where p is the node visit rate
#' and s is the rate of use of its module's codebook.
#'
#' With \code{map_convention = "paper"}, s includes module node visits and
#' module exits, as explicitly defined in Blocker et al. (2022), equations
#' 2 and 9-11. With \code{"infomap"}, s includes node visits only, reproducing
#' Infomap 2.15.1's modular centrality and the paper's Table 1. These two
#' conventions differ when a module has exit flow. The published table does
#' not reproduce the equation's exit-inclusive convention. Both conventions
#' give nonnegative scores; the continuous boundary value is zero if p or
#' s-p is zero. The Zoo summary uses a different codelength subtraction.
#'
#' The default unrecorded link-teleportation model teleports proportionally
#' to out-strength, then records only link-following steps and normalizes
#' their total flow to one. On undirected inputs this gives visit rates
#' proportional to strength, independent of damping. Recorded node
#' teleportation uses uniform destinations and records all moves, including
#' teleportation. Both models teleport away from dangling nodes. Damping is
#' the probability of following a link, default 0.85; it must be less than one.
#' Recorded teleportation remains directed even for reciprocal input arcs.
#'
#' Weights are nonnegative interaction strengths; zero weights are absent.
#' Direction is retained, and undirected edges become reciprocal arcs. Loops
#' are removed. Parallel edges follow centrality's simplify rule; remaining
#' parallel weights sum. Mode, inversion and cutoff are ignored. With no
#' positive edges, unrecorded flow and scores are zero by cograph convention;
#' recorded flow is uniform. Empty and singleton graphs return no scores and
#' zero respectively. Unrecorded isolates score zero; recorded isolates may
#' have positive scores because their teleportation visits are recorded.
#'
#' The partition is held fixed. NULL means one module containing every node,
#' the paper's one-level case. For a hierarchical partition, supply globally
#' unique leaf-module labels: silencing affects only that leaf codebook, so
#' higher levels cancel in the score difference. This function does not run
#' community detection or claim that a supplied partition is optimal.
#'
#' Dense flow calculation takes O(n cubed) time and O(n squared) memory;
#' unrecorded undirected flow takes O(n squared). Extreme weight ranges or
#' numerically singular flow solves raise errors. Optional maximum scaling
#' changes the raw bit units; tiny relative scores may underflow.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param membership One module label per node; NULL gives one module.
#'   Unnamed vectors follow input order. Named vectors must match all input
#'   node names exactly and are reordered to input order.
#' @param map_flow \code{"unrecorded"} (default) for unrecorded link
#'   teleportation, or \code{"recorded"} for recorded uniform node
#'   teleportation.
#' @param map_convention \code{"paper"} (default) includes the exit symbol;
#'   \code{"infomap"} reproduces the visit-only author implementation/table.
#' @param ... Additional arguments to \code{\link{centrality}}, including
#'   \code{damping}, \code{weighted}, \code{simplify}, and \code{normalized}.
#' @return Named numeric vector in input node order.
#' @references Blocker, C., Nieves, J. C. and Rosvall, M. (2022). Map equation
#'   centrality: community-aware centrality based on the map equation.
#'   Applied Network Science, 7, 56. \doi{10.1007/s41109-022-00477-9}.
#'   Lambiotte, R. and Rosvall, M. (2012). Ranking and clustering of nodes in
#'   networks with smart teleportation. Physical Review E, 85, 056107.
#'   \doi{10.1103/PhysRevE.85.056107}.
#' @export
#' @examples
#' g <- igraph::make_graph("Zachary")
#' centrality_map_equation(g)
#' centrality_map_equation(g, membership = rep(1:2, each = 17),
#'                         map_convention = "infomap")
centrality_map_equation <- function(x, membership = NULL,
                                    map_flow = "unrecorded",
                                    map_convention = "paper", ...) {
  df <- centrality(x, measures = "map_equation", membership = membership,
                   map_flow = map_flow, map_convention = map_convention, ...)
  stats::setNames(df$map_equation, df$node)
}
