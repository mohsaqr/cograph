#' Validate the mass, exponent and stopping parameters shared by IRA/IIRA
#' @keywords internal
#' @noRd
.cg_check_ira_args <- function(alpha, tol, max_iter) {
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha)) {
    stop("ira_alpha must be a single finite number", call. = FALSE)
  }
  if (!is.numeric(tol) || length(tol) != 1L || !is.finite(tol) || tol <= 0) {
    stop("ira_tol must be a single finite positive number", call. = FALSE)
  }
  if (!is.numeric(max_iter) || length(max_iter) != 1L ||
        !is.finite(max_iter) || max_iter < 1 || max_iter != trunc(max_iter)) {
    stop("ira_max_iter must be a single whole number of at least one",
         call. = FALSE)
  }
  invisible(NULL)
}

#' Iterative resource allocation
#' @keywords internal
#' @noRd
calculate_ira <- function(g, mass = "coreness", alpha = 1, tol = 1e-6,
                          max_iter = 1000) {
  mass <- match.arg(mass, c("coreness", "degree"))
  .cg_check_ira_args(alpha, tol, max_iter)
  a <- .cg_undirected_view(.cg_path_matrix(g, NULL))
  diag(a) <- 0
  n <- nrow(a)
  if (!n) return(numeric())
  theta <- .cg_resource_mass(a, n, mass)
  weight <- numeric(n)
  linked <- theta > 0
  weight[linked] <- theta[linked]^alpha
  .cg_resource_settle(.cg_resource_matrix(a, weight), tol, max_iter, "ira")
}

#' Improved iterative resource allocation
#' @keywords internal
#' @noRd
calculate_iira <- function(g, mass = "coreness", beta = 0.2, steps = 50) {
  mass <- match.arg(mass, c("coreness", "degree"))
  if (!is.numeric(beta) || length(beta) != 1L || !is.finite(beta) ||
        beta <= 0 || beta > 1) {
    stop("iira_beta must be a single number in (0, 1]", call. = FALSE)
  }
  if (!is.numeric(steps) || length(steps) != 1L || !is.finite(steps) ||
        steps < 0 || steps != trunc(steps)) {
    stop("iira_steps must be a single nonnegative whole number", call. = FALSE)
  }
  a <- .cg_undirected_view(.cg_path_matrix(g, NULL))
  diag(a) <- 0
  n <- nrow(a)
  if (!n) return(numeric())
  theta <- .cg_resource_mass(a, n, mass)
  psi <- 1 - (1 - beta)^rowSums(a)
  .cg_resource_steps(.cg_resource_matrix(a, theta, psi * theta), steps)
}

#' Iterative resource allocation (IRA)
#'
#' Every node starts with one unit of resource and hands it to its
#' neighbours in proportion to the *receiver's* centrality, repeatedly,
#' until the amounts stop moving. The share node \eqn{j} sends to a
#' neighbour \eqn{i} is
#' \eqn{a_{ij}=\theta_i^{\alpha}/\sum_{u\in\Gamma(j)}\theta_u^{\alpha}},
#' the recursion is \eqn{I(t+1)=AI(t)} from \eqn{I(0)=(1,\dots,1)}, and the
#' steady state \eqn{I} ranks the spreaders. Because every non-isolate
#' column of \eqn{A} sums to one, the total resource is conserved:
#' \eqn{\sum_i I_i(t)=n} at every step on a graph with no isolates, and
#' each connected component keeps its own vertex count.
#'
#' The equilibrium has a closed form. Writing \eqn{s_i=\sum_{u\in\Gamma(i)}
#' \theta_u^{\alpha}}, the limit is \eqn{I_i\propto\theta_i^{\alpha}s_i}
#' within each component, scaled so the component's scores sum to its size.
#' On the source's own figure 1(a) that reproduces the printed
#' \eqn{[15/8, 5/4, 5/4, 5/16, 5/16]} exactly. cograph nevertheless
#' iterates, because the iteration is what the source defines and what its
#' table reports, and because the closed form is a limit that need not
#' exist; see the next paragraph.
#'
#' \strong{The iteration does not always converge, and cograph says so.}
#' \eqn{A} is the transition matrix of a reversible walk, so on a bipartite
#' component it has an eigenvalue of exactly \eqn{-1}. The coefficient of
#' that eigenvector in \eqn{I(0)=(1,\dots,1)} is the difference in size
#' between the component's two vertex classes, so the iteration settles into
#' a period-two cycle, never meets \code{ira_tol}, and returns a value that
#' depends on the parity of the last step. The three-star alternates for
#' ever between \eqn{(3,1/3,1/3,1/3)} and \eqn{(1,1,1,1)}, while the
#' four-path, whose classes are equal, converges to
#' \eqn{(2/3,4/3,4/3,2/3)}. Neither the source nor the Centrality Zoo
#' mentions this. cograph runs the source's own rule, stops at
#' \code{ira_max_iter}, raises a \code{cograph_no_converge} warning naming
#' the largest remaining change, and returns \eqn{I(\code{ira_max_iter})}.
#' It does not silently report that iterate as an equilibrium, and it does
#' not substitute the average of the two alternating iterates, which would
#' converge but is not the source's rule. Every graph in the source's own
#' figure 1 carries a triangle and converges.
#'
#' The Centrality Zoo (section 2.204) states the transpose,
#' \eqn{p_{ij}=a_{ij}c_j^{\alpha}/\sum_k a_{ik}c_k^{\alpha}}, and asks for
#' the principal left eigenvector of \eqn{P}. That is the same object up to
#' scale on a graph where the limit exists, but it is not the source's
#' finite iteration: it sidesteps the parity problem instead of reporting
#' it, and it carries no \eqn{\sum_i I_i=n} scale.
#'
#' Uses the simple undirected unweighted skeleton, which is the source
#' domain: either arc creates one edge, parallel edges count once and loops
#' are removed. Edge weights, mode, cutoff and path-weight inversion are
#' ignored. An isolate is in nobody's neighbourhood, so it receives nothing
#' and its own unit is not passed on: it scores zero from the first step,
#' which is the value of the source's empty sum and not an accidental zero,
#' and it is the reason \eqn{\sum_i I_i=n} is stated only for graphs with no
#' isolates. Empty graphs return no scores. Cost is one dense \eqn{n^2}
#' matrix plus one matrix-vector product per iteration.
#'
#' Numerical verification establishes agreement with the source equations
#' and with every value printed in the source's table 1, not parity with
#' author software, which does not exist, and not any claim about spreading
#' performance.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ira_mass Node centrality \eqn{\theta}: \code{"coreness"}
#'   (default, the k-shell index the source's worked example uses) or
#'   \code{"degree"}. The source also mentions closeness and betweenness.
#' @param ira_alpha Exponent \eqn{\alpha} on the mass, a single finite
#'   number; default one, the only value the source uses.
#' @param ira_tol Stopping tolerance \eqn{\varepsilon} on the largest
#'   absolute change between successive iterates; default \code{1e-6}, the
#'   source's own value.
#' @param ira_max_iter Iteration bound, a single whole number of at least
#'   one; default 1000. Reaching it raises \code{cograph_no_converge}.
#' @param ... Additional arguments to \code{\link{centrality}}.
#' @return Named numeric vector in input node order.
#' @references Ren, Z.-M., Zeng, A., Chen, D.-B., Liao, H. and Liu, J.-G.
#'   (2014). Iterative resource allocation for ranking spreaders in complex
#'   networks. EPL (Europhysics Letters), 106(4), 48005. Equations 1-3 on
#'   page 2 and the algorithm i)-iii) on page 3, read in the author
#'   postprint recovered from the Internet Archive.
#'   \doi{10.1209/0295-5075/106/48005}.
#' @seealso \code{\link{centrality_iira}} for the improved variant, and
#'   \code{\link{list_centralities}} for the catalogue.
#' @export
#' @examples
#' # The source's figure 1(a): a triangle with two pendants on one corner.
#' # The printed steady state is 15/8, 5/4, 5/4, 5/16, 5/16.
#' fig1a <- igraph::make_graph(c(1, 2, 1, 3, 2, 3, 1, 4, 1, 5),
#'                             directed = FALSE)
#' centrality_ira(fig1a)
#'
#' # The source's other mass, and a nonlinear exponent
#' centrality_ira(fig1a, ira_mass = "degree", ira_alpha = 2)
centrality_ira <- function(x, ira_mass = "coreness", ira_alpha = 1,
                           ira_tol = 1e-6, ira_max_iter = 1000, ...) {
  df <- centrality(x, measures = "ira", ira_mass = ira_mass,
                   ira_alpha = ira_alpha, ira_tol = ira_tol,
                   ira_max_iter = ira_max_iter, ...)
  stats::setNames(df$ira, df$node)
}

#' Improved iterative resource allocation (IIRA)
#'
#' IIRA is \code{\link{centrality_ira}} with the receiver's share scaled by
#' how much of a spreading process that receiver could actually carry:
#' \eqn{a_{ij}=[1-(1-\beta)^{k_i}]\,\theta_i
#' (\sum_{u\in\Gamma(j)}\theta_u)^{-1}}, where \eqn{k_i} is the degree of
#' \eqn{i} and \eqn{\beta} the spreading rate. The recursion and the
#' initial condition \eqn{I(0)=(1,\dots,1)} are unchanged; there is no
#' \eqn{\alpha} exponent, and the denominator keeps the plain masses.
#'
#' \strong{The scores are tiny and only their order means anything.} The
#' factor \eqn{\psi_i=1-(1-\beta)^{k_i}} is strictly below one, so every
#' column of \eqn{A} sums to less than one, the spectral radius is below
#' one, and \eqn{I(t)\to 0} geometrically. The source runs exactly
#' \eqn{t=50} steps and prints an \eqn{I(50)} of order \eqn{10^{-20}};
#' cograph returns that raw vector, so the printed example is reproducible,
#' and \code{normalized = TRUE} max-scales it into \eqn{[0,1]} for reading.
#' Never compare raw IIRA scores across connected components: each
#' component decays at its own rate, so after \code{iira_steps} steps they
#' sit on different exponential scales. A large \code{iira_steps} underflows
#' to zero.
#'
#' \strong{The Centrality Zoo entry is not this formula.} Section 2.185
#' prints \eqn{p_{ij}=(1-(1-\beta)^{d_i})a_{ij}c_i/\sum_k a_{ik}c_k}, which
#' pairs the numerator's index with the denominator's own neighbourhood; the
#' source pairs them with opposite sets. As printed, the Zoo's row sums are
#' \eqn{\psi_i c_i d_i/\sum_{k\in N(i)}c_k}, so its matrix is stochastic in
#' neither direction although the entry calls it stochastic, and it does not
#' reproduce the source's printed matrix or its printed \eqn{I(50)}.
#' cograph implements the source.
#'
#' Uses the simple undirected unweighted skeleton, which is the source
#' domain: either arc creates one edge, parallel edges count once and loops
#' are removed. Edge weights, mode, cutoff and path-weight inversion are
#' ignored. An isolate has an empty neighbour sum and \eqn{\psi=0}, so it
#' scores zero from the first step; that is the value of the source's empty
#' sum, not an accidental zero. \code{iira_steps = 0} returns the initial
#' \eqn{I(0)}, a vector of ones. Empty graphs return no scores. Cost is one
#' dense \eqn{n^2} matrix plus \code{iira_steps} matrix-vector products.
#'
#' The version of record was not read: what was read is the author preprint
#' arXiv:1505.03214v1, whose method section, worked example and figures
#' carry the definition reproduced here. Numerical verification establishes
#' agreement with those equations and with every value printed in the
#' preprint's figure 2 example, not parity with author software, which does
#' not exist, and not any claim about spreading performance.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ira_mass Node centrality \eqn{\theta}: \code{"coreness"}
#'   (default, the k-shell index the source's worked example uses) or
#'   \code{"degree"}. Shared with \code{\link{centrality_ira}}.
#' @param iira_beta Spreading rate \eqn{\beta}, a single number in
#'   \eqn{(0,1]}; default 0.2, the source's worked-example value. The
#'   source sweeps \eqn{\beta} in its experiments and recommends no other
#'   default.
#' @param iira_steps Number of iterations \eqn{t}, a single nonnegative
#'   whole number; default 50, the source's worked-example value. Zero
#'   returns \eqn{I(0)}.
#' @param ... Additional arguments to \code{\link{centrality}}.
#' @return Named numeric vector in input node order.
#' @references Zhong, L.-F., Liu, J.-G. and Shang, M.-S. (2015). Iterative
#'   resource allocation based on propagation feature of node for
#'   identifying the influential nodes. Physics Letters A, 379(38),
#'   2272-2276. Equations 1, 2 and 4 and figure 2 on page 2 of the author
#'   preprint arXiv:1505.03214v1, which is what was read.
#'   \doi{10.1016/j.physleta.2015.05.021}.
#' @seealso \code{\link{centrality_ira}} for the measure this improves, and
#'   \code{\link{list_centralities}} for the catalogue.
#' @export
#' @examples
#' # The source's figure 2, whose printed I(50) is
#' # 8.19e-20, 4.32e-20, 4.32e-20, 6.7e-21, 6.7e-21
#' fig2 <- igraph::make_graph(c(1, 2, 1, 3, 2, 3, 1, 4, 1, 5),
#'                            directed = FALSE)
#' centrality_iira(fig2)
#'
#' # Only the order carries meaning, so max-scale for reading
#' centrality_iira(fig2, normalized = TRUE)
centrality_iira <- function(x, ira_mass = "coreness", iira_beta = 0.2,
                            iira_steps = 50, ...) {
  df <- centrality(x, measures = "iira", ira_mass = ira_mass,
                   iira_beta = iira_beta, iira_steps = iira_steps, ...)
  stats::setNames(df$iira, df$node)
}
