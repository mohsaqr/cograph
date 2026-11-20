# Inputs and reference-side helpers shared by the equivalence tests, in base R. Nothing here calls cograph:
# each function builds an input both sides read, or shapes the reference's result.

#' A random graph as an adjacency matrix, the same in every run
#'
#' @param nodes Number of nodes.
#' @param density Probability of a tie.
#' @param seed The seed of the draw.
#' @param directed Whether ties are directed.
#' @param weighted Whether ties carry weights (uniform on 0.1 to 1) instead of 1.
#' @param digits Decimals the weights are rounded to. Two decimals make equal path lengths likely; six make a
#'   tie between two weighted paths unlikely, so shortest-path counts do not hinge on rounding.
#' @return A square numeric matrix with zero diagonal and dimnames `N1`, `N2`, ...
ref_graph <- function(nodes, density = 0.3, seed = 1L, directed = FALSE, weighted = FALSE, digits = 2L) {
  stopifnot("`nodes` must be one whole number of at least 2" = length(nodes) == 1L && nodes >= 2,
            "`density` must be one probability" = length(density) == 1L && density >= 0 && density <= 1)
  old <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) get(".Random.seed", envir = globalenv())
  on.exit(if (is.null(old)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", old, envir = globalenv()),
          add = TRUE, after = FALSE)
  set.seed(seed)
  ties <- matrix(stats::runif(nodes * nodes) < density, nodes, nodes)
  weights <- matrix(if (weighted) round(stats::runif(nodes * nodes, 0.1, 1), digits) else 1, nodes, nodes)
  graph <- ties * weights
  diag(graph) <- 0
  if (!directed) {
    graph[lower.tri(graph)] <- 0
    graph <- graph + t(graph)
  }
  dimnames(graph) <- list(paste0("N", seq_len(nodes)), paste0("N", seq_len(nodes)))
  graph
}

#' A connected random graph: `ref_graph()` plus a ring through all nodes
#'
#' @inheritParams ref_graph
#' @return A square numeric matrix; every node reaches every other.
ref_connected_graph <- function(nodes, density = 0.3, seed = 1L, directed = FALSE, weighted = FALSE, digits = 2L) {
  graph <- ref_graph(nodes, density, seed, directed, weighted, digits)
  ring <- cbind(seq_len(nodes), c(seq_len(nodes)[-1L], 1L))
  missing_tie <- graph[ring] == 0
  graph[ring[missing_tie, , drop = FALSE]] <- 1
  if (!directed) graph[ring[missing_tie, 2:1, drop = FALSE]] <- 1
  graph
}

#' Evaluate an expression on a seeded random stream and restore the caller's stream afterwards
#'
#' For a function that draws from the caller's stream and takes no seed (community_fluid(),
#' network_small_world()).
#'
#' @param seed The seed.
#' @param expr The expression, evaluated after `set.seed(seed)`.
#' @return The value of `expr`.
ref_with_seed <- function(seed, expr) {
  old <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) get(".Random.seed", envir = globalenv())
  on.exit(if (is.null(old)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", old, envir = globalenv()),
          add = TRUE, after = FALSE)
  set.seed(seed)
  expr
}

#' Zachary's karate club as an adjacency matrix (34 nodes, 78 ties, from igraph)
#'
#' @return A symmetric 0/1 matrix with dimnames `1` to `34`.
ref_karate <- function() {
  graph <- igraph::as_adjacency_matrix(igraph::make_graph("Zachary"), sparse = FALSE)
  dimnames(graph) <- list(as.character(seq_len(34L)), as.character(seq_len(34L)))
  graph
}

#' An adjacency matrix as an igraph graph, built by igraph itself
#'
#' @param graph A square matrix with dimnames.
#' @param directed Whether the graph is directed (default: the matrix is not symmetric).
#' @param weighted Whether the entries are kept as the edge attribute `weight` (default: some entry is not 0 or 1).
#' @return An igraph graph with vertex names.
ref_igraph <- function(graph, directed = !isSymmetric(unname(graph)), weighted = !all(graph %in% c(0, 1))) {
  igraph::graph_from_adjacency_matrix(graph, mode = if (directed) "directed" else "undirected",
                                      weighted = if (weighted) TRUE else NULL, diag = TRUE)
}

#' A reference's vector with the node names of the input
#'
#' @param values An unnamed numeric vector in node order.
#' @param graph The adjacency matrix the values were computed on.
#' @return `values` named by `rownames(graph)`.
ref_named <- function(values, graph) {
  stopifnot("one value per node is needed" = length(values) == nrow(graph))
  stats::setNames(as.numeric(values), rownames(graph))
}

#' Reference values as a node table, the shape `centrality()` returns
#'
#' @param ... Named numeric vectors, one per column, each named by node.
#' @return A data frame with the column `node` and one column per argument, in the order of the first
#'   vector's names.
ref_nodes <- function(...) {
  columns <- list(...)
  stopifnot("every column must be a named vector" = length(columns) >= 1L && all(nzchar(names(columns))),
            "the first column must be named by node" = !is.null(names(columns[[1L]])))
  nodes <- names(columns[[1L]])
  data.frame(node = nodes, lapply(columns, \(values) unname(as.numeric(values[nodes]))), check.names = FALSE)
}

#' The Python interpreter that holds the reference modules (networkx, numpy, scipy)
#'
#' Taken from `VERIFY_PYTHON`, else `RETICULATE_PYTHON`, else `~/equiv-py/bin/python` (the ci machine).
#'
#' @return A path to an existing interpreter; raises an error when there is none.
ref_python <- function() {
  candidates <- path.expand(c(Sys.getenv("VERIFY_PYTHON"), Sys.getenv("RETICULATE_PYTHON"), "~/equiv-py/bin/python"))
  found <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(found)) stop("No Python with networkx: set VERIFY_PYTHON to an interpreter that imports networkx.", call. = FALSE)
  found[[1L]]
}

# The Python side of ref_networkx(): builds the graph from the JSON it is given, evaluates each expression with
# `nx`, `np`, `G` in scope, and writes the results back as JSON (a dict becomes {node: value}).
.ref_networkx_script <- '
import json, math, sys
import networkx as nx
import numpy as np
spec = json.load(open(sys.argv[1]))
G = nx.DiGraph() if spec["directed"] else nx.Graph()
G.add_nodes_from(spec["nodes"])
for u, v, w in spec["edges"]:
    G.add_edge(u, v, weight=w, distance=w)
def plain(value):
    if isinstance(value, dict):
        return {str(k): plain(v) for k, v in value.items()}
    if isinstance(value, (list, tuple, set, frozenset)):
        return [plain(v) for v in value]
    if isinstance(value, (np.ndarray,)):
        return plain(value.tolist())
    if isinstance(value, (bool, np.bool_)):
        return bool(value)
    if isinstance(value, (int, float, np.integer, np.floating)):
        value = float(value)
        if math.isnan(value):
            return "NaN"
        if math.isinf(value):
            return "Inf" if value > 0 else "-Inf"
        return value
    return str(value)
scope = {"nx": nx, "np": np, "G": G}
out = {"networkx": nx.__version__}
for name, expression in spec["calls"].items():
    out[name] = plain(eval(expression, scope))
json.dump(out, open(sys.argv[2], "w"))
'

#' Measures computed by NetworkX on an adjacency matrix
#'
#' The graph is built in NetworkX from the matrix: one edge per non-zero cell (the upper triangle of a symmetric
#' matrix when `directed` is `FALSE`), its value as the edge attributes `weight` and `distance`. Each
#' expression is evaluated by Python with `nx`, `np` and `G` in scope.
#'
#' @param graph A square numeric matrix with dimnames.
#' @param calls A named character vector of Python expressions, e.g.
#'   `c(betweenness = "nx.betweenness_centrality(G, normalized=False)")`.
#' @param directed Whether the graph is directed (default: the matrix is not symmetric).
#' @return A named list: one element per call (a dict becomes a numeric vector named by node, a number a number,
#'   a list a vector or list), and `networkx`, the version that computed them.
ref_networkx <- function(graph, calls, directed = !isSymmetric(unname(graph))) {
  stopifnot("`graph` must be a square matrix with dimnames" = is.matrix(graph) && nrow(graph) == ncol(graph) &&
              !is.null(rownames(graph)),
            "`calls` must be named Python expressions" = is.character(calls) && all(nzchar(names(calls))))
  cells <- which(graph != 0, arr.ind = TRUE)
  if (!directed) cells <- cells[cells[, 1L] <= cells[, 2L], , drop = FALSE]
  edges <- lapply(seq_len(nrow(cells)), \(i) list(rownames(graph)[cells[i, 1L]], colnames(graph)[cells[i, 2L]],
                                                 graph[cells[i, 1L], cells[i, 2L]]))
  spec <- list(directed = directed, nodes = as.list(rownames(graph)), edges = edges, calls = as.list(calls))
  files <- c(input = tempfile(fileext = ".json"), script = tempfile(fileext = ".py"), output = tempfile(fileext = ".json"))
  on.exit(unlink(files), add = TRUE, after = FALSE)
  jsonlite::write_json(spec, files[["input"]], auto_unbox = TRUE, digits = NA)
  writeLines(.ref_networkx_script, files[["script"]])
  # `-s` keeps the user's site packages out; not `-I`, which implies `-E` and would drop PYTHONHASHSEED, the seed
  # that makes a NetworkX result following the order of a set (group betweenness) the same run after run
  status <- system2(ref_python(), c("-s", shQuote(files[["script"]]), shQuote(files[["input"]]), shQuote(files[["output"]])),
                    env = sprintf("PYTHONHASHSEED=%s", Sys.getenv("PYTHONHASHSEED", "0")))
  if (!identical(status, 0L)) stop(sprintf("NetworkX failed (exit status %s).", status), call. = FALSE)
  result <- jsonlite::read_json(files[["output"]], simplifyVector = FALSE)
  # verify.json states the version of a reference that is not an R package; the run must have used that one
  stated <- jsonlite::read_json("verify.json")$references$networkx$version
  if (!identical(result$networkx, stated)) {
    stop(sprintf("NetworkX %s ran, verify.json states %s.", result$networkx, toString(stated)), call. = FALSE)
  }
  version <- result$networkx
  result$networkx <- NULL
  numeric_of <- \(value) vapply(value, \(v) switch(as.character(v), "NaN" = NaN, "Inf" = Inf, "-Inf" = -Inf, as.numeric(v)),
                                numeric(1))
  lapply(result, \(value) {
    if (is.list(value) && !is.null(names(value)) && all(vapply(value, \(v) length(v) == 1L, logical(1)))) {
      numeric_of(value)
    } else if (is.list(value) && all(vapply(value, \(v) length(v) == 1L && !is.list(v), logical(1)))) {
      if (all(vapply(value, is.numeric, logical(1)))) unlist(value) else value
    } else if (length(value) == 1L && !is.list(value)) {
      if (is.character(value)) numeric_of(list(value)) else value
    } else {
      value
    }
  }) |> c(list(networkx = version))
}
