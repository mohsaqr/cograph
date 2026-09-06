# Which cograph measures rank nodes the same way?
#
# Computes every measure on a set of real, small, undirected, simple,
# unweighted networks, takes pairwise Kendall tau between measures on each
# network and averages over networks. Writes docs/zoo/near_duplicates.csv,
# which docs/zoo/cross_coverage.R renders into
# docs/CENTRALITY-CROSS-COVERAGE.md. Run from the repository root:
#
#   Rscript docs/zoo/near_duplicates.R
#
# Needs igraphdata; uses networkx through reticulate when available.

suppressMessages(devtools::load_all(".", quiet = TRUE))

simple_undirected <- function(g) {
  g <- igraph::as_undirected(g, mode = "collapse")
  g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
  comps <- igraph::components(g)
  igraph::induced_subgraph(g, which(comps$membership == which.max(comps$csize)))
}
strip_weight <- function(g) {
  if ("weight" %in% igraph::edge_attr_names(g)) {
    igraph::delete_edge_attr(g, "weight")
  } else {
    g
  }
}
nets <- list()
add_net <- function(name, g) {
  g <- suppressMessages(igraph::upgrade_graph(g))
  nets[[name]] <<- strip_weight(simple_undirected(g))
}
add_net("karate", igraph::make_graph("Zachary"))
e <- new.env()
utils::data("kite", "UKfaculty", "macaque", "foodwebs",
            package = "igraphdata", envir = e)
add_net("kite", e$kite)
add_net("UKfaculty", e$UKfaculty)
add_net("macaque", e$macaque)
for (nm in names(e$foodwebs)) {
  g <- e$foodwebs[[nm]]
  if (igraph::vcount(g) <= 60) add_net(paste0("foodweb_", nm), g)
}
if (requireNamespace("reticulate", quietly = TRUE) &&
    reticulate::py_module_available("networkx")) {
  nx <- reticulate::import("networkx")
  from_nx <- function(G) {
    el <- do.call(rbind, lapply(reticulate::iterate(G$edges()), function(x)
      c(as.character(x[[1]]), as.character(x[[2]]))))
    igraph::graph_from_edgelist(el, directed = FALSE)
  }
  add_net("les_miserables", from_nx(nx$les_miserables_graph()))
  add_net("florentine", from_nx(nx$florentine_families_graph()))
  add_net("davis", from_nx(nx$davis_southern_women_graph()))
}
sizes <- vapply(nets, function(g) as.integer(igraph::vcount(g)), integer(1))
cat(sprintf("%d networks, %d to %d nodes\n", length(nets), min(sizes), max(sizes)))

all_measures <- list_centralities()$measure
heavy <- list_centralities(costly = TRUE)$measure

per_net <- lapply(names(nets), function(nm) {
  g <- nets[[nm]]
  memb <- igraph::membership(igraph::cluster_louvain(g))
  use <- if (igraph::vcount(g) > 40) setdiff(all_measures, heavy) else all_measures
  # Katz converges only for alpha < 1 / rho(A); the 0.1 default is invalid on
  # several of these networks, so half the valid bound is used instead.
  rho <- max(abs(eigen(as.matrix(igraph::as_adjacency_matrix(g)),
                       only.values = TRUE)$values))
  katz_alpha <- if (is.finite(rho) && rho > 0) 0.5 / rho else 0.1
  cols <- lapply(use, function(msr) {
    tryCatch({
      out <- suppressWarnings(suppressMessages(centrality(
        g, measures = msr, membership = memb, katz_alpha = katz_alpha,
        epc_seed = 1)))
      out[[2]]
    }, error = function(err) {
      cat(sprintf("    %s failed on %s: %s\n", msr, nm,
                  substr(conditionMessage(err), 1, 60)))
      rep(NA_real_, igraph::vcount(g))
    })
  })
  m <- as.matrix(as.data.frame(stats::setNames(cols, use)))
  ok <- vapply(seq_len(ncol(m)), function(j) {
    sum(!is.na(m[, j])) >= 3 && stats::sd(m[, j], na.rm = TRUE) > 0
  }, logical(1))
  m <- m[, ok, drop = FALSE]
  cat(sprintf("  %-22s n=%3d measures=%d\n", nm, igraph::vcount(g), ncol(m)))
  flush(stdout())
  suppressWarnings(stats::cor(m, method = "kendall",
                              use = "pairwise.complete.obs"))
})

measures <- Reduce(union, lapply(per_net, colnames))
avg <- cnt <- matrix(0, length(measures), length(measures),
                     dimnames = list(measures, measures))
invisible(lapply(per_net, function(tau) {
  ids <- colnames(tau)
  filled <- tau
  filled[is.na(filled)] <- 0
  avg[ids, ids] <<- avg[ids, ids] + filled
  cnt[ids, ids] <<- cnt[ids, ids] + !is.na(tau)
}))
avg <- avg / pmax(cnt, 1)
avg[cnt == 0] <- NA
diag(avg) <- NA

pairs <- which(upper.tri(avg) & !is.na(avg), arr.ind = TRUE)
out <- data.frame(
  measure_1 = measures[pairs[, 1]],
  measure_2 = measures[pairs[, 2]],
  tau = round(avg[pairs], 4),
  networks = cnt[pairs],
  stringsAsFactors = FALSE
)
out <- out[order(-abs(out$tau)), ]
utils::write.csv(out, "docs/zoo/near_duplicates.csv", row.names = FALSE)
cat(sprintf("%d measures, %d pairs, %d at |tau| >= 0.99\n",
            length(measures), nrow(out), sum(abs(out$tau) >= 0.99)))
