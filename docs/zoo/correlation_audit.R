# Correlation audit of native defaults and original-candidate parameter calls.
# Run from the repository root. Does not change implementations or coverage.
suppressMessages(devtools::load_all(".", quiet = TRUE))
here <- "docs/zoo/correlation-audit"
private <- "local_testing_and_equivalence/correlation_audit"
dir.create(here, recursive = TRUE, showWarnings = FALSE)
dir.create(private, recursive = TRUE, showWarnings = FALSE)
meta <- list_centralities()
ledger <- utils::read.csv("docs/zoo/parameter_candidate_status.csv")
ledger <- ledger[ledger$status == "implemented", ]
configs <- stats::setNames(lapply(meta$measure, function(m) list(measures = m)),
                          meta$measure)
signature <- function(args) jsonlite::toJSON(args[sort(names(args))],
                                            auto_unbox = TRUE, null = "null")
keys <- vapply(configs, signature, character(1))
labels <- list()
k <- 3
for (i in seq_len(nrow(ledger))) {
  expr <- as.list(match.call(centrality, parse(text = ledger$call[i])[[1]]))[-1]
  expr$x <- NULL
  args <- lapply(expr, eval)
  for (nm in setdiff(names(args), "measures")) {
    default <- eval(formals(centrality)[[nm]])
    if (isTRUE(all.equal(args[[nm]], default))) args[nm] <- NULL
  }
  sig <- signature(args)
  found <- match(sig, keys)
  if (is.na(found)) {
    id <- paste0("candidate_", i)
    configs[[id]] <- args
    keys[id] <- sig
  } else {
    id <- names(keys)[found]
  }
  labels[[i]] <- data.frame(zoo_label = ledger$zoo_label[i], config = id,
                            call = ledger$call[i])
}
candidate_map <- do.call(rbind, labels)
utils::write.csv(candidate_map, file.path(here, "candidate_map.csv"),
                 row.names = FALSE)
config_table <- data.frame(
  config = names(configs),
  measure = vapply(configs, `[[`, character(1), "measures"),
  arguments = vapply(configs, signature, character(1)),
  native_default = names(configs) %in% meta$measure
)
utils::write.csv(config_table, file.path(here, "configurations.csv"),
                 row.names = FALSE)
nets <- list()
manifest <- list()
add_net <- function(name, g, family, source) {
  g <- suppressMessages(igraph::upgrade_graph(g))
  original_n <- igraph::vcount(g)
  original_m <- igraph::ecount(g)
  directed <- igraph::is_directed(g)
  weighted <- "weight" %in% igraph::edge_attr_names(g)
  g <- igraph::as_undirected(g, mode = "collapse")
  g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
  components <- igraph::components(g)
  g <- igraph::induced_subgraph(g, which(components$membership ==
                                         which.max(components$csize)))
  if ("weight" %in% igraph::edge_attr_names(g)) {
    g <- igraph::delete_edge_attr(g, "weight")
  }
  if (is.null(igraph::V(g)$name)) igraph::V(g)$name <- as.character(
    seq_len(igraph::vcount(g)))
  stopifnot(!anyDuplicated(igraph::V(g)$name), igraph::is_simple(g),
            igraph::is_connected(g), !igraph::is_directed(g))
  nets[[name]] <<- g
  manifest[[name]] <<- data.frame(
    network = name, family, source, original_n, original_m,
    original_directed = directed, original_weighted = weighted,
    n = igraph::vcount(g), m = igraph::ecount(g),
    retained_fraction = igraph::vcount(g) / original_n,
    adjacency_sha256 = digest::digest(as.matrix(igraph::as_adjacency_matrix(g)),
                                      algo = "sha256")
  )
}
add_net("karate", igraph::make_graph("Zachary"), "social", "igraph Zachary")
e <- new.env()
utils::data("kite", "UKfaculty", "macaque", "foodwebs",
            package = "igraphdata", envir = e)
add_net("kite", e$kite, "illustrative", "igraphdata kite")
add_net("UKfaculty", e$UKfaculty, "social", "igraphdata UKfaculty")
add_net("macaque", e$macaque, "neural", "igraphdata macaque")
for (nm in names(e$foodwebs)) {
  g <- igraph::upgrade_graph(e$foodwebs[[nm]])
  if (igraph::vcount(g) <= 60) {
    add_net(paste0("foodweb_", nm), g, "ecological", "igraphdata foodwebs")
  }
}
nx <- reticulate::import("networkx")
from_nx <- function(g) {
  a <- nx$to_numpy_array(g)
  labels <- vapply(reticulate::iterate(g$nodes()), as.character, character(1))
  out <- igraph::graph_from_adjacency_matrix(a, mode = "undirected", weighted = "weight")
  if (!any(vapply(reticulate::iterate(g$edges(data = TRUE)),
                  function(edge) "weight" %in% names(edge[[3]]), logical(1)))) {
    out <- igraph::delete_edge_attr(out, "weight")
  }
  igraph::V(out)$name <- labels
  out
}
add_net("les_miserables", from_nx(nx$les_miserables_graph()), "literary",
        "NetworkX les_miserables_graph")
add_net("florentine", from_nx(nx$florentine_families_graph()), "social",
        "NetworkX florentine_families_graph")
add_net("davis", from_nx(nx$davis_southern_women_graph()), "social_bipartite",
        "NetworkX davis_southern_women_graph; affiliation graph retained")
manifest <- do.call(rbind, manifest)
utils::write.csv(manifest, file.path(here, "network_manifest.csv"),
                 row.names = FALSE)
saveRDS(nets, file.path(private, "networks.rds"))
cat(length(nets), "networks;", nrow(meta), "native measures;", length(configs),
    "distinct configurations;", nrow(candidate_map), "candidate labels\n")
flush(stdout())
statuses <- list()
values <- list()
correlations <- list()
memberships <- list()
for (ni in seq_along(nets)) {
  nm <- names(nets)[ni]
  g <- nets[[ni]]
  n <- igraph::vcount(g)
  set.seed(20261004 + ni)
  memb <- igraph::membership(igraph::cluster_louvain(g))
  memberships[[nm]] <- data.frame(network = nm, node = igraph::V(g)$name,
                                   community = as.integer(memb))
  raw <- matrix(NA_real_, n, length(configs),
                dimnames = list(igraph::V(g)$name, names(configs)))
  rounded <- raw
  for (ci in seq_along(configs)) {
    id <- names(configs)[ci]
    args <- configs[[ci]]
    measure <- args$measures
    warnings <- character()
    status <- "ok"
    message <- ""
    started <- proc.time()[["elapsed"]]
    costly <- meta$costly[match(measure, meta$measure)]
    if (costly && n > 40) {
      status <- "costly_above_40"
      score <- rep(NA_real_, n)
    } else {
      # Preserve the existing audit's costly-n>40 limit, but record every skip.
      # Parameters stay at defaults; invalid Katz/Hubbell are recorded, not retuned.
      args$x <- g
      args$weighted <- FALSE
      args$normalized <- FALSE
      args$epc_seed <- 20261004
      if (measure %in% .cg_membership_measures()) args$membership <- memb
      set.seed(20261004 + ni)
      score <- tryCatch(withCallingHandlers({
        setTimeLimit(elapsed = 30, transient = TRUE)
        df <- do.call(centrality, args)
        stopifnot(identical(as.character(df$node), igraph::V(g)$name))
        column <- if (measure %in% names(df)) measure else paste0(measure, "_all")
        stopifnot(column %in% names(df), length(df[[column]]) == n)
        as.numeric(df[[column]])
      }, warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }), error = function(err) {
        status <<- "error"
        message <<- conditionMessage(err)
        rep(NA_real_, n)
      }, finally = setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE))
    }
    elapsed <- proc.time()[["elapsed"]] - started
    if (length(score) != n) stop("Wrong node count for ", id)
    raw[, ci] <- score
    finite <- all(is.finite(score))
    distinct <- if (finite) length(unique(score)) else NA_integer_
    if (status == "ok" && !finite) status <- "nonfinite"
    if (finite) {
      scale <- max(abs(score))
      rounded[, ci] <- if (scale > 0) round(score / scale, 12) else score
      if (length(unique(rounded[, ci])) < 2L) status <- "constant"
    }
    statuses[[length(statuses) + 1L]] <- data.frame(
      network = nm, config = id, measure, status, finite_nodes = sum(is.finite(score)),
      nodes = n, distinct_raw = distinct,
      distinct_rank_input = if (finite) length(unique(rounded[, ci])) else NA_integer_,
      elapsed_seconds = elapsed, warning = paste(unique(warnings), collapse = " | "),
      error = message
    )
    if (ci %% 25 == 0) {
      cat(nm, ci, "of", length(configs), "configurations\n")
      flush(stdout())
    }
  }
  valid <- apply(rounded, 2, function(v) all(is.finite(v)) && length(unique(v)) > 1)
  rr <- raw[, valid, drop = FALSE]
  tt <- rounded[, valid, drop = FALSE]
  tau <- stats::cor(tt, method = "kendall")
  rho <- stats::cor(tt, method = "spearman")
  tau_raw <- suppressWarnings(stats::cor(rr, method = "kendall"))
  pearson <- suppressWarnings(stats::cor(rr, method = "pearson"))
  ix <- which(upper.tri(tau), arr.ind = TRUE)
  correlations[[nm]] <- data.frame(
    network = nm, family = manifest$family[match(nm, manifest$network)],
    config_1 = colnames(tau)[ix[, 1]], config_2 = colnames(tau)[ix[, 2]],
    tau_b = tau[ix], spearman = rho[ix], pearson = pearson[ix],
    tau_b_raw = tau_raw[ix], nodes = n
  )
  values[[nm]] <- list(raw = raw, rank_input = rounded)
  saveRDS(list(values = values, statuses = statuses, correlations = correlations,
               memberships = memberships), file.path(private, "checkpoint.rds"))
  cat("Completed", nm, "n", n, "usable", sum(valid), "of", length(valid), "\n")
  flush(stdout())
}
utils::write.csv(do.call(rbind, statuses), file.path(here, "availability.csv"),
                 row.names = FALSE)
utils::write.csv(do.call(rbind, memberships), file.path(here, "memberships.csv"),
                 row.names = FALSE)
per_network <- do.call(rbind, correlations)
utils::write.csv(per_network, gzfile(file.path(here, "per_network_pairs.csv.gz")),
                 row.names = FALSE)
saveRDS(values, file.path(private, "scores.rds"))
software <- c(R = as.character(getRversion()),
              igraph = as.character(packageVersion("igraph")),
              igraphdata = as.character(packageVersion("igraphdata")),
              networkx = as.character(nx$`__version__`))
utils::write.csv(data.frame(software = names(software), version = software),
                 file.path(here, "versions.csv"), row.names = FALSE)
cat("Numerical audit completed. Summarize retained per-network correlations next.\n")
