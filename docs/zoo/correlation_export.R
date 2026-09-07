# Export retained fixtures without recomputing centralities. Run from repo root.
here <- "docs/zoo/correlation-audit"
private <- "local_testing_and_equivalence/correlation_audit"
suppressMessages(devtools::load_all(".", quiet = TRUE))
defaults <- lapply(formals(centrality), function(x) paste(deparse(x), collapse = " "))
jsonlite::write_json(list(centrality_formals = defaults,
                          common_overrides = list(weighted = FALSE, normalized = FALSE,
                                                   epc_seed = 20261004),
                          membership = "Saved Louvain only for membership-required measures"),
                     file.path(here, "default_parameters.json"), pretty = TRUE,
                     auto_unbox = TRUE)
nets <- readRDS(file.path(private, "networks.rds"))
scores <- readRDS(file.path(private, "scores.rds"))
manifest <- utils::read.csv(file.path(here, "network_manifest.csv"))
stopifnot(identical(names(nets), names(scores)), length(nets) == 19)
# Correct NetworkX source metadata from the initial multiplicity import.
# The binary graphs used for scores must remain exactly identical.
nx <- reticulate::import("networkx")
sources <- list(les_miserables = nx$les_miserables_graph(),
                florentine = nx$florentine_families_graph(),
                davis = nx$davis_southern_women_graph())
for (nm in names(sources)) {
  source <- sources[[nm]]
  a <- nx$to_numpy_array(source)
  labels <- vapply(reticulate::iterate(source$nodes()), as.character, character(1))
  old <- as.matrix(igraph::as_adjacency_matrix(nets[[nm]]))
  stopifnot(identical(unname(labels), rownames(old)), all((a != 0) == old))
  i <- match(nm, manifest$network)
  manifest$original_m[i] <- source$number_of_edges()
  manifest$original_weighted[i] <- any(vapply(
    reticulate::iterate(source$edges(data = TRUE)),
    function(edge) "weight" %in% names(edge[[3]]), logical(1)))
}
utils::write.csv(manifest, file.path(here, "network_manifest.csv"), row.names = FALSE)
payload <- list()
for (nm in names(nets)) {
  g <- nets[[nm]]
  x <- scores[[nm]]
  stopifnot(identical(rownames(x$raw), igraph::V(g)$name),
            identical(dimnames(x$raw), dimnames(x$rank_input)),
            ncol(x$raw) == 180,
            identical(digest::digest(as.matrix(igraph::as_adjacency_matrix(g)),
                                     algo = "sha256"),
                      manifest$adjacency_sha256[match(nm, manifest$network)]))
  raw_hex <- matrix(sprintf("%a", x$raw), nrow = nrow(x$raw))
  payload[[nm]] <- list(nodes = rownames(x$raw), configs = colnames(x$raw),
                        raw_hex = raw_hex, rank_input = x$rank_input,
                        adjacency = as.matrix(igraph::as_adjacency_matrix(g)))
}
out <- gzfile(file.path(here, "score_fixtures.json.gz"), "wt")
writeLines(jsonlite::toJSON(payload, digits = NA, na = "string",
                             matrix = "rowmajor"), out)
close(out)
cat("Exported all retained graph/score fixtures; adjacency hashes unchanged.\n")
