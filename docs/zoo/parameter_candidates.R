# Zoo measures that a cograph measure already ranks at 0.90 <= tau < 1.
#
# The working hypothesis for this list: many of these are the same measure
# cograph computes, evaluated at a different parameter -- a path-length
# cutoff, a decay, a radius, an exponent. Where that is so, an argument on
# the existing verb closes the gap instead of a new measure. Writes
# docs/zoo/parameter_candidates.csv. Run from the repository root:
#
#   Rscript docs/zoo/parameter_candidates.R

zoo <- jsonlite::fromJSON(gzfile("docs/zoo/correlation.json.gz"))
mat <- zoo$matrix
dimnames(mat) <- list(zoo$labels, zoo$labels)
map <- utils::read.csv("docs/zoo/coverage_map.csv", stringsAsFactors = FALSE)
map$cograph <- sub(" .*$", "", map$cograph_measure)
covered <- map$zoo_label

nearest <- t(vapply(setdiff(zoo$labels, covered), function(l) {
  row <- mat[l, covered]
  c(max(row), which.max(row))
}, numeric(2)))
out <- data.frame(
  zoo_label = rownames(nearest),
  tau = round(nearest[, 1], 3),
  nearest_zoo = covered[nearest[, 2]],
  nearest_cograph = map$cograph[match(covered[nearest[, 2]], map$zoo_label)],
  stringsAsFactors = FALSE
)
out <- out[out$tau >= 0.90 & out$tau < 0.999, ]
out <- out[order(-out$tau), ]
utils::write.csv(out, "docs/zoo/parameter_candidates.csv", row.names = FALSE)
cat(nrow(out), "candidates at 0.90 <= tau < 1\n")
print(table(cut(out$tau, c(0.90, 0.95, 0.97, 0.99, 1),
                labels = c("0.90-0.95", "0.95-0.97", "0.97-0.99", ">=0.99"))))
cat("\nby the cograph measure they are nearest to:\n")
print(sort(table(out$nearest_cograph), decreasing = TRUE)[1:15])
