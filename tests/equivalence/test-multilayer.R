# Multilayer verbs: supra_adjacency() and its alias supra(), supra_layer() / extract_layer(),
# supra_interlayer() / extract_interlayer(), aggregate_layers() / lagg(), layer_similarity() / lsim() and
# layer_similarity_matrix() / lsim_matrix(). cograph builds these in base R.
#
# The supra-adjacency matrix of L layers of n nodes (Kivela et al. 2014, eq. 4; De Domenico et al. 2013) is the
# block-diagonal matrix of the layers plus the interlayer couplings: omega_ab I_n between layers a and b for
# diagonal coupling, omega_ab 1 1' for full coupling. It is compared with Matrix::bdiag() plus base R's
# kronecker(). The aggregations and the similarity indices are written from their definitions beside each
# comparison.

layers <- list(
  L1 = ref_graph(6, density = 0.4, seed = 81L, directed = TRUE, weighted = TRUE, digits = 6L),
  L2 = ref_graph(6, density = 0.5, seed = 82L, directed = TRUE, weighted = TRUE, digits = 6L),
  L3 = ref_graph(6, density = 0.3, seed = 83L, directed = TRUE, weighted = TRUE, digits = 6L)
)
omega <- matrix(c(0, 0.5, 0.2, 0.5, 0, 0.8, 0.2, 0.8, 0), 3, 3)
supra_names <- paste(rep(names(layers), each = 6), rep(rownames(layers$L1), 3), sep = "_")
# The layers on the diagonal, and between layers a and b the block `coupling[a, b] * pattern`.
ref_supra <- function(pattern) {
  blocks <- as.matrix(Matrix::bdiag(layers))
  coupled <- blocks + kronecker(omega, pattern)
  dimnames(coupled) <- list(supra_names, supra_names)
  coupled
}

# Sums and products of a few numbers: rounding only.
exact <- 1e-12

test_that("the supra-adjacency matrix equals the block-diagonal layers plus omega times the coupling", {
  skip_without_reference("Matrix")
  expect_reference(unclass(supra_adjacency(layers, omega = omega)), ref_supra(diag(6)), reference = "Matrix",
                   covers = "supra_adjacency()", quantity = "diagonal coupling, omega per pair of layers",
                   tolerance = exact)
  expect_reference(unclass(supra(layers, omega = omega, coupling = "full")), ref_supra(matrix(1, 6, 6)),
                   reference = "Matrix", covers = c("supra()", "supra_adjacency()"),
                   quantity = "full coupling, omega per pair of layers", tolerance = exact)
})

test_that("a layer and an interlayer block of the supra-adjacency matrix are the layer and omega I", {
  built <- supra_adjacency(layers, omega = omega)
  reference <- "independent: the layer matrix and omega_ab I_n, base R"
  expect_reference(supra_layer(built, 2), layers$L2, reference = reference, covers = "supra_layer()",
                   quantity = "layer 2 block", tolerance = 0)
  expect_reference(extract_layer(built, 3), layers$L3, reference = reference, covers = c("extract_layer()", "supra_layer()"),
                   quantity = "layer 3 block", tolerance = 0)
  expect_reference(unname(supra_interlayer(built, 2, 3)), omega[2, 3] * diag(6), reference = reference,
                   covers = "supra_interlayer()", quantity = "interlayer block 2 to 3", tolerance = exact)
  expect_reference(unname(extract_interlayer(built, 1, 3)), omega[1, 3] * diag(6), reference = reference,
                   covers = c("extract_interlayer()", "supra_interlayer()"), quantity = "interlayer block 1 to 3",
                   tolerance = exact)
})

test_that("aggregate_layers() equals the cell-wise sum, mean, maximum, minimum, union and intersection", {
  reference <- "independent: cell-wise aggregation of the layers, base R"
  stacked <- simplify2array(layers)
  cellwise <- \(f) {
    out <- apply(stacked, c(1L, 2L), f)
    dimnames(out) <- dimnames(layers$L1)
    out
  }
  expected <- list(sum = cellwise(sum), mean = cellwise(mean), max = cellwise(max), min = cellwise(min),
                   union = cellwise(\(w) as.numeric(any(w != 0))), intersection = cellwise(\(w) as.numeric(all(w != 0))))
  lapply(names(expected), \(method) {
    expect_reference(unname(aggregate_layers(layers, method = method)), unname(expected[[method]]),
                     reference = reference, covers = "aggregate_layers()", quantity = sprintf("aggregate %s", method),
                     tolerance = exact)
  })
  expect_reference(unname(lagg(layers, method = "sum", weights = c(1, 2, 0.5))),
                   unname(layers$L1 + 2 * layers$L2 + 0.5 * layers$L3), reference = reference,
                   covers = c("lagg()", "aggregate_layers()"), quantity = "weighted sum with weights 1, 2, 0.5",
                   tolerance = exact)
})

test_that("layer_similarity() equals Jaccard, overlap, Hamming, cosine and Pearson from their definitions", {
  first <- layers$L1
  second <- layers$L2
  a <- first > 0
  b <- second > 0
  expected <- c(jaccard = sum(a & b) / sum(a | b), overlap = sum(a & b) / min(sum(a), sum(b)),
                hamming = sum(a != b),
                cosine = sum(first * second) / sqrt(sum(first^2) * sum(second^2)),
                # Pearson's r of the n^2 cells: covariance over the product of standard deviations
                pearson = sum((first - mean(first)) * (second - mean(second))) /
                  sqrt(sum((first - mean(first))^2) * sum((second - mean(second))^2)))
  computed <- vapply(names(expected), \(method) layer_similarity(first, second, method = method), numeric(1))
  expect_reference(computed, expected, reference = "independent: similarity indices of two layers, base R",
                   covers = "layer_similarity()", quantity = "layers 1 and 2", tolerance = exact)
  expect_reference(lsim(first, second, method = "cosine"), expected[["cosine"]],
                   reference = "independent: similarity indices of two layers, base R",
                   covers = c("lsim()", "layer_similarity()"), quantity = "lsim() cosine, layers 1 and 2", tolerance = exact)
})

test_that("layer_similarity_matrix() holds the pairwise similarities, 1 on the diagonal", {
  pairwise <- \(index) outer(seq_along(layers), seq_along(layers), Vectorize(\(i, j) index(layers[[i]], layers[[j]])))
  jaccard <- pairwise(\(x, y) sum(x > 0 & y > 0) / sum(x > 0 | y > 0))
  cosine <- pairwise(\(x, y) sum(x * y) / sqrt(sum(x^2) * sum(y^2)))
  dimnames(jaccard) <- dimnames(cosine) <- list(names(layers), names(layers))
  expect_reference(layer_similarity_matrix(layers, method = "jaccard"), jaccard,
                   reference = "independent: similarity indices of two layers, base R",
                   covers = "layer_similarity_matrix()", quantity = "Jaccard matrix of 3 layers", tolerance = exact)
  expect_reference(lsim_matrix(layers, method = "cosine"), cosine,
                   reference = "independent: similarity indices of two layers, base R",
                   covers = c("lsim_matrix()", "layer_similarity_matrix()"), quantity = "cosine matrix of 3 layers",
                   tolerance = exact)
})
