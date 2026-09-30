# Equivalence tests: cograph against reference implementations (igraph, sna,
# centiserve, NetworkX, the pre-port golden corpus, ...). They live outside
# tests/testthat so R CMD check on CRAN does not run them; CI runs them in the
# `equivalence` job. Run locally with
#   NOT_CRAN=true Rscript -e 'devtools::load_all(); testthat::test_dir("tests/equivalence")'
#
# Load the shared helpers from tests/testthat first (this file sorts first).
helper_env <- environment()
invisible(lapply(
  c("helper-cograph.R", "helper-test-utils.R", "helper-networks.R"),
  function(f) sys.source(testthat::test_path("..", "testthat", f),
                         envir = helper_env)
))
