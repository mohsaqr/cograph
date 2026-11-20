# Runs the declared equivalence tests (the standard: ../equivalence/STANDARD.md).
#   mini-ci verify                                    every file, with the coverage check
#   mini-ci -- Rscript tests/equivalence/run.R REGEX  the files whose name matches, no coverage check
args <- commandArgs(trailingOnly = TRUE)
source("tests/equivalence/helper-verify.R")
verify_tests(lib = "~/equiv-lib", filter = if (length(args)) args[[1L]])
