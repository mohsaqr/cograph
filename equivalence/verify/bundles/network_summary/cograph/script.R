library(cograph)
network <- as.matrix(utils::read.delim("../data/regulation.tsv", row.names = 1))
result <- data.frame(network = "regulation", network_summary(network, extended = TRUE, digits = NULL))
result
