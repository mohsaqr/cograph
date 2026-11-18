library(tna)
network <- as.matrix(utils::read.delim("../data/transitions.tsv", row.names = 1))
model <- tna(network)
measures <- c("InStrength", "OutStrength", "ClosenessIn", "ClosenessOut", "Closeness",
              "Betweenness", "Clustering", "PageRank")
plain <- as.data.frame(centralities(model, measures = measures))
scaled <- as.data.frame(centralities(model, measures = "Diffusion", normalize = TRUE))
result <- data.frame(node = as.character(plain$state),
                     strength_in = plain$InStrength, strength_out = plain$OutStrength,
                     closeness_in = plain$ClosenessIn, closeness_out = plain$ClosenessOut,
                     closeness_all = plain$Closeness, betweenness = plain$Betweenness,
                     transitivity = plain$Clustering, pagerank = plain$PageRank,
                     diffusion_in = scaled$Diffusion)
result
