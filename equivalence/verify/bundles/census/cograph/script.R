library(cograph)
network <- as.matrix(utils::read.delim("../data/regulation.tsv", row.names = 1))
triads <- triad_census(network)
dyads <- as.data.frame(dyad_census(network))
triads
dyads
result <- rbind(triads, subset(dyads, select = c(type, count)))
