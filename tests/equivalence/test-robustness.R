# robustness(), robustness_auc(), robustness_summary() (attack tolerance, Albert, Jeong & Barabasi 2000) and
# vulnerability() (Latora & Marchiori 2005). robustness() removes nodes and reads component sizes with igraph
# (igraph::delete_vertices(), components(), betweenness(), degree() in R/robustness.R), and vulnerability() reads
# igraph::distances(); igraph is therefore `internal`, and the counted references are brainGraph (robustness(),
# vulnerability()), NetworkX and the definitions. Random attacks draw their order inside the function with no
# argument to supply it; they are not compared.
#
# Graphs: connected random undirected graphs from helper-reference.R, unweighted (brainGraph's betweenness and
# cograph's attack order are unweighted on them).

attacked <- ref_connected_graph(14, density = 0.2, seed = 101L)
attacked_dense <- ref_connected_graph(16, density = 0.3, seed = 102L)

exact <- 1e-10

test_that("robustness(strategy = 'static') equals brainGraph's robustness()", {
  skip_without_reference(c("brainGraph", "igraph"))
  graph <- ref_igraph(attacked)
  # brainGraph removes the nodes in the order of their centrality in the intact graph: cograph's static strategy.
  expect_reference(robustness(attacked, measure = "betweenness", strategy = "static")$comp_pct,
                   brainGraph::robustness(graph, type = "vertex", measure = "btwn.cent")$comp.pct,
                   reference = "brainGraph", covers = "robustness()",
                   quantity = "largest component share, static betweenness attack", tolerance = exact)
  expect_reference(robustness(attacked, measure = "degree", strategy = "static")$comp_pct,
                   brainGraph::robustness(graph, type = "vertex", measure = "degree")$comp.pct,
                   reference = "brainGraph", covers = "robustness()",
                   quantity = "largest component share, static degree attack", tolerance = exact)
})

test_that("robustness() sequential degree attack equals the attack from its definition", {
  expected <- d1_degree_attack(attacked)
  expect_reference(robustness(attacked, measure = "degree")$comp_size, expected,
                   reference = "independent: sequential removal of the highest-degree node, components by reachability",
                   covers = "robustness()", quantity = "largest component, sequential degree attack",
                   tolerance = exact)
  on_dense <- d1_degree_attack(attacked_dense)
  expect_reference(robustness(attacked_dense, measure = "degree")$comp_pct, on_dense / on_dense[[1L]],
                   reference = "independent: sequential removal of the highest-degree node, components by reachability",
                   covers = "robustness()", quantity = "largest component share, sequential degree attack, dense",
                   tolerance = exact)
})

test_that("robustness_auc() and robustness_summary() equal the trapezoid rule and thresholds of the curve", {
  sizes <- d1_degree_attack(attacked)
  share <- sizes / sizes[[1L]]
  removed <- seq(0, 1, length.out = length(share))
  trapezoid <- sum(diff(removed) * (utils::head(share, -1L) + utils::tail(share, -1L)) / 2)
  attack <- robustness(attacked, measure = "degree")
  expect_reference(robustness_auc(attack), trapezoid,
                   reference = "independent: trapezoid rule over the attack curve", covers = "robustness_auc()",
                   quantity = "area under the sequential degree attack curve", tolerance = exact)
  # robustness_summary() rounds to four decimals: half a unit in the fourth place, absolute.
  expect_reference(robustness_summary(attack),
                   data.frame(measure = "degree", auc = trapezoid, critical_50 = removed[which(share < 0.5)[1L]],
                              critical_10 = removed[which(share < 0.1)[1L]]),
                   reference = "independent: trapezoid rule and the first removed share below 50% and 10%",
                   covers = "robustness_summary()", quantity = "robustness summary, sequential degree attack",
                   tolerance = 5e-5, key = "measure")
})

test_that("vulnerability() equals brainGraph's vulnerability() once the denominators are aligned", {
  skip_without_reference(c("brainGraph", "igraph"))
  # brainGraph's vulnerability() reads the efficiency of the whole graph from the graph attribute E.global (and
  # recomputes it only when the attribute is there); without it the call fails, so the attribute is set first.
  graph <- igraph::set_graph_attr(ref_igraph(attacked), "E.global", brainGraph::efficiency(ref_igraph(attacked), "global"))
  from_braingraph <- brainGraph::vulnerability(graph, use.parallel = FALSE)
  reason <- paste("vulnerability() takes the efficiency of the graph without node i over the n (n - 1) pairs of the",
                  "intact graph, as its help page states; brainGraph takes it over the (n - 1)(n - 2) pairs of the",
                  "reduced graph, so 1 - V(brainGraph) = (1 - V(cograph)) n / (n - 2).")
  expect_known_difference(vulnerability(attacked),
                          data.frame(node = rownames(attacked), vulnerability = from_braingraph),
                          reference = "brainGraph", covers = "vulnerability()",
                          quantity = "vulnerability, unweighted, brainGraph's denominator", tolerance = exact,
                          key = "node", reason = reason)
  n <- nrow(attacked)
  expect_reference(vulnerability(attacked),
                   data.frame(node = rownames(attacked), vulnerability = 1 - (1 - from_braingraph) * (n - 2) / n),
                   reference = "brainGraph", covers = "vulnerability()",
                   quantity = "vulnerability, unweighted, over the n (n - 1) pairs of the intact graph",
                   tolerance = exact, key = "node")
})

test_that("vulnerability() equals the drop in NetworkX's global efficiency", {
  # Latora & Marchiori (2005): V_i = (E - E_i) / E, with E_i the efficiency of the graph without node i taken
  # over the n (n - 1) ordered pairs of the intact graph (the unreachable pairs, and those through i, count 0).
  nx <- ref_networkx(attacked, c(vulnerability = paste0(
    "{v: 1 - sum(1 / x for s in dict(nx.all_pairs_shortest_path_length(G.subgraph(set(G) - {v}))).values() ",
    "for x in s.values() if x > 0) / (len(G) * (len(G) - 1)) / nx.global_efficiency(G) for v in G}")))
  expect_reference(vulnerability(attacked), data.frame(node = names(nx$vulnerability), vulnerability = nx$vulnerability),
                   reference = "networkx", covers = "vulnerability()", quantity = "vulnerability, NetworkX efficiencies",
                   tolerance = exact, key = "node")
})
