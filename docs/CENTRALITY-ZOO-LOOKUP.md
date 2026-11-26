# Looking for a centrality from the Zoo? Here is what cograph has

Every measure in the [Centrality Zoo](https://centralityzoo.github.io/comparison/)
(349 measures, Shvydun 2025) appears exactly once below. `tau` is the
average Kendall rank correlation between Zoo implementations over 648
empirical networks. A mapped cograph name identifies a corresponding
definition; these are not comparisons against cograph outputs. Values are
rounded: displayed 1.00 does not prove exact rank or numerical equivalence.

| Section | Meaning | Count |
|---|---|---|
| Available | cograph implements the measure itself | 193 |
| Correlated candidate (tau >= 0.90) | not implemented; a mapped Zoo measure has high average rank correlation | 91 |
| Not available (tau < 0.90) | not implemented; nearest mapped Zoo measure has lower average correlation | 65 |

Only 193 of the 349 Zoo labels are mapped as implemented.
Another 91 have a correlated candidate, including 21
at tau >= 0.99. Correlation does not establish interchangeable measures.

Caveat: the Zoo computed tau on undirected, unweighted networks, so
agreement on a directed or weighted network may be lower. Rank agreement
does not establish numerical equivalence or accuracy. No fixed conversion
between Kendall and Spearman correlation is assumed.

## Available in cograph

| Zoo measure | cograph measure | Call |
|---|---|---|
| Access information | `access_information` | `centrality(x, measures = "access_information")` |
| Adaptive LeaderRank | `adaptive_leaderrank` | `centrality(x, measures = "adaptive_leaderrank")` |
| All cycle betweenness (ACC) | `all_cycle_betweenness` | `centrality(x, measures = "all_cycle_betweenness")` |
| beta-measure | `beta_measure` | `centrality(x, measures = "beta_measure")` |
| Betweenness | `betweenness` | `centrality(x, measures = "betweenness")` |
| BG-index | `beta_measure` | `centrality(x, measures = "beta_measure")` |
| Biased random walk centrality | `biased_random_walk` | `centrality(x, measures = "biased_random_walk")` |
| Borgatti's effective size | `effective_size` | `centrality(x, measures = "effective_size")` |
| BottleNeck | `bottleneck` | `centrality(x, measures = "bottleneck")` |
| Bridging capital | `bridging_capital` | `centrality(x, measures = "bridging_capital")` |
| Bridging centrality | `bridging` | `centrality(x, measures = "bridging")` |
| Bridging coefficient | `bridging_coefficient` | `centrality(x, measures = "bridging_coefficient")` |
| Burt's constraint | `constraint` | `centrality(x, measures = "constraint")` |
| Centroid | `centroid` | `centrality(x, measures = "centroid")` |
| Closeness | `closeness` | `centrality(x, measures = "closeness")` |
| Closeness vitality | `closeness_vitality` | `centrality(x, measures = "closeness_vitality")` |
| Clustering degree algorithm (CDA) | `cda` | `centrality(x, measures = "cda")` |
| ClusterRank | `clusterrank` | `centrality(x, measures = "clusterrank")` |
| Coleman-Theil disorder index | `coleman_theil` | `centrality(x, measures = "coleman_theil")` |
| CollInf | `collective_influence` | `centrality(x, measures = "collective_influence")` |
| Comm Centrality | `comm_centrality` | `centrality(x, measures = "comm_centrality")` |
| Communicability betweenness | `communicability_betweenness` | `centrality(x, measures = "communicability_betweenness")` |
| Community Hub‑Bridge measure | `community_hub_bridge` | `centrality(x, measures = "community_hub_bridge")` |
| Community-based centrality (CbC) | `community_based` | `centrality(x, measures = "community_based")` |
| Community-based mediator (CbM) | `community_mediator` | `centrality(x, measures = "community_mediator")` |
| CON score | `con` | `centrality(x, measures = "con")` |
| ControlRank | `controlrank` | `centrality(x, measures = "controlrank")` |
| Counting Betweenness | `counting_betweenness` | `centrality(x, measures = "counting_betweenness")` |
| Cross-Clique Connectivity | `cross_clique` | `centrality(x, measures = "cross_clique")` |
| Current-flow betweenness | `current_flow_betweenness` | `centrality(x, measures = "current_flow_betweenness")` |
| Current-flow Closeness | `current_flow_closeness` | `centrality(x, measures = "current_flow_closeness")` |
| Decay | `decay` | `centrality(x, measures = "decay")` |
| Degree | `degree` | `centrality(x, measures = "degree")` |
| Degree and Importance of Lines (DIL) | `dil` | `centrality(x, measures = "dil")` |
| DegreeDiscountIC | `degree_discount` | `centrality(x, measures = "degree_discount")` |
| delta-betweenness | `delta_betweenness` | `centrality(x, measures = "delta_betweenness")` |
| delta-closeness | `delta_closeness` | `centrality(x, measures = "delta_closeness")` |
| Density centrality | `density_centrality` | `centrality(x, measures = "density_centrality")` |
| Diffusion centrality | `diffusion_centrality` | `centrality(x, measures = "diffusion_centrality", diffusion_q = 1, diffusion_steps = 3)` |
| Diffusion Degree | `diffusion` | `centrality(x, measures = "diffusion")` |
| Distance entropy | `distance_entropy` | `centrality(x, measures = "distance_entropy")` |
| Distance-weighted fragmentation | `fragmentation` | `centrality(x, measures = "fragmentation")` |
| Diversity coefficient | `diversity` | `centrality(x, measures = "diversity")` |
| DK-based gravity model (DKGM) | `dkgm` | `centrality(x, measures = "dkgm")` |
| DMNC | `dmnc` | `centrality(x, measures = "dmnc")` |
| DST | `dst` | `centrality(x, measures = "dst")` |
| Dynamical importance | `dynamical_importance` | `centrality(x, measures = "dynamical_importance")` |
| Dynamics-sensitive (DS) centrality | `dynamics_sensitive` | `centrality(x, measures = "dynamics_sensitive", ds_beta = 0.1, ds_mu = 1, ds_steps = 5)` |
| Eccentricity | `eccentricity` | `centrality(x, measures = "eccentricity")` |
| Effective size | `effective_size` | `centrality(x, measures = "effective_size")` |
| Egocentric betweenness | `ego_betweenness` | `centrality(x, measures = "ego_betweenness")` |
| Eigenvector | `eigenvector` | `centrality(x, measures = "eigenvector")` |
| EnRenew | `enrenew` | `centrality(x, measures = "enrenew")` |
| Entropy | `entropy` | `centrality(x, measures = "entropy")` |
| Entropy and mutual information-based centrality (EMI) | `emi` | `centrality(x, measures = "emi")` |
| Entropy variation (betweenness) | `entropy_variation_betweenness` | `centrality(x, measures = "entropy_variation_betweenness")` |
| Entropy variation (degree) | `entropy_variation_degree` | `centrality(x, measures = "entropy_variation_degree")` |
| Exogenous centrality | `exogenous` | `centrality(x, measures = "exogenous")` |
| Expected force (ExF) | `expected_force` | `centrality(x, measures = "expected_force")` |
| Expected force (ExFm) | `modified_expected_force` | `centrality(x, measures = "modified_expected_force")` |
| Extended gravity centrality | `extended_gravity` | `centrality(x, measures = "extended_gravity")` |
| Extended hybrid characteristic centrality (EHCC) | `ehcc` | `centrality(x, measures = "ehcc")` |
| Extended LBC | `extended_local_bridging` | `centrality(x, measures = "extended_local_bridging")` |
| Extended mixed gravitational centrality | `extended_mixed_gravity` | `centrality(x, measures = "extended_mixed_gravity")` |
| Extended neighborhood coreness | `extended_coreness` | `centrality(x, measures = "extended_coreness")` |
| Flow betweenness | `flow_betweenness` | `centrality(x, measures = "flow_betweenness")` |
| Flow coefficient | `flow_coefficient` | `centrality(x, measures = "flow_coefficient")` |
| Fuzzy local dimension (FLD) | `fuzzy_local_dimension` | `centrality(x, measures = "fuzzy_local_dimension")` |
| Gateway coefficient | `gateway` | `centrality(x, measures = "gateway")` |
| Geodesic k-path | `geodesic_kpath` | `centrality(x, measures = "geodesic_kpath")` |
| Gil-Schmidt Power Index | `gilschmidt` | `centrality(x, measures = "gilschmidt")` |
| Global and local information (GLI) method | `gli` | `centrality(x, measures = "gli")` |
| Global importance of nodes (GIN) | `gin` | `centrality(x, measures = "gin")` |
| Global Structure Model (GSM) | `global_structure` | `centrality(x, measures = "global_structure")` |
| Godfather index | `godfather` | `centrality(x, measures = "godfather")` |
| Graph regularization centrality (GRC) | `graph_regularization` | `centrality(x, measures = "graph_regularization")` |
| Gravity centrality | `gravity` | `centrality(x, measures = "gravity")` |
| Gravity model | `gravity` | `centrality(x, measures = "gravity", gravity_mass = "degree", gravity_radius = NULL)` |
| h-index strength | `hindex_strength` | `centrality(x, measures = "hindex_strength")` |
| Harmonic | `harmonic` | `centrality(x, measures = "harmonic")` |
| Heatmap centrality | `heatmap` | `centrality(x, measures = "heatmap")` |
| Hide information | `hide_information` | `centrality(x, measures = "hide_information")` |
| Hubbel | `hubbell` | `centrality(x, measures = "hubbell")` |
| Hybrid centrality (HC) | `hybrid` | `centrality(x, measures = "hybrid")` |
| Hybrid Characteristic Centrality (HCC) | `hcc` | `centrality(x, measures = "hcc")` |
| Hybrid Global Structure Model (H-GSM) | `hybrid_global_structure` | `centrality(x, measures = "hybrid_global_structure")` |
| Icentr | `icentr` | `centrality(x, measures = "icentr")` |
| Immediate Effects Centrality (IEC) | `iec` | `centrality(x, measures = "iec")` |
| Improved closeness centrality (ICC) | `improved_closeness` | `centrality(x, measures = "improved_closeness")` |
| Improved global structure model (IGSM) | `improved_global_structure` | `centrality(x, measures = "improved_global_structure")` |
| Improved IMC | `node_contraction_improved` | `centrality(x, measures = "node_contraction_improved")` |
| Improved iterative resource allocation (IIRA) | `iira` | `centrality(x, measures = "iira")` |
| Improved K-shell decomposition (IKSD) | `iksd` | `centrality(x, measures = "iksd")` |
| Improved neighbors’ k-core (INK) | `ink` | `centrality(x, measures = "ink")` |
| INF centrality | `beta_measure` | `centrality(x, measures = "beta_measure")` |
| Infection number | `infection` | `centrality(x, measures = "infection")` |
| Integration | `integration` | `centrality(x, measures = "integration")` |
| Intra-module degree | `within_module_z` | `centrality(x, measures = "within_module_z")` |
| Iterative resource allocation (IRA) | `ira` | `centrality(x, measures = "ira")` |
| k-betweenness | `betweenness` | `centrality(x, measures = "betweenness", cutoff = k)` |
| k-shell | `coreness` | `centrality(x, measures = "coreness")` |
| k-truss number | `truss` | `centrality(x, measures = "truss")` |
| Katz | `katz` | `centrality(x, measures = "katz")` |
| KDEC method | `kdec` | `centrality(x, measures = "kdec")` |
| KED | `ked` | `centrality(x, measures = "ked")` |
| Laplacian | `laplacian` | `centrality(x, measures = "laplacian")` |
| LeaderRank | `leaderrank` | `centrality(x, measures = "leaderrank")` |
| Length-scaled betweenness | `length_scaled_betweenness` | `centrality(x, measures = "length_scaled_betweenness")` |
| Leverage | `leverage` | `centrality(x, measures = "leverage")` |
| Lhc method | `lhc` | `centrality(x, measures = "lhc")` |
| Lin's index | `lin` | `centrality(x, measures = "lin")` |
| LineRank | `linerank` | `centrality(x, measures = "linerank")` |
| Load | `load` | `centrality(x, measures = "load")` |
| Lobby index | `lobby` | `centrality(x, measures = "lobby")` |
| Local clustering coefficient | `transitivity` | `centrality(x, measures = "transitivity")` |
| Local dimension (LD) | `local_dimension_fixed` | `centrality(x, measures = "local_dimension_fixed")` |
| Local dimension (Pu) | `local_dimension` | `centrality(x, measures = "local_dimension")` |
| Local entropy (LE) | `local_entropy` | `centrality(x, measures = "local_entropy")` |
| Local gravity model | `gravity` | `centrality(x, measures = "gravity", gravity_mass = "degree", gravity_radius = "auto")` |
| Local H-index | `local_hindex` | `centrality(x, measures = "local_hindex")` |
| Local information dimensionality (LID) | `local_information_dimension` | `centrality(x, measures = "local_information_dimension")` |
| Local neighbor contribution (LNC) | `lnc` | `centrality(x, measures = "lnc")` |
| Local volume dimension (LVD) | `local_volume_dimension` | `centrality(x, measures = "local_volume_dimension")` |
| Localized bridging centrality | `localized_bridging` | `centrality(x, measures = "localized_bridging")` |
| LocalRank | `semilocal` | `centrality(x, measures = "semilocal")` |
| LRIC (max) | `lric_max` | `centrality(x, measures = "lric_max")` |
| m-reach | `kreach` | `centrality(x, measures = "kreach")` |
| Malatya centrality | `malatya` | `centrality(x, measures = "malatya")` |
| Map Equation Centrality (MEC) | `map_equation` | `centrality(x, measures = "map_equation")` |
| Markov | `markov` | `centrality(x, measures = "markov")` |
| MCC | `mcc` | `centrality(x, measures = "mcc")` |
| Mediative Effects Centrality (MEC) | `mec` | `centrality(x, measures = "mec")` |
| Mixed Degree Decomposition (MDD) | `mdd` | `centrality(x, measures = "mdd")` |
| Mixed gravitational centrality | `mixed_gravity` | `centrality(x, measures = "mixed_gravity")` |
| MNC | `mnc` | `centrality(x, measures = "mnc")` |
| Modularity vitality | `modularity_vitality` | `centrality(x, measures = "modularity_vitality")` |
| Multi-characteristics gravity model (MCGM) | `mcgm` | `centrality(x, measures = "mcgm")` |
| NCVoteRank | `ncvoterank` | `centrality(x, measures = "ncvoterank")` |
| Neighbor distance centrality | `neighbor_distance` | `centrality(x, measures = "neighbor_distance")` |
| Neighborhood centrality | `neighbor_distance` | `centrality(x, measures = "neighbor_distance")` |
| Neighborhood connectivity | `neighborhood_connectivity` | `centrality(x, measures = "neighborhood_connectivity")` |
| Node and Neighbor Layer Information (NINL) centrality | `ninl` | `centrality(x, measures = "ninl")` |
| Node contraction (IMC) | `node_contraction` | `centrality(x, measures = "node_contraction")` |
| Node importance evaluation matrix (NIEM) method | `niem` | `centrality(x, measures = "niem")` |
| Non-backtracking centrality | `nonbacktracking` | `centrality(x, measures = "nonbacktracking")` |
| NWRank | `nwrank` | `centrality(x, measures = "nwrank")` |
| PageRank | `pagerank` | `centrality(x, measures = "pagerank")` |
| Pairwise disconnectivity | `pairwisedis` | `centrality(x, measures = "pairwisedis")` |
| Participation coefficient | `participation` | `centrality(x, measures = "participation")` |
| Path-transfer centrality | `path_transfer` | `centrality(x, measures = "path_transfer")` |
| Percolation | `percolation` | `centrality(x, measures = "percolation")` |
| Proximal betwenness | `proximal_betweenness` | `centrality(x, measures = "proximal_betweenness")` |
| Random walk centrality | `random_walk` | `centrality(x, measures = "random_walk")` |
| Random walk decay | `random_walk_decay` | `centrality(x, measures = "random_walk_decay")` |
| Randomized shortest paths (RSP) betweenness | `rsp_betweenness` | `centrality(x, measures = "rsp_betweenness")` |
| Redundancy | `redundancy` | `centrality(x, measures = "redundancy")` |
| Relative entropy | `relative_entropy` | `centrality(x, measures = "relative_entropy")` |
| Renewed coreness | `renewed_coreness` | `centrality(x, measures = "renewed_coreness")` |
| Residual closeness | `residual_closeness` | `centrality(x, measures = "residual_closeness")` |
| Resistance curvature | `resistance_curvature` | `centrality(x, measures = "resistance_curvature")` |
| Rumor centrality | `rumor` | `centrality(x, measures = "rumor")` |
| s-shell index | `s_shell` | `centrality(x, measures = "s_shell")` |
| SALSA | `salsa` | `centrality(x, measures = "salsa")` |
| Second order centrality | `second_order` | `centrality(x, measures = "second_order")` |
| Semi-local ranking (SLC) | `semilocal` | `centrality(x, measures = "semilocal")` |
| Shapley value (game 1) | `shapley_game1` | `centrality(x, measures = "shapley_game1")` |
| Shapley value (game 2) | `shapley_game2` | `centrality(x, measures = "shapley_game2")` |
| Shapley value (game 3) | `shapley_game3` | `centrality(x, measures = "shapley_game3")` |
| SingleDiscount | `single_discount` | `centrality(x, measures = "single_discount")` |
| Spanning tree centrality (STC) | `spanning_tree` | `centrality(x, measures = "spanning_tree")` |
| SpectralRank | `spectralrank` | `centrality(x, measures = "spectralrank")` |
| Stress | `stress` | `centrality(x, measures = "stress")` |
| Subgraph | `subgraph` | `centrality(x, measures = "subgraph")` |
| Support | `support` | `centrality(x, measures = "support")` |
| Topological | `topological_coefficient` | `centrality(x, measures = "topological_coefficient")` |
| TOPSIS-RE | `topsis_re` | `centrality(x, measures = "topsis_re")` |
| Total communicability | `communicability` | `centrality(x, measures = "communicability")` |
| Total Effects Centrality (TEC) | `tec` | `centrality(x, measures = "tec")` |
| Transportation centrality | `transportation` | `centrality(x, measures = "transportation")` |
| Trust-PageRank | `trust_pagerank` | `centrality(x, measures = "trust_pagerank")` |
| Two-way random walk betweenness (2RW) | `two_way_rw` | `centrality(x, measures = "two_way_rw")` |
| Volume centrality | `volume` | `centrality(x, measures = "volume")` |
| VoteRank | `voterank` | `centrality(x, measures = "voterank")` |
| VoteRank++ | `voterank_plus` | `centrality(x, measures = "voterank_plus")` |
| Weighted formal concept analysis (WFCA) | `wfca` | `centrality(x, measures = "wfca")` |
| Weighted h-index | `weighted_h_index` | `centrality(x, measures = "weighted_h_index")` |
| Weighted k-shell decomposition (Wks) | `weighted_kshell` | `centrality(x, measures = "weighted_kshell")` |
| Weighted k-shell degree neighborhood (Maji) | `wksdn` | `centrality(x, measures = "wksdn")` |
| Weighted LeaderRank | `weighted_leaderrank` | `centrality(x, measures = "weighted_leaderrank")` |
| WVoteRank | `wvoterank` | `centrality(x, measures = "wvoterank")` |
| X-degree centrality | `x_degree` | `centrality(x, measures = "x_degree")` |
| X-nonbacktracking centrality | `x_nonbacktracking` | `centrality(x, measures = "x_nonbacktracking")` |
| μ-Power Community Index (μ-PCI) | `mu_pci` | `centrality(x, measures = "mu_pci")` |

## Correlated candidates (tau >= 0.90)

These measures are not implemented. The nearest mapped Zoo measure is
a research lead, not a verified substitute. Average correlation alone
cannot establish equivalence on an individual network.

| Zoo measure | Correlated cograph candidate | Call | tau | Evidence |
|---|---|---|---|---|
| AIC | `lin` | `centrality(x, measures = "lin")` | 1.00 | high average rank correlation; not equivalence |
| All-around score | `mdd` | `centrality(x, measures = "mdd")` | 0.95 | high average rank correlation; not equivalence |
| Analytic Hierarchy Process (AHP) centrality | `density_centrality` | `centrality(x, measures = "density_centrality")` | 0.94 | high average rank correlation; not equivalence |
| ArticleRank | `pagerank` | `centrality(x, measures = "pagerank")` | 0.95 | high average rank correlation; not equivalence |
| Average shortest path centrality (AC) | `closeness_vitality` | `centrality(x, measures = "closeness_vitality")` | 0.98 | high average rank correlation; not equivalence |
| Cc-Burt | `iira` | `centrality(x, measures = "iira")` | 0.90 | high average rank correlation; not equivalence |
| Classified neighbors centrality | `mdd` | `centrality(x, measures = "mdd")` | 0.96 | high average rank correlation; not equivalence |
| Correlation centrality | `infection` | `centrality(x, measures = "infection")` | 0.94 | high average rank correlation; not equivalence |
| Cumulative contact probability (CCP) | `tec` | `centrality(x, measures = "tec")` | 1.00 | high average rank correlation; not equivalence |
| Curvature index | `dil` | `centrality(x, measures = "dil")` | 0.93 | high average rank correlation; not equivalence |
| Degree and clustering coefficient (DCC) | `diffusion` | `centrality(x, measures = "diffusion")` | 0.93 | high average rank correlation; not equivalence |
| Degree mass | `diffusion` | `centrality(x, measures = "diffusion")` | 1.00 | high average rank correlation; not equivalence |
| DirichletRank | `tec` | `centrality(x, measures = "tec")` | 1.00 | high average rank correlation; not equivalence |
| Diversity-strength centrality (DSC) | `tec` | `centrality(x, measures = "tec")` | 0.91 | high average rank correlation; not equivalence |
| Diversity-strength ranking (DSR) | `iira` | `centrality(x, measures = "iira")` | 0.94 | high average rank correlation; not equivalence |
| Dynamical Influence | `eigenvector` | `centrality(x, measures = "eigenvector")` | 1.00 | high average rank correlation; not equivalence |
| Edge clustering coefficient centrality (NC) | `cross_clique` | `centrality(x, measures = "cross_clique")` | 0.90 | high average rank correlation; not equivalence |
| Edge-disjoint k-path | `iira` | `centrality(x, measures = "iira")` | 0.93 | high average rank correlation; not equivalence |
| Effective gravity model (EGM) | `gravity` | `centrality(x, measures = "gravity", gravity_mass = "degree", gravity_radius = NULL)` | 0.92 | high average rank correlation; not equivalence |
| Efficiency centrality (EffC) | `fragmentation` | `centrality(x, measures = "fragmentation")` | 1.00 | high average rank correlation; not equivalence |
| Eigentrust | `tec` | `centrality(x, measures = "tec")` | 0.93 | high average rank correlation; not equivalence |
| Electrical closeness | `current_flow_closeness` | `centrality(x, measures = "current_flow_closeness")` | 1.00 | high average rank correlation; not equivalence |
| Entropy-based gravity model | `mec` | `centrality(x, measures = "mec")` | 0.95 | high average rank correlation; not equivalence |
| Entropy-Based Ranking Measure (ERM) | `ninl` | `centrality(x, measures = "ninl")` | 0.97 | high average rank correlation; not equivalence |
| epsilon-betweenness | `percolation` | `centrality(x, measures = "percolation")` | 1.00 | high average rank correlation; not equivalence |
| Extended Cluster Coefficient Ranking Measure (ECRM) | `ninl` | `centrality(x, measures = "ninl")` | 0.98 | high average rank correlation; not equivalence |
| Extended diversity-strength ranking (EDSR) | `ninl` | `centrality(x, measures = "ninl")` | 0.95 | high average rank correlation; not equivalence |
| Extended H-index centrality (EHC) | `ninl` | `centrality(x, measures = "ninl")` | 0.98 | high average rank correlation; not equivalence |
| Extended improved k-shell hybrid (EIKH) | `infection` | `centrality(x, measures = "infection")` | 0.98 | high average rank correlation; not equivalence |
| Extended k-shell hybrid method | `infection` | `centrality(x, measures = "infection")` | 0.98 | high average rank correlation; not equivalence |
| Extended RMD-weighted degree (EWD) | `extended_coreness` | `centrality(x, measures = "extended_coreness")` | 0.98 | high average rank correlation; not equivalence |
| Extended weight degree centrality (EWdc) | `communicability` | `centrality(x, measures = "communicability")` | 0.97 | high average rank correlation; not equivalence |
| Global and Local Structure(GLS) | `linerank` | `centrality(x, measures = "linerank")` | 0.96 | high average rank correlation; not equivalence |
| Hierarchical k-shell (HKS) | `eigenvector` | `centrality(x, measures = "eigenvector")` | 0.95 | high average rank correlation; not equivalence |
| Hybrid degree centrality | `hubbell` | `centrality(x, measures = "hubbell")` | 0.99 | high average rank correlation; not equivalence |
| Improved entropy-based centrality | `ked` | `centrality(x, measures = "ked")` | 0.97 | high average rank correlation; not equivalence |
| Improved k-shell hybrid (IKH) | `neighbor_distance` | `centrality(x, measures = "neighbor_distance")` | 0.97 | high average rank correlation; not equivalence |
| Influence capability (IC) | `ninl` | `centrality(x, measures = "ninl")` | 0.97 | high average rank correlation; not equivalence |
| Integral k-shell | `neighbor_distance` | `centrality(x, measures = "neighbor_distance")` | 0.91 | high average rank correlation; not equivalence |
| Inward accessibility | `trust_pagerank` | `centrality(x, measures = "trust_pagerank")` | 0.91 | high average rank correlation; not equivalence |
| k-path | `diffusion` | `centrality(x, measures = "diffusion")` | 0.99 | high average rank correlation; not equivalence |
| k-shell based on gravity centrality (KSGC) | `hubbell` | `centrality(x, measures = "hubbell")` | 0.95 | high average rank correlation; not equivalence |
| k-shell hybrid method | `neighbor_distance` | `centrality(x, measures = "neighbor_distance")` | 0.97 | high average rank correlation; not equivalence |
| k-shell iteration factor(KS-IF) | `extended_coreness` | `centrality(x, measures = "extended_coreness")` | 0.95 | high average rank correlation; not equivalence |
| Laplacian gravity centrality (LGC) | `diffusion` | `centrality(x, measures = "diffusion")` | 0.97 | high average rank correlation; not equivalence |
| Linearly scaled betweenness | `percolation` | `centrality(x, measures = "percolation")` | 1.00 | high average rank correlation; not equivalence |
| Local structural centrality (LSC) | `iira` | `centrality(x, measures = "iira")` | 0.95 | high average rank correlation; not equivalence |
| LRIC (PPR) | `tec` | `centrality(x, measures = "tec")` | 0.93 | high average rank correlation; not equivalence |
| M-centrality | `iira` | `centrality(x, measures = "iira")` | 0.92 | high average rank correlation; not equivalence |
| Mapping entropy (ME) | `hubbell` | `centrality(x, measures = "hubbell")` | 0.98 | high average rank correlation; not equivalence |
| Mapping Entropy Betweenness (MEB) | `relative_entropy` | `centrality(x, measures = "relative_entropy")` | 0.96 | high average rank correlation; not equivalence |
| Meta-centrality | `iira` | `centrality(x, measures = "iira")` | 0.93 | high average rank correlation; not equivalence |
| Mixed core, degree and entropy (MCDE) | `tec` | `centrality(x, measures = "tec")` | 0.97 | high average rank correlation; not equivalence |
| Mixed core, degree and weighted entropy (MCDWE) | `mdd` | `centrality(x, measures = "mdd")` | 0.97 | high average rank correlation; not equivalence |
| Mixed core, semi-local degree and entropy (MCSDE) | `semilocal` | `centrality(x, measures = "semilocal")` | 1.00 | high average rank correlation; not equivalence |
| Mixed core, semi-local degree and weighted entropy (MCSDWE) | `semilocal` | `centrality(x, measures = "semilocal")` | 1.00 | high average rank correlation; not equivalence |
| Modified Local Centrality (MLC) | `semilocal` | `centrality(x, measures = "semilocal")` | 1.00 | high average rank correlation; not equivalence |
| Multi-evidence centrality (MeC) | `mec` | `centrality(x, measures = "mec")` | 1.00 | high average rank correlation; not equivalence |
| Neighborhood core diversity centrality (Cncd) | `extended_coreness` | `centrality(x, measures = "extended_coreness")` | 0.98 | high average rank correlation; not equivalence |
| New evidential centrality (NEC) | `tec` | `centrality(x, measures = "tec")` | 0.91 | high average rank correlation; not equivalence |
| Nieminen's closeness | `lin` | `centrality(x, measures = "lin")` | 1.00 | high average rank correlation; not equivalence |
| NL centrality | `iira` | `centrality(x, measures = "iira")` | 0.95 | high average rank correlation; not equivalence |
| Node importance contribution correlation matrix (NICCM) method | `iira` | `centrality(x, measures = "iira")` | 0.95 | high average rank correlation; not equivalence |
| Normalized local centrality (NLC) | `iira` | `centrality(x, measures = "iira")` | 0.94 | high average rank correlation; not equivalence |
| PhysarumSpreader | `beta_measure` | `centrality(x, measures = "beta_measure")` | 1.00 | high average rank correlation; not equivalence |
| Probabilistic-jumping Random Walk (PJRW) | `tec` | `centrality(x, measures = "tec")` | 0.91 | high average rank correlation; not equivalence |
| ProfitLeader | `support` | `centrality(x, measures = "support")` | 0.95 | high average rank correlation; not equivalence |
| QJSD centrality | `tec` | `centrality(x, measures = "tec")` | 1.00 | high average rank correlation; not equivalence |
| Random walk-based gravity (DFS-Gravity) | `laplacian` | `centrality(x, measures = "laplacian")` | 0.94 | high average rank correlation; not equivalence |
| RCFB centrality | `current_flow_betweenness` | `centrality(x, measures = "current_flow_betweenness")` | 0.92 | high average rank correlation; not equivalence |
| RMD-weighted degree (WD) | `local_hindex` | `centrality(x, measures = "local_hindex")` | 0.96 | high average rank correlation; not equivalence |
| Routing betweenness | `load` | `centrality(x, measures = "load")` | 1.00 | high average rank correlation; not equivalence |
| Seeley’s index | `tec` | `centrality(x, measures = "tec")` | 0.93 | high average rank correlation; not equivalence |
| Semi-local iterative algorithm (semi-IA) | `icentr` | `centrality(x, measures = "icentr")` | 0.94 | high average rank correlation; not equivalence |
| Shell clustering coefficient | `modified_expected_force` | `centrality(x, measures = "modified_expected_force")` | 0.97 | high average rank correlation; not equivalence |
| Similarity-based PageRank | `malatya` | `centrality(x, measures = "malatya")` | 0.91 | high average rank correlation; not equivalence |
| Spreading probability (SP) | `icentr` | `centrality(x, measures = "icentr")` | 0.93 | high average rank correlation; not equivalence |
| Synthesize centrality (SC) | `tec` | `centrality(x, measures = "tec")` | 0.91 | high average rank correlation; not equivalence |
| TOPSIS | `density_centrality` | `centrality(x, measures = "density_centrality")` | 0.94 | high average rank correlation; not equivalence |
| Total centrality | `fragmentation` | `centrality(x, measures = "fragmentation")` | 1.00 | high average rank correlation; not equivalence |
| Vertex-disjoint k-path | `iira` | `centrality(x, measures = "iira")` | 0.92 | high average rank correlation; not equivalence |
| ViralRank | `current_flow_closeness` | `centrality(x, measures = "current_flow_closeness")` | 1.00 | high average rank correlation; not equivalence |
| VMM algorithm | `tec` | `centrality(x, measures = "tec")` | 0.96 | high average rank correlation; not equivalence |
| Weight degree centrality (Wdc) | `diffusion` | `centrality(x, measures = "diffusion")` | 0.97 | high average rank correlation; not equivalence |
| Weight neighborhood centrality | `wksdn` | `centrality(x, measures = "wksdn")` | 0.98 | high average rank correlation; not equivalence |
| Weighted gravity model (WGravity) | `diffusion_centrality` | `centrality(x, measures = "diffusion_centrality", diffusion_q = 1, diffusion_steps = 3)` | 0.98 | high average rank correlation; not equivalence |
| Weighted k-shell degree neighborhood (Wksd) | `gravity` | `centrality(x, measures = "gravity")` | 0.96 | high average rank correlation; not equivalence |
| Weighted TOPSIS | `harmonic` | `centrality(x, measures = "harmonic")` | 0.94 | high average rank correlation; not equivalence |
| wkpath | `infection` | `centrality(x, measures = "infection")` | 0.96 | high average rank correlation; not equivalence |
| WRank | `niem` | `centrality(x, measures = "niem")` | 0.91 | high average rank correlation; not equivalence |
| Zeta vector centrality | `second_order` | `centrality(x, measures = "second_order")` | 1.00 | high average rank correlation; not equivalence |

## Not available (tau < 0.90)

These measures are not implemented. The nearest mapped Zoo measure and
its average tau provide comparison context, without proving equivalence.

| Zoo measure | Nearest cograph measure | tau |
|---|---|---|
| Absorbing Random-Walk (ARW) | `single_discount` | 0.65 |
| Algebraic centrality | `closeness_vitality` | 0.74 |
| BridgeRank | `lin` | 0.76 |
| Clustered local-degree (CLD) | `con` | 0.85 |
| Community centrality | `ego_betweenness` | 0.66 |
| Contribution centrality | `improved_closeness` | 0.88 |
| Degree and Clustering coefficient and Location (DCL) | `clusterrank` | 0.58 |
| DegreeDistance | `degree_discount` | 0.30 |
| DegreePunishment | `constraint` | 0.55 |
| DSHC method | `voterank_plus` | 0.73 |
| E-Burt | `constraint` | 0.71 |
| Effective distance gravity (EffG) | `improved_global_structure` | 0.84 |
| Entropy-based influence disseminator (EbID) | `tec` | 0.87 |
| EPC | `mu_pci` | 0.80 |
| Expected rank | `resistance_curvature` | 0.77 |
| Fractional Graph Fourier Transform (FrGFTC) | `tec` | 0.59 |
| Game centrality (GC) | `local_dimension` | 0.61 |
| Generalized gravity centrality (GGC) | `gravity` | 0.86 |
| Graph Fourier Transform Centrality (GFT-C) | `mdd` | 0.88 |
| Graph-theoretic power index (GPI) | `fuzzy_local_dimension` | 0.41 |
| Graphlet degree centrality (GDC) | `hybrid` | 0.83 |
| Gromov centrality | `salsa` | 0.81 |
| Hierarchical reduction by betweenness | `godfather` | 0.87 |
| Hybrid (Pozzi) | `mu_pci` | 0.00 |
| HybridRank | `degree_discount` | 0.27 |
| Improved coloring method (IIS) | `constraint` | 0.37 |
| Improved k-shell method (IKS) | `clusterrank` | 0.32 |
| Improved WVoteRank | `proximal_betweenness` | 0.67 |
| Information distance index | `tec` | 0.63 |
| Interdependence | `tec` | 0.87 |
| IS method | `dmnc` | 0.54 |
| Isolating Centrality (ISC) | `pairwisedis` | 0.89 |
| K-shell Physarum centrality | `ego_betweenness` | 0.52 |
| Link influence entropy (LInE) | `flow_coefficient` | 0.53 |
| Local degree dimension (LDD) | `iira` | 0.88 |
| Local fuzzy information centrality (LFIC) | `neighborhood_connectivity` | 0.28 |
| Local RASP | `load` | 0.58 |
| LRIC (maxmin) | `lric_max` | 0.72 |
| LRIC-sim | `voterank_plus` | 0.67 |
| MABIE | `nwrank` | 0.90 |
| Multi-criteria influence maximization (MCIM) | `iira` | 0.68 |
| Multi-local dimension (MLD) | `fuzzy_local_dimension` | 0.80 |
| Multiple local attributes weighted centrality (LWC) | `support` | 0.89 |
| Mutual information | `shapley_game1` | 0.82 |
| Neighborhood density | `support` | 0.78 |
| Node importance contribution matrix (NICM) method | `closeness_vitality` | 0.81 |
| Node information dimension (NID) | `local_dimension` | 0.41 |
| Node local centrality (NLC) | `mcc` | 0.87 |
| Outward accessibility | `icentr` | 0.85 |
| Partition-Based Spreaders Identification (PBSI) | `tec` | 0.64 |
| PathRank | `nonbacktracking` | 0.90 |
| Physarum centrality | `bottleneck` | 0.44 |
| Random walk accessibility (RWA) | `icentr` | 0.87 |
| Relative local–global importance (RLGI) | `malatya` | 0.82 |
| Return Random Walk Gravity (RRWG) | `iira` | 0.84 |
| Semi-local degree and clustering coefficient | `kreach` | 0.76 |
| Shapley Value based Information Delimiters (SVID) | `tec` | 0.74 |
| Shortest cycle closeness (SCC) | `improved_closeness` | 0.86 |
| Spreading strength | `geodesic_kpath` | 0.88 |
| SRIC | `leverage` | 0.82 |
| theta-centrality | `gravity` | 0.90 |
| Truncated curvature | `iira` | 0.57 |
| Two-step framework (IF) | `effective_size` | 0.60 |
| Weighted community betweenness | `bridging` | 0.88 |
| Weighted volume centrality | `geodesic_kpath` | 0.78 |

Generated by `docs/zoo/zoo_coverage.py`; see `CENTRALITY-ZOO-COVERAGE.md`
for the same data with nearest-measure detail and the skipped list.
