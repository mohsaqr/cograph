# Looking for a centrality from the Zoo? Here is what cograph has

Every measure in the [Centrality Zoo](https://centralityzoo.github.io/comparison/)
(349 measures, Shvydun 2025) appears exactly once below. `tau` is the
average Kendall rank correlation between Zoo implementations over 648
empirical networks. A mapped cograph name identifies a corresponding
definition; these are not comparisons against cograph outputs. Values are
rounded: displayed 1.00 does not prove exact rank or numerical equivalence.

| Section | Meaning | Count |
|---|---|---|
| Available | cograph implements the measure itself | 252 |
| Correlated candidate (tau >= 0.90) | not implemented; a mapped Zoo measure has high average rank correlation | 33 |
| Not available (tau < 0.90) | not implemented; nearest mapped Zoo measure has lower average correlation | 64 |

Only 252 of the 349 Zoo labels are mapped as implemented.
Another 33 have a correlated candidate, including 15
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
| All-around score | `all_around` | `centrality(x, measures = "all_around")` |
| Analytic Hierarchy Process (AHP) centrality | `ahp` | `centrality(x, measures = "ahp")` |
| ArticleRank | `articlerank` | `centrality(x, measures = "articlerank")` |
| Average shortest path centrality (AC) | `asp_centrality` | `centrality(x, measures = "asp_centrality")` |
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
| Classified neighbors centrality | `classified_neighbors` | `centrality(x, measures = "classified_neighbors")` |
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
| Correlation centrality | `correlation_centrality` | `centrality(x, measures = "correlation_centrality")` |
| Counting Betweenness | `counting_betweenness` | `centrality(x, measures = "counting_betweenness")` |
| Cross-Clique Connectivity | `cross_clique` | `centrality(x, measures = "cross_clique")` |
| Current-flow betweenness | `current_flow_betweenness` | `centrality(x, measures = "current_flow_betweenness")` |
| Current-flow Closeness | `current_flow_closeness` | `centrality(x, measures = "current_flow_closeness")` |
| Decay | `decay` | `centrality(x, measures = "decay")` |
| Degree | `degree` | `centrality(x, measures = "degree")` |
| Degree and clustering coefficient (DCC) | `dcc` | `centrality(x, measures = "dcc")` |
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
| Diversity-strength centrality (DSC) | `dsc` | `centrality(x, measures = "dsc")` |
| Diversity-strength ranking (DSR) | `dsr` | `centrality(x, measures = "dsr")` |
| DK-based gravity model (DKGM) | `dkgm` | `centrality(x, measures = "dkgm")` |
| DMNC | `dmnc` | `centrality(x, measures = "dmnc")` |
| DST | `dst` | `centrality(x, measures = "dst")` |
| Dynamical importance | `dynamical_importance` | `centrality(x, measures = "dynamical_importance")` |
| Dynamics-sensitive (DS) centrality | `dynamics_sensitive` | `centrality(x, measures = "dynamics_sensitive", ds_beta = 0.1, ds_mu = 1, ds_steps = 5)` |
| Eccentricity | `eccentricity` | `centrality(x, measures = "eccentricity")` |
| Edge clustering coefficient centrality (NC) | `edge_clustering` | `centrality(x, measures = "edge_clustering")` |
| Effective gravity model (EGM) | `egm` | `centrality(x, measures = "egm")` |
| Effective size | `effective_size` | `centrality(x, measures = "effective_size")` |
| Egocentric betweenness | `ego_betweenness` | `centrality(x, measures = "ego_betweenness")` |
| Eigentrust | `pagerank` | `centrality(x, measures = "pagerank", damping = 1 - a, personalized = p)` |
| Eigenvector | `eigenvector` | `centrality(x, measures = "eigenvector")` |
| EnRenew | `enrenew` | `centrality(x, measures = "enrenew")` |
| Entropy | `entropy` | `centrality(x, measures = "entropy")` |
| Entropy and mutual information-based centrality (EMI) | `emi` | `centrality(x, measures = "emi")` |
| Entropy variation (betweenness) | `entropy_variation_betweenness` | `centrality(x, measures = "entropy_variation_betweenness")` |
| Entropy variation (degree) | `entropy_variation_degree` | `centrality(x, measures = "entropy_variation_degree")` |
| Entropy-based gravity model | `entropy_gravity` | `centrality(x, measures = "entropy_gravity")` |
| Entropy-Based Ranking Measure (ERM) | `erm` | `centrality(x, measures = "erm")` |
| Exogenous centrality | `exogenous` | `centrality(x, measures = "exogenous")` |
| Expected force (ExF) | `expected_force` | `centrality(x, measures = "expected_force")` |
| Expected force (ExFm) | `modified_expected_force` | `centrality(x, measures = "modified_expected_force")` |
| Extended Cluster Coefficient Ranking Measure (ECRM) | `ecrm` | `centrality(x, measures = "ecrm")` |
| Extended diversity-strength ranking (EDSR) | `edsr` | `centrality(x, measures = "edsr")` |
| Extended gravity centrality | `extended_gravity` | `centrality(x, measures = "extended_gravity")` |
| Extended H-index centrality (EHC) | `ehc` | `centrality(x, measures = "ehc")` |
| Extended hybrid characteristic centrality (EHCC) | `ehcc` | `centrality(x, measures = "ehcc")` |
| Extended improved k-shell hybrid (EIKH) | `eikh` | `centrality(x, measures = "eikh")` |
| Extended k-shell hybrid method | `eskh` | `centrality(x, measures = "eskh")` |
| Extended LBC | `extended_local_bridging` | `centrality(x, measures = "extended_local_bridging")` |
| Extended mixed gravitational centrality | `extended_mixed_gravity` | `centrality(x, measures = "extended_mixed_gravity")` |
| Extended neighborhood coreness | `extended_coreness` | `centrality(x, measures = "extended_coreness")` |
| Extended RMD-weighted degree (EWD) | `ewd` | `centrality(x, measures = "ewd")` |
| Extended weight degree centrality (EWdc) | `ewdc` | `centrality(x, measures = "ewdc")` |
| Flow betweenness | `flow_betweenness` | `centrality(x, measures = "flow_betweenness")` |
| Flow coefficient | `flow_coefficient` | `centrality(x, measures = "flow_coefficient")` |
| Fuzzy local dimension (FLD) | `fuzzy_local_dimension` | `centrality(x, measures = "fuzzy_local_dimension")` |
| Gateway coefficient | `gateway` | `centrality(x, measures = "gateway")` |
| Geodesic k-path | `geodesic_kpath` | `centrality(x, measures = "geodesic_kpath")` |
| Gil-Schmidt Power Index | `gilschmidt` | `centrality(x, measures = "gilschmidt")` |
| Global and local information (GLI) method | `gli` | `centrality(x, measures = "gli")` |
| Global and Local Structure(GLS) | `gls` | `centrality(x, measures = "gls")` |
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
| Hierarchical k-shell (HKS) | `hks` | `centrality(x, measures = "hks")` |
| Hubbel | `hubbell` | `centrality(x, measures = "hubbell")` |
| Hybrid centrality (HC) | `hybrid` | `centrality(x, measures = "hybrid")` |
| Hybrid Characteristic Centrality (HCC) | `hcc` | `centrality(x, measures = "hcc")` |
| Hybrid degree centrality | `hybrid_degree` | `centrality(x, measures = "hybrid_degree", hc_p = p)` |
| Hybrid Global Structure Model (H-GSM) | `hybrid_global_structure` | `centrality(x, measures = "hybrid_global_structure")` |
| Icentr | `icentr` | `centrality(x, measures = "icentr")` |
| Immediate Effects Centrality (IEC) | `iec` | `centrality(x, measures = "iec")` |
| Improved closeness centrality (ICC) | `improved_closeness` | `centrality(x, measures = "improved_closeness")` |
| Improved entropy-based centrality | `improved_entropy` | `centrality(x, measures = "improved_entropy")` |
| Improved global structure model (IGSM) | `improved_global_structure` | `centrality(x, measures = "improved_global_structure")` |
| Improved IMC | `node_contraction_improved` | `centrality(x, measures = "node_contraction_improved")` |
| Improved iterative resource allocation (IIRA) | `iira` | `centrality(x, measures = "iira")` |
| Improved K-shell decomposition (IKSD) | `iksd` | `centrality(x, measures = "iksd")` |
| Improved k-shell hybrid (IKH) | `ikh` | `centrality(x, measures = "ikh")` |
| Improved neighbors’ k-core (INK) | `ink` | `centrality(x, measures = "ink")` |
| INF centrality | `beta_measure` | `centrality(x, measures = "beta_measure")` |
| Infection number | `infection` | `centrality(x, measures = "infection")` |
| Influence capability (IC) | `influence_capability` | `centrality(x, measures = "influence_capability")` |
| Integration | `integration` | `centrality(x, measures = "integration")` |
| Intra-module degree | `within_module_z` | `centrality(x, measures = "within_module_z")` |
| Inward accessibility | `inward_accessibility` | `centrality(x, measures = "inward_accessibility", ia_steps = h)` |
| Iterative resource allocation (IRA) | `ira` | `centrality(x, measures = "ira")` |
| k-betweenness | `betweenness` | `centrality(x, measures = "betweenness", cutoff = k)` |
| k-shell | `coreness` | `centrality(x, measures = "coreness")` |
| k-shell based on gravity centrality (KSGC) | `ksgc` | `centrality(x, measures = "ksgc")` |
| k-shell hybrid method | `ksh` | `centrality(x, measures = "ksh")` |
| k-shell iteration factor(KS-IF) | `ks_if` | `centrality(x, measures = "ks_if")` |
| k-truss number | `truss` | `centrality(x, measures = "truss")` |
| Katz | `katz` | `centrality(x, measures = "katz")` |
| KDEC method | `kdec` | `centrality(x, measures = "kdec")` |
| KED | `ked` | `centrality(x, measures = "ked")` |
| Laplacian | `laplacian` | `centrality(x, measures = "laplacian")` |
| Laplacian gravity centrality (LGC) | `lgc` | `centrality(x, measures = "lgc")` |
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
| Local structural centrality (LSC) | `lsc` | `centrality(x, measures = "lsc")` |
| Local volume dimension (LVD) | `local_volume_dimension` | `centrality(x, measures = "local_volume_dimension")` |
| Localized bridging centrality | `localized_bridging` | `centrality(x, measures = "localized_bridging")` |
| LocalRank | `semilocal` | `centrality(x, measures = "semilocal")` |
| LRIC (max) | `lric_max` | `centrality(x, measures = "lric_max")` |
| M-centrality | `m_centrality` | `centrality(x, measures = "m_centrality")` |
| m-reach | `kreach` | `centrality(x, measures = "kreach")` |
| Malatya centrality | `malatya` | `centrality(x, measures = "malatya")` |
| Map Equation Centrality (MEC) | `map_equation` | `centrality(x, measures = "map_equation")` |
| Mapping entropy (ME) | `mapping_entropy` | `centrality(x, measures = "mapping_entropy")` |
| Mapping Entropy Betweenness (MEB) | `mapping_entropy_betweenness` | `centrality(x, measures = "mapping_entropy_betweenness")` |
| Markov | `markov` | `centrality(x, measures = "markov")` |
| MCC | `mcc` | `centrality(x, measures = "mcc")` |
| Mediative Effects Centrality (MEC) | `mec` | `centrality(x, measures = "mec")` |
| Mixed core, degree and entropy (MCDE) | `mcde` | `centrality(x, measures = "mcde")` |
| Mixed core, degree and weighted entropy (MCDWE) | `mcdwe` | `centrality(x, measures = "mcdwe")` |
| Mixed core, semi-local degree and entropy (MCSDE) | `mcsde` | `centrality(x, measures = "mcsde")` |
| Mixed core, semi-local degree and weighted entropy (MCSDWE) | `mcsdwe` | `centrality(x, measures = "mcsdwe")` |
| Mixed Degree Decomposition (MDD) | `mdd` | `centrality(x, measures = "mdd")` |
| Mixed gravitational centrality | `mixed_gravity` | `centrality(x, measures = "mixed_gravity")` |
| MNC | `mnc` | `centrality(x, measures = "mnc")` |
| Modified Local Centrality (MLC) | `mlc` | `centrality(x, measures = "mlc")` |
| Modularity vitality | `modularity_vitality` | `centrality(x, measures = "modularity_vitality")` |
| Multi-characteristics gravity model (MCGM) | `mcgm` | `centrality(x, measures = "mcgm")` |
| Multi-evidence centrality (MeC) | `multi_evidence` | `centrality(x, measures = "multi_evidence")` |
| NCVoteRank | `ncvoterank` | `centrality(x, measures = "ncvoterank")` |
| Neighbor distance centrality | `neighbor_distance` | `centrality(x, measures = "neighbor_distance")` |
| Neighborhood centrality | `neighbor_distance` | `centrality(x, measures = "neighbor_distance")` |
| Neighborhood connectivity | `neighborhood_connectivity` | `centrality(x, measures = "neighborhood_connectivity")` |
| NL centrality | `nl` | `centrality(x, measures = "nl")` |
| Node and Neighbor Layer Information (NINL) centrality | `ninl` | `centrality(x, measures = "ninl")` |
| Node contraction (IMC) | `node_contraction` | `centrality(x, measures = "node_contraction")` |
| Node importance contribution correlation matrix (NICCM) method | `niccm` | `centrality(x, measures = "niccm")` |
| Node importance evaluation matrix (NIEM) method | `niem` | `centrality(x, measures = "niem")` |
| Non-backtracking centrality | `nonbacktracking` | `centrality(x, measures = "nonbacktracking")` |
| NWRank | `nwrank` | `centrality(x, measures = "nwrank")` |
| PageRank | `pagerank` | `centrality(x, measures = "pagerank")` |
| Pairwise disconnectivity | `pairwisedis` | `centrality(x, measures = "pairwisedis")` |
| Participation coefficient | `participation` | `centrality(x, measures = "participation")` |
| Path-transfer centrality | `path_transfer` | `centrality(x, measures = "path_transfer")` |
| Percolation | `percolation` | `centrality(x, measures = "percolation")` |
| PhysarumSpreader | `physarum_spreader` | `centrality(x, measures = "physarum_spreader", physarum_steps = t)` |
| ProfitLeader | `profitleader` | `centrality(x, measures = "profitleader")` |
| Proximal betwenness | `proximal_betweenness` | `centrality(x, measures = "proximal_betweenness")` |
| Random walk centrality | `random_walk` | `centrality(x, measures = "random_walk")` |
| Random walk decay | `random_walk_decay` | `centrality(x, measures = "random_walk_decay")` |
| Randomized shortest paths (RSP) betweenness | `rsp_betweenness` | `centrality(x, measures = "rsp_betweenness")` |
| RCFB centrality | `rcfb` | `centrality(x, measures = "rcfb")` |
| Redundancy | `redundancy` | `centrality(x, measures = "redundancy")` |
| Relative entropy | `relative_entropy` | `centrality(x, measures = "relative_entropy")` |
| Renewed coreness | `renewed_coreness` | `centrality(x, measures = "renewed_coreness")` |
| Residual closeness | `residual_closeness` | `centrality(x, measures = "residual_closeness")` |
| Resistance curvature | `resistance_curvature` | `centrality(x, measures = "resistance_curvature")` |
| RMD-weighted degree (WD) | `wd` | `centrality(x, measures = "wd")` |
| Rumor centrality | `rumor` | `centrality(x, measures = "rumor")` |
| s-shell index | `s_shell` | `centrality(x, measures = "s_shell")` |
| SALSA | `salsa` | `centrality(x, measures = "salsa")` |
| Second order centrality | `second_order` | `centrality(x, measures = "second_order")` |
| Seeley’s index | `pagerank` | `centrality(x, measures = "pagerank", damping = 1)` |
| Semi-local iterative algorithm (semi-IA) | `semi_ia` | `centrality(x, measures = "semi_ia")` |
| Semi-local ranking (SLC) | `semilocal` | `centrality(x, measures = "semilocal")` |
| Shapley value (game 1) | `shapley_game1` | `centrality(x, measures = "shapley_game1")` |
| Shapley value (game 2) | `shapley_game2` | `centrality(x, measures = "shapley_game2")` |
| Shapley value (game 3) | `shapley_game3` | `centrality(x, measures = "shapley_game3")` |
| Shell clustering coefficient | `shell_clustering` | `centrality(x, measures = "shell_clustering")` |
| SingleDiscount | `single_discount` | `centrality(x, measures = "single_discount")` |
| Spanning tree centrality (STC) | `spanning_tree` | `centrality(x, measures = "spanning_tree")` |
| SpectralRank | `spectralrank` | `centrality(x, measures = "spectralrank")` |
| Spreading probability (SP) | `spreading_probability` | `centrality(x, measures = "spreading_probability")` |
| Stress | `stress` | `centrality(x, measures = "stress")` |
| Subgraph | `subgraph` | `centrality(x, measures = "subgraph")` |
| Support | `support` | `centrality(x, measures = "support")` |
| Synthesize centrality (SC) | `synthesize` | `centrality(x, measures = "synthesize")` |
| Topological | `topological_coefficient` | `centrality(x, measures = "topological_coefficient")` |
| TOPSIS | `topsis` | `centrality(x, measures = "topsis")` |
| TOPSIS-RE | `topsis_re` | `centrality(x, measures = "topsis_re")` |
| Total communicability | `communicability` | `centrality(x, measures = "communicability")` |
| Total Effects Centrality (TEC) | `tec` | `centrality(x, measures = "tec")` |
| Transportation centrality | `transportation` | `centrality(x, measures = "transportation")` |
| Trust-PageRank | `trust_pagerank` | `centrality(x, measures = "trust_pagerank")` |
| Two-way random walk betweenness (2RW) | `two_way_rw` | `centrality(x, measures = "two_way_rw")` |
| Volume centrality | `volume` | `centrality(x, measures = "volume")` |
| VoteRank | `voterank` | `centrality(x, measures = "voterank")` |
| VoteRank++ | `voterank_plus` | `centrality(x, measures = "voterank_plus")` |
| Weight degree centrality (Wdc) | `wdc` | `centrality(x, measures = "wdc")` |
| Weight neighborhood centrality | `weight_neighborhood` | `centrality(x, measures = "weight_neighborhood")` |
| Weighted formal concept analysis (WFCA) | `wfca` | `centrality(x, measures = "wfca")` |
| Weighted gravity model (WGravity) | `wgravity` | `centrality(x, measures = "wgravity")` |
| Weighted h-index | `weighted_h_index` | `centrality(x, measures = "weighted_h_index")` |
| Weighted k-shell decomposition (Wks) | `weighted_kshell` | `centrality(x, measures = "weighted_kshell")` |
| Weighted k-shell degree neighborhood (Maji) | `wksdn` | `centrality(x, measures = "wksdn")` |
| Weighted k-shell degree neighborhood (Wksd) | `wksd` | `centrality(x, measures = "wksd", wksd_alpha = alpha, wksd_mu = mu)` |
| Weighted LeaderRank | `weighted_leaderrank` | `centrality(x, measures = "weighted_leaderrank")` |
| Weighted TOPSIS | `topsis` | `centrality(x, measures = "topsis", topsis_weights = w)` |
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
| Cc-Burt | `iira` | `centrality(x, measures = "iira")` | 0.90 | high average rank correlation; not equivalence |
| Cumulative contact probability (CCP) | `tec` | `centrality(x, measures = "tec")` | 1.00 | high average rank correlation; not equivalence |
| Curvature index | `dil` | `centrality(x, measures = "dil")` | 0.93 | high average rank correlation; not equivalence |
| Degree mass | `diffusion` | `centrality(x, measures = "diffusion")` | 1.00 | high average rank correlation; not equivalence |
| DirichletRank | `tec` | `centrality(x, measures = "tec")` | 1.00 | high average rank correlation; not equivalence |
| Dynamical Influence | `eigenvector` | `centrality(x, measures = "eigenvector")` | 1.00 | high average rank correlation; not equivalence |
| Edge-disjoint k-path | `iira` | `centrality(x, measures = "iira")` | 0.93 | high average rank correlation; not equivalence |
| Efficiency centrality (EffC) | `fragmentation` | `centrality(x, measures = "fragmentation")` | 1.00 | high average rank correlation; not equivalence |
| Electrical closeness | `current_flow_closeness` | `centrality(x, measures = "current_flow_closeness")` | 1.00 | high average rank correlation; not equivalence |
| epsilon-betweenness | `percolation` | `centrality(x, measures = "percolation")` | 1.00 | high average rank correlation; not equivalence |
| Integral k-shell | `neighbor_distance` | `centrality(x, measures = "neighbor_distance")` | 0.91 | high average rank correlation; not equivalence |
| k-path | `diffusion` | `centrality(x, measures = "diffusion")` | 0.99 | high average rank correlation; not equivalence |
| Linearly scaled betweenness | `percolation` | `centrality(x, measures = "percolation")` | 1.00 | high average rank correlation; not equivalence |
| LRIC (PPR) | `tec` | `centrality(x, measures = "tec")` | 0.93 | high average rank correlation; not equivalence |
| MABIE | `topsis` | `centrality(x, measures = "topsis")` | 0.91 | high average rank correlation; not equivalence |
| Meta-centrality | `iira` | `centrality(x, measures = "iira")` | 0.93 | high average rank correlation; not equivalence |
| Neighborhood core diversity centrality (Cncd) | `extended_coreness` | `centrality(x, measures = "extended_coreness")` | 0.98 | high average rank correlation; not equivalence |
| New evidential centrality (NEC) | `tec` | `centrality(x, measures = "tec")` | 0.91 | high average rank correlation; not equivalence |
| Nieminen's closeness | `lin` | `centrality(x, measures = "lin")` | 1.00 | high average rank correlation; not equivalence |
| Normalized local centrality (NLC) | `lsc` | `centrality(x, measures = "lsc")` | 0.96 | high average rank correlation; not equivalence |
| Probabilistic-jumping Random Walk (PJRW) | `tec` | `centrality(x, measures = "tec")` | 0.91 | high average rank correlation; not equivalence |
| QJSD centrality | `tec` | `centrality(x, measures = "tec")` | 1.00 | high average rank correlation; not equivalence |
| Random walk-based gravity (DFS-Gravity) | `laplacian` | `centrality(x, measures = "laplacian")` | 0.94 | high average rank correlation; not equivalence |
| Routing betweenness | `load` | `centrality(x, measures = "load")` | 1.00 | high average rank correlation; not equivalence |
| Similarity-based PageRank | `malatya` | `centrality(x, measures = "malatya")` | 0.91 | high average rank correlation; not equivalence |
| Total centrality | `fragmentation` | `centrality(x, measures = "fragmentation")` | 1.00 | high average rank correlation; not equivalence |
| Vertex-disjoint k-path | `iira` | `centrality(x, measures = "iira")` | 0.92 | high average rank correlation; not equivalence |
| ViralRank | `current_flow_closeness` | `centrality(x, measures = "current_flow_closeness")` | 1.00 | high average rank correlation; not equivalence |
| VMM algorithm | `tec` | `centrality(x, measures = "tec")` | 0.96 | high average rank correlation; not equivalence |
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
| Contribution centrality | `semi_ia` | 0.88 |
| Degree and Clustering coefficient and Location (DCL) | `clusterrank` | 0.58 |
| DegreeDistance | `degree_discount` | 0.30 |
| DegreePunishment | `constraint` | 0.55 |
| DSHC method | `voterank_plus` | 0.73 |
| E-Burt | `constraint` | 0.71 |
| Effective distance gravity (EffG) | `dcc` | 0.87 |
| Entropy-based influence disseminator (EbID) | `tec` | 0.87 |
| EPC | `mu_pci` | 0.80 |
| Expected rank | `resistance_curvature` | 0.77 |
| Fractional Graph Fourier Transform (FrGFTC) | `tec` | 0.59 |
| Game centrality (GC) | `local_dimension` | 0.61 |
| Generalized gravity centrality (GGC) | `gravity` | 0.86 |
| Graph Fourier Transform Centrality (GFT-C) | `mdd` | 0.88 |
| Graph-theoretic power index (GPI) | `ksgc` | 0.52 |
| Graphlet degree centrality (GDC) | `spreading_probability` | 0.84 |
| Gromov centrality | `salsa` | 0.81 |
| Hierarchical reduction by betweenness | `godfather` | 0.87 |
| Hybrid (Pozzi) | `mu_pci` | 0.00 |
| HybridRank | `degree_discount` | 0.27 |
| Improved coloring method (IIS) | `constraint` | 0.37 |
| Improved k-shell method (IKS) | `lgc` | 0.36 |
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
| Multi-criteria influence maximization (MCIM) | `ksgc` | 0.72 |
| Multi-local dimension (MLD) | `fuzzy_local_dimension` | 0.80 |
| Multiple local attributes weighted centrality (LWC) | `support` | 0.89 |
| Mutual information | `shapley_game1` | 0.82 |
| Neighborhood density | `profitleader` | 0.80 |
| Node importance contribution matrix (NICM) method | `asp_centrality` | 0.82 |
| Node information dimension (NID) | `local_dimension` | 0.41 |
| Node local centrality (NLC) | `mcc` | 0.87 |
| Outward accessibility | `icentr` | 0.85 |
| Partition-Based Spreaders Identification (PBSI) | `tec` | 0.64 |
| PathRank | `nonbacktracking` | 0.90 |
| Physarum centrality | `bottleneck` | 0.44 |
| Random walk accessibility (RWA) | `icentr` | 0.87 |
| Relative local–global importance (RLGI) | `articlerank` | 0.83 |
| Return Random Walk Gravity (RRWG) | `iira` | 0.84 |
| Semi-local degree and clustering coefficient | `kreach` | 0.76 |
| Shapley Value based Information Delimiters (SVID) | `tec` | 0.74 |
| Shortest cycle closeness (SCC) | `improved_closeness` | 0.86 |
| Spreading strength | `geodesic_kpath` | 0.88 |
| SRIC | `leverage` | 0.82 |
| theta-centrality | `gravity` | 0.90 |
| Truncated curvature | `edge_clustering` | 0.63 |
| Two-step framework (IF) | `effective_size` | 0.60 |
| Weighted community betweenness | `bridging` | 0.88 |
| Weighted volume centrality | `geodesic_kpath` | 0.78 |

Generated by `docs/zoo/zoo_coverage.py`; see `CENTRALITY-ZOO-COVERAGE.md`
for the same data with nearest-measure detail and the skipped list.
