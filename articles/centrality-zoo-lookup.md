# cograph and the Centrality Zoo

The number of proposed node centralities has grown far faster than the
number of distinct ideas behind them. Many new measures recombine a
small set of ingredients, such as degree, shortest-path distance, walk
counts, the leading eigenvector, the k-shell decomposition and local
clustering. A practitioner who meets an unfamiliar measure needs to know
whether it is available and, more importantly, whether it adds
information to simpler measures.

`cograph` offers the largest collection of node centralities of any
package compared in this article, in R or in Python. Its 274 measures
reach 252 of the 349 measures catalogued in the Centrality Zoo (Shvydun,
2025). Nine established centrality packages, brainGraph, centiserve,
CINNA, igraph, influenceR, keyplayer, NetworkX, sna and tidygraph, reach
62 between them when every function they offer is pooled (Table 1). The
collection is also growing quickly: `cograph` 2.4.4, the release on CRAN
in July 2026, offered 89 measures, and the current version offers 274,
3.1 times as many. Every measure is computed by a single function,
[`centrality()`](https://sonsoles.me/cograph/reference/centrality.md),
which returns a tidy data frame.

| Package                 | Language | Centrality functions | Zoo measures reached |
|:------------------------|:---------|---------------------:|---------------------:|
| **cograph**             | R        |                  274 |                  252 |
| CINNA                   | R        |                   49 |                   38 |
| centiserve              | R        |                   33 |                   30 |
| NetworkX                | Python   |                   30 |                   26 |
| tidygraph               | R        |                   25 |                   21 |
| igraph                  | R        |                   18 |                   16 |
| sna                     | R        |                   11 |                    8 |
| influenceR              | R        |                    4 |                    6 |
| brainGraph              | R        |                    7 |                    4 |
| keyplayer               | R        |                    4 |                    3 |
| All nine others, pooled |          |                  181 |                   62 |

Table 1. Node centralities in cograph and in nine other packages. A
function counts towards the Zoo only if the measure it computes is
catalogued there; two functions computing the same measure count once.
{.table}

## Zoo measures available in cograph

| Zoo measure | cograph measure |
|:---|:---|
| Access information | `access_information` |
| Adaptive LeaderRank | `adaptive_leaderrank` |
| All cycle betweenness (ACC) | `all_cycle_betweenness` |
| All-around score | `all_around` |
| Analytic Hierarchy Process (AHP) centrality | `ahp` |
| ArticleRank | `articlerank` |
| Average shortest path centrality (AC) | `asp_centrality` |
| beta-measure | `beta_measure` |
| Betweenness | `betweenness` |
| BG-index | `beta_measure` |
| Biased random walk centrality | `biased_random_walk` |
| Borgatti’s effective size | `effective_size` |
| BottleNeck | `bottleneck` |
| Bridging capital | `bridging_capital` |
| Bridging centrality | `bridging` |
| Bridging coefficient | `bridging_coefficient` |
| Burt’s constraint | `constraint` |
| Centroid | `centroid` |
| Classified neighbors centrality | `classified_neighbors` |
| Closeness | `closeness` |
| Closeness vitality | `closeness_vitality` |
| Clustering degree algorithm (CDA) | `cda` |
| ClusterRank | `clusterrank` |
| Coleman-Theil disorder index | `coleman_theil` |
| CollInf | `collective_influence` |
| Comm Centrality | `comm_centrality` |
| Communicability betweenness | `communicability_betweenness` |
| Community Hub‑Bridge measure | `community_hub_bridge` |
| Community-based centrality (CbC) | `community_based` |
| Community-based mediator (CbM) | `community_mediator` |
| CON score | `con` |
| ControlRank | `controlrank` |
| Correlation centrality | `correlation_centrality` |
| Counting Betweenness | `counting_betweenness` |
| Cross-Clique Connectivity | `cross_clique` |
| Current-flow betweenness | `current_flow_betweenness` |
| Current-flow Closeness | `current_flow_closeness` |
| Decay | `decay` |
| Degree | `degree` |
| Degree and clustering coefficient (DCC) | `dcc` |
| Degree and Importance of Lines (DIL) | `dil` |
| DegreeDiscountIC | `degree_discount` |
| delta-betweenness | `delta_betweenness` |
| delta-closeness | `delta_closeness` |
| Density centrality | `density_centrality` |
| Diffusion centrality | `diffusion_centrality` |
| Diffusion Degree | `diffusion` |
| Distance entropy | `distance_entropy` |
| Distance-weighted fragmentation | `fragmentation` |
| Diversity coefficient | `diversity` |
| Diversity-strength centrality (DSC) | `dsc` |
| Diversity-strength ranking (DSR) | `dsr` |
| DK-based gravity model (DKGM) | `dkgm` |
| DMNC | `dmnc` |
| DST | `dst` |
| Dynamical importance | `dynamical_importance` |
| Dynamics-sensitive (DS) centrality | `dynamics_sensitive` |
| Eccentricity | `eccentricity` |
| Edge clustering coefficient centrality (NC) | `edge_clustering` |
| Effective gravity model (EGM) | `egm` |
| Effective size | `effective_size` |
| Egocentric betweenness | `ego_betweenness` |
| Eigentrust | `pagerank` |
| Eigenvector | `eigenvector` |
| EnRenew | `enrenew` |
| Entropy | `entropy` |
| Entropy and mutual information-based centrality (EMI) | `emi` |
| Entropy variation (betweenness) | `entropy_variation_betweenness` |
| Entropy variation (degree) | `entropy_variation_degree` |
| Entropy-based gravity model | `entropy_gravity` |
| Entropy-Based Ranking Measure (ERM) | `erm` |
| Exogenous centrality | `exogenous` |
| Expected force (ExF) | `expected_force` |
| Expected force (ExFm) | `modified_expected_force` |
| Extended Cluster Coefficient Ranking Measure (ECRM) | `ecrm` |
| Extended diversity-strength ranking (EDSR) | `edsr` |
| Extended gravity centrality | `extended_gravity` |
| Extended H-index centrality (EHC) | `ehc` |
| Extended hybrid characteristic centrality (EHCC) | `ehcc` |
| Extended improved k-shell hybrid (EIKH) | `eikh` |
| Extended k-shell hybrid method | `eskh` |
| Extended LBC | `extended_local_bridging` |
| Extended mixed gravitational centrality | `extended_mixed_gravity` |
| Extended neighborhood coreness | `extended_coreness` |
| Extended RMD-weighted degree (EWD) | `ewd` |
| Extended weight degree centrality (EWdc) | `ewdc` |
| Flow betweenness | `flow_betweenness` |
| Flow coefficient | `flow_coefficient` |
| Fuzzy local dimension (FLD) | `fuzzy_local_dimension` |
| Gateway coefficient | `gateway` |
| Geodesic k-path | `geodesic_kpath` |
| Gil-Schmidt Power Index | `gilschmidt` |
| Global and local information (GLI) method | `gli` |
| Global and Local Structure(GLS) | `gls` |
| Global importance of nodes (GIN) | `gin` |
| Global Structure Model (GSM) | `global_structure` |
| Godfather index | `godfather` |
| Graph regularization centrality (GRC) | `graph_regularization` |
| Gravity centrality | `gravity` |
| Gravity model | `gravity` |
| h-index strength | `hindex_strength` |
| Harmonic | `harmonic` |
| Heatmap centrality | `heatmap` |
| Hide information | `hide_information` |
| Hierarchical k-shell (HKS) | `hks` |
| Hubbel | `hubbell` |
| Hybrid centrality (HC) | `hybrid` |
| Hybrid Characteristic Centrality (HCC) | `hcc` |
| Hybrid degree centrality | `hybrid_degree` |
| Hybrid Global Structure Model (H-GSM) | `hybrid_global_structure` |
| Icentr | `icentr` |
| Immediate Effects Centrality (IEC) | `iec` |
| Improved closeness centrality (ICC) | `improved_closeness` |
| Improved entropy-based centrality | `improved_entropy` |
| Improved global structure model (IGSM) | `improved_global_structure` |
| Improved IMC | `node_contraction_improved` |
| Improved iterative resource allocation (IIRA) | `iira` |
| Improved K-shell decomposition (IKSD) | `iksd` |
| Improved k-shell hybrid (IKH) | `ikh` |
| Improved neighbors’ k-core (INK) | `ink` |
| INF centrality | `beta_measure` |
| Infection number | `infection` |
| Influence capability (IC) | `influence_capability` |
| Integration | `integration` |
| Intra-module degree | `within_module_z` |
| Inward accessibility | `inward_accessibility` |
| Iterative resource allocation (IRA) | `ira` |
| k-betweenness | `betweenness` |
| k-shell | `coreness` |
| k-shell based on gravity centrality (KSGC) | `ksgc` |
| k-shell hybrid method | `ksh` |
| k-shell iteration factor(KS-IF) | `ks_if` |
| k-truss number | `truss` |
| Katz | `katz` |
| KDEC method | `kdec` |
| KED | `ked` |
| Laplacian | `laplacian` |
| Laplacian gravity centrality (LGC) | `lgc` |
| LeaderRank | `leaderrank` |
| Length-scaled betweenness | `length_scaled_betweenness` |
| Leverage | `leverage` |
| Lhc method | `lhc` |
| Lin’s index | `lin` |
| LineRank | `linerank` |
| Load | `load` |
| Lobby index | `lobby` |
| Local clustering coefficient | `transitivity` |
| Local dimension (LD) | `local_dimension_fixed` |
| Local dimension (Pu) | `local_dimension` |
| Local entropy (LE) | `local_entropy` |
| Local gravity model | `gravity` |
| Local H-index | `local_hindex` |
| Local information dimensionality (LID) | `local_information_dimension` |
| Local neighbor contribution (LNC) | `lnc` |
| Local structural centrality (LSC) | `lsc` |
| Local volume dimension (LVD) | `local_volume_dimension` |
| Localized bridging centrality | `localized_bridging` |
| LocalRank | `semilocal` |
| LRIC (max) | `lric_max` |
| M-centrality | `m_centrality` |
| m-reach | `kreach` |
| Malatya centrality | `malatya` |
| Map Equation Centrality (MEC) | `map_equation` |
| Mapping entropy (ME) | `mapping_entropy` |
| Mapping Entropy Betweenness (MEB) | `mapping_entropy_betweenness` |
| Markov | `markov` |
| MCC | `mcc` |
| Mediative Effects Centrality (MEC) | `mec` |
| Mixed core, degree and entropy (MCDE) | `mcde` |
| Mixed core, degree and weighted entropy (MCDWE) | `mcdwe` |
| Mixed core, semi-local degree and entropy (MCSDE) | `mcsde` |
| Mixed core, semi-local degree and weighted entropy (MCSDWE) | `mcsdwe` |
| Mixed Degree Decomposition (MDD) | `mdd` |
| Mixed gravitational centrality | `mixed_gravity` |
| MNC | `mnc` |
| Modified Local Centrality (MLC) | `mlc` |
| Modularity vitality | `modularity_vitality` |
| Multi-characteristics gravity model (MCGM) | `mcgm` |
| Multi-evidence centrality (MeC) | `multi_evidence` |
| NCVoteRank | `ncvoterank` |
| Neighbor distance centrality | `neighbor_distance` |
| Neighborhood centrality | `neighbor_distance` |
| Neighborhood connectivity | `neighborhood_connectivity` |
| NL centrality | `nl` |
| Node and Neighbor Layer Information (NINL) centrality | `ninl` |
| Node contraction (IMC) | `node_contraction` |
| Node importance contribution correlation matrix (NICCM) method | `niccm` |
| Node importance evaluation matrix (NIEM) method | `niem` |
| Non-backtracking centrality | `nonbacktracking` |
| NWRank | `nwrank` |
| PageRank | `pagerank` |
| Pairwise disconnectivity | `pairwisedis` |
| Participation coefficient | `participation` |
| Path-transfer centrality | `path_transfer` |
| Percolation | `percolation` |
| PhysarumSpreader | `physarum_spreader` |
| ProfitLeader | `profitleader` |
| Proximal betwenness | `proximal_betweenness` |
| Random walk centrality | `random_walk` |
| Random walk decay | `random_walk_decay` |
| Randomized shortest paths (RSP) betweenness | `rsp_betweenness` |
| RCFB centrality | `rcfb` |
| Redundancy | `redundancy` |
| Relative entropy | `relative_entropy` |
| Renewed coreness | `renewed_coreness` |
| Residual closeness | `residual_closeness` |
| Resistance curvature | `resistance_curvature` |
| RMD-weighted degree (WD) | `wd` |
| Rumor centrality | `rumor` |
| s-shell index | `s_shell` |
| SALSA | `salsa` |
| Second order centrality | `second_order` |
| Seeley’s index | `pagerank` |
| Semi-local iterative algorithm (semi-IA) | `semi_ia` |
| Semi-local ranking (SLC) | `semilocal` |
| Shapley value (game 1) | `shapley_game1` |
| Shapley value (game 2) | `shapley_game2` |
| Shapley value (game 3) | `shapley_game3` |
| Shell clustering coefficient | `shell_clustering` |
| SingleDiscount | `single_discount` |
| Spanning tree centrality (STC) | `spanning_tree` |
| SpectralRank | `spectralrank` |
| Spreading probability (SP) | `spreading_probability` |
| Stress | `stress` |
| Subgraph | `subgraph` |
| Support | `support` |
| Synthesize centrality (SC) | `synthesize` |
| Topological | `topological_coefficient` |
| TOPSIS | `topsis` |
| TOPSIS-RE | `topsis_re` |
| Total communicability | `communicability` |
| Total Effects Centrality (TEC) | `tec` |
| Transportation centrality | `transportation` |
| Trust-PageRank | `trust_pagerank` |
| Two-way random walk betweenness (2RW) | `two_way_rw` |
| Volume centrality | `volume` |
| VoteRank | `voterank` |
| VoteRank++ | `voterank_plus` |
| Weight degree centrality (Wdc) | `wdc` |
| Weight neighborhood centrality | `weight_neighborhood` |
| Weighted formal concept analysis (WFCA) | `wfca` |
| Weighted gravity model (WGravity) | `wgravity` |
| Weighted h-index | `weighted_h_index` |
| Weighted k-shell decomposition (Wks) | `weighted_kshell` |
| Weighted k-shell degree neighborhood (Maji) | `wksdn` |
| Weighted k-shell degree neighborhood (Wksd) | `wksd` |
| Weighted LeaderRank | `weighted_leaderrank` |
| Weighted TOPSIS | `topsis` |
| WVoteRank | `wvoterank` |
| X-degree centrality | `x_degree` |
| X-nonbacktracking centrality | `x_nonbacktracking` |
| μ-Power Community Index (μ-PCI) | `mu_pci` |

## Zoo measures missing from cograph

| Zoo measure | Closest measure in cograph | Zoo correlation |
|:---|:---|:---|
| AIC | `closeness` | 1.00 |
| Cumulative contact probability (CCP) | `degree` | 1.00 |
| Degree mass | `diffusion` | 1.00 |
| DirichletRank | `degree` | 1.00 |
| Dynamical Influence | `eigenvector` | 1.00 |
| Efficiency centrality (EffC) | `fragmentation` | 1.00 |
| Electrical closeness | `current_flow_closeness` | 1.00 |
| epsilon-betweenness | `betweenness` | 1.00 |
| Linearly scaled betweenness | `betweenness` | 1.00 |
| Nieminen’s closeness | `closeness` | 1.00 |
| QJSD centrality | `degree` | 1.00 |
| Routing betweenness | `load` | 1.00 |
| Total centrality | `fragmentation` | 1.00 |
| ViralRank | `current_flow_closeness` | 1.00 |
| Zeta vector centrality | `second_order` | 1.00 |
| k-path | `diffusion` | 0.99 |
| Neighborhood core diversity centrality (Cncd) | `extended_coreness` | 0.98 |
| Normalized local centrality (NLC) | `lsc` | 0.96 |
| VMM algorithm | `degree` | 0.96 |
| wkpath | `infection` | 0.96 |
| Random walk-based gravity (DFS-Gravity) | `laplacian` | 0.94 |
| Curvature index | `dil` | 0.93 |
| Edge-disjoint k-path | `iira` | 0.93 |
| LRIC (PPR) | `degree` | 0.93 |
| Meta-centrality | `iira` | 0.93 |
| Vertex-disjoint k-path | `iira` | 0.92 |
| Integral k-shell | `neighbor_distance` | 0.91 |
| MABIE | `topsis` | 0.91 |
| New evidential centrality (NEC) | `degree` | 0.91 |
| Probabilistic-jumping Random Walk (PJRW) | `degree` | 0.91 |
| Similarity-based PageRank | `malatya` | 0.91 |
| WRank | `niem` | 0.91 |
| Cc-Burt | `iira` | 0.90 |
| PathRank | `nonbacktracking` | 0.90 |
| theta-centrality | `gravity` | 0.90 |
| Isolating Centrality (ISC) | `pairwisedis` | 0.89 |
| Multiple local attributes weighted centrality (LWC) | `support` | 0.89 |
| Contribution centrality | `semi_ia` | 0.88 |
| Graph Fourier Transform Centrality (GFT-C) | `mdd` | 0.88 |
| Local degree dimension (LDD) | `iira` | 0.88 |
| Spreading strength | `geodesic_kpath` | 0.88 |
| Weighted community betweenness | `bridging` | 0.88 |
| Effective distance gravity (EffG) | `dcc` | 0.87 |
| Entropy-based influence disseminator (EbID) | `degree` | 0.87 |
| Hierarchical reduction by betweenness | `godfather` | 0.87 |
| Interdependence | `degree` | 0.87 |
| Node local centrality (NLC) | `mcc` | 0.87 |
| Random walk accessibility (RWA) | `icentr` | 0.87 |
| Generalized gravity centrality (GGC) | `gravity` | 0.86 |
| Shortest cycle closeness (SCC) | `improved_closeness` | 0.86 |
| Clustered local-degree (CLD) | `con` | 0.85 |
| Outward accessibility | `icentr` | 0.85 |
| Graphlet degree centrality (GDC) | `spreading_probability` | 0.84 |
| Return Random Walk Gravity (RRWG) | `iira` | 0.84 |
| Relative local–global importance (RLGI) | `articlerank` | 0.83 |
| Mutual information | `shapley_game1` | 0.82 |
| Node importance contribution matrix (NICM) method | `asp_centrality` | 0.82 |
| SRIC | `leverage` | 0.82 |
| Gromov centrality | `degree` | 0.81 |
| EPC | `mu_pci` | 0.80 |
| Multi-local dimension (MLD) | `fuzzy_local_dimension` | 0.80 |
| Neighborhood density | `profitleader` | 0.80 |
| Weighted volume centrality | `geodesic_kpath` | 0.78 |
| Expected rank | `resistance_curvature` | 0.77 |
| BridgeRank | `closeness` | 0.76 |
| Semi-local degree and clustering coefficient | `kreach` | 0.76 |
| Algebraic centrality | `closeness_vitality` | 0.74 |
| Shapley Value based Information Delimiters (SVID) | `degree` | 0.74 |
| DSHC method | `voterank_plus` | 0.73 |
| LRIC (maxmin) | `lric_max` | 0.72 |
| Multi-criteria influence maximization (MCIM) | `ksgc` | 0.72 |
| E-Burt | `constraint` | 0.71 |
| Improved WVoteRank | `proximal_betweenness` | 0.67 |
| LRIC-sim | `voterank_plus` | 0.67 |
| Community centrality | `ego_betweenness` | 0.66 |
| Absorbing Random-Walk (ARW) | `single_discount` | 0.65 |
| Partition-Based Spreaders Identification (PBSI) | `degree` | 0.64 |
| Information distance index | `degree` | 0.63 |
| Truncated curvature | `edge_clustering` | 0.63 |
| Game centrality (GC) | `local_dimension` | 0.61 |
| Two-step framework (IF) | `effective_size` | 0.60 |
| Fractional Graph Fourier Transform (FrGFTC) | `degree` | 0.59 |
| Degree and Clustering coefficient and Location (DCL) | `clusterrank` | 0.58 |
| Local RASP | `load` | 0.58 |
| DegreePunishment | `constraint` | 0.55 |
| IS method | `dmnc` | 0.54 |
| Link influence entropy (LInE) | `flow_coefficient` | 0.53 |
| Graph-theoretic power index (GPI) | `ksgc` | 0.52 |
| K-shell Physarum centrality | `ego_betweenness` | 0.52 |
| Physarum centrality | `bottleneck` | 0.44 |
| Node information dimension (NID) | `local_dimension` | 0.41 |
| Improved coloring method (IIS) | `constraint` | 0.37 |
| Improved k-shell method (IKS) | `lgc` | 0.36 |
| DegreeDistance | `degree_discount` | 0.30 |
| Local fuzzy information centrality (LFIC) | `neighborhood_connectivity` | 0.28 |
| HybridRank | `degree_discount` | 0.27 |
| Hybrid (Pozzi) | `degree` | 0.00 |

## References

Shvydun, S. (2025). *Zoo of centralities: Encyclopedia of node metrics
in complex networks*. arXiv:2511.05122.
<https://arxiv.org/abs/2511.05122>
