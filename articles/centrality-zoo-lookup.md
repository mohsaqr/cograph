# cograph and the Centrality Zoo

The number of proposed node centralities has grown far faster than the
number of distinct ideas behind them. Many new measures recombine a
small set of ingredients, such as degree, shortest-path distance, walk
counts, the leading eigenvector, the k-shell decomposition and local
clustering. A practitioner who meets an unfamiliar measure needs to know
whether it is available and, more importantly, whether it adds
information to simpler measures.

`cograph` offers the largest collection of node centralities of any
package compared in this article, in R or in Python. Its 191 measures
reach 166 of the 349 measures catalogued in the Centrality Zoo (Shvydun,
2025). Nine established centrality packages, brainGraph, centiserve,
CINNA, igraph, influenceR, keyplayer, NetworkX, sna and tidygraph, reach
60 between them when every function they offer is pooled (Table 1). The
collection is also growing quickly: `cograph` 2.4.4, the release on CRAN
in July 2026, offered 89 measures, and the current version offers 191,
2.1 times as many. Every measure is computed by a single function,
[`centrality()`](https://sonsoles.me/cograph/reference/centrality.md),
which returns a tidy data frame.

| Package                 | Language | Centrality functions | Zoo measures reached |
|:------------------------|:---------|---------------------:|---------------------:|
| **cograph**             | R        |                  191 |                  166 |
| CINNA                   | R        |                   49 |                   38 |
| centiserve              | R        |                   33 |                   30 |
| NetworkX                | Python   |                   30 |                   24 |
| tidygraph               | R        |                   25 |                   19 |
| igraph                  | R        |                   18 |                   14 |
| sna                     | R        |                   11 |                    8 |
| influenceR              | R        |                    4 |                    6 |
| brainGraph              | R        |                    7 |                    4 |
| keyplayer               | R        |                    4 |                    3 |
| All nine others, pooled |          |                  181 |                   60 |

Table 1. Node centralities in cograph and in nine other packages. A
function counts towards the Zoo only if the measure it computes is
catalogued there; two functions computing the same measure count once.
{.table}

## Zoo measures available in cograph

| Zoo measure | cograph measure |
|:---|:---|
| Access information | `access_information` |
| Adaptive LeaderRank | `adaptive_leaderrank` |
| beta-measure | `beta_measure` |
| Betweenness | `betweenness` |
| BG-index | `beta_measure` |
| Borgatti’s effective size | `effective_size` |
| BottleNeck | `bottleneck` |
| Bridging capital | `bridging_capital` |
| Bridging centrality | `bridging` |
| Bridging coefficient | `bridging_coefficient` |
| Burt’s constraint | `constraint` |
| Centroid | `centroid` |
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
| ControlRank | `controlrank` |
| Cross-Clique Connectivity | `cross_clique` |
| Current-flow betweenness | `current_flow_betweenness` |
| Current-flow Closeness | `current_flow_closeness` |
| Decay | `decay` |
| Degree | `degree` |
| Degree and Importance of Lines (DIL) | `dil` |
| DegreeDiscountIC | `degree_discount` |
| delta-betweenness | `delta_betweenness` |
| delta-closeness | `delta_closeness` |
| Diffusion centrality | `diffusion_centrality` |
| Diffusion Degree | `diffusion` |
| Distance entropy | `distance_entropy` |
| Distance-weighted fragmentation | `fragmentation` |
| Diversity coefficient | `diversity` |
| DK-based gravity model (DKGM) | `dkgm` |
| DMNC | `dmnc` |
| Dynamical importance | `dynamical_importance` |
| Dynamics-sensitive (DS) centrality | `dynamics_sensitive` |
| Eccentricity | `eccentricity` |
| Effective size | `effective_size` |
| Egocentric betweenness | `ego_betweenness` |
| Eigenvector | `eigenvector` |
| EnRenew | `enrenew` |
| Entropy | `entropy` |
| Entropy variation (betweenness) | `entropy_variation_betweenness` |
| Entropy variation (degree) | `entropy_variation_degree` |
| Exogenous centrality | `exogenous` |
| Expected force (ExF) | `expected_force` |
| Expected force (ExFm) | `modified_expected_force` |
| Extended gravity centrality | `extended_gravity` |
| Extended hybrid characteristic centrality (EHCC) | `ehcc` |
| Extended LBC | `extended_local_bridging` |
| Extended mixed gravitational centrality | `extended_mixed_gravity` |
| Extended neighborhood coreness | `extended_coreness` |
| Flow betweenness | `flow_betweenness` |
| Flow coefficient | `flow_coefficient` |
| Fuzzy local dimension (FLD) | `fuzzy_local_dimension` |
| Gateway coefficient | `gateway` |
| Geodesic k-path | `geodesic_kpath` |
| Gil-Schmidt Power Index | `gilschmidt` |
| Global Structure Model (GSM) | `global_structure` |
| Godfather index | `godfather` |
| Graph regularization centrality (GRC) | `graph_regularization` |
| Gravity centrality | `gravity` |
| Gravity model | `gravity` |
| h-index strength | `hindex_strength` |
| Harmonic | `harmonic` |
| Heatmap centrality | `heatmap` |
| Hide information | `hide_information` |
| Hubbel | `hubbell` |
| Hybrid Characteristic Centrality (HCC) | `hcc` |
| Hybrid Global Structure Model (H-GSM) | `hybrid_global_structure` |
| Immediate Effects Centrality (IEC) | `iec` |
| Improved closeness centrality (ICC) | `improved_closeness` |
| Improved global structure model (IGSM) | `improved_global_structure` |
| Improved IMC | `node_contraction_improved` |
| Improved iterative resource allocation (IIRA) | `iira` |
| Infection number | `infection` |
| Integration | `integration` |
| Intra-module degree | `within_module_z` |
| Iterative resource allocation (IRA) | `ira` |
| k-betweenness | `betweenness` |
| k-shell | `coreness` |
| k-truss number | `truss` |
| Katz | `katz` |
| KED | `ked` |
| Laplacian | `laplacian` |
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
| Local volume dimension (LVD) | `local_volume_dimension` |
| Localized bridging centrality | `localized_bridging` |
| LocalRank | `semilocal` |
| m-reach | `kreach` |
| Malatya centrality | `malatya` |
| Map Equation Centrality (MEC) | `map_equation` |
| Markov | `markov` |
| MCC | `mcc` |
| Mixed Degree Decomposition (MDD) | `mdd` |
| Mixed gravitational centrality | `mixed_gravity` |
| MNC | `mnc` |
| Modularity vitality | `modularity_vitality` |
| Multi-characteristics gravity model (MCGM) | `mcgm` |
| NCVoteRank | `ncvoterank` |
| Neighbor distance centrality | `neighbor_distance` |
| Neighborhood centrality | `neighbor_distance` |
| Neighborhood connectivity | `neighborhood_connectivity` |
| Node and Neighbor Layer Information (NINL) centrality | `ninl` |
| Node contraction (IMC) | `node_contraction` |
| Non-backtracking centrality | `nonbacktracking` |
| PageRank | `pagerank` |
| Pairwise disconnectivity | `pairwisedis` |
| Participation coefficient | `participation` |
| Percolation | `percolation` |
| Proximal betwenness | `proximal_betweenness` |
| Random walk centrality | `random_walk` |
| Random walk decay | `random_walk_decay` |
| Randomized shortest paths (RSP) betweenness | `rsp_betweenness` |
| Redundancy | `redundancy` |
| Relative entropy | `relative_entropy` |
| Renewed coreness | `renewed_coreness` |
| Residual closeness | `residual_closeness` |
| Resistance curvature | `resistance_curvature` |
| Rumor centrality | `rumor` |
| s-shell index | `s_shell` |
| SALSA | `salsa` |
| Second order centrality | `second_order` |
| Semi-local ranking (SLC) | `semilocal` |
| Shapley value (game 1) | `shapley_game1` |
| Shapley value (game 2) | `shapley_game2` |
| Shapley value (game 3) | `shapley_game3` |
| SingleDiscount | `single_discount` |
| Spanning tree centrality (STC) | `spanning_tree` |
| SpectralRank | `spectralrank` |
| Stress | `stress` |
| Subgraph | `subgraph` |
| Support | `support` |
| Topological | `topological_coefficient` |
| Total communicability | `communicability` |
| Trust-PageRank | `trust_pagerank` |
| Two-way random walk betweenness (2RW) | `two_way_rw` |
| Volume centrality | `volume` |
| VoteRank | `voterank` |
| VoteRank++ | `voterank_plus` |
| Weighted h-index | `weighted_h_index` |
| Weighted k-shell decomposition (Wks) | `weighted_kshell` |
| Weighted LeaderRank | `weighted_leaderrank` |
| WVoteRank | `wvoterank` |
| X-degree centrality | `x_degree` |

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
| Improved neighbors’ k-core (INK) | `local_hindex` | 1.00 |
| INF centrality | `beta_measure` | 1.00 |
| Linearly scaled betweenness | `betweenness` | 1.00 |
| Mixed core, semi-local degree and entropy (MCSDE) | `semilocal` | 1.00 |
| Mixed core, semi-local degree and weighted entropy (MCSDWE) | `semilocal` | 1.00 |
| Modified Local Centrality (MLC) | `semilocal` | 1.00 |
| Nieminen’s closeness | `closeness` | 1.00 |
| PhysarumSpreader | `beta_measure` | 1.00 |
| QJSD centrality | `degree` | 1.00 |
| Routing betweenness | `load` | 1.00 |
| Total centrality | `fragmentation` | 1.00 |
| Total Effects Centrality (TEC) | `degree` | 1.00 |
| ViralRank | `current_flow_closeness` | 1.00 |
| Zeta vector centrality | `second_order` | 1.00 |
| Hybrid degree centrality | `hubbell` | 0.99 |
| k-path | `diffusion` | 0.99 |
| Average shortest path centrality (AC) | `closeness_vitality` | 0.98 |
| Extended Cluster Coefficient Ranking Measure (ECRM) | `ninl` | 0.98 |
| Extended H-index centrality (EHC) | `ninl` | 0.98 |
| Extended improved k-shell hybrid (EIKH) | `infection` | 0.98 |
| Extended k-shell hybrid method | `infection` | 0.98 |
| Extended RMD-weighted degree (EWD) | `extended_coreness` | 0.98 |
| Global importance of nodes (GIN) | `improved_global_structure` | 0.98 |
| Mapping entropy (ME) | `hubbell` | 0.98 |
| Neighborhood core diversity centrality (Cncd) | `extended_coreness` | 0.98 |
| Weighted gravity model (WGravity) | `diffusion_centrality` | 0.98 |
| Weighted k-shell degree neighborhood (Maji) | `linerank` | 0.98 |
| Entropy-Based Ranking Measure (ERM) | `ninl` | 0.97 |
| Extended weight degree centrality (EWdc) | `communicability` | 0.97 |
| Improved entropy-based centrality | `ked` | 0.97 |
| Improved k-shell hybrid (IKH) | `neighbor_distance` | 0.97 |
| Influence capability (IC) | `ninl` | 0.97 |
| k-shell hybrid method | `neighbor_distance` | 0.97 |
| Laplacian gravity centrality (LGC) | `diffusion` | 0.97 |
| Mixed core, degree and entropy (MCDE) | `degree` | 0.97 |
| Mixed core, degree and weighted entropy (MCDWE) | `mdd` | 0.97 |
| Node importance evaluation matrix (NIEM) method | `semilocal` | 0.97 |
| Shell clustering coefficient | `modified_expected_force` | 0.97 |
| Weight degree centrality (Wdc) | `diffusion` | 0.97 |
| Weight neighborhood centrality | `dkgm` | 0.97 |
| Weighted formal concept analysis (WFCA) | `mcc` | 0.97 |
| Classified neighbors centrality | `mdd` | 0.96 |
| CON score | `semilocal` | 0.96 |
| Counting Betweenness | `ked` | 0.96 |
| Density centrality | `gravity` | 0.96 |
| Global and Local Structure(GLS) | `linerank` | 0.96 |
| Icentr | `improved_global_structure` | 0.96 |
| Mapping Entropy Betweenness (MEB) | `relative_entropy` | 0.96 |
| RMD-weighted degree (WD) | `local_hindex` | 0.96 |
| VMM algorithm | `degree` | 0.96 |
| Weighted k-shell degree neighborhood (Wksd) | `gravity` | 0.96 |
| wkpath | `infection` | 0.96 |
| All-around score | `mdd` | 0.95 |
| ArticleRank | `pagerank` | 0.95 |
| Entropy and mutual information-based centrality (EMI) | `coleman_theil` | 0.95 |
| Extended diversity-strength ranking (EDSR) | `ninl` | 0.95 |
| Global and local information (GLI) method | `improved_global_structure` | 0.95 |
| Hierarchical k-shell (HKS) | `eigenvector` | 0.95 |
| k-shell based on gravity centrality (KSGC) | `hubbell` | 0.95 |
| k-shell iteration factor(KS-IF) | `extended_coreness` | 0.95 |
| KDEC method | `mixed_gravity` | 0.95 |
| Local structural centrality (LSC) | `iira` | 0.95 |
| Mediative Effects Centrality (MEC) | `communicability_betweenness` | 0.95 |
| Multi-evidence centrality (MeC) | `communicability_betweenness` | 0.95 |
| NL centrality | `iira` | 0.95 |
| Node importance contribution correlation matrix (NICCM) method | `iira` | 0.95 |
| ProfitLeader | `support` | 0.95 |
| TOPSIS-RE | `relative_entropy` | 0.95 |
| Correlation centrality | `infection` | 0.94 |
| Diversity-strength ranking (DSR) | `iira` | 0.94 |
| Improved K-shell decomposition (IKSD) | `lobby` | 0.94 |
| Normalized local centrality (NLC) | `iira` | 0.94 |
| Random walk-based gravity (DFS-Gravity) | `laplacian` | 0.94 |
| Weighted TOPSIS | `harmonic` | 0.94 |
| X-nonbacktracking centrality | `iira` | 0.94 |
| μ-Power Community Index (μ-PCI) | `lobby` | 0.94 |
| Analytic Hierarchy Process (AHP) centrality | `harmonic` | 0.93 |
| Curvature index | `dil` | 0.93 |
| Degree and clustering coefficient (DCC) | `diffusion` | 0.93 |
| DST | `resistance_curvature` | 0.93 |
| Edge-disjoint k-path | `iira` | 0.93 |
| Eigentrust | `degree` | 0.93 |
| Entropy-based gravity model | `communicability_betweenness` | 0.93 |
| LRIC (PPR) | `degree` | 0.93 |
| Meta-centrality | `iira` | 0.93 |
| Seeley’s index | `degree` | 0.93 |
| Semi-local iterative algorithm (semi-IA) | `improved_closeness` | 0.93 |
| Spreading probability (SP) | `infection` | 0.93 |
| All cycle betweenness (ACC) | `cross_clique` | 0.92 |
| Effective gravity model (EGM) | `gravity` | 0.92 |
| M-centrality | `iira` | 0.92 |
| Path-transfer centrality | `controlrank` | 0.92 |
| RCFB centrality | `current_flow_betweenness` | 0.92 |
| Vertex-disjoint k-path | `iira` | 0.92 |
| Diversity-strength centrality (DSC) | `degree` | 0.91 |
| Hybrid centrality (HC) | `harmonic` | 0.91 |
| Integral k-shell | `neighbor_distance` | 0.91 |
| Inward accessibility | `trust_pagerank` | 0.91 |
| New evidential centrality (NEC) | `degree` | 0.91 |
| NWRank | `communicability_betweenness` | 0.91 |
| Probabilistic-jumping Random Walk (PJRW) | `degree` | 0.91 |
| Similarity-based PageRank | `malatya` | 0.91 |
| Synthesize centrality (SC) | `degree` | 0.91 |
| TOPSIS | `fragmentation` | 0.91 |
| Transportation centrality | `rsp_betweenness` | 0.91 |
| WRank | `semilocal` | 0.91 |
| Biased random walk centrality | `degree` | 0.90 |
| Cc-Burt | `iira` | 0.90 |
| Edge clustering coefficient centrality (NC) | `cross_clique` | 0.90 |
| LRIC (max) | `degree` | 0.90 |
| PathRank | `nonbacktracking` | 0.90 |
| theta-centrality | `gravity` | 0.90 |
| Isolating Centrality (ISC) | `pairwisedis` | 0.89 |
| MABIE | `dil` | 0.89 |
| Multiple local attributes weighted centrality (LWC) | `support` | 0.89 |
| Contribution centrality | `improved_closeness` | 0.88 |
| Graph Fourier Transform Centrality (GFT-C) | `mdd` | 0.88 |
| Local degree dimension (LDD) | `iira` | 0.88 |
| Spreading strength | `geodesic_kpath` | 0.88 |
| Weighted community betweenness | `bridging` | 0.88 |
| Entropy-based influence disseminator (EbID) | `degree` | 0.87 |
| Hierarchical reduction by betweenness | `godfather` | 0.87 |
| Interdependence | `degree` | 0.87 |
| Node local centrality (NLC) | `mcc` | 0.87 |
| Generalized gravity centrality (GGC) | `gravity` | 0.86 |
| Random walk accessibility (RWA) | `improved_closeness` | 0.86 |
| Shortest cycle closeness (SCC) | `improved_closeness` | 0.86 |
| Outward accessibility | `improved_closeness` | 0.85 |
| Clustered local-degree (CLD) | `hcc` | 0.84 |
| Effective distance gravity (EffG) | `improved_global_structure` | 0.84 |
| Return Random Walk Gravity (RRWG) | `iira` | 0.84 |
| Graphlet degree centrality (GDC) | `gravity` | 0.82 |
| Mutual information | `shapley_game1` | 0.82 |
| Relative local–global importance (RLGI) | `malatya` | 0.82 |
| SRIC | `leverage` | 0.82 |
| Gromov centrality | `degree` | 0.81 |
| Node importance contribution matrix (NICM) method | `closeness_vitality` | 0.81 |
| EPC | `random_walk` | 0.80 |
| Multi-local dimension (MLD) | `fuzzy_local_dimension` | 0.80 |
| Neighborhood density | `support` | 0.78 |
| Weighted volume centrality | `geodesic_kpath` | 0.78 |
| Expected rank | `resistance_curvature` | 0.77 |
| BridgeRank | `closeness` | 0.76 |
| Semi-local degree and clustering coefficient | `kreach` | 0.76 |
| Algebraic centrality | `closeness_vitality` | 0.74 |
| Shapley Value based Information Delimiters (SVID) | `degree` | 0.74 |
| DSHC method | `voterank_plus` | 0.73 |
| E-Burt | `constraint` | 0.71 |
| LRIC (maxmin) | `dil` | 0.70 |
| Multi-criteria influence maximization (MCIM) | `iira` | 0.68 |
| Improved WVoteRank | `proximal_betweenness` | 0.67 |
| LRIC-sim | `voterank_plus` | 0.67 |
| Community centrality | `ego_betweenness` | 0.66 |
| Absorbing Random-Walk (ARW) | `single_discount` | 0.65 |
| Partition-Based Spreaders Identification (PBSI) | `degree` | 0.64 |
| Information distance index | `degree` | 0.63 |
| Game centrality (GC) | `local_dimension` | 0.61 |
| Two-step framework (IF) | `effective_size` | 0.60 |
| Fractional Graph Fourier Transform (FrGFTC) | `degree` | 0.59 |
| Degree and Clustering coefficient and Location (DCL) | `clusterrank` | 0.58 |
| Local RASP | `load` | 0.58 |
| Truncated curvature | `iira` | 0.57 |
| DegreePunishment | `constraint` | 0.55 |
| IS method | `dmnc` | 0.54 |
| Link influence entropy (LInE) | `flow_coefficient` | 0.53 |
| K-shell Physarum centrality | `ego_betweenness` | 0.52 |
| Physarum centrality | `bottleneck` | 0.44 |
| Graph-theoretic power index (GPI) | `fuzzy_local_dimension` | 0.41 |
| Node information dimension (NID) | `local_dimension` | 0.41 |
| Improved coloring method (IIS) | `constraint` | 0.37 |
| Improved k-shell method (IKS) | `clusterrank` | 0.32 |
| DegreeDistance | `degree_discount` | 0.30 |
| Local fuzzy information centrality (LFIC) | `neighborhood_connectivity` | 0.28 |
| HybridRank | `degree_discount` | 0.27 |
| Hybrid (Pozzi) | `degree` | 0.00 |

## References

Shvydun, S. (2025). *Zoo of centralities: Encyclopedia of node metrics
in complex networks*. arXiv:2511.05122.
<https://arxiv.org/abs/2511.05122>
