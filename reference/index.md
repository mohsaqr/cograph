# Package index

## Base R plotting

- [`splot()`](http://sonsoles.me/cograph/reference/splot.md) : Base R
  Graphics Network Plotting
- [`sn_save()`](http://sonsoles.me/cograph/reference/sn_save.md) : Save
  Network Visualization

## Plotting based on `ggplot2`

- [`soplot()`](http://sonsoles.me/cograph/reference/soplot.md)
  [`sn_render()`](http://sonsoles.me/cograph/reference/soplot.md) : Plot
  Cograph Network
- [`sn_ggplot()`](http://sonsoles.me/cograph/reference/sn_ggplot.md) :
  Convert Network to ggplot2
- [`sn_save_ggplot()`](http://sonsoles.me/cograph/reference/sn_save_ggplot.md)
  : Save as ggplot2

## Transition Network Analysis

- [`from_tna()`](http://sonsoles.me/cograph/reference/from_tna.md) :
  Convert a tna object to cograph parameters
- [`plot_tna()`](http://sonsoles.me/cograph/reference/plot_tna.md)
  [`tplot()`](http://sonsoles.me/cograph/reference/plot_tna.md) :
  TNA-Style Network Plot (qgraph Compatible)
- [`plot_htna()`](http://sonsoles.me/cograph/reference/plot_htna.md)
  [`htna()`](http://sonsoles.me/cograph/reference/plot_htna.md) : Plot
  Heterogeneous TNA Network (Multi-Group Layout)

## Multi-Cluster TNA Network Plot

- [`plot_mtna()`](http://sonsoles.me/cograph/reference/plot_mtna.md)
  [`mtna()`](http://sonsoles.me/cograph/reference/plot_mtna.md) :
  Multi-Cluster TNA Network Plot

## Multilevel Network Visualization

- [`plot_mlna()`](http://sonsoles.me/cograph/reference/plot_mlna.md)
  [`mlna()`](http://sonsoles.me/cograph/reference/plot_mlna.md) :
  Multilevel Network Visualization

## Network functions

- [`cograph()`](http://sonsoles.me/cograph/reference/cograph.md) :
  Create a Network Visualization
- [`as_cograph()`](http://sonsoles.me/cograph/reference/as_cograph.md)
  [`to_cograph()`](http://sonsoles.me/cograph/reference/as_cograph.md) :
  Convert to Cograph Network
- [`n_nodes()`](http://sonsoles.me/cograph/reference/n_nodes.md) : Get
  Number of Nodes
- [`n_edges()`](http://sonsoles.me/cograph/reference/n_edges.md) : Get
  Number of Edges
- [`get_edges()`](http://sonsoles.me/cograph/reference/get_edges.md) :
  Get Edges from Cograph Network
- [`get_labels()`](http://sonsoles.me/cograph/reference/get_labels.md) :
  Get Labels from Cograph Network
- [`get_nodes()`](http://sonsoles.me/cograph/reference/get_nodes.md) :
  Get Nodes from Cograph Network
- [`set_edges()`](http://sonsoles.me/cograph/reference/set_edges.md) :
  Set Edges in Cograph Network
- [`set_nodes()`](http://sonsoles.me/cograph/reference/set_nodes.md) :
  Set Nodes in Cograph Network
- [`sn_edges()`](http://sonsoles.me/cograph/reference/sn_edges.md) : Set
  Edge Aesthetics
- [`sn_ggplot()`](http://sonsoles.me/cograph/reference/sn_ggplot.md) :
  Convert Network to ggplot2
- [`sn_nodes()`](http://sonsoles.me/cograph/reference/sn_nodes.md) : Set
  Node Aesthetics
- [`sn_save()`](http://sonsoles.me/cograph/reference/sn_save.md) : Save
  Network Visualization
- [`aes-nodes`](http://sonsoles.me/cograph/reference/aes-nodes.md) :
  Node Aesthetics
- [`aes-edges`](http://sonsoles.me/cograph/reference/aes-edges.md) :
  Edge Aesthetics
- [`output-save`](http://sonsoles.me/cograph/reference/output-save.md) :
  Output and Saving
- [`from_qgraph()`](http://sonsoles.me/cograph/reference/from_qgraph.md)
  : Convert a qgraph object to cograph parameters

## Palettes

- [`list_palettes()`](http://sonsoles.me/cograph/reference/list_palettes.md)
  : List Available Color Palettes
- [`sn_palette()`](http://sonsoles.me/cograph/reference/sn_palette.md) :
  Apply Color Palette to Network
- [`palette_blues()`](http://sonsoles.me/cograph/reference/palette_blues.md)
  : Blues Palette
- [`palette_colorblind()`](http://sonsoles.me/cograph/reference/palette_colorblind.md)
  : Colorblind-friendly Palette
- [`palette_diverging()`](http://sonsoles.me/cograph/reference/palette_diverging.md)
  : Diverging Palette
- [`palette_pastel()`](http://sonsoles.me/cograph/reference/palette_pastel.md)
  : Pastel Palette
- [`palette_rainbow()`](http://sonsoles.me/cograph/reference/palette_rainbow.md)
  : Rainbow Palette
- [`palette_reds()`](http://sonsoles.me/cograph/reference/palette_reds.md)
  : Reds Palette
- [`palette_viridis()`](http://sonsoles.me/cograph/reference/palette_viridis.md)
  : Viridis Palette
- [`palettes`](http://sonsoles.me/cograph/reference/palettes.md) : Color
  Palettes

## Shapes

- [`get_shape()`](http://sonsoles.me/cograph/reference/get_shape.md) :
  Get a Registered Shape
- [`list_shapes()`](http://sonsoles.me/cograph/reference/list_shapes.md)
  : List Available Shapes
- [`list_svg_shapes()`](http://sonsoles.me/cograph/reference/list_svg_shapes.md)
  : List Registered SVG Shapes
- [`register_shape()`](http://sonsoles.me/cograph/reference/register_shape.md)
  : Register a Custom Shape
- [`register_svg_shape()`](http://sonsoles.me/cograph/reference/register_svg_shape.md)
  : Register Custom SVG Shape
- [`unregister_svg_shape()`](http://sonsoles.me/cograph/reference/unregister_svg_shape.md)
  : Unregister SVG Shape

## Themes

- [`get_theme()`](http://sonsoles.me/cograph/reference/get_theme.md) :
  Get a Registered Theme
- [`list_themes()`](http://sonsoles.me/cograph/reference/list_themes.md)
  : List Available Themes
- [`sn_theme()`](http://sonsoles.me/cograph/reference/sn_theme.md) :
  Apply Theme to Network
- [`register_theme()`](http://sonsoles.me/cograph/reference/register_theme.md)
  : Register a Custom Theme
- [`theme_cograph_classic()`](http://sonsoles.me/cograph/reference/theme_cograph_classic.md)
  : Classic Theme
- [`theme_cograph_colorblind()`](http://sonsoles.me/cograph/reference/theme_cograph_colorblind.md)
  : Colorblind-friendly Theme
- [`theme_cograph_dark()`](http://sonsoles.me/cograph/reference/theme_cograph_dark.md)
  : Dark Theme
- [`theme_cograph_gray()`](http://sonsoles.me/cograph/reference/theme_cograph_gray.md)
  : Grayscale Theme
- [`theme_cograph_minimal()`](http://sonsoles.me/cograph/reference/theme_cograph_minimal.md)
  : Minimal Theme
- [`theme_cograph_nature()`](http://sonsoles.me/cograph/reference/theme_cograph_nature.md)
  : Nature Theme
- [`theme_cograph_viridis()`](http://sonsoles.me/cograph/reference/theme_cograph_viridis.md)
  : Viridis Theme

## Layouts

- [`get_layout()`](http://sonsoles.me/cograph/reference/get_layout.md) :
  Get a Registered Layout
- [`set_layout()`](http://sonsoles.me/cograph/reference/set_layout.md) :
  Set Layout in Cograph Network
- [`sn_layout()`](http://sonsoles.me/cograph/reference/sn_layout.md) :
  Apply Layout to Network
- [`layout-circle`](http://sonsoles.me/cograph/reference/layout-circle.md)
  : Circular Layout
- [`layout-groups`](http://sonsoles.me/cograph/reference/layout-groups.md)
  : Group-based Layout
- [`layout-oval`](http://sonsoles.me/cograph/reference/layout-oval.md) :
  Oval/Ellipse Layout
- [`layout-spring`](http://sonsoles.me/cograph/reference/layout-spring.md)
  : Fruchterman-Reingold Spring Layout
- [`layout_circle()`](http://sonsoles.me/cograph/reference/layout_circle.md)
  : Circular Layout
- [`layout_groups()`](http://sonsoles.me/cograph/reference/layout_groups.md)
  : Group-based Layout
- [`layout_oval()`](http://sonsoles.me/cograph/reference/layout_oval.md)
  : Oval Layout
- [`layout_spring()`](http://sonsoles.me/cograph/reference/layout_spring.md)
  : Fruchterman-Reingold Spring Layout
- [`list_layouts()`](http://sonsoles.me/cograph/reference/list_layouts.md)
  : List Available Layouts
- [`register_layout()`](http://sonsoles.me/cograph/reference/register_layout.md)
  : Register a Custom Layout

## Motif Analysis

- [`motif_census()`](http://sonsoles.me/cograph/reference/motif_census.md)
  [`print(`*`<cograph_motifs>`*`)`](http://sonsoles.me/cograph/reference/motif_census.md)
  : Network Motif Analysis
- [`extract_motifs()`](http://sonsoles.me/cograph/reference/extract_motifs.md)
  [`print(`*`<cograph_motif_analysis>`*`)`](http://sonsoles.me/cograph/reference/extract_motifs.md)
  : Extract Motifs from Network Data
- [`extract_triads()`](http://sonsoles.me/cograph/reference/extract_triads.md)
  : Extract Triads with Node Labels
- [`triad_census()`](http://sonsoles.me/cograph/reference/triad_census.md)
  : Triad Census

## All Other Functions

- [`centrality()`](http://sonsoles.me/cograph/reference/centrality.md) :
  Calculate Network Centrality Measures
- [`centrality_alpha()`](http://sonsoles.me/cograph/reference/centrality_alpha.md)
  : Alpha (Katz) Centrality
- [`centrality_authority()`](http://sonsoles.me/cograph/reference/centrality_authority.md)
  [`centrality_hub()`](http://sonsoles.me/cograph/reference/centrality_authority.md)
  : HITS Authority and Hub Scores
- [`centrality_betweenness()`](http://sonsoles.me/cograph/reference/centrality_betweenness.md)
  : Betweenness Centrality
- [`centrality_closeness()`](http://sonsoles.me/cograph/reference/centrality_closeness.md)
  [`centrality_incloseness()`](http://sonsoles.me/cograph/reference/centrality_closeness.md)
  [`centrality_outcloseness()`](http://sonsoles.me/cograph/reference/centrality_closeness.md)
  : Closeness Centrality
- [`centrality_constraint()`](http://sonsoles.me/cograph/reference/centrality_constraint.md)
  : Burt's Constraint
- [`centrality_coreness()`](http://sonsoles.me/cograph/reference/centrality_coreness.md)
  : K-Core Decomposition (Coreness)
- [`centrality_current_flow_betweenness()`](http://sonsoles.me/cograph/reference/centrality_current_flow_betweenness.md)
  : Current Flow Betweenness Centrality
- [`centrality_current_flow_closeness()`](http://sonsoles.me/cograph/reference/centrality_current_flow_closeness.md)
  : Current Flow Closeness Centrality
- [`centrality_degree()`](http://sonsoles.me/cograph/reference/centrality_degree.md)
  [`centrality_indegree()`](http://sonsoles.me/cograph/reference/centrality_degree.md)
  [`centrality_outdegree()`](http://sonsoles.me/cograph/reference/centrality_degree.md)
  : Degree Centrality
- [`centrality_diffusion()`](http://sonsoles.me/cograph/reference/centrality_diffusion.md)
  : Diffusion Centrality
- [`centrality_eccentricity()`](http://sonsoles.me/cograph/reference/centrality_eccentricity.md)
  [`centrality_ineccentricity()`](http://sonsoles.me/cograph/reference/centrality_eccentricity.md)
  [`centrality_outeccentricity()`](http://sonsoles.me/cograph/reference/centrality_eccentricity.md)
  : Eccentricity
- [`centrality_eigenvector()`](http://sonsoles.me/cograph/reference/centrality_eigenvector.md)
  : Eigenvector Centrality
- [`centrality_harmonic()`](http://sonsoles.me/cograph/reference/centrality_harmonic.md)
  [`centrality_inharmonic()`](http://sonsoles.me/cograph/reference/centrality_harmonic.md)
  [`centrality_outharmonic()`](http://sonsoles.me/cograph/reference/centrality_harmonic.md)
  : Harmonic Centrality
- [`centrality_kreach()`](http://sonsoles.me/cograph/reference/centrality_kreach.md)
  : Geodesic K-Path Centrality
- [`centrality_laplacian()`](http://sonsoles.me/cograph/reference/centrality_laplacian.md)
  : Laplacian Centrality
- [`centrality_leverage()`](http://sonsoles.me/cograph/reference/centrality_leverage.md)
  : Leverage Centrality
- [`centrality_load()`](http://sonsoles.me/cograph/reference/centrality_load.md)
  : Load Centrality
- [`centrality_pagerank()`](http://sonsoles.me/cograph/reference/centrality_pagerank.md)
  : PageRank Centrality
- [`centrality_percolation()`](http://sonsoles.me/cograph/reference/centrality_percolation.md)
  : Percolation Centrality
- [`centrality_power()`](http://sonsoles.me/cograph/reference/centrality_power.md)
  : Bonacich Power Centrality
- [`centrality_strength()`](http://sonsoles.me/cograph/reference/centrality_strength.md)
  [`centrality_instrength()`](http://sonsoles.me/cograph/reference/centrality_strength.md)
  [`centrality_outstrength()`](http://sonsoles.me/cograph/reference/centrality_strength.md)
  : Strength Centrality (Weighted Degree)
- [`centrality_subgraph()`](http://sonsoles.me/cograph/reference/centrality_subgraph.md)
  : Subgraph Centrality
- [`centrality_transitivity()`](http://sonsoles.me/cograph/reference/centrality_transitivity.md)
  : Local Transitivity (Clustering Coefficient)
- [`centrality_voterank()`](http://sonsoles.me/cograph/reference/centrality_voterank.md)
  : VoteRank Centrality
- [`community_consensus()`](http://sonsoles.me/cograph/reference/community_consensus.md)
  [`com_consensus()`](http://sonsoles.me/cograph/reference/community_consensus.md)
  : Consensus Community Detection
- [`community_edge_betweenness()`](http://sonsoles.me/cograph/reference/community_edge_betweenness.md)
  [`com_eb()`](http://sonsoles.me/cograph/reference/community_edge_betweenness.md)
  : Edge Betweenness Community Detection
- [`community_fast_greedy()`](http://sonsoles.me/cograph/reference/community_fast_greedy.md)
  [`com_fg()`](http://sonsoles.me/cograph/reference/community_fast_greedy.md)
  : Fast Greedy Community Detection
- [`community_fluid()`](http://sonsoles.me/cograph/reference/community_fluid.md)
  [`com_fl()`](http://sonsoles.me/cograph/reference/community_fluid.md)
  : Fluid Communities Detection
- [`community_infomap()`](http://sonsoles.me/cograph/reference/community_infomap.md)
  [`com_im()`](http://sonsoles.me/cograph/reference/community_infomap.md)
  : Infomap Community Detection
- [`community_label_propagation()`](http://sonsoles.me/cograph/reference/community_label_propagation.md)
  [`com_lp()`](http://sonsoles.me/cograph/reference/community_label_propagation.md)
  : Label Propagation Community Detection
- [`community_leading_eigenvector()`](http://sonsoles.me/cograph/reference/community_leading_eigenvector.md)
  [`com_le()`](http://sonsoles.me/cograph/reference/community_leading_eigenvector.md)
  : Leading Eigenvector Community Detection
- [`community_leiden()`](http://sonsoles.me/cograph/reference/community_leiden.md)
  [`com_ld()`](http://sonsoles.me/cograph/reference/community_leiden.md)
  : Leiden Community Detection
- [`community_louvain()`](http://sonsoles.me/cograph/reference/community_louvain.md)
  [`com_lv()`](http://sonsoles.me/cograph/reference/community_louvain.md)
  : Louvain Community Detection
- [`community_optimal()`](http://sonsoles.me/cograph/reference/community_optimal.md)
  [`com_op()`](http://sonsoles.me/cograph/reference/community_optimal.md)
  : Optimal Community Detection
- [`community_sizes()`](http://sonsoles.me/cograph/reference/community_sizes.md)
  : Get Community Sizes
- [`community_spinglass()`](http://sonsoles.me/cograph/reference/community_spinglass.md)
  [`com_sg()`](http://sonsoles.me/cograph/reference/community_spinglass.md)
  : Spinglass Community Detection
- [`community_walktrap()`](http://sonsoles.me/cograph/reference/community_walktrap.md)
  [`com_wt()`](http://sonsoles.me/cograph/reference/community_walktrap.md)
  : Walktrap Community Detection
- [`select_bridges()`](http://sonsoles.me/cograph/reference/select_bridges.md)
  : Select Bridge Edges
- [`select_component()`](http://sonsoles.me/cograph/reference/select_component.md)
  : Select Connected Component
- [`select_edges()`](http://sonsoles.me/cograph/reference/select_edges.md)
  : Select Edges with Lazy Computation
- [`select_edges_between()`](http://sonsoles.me/cograph/reference/select_edges_between.md)
  : Select Edges Between Node Sets
- [`select_edges_involving()`](http://sonsoles.me/cograph/reference/select_edges_involving.md)
  : Select Edges Involving Nodes
- [`select_neighbors()`](http://sonsoles.me/cograph/reference/select_neighbors.md)
  : Select Node Neighbors (Ego Network)
- [`select_nodes()`](http://sonsoles.me/cograph/reference/select_nodes.md)
  : Select Nodes with Lazy Centrality Computation
- [`select_top()`](http://sonsoles.me/cograph/reference/select_top.md) :
  Select Top N Nodes by Centrality
- [`select_top_edges()`](http://sonsoles.me/cograph/reference/select_top_edges.md)
  : Select Top N Edges
- [`filter_edges()`](http://sonsoles.me/cograph/reference/filter_edges.md)
  [`subset_edges()`](http://sonsoles.me/cograph/reference/filter_edges.md)
  : Filter Edges by Metadata
- [`filter_nodes()`](http://sonsoles.me/cograph/reference/filter_nodes.md)
  [`subset_nodes()`](http://sonsoles.me/cograph/reference/filter_nodes.md)
  : Filter Nodes by Metadata or Centrality
- [`network_bridges()`](http://sonsoles.me/cograph/reference/network_bridges.md)
  : Bridge Edges
- [`network_clique_size()`](http://sonsoles.me/cograph/reference/network_clique_size.md)
  : Largest Clique Size
- [`network_cut_vertices()`](http://sonsoles.me/cograph/reference/network_cut_vertices.md)
  : Cut Vertices (Articulation Points)
- [`network_girth()`](http://sonsoles.me/cograph/reference/network_girth.md)
  : Network Girth (Shortest Cycle Length)
- [`network_global_efficiency()`](http://sonsoles.me/cograph/reference/network_global_efficiency.md)
  : Global Efficiency
- [`network_local_efficiency()`](http://sonsoles.me/cograph/reference/network_local_efficiency.md)
  : Local Efficiency
- [`network_radius()`](http://sonsoles.me/cograph/reference/network_radius.md)
  : Network Radius
- [`network_rich_club()`](http://sonsoles.me/cograph/reference/network_rich_club.md)
  : Rich Club Coefficient
- [`network_small_world()`](http://sonsoles.me/cograph/reference/network_small_world.md)
  : Small-World Coefficient (Sigma)
- [`network_summary()`](http://sonsoles.me/cograph/reference/network_summary.md)
  : Network-Level Summary Statistics
- [`network_vertex_connectivity()`](http://sonsoles.me/cograph/reference/network_vertex_connectivity.md)
  : Network Vertex Connectivity
- [`as_cograph()`](http://sonsoles.me/cograph/reference/as_cograph.md)
  [`to_cograph()`](http://sonsoles.me/cograph/reference/as_cograph.md) :
  Convert to Cograph Network
- [`to_data_frame()`](http://sonsoles.me/cograph/reference/to_data_frame.md)
  [`to_df()`](http://sonsoles.me/cograph/reference/to_data_frame.md) :
  Export Network as Edge List Data Frame
- [`to_igraph()`](http://sonsoles.me/cograph/reference/to_igraph.md) :
  Convert Network to igraph Object
- [`to_matrix()`](http://sonsoles.me/cograph/reference/to_matrix.md) :
  Convert Network to Adjacency Matrix
- [`to_network()`](http://sonsoles.me/cograph/reference/to_network.md) :
  Convert Network to statnet network Object
- [`plot_alluvial()`](http://sonsoles.me/cograph/reference/plot_alluvial.md)
  : Plot Alluvial Diagram
- [`plot_chord()`](http://sonsoles.me/cograph/reference/plot_chord.md) :
  Chord Diagram
- [`plot_compare()`](http://sonsoles.me/cograph/reference/plot_compare.md)
  : Plot Network Difference
- [`plot_comparison_heatmap()`](http://sonsoles.me/cograph/reference/plot_comparison_heatmap.md)
  : Plot Comparison Heatmap
- [`plot_edge_diff_forest()`](http://sonsoles.me/cograph/reference/plot_edge_diff_forest.md)
  : Forest Plot for Bootstrap Edge Differences
- [`splot.group_tna_permutation()`](http://sonsoles.me/cograph/reference/plot_group_permutation.md)
  [`plot_group_permutation()`](http://sonsoles.me/cograph/reference/plot_group_permutation.md)
  : Plot Group Permutation Test Results
- [`plot_heatmap()`](http://sonsoles.me/cograph/reference/plot_heatmap.md)
  : Plot Network as Heatmap
- [`plot_htna()`](http://sonsoles.me/cograph/reference/plot_htna.md)
  [`htna()`](http://sonsoles.me/cograph/reference/plot_htna.md) : Plot
  Heterogeneous TNA Network (Multi-Group Layout)
- [`plot_mcml()`](http://sonsoles.me/cograph/reference/plot_mcml.md) :
  Plot Multi-Cluster Multi-Layer Network
- [`plot_mixed_network()`](http://sonsoles.me/cograph/reference/plot_mixed_network.md)
  : Plot Mixed Network
- [`plot_ml_heatmap()`](http://sonsoles.me/cograph/reference/plot_ml_heatmap.md)
  : Multilayer Network Heatmap
- [`plot_mlna()`](http://sonsoles.me/cograph/reference/plot_mlna.md)
  [`mlna()`](http://sonsoles.me/cograph/reference/plot_mlna.md) :
  Multilevel Network Visualization
- [`plot_mtna()`](http://sonsoles.me/cograph/reference/plot_mtna.md)
  [`mtna()`](http://sonsoles.me/cograph/reference/plot_mtna.md) :
  Multi-Cluster TNA Network Plot
- [`plot_netobject_group()`](http://sonsoles.me/cograph/reference/plot_netobject_group.md)
  [`plot(`*`<netobject_group>`*`)`](http://sonsoles.me/cograph/reference/plot_netobject_group.md)
  : Plot a Group of Nestimate netobjects
- [`plot_netobject_ml()`](http://sonsoles.me/cograph/reference/plot_netobject_ml.md)
  [`plot(`*`<netobject_ml>`*`)`](http://sonsoles.me/cograph/reference/plot_netobject_ml.md)
  : Plot a Multilevel Nestimate netobject
- [`splot.tna_permutation()`](http://sonsoles.me/cograph/reference/plot_permutation.md)
  [`plot_permutation()`](http://sonsoles.me/cograph/reference/plot_permutation.md)
  : Plot Permutation Test Results
- [`plot_robustness()`](http://sonsoles.me/cograph/reference/plot_robustness.md)
  : Plot Network Robustness
- [`plot_simplicial()`](http://sonsoles.me/cograph/reference/plot_simplicial.md)
  : Simplicial Complex Visualization
- [`plot_tna()`](http://sonsoles.me/cograph/reference/plot_tna.md)
  [`tplot()`](http://sonsoles.me/cograph/reference/plot_tna.md) :
  TNA-Style Network Plot (qgraph Compatible)
- [`plot_trajectories()`](http://sonsoles.me/cograph/reference/plot_trajectories.md)
  : Plot Individual Trajectories
- [`plot_transitions()`](http://sonsoles.me/cograph/reference/plot_transitions.md)
  : Plot Transitions Between States
- [`CographLayout`](http://sonsoles.me/cograph/reference/CographLayout.md)
  : CographLayout R6 Class
- [`CographNetwork`](http://sonsoles.me/cograph/reference/CographNetwork.md)
  : CographNetwork R6 Class
- [`CographTheme`](http://sonsoles.me/cograph/reference/CographTheme.md)
  : CographTheme R6 Class
- [`aggregate_layers()`](http://sonsoles.me/cograph/reference/aggregate_layers.md)
  [`lagg()`](http://sonsoles.me/cograph/reference/aggregate_layers.md) :
  Aggregate Layers
- [`aggregate_weights()`](http://sonsoles.me/cograph/reference/aggregate_weights.md)
  [`wagg()`](http://sonsoles.me/cograph/reference/aggregate_weights.md)
  : Aggregate Edge Weights
- [`cluster_quality()`](http://sonsoles.me/cograph/reference/cluster_quality.md)
  [`cqual()`](http://sonsoles.me/cograph/reference/cluster_quality.md) :
  Cluster Quality Metrics
- [`cluster_summary()`](http://sonsoles.me/cograph/reference/cluster_summary.md)
  [`csum()`](http://sonsoles.me/cograph/reference/cluster_summary.md) :
  Cluster Summary Statistics
- [`cograph-main`](http://sonsoles.me/cograph/reference/cograph-main.md)
  : Main Entry Point
- [`cograph-package`](http://sonsoles.me/cograph/reference/cograph-package.md)
  : cograph: Modern Network Visualization for R
- [`color_communities()`](http://sonsoles.me/cograph/reference/color_communities.md)
  : Color Nodes by Community
- [`communities()`](http://sonsoles.me/cograph/reference/communities.md)
  : Community Detection
- [`compare_communities()`](http://sonsoles.me/cograph/reference/compare_communities.md)
  : Compare Community Structures
- [`degree_distribution()`](http://sonsoles.me/cograph/reference/degree_distribution.md)
  : Degree Distribution Visualization
- [`detect_communities()`](http://sonsoles.me/cograph/reference/detect_communities.md)
  : Detect Communities in a Network
- [`edge_centrality()`](http://sonsoles.me/cograph/reference/edge_centrality.md)
  [`edge_betweenness()`](http://sonsoles.me/cograph/reference/edge_centrality.md)
  : Calculate Edge Centrality Measures
- [`get_edge_list()`](http://sonsoles.me/cograph/reference/get_edge_list.md)
  : Extract Raw Edge List from TNA Model
- [`get_groups()`](http://sonsoles.me/cograph/reference/get_groups.md) :
  Get Node Groups from Cograph Network
- [`get_shape()`](http://sonsoles.me/cograph/reference/get_shape.md) :
  Get a Registered Shape
- [`ggplot_robustness()`](http://sonsoles.me/cograph/reference/ggplot_robustness.md)
  : Compare Network Robustness (ggplot2)
- [`is_directed()`](http://sonsoles.me/cograph/reference/is_directed.md)
  : Check if Network is Directed
- [`is_tna_network()`](http://sonsoles.me/cograph/reference/is_tna_network.md)
  : Check if Network is TNA-based
- [`membership.cograph_communities()`](http://sonsoles.me/cograph/reference/membership.cograph_communities.md)
  : Get Community Membership
- [`modularity.cograph_communities()`](http://sonsoles.me/cograph/reference/modularity.cograph_communities.md)
  : Get Modularity Score
- [`mcml()`](http://sonsoles.me/cograph/reference/mcml.md)
  **\[deprecated\]** : mcml - Deprecated alias for cluster_summary
- [`n_communities()`](http://sonsoles.me/cograph/reference/n_communities.md)
  : Get Number of Communities
- [`nodes()`](http://sonsoles.me/cograph/reference/nodes.md) : Get Nodes
  from Cograph Network (Deprecated)
- [`robustness()`](http://sonsoles.me/cograph/reference/robustness.md) :
  Network Robustness Analysis
- [`robustness_auc()`](http://sonsoles.me/cograph/reference/robustness_auc.md)
  : Calculate Area Under Robustness Curve (AUC)
- [`robustness_summary()`](http://sonsoles.me/cograph/reference/robustness_summary.md)
  : Summary of Robustness Analysis
- [`set_groups()`](http://sonsoles.me/cograph/reference/set_groups.md) :
  Set Node Groups
- [`supra_adjacency()`](http://sonsoles.me/cograph/reference/supra_adjacency.md)
  [`supra()`](http://sonsoles.me/cograph/reference/supra_adjacency.md) :
  Supra-Adjacency Matrix
- [`supra_interlayer()`](http://sonsoles.me/cograph/reference/supra_interlayer.md)
  [`extract_interlayer()`](http://sonsoles.me/cograph/reference/supra_interlayer.md)
  : Extract Inter-Layer Block
- [`supra_layer()`](http://sonsoles.me/cograph/reference/supra_layer.md)
  [`extract_layer()`](http://sonsoles.me/cograph/reference/supra_layer.md)
  : Extract Layer from Supra-Adjacency Matrix
- [`verify_with_igraph()`](http://sonsoles.me/cograph/reference/verify_with_igraph.md)
  [`verify_igraph()`](http://sonsoles.me/cograph/reference/verify_with_igraph.md)
  : Verify Against igraph
- [`layer_degree_correlation()`](http://sonsoles.me/cograph/reference/layer_degree_correlation.md)
  [`ldegcor()`](http://sonsoles.me/cograph/reference/layer_degree_correlation.md)
  : Degree Correlation Between Layers
- [`layer_similarity()`](http://sonsoles.me/cograph/reference/layer_similarity.md)
  [`lsim()`](http://sonsoles.me/cograph/reference/layer_similarity.md) :
  Layer Similarity
- [`layer_similarity_matrix()`](http://sonsoles.me/cograph/reference/layer_similarity_matrix.md)
  [`lsim_matrix()`](http://sonsoles.me/cograph/reference/layer_similarity_matrix.md)
  : Pairwise Layer Similarities
- [`themes-builtin`](http://sonsoles.me/cograph/reference/themes-builtin.md)
  : Built-in Themes
- [`motifs()`](http://sonsoles.me/cograph/reference/motifs.md)
  [`print(`*`<cograph_motif_result>`*`)`](http://sonsoles.me/cograph/reference/motifs.md)
  [`plot(`*`<cograph_motif_result>`*`)`](http://sonsoles.me/cograph/reference/motifs.md)
  : Network Motif Analysis
- [`print(`*`<cograph_communities>`*`)`](http://sonsoles.me/cograph/reference/print.cograph_communities.md)
  : Print Community Structure
- [`print(`*`<cograph_network>`*`)`](http://sonsoles.me/cograph/reference/print.cograph_network.md)
  : Print cograph_network Object
- [`plot(`*`<cograph_cluster_significance>`*`)`](http://sonsoles.me/cograph/reference/plot.cograph_cluster_significance.md)
  : Plot Cluster Significance
- [`plot(`*`<cograph_communities>`*`)`](http://sonsoles.me/cograph/reference/plot.cograph_communities.md)
  : Plot Community Structure
- [`plot(`*`<cograph_motif_analysis>`*`)`](http://sonsoles.me/cograph/reference/plot.cograph_motif_analysis.md)
  : Plot Motif Analysis Results
- [`plot(`*`<cograph_motifs>`*`)`](http://sonsoles.me/cograph/reference/plot.cograph_motifs.md)
  : Plot Network Motifs
- [`plot(`*`<cograph_network>`*`)`](http://sonsoles.me/cograph/reference/plot.cograph_network.md)
  : Plot cograph_network Object
- [`plot(`*`<tna_disparity>`*`)`](http://sonsoles.me/cograph/reference/plot.tna_disparity.md)
  : Plot Disparity Filter Result
- [`summary(`*`<cograph_network>`*`)`](http://sonsoles.me/cograph/reference/summary.cograph_network.md)
  : Summary of cograph_network Object
- [`splot.tna_bootstrap()`](http://sonsoles.me/cograph/reference/splot.tna_bootstrap.md)
  : Plot Bootstrap Results
- [`splot.tna_disparity()`](http://sonsoles.me/cograph/reference/splot.tna_disparity.md)
  : Plot Disparity Results with splot
- [`abbrev_label()`](http://sonsoles.me/cograph/reference/abbrev_label.md)
  [`label_abbrev()`](http://sonsoles.me/cograph/reference/abbrev_label.md)
  : Abbreviate Labels
- [`as_mcml()`](http://sonsoles.me/cograph/reference/as_mcml.md) :
  Convert to mcml
- [`as_tna()`](http://sonsoles.me/cograph/reference/as_tna.md) : Convert
  cluster_summary to tna Objects
- [`build_mcml()`](http://sonsoles.me/cograph/reference/build_mcml.md) :
  Build MCML from Raw Transition Data
- [`cluster_significance()`](http://sonsoles.me/cograph/reference/cluster_significance.md)
  [`csig()`](http://sonsoles.me/cograph/reference/cluster_significance.md)
  : Test Significance of Community Structure
- [`cograph()`](http://sonsoles.me/cograph/reference/cograph.md) :
  Create a Network Visualization
- [`disparity_filter()`](http://sonsoles.me/cograph/reference/disparity_filter.md)
  : Disparity Filter
- [`from_qgraph()`](http://sonsoles.me/cograph/reference/from_qgraph.md)
  : Convert a qgraph object to cograph parameters
- [`from_tna()`](http://sonsoles.me/cograph/reference/from_tna.md) :
  Convert a tna object to cograph parameters
- [`get_data()`](http://sonsoles.me/cograph/reference/get_data.md) : Get
  Original Data from Cograph Network
- [`get_edges()`](http://sonsoles.me/cograph/reference/get_edges.md) :
  Get Edges from Cograph Network
- [`get_labels()`](http://sonsoles.me/cograph/reference/get_labels.md) :
  Get Labels from Cograph Network
- [`get_layout()`](http://sonsoles.me/cograph/reference/get_layout.md) :
  Get a Registered Layout
- [`get_meta()`](http://sonsoles.me/cograph/reference/get_meta.md) : Get
  Metadata from Cograph Network
- [`get_nodes()`](http://sonsoles.me/cograph/reference/get_nodes.md) :
  Get Nodes from Cograph Network
- [`get_source()`](http://sonsoles.me/cograph/reference/get_source.md) :
  Get Source Type from Cograph Network
- [`get_theme()`](http://sonsoles.me/cograph/reference/get_theme.md) :
  Get a Registered Theme
- [`coding`](http://sonsoles.me/cograph/reference/hai_datasets.md)
  [`coding_detailed`](http://sonsoles.me/cograph/reference/hai_datasets.md)
  [`ai_coding`](http://sonsoles.me/cograph/reference/hai_datasets.md)
  [`ai_detailed`](http://sonsoles.me/cograph/reference/hai_datasets.md)
  [`human_ai`](http://sonsoles.me/cograph/reference/hai_datasets.md)
  [`human_ai_detailed`](http://sonsoles.me/cograph/reference/hai_datasets.md)
  : Human-AI Interaction Coding Sequences
- [`layout_circle()`](http://sonsoles.me/cograph/reference/layout_circle.md)
  : Circular Layout
- [`layout_groups()`](http://sonsoles.me/cograph/reference/layout_groups.md)
  : Group-based Layout
- [`layout_oval()`](http://sonsoles.me/cograph/reference/layout_oval.md)
  : Oval Layout
- [`layout_spring()`](http://sonsoles.me/cograph/reference/layout_spring.md)
  : Fruchterman-Reingold Spring Layout
- [`list_layouts()`](http://sonsoles.me/cograph/reference/list_layouts.md)
  : List Available Layouts
- [`list_palettes()`](http://sonsoles.me/cograph/reference/list_palettes.md)
  : List Available Color Palettes
- [`list_shapes()`](http://sonsoles.me/cograph/reference/list_shapes.md)
  : List Available Shapes
- [`list_svg_shapes()`](http://sonsoles.me/cograph/reference/list_svg_shapes.md)
  : List Registered SVG Shapes
- [`list_themes()`](http://sonsoles.me/cograph/reference/list_themes.md)
  : List Available Themes
- [`n_edges()`](http://sonsoles.me/cograph/reference/n_edges.md) : Get
  Number of Edges
- [`n_nodes()`](http://sonsoles.me/cograph/reference/n_nodes.md) : Get
  Number of Nodes
- [`overlay_communities()`](http://sonsoles.me/cograph/reference/overlay_communities.md)
  : Overlay Community Blobs on a Network Plot
- [`palette_blues()`](http://sonsoles.me/cograph/reference/palette_blues.md)
  : Blues Palette
- [`palette_colorblind()`](http://sonsoles.me/cograph/reference/palette_colorblind.md)
  : Colorblind-friendly Palette
- [`palette_diverging()`](http://sonsoles.me/cograph/reference/palette_diverging.md)
  : Diverging Palette
- [`palette_pastel()`](http://sonsoles.me/cograph/reference/palette_pastel.md)
  : Pastel Palette
- [`palette_rainbow()`](http://sonsoles.me/cograph/reference/palette_rainbow.md)
  : Rainbow Palette
- [`palette_reds()`](http://sonsoles.me/cograph/reference/palette_reds.md)
  : Reds Palette
- [`palette_viridis()`](http://sonsoles.me/cograph/reference/palette_viridis.md)
  : Viridis Palette
- [`palettes`](http://sonsoles.me/cograph/reference/palettes.md) : Color
  Palettes
- [`register_layout()`](http://sonsoles.me/cograph/reference/register_layout.md)
  : Register a Custom Layout
- [`register_shape()`](http://sonsoles.me/cograph/reference/register_shape.md)
  : Register a Custom Shape
- [`register_svg_shape()`](http://sonsoles.me/cograph/reference/register_svg_shape.md)
  : Register Custom SVG Shape
- [`register_theme()`](http://sonsoles.me/cograph/reference/register_theme.md)
  : Register a Custom Theme
- [`render-ggplot`](http://sonsoles.me/cograph/reference/render-ggplot.md)
  : ggplot2 Conversion
- [`render-grid`](http://sonsoles.me/cograph/reference/render-grid.md) :
  Grid Rendering
- [`set_edges()`](http://sonsoles.me/cograph/reference/set_edges.md) :
  Set Edges in Cograph Network
- [`set_layout()`](http://sonsoles.me/cograph/reference/set_layout.md) :
  Set Layout in Cograph Network
- [`set_nodes()`](http://sonsoles.me/cograph/reference/set_nodes.md) :
  Set Nodes in Cograph Network
- [`simplify()`](http://sonsoles.me/cograph/reference/simplify.md) :
  Simplify a Network
- [`sn_edges()`](http://sonsoles.me/cograph/reference/sn_edges.md) : Set
  Edge Aesthetics
- [`sn_ggplot()`](http://sonsoles.me/cograph/reference/sn_ggplot.md) :
  Convert Network to ggplot2
- [`sn_layout()`](http://sonsoles.me/cograph/reference/sn_layout.md) :
  Apply Layout to Network
- [`sn_nodes()`](http://sonsoles.me/cograph/reference/sn_nodes.md) : Set
  Node Aesthetics
- [`sn_palette()`](http://sonsoles.me/cograph/reference/sn_palette.md) :
  Apply Color Palette to Network
- [`sn_save()`](http://sonsoles.me/cograph/reference/sn_save.md) : Save
  Network Visualization
- [`sn_save_ggplot()`](http://sonsoles.me/cograph/reference/sn_save_ggplot.md)
  : Save as ggplot2
- [`sn_theme()`](http://sonsoles.me/cograph/reference/sn_theme.md) :
  Apply Theme to Network
- [`soplot()`](http://sonsoles.me/cograph/reference/soplot.md)
  [`sn_render()`](http://sonsoles.me/cograph/reference/soplot.md) : Plot
  Cograph Network
- [`splot()`](http://sonsoles.me/cograph/reference/splot.md) : Base R
  Graphics Network Plotting
- [`student_interactions`](http://sonsoles.me/cograph/reference/student_interactions.md)
  : Student Interaction Edge List
- [`subgraphs()`](http://sonsoles.me/cograph/reference/subgraphs.md) :
  Extract Specific Motif Instances (Subgraphs)
- [`summarize_network()`](http://sonsoles.me/cograph/reference/summarize_network.md)
  [`cluster_network()`](http://sonsoles.me/cograph/reference/summarize_network.md)
  [`cnet()`](http://sonsoles.me/cograph/reference/summarize_network.md)
  : Summarize Network by Clusters
- [`theme_cograph_classic()`](http://sonsoles.me/cograph/reference/theme_cograph_classic.md)
  : Classic Theme
- [`theme_cograph_colorblind()`](http://sonsoles.me/cograph/reference/theme_cograph_colorblind.md)
  : Colorblind-friendly Theme
- [`theme_cograph_dark()`](http://sonsoles.me/cograph/reference/theme_cograph_dark.md)
  : Dark Theme
- [`theme_cograph_gray()`](http://sonsoles.me/cograph/reference/theme_cograph_gray.md)
  : Grayscale Theme
- [`theme_cograph_minimal()`](http://sonsoles.me/cograph/reference/theme_cograph_minimal.md)
  : Minimal Theme
- [`theme_cograph_nature()`](http://sonsoles.me/cograph/reference/theme_cograph_nature.md)
  : Nature Theme
- [`theme_cograph_viridis()`](http://sonsoles.me/cograph/reference/theme_cograph_viridis.md)
  : Viridis Theme
- [`unregister_svg_shape()`](http://sonsoles.me/cograph/reference/unregister_svg_shape.md)
  : Unregister SVG Shape
