# Shared Legend Renderer

Single internal entry point for every base-R legend in the cograph
package so that device-scale compensation (`visual_scale`) is applied
uniformly. Replaces five divergent compensation schemes (splot literal,
plot_mcml literal, plot_htna/mtna/mlna `1/sqrt(scale)`, render-grid
hardcoded 8pt, ggplot absolute cm).
