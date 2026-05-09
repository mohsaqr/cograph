# Shared Legend Renderer

Shared internal helper for base-R legends that opt into device-scale
compensation (`visual_scale`). Used by core base-R plotters to avoid
per-plotter size drift; some legacy direct
[`graphics::legend()`](https://rdrr.io/r/graphics/legend.html) calls
remain in older helpers.
