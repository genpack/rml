# Draws the network of every model config in inst/extdata/gallery and saves it to
# vignettes/figures/gallery/<file name>.png, for vignette('rml-config-gallery').
# The vignette shows these images, so building it does not need DiagrammeR.
#
# Run from the package folder after installing rml:  Rscript data-raw/gallery_figures.R
# Configs of scikit-learn models need Python with scikit-learn and category_encoders (see vignette('rml-troubleshooting')).
# With DiagrammeR, DiagrammeRsvg and rsvg installed the charts use Graphviz's layout (as plot.network() does),
# otherwise the static igraph chart of plot.network('igraph').
library(rml)
configs = list.files('inst/extdata/gallery', pattern = '\\.(yml|yaml|json)$', full.names = TRUE)
out_dir = 'vignettes/figures/gallery'
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
use_graphviz = all(sapply(c('DiagrammeR', 'DiagrammeRsvg', 'rsvg'), requireNamespace, quietly = TRUE))
message(if(use_graphviz) 'Drawing with DiagrammeR (Graphviz).' else 'DiagrammeR, DiagrammeRsvg or rsvg missing: drawing with igraph.')

for(f in configs){
  model = suppressWarnings(model_from_config(f))
  png_file = file.path(out_dir, sub('\\.[a-z]+$', '.png', basename(f)))
  if(use_graphviz){
    net = model$info.network()
    colors = c(feeds = '#7f8c8d', 'boosted by' = '#e67e22', prediction = '#2c3e50')
    net$links$color = ifelse(net$links$relation %in% names(colors), colors[net$links$relation], '#3498db')
    svg = DiagrammeRsvg::export_svg(DiagrammeR::grViz(rml:::network_dot(net)))
    rsvg::rsvg_png(charToRaw(svg), png_file, width = 1800)
  } else {
    n_nodes = nrow(model$info.network()$nodes)
    grDevices::png(png_file, width = 8, height = max(3, min(7, 1 + 0.36*n_nodes)), units = 'in', res = 110)
    model$plot.network('igraph')
    grDevices::dev.off()
  }
  message('saved ', png_file)
}
