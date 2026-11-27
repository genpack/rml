test_that("info.network() and the Graphviz code describe the model network", {
  d = make_binary_data()
  base = CLS.STATS.LR(name = 'base', features.include = c('x1', 'x2'))
  top  = CLS.XGBOOST(name = 'top', nrounds = 5, transformers = list(MAP.RML.ZFS(name = 'z', features.include = c('x1', 'x4'))),
                     gradient_transformers = list(base))
  top$fit(d$X, d$y)
  net = top$info.network()
  expect_setequal(net$nodes$id, c('INPUT', 'top', 'z', 'base', 'OUTPUT'))
  expect_true(any(net$links$source == 'base' & net$links$target == 'top' & net$links$relation == 'boosted by'))
  net$links$color = '#7f8c8d'
  dot = rml:::network_dot(net)
  expect_match(dot, '^digraph rml \\{')
  expect_match(dot, '"base" -> "top" \\[[^]]*style = dashed')
  expect_match(dot, '"z" \\[label = "z\\\\nMAP.RML.ZFS"', fixed = FALSE)
  expect_equal(lengths(regmatches(dot, gregexpr('\\{', dot))), lengths(regmatches(dot, gregexpr('\\}', dot))))
  expect_false(grepl('labelloc', rml:::network_dot(net, legend = FALSE)))
  # names with quotes are escaped
  net$nodes$id[2] = 'a"b'; net$nodes$label[2] = 'a"b'
  expect_match(rml:::network_dot(net), '"a\\\\"b"', fixed = FALSE)
})

test_that("plot.network() draws with the igraph fallback", {
  skip_if_not_installed('igraph')
  d = make_binary_data()
  m = CLS.STATS.LR(name = 'lr', transformers = list(MAP.RML.ZFS(name = 'z')))
  m$fit(d$X, d$y)
  f = tempfile(fileext = '.png'); grDevices::png(f); on.exit(unlink(f))
  expect_silent(m$plot.network('igraph'))
  grDevices::dev.off()
})
