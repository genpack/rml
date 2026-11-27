test_that("model_from_config builds a network from yaml text, json text and a list", {
  skip_if_not_installed('yaml')
  yml = "
model:
  class: CLS.STATS.LR
  name: top
  hyper_parameters:
    fe.enabled: no
  transformers:
  - class: MAP.RML.ZFS
    name: z
    features: [x1, x2]
  - class: FNT.RML.POLY
    n_terms: 2
    features: [x3]
features: [x1, x2, x3]
"
  m = model_from_config(yml)
  expect_true(inherits(m, 'CLS.STATS.LR'))
  expect_equal(m$name, 'top')
  expect_equal(m$config$features.include, c('x1', 'x2', 'x3'))
  expect_equal(sapply(m$transformers, function(t) t$name), c('z', 'poly_1'))
  expect_equal(m$transformers[[2]]$config$n_terms, 2)
  expect_false(m$config$fe.enabled)
  d = make_binary_data()
  m$fit(d$X, d$y)
  expect_gt(gini(m$predict(d$X)[[1]], d$y), 0.3)

  json = '{"class": "CLS.XGBOOST", "nrounds": 5, "gradient_transformers": [{"class": "CLS.STATS.LR", "name": "base"}]}'
  b = model_from_config(json)
  expect_equal(b$gradient_transformers[[1]]$name, 'base')
  b$fit(d$X, d$y)
  expect_true(any(b$info.network()$links$relation == 'boosted by'))

  l = model_from_config(list(class = 'ENS.RML.AGGR', transformers = list(list(class = 'CLS.STATS.LR'), list(class = 'CLS.STATS.LR'))))
  expect_equal(sapply(l$transformers, function(t) t$name), c('lr_1', 'lr_2'))
})

test_that("model_from_config reads files, feature files and reports errors", {
  skip_if_not_installed('yaml')
  dir = tempfile(); dir.create(dir)
  write.csv(data.frame(day = c(1, 1, 2), gini = c(0.3, 0.5, 0.9), fname = c('x1', 'x4', 'x2')), file.path(dir, 'sel.csv'), row.names = FALSE)
  writeLines(c("model:", "  class: CLS.STATS.LR", "features:", "  - x3",
               "  - file_path: sel.csv", "    operations:", "    - fun: filter", "      arguments: 'day == 1'",
               "    - fun: filter", "      arguments: 'gini == max(gini)'", "    - fun: pull", "      arguments: fname"),
             file.path(dir, 'm.yml'))
  m = model_from_config(file.path(dir, 'm.yml'))
  expect_equal(m$config$features.include, c('x3', 'x4'))

  expect_error(model_from_config(list(class = 'CLS.XGBOST')), "Did you mean 'CLS.XGBOOST'")
  expect_equal(class(model_from_config(list(class = 'enc.rml.te')))[1], 'ENC.RML.TE')
  expect_error(model_from_config(list(class = 'CLS.XGBOOST', nrounds = 5, hyper_parameters = list(nrounds = 6))), 'given both')
  expect_error(model_from_config(list(class = 'CLS.STATS.LR', features = list(list(file_path = file.path(dir, 'sel.csv'),
               operations = list(list(fun = 'system', arguments = 'ls')))))), 'not supported')
  expect_warning(model_from_config(list(class = 'CLS.STATS.LR', name = 'a', transformers = list(list(class = 'MAP.RML.ZFS', name = 'a')))),
                 'more than once')
})

test_that("from_config sets an existing model and checks its class", {
  m = CLS.STATS.LR(name = 'keep_me')
  m$from_config(list(transformers = list(list(class = 'MAP.RML.ZFS', name = 'z'))))
  expect_equal(m$name, 'keep_me')
  expect_equal(m$transformers[[1]]$name, 'z')
  expect_error(m$from_config(list(class = 'CLS.XGBOOST')), 'not CLS.STATS.LR')
})

test_that("the gallery configs build", {
  skip_if_not_installed('yaml')
  files = list.files(system.file('extdata', 'gallery', package = 'rml'), full.names = TRUE)
  expect_gte(length(files), 10)
  r_only = files[!sapply(files, function(f) any(grepl('SKLEARN|CATEGORY_ENCODERS', readLines(f))))]
  for(f in r_only) expect_true(inherits(model_from_config(f), 'MODEL'), info = basename(f))
})

test_that("CLS.RPART.DT and the smoothing of ENC.RML.TE work", {
  skip_if_not_installed('rpart')
  d = make_binary_data()
  m = CLS.RPART.DT(max_depth = 4)
  m$fit(d$X, d$y)
  p = m$predict(d$X)[[1]]
  expect_true(all(p >= 0 & p <= 1))
  expect_gt(gini(p, d$y), 0.4)
  X = data.frame(g = c('a', 'a', 'b', 'b', 'b', 'b'), stringsAsFactors = FALSE); y = c(1, 1, 0, 0, 0, 1)
  te = ENC.RML.TE(smoothing = 3); te$fit(X, y)
  expect_equal(te$predict(data.frame(g = 'a', stringsAsFactors = FALSE))[[1]], (2 + 3*0.5)/(2 + 3))
})
