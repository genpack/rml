test_that("reset() of an unfitted model keeps features.include", {
  m = CLS.STATS.LR(name = 'lr', features.include = c('x1', 'x2'))
  m$reset(); m$reset()
  expect_equal(m$config$features.include, c('x1', 'x2'))
})

test_that("FNT.RML.LOGIT scales with the training range", {
  X = data.frame(a = c(0, 5, 10))
  m = FNT.RML.LOGIT(name = 'lg', verbose = 0)
  m$fit(X)
  out = m$predict(data.frame(a = 5))[[1]]
  expect_equal(out, 0, tolerance = 1e-8)
})

test_that("ENS.RML.BS combines two pupils", {
  skip_if_not_installed('xgboost')
  d = make_binary_data(800)
  m = ENS.RML.BS(name = 'bs', verbose = 0, model_config = list(nrounds = 10, max_depth = 2),
    transformers = list(CLS.STATS.LR(name = 'p1', features.include = c('x1', 'x2')),
                        CLS.STATS.LR(name = 'p2', features.include = c('x3', 'x4')),
                        MAP.RML.IDT(name = 'raw')))
  m$fit(d$X, d$y)
  expect_equal(m$objects$pupils, c('p1_probs', 'p2_probs'))
  p = m$predict(d$X)[[1]]
  expect_true(all(p >= 0 & p <= 1))
  expect_gt(gini(p, d$y), 0.3)
})

test_that("ENS.RML.AGGR averages its transformers", {
  d = make_binary_data()
  a = CLS.STATS.LR(name = 'a', features.include = 'x1'); b = CLS.STATS.LR(name = 'b', features.include = 'x2')
  m = ENS.RML.AGGR(name = 'avg', verbose = 0, transformers = list(a, b))
  m$fit(d$X, d$y)
  expect_equal(m$predict(d$X)[[1]], (a$predict(d$X)[[1]] + b$predict(d$X)[[1]])/2, tolerance = 1e-8)
})

test_that("ENC.RML.ME works with its default segment model", {
  set.seed(5)
  X = data.frame(g = sample(c('a', 'b'), 600, replace = TRUE), x = rnorm(600), stringsAsFactors = FALSE)
  y = as.numeric(runif(600) < plogis(ifelse(X$g == 'a', 2*X$x, -2*X$x)))
  m = ENC.RML.ME(name = 'me', min_rows = 100)
  m$fit(X, y)
  expect_gt(gini(m$predict(X)[[1]], y), 0.5)
})

test_that("optSplitColumns.chi skips constant columns", {
  d = make_binary_data()
  df = cbind(const = 1, d$X, label = d$y)
  res = optSplitColumns.chi(df, c('const', 'x1', 'x2'), label_col = 'label')
  expect_equal(as.character(res$Column), c('x1', 'x2'))
})

test_that("service_models and hp_booster rank models with a single metric", {
  skip_if_not_installed('purrr')
  d = make_binary_data(800)
  tr = 1:500
  base = CLS.STATS.LR(name = 'lr', verbose = 0)
  res = hp_booster(base, 'features.include', list(c('x1'), c('x1', 'x2', 'x3')),
                   X_train = d$X[tr, ], y_train = d$y[tr], X_test = d$X[-tr, ], y_test = d$y[-tr], metrics = 'gini')
  expect_equal(res$best_model$name, 'lr_HPB2')
  expect_true('gini' %in% colnames(res$results))
})

test_that("build_model_from_template uses the template's transformers", {
  tmp = list(top = list(class = 'CLS.STATS.LR', transformers = list(list(templates = 'z', probability = 1))),
             z = list(class = 'MAP.RML.ZFS'))
  m = build_model_from_template('top', model_name = 'm', templates = tmp)
  expect_s4_class(m, 'CLS.STATS.LR')
  expect_length(m$transformers, 1)
  expect_s4_class(m$transformers[[1]], 'MAP.RML.ZFS')
})
