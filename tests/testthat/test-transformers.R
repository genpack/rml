test_that("transformer outputs become features of the main model", {
  d = make_binary_data()
  m = CLS.STATS.LR(name = 'lr', transformers = list(MAP.RML.MMS(name = 'mms'), CLS.XGBOOST(name = 'xgb', nrounds = 10, return = 'logit')))
  m$fit(d$X, d$y)
  expect_setequal(m$objects$features$fname, c(paste0('mms_', colnames(d$X)), 'xgb_logit'))
  expect_equal(nrow(m$predict(d$X)), nrow(d$X))
})

test_that("keep_columns and keep_features pass input columns through", {
  d = make_binary_data()
  X = d$X; X$label_text = 'a'
  t = MAP.RML.ZFS(name = 'zfs', keep_columns = TRUE)
  t$fit(X)
  out = t$predict(X)
  # keep_columns passes the columns the model did not use:
  expect_true('label_text' %in% colnames(out))
  expect_false('x1' %in% colnames(out))
  t2 = MAP.RML.ZFS(name = 'zfs2', keep_columns = TRUE, keep_features = TRUE)
  t2$fit(X)
  out2 = t2$predict(X)
  # with keep_features as well, all input columns are passed:
  expect_true(all(colnames(X) %in% colnames(out2)))
  expect_true(all(paste0('zfs2_', colnames(d$X)) %in% colnames(out2)))
})

test_that("features.include restricts the input columns", {
  d = make_binary_data()
  m = CLS.STATS.LR(features.include = c('x1', 'x3'))
  m$fit(d$X, d$y)
  expect_setequal(m$objects$features$fname, c('x1', 'x3'))
})

test_that("constant columns are removed by the default column filter", {
  d = make_binary_data()
  X = d$X; X$const = 1
  m = CLS.STATS.LR()
  m$fit(X, d$y)
  expect_false('const' %in% m$objects$features$fname)
})

test_that("correlation computes gini and log loss", {
  d = make_binary_data()
  x = d$X$x1
  expect_equal(correlation(x, d$y, 'gini'), gini(x, d$y))
  p = rep(0.5, length(d$y))
  expect_equal(correlation(p, d$y, 'log_loss'), log(2))
})
