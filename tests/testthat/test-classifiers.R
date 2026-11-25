test_that("CLS.XGBOOST returns calibrated probabilities by default", {
  d = make_binary_data()
  m = CLS.XGBOOST(name = 'xgb', nrounds = 30, max_depth = 3, verbose = 0)
  m$fit(d$X, d$y)
  p = m$predict(d$X)
  expect_equal(colnames(p), 'xgb_probs')
  expect_true(all(p[[1]] > 0 & p[[1]] < 1))
  # calibrated: mean predicted probability close to the label rate
  expect_lt(abs(mean(p[[1]]) - mean(d$y)), 0.05)
  expect_gt(gini(p[[1]], d$y), 0.6)
})

test_that("classifier return types are consistent", {
  d = make_binary_data()
  m = CLS.XGBOOST(name = 'xgb', nrounds = 20, return = 'logit')
  m$fit(d$X, d$y)
  lg = m$predict(d$X)[[1]]
  m$config$return = 'probs'
  pr = m$predict(d$X)[[1]]
  expect_equal(lg, logit_fwd(pr), tolerance = 1e-6)
  m$config$return = 'class'
  cl = m$predict(d$X)[[1]]
  expect_setequal(unique(cl), c(0, 1))
  expect_equal(cl, as.numeric(pr > m$config$threshold))
})

test_that("CLS.STATS.LR fits a logistic regression", {
  d = make_binary_data()
  m = CLS.STATS.LR(name = 'lr')
  m$fit(d$X, d$y)
  p = m$predict(d$X)[[1]]
  ref = stats::glm(y ~ ., data = cbind(d$X, y = d$y), family = binomial)
  expect_equal(unname(p), unname(stats::predict(ref, type = 'response')), tolerance = 1e-8)
})

test_that("threshold determination works", {
  d = make_binary_data()
  m = CLS.STATS.LR(name = 'lr', threshold_determination = 'maximum_f1')
  m$fit(d$X, d$y)
  expect_true(m$config$threshold > 0 & m$config$threshold < 1)
  m2 = CLS.STATS.LR(name = 'lr2', threshold_determination = 'label_rate_quantile')
  m2$fit(d$X, d$y)
  expect_true(m2$config$threshold > 0 & m2$config$threshold < 1)
})

test_that("verbose and other config values given by the user are kept", {
  m = CLS.STATS.LR(verbose = 0, metrics = 'gini')
  expect_equal(m$config$verbose, 0L)
  expect_equal(m$config$metrics, 'gini')
  expect_equal(CLS.STATS.LR()$config$metrics, 'pearson')
  expect_equal(REG.STATS.LNR()$config$metrics, 'mae')
})

test_that("performance and cross-validation work and do not touch the original model", {
  d = make_binary_data()
  m = CLS.STATS.LR(name = 'lr', transformers = MAP.RML.ZFS(name = 'zfs'), cv.ntest = 2)
  scores = m$performance.cv(d$X, d$y, metrics = 'gini')
  expect_length(scores, 2)
  expect_true(all(unlist(scores) > 0.5))
  expect_false(m$fitted)
  expect_false(m$transformers[[1]]$fitted)
})
