test_that("xgboost boosts a logistic regression (gradient transformer)", {
  d = make_binary_data(n = 1000)
  lr = CLS.STATS.LR(name = 'lr', return = 'logit')
  xgb = CLS.XGBOOST(name = 'xgb', nrounds = 20, max_depth = 2, eta = 0.1, gradient_transformers = list(lr))
  xgb$fit(d$X, d$y)
  p = xgb$predict(d$X)[[1]]
  expect_true(all(p > 0 & p < 1))
  # boosted model is at least as good as the base logistic regression on training data
  base = lr$predict(d$X)[[1]]
  expect_gte(gini(p, d$y), gini(base, d$y) - 1e-6)
  # With zero rounds of correction the output equals the gradient transformer's output
  xgb0 = CLS.XGBOOST(name = 'xgb0', nrounds = 1, eta = 0, gradient_transformers = list(lr))
  xgb0$fit(d$X, d$y)
  expect_equal(xgb0$predict(d$X)[[1]], logit_inv(base), tolerance = 1e-5)
})

test_that("a classifier gradient transformer returning probabilities is used on the logit scale", {
  d = make_binary_data()
  m1 = CLS.XGBOOST(name = 'a', nrounds = 10, gradient_transformers = list(CLS.STATS.LR(name = 'g1', return = 'logit')))
  m2 = CLS.XGBOOST(name = 'b', nrounds = 10, gradient_transformers = list(CLS.STATS.LR(name = 'g2', return = 'probs')))
  m1$fit(d$X, d$y); m2$fit(d$X, d$y)
  expect_equal(m1$predict(d$X)[[1]], m2$predict(d$X)[[1]], tolerance = 1e-6)
})

test_that("a gradient transformer can be a column of offsets", {
  d = make_binary_data()
  X = d$X; X$offset = 0.5
  m = CLS.XGBOOST(name = 'o', nrounds = 5, gradient_transformers = list('offset'), features.exclude = 'offset')
  m$fit(X, d$y)
  expect_true(all(m$predict(X)[[1]] > 0))
})

test_that("row sampling keeps the gradient aligned with y", {
  d = make_binary_data()
  m = CLS.XGBOOST(name = 's', nrounds = 5, smp.enabled = TRUE, smp.method = 'downsample', smp.class_ratio = 0.5,
                  gradient_transformers = list(CLS.STATS.LR(name = 'g', return = 'logit')))
  captured = NULL
  m$fit(d$X, d$y)
  expect_true(m$fitted)
  # the trained trees saw a base margin: their own output alone is not the full prediction
  full = m$predict(d$X)[[1]]
  trees = m$model.predict(d$X)[[1]]
  expect_false(isTRUE(all.equal(full, trees)))
})

test_that("multiple gradient transformers are averaged", {
  d = make_binary_data()
  g1 = CLS.STATS.LR(name = 'g1', return = 'logit')
  g2 = CLS.STATS.LR(name = 'g2', return = 'logit', features.include = c('x1', 'x2'))
  m = CLS.XGBOOST(name = 'm', nrounds = 1, eta = 0, gradient_transformers = list(g1, g2))
  m$fit(d$X, d$y)
  expected = logit_inv((g1$predict(d$X)[[1]] + g2$predict(d$X)[[1]])/2)
  expect_equal(m$predict(d$X)[[1]], expected, tolerance = 1e-5)
})

test_that("regressors are trained on the residual of the gradient transformer", {
  d = make_regression_data()
  lm1 = REG.STATS.LNR(name = 'lm')
  m = REG.XGBOOST(name = 'xgbreg', nrounds = 50, max_depth = 3, gradient_transformers = list(lm1))
  m$fit(d$X, d$y)
  pred = m$predict(d$X)[[1]]
  base = lm1$predict(d$X)[[1]]
  expect_lt(mean(abs(pred - d$y)), mean(abs(base - d$y)))
  expect_equal(pred, base + m$model.predict(d$X)[[1]], tolerance = 1e-6)
})

test_that("classifiers without gradient support refuse gradient transformers", {
  d = make_binary_data()
  m = CLS.STATS.LR(gradient_transformers = list(CLS.STATS.LR(return = 'logit')))
  expect_error(m$fit(d$X, d$y), 'does not support gradient transformers')
})
