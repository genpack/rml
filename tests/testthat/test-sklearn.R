test_that("sklearn classifiers work through reticulate", {
  skip_if_not(has_python_module('sklearn'), 'scikit-learn not available')
  d = make_binary_data()
  m = CLS.SKLEARN.LR(name = 'sklr', return = 'logit')
  m$fit(d$X, d$y)
  out = m$predict(d$X)
  expect_equal(colnames(out), 'sklr_logit')
  expect_true(any(out[[1]] < 0))
  expect_gt(gini(out[[1]], d$y), 0.5)
  # CLASSIFIER methods are available on CLS.SKLEARN models:
  expect_true(is.numeric(unlist(m$performance(d$X, d$y, metrics = 'gini'))))
})

test_that("sklearn decision tree and KNN classifiers fit and predict", {
  skip_if_not(has_python_module('sklearn'), 'scikit-learn not available')
  d = make_binary_data()
  for(m in list(CLS.SKLEARN.DT(max_depth = 3L), CLS.SKLEARN.KNN(n_neighbors = 10L))){
    m$fit(d$X, d$y)
    p = m$predict(d$X)[[1]]
    expect_true(all(p >= 0 & p <= 1))
  }
})
