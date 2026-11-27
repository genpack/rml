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

test_that("whole numbers are passed to Python as integers", {
  args = rml:::python_args(list(max_iter = 100, C = 0.5, name = 'x', n = 3L, v = c(1, 2)), reserved_words = 'name')
  expect_identical(args$max_iter, 100L)
  expect_identical(args$C, 0.5)
  expect_identical(args$v, c(1L, 2L))
  expect_null(args$name)
})

test_that("CLS.SKLEARN.LR accepts max_iter as a double and defaults to 1000", {
  skip_if_not(has_python_module('sklearn'))
  d = make_binary_data()
  expect_equal(CLS.SKLEARN.LR(name = 'a')$config$max_iter, 1000L)
  m = CLS.SKLEARN.LR(name = 'lr', max_iter = 200)
  m$fit(d$X, d$y)
  expect_true(m$fitted)
  knn = CLS.SKLEARN.KNN(name = 'knn', n_neighbors = 15)
  knn$fit(d$X, d$y)
  expect_true(knn$fitted)
})
