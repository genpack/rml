# Synthetic binary classification and regression data used by the tests
make_binary_data = function(n = 600, seed = 1){
  set.seed(seed)
  X = data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = runif(n), x4 = rnorm(n))
  logit = 1.5*X$x1 - X$x2 + 2*(X$x3 > 0.5) + 0.8*X$x1*X$x4
  y = as.numeric(runif(n) < 1/(1 + exp(-logit)))
  list(X = X, y = y)
}

make_regression_data = function(n = 600, seed = 2){
  set.seed(seed)
  X = data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = runif(n))
  y = 3*X$x1 - 2*X$x2 + 4*sin(3*X$x3) + rnorm(n, sd = 0.3)
  list(X = X, y = y)
}

gini = function(score, y) 2*rml:::auc_binary(score, y) - 1

has_python_module = function(module){
  requireNamespace('reticulate', quietly = TRUE) && reticulate::py_module_available(module)
}
