# Helpers shared by all modules.
# They are exported because methods inherited by model classes defined outside rml run in the user's environment
# and can only see exported functions.
#
# WIDETABLE objects used to come from package 'rbig', which has since been renamed to 'rwidetable'.
# 'rbig' exported S4 methods colnames(), as.matrix() and as.data.frame() while 'rwidetable' registers
# S3/S4 methods on the base generics. These helpers work with plain data.frames, matrices and tibbles,
# and with WIDETABLE objects from either package.

wt_pkg_function = function(fname){
  if(requireNamespace('rbig', quietly = TRUE)){
    if(fname %in% getNamespaceExports('rbig')) return(getExportedValue('rbig', fname))
  }
  return(NULL)
}

# Column names of a data.frame, matrix, tibble or WIDETABLE
wt_colnames = function(X){
  if(inherits(X, 'WIDETABLE')){
    fun = wt_pkg_function('colnames')
    if(!is.null(fun)) return(fun(X))
  }
  colnames(X)
}

wt_as_matrix = function(X){
  if(inherits(X, 'WIDETABLE')){
    fun = wt_pkg_function('as.matrix')
    if(!is.null(fun)) return(fun(X))
  }
  as.matrix(X)
}

wt_as_data_frame = function(X){
  if(inherits(X, 'WIDETABLE')){
    fun = wt_pkg_function('as.data.frame')
    if(!is.null(fun)) return(fun(X))
  }
  as.data.frame(X)
}

# Mean logistic (log) loss between predictions x and binary labels y.
# x is taken as probabilities when all values lie in [0, 1], otherwise as logits (log-odds).
log_loss = function(x, y, eps = 1e-15){
  x = as.numeric(x)
  if(any(x < 0 | x > 1, na.rm = TRUE)){x = 1.0/(1.0 + exp(- x))}
  x = pmin(pmax(x, eps), 1 - eps)
  y = as.numeric(y)
  - mean(y*log(x) + (1 - y)*log(1 - x), na.rm = TRUE)
}

# Area under the ROC curve for a numeric score and binary labels (Mann-Whitney U statistic, ties count half).
# Same value as AUC::auc(AUC::roc(score, factor(labels))) but needs no extra package and runs in O(n log n).
auc_binary = function(score, labels){
  score  = as.numeric(score)
  labels = as.numeric(labels)
  keep   = !is.na(score) & !is.na(labels)
  score  = score[keep]; labels = labels[keep]
  pos    = labels == max(labels)
  n1     = sum(pos); n0 = length(labels) - n1
  if(n1 == 0 | n0 == 0) return(NA_real_)
  r = rank(score, ties.method = 'average')
  (sum(r[pos]) - n1*(n1 + 1)/2)/(as.numeric(n1)*n0)
}

# xgboost compatibility layer: the R API changed in xgboost 3.0 (watchlist -> evals, obj -> objective, feval -> custom_metric).
xgb_new_api = function(){
  utils::packageVersion('xgboost') >= '2.0.0'
}

xgb_dmatrix = function(X, label = NULL, base_margin = NULL){
  X = as.matrix(X)
  storage.mode(X) <- 'double'
  args = list(data = X)
  if(!is.null(label)){args$label <- label}
  if(!is.null(base_margin)){args$base_margin <- as.numeric(base_margin)}
  do.call(xgboost::xgb.DMatrix, args)
}

xgb_train = function(params, data, nrounds, evals = list(), obj = NULL, feval = NULL, ...){
  args = list(params = params, data = data, nrounds = nrounds, ...)
  if(xgb_new_api()){
    args$evals <- evals
    args$objective <- obj
    args$custom_metric <- feval
  } else {
    args$watchlist <- evals
    args$obj   <- obj
    args$feval <- feval
  }
  args = args[!vapply(args, is.null, logical(1))]
  do.call(xgboost::xgb.train, args)
}

# Returns data.frame((X[[col]] - center)/scale) for each column; non-finite results (missing values, zero scale) become 0
scale_columns = function(X, columns, center, scale){
  out = vector('list', length(columns))
  for(i in seq_along(columns)){
    v = (X[[columns[i]]] - center[i])/scale[i]
    v[!is.finite(v)] <- 0
    out[[i]] <- v
  }
  names(out) <- columns
  as.data.frame(out, optional = TRUE, stringsAsFactors = FALSE)
}
