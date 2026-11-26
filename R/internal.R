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
#' @title Helpers for data.frames and WIDETABLE objects
#' @description
#' Column names and conversion of data.frames, matrices and WIDETABLE objects (from package rbig or its successor rwidetable).
#' @param X \code{data.frame}, matrix or WIDETABLE.
#' @return Column names, matrix or data.frame.
#' @rdname widetable_helpers
#' @export
wt_colnames = function(X){
  if(inherits(X, 'WIDETABLE')){
    fun = wt_pkg_function('colnames')
    if(!is.null(fun)) return(fun(X))
  }
  colnames(X)
}

#' @rdname widetable_helpers
#' @export
wt_as_matrix = function(X){
  if(inherits(X, 'WIDETABLE')){
    fun = wt_pkg_function('as.matrix')
    if(!is.null(fun)) return(fun(X))
  }
  as.matrix(X)
}

#' @rdname widetable_helpers
#' @export
wt_as_data_frame = function(X){
  if(inherits(X, 'WIDETABLE')){
    fun = wt_pkg_function('as.data.frame')
    if(!is.null(fun)) return(fun(X))
  }
  as.data.frame(X)
}

# Mean logistic (log) loss between predictions x and binary labels y.
# x is taken as probabilities when all values lie in [0, 1], otherwise as logits (log-odds).
#' @title Logistic loss
#' @description
#' Mean logistic (log) loss between predictions and binary labels. Predictions are taken as probabilities when all lie in [0, 1], otherwise as logits.
#' @param x \code{numeric} predictions.
#' @param y \code{numeric} binary labels (0/1).
#' @param eps Probabilities are clipped to \code{[eps, 1 - eps]}.
#' @return \code{numeric} scalar.
#' @examples
#' log_loss(c(0.9, 0.2, 0.6), c(1, 0, 1))
#' @export
log_loss = function(x, y, eps = 1e-15){
  x = as.numeric(x)
  if(any(x < 0 | x > 1, na.rm = TRUE)){x = 1.0/(1.0 + exp(- x))}
  x = pmin(pmax(x, eps), 1 - eps)
  y = as.numeric(y)
  - mean(y*log(x) + (1 - y)*log(1 - x), na.rm = TRUE)
}

# Area under the ROC curve for a numeric score and binary labels (Mann-Whitney U statistic, ties count half).
# Same value as AUC::auc(AUC::roc(score, factor(labels))) but needs no extra package and runs in O(n log n).
#' @title Area under the ROC curve
#' @description
#' AUC of a numeric score for binary labels, computed with the Mann-Whitney statistic (ties count half). Gini is \code{2*AUC - 1}.
#' @param score \code{numeric} score, higher meaning more likely class 1.
#' @param labels \code{numeric} or \code{logical} binary labels.
#' @return \code{numeric} AUC between 0 and 1, or \code{NA} if only one class is present.
#' @examples
#' auc_binary(c(0.1, 0.4, 0.35, 0.8), c(0, 0, 1, 1))
#' @export
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

#' @title xgboost helpers
#' @description
#' Build an \code{xgb.DMatrix} (with optional base margin) and call \code{xgb.train} with the argument names of the installed xgboost version (1.x or 3.x).
#' @param X \code{data.frame} or matrix of numeric features.
#' @param label Target vector.
#' @param base_margin Optional starting log-odds (or raw score) per row.
#' @param params Booster parameters.
#' @param data \code{xgb.DMatrix}.
#' @param nrounds Boosting rounds.
#' @param evals Named list of DMatrix objects to evaluate on.
#' @param obj Custom objective function.
#' @param feval Custom evaluation function.
#' @param ... Other arguments of \code{xgb.train}.
#' @return An \code{xgb.DMatrix} or a trained booster.
#' @rdname xgboost_helpers
#' @export
xgb_dmatrix = function(X, label = NULL, base_margin = NULL){
  X = as.matrix(X)
  storage.mode(X) <- 'double'
  args = list(data = X)
  if(!is.null(label)){args$label <- label}
  if(!is.null(base_margin)){args$base_margin <- as.numeric(base_margin)}
  do.call(xgboost::xgb.DMatrix, args)
}

#' @rdname xgboost_helpers
#' @export
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
#' @title Scale columns
#' @description
#' Returns \code{(X[[col]] - center)/scale} for each column; missing and infinite results become 0.
#' @param X \code{data.frame}.
#' @param columns Column names.
#' @param center \code{numeric} centre per column.
#' @param scale \code{numeric} scale per column.
#' @return \code{data.frame}.
#' @export
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
