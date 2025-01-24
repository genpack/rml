# mltools.R
#' @export
cross_accuracy = function(v1, v2){
  N = length(v2)
  if(is.null(dim(v1)) & inherits(v1, c('logical', 'integer', 'numeric'))){
    a = (v1 %>% xor(v2) %>% sum)/N
  } else if (nrow(v1) == N){
    a = (v1 %>% xor(v2) %>% colSums)/N
  } else {
    stop('Something is wrong!')
  }
  return(a %>% sapply(function(x) max(x, 1-x)))
}

#' @export
rmse = function(y1, y2){
  mean((y1 - y2)^2, na.rm = T) %>% sqrt
}

#' @export
mae = function(y1, y2){
  (y1 - y2) %>% abs %>% mean(na.rm = T)
}

#' @export
mape = function(actual, pred){
  assert(length(actual) == length(pred), "Lenghts of the two vectors must be identical!")
  tk = which(abs(actual) > 0)
  abs((actual[tk] - pred[tk])/actual[tk]) %>% mean(na.rm = T)
}

#' @export
geo_mape = function(actual, pred){
  assert(length(actual) == length(pred), "Lenghts of the two vectors must be identical!")
  tk = which(abs(actual) > 0)
  abs((actual[tk] - pred[tk])/actual[tk]) %>% log %>% mean(na.rm = T) %>% exp
}


#' @export
medae = function(y1, y2){
  (y1 - y2) %>% abs %>% median(na.rm = T)
}

accuracy_mae = function(pred, actual){
  1.0 - mae(actual, pred)/mae(actual, 0)
}

accuracy_medae = function(pred, actual){
  1.0 - medae(actual, pred)/medae(actual, 0)
}

accuracy_rmse = function(pred, actual){
  1.0 - rmse(actual, pred)/rmse(actual, 0)
}

r_squared = function(pred, actual){
  mean_actual = mean(actual, na.rm = T)
  1.0 - sum((pred - actual)^2, na.rm = T)/sum((actual-mean_actual)^2, na.rm = T)
}


#' @export
cross_f1 = function(v1, v2){
  v2 = verify(v2, c('logical', 'integer', 'numeric'), null_allowed = F) %>% as.logical
  N  = length(v2)

  if(is.null(dim(v1)) & inherits(v1, c('logical', 'integer', 'numeric'))){
    v1  %<>% as.logical
    tp = sum(v1 & v2)
    fp = sum(v1 & (!v2))
    tn = sum((!v1) & (!v2))
    fn = sum((!v1) & v2)
    pr = tp/(tp + fp)
    rc = tp/(tp + fn)
    return(2*pr*rc/(pr+rc))

  } else if (nrow(v1) == N){
    return(v1 %>% apply(2, function(x) cross_f1(x, v2)))
  } else {
    stop('Something is wrong!')
  }
  return(a %>% sapply(function(x) max(x, 1-x)))
}

# converts probabilities to logit
#' @export
logit_fwd = function(x) {
  x    = abs(x)
  mask = x < .Machine$double.eps
  x[mask] <- .Machine$double.eps
  mask = x > 1.0 - .Machine$double.eps
  x[mask] <- 1.0 - .Machine$double.eps
  log(x/(1-x))
}

# convert logit to probabilities
#' @export
logit_inv = function(x){
  e = exp(x)
  return(1.0 - 1.0/(1 + e))
}

# todo: should work for WideTables as well
#' @export
int_ordinals = function(X){
  if(inherits(X, 'WIDETABLE')) {X <- rbig::as.data.frame(X)}
  if(inherits(X, 'matrix')) {X %<>% as.data.frame}
  nms = colnames(X)
  for (col in nms){
    v = X[[col]]
    if(inherits(v, c('numeric', 'integer', 'integer64'))){
      if(sum(as.integer(v) != v, na.rm = T) == 0) X[,col] %<>%  as.integer
    }
  }
  return(X)
}


# Internal Function
add_keras_layer_dense = function(input_layer, layer, ...){
  input_layer %<>% layer_dense(
    units = verify(layer$units, c('numeric', 'integer'), lengths = 1, null_allowed = F) %>% as.integer,
    activation  = verify(layer$activation, 'character', lengths = 1, default = 'relu'),
    use_bias    = verify(layer$use_bias, 'logical', lengths = 1, domain = c(T,F), default = T),
    kernel_regularizer = layer$kernel_regularizer,
    bias_regularizer   = layer$kernel_regularizer,
    activity_regularizer = layer$activity_regularizer,
    kernel_constraint = layer$kernel_constraint,
    bias_constraint = layer$bias_constraint,
    batch_input_shape = layer$batch_input_shape,
    batch_size = layer$batch_size,
    name = layer$name, trainable = layer$trainable,
    weights = layer$weights,
    ...)
  if(!is.null(layer$dropout)){
    input_layer %<>% layer_dropout(rate = layer$dropout)
  }
}

# Internal Function
create_keras_layers = function(config){
  build_regularizer = function(l1, l2){
    if(l1 == 0){
      if (l2 == 0){
        kr = NULL
      } else {
        kr = regularizer_l2(l = l2)
      }
    } else if (l2 == 0){
      kr = regularizer_l1(l = l1)
    } else {
      kr = regularizer_l1_l2(l1 = l1, l2 = l2)
    }
  }

  layers  = list()
  n_nodes = config$first_layer_nodes
  j = 0

  for(i in sequence(config$num_layers)){
    if(n_nodes > 0){
      j = j + 1
      layers[[j]] <- list(name = paste('dense', j, sep = '_'), units = n_nodes,
                          activation = config$layers_activation, dropout = config$layers_dropout,
                          kernel_initializer   = initializer_random_uniform(minval = config$initializer_minval, maxval = config$initializer_maxval, seed = config$initializer_seed),
                          kernel_regularizer   = build_regularizer(config$kernel_regularization_penalty_l1, config$kernel_regularization_penalty_l2),
                          activity_regularizer = build_regularizer(config$activity_regularization_penalty_l1, config$activity_regularization_penalty_l2))
      n_nodes     <- as.integer(n_nodes*config$layer_nodes_ratio)
    }
  }
  return(layers)
}

#' This function gets a vector and returns a vector of logical which is TRUE if the corresponding value is an outlier
#' 
#' @field x \code{numeric} : Input vector
#' @field sd_threshold \code{numeric} : Standard Deviation threshold as a criteria for outlier determination.
#' @field recursive \code{logical} : TRUE if the process of outlier detection should continue with eliminated outliers
#' @return logical vector specifying which element of the given vector \code{x} is outlier.
#' @export
outlier = function(x, sd_threshold = 4, recursive = F){
  avg   = mean(x, na.rm = T)
  sdv   = sd(x, na.rm = T)
  isout = (x > avg + sd_threshold*sdv) | (x < avg - sd_threshold*sdv)
  if(!recursive){
    return(isout)
  } else {
    wout = which(isout)
    ind  = x %>% length %>% sequence %>% setdiff(isout)
    while(length(wout) > 0){
      wout = outlier(x[ind], sd_threshold = sd_threshold, recursive = F) %>% which
      ind  = ind %-% ind[wout]
    }
    out = rep(TRUE, length(x))
    out[ind] <- FALSE
    return(out)
  }
}


#' This function gets a data.frame and replaces each cell with its rank among all the cells in its column.
#' 
#' @field X \code{data.frame} or \code{matrix}: Input table to be ranked
#' @return \code{data.frame} containing ranked values. Suffix \code{_rank} is added to each column header. 
#' @export
ranker = function(X){
  X %<>% as.data.frame
  for(col in rbig::colnames(X)){
    v = X %>% pull(col)
    if(inherits(v, 'numeric')){
      X[, col %>% paste('rank', sep = '_')] <-  order(v)
    }
  }
  return(X)
}

#' @export
trim_outliers = function(X, adaptive = F, ...){
  # Since this function will edit the table, WideTable object cannot be passed to it currently.
  X %<>% as.data.frame()
  for(i in sequence(ncol(X))){
    wout = X[[i]]  %>% outlier(...) %>% which
    if(length(wout) > 0){
      maxx = X[-wout, i] %>% max(na.rm =  T)
      minx = X[-wout, i] %>% min(na.rm =  T)
      imax = which(X[[i]] > maxx)
      imin = which(X[[i]] < minx)
      if(adaptive){
        X[imax, i] <- maxx + log(1.0 + X[imax, i] - maxx)
        X[imin, i] <- minx - log(1.0 + minx - X[imin, i])
      } else {
        X[imax, i] <- maxx
        X[imin, i] <- minx
      }
    }
  }
  return(X)
}

#' Imputes missing values with the median of non-existing values in each column
#' 
#' @field X \code{data.frame} or \code{matrix}: Input table to be imputed
#' @return \code{data.frame} with missing values replaced by imputed values
#' @export
na2median = function(X){
  X %<>% data.frame
  for(i in 1:ncol(X)) if(inherits(X[,i], 'numeric')) X[is.na(X[,i]), i] <- median(X[,i], na.rm = T)
  return(X)
}

#' Use this function to get correlation of two vectors with various metrics.
#' 
#' @field x \code{vector or table} first vector. If a table or matrix is passed, correlation is measured for each column of the table.
#' @field y \code{vector} second vector (must have the same number of elements/rows as vector/table \code{x})
#' @field metrics \code{character} specifying which metrics you desire to be computed. Valid values are
#' \code{pearson, log_loss, aurc, gini, tp, fn, fp, tn, tpr, fnr, fpr, tnr, ppv, fdr, npv, pt, ts, csi, ba, 
#' recall, sensitivity, hit_rate, miss_rate, f1, specificity, selectivity, fall_out, precision, accuracy, fmi, 
#' informedness, markedness, mcc, for, lift, optsplit.chi, optsplit.f1}
#' @return \code{list} The values of specified correlation metrics between two given vectors by the specified metrics. 
#' @export
correlation = function(x, y, metrics = 'pearson', threshold = NULL, quantiles = NULL){

  if(inherits(x, c('data.frame', 'WIDETABLE', 'matrix'))){
    if(!inherits(x, 'WIDETABLE')){cols = rbig::colnames(x)} else {cols = colnames(x)}
    if(is.null(cols)){cols = sequence(ncol(x))}
    out = list()
    for(col in cols){
      out[[col]] <- correlation(x[, col], y, metrics = metrics, threshold = threshold, quantiles = quantiles)
    }
  } else {
    if(inherits(x, 'character')) {x = as.factor(x)}
    x = as.numeric(x)
    y = as.numeric(y)
    
    out = list()
    for (metric in metrics %^% 'pearson'){
      out[[metric]] <- cor(x, y) 
    }

    for (metric in metrics %^% 'optsplit.chi'){
      assert(length(unique(y)) == 2, "Only works for binary y")
      cbind(feature = x, label = y) %>% optSplit.chi(num_col = 'feature', cat_col = 'label') -> res
      out[[metric]] <- res$correlation
      out[[metric %>% paste('cutpoint', sep = '.')]] <- res$split
      out[[metric %>% paste('pvalue', sep = '.')]] <- res$pvalue
    }

    for (metric in metrics %^% 'optsplit.f1'){
      assert(length(unique(y)) == 2, "Only works for binary y")
      cbind(feature = x, label = y) %>% optSplit.f1(num_col = 'feature', cat_col = 'label') -> res
      out[[metric]] <- res$f1
      out[[metric %>% paste('cutpoint', sep = '.')]] <- res$split
    }
    
    subset = metrics %^% c('aurc', 'gini')
    if(length(subset) > 0){
      aurc = AUC::auc(AUC::roc(x %>% vect.map, y %>% as.factor))
      for (metric in subset){
        if(metric == 'aurc') out[[metric]] = aurc else out[[metric]] = 2*aurc - 1
      }
    }
    
    for(metric in metrics %^% 'log_loss'){
      lossfun = rfun::logistic$copy()
      lossfun$reset()
      lossfun$inputs$x = x
      lossfun$inputs$y = y
      out[[metric]] <- lossfun$get.output() %>% mean
    }
    
    for(metric in metrics %^% c('mae', 'rmse', 'medae', 'r_squared', 'mape', 'geo_mape')){
      out[[metric]] <- do.call(what = metric, args = list(x,y))
    }

    for(metric in metrics %^% c('accuracy_mae', 'accuracy_rmse', 'accuracy_medae')){
      out[[metric]] <- do.call(what = metric, args = list(x,y))
    }
    
    subset = metrics %^% all_binary_predictive_scores
    if(length(subset) > 0){
      if (!is.null(quantiles)){
        for(q in quantiles){
          cut_q = quantile(x, prob = 1 - q, na.rm = T)
          score = data.frame(y_pred = as.logical(x > cut_q), y_true = as.logical(y)) %>% scorer('y_pred', 'y_true')
          for (metric in subset){
            out[[paste0(metric, '_', round(100*q), 'pc')]] = score[[metric]]
          }
        }
      } else if (is.null(threshold)){
        threshold = quantile(x, prob = 1.0 - mean(y, na.rm = T), na.rm = T)
      }
      
      if(!is.null(threshold)){
        score = data.frame(y_pred = as.logical(x > threshold), y_true = as.logical(y)) %>% scorer('y_pred', 'y_true')
        for (metric in subset){
          out[[metric]] = score[[metric]]
        }
      }
    }    
  }  


  while(inherits(out, 'list') & (length(out) == 1)) out = out[[1]]    

  return(out)
}

# Groups features based on count of their unique values
#' @export
group_features = function(X, nominals_only = F){
  if(nominals_only) X = X[nominals(X)]
  colnames(X) %>% sapply(function(x) X %>% pull(x) %>% unique %>% length) %>% unlist -> lst
  ns = names(lst)
  list(
    unique = ns[which(lst == 1)],
    binary = ns[which(lst == 2)],
    triple = ns[which(lst == 3)],
    lest10 = ns[which((lst < 10) & (lst > 3))],
    lest20 = ns[which((lst < 20) & (lst > 10))],
    lest50 = ns[which((lst < 50) & (lst > 20))],
    numers = ns[which(lst >= 50)]
  )
}

# Returns binary Chi-Squared statistics for two binary columns
#' @export
spark.binchisq = function(tbl, col1, col2){
  tbl %>% rename(x = col1, y = col2) %>% dplyr::select(x, y) %>%
    mutate(x = as.numeric(x), y = as.numeric(y), z = as.numeric(as.logical(x) & as.logical(y))) %>%
    sdf_describe %>% collect -> dsc

  a = dsc[2, 'x'] %>% as.numeric
  b = dsc[2, 'y'] %>% as.numeric
  c = dsc[2, 'z'] %>% as.numeric
  den = a*b*(1-a)*(1-b)
  return((c- a*b)^2/den)
}

# Transfer to package Optimer
#' @export
optimSearch1d = function(fun, domain, ...){
  mx = max(domain)
  mn = min(domain)
  h  = mx - mn
  k  = 0.5
  x  = mn
  f0 = fun(x, ...)
  d  = 1
  fl = T
  while(fl){
    keep = fv
    while(k > 0.0001){
      x  = x + d*k*h
      fv = fun(x, ...)
      if( fv > f0 ){
        x  = x - d*k*h
        k  = 0.5*k
        fv = f0
      }
      f0 <- fv
    }
    fl = (keep - fv > 0.0000001)
    k  = 0.5
    d  = - d
  }

  return(x)
}

#' @export
optSplit.chi = function(tbl, num_col, cat_col, fun = NULL){
  tbl %<>% rename(x = num_col, y = cat_col) %>% dplyr::select(x, y)
  if(!is.null(fun)){tbl %<>% mutate(x = fun(x))}
  N = nrow(tbl)
  b = tbl %>% pull('y') %>% mean
  tbl %<>%
    group_by(x) %>% summarise(X = length(x), Y = sum(y)) %>% arrange(x) %>%
    mutate(a = cumsum(X)/N, c = cumsum(Y)/N) %>% filter(a < 1) %>%
    mutate(den = a*b*(1-a)*(1-b)) %>%
    mutate(chi = (c- a*b)^2/den) %>% arrange(chi)

  out <- tbl %>% tail(1) %>% dplyr::select(split = x, correlation = chi) %>% as.list

  #out$chisq  <- N*out$chisq
  out$pvalue <- pchisq(out$correlation, df = 1, lower.tail = F)
  return(out)
}

#' @export
spark.optSplit.chi = function(tbl, num_col, cat_col, breaks = 1000, fun = NULL){
  tbl %<>% rename(xxx = num_col, yy = cat_col) %>% dplyr::select(xxx, yy)
  if(!is.null(fun)){tbl %<>% mutate(xxx = fun(xxx))}
  tbl %>% sdf_describe %>% collect -> dsc

  mn = dsc[4, 'xxx'] %>% as.numeric
  mx = dsc[5, 'xxx'] %>% as.numeric
  hh = mx - mn
  N  = dsc[1, 'xxx'] %>% as.numeric
  b  = dsc[2, 'yy'] %>% as.numeric

  tbl %<>% mutate(xx = as.integer(breaks*(xxx - mn)/hh)) %>% group_by(xx) %>%
    summarise(X = COUNT(xx), Y = sum(yy, na.rm = T)) %>%
    dbplyr::window_frame(-Inf, 0) %>% dbplyr::window_order(xx) %>%
    mutate(a = sum(X, na.rm = T)/N, c = sum(Y, na.rm = T)/N) %>% filter(a < 1) %>%
    mutate(den = a*b*(1-a)*(1-b)) %>%
    mutate(chi = (c- a*b)^2/den) %>% arrange(desc(chi))

  # tbl %<>%
  #   group_by(xx) %>% summarise(X = COUNT(xx), Y = sum(yy, na.rm = T)) %>% arrange(X) %>%
  #   dbplyr::window_frame(-Inf, 0) %>%
  #   mutate(a = sum(X, na.rm = T)/N, c = sum(Y, na.rm = T)/N) %>% filter(a < 1) %>%
  #   mutate(den = a*b*(1-a)*(1-b)) %>%
  #   mutate(chi = (c- a*b)^2/den) %>% arrange(desc(chi))

  tbl %>% head(1) %>% collect %>% mutate(split = mn + xx*hh/breaks) %>%
    dplyr::select(split, correlation = chi) %>% as.list -> out
  #out$chisq  <- N*out$chisq
  #out$pvalue <- pchisq(out$chisq, df = 1, lower.tail = F)
  return(out)
}

# prob_col : column name containing probabilities of class 1 or positive
# label_col: column name containing actual class labels
# dte stands for decision threshold evaluator:
# Returns a table containing performance metrics for each decision threshoild
#' @export
spark.dte = function(tbl, prob_col, label_col, breaks = 1000, fun = NULL){
  tbl %<>% rename(xxx = prob_col, yy = label_col) %>% dplyr::select(xxx, yy)
  if(!is.null(fun)){tbl %<>% mutate(xxx = fun(xxx))}
  tbl %>% sdf_describe %>% collect -> dsc

  mn = dsc[4, 'xxx'] %>% as.numeric
  mx = dsc[5, 'xxx'] %>% as.numeric
  hh = mx - mn

  tbl %>% mutate(xx = as.integer(breaks*(xxx - mn)/hh)) %>% group_by(xx) %>%
    summarise(X = COUNT(xx), Y = sum(yy, na.rm = T)) %>%
    dbplyr::window_frame(-Inf, 0) %>% dbplyr::window_order(xx) %>%
    mutate(a = sum(X, na.rm = T), fn = sum(Y, na.rm = T)) %>%
    dbplyr::window_frame(1, Inf) %>%
    mutate(e = sum(X, na.rm = T), tp = sum(Y, na.rm = T)) %>%
    mutate(tn = a - fn, fp = e - tp) %>%
    mutate(precision = tp/(tp + fp), recall = tp/(tp + fn)) %>%
    mutate(f1 = 2*precision*recall/(precision + recall)) %>%
    mutate(split = mn + xx*hh/breaks) %>%
    dplyr::select(split, tp, fp, tn, fn, precision, recall, f1)
}

#' @export
optSplitColumns.f1 = function(df, columns = NULL, label_col = 'label', fun = NULL){
  defcols = numerics(df) %-% label_col
  columns = verify(columns, 'character', domain = defcols, default = defcols)
  for(i in sequence(length(columns))){
    col = columns[i]
    res1 <- df %>% optSplit.f1(prob_col = col, label_col = label_col, fun = fun)
    res2 <- df %>% spark.mutate('-' %>% paste(col) %>% {names(.)<-col;.}) %>% optSplit.f1(prob_col = col, label_col = label_col, fun = fun)
    res2$split = - res2$split
    res  <- chif(res1$f1 > res2$f1, res1, res2)
    res  <- c(Column = col, res) %>% as.data.frame
    if(i == 1){sp = res} else {sp %<>% rbind(res)}
    print(res)
  }
  return(sp)
}

#' @export
optSplitColumns.chi = function(df, columns = numerics(df), label_col = 'label', fun = NULL){
  columns %<>% setdiff(label_col)
  for(i in sequence(length(columns))){
    col = columns[i]
    if(length(unique(df[,col])) > 1){
      res <- df %>% optSplit.chi(num_col = col, cat_col = label_col, fun = fun)
      res <- c(Column = col, res) %>% as.data.frame
      if(i == 1){sp = res} else {sp %<>% rbind(res)}
      print(res)
    }
  }
  return(sp)
}


#' @export
spark.optSplitColumns.chi = function(tbl, columns, label_col = 'label', fun = NULL){
  for(i in sequence(length(columns))){
    col = columns[i]
    res <- tbl %>% spark.optSplit.chi(num_col = col, cat_col = label_col, fun = fun)
    res <- c(Column = col, res) %>% as.data.frame
    if(i == 1){sp = res} else {sp %<>% rbind(res)}
    print(res)
  }
  return(sp)
}

#' @export
spark.optSplitColumns.f1 = function(tbl, columns, label_col = 'label'){
  for(i in sequence(length(columns))){
    col = columns[i]
    res1 <- tbl %>% spark.optSplit.f1(prob_col = col, label_col = label_col, fun = fun)
    res2 <- tbl %>% spark.mutate('-' %>% paste(col) %>% {names(.)<-col;.}) %>% spark.optSplit.f1(prob_col = col, label_col = label_col, fun = fun)
    res2$split = - res2$split
    res  <- chif(res1$f1 > res2$f1, res1, res2)
    res  <- c(Column = col, res) %>% as.data.frame
    if(i == 1){sp = res} else {sp %<>% rbind(res)}
    print(res)
  }
  return(sp)
}

# prob_col : column name containing probabilities of class 1 or positive
# label_col: column name containing actual class labels
# dte stands for decision threshold evaluator:
# Returns a table containing performance metrics for each decision threshoild
#' @export
dte = function(df, prob_col, label_col, breaks = 1000, fun = NULL){
  df %<>% rename(xxx = prob_col, yy = label_col) %>% dplyr::select(xxx, yy)

  if(!is.null(fun)){df %<>% mutate(xxx = fun(xxx))}

  mn = df %>% pull(xxx) %>% min(na.rm = T)
  mx = df %>% pull(xxx) %>% max(na.rm = T)
  hh = mx - mn

  df %>% mutate(xx = as.integer(breaks*(xxx - mn)/hh)) %>% group_by(xx) %>%
    summarise(X = length(xx), Y = sum(yy, na.rm = T)) %>%
    arrange(xx) %>%
    mutate(a = cumsum(X), fn = cumsum(Y)) %>%
    mutate(e = cumsum(X %>% rev) %>% rev, tp = cumsum(Y %>% rev) %>% rev) %>%
    mutate(tn = a - fn, fp = e - tp) %>%
    mutate(precision = tp/(tp + fp), recall = tp/(tp + fn)) %>%
    mutate(f1 = 2*precision*recall/(precision + recall)) %>%
    mutate(split = mn + xx*hh/breaks) %>%
    mutate(ratio_right = tp/(tp + fp), ratio_left = fn/(tn + fn)) %>% 
    dplyr::select(split, tp, fp, tn, fn, precision, recall, f1)
}

#' @export
optSplit.f1 = function(df, prob_col, label_col, breaks = 1000, fun = NULL){
  df %>% dte(prob_col, label_col, breaks, fun = fun) %>% arrange(desc(f1)) %>%
    head(1) %>% as.list
}

# finds the optimal decision threshold to maximize f1 score
#' @export
spark.optSplit.f1 = function(tbl, prob_col, label_col, breaks = 1000, fun = NULL){
  tbl %>% spark.dte(prob_col, label_col, breaks, fun = fun) %>% arrange(desc(f1)) %>%
    head(1) %>% collect %>% as.list
}

#' @export
spark.scorer = function(tbl, prediction_col, actual_col){
  tbl %>% rename(x = prediction_col, y = actual_col) %>%
    group_by(x,y) %>% summarise(count = COUNT(x)) %>% collect -> scores
  TP = scores %>% filter(x, y)   %>% pull('count')
  FN = scores %>% filter(!x, y)  %>% pull('count')
  FP = scores %>% filter(x, !y)  %>% pull('count')
  TN = scores %>% filter(!x, !y) %>% pull('count')
  prc = TP/(TP + FP)
  rcl = TP/(TP + FN)
  list(
    precision = prc,
    recall    = rcl,
    accuracy  = (TP+TN)/(TP+FN+FP+TN),
    f1        = 2*prc*rcl/(prc+rcl)
  )
}


all_binary_predictive_scores = c('tp', 'fn', 'fp', 'tn', 'tpr', 'fnr', 'fpr', 'tnr', 'ppv', 'fdr', 'npv', 'pt', 'ts', 'csi', 'ba',
'recall', 'sensitivity', 'hit_rate', 'miss_rate', 'f1',
'specificity', 'selectivity', 'fall_out', 'precision', 'accuracy',
'fmi', 'informedness', 'markedness', 'mcc', 'for', 'lift')

#' Computes various predictive performance scores between two binary columns of a table
#' 
#' @field df \code{data.frame, tibble, data.table} or \code{matrix}: Input table containing at least two columns with binary data
#' @field prediction_col \code{character} header of the column containing predicted binary classes
#' @field actual_col \code{character} header of the column containing actual binary classes
#' @return \code{list} containing various scores. Returned scores are:
#' tp (True Positive), tn (True Negative), fp (False Positive), fn (False Negative),
#' tpr (True Positive Rate) or recall ro sensitivity or hit_rate,
#' fnr (False Negative Rate) or miss_rate,
#' tnr (True Negative Rate) or specificity or selectivity
#' fpr (False Positive Rate) or fall_out,
#' ppv (Positive Predictive Value) or precision
#' fdr (False Discovery Rate),
#' for (False Omission Rate),
#' pt (Prevalence Threshold),
#' ts (Threat Score) or csi (Critical Success Index)
#' accuracy, ba (Balanced Accuracy)
#' f1 (F1 Score), fmi (Fowlkes–Mallows Index),
#' informedness, markedness
#' mcc (Matthews Correlation Coefficient)
#' lift
#' @export
scorer = function(df, prediction_col, actual_col){
  df %>% as.data.frame %>% 
    rename(x = prediction_col, y = actual_col) %>%
    group_by(x,y) %>% summarise(count = length(x)) %>% ungroup %>% 
    mutate(x = as.logical(x), y = as.logical(y)) -> scores

  TP = scores %>% filter(x, y)   %>% pull('count')
  FN = scores %>% filter(!x, y)  %>% pull('count')
  FP = scores %>% filter(x, !y)  %>% pull('count')
  TN = scores %>% filter(!x, !y) %>% pull('count')
  
  if(is.empty(TP)) TP = 0
  if(is.empty(FN)) FN = 0
  if(is.empty(FP)) FP = 0
  if(is.empty(TN)) TN = 0
  
  TPR = TP/(TP + FN)
  TNR = TN/(TN + FP)
  PPV = TP/(TP + FP)
  NPV = TN/(TN + FN)
  # FDR: False Discovery Rate
  # NPV: Negative Predictive Value
  # FOR: False Omission Rate
  # PT:  Prevalence Threshold,
  PT  = (sqrt(TPR*(1.0 - TNR)) + TNR - 1.0)/(TPR + TNR - 1.0)
  # TS: Threat Score
  # CSI: Critical Success Index
  TS  = TP/(TP + FN + FP)
  
  out = list(
    tp  = TP, fn  = FN, fp = FP, tn = TN, 
    tpr = TPR, recall = TPR, sensitivity = TPR, hit_rate = TPR, fnr = 1.0 - TPR, miss_rate = 1.0 - TPR,
    tnr = TNR, specificity = TNR, selectivity = TNR, fpr = 1.0 - TNR, fall_out = 1.0 - TNR,
    ppv = PPV, precision = PPV, fdr = 1.0 - PPV,
    npv = NPV, 
    pt  = PT, 
    ts  = TS, csi = TS,
    accuracy  = (TP+TN)/(TP+FN+FP+TN),
    ba = (TPR + TNR)/2, # balanced accuracy (BA)
    f1 = 2*PPV*TPR/(PPV+TPR),
    fmi = sqrt(PPV*TPR), # Fowlkes–Mallows index
    informedness = TPR + TNR - 1.0,
    markedness = PPV + NPV - 1.0,
    # Matthews correlation coefficient
    mcc = (TP*TN - FP*FN)/(sqrt(TP + FP)*sqrt(TP + FN)*sqrt(TN + FP)*sqrt(TN + FN))
  )
  out[['for']]  <- 1.0 - NPV
  out[['lift']] <- PPV/mean(df[[actual_col]], na.rm = T)
  return(out)
}


#' Computes actual performance scores assuming the test set is down-sampled for class 0, by 'weight' times
#' The precision is expected to be lower than scorer, but recall remains the same.
scorer_downsampled = function(tbl, prediction_col, actual_col, weight = 2){
  tbl %>% rename(x = prediction_col, y = actual_col) %>%
    group_by(x,y) %>% summarise(count = length(x)) -> scores

  TP = scores %>% filter(x, y)   %>% pull('count')
  FN = scores %>% filter(!x, y)  %>% pull('count')
  FP = scores %>% filter(x, !y)  %>% pull('count') %>% {weight*.}
  TN = scores %>% filter(!x, !y) %>% pull('count') %>% {weight*.}

  prc = TP/(TP + FP)
  rcl = TP/(TP + FN)
  list(
    precision = prc,
    recall    = rcl,
    accuracy  = (TP+TN)/(TP+FN+FP+TN),
    f1        = 2*prc*rcl/(prc+rcl)
  )
}

#' @export
remove_invariant_features = function(X){
  fsds = X %>% apply(2, function(x) length(unique(x)))
  X[, which(fsds > 1), drop = F]
}


# Internal function used by grouper module
get_map = function(X, y, source, target, encoding = 'target_ratio'){
  mu = mean(y)
  if(encoding == 'target_ratio'){
    suppressWarnings({X %>% cbind(label = y) %>% group_by_(source) %>% summarise(cnt  = length(label), ratio = mean(label), suml = sum(label)) %>% arrange(ratio) -> tbl})
  } else if (encoding == 'flasso'){
    fl = CLS.FLASSO(lambda1 = 5, lambda2 = 1, transformers = DUMMIFIER(name = 'D', features.include = source))
    fl$fit(X, y)
    fl$objects$features$fname %>%
      gsub(pattern = paste0('D_', source, '_'), replacement = '') %>%
      data.frame(fl$objects$model$coefficient) %>%
      {names(.) <- c(source, 'ratio');.} -> tbl
  } else stop('Unknown encoding ', encoding)

  nr = tbl %>% pull(ratio) %>% unique %>% length
  elbow(tbl['ratio'], max.num.clusters = nr) -> res
  logpval = c()

  for(clust in res$clst){
    nc = max(clust$cluster)
    tbl[, 'cluster'] = clust$cluster

    contingency = tbl %>% group_by(cluster) %>% summarise(total = sum(cnt), positive = sum(suml)) %>% mutate(negative = total - positive)
    last_row    = contingency %>% colSums
    observed    = contingency %>% select(positive, negative, cluster) %>% column2Rownames('cluster') %>% as.matrix
    expected    = contingency$total %>% matrix(nrow = nc) %*% last_row[c('positive', 'negative')] %>% {./last_row['total']}
    logpval     %<>%  c(pchisq(sum((observed - expected)^2/expected), df = nc - 1, lower.tail = FALSE, log.p = T))
  }

  res$bnc = logpval %>% order %>% first

  tbl[, target] = res$clst[[res$bnc]]$cluster

  return(tbl[, c(source, target)])
}



# Internal function used by grouper module
apply_map = function(X, mapping){
  X %>% left_join(mapping, by = colnames(mapping)[1])
}

# Internal function
concat_columns = function(X, sources, target){
  scr = paste0("X %>% mutate(", target, " = paste(", sources %>% paste(collapse = ','), ", sep = '-'))")
  parse(text = scr) %>% eval
}

# Internal function used by grouper module
fit_map = function(X, y, cats, encoding = 'target_ratio'){
  allmaps  = list()
  scores   = get_chisq_scores(X, y, cats)
  cats     = cats[order(scores)]
  map_base = get_map(X, y, source = cats[1], target = 'M0', encoding = encoding)
  X = apply_map(X, map_base)
  allmaps[[cats[1]]] <- map_base
  columns = cats[-1]
  benchmark = Inf; iii = 1
  for(i in sequence(length(columns))){
    col   = columns[i]
    XT    = concat_columns(X, sources = c('M' %>% paste0(iii - 1), col), target = 'C' %>% paste0(iii))
    mapi  = get_map(XT, y, source = 'C' %>% paste0(iii), target = 'M' %>% paste0(iii), encoding = encoding)
    XT    = apply_map(XT, mapi)
    fval  = suppressWarnings({chisq.test(XT %>% pull('M' %>% paste0(iii)), y)})
    fval  = fval$statistic %>% pchisq(df = fval$parameter['df'], lower.tail = F, log.p = T)

    if(fval < benchmark){
      allmaps[[col]] = mapi
      X = XT
      benchmark = fval
      iii   = iii + 1
    }
  }
  return(allmaps)
}


# Internal function
get_chisq_scores = function(X, y, cats){
  scores = c()
  for(col in cats){
    colv  = X %>% pull(col)
    if((length(unique(colv)) < 2) | length(unique(y)) < 2){
      fval = 0
    } else {
      fval  = suppressWarnings({chisq.test(colv, y)})
      fval  = fval$statistic %>% pchisq(df = fval$parameter['df'], lower.tail = F, log.p = T)
    }
    scores = c(scores, fval)
  }
  return(scores)
}

# Internal function used by grouper module
fit_map_new = function(X, y, cats, encoding = 'target_ratio'){
  allmaps  = list()
  scores   = get_chisq_scores(X, y, cats)
  cats     = cats[order(scores)]
  map_base = get_map(X, y, source = cats[1], target = 'M0', encoding = encoding)
  X = apply_map(X, map_base)
  allmaps[[cats[1]]] <- map_base
  columns = cats[-1]
  benchmark = Inf; iii = 1
  for(col in columns){
    XT    = concat_columns(X, sources = c('M' %>% paste0(iii - 1), col), target = 'C' %>% paste0(iii))
    ind1  = XT %>% nrow %>% sequence %>% sample(floor(0.5*nrow(XT)))
    ind2  = XT %>% nrow %>% sequence %>% setdiff(ind1)
    X1    = XT[ind1,]; X2 = XT[ind2,]; y1 = y[ind1]; y2 = y[ind2]
    mapi  = get_map(X1, y1, source = 'C' %>% paste0(iii), target = 'M' %>% paste0(iii), encoding = encoding)
    X1    = apply_map(X1, mapi)
    X2    = apply_map(X2, mapi)
    p1    = get_chisq_scores(X1, y1, 'M' %>% paste0(iii))
    p2    = get_chisq_scores(X2, y2, 'M' %>% paste0(iii))

    if((p1 < benchmark) & (p2 < benchmark)){
      allmaps[[col]] = mapi
      X = apply_map(XT, mapi)
      benchmark = max(p1, p2)
      iii   = iii + 1
    }
  }
  return(allmaps)
}

# Internal function used by grouper module
predict_map = function(X, maplist){
  if(inherits(maplist, 'character')){
    return(X[maplist])
  }
  columns = names(maplist)
  nmap    = length(maplist)
  for(i in sequence(nmap)){
    map    = maplist[[i]]
    col    = columns[i]
    ns     = colnames(map)
    source = ns[1]
    target = ns[2]
    X      = apply_map(X, map)
    if(i < nmap){
      X    = concat_columns(X, sources = c(target, columns[i+1]), target = colnames(maplist[[i+1]])[1])
    }
  }
  return(X %>% pull(target))
}

predict_glm_fit <- function(glmfit, newmatrix, addintercept=TRUE){
  newmatrix %<>% as.matrix
  if (addintercept)
    newmatrix <- cbind(1,newmatrix)
  eta <- newmatrix %*% glmfit$coef
  glmfit$family$linkinv(eta)
}


# First all the raw data are read from csv file and then
# all combinations of figures are
# function evaluate is modified. It gets the raw data and a list of column numbers as input

# File: init.R must be in the working directory

# Internal function: previously called evaluate
sfs.regression <- function (D, tt_ratio = 0.7, yfun = function(x){x}, yfun.inv = yfun) {
  if(!inherits(D, 'matrix')) D %<>% as.matrix

  N = dim(D)[1]
  m = dim(D)[2]

  prt = D %>% partition(tt_ratio)

  X = prt$part1[, 1:(m - 1)]
  y = prt$part1[, m] %>% yfun

  Xt = prt$part2[, 1:(m - 1)]
  yt = prt$part2[, m] %>% yfun

  # X = scale(X, center = FALSE)
  # sorting the predictors based on R squared in a linear regression with single regressor
  # Number of predictors: m-1

  ter = c()
  bstmdl = NULL

  CC  = cor(x = X, y = y, method = "pearson") %>% na2zero %>% abs
  index = order(CC, decreasing=TRUE)
  CC = CC[index]
  index = index[CC > 0.1]
  if(is.empty(index)){return(NULL)}

  X   = X[,index]
  Xt  = Xt[,index]

  # Construct the initial model using the best predictor
  A  = X[,1]
  At = Xt[,1, drop = F]

  fig.index = index[1]
  # Set zero as the initial value of r adjusted

  # reg = glm.fit(x = A, y = y)
  reg = glm(y ~ A)
  rss = sum(reg$residuals^2)
  dfr = length(reg$coefficients)
  # prediction accuracy with test data:
  prd = reg %>% predict_glm_fit(At, addintercept = !(ncol(At) %>% equals(reg$coefficients %>% length)))
  pss = sum(((prd[,1] %>% yfun.inv) - (yt %>% yfun.inv))^2)

  for (i in sequence(index %>% length) %-% 1){
    # Add predictor to the model
    A_new  = cbind(A, X[,i])
    At_new = cbind(At, Xt[,i, drop = F])
    colnames(At_new)  <- c(colnames(At), colnames(X)[i])
    colnames(A_new) <- colnames(At_new)
    # Run the regression
    # reg     = try(glm.fit(y = y, x = A_new), silent = T)
    reg     = try(glm(y ~ A_new), silent = T)

    if(!inherits(reg, 'try-error')){
      prd_new = reg %>% predict_glm_fit(At_new)
      pss_new = sum(((prd_new[,1] %>% yfun.inv) - (yt %>% yfun.inv))^2)
      # If successful, replace it with the new model
      permit = pss_new < pss
      if(is.na(permit)){permit = F}

      if(permit){
        sum.reg = summary(reg)
        dftest  = nrow(At_new) - dfr
        ft      = dftest*(pss - pss_new)/pss_new
        pvlt    = pf(ft, 1, dftest, lower.tail = F)
        ter_new = sqrt(pss_new/length(yt))
        rss_new = sum(reg$residuals^2)
        fstats  = ((rss - rss_new)*reg$df.residual)/(rss_new)
        pvl     = pf(fstats, 1, reg$df.residual, lower.tail = F)
        pvls    = sum.reg$coefficients[-1, "Pr(>|t|)"]
        det     = det(t(A_new) %*% A_new)
        permit  = !equals(det, 0) & (sum(pvls > 0.05, na.rm = T) %>% equals(0)) & (pvl < 0.05) & (pvlt < 0.05)

        if(is.na(permit)){permit = F}
      }

      if (permit){
        cat("Det            = ", det, "\n")
        cat("Test Error     = ", ter_new, "\n")
        cat("F Statistics   = ", fstats, "\n")
        cat("P-Value        = ", pvl, "\n \n")
        A   = A_new
        At  = At_new
        rss = rss_new
        pss = pss_new
        fig.index = c(fig.index, index[i])
        bstmdl  = reg
        names(bstmdl$coefficients)[-1] = colnames(A)
        fig.names = colnames(A)
        ter = c(ter, ter_new)
      }
    }
  }

  output = list(sig.feature.values = A, sig.feature.indexes = fig.index, sig.feature.names = fig.names,test.error = ter, model = bstmdl)
  return(output)
}

#' @export
model_save = function(model, path = getwd()){
  if(!file.exists(path)) {dir.create(path)}
  path %<>% paste(model$name, sep = '/')
  model$model.save(path)
  model %>% saveRDS(path %>% paste0('/', model$name, '.rds'))
}

#' @export
model_load = function(model_name, path = getwd(), update = F){
  path %<>% paste(model_name, sep = '/')
  assert(file.exists(path), paste0('No folder named as ', model_name, ' found in the given path!'))
  model = readRDS(path %>% paste0('/', model_name, '.rds'))
  if(update) model %<>% model_update
  model$model.load(path)
  return(model)
}

#' @export
model_reset = function(model, reset_transformers = T, reset_gradient_transformers = T){
  model$fitted <- FALSE
  model$objects$features <- NULL
  model$objects$saved_pred <- NULL
  if (reset_transformers & !is.empty(model$transformers)){
    for (transformer in model$transformers) model_reset(model = transformer, reset_transformers = T, reset_gradient_transformers = reset_gradient_transformers)
  }
  if (reset_gradient_transformers & !is.empty(model$gradient_transformers)){
    for (transformer in model$gradient_transformers) model_reset(model = transformer, reset_transformers = reset_transformers, reset_gradient_transformers = T)
  }
}

#' @export
model_size = function(model){
  t_size = list(config = 0, objects = 0, transformers = 0)
  for(tr in model$transformers){
    slist  = model_size(tr)
    t_size$config  = t_size$config  + slist$config
    t_size$objects = t_size$objects + slist$objects
    t_size$transformers = t_size$transformers + slist$transformers
  }
  list(config  = object.size(model$config),
       objects = object.size(model$objects),
       transformers = object.size(model$transformers) + t_size$config + t_size$objects + t_size$transformers)
}

#' @export
model_features = function(model){
  if(model$fitted){
    fet = model$objects$features$fname
  } else if (!is.null(model$config$features.include)){
    fet = model$config$features.include
  } else {
    fet = character()
  }
  for(tr in model$transformers){
    fet  = c(fet, model_features(tr))
  }
  for(tr in model$gradient_transformers){
    fet  = c(fet, model_features(tr))
  }
  return(fet %>% unique)
}

#' @export
model_update = function(model){
  
  mcopy = new(class(model)[1], config = model$config)
  
  mcopy$name <- model$name
  mcopy$type <- model$type
  mcopy$package <- model$package 
  mcopy$fitted  <- model$fitted
  mcopy$description      <- model$description
  mcopy$package_language <- model$package_language 

  mcopy$objects <- model$objects

  for (i in sequence(length(model$transformers))){
    mcopy$transformers[[i]] <- model_update(model$transformers[[i]])
  }
  
  for (i in sequence(length(model$gradient_transformers))){
    mcopy$gradient_transformers[[i]] <- model_update(model$gradient_transformers[[i]])
  }
  
  for (i in sequence(length(model$yin_transformers))){
    mcopy$yin_transformers[[i]] <- model$yin_transformers[[i]]
  }
  
  for (i in sequence(length(model$yout_transformers))){
    mcopy$yout_transformers[[i]] <- model$yout_transformers[[i]]
  }
  
  return(mcopy)
}

#' Changes duplicated transformer names of a given model to ensure all transformers have distinct names 
#' 
#' @field model Any object inheriting from class \code{MODEL} 
#' @return None
#' @export 
model_make_unique_transformer_names = function(model){
  model$transformers %>% lapply(function(x) x$name) %>% unlist -> tnames
  wdp = which(duplicated(tnames))
  while(length(wdp) > 0){
    for(i in wdp){
      model$transformers[[i]]$name <- model$transformers[[i]]$name %>% paste(i, sep = '_')
    }
    model$transformers %>% lapply(function(x) x$name) %>% unlist -> tnames
    wdp = which(duplicated(tnames))
  }
}

#' Returns model type and types of its transformers recursively
#' 
#' @field model Any object inheriting from class \code{MODEL} 
#' @return \code{character} vector containing model type and all its transformer types

#' @export
model_types = function(model){
  mdlns = model$type
  for(tr in model$transformers){
    mdlns = c(mdlns, transformer_types(tr))
  }
  return(mdlns %>% unique)
}

# converts given categorical columns to integer
# take to gener
int_columns = function(x, cats){
  X %<>% as.data.frame
  for(i in intersect(cats, colnames(x))){
    X[, i] <- X[, i] %>% as.integer
  }
  return(x)
}

# converts all columns to numeric
# take to gener
numerize_columns = function(x){
  X %<>% as.data.frame
  for(i in colnames(x))
    X[,i] <- as.numeric(x[,i])
  return(x)
}

# todo: make it parallel
# should be tested again in order to export
# Each feature has only one chance to participate in an experiment
feature_subset_scorer_fast = function(model, X, y, subset_size = 600){
  features = data.frame(fname = colnames(X), model_number = NA, importance = NA, performance = NA, score = NA, stringsAsFactors = F) %>% column2Rownames('fname')
  nf       = nrow(features); cnt = 0
  tempfeat = rownames(features)
  while(nf > 0){
    cnt      = cnt + 1
    sfet     = tempfeat %>% sample(min(nf, subset_size))
    tempfeat = tempfeat %-% sfet
    nf       = length(tempfeat)

    perf = model$performance.cv(X[sfet], y) %>% median

    features[model$objects$features$fname, 'importance']   <- model$objects$features$importance
    features[model$objects$features$fname, 'model_number'] <- cnt
    features[model$objects$features$fname, 'performance']  <- perf
    features[model$objects$features$fname, 'score']        <- perf*vect.normalise(model$objects$features$importance)

    cat('\n Features scored: ', ' Performance: ', perf, ' Remaining: ', nf)
  }

  return(features)
}

# Given a set of train and test data and labels, extracts list of informative features
# by training multiple xgboost model on random subsets of features and eliminating features that their total predictive value is
# less than a certain percentage (specified by argument 'cumulative_gain_threshold') of total predictive value.
# should be tested again in order to export
extract_informative_features = function(X, y, subset_size = 200, cumulative_gain_threshold = 0.9, pre_order = T){
  if(pre_order){
    ef = evaluate_features(X, y, metrics = 'gini')
    remaining_features = rownames(ef)[order(ef$gini, decreasing = T)]
  } else {remaining_features = colnames(X)}
  ifet = NULL
  while(length(remaining_features) > 0){
    nfet = min(subset_size, length(remaining_features))
    if(pre_order){
      fet   = remaining_features %>% head(n = nfet)
    } else {
      fet   = remaining_features %>% sample(size = nfet)
    }
    model = CLS.SKLEARN.XGB(n_jobs = as.integer(4))
    model$fit(X[fet], y)
    fetlog = model$objects$features %>% arrange(desc(importance))
    fetlog$importance %>% sort(decreasing = T) %>% cumsum -> cumgain
    ifet = rbind(ifet, fetlog[cumgain < cumulative_gain_threshold,])
    ifet$model <- model$name
    remaining_features %<>% setdiff(fet)
  }

  return(ifet)
}



#' Returns desired moments of a given numeric vector
#' 
#' @field v \code{numeric} input vector for which moments are to be computed
#' @field m \code{integer} desired moment orders
#' @field na.rm \code{logical} should missing values be removed?
#' @return \code{numeric} output vector containing moment values
#' @export 
moment = function(v, m = c(1,2), na.rm = T){
  if(length(m) == 1) return(sum(v^m))
  M = NULL
  for(i in m){M %<>% c(sum(v^i, na.rm = na.rm))}
  names(M) <- paste0('M', m)
  return(M)
}

# Internal function used in fit method of abstract class MODEL
feature_info_numeric = function(X, probs = seq(0, 1, 0.25)){
  X = X[numerics(X)]
  if(inherits(X, 'WIDETABLE')){
    tables = X$meta %>% filter(class %in% c('numeric', 'integer')) %>% pull(table) %>% unique
    out    = NULL
    for(tn in tables){
      out = X$load_table(tn) %>% feature_info_numeric(probs = probs) %>% rbind(out)
    }
    return(out)
  } else if(inherits(X, c('data.frame', 'matrix', 'tibble', 'data.table'))){
    X %>% as.matrix %>% apply(2, moment, m = 1:4) %>% t %>%
      cbind(
        X %>% as.matrix %>% apply(2, function(v) {c(n_missing = sum(is.na(v)), n_values = sum(!is.na(v)), n_unique = length(unique(v)))}) %>% t) %>%
      cbind(
        X %>% as.matrix %>% apply(2, quantile, probs = probs, na.rm = T) %>% t) %>%
      as.data.frame %>% rownames2Column('fname')
  } else stop('X has unknown class!')
}

# Internal function used in fit method of abstract class MODEL
feature_info_categorical = function(X){
  X = X[nominals(X)]
  if(inherits(X, 'WIDETABLE')){
    tables = X$meta %>% filter(class %in% c('character', 'factor', 'integer')) %>% pull(table) %>% unique
    out    = NULL
    for(tn in tables){
      out = X$load_table(tn) %>% feature_info_categorical %>% rbind(out)
    }
    return(out)

  } else if(inherits(X, c('data.frame', 'matrix', 'tibble', 'data.table'))){
    X[nominals(X)] %>% as.matrix %>% apply(2, function(v) {c(n_missing = sum(is.na(v)), n_values = sum(!is.na(v)), n_unique = length(unique(v)))}) %>%
      t %>% as.data.frame %>%
      cbind(
        values = X %>% as.matrix %>% apply(2, function(v) {
          vu = unique(v)
          if(length(vu) > 4) vu = c(as.character(vu[1:3]), ' ... ', vu[length(vu)])
          paste(vu, collapse = ',')
        })) %>%
      rownames2Column('fname')
  } else stop('X has unknown class!')
}


#' @export 
encode_nominals = function(X, encodings = list(), remove_space = T){
  for(ft in names(encodings) %^% colnames(X)){
    u = encodings[[ft]] %>% unlist
    X[,ft] <- u[X[,ft] %>% as.character]
    if(remove_space){
      X[,ft] %<>% gsub(pattern = "\\s", replacement = "")
    }
  }

  return(X)
}

# This function may be transferred to gener
# dividing x/y sometimes returns Inf when the denominator is zero.
# smart divide, replaces zero denominators with the meinimum non-zero value multiplied by a gain
# This gain should be a value smaller than one. Default is 0.1
# internal function
#' @export 
smart_divide = function(x, y, gain = 0.1, tolerance = .Machine$double.eps){
  w0 = which(equal(y, 0.0, tolerance = tolerance))
  if(length(w0) > 0){
    min_value = min(abs(y[-w0]))
    if(length(min_value) > 0){
      y[w0] <- ifelse(y[w0] >= 0, min_value*gain, - min_value*gain)
    }
  }
  return(x/y)
}

# Given two models A and B, if we want to have an ensemble model,
# one way is to take a number of cases from top probabilities of model A and a different number from top list of model B,
# then merge the two lists and present them as positive cases.
# Question is how many of each model shpuld we take?
# This function returns the lift performance of the ensemble model if r_A percentage of selected cases are taken from
# model A and the rest taken from model B.
# Inputs:
# r   : percentage of entire number of rows (cases) for which lift is being measured
# r_A : percentage of selected cases taken from model A
# y   : labels
# yh_A: probs of model A
# yh_B: probs of model B
ensemble_lift = function(y, yh_A, yh_B, r, r_A){
  N     = length(y)
  assert((N == length(yh_A)) & (N == length(yh_B)), 'length of vectors must be the same!')
  tr_A  = quantile(yh_A, probs = 1.0 - r_A*r)
  sel_A = which(yh_A > tr_A)
  n_req = as.integer(r*N)
  sel_B = order(yh_B) %>% setdiff(sel_A) %>% tail(n_req - length(sel_A))
  mean(y[c(sel_A, sel_B)])/mean(y)
}

# This function cuts the given dataframe into buckets according to the columns in `variable_set_1`
# and gives the bucket sum and cumulative sum of `variable_set_2` as well as 
# count and cumulative count of each bucket.
# Example:
# df = data.frame(Height = rnorm(10000, 165, 20), 
#                 Age = sample(15:60, size = 10000, replace = T), 
#                 Weight = rnorm(10000, 70, 20))
# bucket_moments(df, 'Age', c('Height', 'Weight'), ncuts = 10)
# All variables must be numeric
# todo: Add moments of higher degrees like: sum of squares, sum of cubes, degree 4, 5, ...
#' @export 
bucket_moments = function(df, variable_set_1, variable_set_2, ncuts = 100){
  out   = NULL
  for(f1 in variable_set_1){
    for(f2 in variable_set_2){
      tab = df[c(f1, f2)]
      colnames(tab) <- c('F1', 'F2')
      tab %>% mutate(QNT = cut(F1, breaks = quantile(F1, probs = seq(0, 1, 1.0/ncuts)) %>% unique)) %>% 
        group_by(QNT) %>% summarise(F1 = max(F1), M1_F2 = sum(F2), CNT_F2 = length(F2)) %>% 
        arrange(F1) %>% mutate(M1_F2_CS = cumsum(M1_F2), CNT_F2_CS = cumsum(CNT_F2)) %>% 
        select(bucket = QNT, cutpoint_V1 = F1, bucket_sum_V2 = M1_F2, bucket_count = CNT_F2, cumulative_sum_V2 = M1_F2_CS, cumulative_count = CNT_F2_CS) -> tmp
      tmp$V1 = f1
      tmp$V2 = f2
      out %<>% rbind(tmp)
    }
  }
  return(out)
}


fit_models.old = function(models, X, y){
  for(i in names(models)){
    cat('Training model: ', i, ' ...')
    res = try(models[[i]]$fit(X, y), silent = T)
    cat('Done!', '\n')
    if(inherits(res, 'try-error')){
      cat('\n', 'Model ', i, ' training failed!', '\n', res %>% as.character, '\n')
    }
  }
  
  for(i in names(models)){
    if(!models[[i]]$fitted){
      models[[i]] <- NULL
    }
  }
  
  return(models)
}

#' Trains a given list of model objects and returns a trained list of models
#' @export
fit_models = function(models, X, y, num_cores = 1, verbose = 1, remove_failed_models = T){
  # if(is.null(names(models))){names(models) <- paste0('M', sequence(length(models)))}
  num_cores = verify(num_cores, c('numeric', 'integer'), lengths = 1, domain = c(1, parallel::detectCores()), default = 1)
  
  uft = models %>% lapply(function(x) {!x$fitted}) %>% unlist %>% which
  if((num_cores > 1) & (length(uft) > 1)){
    requirements = models %>% lapply(function(x) x$packages_required) %>% unlist %>% unique
    cl = rutils::setup_multicore(n_jobs = num_cores)
    if(verbose > 0){cat('\n', 'Fitting  %s models ... ' %>% sprintf(length(uft)))}
    
    models <- foreach(model = models, .combine = c, .packages = requirements, .errorhandling = 'pass') %dopar% {
      # Save models temporarily before exiting the loop:
      res = try(model$fit(X, y), silent = T)
      if(inherits(res, 'try-error')){
        model$objects$fitting_error <- as.character(res)
      } else {
        model$keep_model()
      }
      
      gc()
      list(model)
    }
    for(i in sequence(length(models))){models[[i]]$retrieve_model()}
    stopCluster(cl)
    gc()
    if(verbose > 0){cat('Done!', '\n')}
  } else {
    for(i in uft){
      if(verbose > 0){
        cat('\n', 'Fitting model %s of type %s: %s ... ' %>% sprintf(models[[i]]$name, models[[i]]$type, models[[i]]$description))
      }
      res = try(models[[i]]$fit(X, y), silent = T)
      if(inherits(res, 'try-error')){
        models[[i]]$objects$fitting_error <- as.character(res)
        if(verbose > 0){cat('Failed!')}
      } else {
        if(verbose > 0){cat('Done!')}
      }
    }  
  }
  
  ## Remove unfitted (failed) models:
  if(remove_failed_models){
    fitted = models %>% lapply(function(x) {x$fitted}) %>% unlist %>% which
    failed = models %>% length %>% sequence %>% setdiff(fitted)
    
    nt = length(fitted)
    if(verbose > 0) {
      warnif(nt < length(models), sprintf("%s models failed to fit and removed!", length(models) - nt))
    }  
    models <- models %>% list.extract(fitted)
    assert(nt == length(models), "This must not happen!")
  }
  return(models)
}

#' Calls \code{predict} method of each model in the given list and retunrs results in a data.frame
#' @export
predict_models = function(models, X, num_cores = 1, verbose = 1){
  num_cores = verify(num_cores, c('numeric', 'integer'), lengths = 1, domain = c(1, parallel::detectCores()), default = 1)
  
  fitted = models %>% lapply(function(x) {x$fitted}) %>% unlist %>% which
  failed = models %>% length %>% sequence %>% setdiff(fitted)
  
  if(verbose > 0 & length(failed) > 0){
    print("%s models are not fitted! No columns will be generated from these models.")
  }
  
  models %<>% list.extract(fitted)
  nt = length(models)
  if((num_cores > 1) & (nt > 1)){
    requirements = models %>% lapply(function(x) x$packages_required) %>% unlist %>% unique
    cl = rutils::setup_multicore(n_jobs = num_cores)
    if(verbose > 0){cat('\n', 'Generating output from %s models ... ' %>% sprintf(nt))}
    for(i in sequence(length(models))){models[[i]]$keep_model()}
    XT = foreach(model = models, .combine = cbind, .packages = requirements, 
                 .errorhandling = 'remove') %dopar% {
                  gc()
                  model$retrieve_model() 
                  model$predict(X)
                 }
    stopCluster(cl)
    gc()
    for(i in sequence(length(models))){models[[i]]$release_model()}
    if(verbose > 0){
      cat('Done!', '\n')
    }
  } else {
    for(i in sequence(nt)){
      model = models[[i]]
      if(verbose > 0){
        cat('\n', 'Generate output column(s) from model %s ... ' %>% sprintf(model$name))
      }
      if(i == 1){
        XT = model$predict(X)
      } else {
        XT = cbind(XT, model$predict(X) %>% {.[colnames(.) %-% colnames(XT)]})
      }
      if(verbose > 0){
        cat('Done!', '\n')
      }
    }
  }
  
  return(XT)
}

# Runs cross validation for each model in the baglist:
#' @export
evaluate_models = function(models, X, y){
  for(i in names(models)){
    cat('Evaluating model: ', i, ' ...')
    res = try(models[[i]]$performance.cv(X, y), silent = T)
    cat('Done!', '\n')
    if(inherits(res, 'try-error')){
      models[[i]]$objects$performance.cv <- sprintf("Model %s evaluation failed! %s", model$name, res %>% as.character)
    } else {
      models[[i]]$objects$performance.cv <- res
    }
  }
  
  return(models)
}


#' @export
evaluate_models.multicore = function(models, X, y, n_jobs = parallel::detectCores() - 1){
  cl = rutils::setup_multicore(n_jobs = n_jobs)
  # library(doParallel)
  # cl = makeCluster(n_jobs)
  # registerDoParallel(cl)
  # actual_njobs = getDoParWorkers()
  # warnif(actual_njobs < n_jobs, 
  #        sprintf('Parallel run is working with %s cores rather than %s. It may take longer than expected!', actual_njobs, n_jobs))
  
  foreach(i = names(models), .combine = c, .packages = c('magrittr', 'dplyr','rutils', 'reticulate', 'rml', 'rbig', 'rfun'), .errorhandling = 'remove') %dopar% {
    model = models[[i]]
    res = try(model$performance.cv(X, y), silent = T)
    if(inherits(res, 'try-error')){
      model$objects$performance.cv = sprintf("Model %s evaluation failed! %s", model$name, res %>% as.character)
    } else {
      model$objects$performance.cv <- res
    }
    model
  }
  stopCluster(cl)
  gc()
}

#' @export
service_models = function(modlist, X_train, y_train, X_test, y_test, num_cores = 1, metrics = 'gini', quantiles = NULL, reported_parameters = character()){
  modlist %<>% fit_models(X = X_train, y = y_train, num_cores = num_cores, verbose = 1)
  names(modlist) <- modlist %>% rutils::list.pull('name')

  # keep return_type_in_output and name_in_output config properties
  # we need the column naes to be exactly the same as model names
  for(mdl in modlist){
    mdl$objects$return_type_in_output <- mdl$config$return_type_in_output
    mdl$objects$name_in_output <- mdl$config$name_in_output
    mdl$config$return_type_in_output <- FALSE
    mdl$config$name_in_output <- TRUE
  }
    
  yy = predict_models(modlist, X = X_test, num_cores = num_cores, verbose = 1)
  
  # retrieve return_type_in_output and name_in_output:
  for(mdl in modlist){
    mdl$config$return_type_in_output <- mdl$objects$return_type_in_output
    mdl$config$name_in_output <- mdl$objects$name_in_output
  }
  
  pf = correlation(yy, y_test, metrics = metrics, quantiles = quantiles)
  
  rutils::warnif(length(pf) < length(modlist), 'Some models failed to predict!')
  
  pfdf = pf %>% lapply(unlist) %>% purrr::reduce(rbind) %>% as.data.frame %>% {rownames(.)<-names(pf);.}
  print(pfdf)
  
  for(i in rownames(pfdf)){
    cfg = modlist[[i]]$config
    for(j in reported_parameters){
      pfdf[i, j] <- cfg[[j]]
    }
  }

  modlist %>% list.extract(rownames(pfdf)) %>% lapply(function(x) x$objects$features %>% dplyr::mutate(model = x$name, fitting_time = as.character(x$objects$fitting_time))) %>% 
    purrr::reduce(dplyr::bind_rows) %>% 
    dplyr::left_join(pfdf %>% rutils::rownames2Column('model')) -> ptab
  
  ord = order(ptab[[names(pf[[1]])[1]]], decreasing = T)[1]
  for(i in sequence(length(modlist))){modlist[[i]]$release_model()}
  return(list(best_model = modlist[[ptab$model[ord]]], best_performance = max(ptab[[names(pf[[1]])[1]]]), results = ptab))
}



column_rename = function(tbl, ...){
  ns = c(...) %>% verify('character')
  lb = names(ns)
  if(is.null(lb)){return(tbl)}
  scr = paste0("tbl %>% dplyr::rename(", lb %>% paste(ns, sep = ' = ') %>% paste(collapse = ' , '), ")")
  parse(text = scr) %>% eval
}

column_drop = function(tbl, ...){
  ns = c(...) %>% verify('character') %>% intersect(colnames(tbl))
  return(tbl[colnames(tbl) %>% setdiff(ns)])
}