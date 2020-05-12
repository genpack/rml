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

rmse = function(y1, y2){
  mean((y1 - y2)^2, na.rm = T) %>% sqrt
}

mae = function(y1, y2){
  (y1 - y2) %>% abs %>% mean(na.rm = T)
}

medae = function(y1, y2){
  (y1 - y2) %>% abs %>% median(na.rm = T)
}

#' @export
cross_f1 = function(v1, v2){
  v2 %<>% verify(c('logical', 'integer', 'numeric'), null_allowed = F) %>% as.logical
  N = length(v2)

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
logit_fwd = function(x) {
  x    = abs(x)
  mask = x < .Machine$double.eps
  x[mask] <- .Machine$double.eps
  mask = x > 1.0 - .Machine$double.eps
  x[mask] <- 1.0 - .Machine$double.eps
  log(x/(1-x))
}

# convert logit to probabilities
logit_inv = function(x){
  e = exp(x)
  return(1.0 - 1.0/(1 + e))
}

remove_outliers = function(X, sd_threshold = 3){
  m = ncol(X)
  Xs = X %>% scale %>% as.data.frame
  Xs$keep = T
  for(i in sequence(m)){
    tbd = (Xs[,i] < sd_threshold) & (Xs[,i] > - sd_threshold)
    if(sum(!tbd) > 0.1*length(tbd)){cat("-----",i, " ", sum(!tbd))}
    Xs$keep = Xs$keep & (Xs[,i] < sd_threshold) & (Xs[,i] > -sd_threshold)
  }
  X[Xs$keep,]
}

int_ordinals = function(X){
  X %<>% as.data.frame
  nms = colnames(X)
  for (col in nms){
    v = X %>% pull(col)
    if(inherits(v, c('numeric', 'integer', 'integer64'))){
      if(sum(as.integer(v) != v, na.rm = T) == 0) X[,col] %<>%  as.integer
    }
  }
  return(X)
}


add_keras_layer_dense = function(input_layer, layer, ...){
  input_layer %<>% layer_dense(units       = layer$units %>% verify(c('numeric', 'integer'), lengths = 1, null_allowed = F) %>% as.integer,
              activation  = layer$activation %>% verify('character', lengths = 1, default = 'relu'),
              use_bias    = layer$use_bias %>% verify('logical', lengths = 1, domain = c(T,F), default = T),
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


ranker = function(X){
  X %<>% as.data.frame
  for(col in colnames(X)){
    v = X %>% pull(col)
    if(inherits(v, 'numeric')){
      X[, col %>% paste('rank', sep = '_')] <-  order(v)
    }
  }
  return(X)
}


#' @export
outliers = function(X, sd_threshold = 4){
  if(inherits(X, 'numeric')){X = matrix(X, ncol = 1)}
  out = integer()

  for(i in ncol(X)){
    if(inherits(X[,i], 'numeric')){
      mu = mean(X[,i], na.rm = T)
      sg = sd(X[,i], na.rm = T)
      out = c(out, which(abs(X[,i] - mu) > sd_threshold*sg))
    }
  }
  return(unique(out))
}

trim_outliers = function(X, scale = F, center = F, only_numeric = F){
  m = ncol(X)
  for(i in sequence(m)){
    xi   = unique(X[,i])
    mni  = mean(xi)
    sdi  = mean((xi - mni)^2, na.rm = T) %>% sqrt
    isint = inherits(X[, i], 'integer')
    if(sdi > 0){
      xsi  = (X[, i] - mni)/sdi
      tbch = (xsi >  3)
      tbcl = (xsi < -3)
      xh   = mni + 3*sdi + abs(log(X[tbch, i] - mni - 3*sdi))
      xl   = mni - 3*sdi - abs(log(mni - X[tbcl, i] - 3*sdi))

      if(isint){
        if(!only_numeric){
          X[tbch, i] = as.integer(xh)
          X[tbcl, i] = as.integer(xl)
        }
      } else {
        X[tbch, i] = xh
        X[tbcl, i] = xl
      }
      if(scale) if(center) X[, i] = (X[, i] - mni)/sdi else X[, i] = X[, i]/sdi
    } else if(scale) if(center) X[, i] = 0 else X[, i] = 1
    # cat(colnames(X)[i], '\n')
  }
  X
}


na2median = function(X){
  X %<>% data.frame
  for(i in 1:ncol(X)) if(inherits(X[,i], 'numeric')) X[is.na(X[,i]), i] <- median(X[,i], na.rm = T)
  return(X)
}

correlation = function(x, y, metric = 'pearson_correlation', threshold = NULL, lift_ratio = NULL){
  x = as.numeric(x)
  y = as.numeric(y)
  if(metric == 'pearson_correlation'){
    return(cor(x, y))
  }

  ncl = ncol(x)
  if(!is.null(ncl)){
    crl = numeric()
    x %<>% as.data.frame
    for(col in sequence(ncl)){
      crl = c(crl, correlation(x[,col] %>% as.numeric, y, metric = metric, threshold = threshold, lift_ratio = lift_ratio))
    }
    return(crl)
  }

  x %<>% vect.map
  if(!is.null(threshold)) x = as.numeric(x > threshold)

  if(metric %in% c('aurc', 'gini')){
    aurc = AUC::auc(AUC::roc(x, y %>% as.factor))
    if(metric == 'aurc') return(aurc) else return(2*aurc - 1)
  }

  if(metric %in% c('precision', 'recall', 'f1', 'accuracy', 'sensitivity', 'specificity', 'fp', 'fn', 'tp', 'tn')){
    if(metric == 'sensitivity') metric = 'recall'
    if(unique(x) %==% c(0,1)){
      score = data.frame(y_pred = x, y_true = y) %>% scorer('y_pred', 'y_true')
    } else {
      score = data.frame(y_pred = x, y_true = y) %>% optSplit.f1('y_pred', 'y_true')
    }
    return(score[[metric]])
  }

  if(is.null(lift_ratio)){lift_ratio = mean(y, na.rm = T)}
  if(metric == 'lift'){
    cut_q = quantile(x, prob = 1 - lift_ratio)
    prec  = y[x >= cut_q] %>% mean(na.rm = T)
    return(prec/mean(y))
  }
  stop('Unknown metric' %>% paste(metric))
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
  #out$pvalue <- pchisq(out$chisq, df = 1, lower.tail = F)
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
  columns %<>% verify('character', domain = defcols, default = defcols)
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
    dplyr::select(split, tp, fp, tn, fn, precision, recall, f1)
}

#' @export
optSplit.f1 = function(df, prob_col, label_col, breaks = 1000, fun = NULL){
  df %>% dte(prob_col, label_col, breaks, fun = fun) %>% arrange(desc(f1)) %>%
    head(1) %>% collect %>% as.list
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

#' @export
scorer = function(tbl, prediction_col, actual_col){
  tbl %>% rename(x = prediction_col, y = actual_col) %>%
    group_by(x,y) %>% summarise(count = length(x)) -> scores

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


#' @export
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



apply_map = function(X, mapping){
  X %>% left_join(mapping, by = colnames(mapping)[1])
}

concat_columns = function(X, sources, target){
  scr = paste0("X %>% mutate(", target, " = paste(", sources %>% paste(collapse = ','), ", sep = '-'))")
  parse(text = scr) %>% eval
}

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

# previously called evaluate
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

save_model = function(model, path = getwd()){
  if(!file.exists(path)) {dir.create(path)}
  path %<>% paste(model$name, sep = '/')
  model$model.save(path)
  model %>% saveRDS(path %>% paste0('/', model$name, '.rds'))
}

load_model = function(model_name, path = getwd()){
  path %<>% paste(model_name, sep = '/')
  assert(file.exists(path), paste0('No folder named as ', model_name, ' found in the given path!'))
  model = readRDS(path %>% paste0('/', model_name, '.rds'))
  model$model.load(path)
  return(model)
}


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

#converts given categorical columns to integer
int_columns = function(x, cats){
  X %<>% as.data.frame
  for(i in intersect(cats, colnames(x))){
    X[, i] <- X[, i] %>% as.integer
  }
  return(x)
}

# converts all c9l urns to numeric
numerize_columns = function(x){
  X %<>% as.data.frame
  for(i in colnames(x))
    X[,i] <- as.numeric(x[,i])
  return(x)
}

# todo: make it parallel
feature_subset_scorer = function(model, X, y, subset_size = 600){
  features = data.frame(fname = colnames(X), model_number = NA, importance = NA, performance = NA, score = NA, stringsAsFactors = F) %>% column2Rownames('fname')
  nf       = nrow(features); cnt = 0
  tempfeat = rownames(features)
  while(nf > 0){
    cnt      = cnt + 1
    sfet     = tempfeat %>% sample(min(nf, subset_size))
    tempfeat = tempfeat %-% sfet
    nf       = length(tempfeat)

    perf = model$get.performance.cv(X[sfet], y)

    features[model$objects$features$fname, 'importance']   <- model$objects$features$importance
    features[model$objects$features$fname, 'model_number'] <- cnt
    features[model$objects$features$fname, 'performance']  <- perf
    features[model$objects$features$fname, 'score']        <- perf*vect.normalise(model$objects$features$importance)

    cat('\n Features scored: ', ' Performance: ', perf, ' Remaining: ', nf)
  }

  return(features)
}


transformer_types = function(model){
  mdlns = model$type
  for(tr in model$transformers){
    mdlns = c(mdlns, transformer_names(tr))
  }
  return(mdlns %>% unique)
}
