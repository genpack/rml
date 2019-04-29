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
  tbl %>% rename(x = col1, y = col2) %>% select(x, y) %>%
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
  tbl %<>% rename(x = num_col, y = cat_col) %>% select(x, y)
  if(!is.null(fun)){tbl %<>% mutate(x = fun(x))}
  N = nrow(tbl)
  b = tbl %>% pull('y') %>% mean
  tbl %<>%
    group_by(x) %>% summarise(X = length(x), Y = sum(y)) %>% arrange(x) %>%
    mutate(a = cumsum(X)/N, c = cumsum(Y)/N) %>% filter(a < 1) %>%
    mutate(den = a*b*(1-a)*(1-b)) %>%
    mutate(chi = (c- a*b)^2/den) %>% arrange(chi)

  out <- tbl %>% tail(1) %>% select(split = x, correlation = chi) %>% as.list

  #out$chisq  <- N*out$chisq
  #out$pvalue <- pchisq(out$chisq, df = 1, lower.tail = F)
  return(out)
}

#' @export
spark.optSplit.chi = function(tbl, num_col, cat_col, breaks = 1000, fun = NULL){
  tbl %<>% rename(xxx = num_col, yy = cat_col) %>% select(xxx, yy)
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
    select(split, correlation = chi) %>% as.list -> out
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
  tbl %<>% rename(xxx = prob_col, yy = label_col) %>% select(xxx, yy)
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
    select(split, tp, fp, tn, fn, precision, recall, f1)
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
  for(i in sequence(length(columns))){
    col = columns[i]
    res <- df %>% optSplit.chi(num_col = col, cat_col = label_col, fun = fun)
    res <- c(Column = col, res) %>% as.data.frame
    if(i == 1){sp = res} else {sp %<>% rbind(res)}
    print(res)
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
  df %<>% rename(xxx = prob_col, yy = label_col) %>% select(xxx, yy)

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
    select(split, tp, fp, tn, fn, precision, recall, f1)
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
remove_invariant_features = function(X){
  fsds = X %>% apply(2, function(x) length(unique(x)))
  X[, which(fsds > 1)]
}
