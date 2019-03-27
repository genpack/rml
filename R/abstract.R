MODEL = setRefClass('MODEL',
  fields = list(name = "character", type = "character", config = "list", fitted = 'logical', objects = "list"),

  methods = list(
    initialize           = function(name = NULL, transformer = NULL, settings = list(), ...){
      callSuper(...)

      if(is.null(settings$cross_validation)){
        settings$cross_validation = list()
      }
      if(is.null(settings$transformer$keep_original)){settings$transformer$keep_original = T}
      if(is.null(settings$cross_validation$ntest)){settings$cross_validation$ntest <- 10}
      if(is.null(settings$cross_validation$split_ratio)){settings$cross_validation$split_ratio <- 0.3}
      if(is.null(settings$cross_validation$split_method)){settings$cross_validation$split_method <- 'shuffle'}
      if(is.null(settings$cross_validation$reset_transformer)){settings$cross_validation$reset_transformer = T}
      if(is.null(settings$cross_validation$performance_meter)){
        settings$cross_validation$performance_meter <- function(y1, y2){
          err = (y1 - y2)^2 %>% sum
          # den = (y_test - mean(y_test))^2 %>% sum
          den = (y2 - mean(y2))^2 %>% sum
          return(1.0 - min(err/den, 1.0))
        }
      }

      config      <<- settings
      fitted      <<- FALSE
      objects$transformer <<- transformer
    },
    fit                  = function(X, y){},
    reset                = function(reset_transformer = T){
      fitted <<- FALSE
      if (reset_transformer & !is.null(objects$transformer)){
        objects$transformer$reset(reset_transformer = T)
      }},
    get.feature.names    = function(){character()},
    get.featur.weights  = function(){},
    predict              = function(){if(!fitted) stop(paste('from', name, 'of type', type, ':', 'Model not fitted!', '\n'))},
    transform            = function(X, y = NULL){
      if(!is.null(objects$transformer)){
        if(!objects$transformer$fitted) {
          # if(is.null(y)) stop(paste(objects$transformer$name, ', a transformer of type', objects$transformer$name, 'belonging to model', )
          objects$transformer$fit(X, y)
        }
        if(config$transformer$keep_original){
          X = cbind(X, objects$transformer$predict(X))
        } else
        X = objects$transformer$predict(X)
      }
      return(X)
    },
    get.performance.fit  = function(){},

    # todo: add k-fold, chronological shuffle, chronological split
    get.performance.cv = function(X, y, ntest = 20, method = 'shuffle'){
      method = match.arg(method)
      keep   = objects$model

      # X  = transform(X, y)

      N      = nrow(X)
      scores = c()

      for (i in sequence(ntest)){
        trindex = N %>% sequence %>% sample(size = floor((1 - config$cross_validation$split_ratio)*N))

        X_train = X[trindex, ]
        y_train = y[trindex]
        X_test  = X[- trindex,]
        y_test  = y[- trindex]

        reset(config$cross_validation$reset_transformer)
        fit(X_train, y_train)
        yhat   = predict(X_test)
        scores = c(scores, config$cross_validation$performance_meter(yhat, y_test))
      }
      objects$model <<- keep
      return(scores)
    },

    get.parameters       = function(){},
    get.expert.predictor = function(){},
    get.expert.features  = function(){}
))

# REGRESSOR  = setRefClass('REGRESSOR', contains = 'MODEL',
#     methods = list(
#       # todo: add k-fold, chronological shuffle, chronological split
#       get.performance.cv = function(X, y){
#         keep   = objects$model
#         N       = X %>% nrow
#         acc     = c()
#         for(i in sequence(ntest)){
#           test    = N %>% sequence %>% sample(config$cross_validation$split_ratio*N, replace = F)
#           X_train = X[- test, ]
#           X_test  = X[  test, ]
#           y_train = y[- test]
#           y_test  = y[  test]
#           fit(X_train, y_train)
#           yht = predict(X_test)
#           acc = c(acc, config$cross_validation$performance_meter(yht, y_test))
#         }
#         objects$model <<- keep
#         return(acc)
#       }
#
#     ))

# tools.R


# Returns binary Chi-Squared statistics for two binary columns 
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

optSplit.chi = function(tbl, num_col, cat_col){
  tbl %<>% rename(x = num_col, y = cat_col) %>% select(x, y)
  N = nrow(tbl)
  b = tbl %>% pull('y') %>% mean
  tbl %<>% 
    group_by(x) %>% summarise(X = length(x), Y = sum(y)) %>% arrange(x) %>%
    mutate(a = cumsum(X)/N, c = cumsum(Y)/N) %>% filter(a < 1) %>% 
    mutate(den = a*b*(1-a)*(1-b)) %>% 
    mutate(chi = (c- a*b)^2/den) %>% arrange(chi)
  
  tbl %>% tail(1) %>% select(split = x, chisq = chi) %>% as.list
}
  
spark.optSplit.chi = function(tbl, num_col, cat_col, breaks = 1000){
  tbl %<>% rename(xxx = num_col, yy = cat_col) %>% select(xxx, yy)
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
    select(split, chisq = chi) %>% as.list
}

# prob_col : column name containing probabilities of class 1 or positive
# label_col: column name containing actual class labels
# dte stands for decision threshold evaluator:
# Returns a table containing performance metrics for each decision threshoild
spark.dte = function(tbl, prob_col, label_col, breaks = 1000){
  tbl %<>% rename(xxx = prob_col, yy = label_col) %>% select(xxx, yy)
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

# finds the optimal decision threshold to maximize f1 score
spark.optSplit.f1 = function(tbl, prob_col, label_col, breaks = 1000){
  tbl %>% spark.dte(prob_col, label_col, breaks) %>% arrange(desc(f1)) %>% 
    head(1) %>% collect %>% as.list
}

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

