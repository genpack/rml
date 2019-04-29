MODEL = setRefClass('MODEL',
  fields = list(name = "character", type = "character", config = "list", fitted = 'logical', objects = "list"),

  methods = list(
    initialize           = function(..., name = character(), transformer = NULL, mother = NULL, features = NULL, pupils = NULL){
      callSuper(name = name)
      settings = list(...)
      for (sn in sequence(length(settings))){
        set = settings[[sn]]
        if(inherits(set, 'list')) {
          settings = settings %<==>% set
          settings[[sn]] <- NULL
        }
      }
      
      if(is.null(settings$keep_columns)){settings$keep_columns = F}
      if(is.null(settings$keep_features)){settings$keep_features = F}
      if(is.null(settings$cv.ntest)){settings$cv.ntest <- 10}
      if(is.null(settings$cv.split_ratio)){settings$cv.split_ratio <- 0.7}
      if(is.null(settings$cv.split_method)){settings$cv.split_method <- 'shuffle'}
      if(is.null(settings$cv.reset_transformer)){settings$cv.reset_transformer = T}

      config      <<- settings
      fitted      <<- FALSE
      objects$transformer <<- transformer
      objects$mother      <<- mother
      objects$features    <<- features
      objects$pupils      <<- pupils
    },
    reset                = function(reset_transformer = T){
      fitted <<- FALSE
      objects$features <<- NULL
      if (reset_transformer & !is.null(objects$transformer)){
        objects$transformer$reset(reset_transformer = T)
      }},
    get.feature.names    = function(){character()},
    get.feature.weights  = function(){},
    predict              = function(X){
      if(!fitted) stop(paste('from', name, 'of type', type, ':', 'Model not fitted!', '\n'))
      X = transform(X)
    },
    treat                = function(out, fet, org){
      if(config$keep_columns)
        if(config$keep_features) return(cbind(org, out)) 
        else return(cbind(org %>% spark.unselect(colnames(fet)), out))
      else if (config$keep_features) return(cbind(fet, out)) 
           else return(out)
    },
    
    fit = function(X, y){
      X = transform(X, y)
      if(!is.null(config$features.include)){X = X %>% spark.select(config$features.include %^% colnames(X))}
      if(!is.null(config$features.exclude)){X = X %>% spark.select(colnames(X) %-% config$features.exclude)}
      X %<>% remove_invariant_features
      objects$features <<- colnames(X) %>% sapply(function(i) X %>% pull(i) %>% class) %>% as.data.frame %>% {colnames(.)<-'fclass';.} %>% rownames2Column('fname') %>% mutate(fname = as.character(fname), fclass = as.character(fclass))
      return(X)
    },
    
    transform            = function(X, y = NULL){
      if(!is.null(objects$transformer)){
        if(!objects$transformer$fitted) {
          objects$transformer$fit(X, y)
        }
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

      # Split by shuffling: todo: support other splitting methods(i.e.: chronological)
      N       = nrow(X)
      
      scores = c()

      for (i in sequence(ntest)){
        trindex = N %>% sequence %>% sample(size = floor(config$cv.split_ratio*N), replace = F)
        
        X_train = X[trindex, ]
        y_train = y[trindex]
        X_test  = X[- trindex,]
        y_test  = y[- trindex]
        
        perf   = get.performance(X_train, y_train, X_test, y_test)
        scores = c(scores, config$metric(perf$y_pred, perf$y_true))
      }
      objects$model <<- keep
      return(scores)
    },
    
    get.performance = function(X_train, y_train, X_test, y_test){
      keep   = objects$model

      reset(config$cv.reset_transformer)
      .self$fit(X_train, y_train)
      yhat   = predict(X_test) # this has error! Fix it!
      objects$model <<- keep
      return(list(y_pred = yhat, y_true = y_test))
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
#           test    = N %>% sequence %>% sample(config$cv.split_ratio*N, replace = F)
#           X_train = X[- test, ]
#           X_test  = X[  test, ]
#           y_train = y[- test]
#           y_test  = y[  test]
#           fit(X_train, y_train)
#           yht = predict(X_test)
#           acc = c(acc, config$metric(yht, y_test))
#         }
#         objects$model <<- keep
#         return(acc)
#       }
#
#     ))

# tools.R



