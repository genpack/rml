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



