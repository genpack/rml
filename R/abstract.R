MODEL = setRefClass('MODEL',
                    fields = list(name = "character", type = "character", config = "list", fitted = 'logical', objects = "list"),

                    methods = list(
                      initialize           = function(transformer = NULL, settings = list(), ...){
                        callSuper(...);

                        if(is.null(settings$cross_validation)){
                          settings$cross_validation = list()
                        }
                        if(is.null(settings$cross_validation$ntest)){settings$cross_validation$ntest <- 10}
                        if(is.null(settings$cross_validation$split_ratio)){settings$cross_validation$split_ratio <- 0.3}
                        if(is.null(settings$cross_validation$split_method)){settings$cross_validation$split_method <- 'shuffle'}

                        config      <<- settings
                        fitted      <<- FALSE
                        objects$transformer <<- transformer
                      },
                      fit                  = function(X, y){},
                      reset                = function(){objects <<- list(); fitted <<- FALSE},
                      get.features.name    = function(){character()},
                      get.features.weight  = function(){},
                      predict              = function(){if(!fitted) stop(paste('from', name, 'of type', type, ':', 'Model not fitted!', '\n'))},
                      transform            = function(X){
                        if(!is.null(objects$transformer)){
                          if(!objects$transformer$fitted) objects$transformer$fit(X, y)
                          X = objects$transformer$predict(X)
                        }
                        return(X)
                      },
                      get.performance.fit  = function(){},
                      get.performance.cv   = function(){},
                      get.parameters       = function(){},
                      get.expert.predictor = function(){},
                      get.expert.features  = function(){}
                    ))


REGRESSOR  = setRefClass('REGRESSOR', contains = 'MODEL',
    methods = list(
      # todo: add k-fold, chronological shuffle, chronological split
      get.performance.cv = function(X, y){
        keep   = objects$model
        N       = X %>% nrow
        acc     = c()
        for(i in sequence(ntest)){
          test    = N %>% sequence %>% sample(config$split_ratio*N, replace = F)
          X_train = X[- test, ]
          X_test  = X[  test, ]
          y_train = y[- test]
          y_test  = y[  test]
          fit(X_train, y_train)
          yht = predict(X_test)
          acc = c(acc, objetcs$performance_meter(yht, y_test))
        }
        objects$model <<- keep
        return(acc)
      }

    ))
