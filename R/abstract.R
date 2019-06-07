#' @export MODEL
MODEL = setRefClass('MODEL',
  fields = list(name = "character", type = "character", config = "list", fitted = 'logical', transformers = 'list', objects = "list"),

  methods = list(
    initialize           = function(..., name = character(), transformers = list(), mother = NULL, features = NULL, pupils = NULL){
      callSuper(name = name)
      settings = list(...)
      ns       = names(settings)
      for (sn in ns){
        set = settings[[sn]]
        if(inherits(set, 'list') &  sn == 'config') {
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
      if(is.null(settings$rfe.enabled)){settings$rfe.enabled = F}
      if(is.null(settings$rfe.importance_threshold)){settings$rfe.importance_threshold = 0}
      if(is.null(settings$remove_invariant_features)){settings$remove_invariant_features = T}

      config       <<- settings
      fitted       <<- FALSE
      transformers <<- transformers %>% {if(inherits(.,'list')) . else list(.)}
      objects$mother       <<- mother
      objects$features     <<- features
      objects$pupils       <<- pupils
    },
    reset                = function(reset_transformers = T){
      fitted <<- FALSE
      objects$features <<- NULL
      if (reset_transformers & !is.empty(transformers)){
        for (transformer in transformers) transformer$reset(reset_transformers = T)
      }
    },
    get.feature.names    = function(){character()},
    get.feature.weights  = function(){},
    predict              = function(X){
      if(!fitted) stop(paste('from', name, 'of type', type, ':', 'Model not fitted!', '\n'))
      if(inherits(X, 'matrix')){X %<>% as.data.frame}
      X = transform(X)
    },
    treat                = function(out, fet, org){
      if(!is.null(config$pass_columns)) org = org[config$pass_columns]
      if(!is.null(config$filter_columns)) org = org[colnames(org) %-% config$filter_columns]
      
      if(config$keep_columns)
        if(config$keep_features) return(cbind(org, out))
        else return(cbind(org %>% spark.unselect(colnames(fet)), out))
      else if (config$keep_features){
        # if(!is.null(config$features.include)){
        #   extra = config$features.include %-% colnames(fet)
        #   return(cbind(fet, org[extra], out))
        # } else return(cbind(fet, out))
        return(cbind(fet, out))
      } 
           else return(out)
    },

    # Fitting with recursive feature elimination (RFE). Works only for model fitters which generate feature importance
    fit.rfe = function(X, y){
      .self$model.fit(X, y)
      if('importance' %in% colnames(objects$features)){
        fns = objects$features$fname
        ftk = fns[which(objects$features$importance > config$rfe.importance_threshold)] # features to keep
        fte = fns %-% ftk
        while((length(fte) > 0) & (length(fte) < nrow(objects$features))){
          objects$features <<- objects$features %>% filter(fname %in% ftk) 
          .self$model.fit(X, y)
          fns = objects$features$fname
          ftk = fns[which(objects$features$importance > config$rfe.importance_threshold)] # features to keep
          fte = fns %-% ftk
        }
        
        if(length(fte) == nrow(objects$features)){
          cat('No features will be left after elimination. RFE process terminated!')
        }
      }
    },
    
    fit = function(X, y = NULL){
      if(!fitted){
        if(inherits(X, 'matrix')){X %<>% as.data.frame}
        if(!is.null(config$max_train)){
          mxt = min(config$max_train, nrow(X))
          ind = nrow(X) %>% sequence %>% sample(mxt)
          X = X[ind,]
          y = y[ind]
        }
        X = transform(X, y)
        if(!is.null(config$features.include)){X = X %>% spark.select(config$features.include %^% colnames(X))}
        if(!is.null(config$features.exclude)){X = X %>% spark.select(colnames(X) %-% config$features.exclude)}
        if(config$remove_invariant_features) X %<>% remove_invariant_features
        objects$features <<- colnames(X) %>% sapply(function(i) X %>% pull(i) %>% class) %>% as.data.frame %>% {colnames(.)<-'fclass';.} %>% rownames2Column('fname') %>% mutate(fname = as.character(fname), fclass = as.character(fclass))
        if(config$rfe.enabled) {fit.rfe(X, y)} else {.self$model.fit(X, y)} 
      }
      fitted <<- TRUE
    },

    transform            = function(X, y = NULL){
      nt = length(transformers)
      if(nt > 0){
        for(i in sequence(nt)){
          transformer = transformers[[i]]
          if(!transformer$fitted) {transformer$fit(X, y)}
          if(i == 1){
            XT = transformer$predict(X)
          } else {
            XT = cbind(XT, transformer$predict(X) %>% {.[colnames(.) %-% colnames(XT)]})
          }
        }
      } else {XT = X}
      return(XT)
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


COLFILTER = setRefClass('COLFILOTER', contains = 'MODEL', methods = list(
  fit = function(X, y = NULL){
    if(!fitted){
      callSuper(X, y)
    }  
    fitted <<- T
  },
  
  predict = function(X, prob){
    XORG = callSuper(X)
    XFET = XORG[objects$features$fname]
    XOUT = XFET[character()]
    treat(XOUT, XFET, XORG)
  }
  
))
