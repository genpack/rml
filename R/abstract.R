# 25 Oct 2019 'fitted', 'features.include' and 'transformer' added to maler_words


maler_words = c('keep_columns', 'keep_features', 'max_train',
                'cv.ntrain', 'cv.ntest', 'cv.test_ratio','cv.train_ratio', 'cv.split_method', 'cv.performance_metric', 'cv.reset_transformer', 'cv.restore_model',
                'sfs.enabled', 'rfe.enabled', 'rfe.importance_threshold', 'remove_invariant_features', 'sig_level', 'predict_probabilities',
                'decision_threshold', 'threshold_determination', 'metric', 'return_logit', 'transformers', 'fitted', 
                'segmentation_features', 'features.include')


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
      if(is.null(settings$cv.ntest)){settings$cv.ntest <- 5}
      if(is.null(settings$cv.ntrain)){settings$cv.ntrain <- 1}
      if(is.null(settings$cv.restore_model)){settings$cv.restore_model <- F}
      if(is.null(settings$cv.train_ratio)){settings$cv.train_ratio <- 0.5}
      if(is.null(settings$cv.test_ratio)){settings$cv.test_ratio <- 0.2}
      if(is.null(settings$cv.performance_metric)){settings$cv.performance_metric <- 'gini'}
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
        while(length(fte) > 0){
          objects$features <<- objects$features %>% filter(fname %in% ftk)
          if(is.empty(objects$features)){.self$fit.distribution(X, y); fte = c()} else {
            .self$model.fit(X[objects$features$fname], y)
            fns = objects$features$fname
            ftk = fns[which(objects$features$importance > config$rfe.importance_threshold)] # features to keep
            fte = fns %-% ftk
          }
        }

        if(is.empty(objects$features)){
          cat(name, ': ', 'No features left after elimination! Distribution fitted for output variable.', '\n')
        }
      }
    },

    fit.quad = function(X, y){
      fit.rfe(X, y)
      clmns = colnames(X)
      XP    = X[objects$features$fname]
      for(i in 1:ncol(X)){
        v  = X[,i]
        Xi <- X %>% as.matrix %>% apply(2, function(u) u*v) %>% as.data.frame %>% {colnames(.) <- clmns %>% paste(clmns[i], sep = 'X');.}
        XP = XP %>% cbind(Xi)
        objects$features <<- colnames(XP) %>% sapply(function(i) XP %>% pull(i) %>% class) %>% as.data.frame %>% {colnames(.)<-'fclass';.} %>% rownames2Column('fname') %>% mutate(fname = as.character(fname), fclass = as.character(fclass))
        fit.rfe(XP, y)
        XP  = XP[objects$features$fname]
      }
    },
    
    fit.distribution = function(X = NULL, y){
      out = outliers(y); while(!is.empty(out)){y = y[-out]; out = outliers(y)}
      objects$model <<- list(family = 'normal', mean = mean(y, na.rm = T), sd = sd(y, na.rm = T))
    },
    
    predict.distribution = function(X){
      N = try(nrow(X), silent = T)
      if(!inherits(N, 'integer')) N = as.integer(N)
      # rnorm(N, objects$model$mean, objects$model$sd) %>% as.data.frame 
      rep(objects$model$mean, N) %>% as.data.frame
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
        
        if(!is.null(config$upsample)){
          w1 = which(y == 1)
          w0 = which(y == 0) %>% sample(length(w1))
          ww = c(w1, w2) %>% sample(length(w1) + length(w2))
          X = X[ww,]; y = y[ww]
        }
        X = transform(X, y)
        if(!is.null(config$features.include)){X = X %>% spark.select(config$features.include %^% colnames(X))}
        if(!is.null(config$features.exclude)){X = X %>% spark.select(colnames(X) %-% config$features.exclude)}
        if(config$remove_invariant_features) X %<>% remove_invariant_features
        objects$features <<- colnames(X) %>% sapply(function(i) X %>% pull(i) %>% class) %>% as.data.frame %>% {colnames(.)<-'fclass';.} %>% rownames2Column('fname') %>% mutate(fname = as.character(fname), fclass = as.character(fclass))
        if(is.empty(objects$features)){fit.distribution(X, y)} 
        else if(config$rfe.enabled) {fit.rfe(X, y)} else {.self$model.fit(X, y)}
        # if(config$quad.enabled) {fit.quad(X, y)}
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
    
    transformer_count = function(){
      cnt = 1
      for(tr in transformers){
        cnt = cnt + tr$transformer_count()
      }
      return(cnt)
    },
    
    model.save = function(path = getwd()){
      if(!file.exists(path)) {dir.create(path)}
      for(tr in transformers){
        tr$model.save(path)
      }
    },
    
    model.load = function(path = getwd()){
      for(tr in transformers){
        tr$model.load(path)
      }
    },
    
    transformer_names = function(){
      mdlns = name
      for(tr in transformers){
        mdlns = c(mdlns, tr$transformer_names())
      }
      return(mdlns %>% unique)
    },

    # todo: add k-fold, chronological shuffle, chronological split
    get.performance.cv = function(X, y, method = 'shuffle'){
      method = match.arg(method)
      if(config$cv.restore_model){
        keep   = list(objects = objects, fitted = fitted, config = config)
      }

      # Split by shuffling: todo: support other splitting methods(i.e.: chronological)
      N       = nrow(X)

      scores = c()

      for (i in sequence(config$cv.ntrain)){
        ind_train = N %>% sequence %>% sample(size = floor(config$cv.train_ratio*N), replace = F)

        X_train = X[ind_train, ]
        y_train = y[ind_train]
         
        reset(config$cv.reset_transformer)
        .self$fit(X_train, y_train)
        
        for(j in sequence(config$cv.ntest)){
          N2 = N - length(ind_train)
          ind_test = sequence(N) %>% setdiff(ind_train) %>% sample(size = floor(config$cv.test_ratio*N2), replace = F)
          X_test  = X[ind_test,]
          y_test  = y[ind_test]
          scores = c(scores, .self$performance(X_test, y_test, metric = config$cv.performance_metric))
        }
      }
      
      if(config$cv.restore_model){
        objects <<- keep$objects
        fitted  <<- keep$fitted
        config  <<- keep$config
      }
      return(scores)
    },

    # get.performance = function(X_train, y_train, X_test, y_test){
    #   
    # 
    #   reset(config$cv.reset_transformer)
    #   .self$fit(X_train, y_train)
    #   yhat   = predict(X_test) # this has error! Fix it!
    #   objects$model <<- keep
    #   return(list(y_pred = yhat, y_true = y_test))
    # },

    get.size             = function(){
      t_size = list(config = 0, objects = 0, transformers = 0)
      for(tr in transformers){
        slist          = tr$get.size()
        t_size$config  = t_size$config  + slist$config
        t_size$objects = t_size$objects + slist$objects
        t_size$transformers = t_size$transformers + slist$transformers
      }
      list(config  = object.size(config), 
           objects = object.size(objects),
           transformers = object.size(transformers) + t_size$config + t_size$objects + t_size$transformers)
      
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
