# 25 Oct 2019 'fitted', 'features.include' and 'transformer' added to reserved_words
# Todos:
# 1- Add imputer module to config to impute for missing values
# look at Recipes: https://conf20-intro-ml.netlify.app/slides/06-recipes.html#2
# 2- Add upsampling and downsampling modules

library(magrittr)

#' Abstract Model Class

#' @description  Reference Class \code{MODEL} is the highest level of herarchy in a series of classes defined in the \code{rml} package.
#' All transformers, classifiers, regressors and survival models inherit from this class.
#'
#' @field name character containing name of the model
#'
#' @field type character containing model type. Specifies transformer type
#' @field package character specifies name of the package model is using
#' @field package_language character specifies programming language of the package either R or Python.
#' @field description character descripbes what the model is.
#' @field config list containing all parameters and settings of the model. Values depend on the model class.
#' @field fitted logical specifies if the model is fitted (trained) or not
#'
#' @field transformers list containing all transformer objects. Each transformer is an instance inheriting from MODEL class.
#' @field gradient_transformers list containing all gradient transformer objects. Each gradient transformer is an instance inheriting from MODEL class.
#' @field yin_transformers list of functions.
#' @field yout_transformers list of functions.
#' @field objects list containing additional data
#' @field objects$features data.frame containing list of features and their characteristics. This table is created after fitting the model.
#' @field objects$fitting_time POSIXct containing time when the model's training is complete. Created after the model is fitted.
#'
#' @export MODEL
#' @exportClass MODEL
MODEL = setRefClass('MODEL',
  fields = list(name = "character", type = "character", package = "character", package_language = "character", description = "character", reserved_words = "character",
                config = "list", fitted = 'logical', transformers = 'list', gradient_transformers = 'list', yin_transformers = 'list', yout_transformers = 'list', objects = "list"),

  methods = list(
    initialize          = function(..., name = character(), transformers = list(), gradient_transformers = list(), mother = NULL, features = NULL, pupils = NULL){
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

      reserved_words <<- c('keep_columns', 'keep_features', 
                         'max_train',
                         'cv.ntrain', 'cv.ntest', 'cv.test_ratio','cv.train_ratio', 'cv.split_method', 'cv.performance_metric', 'cv.reset_transformer', 'cv.restore_model', 'cv.set',
                         'sfs.enabled', 
                         'fe.enabled','fe.recursive', 'fe.importance_threshold', 'fe.quantile',
                         'pp.remove_invariant_features', 'pp.remove_nominal_features', 'pp.remove_numeric_features', 'pp.coerce_integer_features',
                         'model.class', 'model.config', 'model.module',
                         'eda.enabled', 
                         'smp.enabled', 'smp.fix_class_ratio', 'smp.num_rows',
                         'features.include', 'features.include.at', 'features.exclude', 'features.exclude.at',
                         'ts.enabled', 'ts.id_col', 'ts.time_col', 
                         'return_features', 'feature_subset_size', 'gradient_transformers_aggregator', 'save_predictions',
                         'metric', 'transformers', 'fitted')
      
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
      if(is.null(settings$fe.enabled)){settings$fe.enabled = F}
      if(is.null(settings$fe.recursive)){settings$fe.recursive = F}
      if(is.null(settings$fe.importance_threshold)){settings$fe.importance_threshold = 0}
      if(is.null(settings$pp.remove_invariant_features)){settings$pp.remove_invariant_features = T}
      if(is.null(settings$pp.remove_nominal_features)){settings$pp.remove_nominal_features = T}
      if(is.null(settings$pp.remove_numeric_features)){settings$pp.remove_numeric_features = F}
      if(is.null(settings$pp.coerce_integer_features)){settings$pp.coerce_integer_features = F}
      if(is.null(settings$eda.enabled)){settings$eda.enabled = F}
      if(is.null(settings$smp.enabled)){settings$smp.enabled = F}
      if(is.null(settings$ts.enabled)){settings$ts.enabled = F}
      if(is.null(settings$ts.id_col)){settings$ts.id_col = 'caseID'}
      if(is.null(settings$ts.time_col)){settings$ts.time_col = 'time'}
      if(is.null(settings$gradient_transformers_aggregator)){settings$gradient_transformers_aggregator = mean}
      if(is.null(settings$save_predictions)){settings$save_predictions = F}
      
      config       <<- settings
      fitted       <<- FALSE
      transformers <<- transformers %>% {if(inherits(.,'list')) . else list(.)}
      gradient_transformers <<- gradient_transformers %>% {if(inherits(.,'list')) . else list(.)}
      objects$mother        <<- mother
      objects$features      <<- features
      objects$pupils        <<- pupils
    },

    reset               = function(reset_transformers = T, reset_gradient_transformers = T){
      fitted <<- FALSE
      objects$features <<- NULL
      objects$saved_pred <<- NULL
      if (reset_transformers & !is.empty(transformers)){
        for (transformer in transformers) transformer$reset(reset_transformers = T, reset_gradient_transformers = reset_gradient_transformers)
      }
      if (reset_gradient_transformers & !is.empty(gradient_transformers)){
        for (transformer in gradient_transformers) transformer$reset(reset_transformers = reset_transformers, reset_gradient_transformers = T)
      }
    },

    info.features       = function(include_transformers = T){
      fet = objects$features
      if(include_transformers){
        for(tr in transformers){
          fet %<>% rbind(tr$info.features())
        }
      }
      return(fet)
    },

    canuse_saved_prediction = function(X){
      permit = F
      if(!is.null(objects$saved_pred)){
        X      = X[numerics(X)]
        permit = identical(colSums(X), objects$saved_pred$CSUMS)
        permit = permit & identical(rowSums(X), objects$saved_pred$RSUMS)
        permit = permit & identical(X[objects$saved_pred$RNDM, ], objects$saved_pred$XSAMP)
      }
      return(permit)
    },

    keep_prediction      = function(X, Y){
      X    = X[numerics(X)]
      rnd  = sequence(nrow(X)) %>% sample(size = ncol(X), replace = T)
      objects$saved_pred <<- list(CSUMS = colSums(X), RSUMS = rowSums(X), XOUT = Y, RNDM = rnd, XSAMP = X[rnd,])
    },

    predict              = function(X){
      if(!fitted) stop(paste('from', name, 'of type', type, ':', 'Model not fitted!', '\n'))
      if(inherits(X, 'matrix')){X %<>% as.data.frame}
      XORG = transform_x(X)
      zero = objects$features$fname %-% colnames(XORG)
      if(length(zero) > 0){
        for(z in zero) XORG[,z] <- 0
        cat('Warning from ', name, ': Features ', paste(zero, collapse = ','), ' were not generated by transformers! Filled with zero.', '\n')
      }
      XFET = XORG[objects$features$fname]

      cusp = config$save_predictions
      if(cusp){
        cusp = canuse_saved_prediction(XFET)
      }
      if(cusp){
        XOUT = objects$saved_pred$XOUT
      } else {
        XOUT = .self$model.predict(XFET)
        if(config$save_predictions){
          keep_prediction(XFET, XOUT)
        }
      }

      XOUT = transform_yout(X, XOUT)
      if(ncol(XOUT) > 0) colnames(XOUT) <- name %>% paste(colnames(XOUT), sep = '_')

      objects$n_output <<- ncol(XOUT)
      treat(XOUT, XFET, XORG)
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

    # Fitting with feature elimination (FE). Works only for model fitters which generate feature importance
    fit.fe = function(X, y){
      .self$model.fit(X, y)
      if('importance' %in% colnames(objects$features)){
        if(!is.null(config$fe.quantile)){
          threshold = objects$features$importance %>% quantile(probs = config$fe.quantile)
        } else {
          threshold = config$fe.importance_threshold
        }
        fns = objects$features$fname
        ftk = fns[which(objects$features$importance > threshold)] # features to keep
        fte = fns %-% ftk
        while(length(fte) > 0){
          objects$features <<- objects$features %>% filter(fname %in% ftk)
          if(is.empty(objects$features)){.self$fit.distribution(X, y); fte = c()} else {
            .self$model.fit(X[objects$features$fname], y)
            fns = objects$features$fname
            ftk = fns[which(objects$features$importance > threshold)] # features to keep
            fte = chif(config$fe.recursive, fns %-% ftk, c())
          }
        }
        config$features.include <<- objects$features$fname

        if(is.empty(objects$features)){
          cat(name, ': ', 'No features left after elimination! Distribution fitted for output variable.', '\n')
        }
      }
    },

    fit.quad = function(X, y){
      fit.fe(X, y)
      clmns = colnames(X)
      XP    = X[objects$features$fname]
      for(i in 1:ncol(X)){
        v  = X[,i]
        Xi <- X %>% as.matrix %>% apply(2, function(u) u*v) %>% as.data.frame %>% {colnames(.) <- clmns %>% paste(clmns[i], sep = 'X');.}
        XP = XP %>% cbind(Xi)
        objects$features <<- colnames(XP) %>% sapply(function(i) XP %>% pull(i) %>% class) %>% as.data.frame %>% {colnames(.)<-'fclass';.} %>% rownames2Column('fname') %>% mutate(fname = as.character(fname), fclass = as.character(fclass))
        fit.fe(XP, y)
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
        if(config$smp.enabled){
          actual_ratio = mean(y)
          n_train_rows = length(y)
          
          if(!is.null(verify(config$smp.fix_class_ratio, 'numeric', domian = c(.Machine$double.eps, 1 - .Machine$double.eps), null_allowed = T))){
            if(config$fix_class_ratio >= actual_ratio){
              # downsample negative class, keep all positive class:
              w1 = which(y == 1)
              n1 = length(w1)
              rr = config$fix_class_ratio
              w0 = which(y == 0) %>% sample(as.integer(n1*(1.0 - rr)/rr))
            } else {
              # downsample positive class, keep all negative class:
              w0 = which(y == 0)
              n0 = length(w0)
              rr = config$fix_class_ratio
              w1 = which(y == 1) %>% sample(as.integer(n0*(1.0 - rr)/rr))
            }
            ind = c(w0, w1) %>% sample(length(c(w0, w1))) 
            X = X[ind,]; y = y[ind]
            n_train_rows = length(y)
          }
          
          n_rows = verify(config$smp.num_rows, c('numeric', 'integer'), lengths = 1, domain = c(1, Inf), default = n_train_rows) %>% 
            min(n_train_rows) %>% {.*verify(config$smp.ratio, 'numeric', lengths = 1, domain = c(0, 1), default = 1.0)} %>% as.integer

          ind = n_train_rows %>% sequence %>% sample(n_rows)
          X = X[ind,]; y = y[ind]
        }
        y = transform_yin(X, y)
        if(!is.null(config[['features.include']])){X = X[config$features.include %^% colnames(X)]}
        if(!is.null(config[['features.exclude']])){X = X[colnames(X) %-% config$features.exclude]}
        X = transform_x(X, y)
        if(!is.null(config[['features.include.at']])){X = X[config$features.include.at %^% colnames(X)]}
        if(!is.null(config[['features.exclude.at']])){X = X[colnames(X) %-% config$features.exclude.at]}

        assert(ncol(X) > 0, 'No column found in the training dataset!')

        if(config$pp.coerce_integer_features){
          X %<>% int_ordinals
        }
        
        if(inherits(X, 'WIDETABLE')){
          objects$features <<- X$meta %>% distinct(column, .keep_all = T) %>% select(fname = column, fclass = class, n_unique = n_unique)
        } else {
          objects$features <<- colnames(X) %>% sapply(function(i) X %>% pull(i) %>% class) %>% as.data.frame %>% {colnames(.)<-'fclass';.} %>% rownames2Column('fname') %>% mutate(fname = as.character(fname), fclass = as.character(fclass))
        }
        nums = objects$features$fclass %in% c('numeric', 'integer')
        cats = objects$features$fclass %in% c('character', 'integer', 'factor')
        if((sum(nums) > 0) & (config$eda.enabled)){
          numinfo = feature_info_numeric(X)
          objects$features <<- objects$features %>% left_join(numinfo, by = 'fname')
        }
        if((sum(cats) > 0) & (config$eda.enabled)){
          catinfo = feature_info_categorical(X)
          objects$features <<- objects$features %>% merge(catinfo, all = T)
        }

        if(config$pp.remove_invariant_features){
          if(!'n_unique' %in% colnames(objects$features)){
            objects$features$n_unique <<- colnames(X) %>% sapply(function(x) X %>% pull(x) %>% unique %>% length) %>% unlist
          }
          objects$features <<- objects$features %>% filter(n_unique > 1)
          X = X[objects$features$fname]
        }
        
        # todo: Add other preprocessing: pp.treat_outliers (remove, trim, adapted trim, ...), ...
        # remove outliers can be considered as a transformer as well
        if(config$pp.remove_nominal_features){
          objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer', 'float', 'float64'))
          X = X[objects$features$fname]
        }
        
        if(config$pp.remove_numeric_features){
          objects$features <<- objects$features %>% filter(!fclass %in% c('numeric', 'float', 'float64'))
          X = X[objects$features$fname]
        }
        
        if(is.empty(objects$features)){fit.distribution(X, y)}
        else if(config$fe.enabled) {fit.fe(X, y)} else {.self$model.fit(X, y)}
        # if(config$quad.enabled) {fit.quad(X, y)}
      }
      fitted <<- TRUE
      objects$fitting_time <<- Sys.time()
    },

    transform_x = function(X, y = NULL){
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

    y_gradient = function(X, y = NULL){
      nt = length(gradient_transformers)
      if(nt > 0){
        for(i in sequence(nt)){
          transformer = gradient_transformers[[i]]
          if(inherits(transformer, 'character')){
            assert(transformer %in% colnames(X))
            if(i == 1){
              YT = X[transformer]
            } else {
              YT = cbind(YT, X[transformer])
            }
          } else if (inherits(transformer, 'MODEL')){
            if(!transformer$fitted) {transformer$fit(X, y)}

            if(i == 1){
              YT = transformer$predict(X)
            } else {
              YT = cbind(YT, transformer$predict(X) %>% {.[colnames(.) %-% colnames(YT)]})
            }
          }
          yt = YT %>% as.matrix %>% apply(1, config$gradient_transformers_aggregator)
        }
      } else {yt = 0}
      return(yt)
    },

    transform_yin = function(X, y){
      grad = y_gradient(X, y)
      if(sum(abs(grad)) > .Machine$double.eps){
        attr(y, 'gradient') <- grad
      }
      return(y)
    },

    transform_yout = function(X, Y){
      if(length(gradient_transformers) > 0){
        grad = y_gradient(X)

        for(i in numerics(Y)){
          Y[, i] <- Y[, i] + grad
        }
        # todo: apply y_transformers (list of functions)
      }
      return(Y)
    },

    performance.fit  = function(){},

    info.transformer_count = function(){
      cnt = 1
      for(tr in transformers){
        cnt = cnt + tr$info.transformer_count()
      }
      return(cnt)
    },

    info.transformer_names = function(){
      mdlns = name
      for(tr in transformers){
        mdlns = c(mdlns, tr$info.transformer_names())
      }
      return(mdlns %>% unique)
    },

    info.edge_list = function(first = T){
      edgelist = NULL
      if(is.empty(transformers)){
        edgelist %<>% rbind(c('INPUT', name))
      } else {
        for(tr in transformers){
          edgelist %<>% rbind(c(tr$name, name))
          edgelist %<>% rbind(tr$info.edge_list(first = F))
        }
      }
      if(first){
        edgelist %<>% rbind(c(name, 'OUTPUT'))
      }
      return(edgelist)
    },

    info.model = function(){
      info = list(name = name, type = type, class = class(.self)[1], description = description, package = package,
                  language = package_language, return = config$return,
                  fitted = fitted, keep_columns = config$keep_columns, keep_features = config$keep_features,
                  max_train = config$max_train, fe = config$fe.enabled, metric = config$metric,
                  outputs = objects$n_output)
      for(i in names(info)){
        if(is.null(info[[i]])){ info[[i]] <- 'NULL'}
      }
      return(info)
    },

    info.transformers = function(){
      tbl = do.call(data.frame, info.model() %>% list.add(stringsAsFactors = F))

      for(tr in transformers){
        tbl_tr = do.call(data.frame, tr$info.model() %>% list.add(stringsAsFactors = F))
        tbl %<>% rbind(tbl_tr)
      }
      return(tbl)
    },

    plot.network = function(plotter = 'visNetwork', ...){
      nodes = xgb$info.transformers()
      rownames(nodes) <- nodes$name
      nodes['INPUT', 'name'] <- 'INPUT'
      nodes['INPUT', 'type'] <- 'Data'
      nodes['INPUT', 'class'] <- 'INPUT'
      nodes['INPUT', 'description'] <- 'Features'
      nodes['OUTPUT', 'name'] <- 'OUTPUT'
      nodes['OUTPUT', 'type'] <- 'Final Prediction'
      nodes['OUTPUT', 'class'] <- 'OUTPUT'
      nodes['OUTPUT', 'description'] <- ''
      nodes$outputs[is.na(nodes$outputs)] <- '?'
      nodes[is.na(nodes)] <- 'NULL'
      nodes$type %<>% as.factor

      nodes$description = paste(nodes$class, nodes$description, nodes$type, sep = '\n')

      links = xgb$info.edge_list() %>% {colnames(.) <- c('source', 'target');.} %>% as.data.frame %>%
        mutate(source = as.character(source), target = as.character(target)) %>%
        left_join(nodes %>% select(source = name, outputs), by = 'source')

      viser::viserPlot(list(nodes = nodes, links = links), source = 'source', target = 'target', linkWidth = 'outputs', key = 'name', label = 'description',
                       color = 'type', config = list(...), linkLabel = 'outputs', type = 'graph', plotter = plotter)

    },

    # info.transformers = function(){
    #   translist = data.frame()
    #   translist
    #   for(tr in transformers){
    #     mdlns = c(mdlns, tr$info.transformer_names())
    #   }
    #   return(mdlns %>% unique)
    # },

    model.save = function(path = getwd()){
      if(!file.exists(path)) {dir.create(path)}
      for(tr in transformers){
        tr$model.save(path)
      }
      for(gtr in gradient_transformers){
        gtr$model.save(path)
      }
    },

    model.load = function(path = getwd()){
      for(tr in transformers){
        tr$model.load(path)
      }
      for(gtr in gradient_transformers){
        gtr$model.load(path)
      }
    },

    # todo: add k-fold, chronological shuffle, chronological split
    performance.cv = function(X, y){
      if(config$cv.restore_model){
        keep   = list(objects = objects, fitted = fitted, config = config)
      }

      # Split by shuffling: todo: support other splitting methods(i.e.: chronological)
      N       = nrow(X)

      scores = c()

      for (i in sequence(config$cv.ntrain)){
        ind_train = N %>% sequence %>% sample(size = floor(config$cv.train_ratio*N), replace = F)

        X_train = X[ind_train, ,drop = F]
        y_train = y[ind_train]

        reset(config$cv.reset_transformer)
        .self$fit(X_train, y_train)

        if(is.null(config$cv.set)){
          for(j in sequence(config$cv.ntest)){
            N2 = N - length(ind_train)
            ind_test = sequence(N) %>% setdiff(ind_train) %>% sample(size = floor(config$cv.test_ratio*N2), replace = F)
            X_test  = X[ind_test, objects$features$fname, drop = F]
            y_test  = y[ind_test]
            scores = c(scores, .self$performance(X_test, y_test, metric = config$cv.performance_metric))
          }
        } else {
          for(vset in config$cv.set){
            scores = c(scores, .self$performance(vset$X[objects$features$fname], vset$y, metric = config$cv.performance_metric))
          }
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

    info.size             = function(){
      t_size = list(config = 0, objects = 0, transformers = 0)
      for(tr in transformers){
        slist          = tr$info.size()
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
