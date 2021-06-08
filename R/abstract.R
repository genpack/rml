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
  fields = list(name = "character", type = "character", package = "character", package_language = "character", packages_required = "character", description = "character", reserved_words = "character",
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
      
      
      packages_required <<- c('magrittr', 'dplyr', 'rutils', 'rml', 'rbig', 'reticulate')

      reserved_words <<- c('keep_columns', 'keep_features', 
                         'name_in_output', 'metrics', 
                         'cv.ntrain', 'cv.ntest', 'cv.test_ratio','cv.train_ratio', 'cv.split_method', 
                         'cv.reset_transformer', 'cv.restore_model', 'cv.set', 
                         'sfs.enabled', 
                         'fe.enabled','fe.recursive', 'fe.importance_threshold', 'fe.quantile',
                         'mc.enabled', 'mc.num_cores',
                         'remove_failed_transformers',
                         'pp.coerce_integer_features',
                         'pp.trim_outliers', 'pp.trim_outliers.adaptive', 'pp.trim_outliers.recursive', 'pp.trim_outliers.sd_threshold',
                         'pp.mask_missing_values',
                         'model.class', 'model.config', 'model.module',
                         'eda.enabled', 
                         'verbose', 'pass_columns', 'remove_columns', 'name', 'column_filters',
                         # smp.enabled: boolean parameter. default = FALSE. Should I do any sampling of the training rows at all? If FALSE (Default) 
                         #              model will be trained by the entire training data table (X) without any changes.
                         'smp.enabled', 'smp.class_ratio', 'smp.sample_ratio', 'smp.num_rows', 'smp.method', 'smp.config',
                         'features.include', 'features.include.at', 'features.exclude', 'features.exclude.at',
                         'ts.enabled', 'ts.id_col', 'ts.time_col', 
                         'return_features', 'feature_subset_size', 'gradient_transformers_aggregator', 'save_predictions',
                         'metric', 'transformers', 'fitted')
      
      for(pn in c('keep_columns', 'keep_features', 'cv.restore_model', 'fe.enabled', 'mc.enabled', 
                  'pp.coerce_integer_features', 'pp.trim_outliers',
                  'eda.enabled', 'smp.enabled', 'ts.enabled', 'save_predictions', 'remove_failed_transformers')){
        settings[[pn]] <- verify(settings[[pn]], 'logical', lengths = 1, domain = c(T,F), default = F)
      }
      
      for(pn in c('cv.reset_transformer', 'name_in_output')){
        settings[[pn]] <- verify(settings[[pn]], 'logical', lengths = 1, domain = c(T,F), default = T)
      }
      
      settings$verbose <- verify(config$verbose, c('numeric', 'integer'), lengths = 1, default = 1) %>% as.integer
      settings$column_filters  <- verify(settings$column_filters, 'list', default = list(list(column = 'n_unique', filter = ' > 1')))

      settings$metrics   <- verify(settings$metrics,  'character', default = 'pearson')
      settings$cv.split_method <- verify(settings$cv.split_method, lengths = 1, 'character', default = 'shuffle')
      
      settings$cv.ntest  <- verify(settings$cv.ntest , c('numeric', 'integer'), lengths = 1, domain = c(1, Inf), default = 5) %>% as.integer
      settings$cv.ntrain <- verify(settings$cv.ntrain, c('numeric', 'integer'), lengths = 1, domain = c(1, Inf), default = 1) %>% as.integer
      settings$cv.train_ratio <- verify(settings$cv.train_ratio, c('numeric', 'integer'), lengths = 1, domain = c(0, 1), default = 0.5) %>% as.numeric
      settings$cv.test_ratio  <- verify(settings$cv.test_ratio, c('numeric', 'integer') , lengths = 1, domain = c(0, 1), default = 0.2) %>% as.numeric
      settings$fe.importance_threshold  <- verify(settings$fe.importance_threshold, c('numeric', 'integer') , lengths = 1, default = 0) %>% as.numeric

      # if(is.null(settings$ts.id_col)){settings$ts.id_col = 'caseID'}
      # if(is.null(settings$ts.time_col)){settings$ts.time_col = 'time'}
      if(is.null(settings$gradient_transformers_aggregator)){settings$gradient_transformers_aggregator = mean}

      config       <<- settings
      fitted       <<- FALSE
      transformers <<- transformers %>% {if(inherits(.,'list')) . else list(.)}
      gradient_transformers <<- gradient_transformers %>% {if(inherits(.,'list')) . else list(.)}
      objects$mother        <<- mother
      objects$features      <<- features
      objects$pupils        <<- pupils
    },

    reset               = function(reset_transformers = T, reset_gradient_transformers = T, set_features.include = T){
      fitted <<- FALSE
      if(set_features.include & (length(transformers) == 0)){
        config$features.include <<- objects$features$fname
      }
      objects$features <<- NULL
      objects$model    <<- NULL
      objects$saved_pred <<- NULL
      if (reset_transformers & !is.empty(transformers)){
        for (transformer in transformers) transformer$reset(reset_transformers = T, reset_gradient_transformers = reset_gradient_transformers, set_features.include = set_features.include)
      }
      if (reset_gradient_transformers & !is.empty(gradient_transformers)){
        for (transformer in gradient_transformers) transformer$reset(reset_transformers = reset_transformers, reset_gradient_transformers = T, set_features.include = set_features.include)
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
      zero = objects$features$fname %-% rbig::colnames(XORG)
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
      if((ncol(XOUT) > 0) & config$name_in_output) colnames(XOUT) <- name %>% paste(colnames(XOUT), sep = '_')

      objects$n_output <<- ncol(XOUT)
      treat(XOUT, XFET, XORG)
    },

    treat                = function(out, fet, org){
      if(!is.null(config$pass_columns)) org = org[config$pass_columns]
      if(!is.null(config$remove_columns)) org = org[colnames(org) %-% config$remove_columns]

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
          if(is.empty(objects$features)){
            # .self$fit.distribution(X, y); fte = c()
            stop('No features left for training the model!')
          } else {
            .self$model.fit(X[objects$features$fname], y)
            fns = objects$features$fname
            ftk = fns[which(objects$features$importance > threshold)] # features to keep
            fte = chif(config$fe.recursive, fns %-% ftk, c())
          }
        }

        if(is.empty(objects$features)){
          cat(name, ': ', 'No features left after elimination! Distribution fitted for output variable.', '\n')
        }
      }
    },

    fit.quad = function(X, y){
      fit.fe(X, y)
      clmns = rbig::colnames(X)
      XP    = X[objects$features$fname]
      for(i in 1:ncol(X)){
        v  = X[,i]
        Xi <- X %>% as.matrix %>% apply(2, function(u) u*v) %>% as.data.frame %>% {colnames(.) <- clmns %>% paste(clmns[i], sep = 'X');.}
        XP = XP %>% cbind(Xi)
        objects$features <<- rbig::colnames(XP) %>% sapply(function(i) XP %>% pull(i) %>% class) %>% as.data.frame %>% {colnames(.)<-'fclass';.} %>% rownames2Column('fname') %>% mutate(fname = as.character(fname), fclass = as.character(fclass))
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
        y = transform_yin(X, y)
        if(!is.null(config[['features.include']])){X = X[config$features.include %^% rbig::colnames(X)]}
        if(!is.null(config[['features.exclude']])){X = X[rbig::colnames(X) %-% config$features.exclude]}
        X = transform_x(X, y)
        if(!is.null(config[['features.include.at']])){X = X[config$features.include.at %^% rbig::colnames(X)]}
        if(!is.null(config[['features.exclude.at']])){X = X[rbig::colnames(X) %-% config$features.exclude.at]}
        if(config$smp.enabled){
          smp_method <- verify(config$smp.method, 'character', domain = c('upsample', 'downsample', 'smote'), default = 'upsample') 

          actual_ratio = mean(y)
          n_train_rows = length(y)
          
          if(!is.null(verify(config$smp.class_ratio, 'numeric', domain = c(.Machine$double.eps, 1 - .Machine$double.eps), null_allowed = T))){
            if(smp_method == 'downsample'){
              if(config$smp.class_ratio >= actual_ratio){
                # downsample negative class, keep all positive class:
                w1 = which(y == 1)
                n1 = length(w1)
                rr = config$smp.class_ratio
                w0 = which(y == 0) %>% sample(as.integer(n1*(1.0 - rr)/rr))
              } else {
                # downsample positive class, keep all negative class:
                w0 = which(y == 0)
                n0 = length(w0)
                rr = config$smp.class_ratio
                w1 = which(y == 1) %>% sample(as.integer(n0*(1.0 - rr)/rr))
              }
            } else if(smp_method == 'upsample'){
              if(config$smp.class_ratio >= actual_ratio){
                # upsample positive class, keep all negative class:
                w0 = which(y == 0)
                n0 = length(w0)
                rr = config$smp.class_ratio
                w1 = which(y == 1) %>% sample(as.integer(n0*rr/(1.0 - rr)), replace = T)
              } else {
                # upsample negative class, keep all positive class:
                w1 = which(y == 1)
                n1 = length(w1)
                rr = config$smp.class_ratio
                w0 = which(y == 0) %>% sample(as.integer(n1*(1.0 - rr)/rr), replace = T)
              }
            } else if (smp_method == 'smote'){
              sv = reticulate::import('smote_variants')
              smote_model = verify(config$smp.config[['model']], 'character', domain = names(sv), lengths = 1, default = 'distance_SMOTE')
              config$smp.config[['proportion']] <<- verify(config$smp.config[['proportion']], 'numeric', lengths = 1, default = config$smp.class_ratio/(1.0 - config$smp.class_ratio))
              
              oversampler = do.call(sv[[smote_model]], config$smp.config %>% list.remove('model'))
              res = oversampler$sample(X %>% as.matrix, y)
              X  = res[[1]] %>% as.data.frame %>% {colnames(.) <- colnames(X);.}
              y  = res[[2]] %>% as.numeric
              w0 = which(y == 0)
              w1 = which(y == 1)
            } else {stop('Unknown sampling method specified!')}
            ind = c(w0, w1) %>% sample(length(c(w0, w1))) 
            # X = X[ind,]; y = y[ind]
            n_train_rows = length(ind)
          } else {ind = sequence(length(y))}
          
          n_rows = verify(config$smp.num_rows, c('numeric', 'integer'), lengths = 1, domain = c(1, Inf), default = n_train_rows) %>% 
            min(n_train_rows) %>% {.*verify(config$smp.sample_ratio, 'numeric', lengths = 1, domain = c(0, 1), default = 1.0)} %>% as.integer
          
          ind_2 = n_train_rows %>% sequence %>% sample(n_rows)
          X = X[ind[ind_2],]; y = y[ind[ind_2]]
        }
        
        assert(ncol(X) > 0, 'No column found in the training dataset!')

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

        if(inherits(config$column_filters, 'list')){
          for(fitem in config$column_filters){
            verify(fitem, 'list', names_include = c('column', 'filter'))
            assert(is.character(fitem$column), "Element 'column' must be character. %s was of class %s!" %>% sprintf(fitem$column, class(fitem$column)[1]))
            assert(is.character(fitem$filter), "Element 'filter' must be character. %s was of class %s!" %>% sprintf(fitem$filter, class(fitem$filter)[1]))
            if((fitem$column == 'n_unique') & !('n_unique' %in% colnames(objects$features))){
              objects$features$n_unique <<- rbig::colnames(X) %>% sapply(function(x) X %>% pull(x) %>% unique %>% length) %>% unlist
            }
            script = 'objects$features'
            script %<>% paste("filter(%s %s)" %>% sprintf(fitem$column, fitem$filter), sep = " %>% ")
            objects$features <<- parse(text = script) %>% eval
          }
          X = X[objects$features$fname]
        }
        
        assert(nrow(objects$features) > 0, 'No features left for training!')
        
        if(config$fe.enabled) {
          config$fe.recursive <<- verify(config$fe.recursive, 'logical', lengths = 1, domain = c(T,F), default = F)
          fit.fe(X, y)
        } else {.self$model.fit(X, y)}
        # todo: if(config$quad.enabled) {fit.quad(X, y)}
      }
      fitted <<- TRUE
      objects$fitting_time <<- Sys.time()
    },

    transform_x = function(X, y = NULL){
      
      nt = length(transformers)
      if(nt > 0){
        ## Fitting:
        transformers <<- fit_models(transformers, X = X, y = y, num_cores = config$mc.num_cores, verbose = config$verbose, 
                                      remove_failed_models = config$remove_failed_transformers)
        # # (uft: unfitted transformers)
        # uft = transformers %>% lapply(function(x) {!x$fitted}) %>% unlist %>% which
        # num_cores = 1
        # if(config$mc.enabled){
        #   ncores_available = parallel::detectCores()
        #   num_cores <- verify(config$mc.num_cores, c('numeric', 'integer'), lengths = 1, domain = c(1, ncores_available), default = ncores_available - 1) %>% as.integer
        # }
        # if(config$mc.enabled & (num_cores > 1) & (length(uft) > 1)){
        #   requirements = transformers %>% lapply(function(x) x$packages_required) %>% unlist %>% unique
        #   cl  = rutils::setup_multicore(n_jobs = num_cores)
        #   if(!config$silent){cat('\n', 'Fitting  %s transformers ... ' %>% sprintf(length(uft)))}
        #   transformers <<- foreach(transformer = transformers, 
        #                            .combine = c, .packages = requirements, 
        #                            .errorhandling = rutils::chif(config$remove_failed_transformers, 'remove', 'stop')) %dopar% {
        #     if(!transformer$fitted){
        #       transformer$fit(X, y)
        #     }
        #     gc()
        #     list(transformer)
        #   }
        #   stopCluster(cl)
        #   gc()
        #   if(!config$silent){cat('Done!')}
        # } else {
        #   for(i in uft){
        #       transformer = transformers[[i]]
        #       if(!config$silent){
        #         cat('\n', 'Fitting transformer %s of type %s: %s ... ' %>% sprintf(transformer$name, transformer$type, transformer$description))
        #       }
        #       res = try(transformer$fit(X, y), silent = config$remove_failed_transformers)
        #       if(!config$silent){
        #         rutils::warnif(inherits(res, 'try-error'), sprintf("Transformer %s failed to fit!", transformer$name), as.character(res))
        #         cat('Done!')
        #       }
        #     }  
        # }
        # ## Remove unfitted transformers:
        # # ft: fitted transformers
        # ft = transformers %>% lapply(function(x) {x$fitted}) %>% unlist %>% which
        # nt = length(ft)
        # warnif(nt < length(transformers), sprintf("%s transformers failed to fit and removed!", length(transformers) - nt))
        # transformers <<- transformers %>% list.extract(ft)
        # assert(nt == length(transformers))
        nt = length(transformers)
      }
      if(nt > 0){
        ## Prediction:
        XT = predict_models(transformers, X = X, num_cores = config$mc.num_cores, verbose = config$verbose)
        # if(config$mc.enabled & (num_cores > 1) & (nt > 1)){
        #   requirements = transformers %>% lapply(function(x) x$packages_required) %>% unlist %>% unique
        #   cl = rutils::setup_multicore(n_jobs = num_cores)
        #   if(!config$silent){cat('\n', 'Generate transformed columns from %s transformers ... ' %>% sprintf(nt))}
        #   XT = foreach(transformer = transformers, .combine = cbind, .packages = requirements, 
        #                .errorhandling = rutils::chif(config$remove_failed_transformers, 'remove', 'stop')) %dopar% {
        #     gc()
        #     transformer$predict(X)
        #   }
        #   stopCluster(cl)
        #   gc()
        #   if(!config$silent){
        #     cat('Done!')
        #   }
        # } else {
        #   for(i in sequence(nt)){
        #     transformer = transformers[[i]]
        #     if(!config$silent){
        #       cat('\n', 'Generate transformed columns from transformer %s ... ' %>% sprintf(transformer$name))
        #     }
        #     if(i == 1){
        #       XT = transformer$predict(X)
        #     } else {
        #       XT = cbind(XT, transformer$predict(X) %>% {.[colnames(.) %-% colnames(XT)]})
        #     }
        #     if(!config$silent){
        #       cat('Done!')
        #     }
        #   }
      } else {XT = X}
      
      # Apply preprocessing:
      # todo: missing values should be treated differently for each column or groups of columns or classes of columns
      # todo: missing values could be imputed by aggregations of non-missing values like mean, median or most_frequent
      if(!is.null(config$pp.mask_missing_values)){
        # This will edit the table, so WideTables cannot be used yet.
        if(inherits(XT, 'WIDETABLE')){XT = rbig::as.data.frame(XT)}
        for(i in sequence(ncol(XT))){
          wna = which(is.na(XT[[i]]))
          if(length(wna) > 0){
            XT[wna, i] <- config$pp.mask_missing_values[1] %>% rutils::coerce(class(XT[wna, i])[1])
          }
        }
      }
      if(config$pp.coerce_integer_features){
        XT %<>% int_ordinals
      }
      # todo: remove outliers can be considered as a transformer as well. Add transformers (transformer type: preprocessor)
      if(config$pp.trim_outliers){
        adapt = verify(config$pp.trim_outliers.adaptive, 'logical', default = F)
        recur = verify(config$pp.trim_outliers.recursive, 'logical', default = F)
        sdcut = verify(config$pp.trim_outliers.sd_threshold, 'numeric', domain = c(0,Inf), default = 4)
        XT %<>% trim_outliers(sd_threshold = sdcut, adaptive = adapt, recursive = recur)
      }
      
      return(XT)
    },

    y_gradient = function(X, y = NULL){
      nt = length(gradient_transformers)
      if(nt > 0){
        for(i in sequence(nt)){
          transformer = gradient_transformers[[i]]
          if(inherits(transformer, 'character')){
            assert(transformer %in% rbig::colnames(X))
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
                  language = package_language, outputs = objects$n_output, fitted = fitted) %<==>%
       list.extract(config, c('keep_columns', 'keep_features', 'smp.enabled', 'smp.class_ratio', 'smp.sample_ratio',
                                  'smp.method', 'fe.enabled', 'metric')) %>% list.clean
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

      rvis::viserPlot(list(nodes = nodes, links = links), source = 'source', target = 'target', linkWidth = 'outputs', key = 'name', label = 'description',
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

    # if name_suffix is specified, it will be added to the model names and all its transformers and gradient transformers
    deep_copy = function(name_suffix = NULL){
      obj = .self$copy()
      if(!is.null(name_suffix)){obj$name = obj$name %>% paste0(name_suffix)}
      for (i in sequence(length(transformers))){
        obj$transformers[[i]] <- transformers[[i]]$deep_copy(name_suffix = name_suffix)
      }
      for (i in sequence(length(gradient_transformers))){
        obj$gradient_transformers[[i]] <- gradient_transformers[[i]]$deep_copy(name_suffix = name_suffix)
      }
      return(obj)
    },
    
    # todo: add k-fold, chronological shuffle, chronological split
    performance.cv = function(X, y, metrics = config$metrics, ...){
      cvmodel = .self$copy()

      # Split by shuffling: todo: support other splitting methods(i.e.: chronological)
      N       = nrow(X)

      scores = list()

      for (i in sequence(cvmodel$config$cv.ntrain)){
        ind_train = N %>% sequence %>% sample(size = floor(config$cv.train_ratio*N), replace = F)

        X_train = X[ind_train, ,drop = F]
        y_train = y[ind_train]

        cvmodel$reset(config$cv.reset_transformer)
        cvmodel$fit(X_train, y_train)

        if(is.null(config$cv.set)){
          for(j in sequence(config$cv.ntest)){
            N2       = N - length(ind_train)
            ind_test = sequence(N) %>% setdiff(ind_train) %>% sample(size = floor(config$cv.test_ratio*N2), replace = F)
            X_test   = X[ind_test, , drop = F]
            y_test   = y[ind_test]
            scores[[length(scores) + 1]]  <- cvmodel$performance(X_test, y_test, metrics = metrics, ...)
          }
        } else {
          for(vset in config$cv.set){
            scores = c(scores, cvmodel$performance(vset$X, vset$y, metrics = config$metrics))
          }
        }
      }

      # while((length(scores) == 1) & inherits(scores, 'list')) scores = scores[[1]]
      # if(inherits(scores, 'list')) {
      #   cls = scores %>% lapply(class) %>% unlist
      #   if(length(unique(cls)) == 1) {scores %<>% unlist}
      # }
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

