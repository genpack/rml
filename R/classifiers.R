#' @include transformers.R

#' @export
CLASSIFIER = setRefClass('CLASSIFIER', contains = "MODEL",
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Binary Classifier'
      config$sig_level <<- verify(config$sig_level, 'numeric', domain = c(0,1), default = 0.1)
      config[['return']]    <<- verify(config[['return']], 'character', domain = c('probs', 'logit', 'class'), default = 'probs')
      config[['threshold']] <<- verify(config[['threshold']], 'numeric', domain = c(0,1), default = 0.5, lengths = 1)
      # config$quantiles <<- verify(config$quantiles, 'numeric', domain = c(0,1), default = 0.5)
      config$threshold_determination <<- verify(config$threshold_determination, 'character', lengths = 1,
                                                domain = c('set_as_config', 'maximum_f1', 'maximum_chi', 'label_rate_quantile'), default = 'set_as_config')
      # todo: add 'target_precision', 'target_recall', 
      reserved_words <<- c(reserved_words, 'sig_level', 'return', 'threshold', 'threshold_determination')
      if(!('metrics' %in% names(list(...)))){
        config$metrics <<- chif(config$return == 'class', 'f1', 'gini')
      }
    },
    
    # Since this class is being used as a second parent class, it requires another init method.
    init = function(...){
      type             <<- 'Binary Classifier'
      config$sig_level <<- verify(config$sig_level, 'numeric', domain = c(0,1), default = 0.1)
      config$return    <<- verify(config$return, 'character', domain = c('probs', 'logit', 'class'), default = 'probs')
      config$threshold <<- verify(config$threshold, 'numeric', lengths = 1, domain = c(0,1), default = 0.5)
      config$threshold_determination <<- verify(config$threshold_determination, 'character', lengths = 1,
                                                domain = c('set_as_config', 'maximum_f1', 'maximum_chi'), default = 'set_as_config')
      # todo: add 'target_precision', 'target_recall', 'ratio_quantile'
      reserved_words <<- c(reserved_words, 'sig_level', 'return', 'threshold', 'threshold_determination')
      packages_required <<- c(packages_required, 'stats')
      if(is.null(config$metric)){
        config$metric <<- chif(config$return == 'class', 'f1', 'gini')
      }
    },

    fit = function(X, y){
      if(!fitted){
        callSuper(X, y)
        if(is.null(objects$features$importance)){
          objects$features$importance <<- 1.0/nrow(objects$features)
        }

        set_threshold(X, y)
        objects$n_output <<- as.integer(1)
      }
    },

    transform_yout = function(X, Y = NULL){
      has_gradient = length(gradient_transformers) > 0
      if (config$return == 'class'){for(i in sequence(ncol(Y))) {Y[,i] = as.numeric(Y[,i] > config$threshold)}} else
      if (config$return == 'logit' | has_gradient){Y %<>% as.matrix %>% apply(2, logit_fwd) %>% as.data.frame}
      Y = callSuper(X, Y)
      if (config$return == 'probs' & has_gradient){Y %<>% as.matrix %>% apply(2, logit_inv) %>% as.data.frame}
      colnames(Y) <- config$return
      return(Y)
    },

    set_threshold = function(X, y){
      if(fitted){
        if(config$threshold_determination == 'maximum_f1'){
          y_prob = predict(X) %>% pull(name %>% paste('out', sep = '_'))
          res = data.frame(y_pred = y_prob, y_true = y) %>% optSplit.f1('y_pred', 'y_true')
          config$threshold <<- res$split
        }
        else if(config$threshold_determination == 'maximum_chi'){
          y_prob = predict(X) %>% pull(name %>% paste('out', sep = '_'))
          res = data.frame(y_pred = y_prob, y_true = y) %>% optSplit.chi('y_pred', 'y_true')
          config$threshold <<- res$split
        }
        else if(config$threshold_determination == 'label_rate_quantile'){
          y_prob = predict(X) %>% pull(name %>% paste('out', sep = '_'))
          config$threshold <<- quantile(y_prob, probs = 1 - mean(y, na.rm = T))
        }
        #todo: do for other options
      }
    },

    performance = function(X, y, metrics = config$metrics, ...){
      yp = predict(X)[, 1]
      correlation(yp, y, metrics = metrics, ...)
    }
  )
)

#' @export CLS.SKLEARN
CLS.SKLEARN = setRefClass('CLS.SKLEARN', contains = c('TRM.SKLEARN', "CLASSIFIER"),
   methods = list(
     initialize = function(...){
       callSuper(...)
       init()
     },
     model.predict = function(X){
       if(inherits(X, 'WIDETABLE')){X = rbig::as.matrix(X)}
       objects$model$predict_proba(X %>% data.matrix)[,2, drop = F] %>% as.data.frame
     }
   )
)

#' @export CLS.SKLEARN.KNN
CLS.SKLEARN.KNN = setRefClass('CLS.SKLEARN.KNN', contains = "CLS.SKLEARN",
  methods = list(
    initialize = function(...){
      callSuper(model.module = 'neighbors', model.class = 'KNeighborsClassifier', ...)
      description <<- 'K-Nearest Neighbors'

      # config$num_neighbors <<- verify(config$num_neighbors, c('numeric', 'integer'), default = 100) %>% as.integer
    }
  )
)

#' @export CLS.HDPENREG.FLASSO
CLS.HDPENREG.FLASSO = setRefClass('CLS.HDPENREG.FLASSO', contains = 'CLASSIFIER', methods = list(
  initialize = function(...){
    callSuper(...)
    package     <<- 'HDPenReg'
    package_language <<- 'R'
    packages_required <<- c(packages_required, 'HDPenReg')
    description <<- 'Logistic Regression with Fusion Lasso'
    if(is.empty(name)){name <<- 'FLS' %>% paste0(sample(10000:99999, 1))}

    config$lambda1 <<- verify(config$lambda1, 'numeric', default = 1)
    config$lambda2 <<- verify(config$lambda2, 'numeric', default = 1)
    config$model   <<- verify(config$model,   'character', default = 'logistic')
  },

  model.fit = function(X, y){
    objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
    X = X[objects$features$fname]
    objects$model <<- do.call(HDPenReg::EMfusedlasso, list(X = X %>% as.matrix, y = y) %<==>% (config %>% list.remove(reserved_words)))
    objects$features$importance <<- objects$model$coefficient
  },

  model.predict = function(X){
    X %>% as.matrix %*% objects$model$coefficient %>% logit_inv %>% as.data.frame
  }
))

#' @export CLS.MLR
CLS.MLR = setRefClass('CLS.MLR', contains = "CLASSIFIER",
    methods = list(
      initialize = function(...){
        library(mlr)
        callSuper(...)
        description <<- 'MLR Model'
        package     <<- 'mlr'
        package_language <<- 'R'
        packages_required <<- c(packages_required, 'mlr')
        
        config$mlr_classification_models <<- mlr::listLearners('classif')

        if(is.empty(name)){name <<- 'MLR' %>% paste0(sample(10000:99999, 1))}
        config$model_type <<- verify(config$model_type, 'character', domain = config$mlr_classification_models %>% pull(class), default = 'classif.gbm')
        objects$model     <<- mlr::makeLearner(cl = config$model_type, predict.type = "prob")
      },

      model.fit = function(X, y){
          if(!inherits(y, 'factor')){y %<>% as.factor; assert(length(levels(y)) == 2)}

          tsk = mlr::makeClassifTask(data = cbind(X, label = y), target = 'label')
          mlr::train(objects$model, tsk) ->> objects$model
      },

      model.predict = function(X){
        stats::predict(objects$model, newdata = X) -> pred
        getPredictionProbabilities(pred) %>% as.data.frame
      }
    )
)

# A simple logistic regression classifier from SKLEARN python package:
# It extracts only numeric features, does no dummification for categorical columns.
#' @export CLS.SKLEARN.LR
CLS.SKLEARN.LR = setRefClass('CLS.SKLEARN.LR', contains = "CLS.SKLEARN",
    methods = list(
      initialize = function(...){
        callSuper(model.module = 'linear_model', model.class = 'LogisticRegression', ...)
        description <<- 'Logistic Regression'
        if(is.empty(name)){name <<- 'SKLR' %>% paste0(sample(10000:99999, 1))}
      },

      model.fit = function(X, y){
          callSuper(X, y)
          X = X[objects$features$fname]
          objects$model$coef_ %>% abs %>% as.numeric -> weights
          assert(sum(weights > 0) > 0, "All coefficients are zero. SKLEARN-LR Model training failed!")
          # objects$features$importance <<- (weights/(X %>% apply(2, sd))) %>% na2zero %>% {./geomean(.[.>0])} %>% as.numeric
          objects$features$importance <<- (weights/(X %>% apply(2, sd))) %>% na2zero %>% {./geomean(.[.>0])} %>% vect.normalise %>% as.numeric
      },

      get.function = function(...){
        assert(fitted, 'Model not fitted!')
        build_lincomb(name = name, length(objects$features$fname), features = objects$features$fname, parameter_values = c(as.numeric(objects$model$intercept_), as.numeric(objects$model$coef_)), ...)
      }
   )
)

# A simple decision tree classifier from SKLEARN python package:
#' @export CLS.SKLEARN.DT
CLS.SKLEARN.DT = setRefClass('CLS.SKLEARN.DT', contains = "CLS.SKLEARN",
  methods = list(
    initialize = function(...){
      callSuper(model.module = 'tree', model.class = 'DecisionTreeClassifier', ...)
      description <<- 'Decision Tree'
      if(is.empty(name)){name <<- 'SKDT' %>% paste0(sample(10000:99999, 1))}
    }
  )
)

#' @export CLS.STATS.LR
CLS.STATS.LR = setRefClass('CLS.STATS.LR', contains = "CLASSIFIER",
                         methods = list(
                           initialize = function(...){
                             type             <<- 'Classifier'
                             description      <<- 'Logistic Regression'
                             package          <<- 'stats'
                             package_language <<- 'R'
                             
                             callSuper(...)
                             config$family <<- binomial
                             if(is.empty(name)){name <<- 'GLMLR' %>% paste0(sample(10000:99999, 1))}
                           },
                           
                           get.features.weight = function(){
                             objects$model.summary <<- summary(objects$model)
                             pv   = objects$model.summary$coefficients[-1, 'Pr(>|t|)'] %>% na2zero
                             # Usually NA p-values appear when there is a perfect fit (100% R-squared), so each feature shall be considerd as important!?
                             keep = (pv < 0.1)
                             weights = pv
                             weights[!keep] <- 0
                             weights[keep]  <- 1.0 - weights[keep]/0.1
                             return(weights)
                           },
                           
                           model.fit = function(X, y){
                             arguments = config %>% list.remove(reserved_words)
                             arguments$data <- X %>% cbind(y = y)
                             arguments$formula <- "%s ~ %s" %>% sprintf('y', colnames(X) %>% paste(collapse = ' + ')) %>% as.formula
                             arguments$family <- stats::binomial
                             objects$model <<- do.call(stats::glm, args = arguments)
                             # Feature Importances:
                             gw = get.features.weight()
                             objects$features$importance <<- gw[objects$features$fname]
                           },
                           
                           model.predict = function(X){
                             if(inherits(X, 'WIDETABLE')){X = rbig::as.data.frame(X)}
                             objects$model %>% stats::predict(X %>% na2zero, type = "response") %>% as.data.frame
                           }
                           
                           # get.function = function(...){
                           #   assert(fitted, 'Model not fitted!')
                           #   xseq = paste0('x', sequence(nrow(objects$features)))
                           #   ldep = list(output = objects$features$fname)
                           #   for(pm in xseq){ldep[[pm]] <- objects$features$fname}
                           #   if(is.empty(transformers)){
                           #     fun = new('FUNCTION',
                           #               inputs        = objects$features$fname %>% as.list %>% {names(.)<-xseq;.},
                           #               objects       = list(model = objects$model),
                           #               rule.output   = function(inputs, params, objects){
                           #                 if(inherits(X, 'WIDETABLE')){X = rbig::as.matrix(X)}
                           #                 objects$model$predict_proba(inputs %>% as.data.frame %>% data.matrix)[,2] %>% as.numeric
                           #               },
                           #               local_dependencies = ldep)
                           #     
                           #     if(config$return== 'logit'){
                           #       out = logit$copy()
                           #       out$inputs$x <- fun
                           #     } else {out = fun}
                           #     
                           #     out$name = paste('FN', name, sep = '_')
                           #   }
                           #   return(out)
                           # }
                         )
)


#' @export CLS.SKLEARN.XGB
CLS.SKLEARN.XGB = setRefClass('CLS.SKLEARN.XGB', contains = "CLASSIFIER",
    methods = list(
      initialize = function(...){
        config$model.module <<- 'xgboost'
        config$model.class  <<- 'XGBClassifier'
        
        if(!require(reticulate)) stop("Package 'reticulate' is not installed!")
        if(!is.null(config$python_address)){
          use_python(config$python_address)
        }

        type             <<- 'Classifier'
        description      <<- 'Extreme Gradient Boosting'
        package          <<- 'sklearn'
        package_language <<- 'Python'
        
        callSuper(...)
        if(is.empty(name)){name <<- 'SKXGB' %>% paste0(sample(10000:99999, 1))}
        if(!is.null(config$n_jobs)){config$n_jobs <<- as.integer(config$n_jobs)}
        objects$module <<- reticulate::import('xgboost')
      },
      
      model.save = function(path = getwd()){
        callSuper(path)
        joblib = reticulate::import('joblib')
        joblib$dump(objects$model, paste0(path, '/', name, '.joblib'))
      },
      
      model.load = function(path = getwd()){
        callSuper(path)
        fn   = paste0(path, '/', name, '.joblib')
        pass = file.exists(fn)
        warnif(!pass, paste0('File ', fn , ' does not exist!'))
        if(pass){
          joblib = reticulate::import('joblib')
          objects$model  <<- joblib$load(fn)
          objects$module <<- reticulate::import('xgboost')
        }
      },
      
      model.fit = function(X, y){
        if(inherits(X, 'WIDETABLE')){X = rbig::as.matrix(X)}
        objects$model <<- do.call(objects$module$XGBClassifier, config %>% list.remove(reserved_words))
        objects$model$fit(X %>% data.matrix, y)
          imp = try(objects$model$feature_importances_ %>% as.numeric, silent = T)
          if(inherits(imp, 'numeric')) objects$features$importance <<- imp
      },

      model.predict = function(X){
        if(inherits(X, 'WIDETABLE')){X = rbig::as.matrix(X)}
        objects$model$predict_proba(X %>% data.matrix)[,2, drop = F] %>% as.data.frame
      },
      
      get.function = function(...){
        assert(fitted, 'Model not fitted!')
        xseq = paste0('x', sequence(nrow(objects$features)))
        ldep = list(output = objects$features$fname)
        for(pm in xseq){ldep[[pm]] <- objects$features$fname}
        if(is.empty(transformers)){
          fun = new('FUNCTION',
                    inputs        = objects$features$fname %>% as.list %>% {names(.)<-xseq;.},
                    objects       = list(model = objects$model),
                    rule.output   = function(inputs, params, objects){
                      if(inherits(X, 'WIDETABLE')){X = rbig::as.matrix(X)}
                      objects$model$predict_proba(inputs %>% as.data.frame %>% data.matrix)[,2] %>% as.numeric
                    },
                    local_dependencies = ldep)

          if(config$return== 'logit'){
            out = logit$copy()
            out$inputs$x <- fun
          } else {out = fun}

          out$name = paste('FN', name, sep = '_')
        }
        return(out)
      }
    )
)

#' @export CLS.SKLEARN.SVM
CLS.SKLEARN.SVM = setRefClass('CLS.SKLEARN.SVM', contains = "CLS.SKLEARN",
   methods = list(
     initialize = function(...){
       callSuper(model.module = 'svm', model.class = 'SVC', ...)
       description <<- 'Support Vector Machine'
       config$probability <<- T
       
       if(is.empty(name)){name <<- 'SKSVM' %>% paste0(sample(10000:99999, 1))}
     }
   )
)

#' @export CLS.KERAS.DNN
CLS.KERAS.DNN = setRefClass('CLS.KERAS.DNN', contains = 'CLASSIFIER',
  methods = c(
    initialize = function(...){
      callSuper(...)
      description       <<- 'Deep Neural Network'
      package           <<- 'keras'
      package_language  <<- 'python'
      packages_required <<- c(packages_required, 'keras', 'tensorflow')
      

      library(tensorflow)
      library(keras)
      if(is.empty(name)){name <<- 'KRSNN' %>% paste0(sample(10000:99999, 1))}

      # config$outputs <<- config$outputs %>% verify(c('numeric', 'integer'), lengths = 1, default = 2) %>% as.integer
      config$kernel_regularization_penalty_l1    <<- verify(config$kernel_regularization_penalty_l1,   'numeric', lengths = 1, default = 0.0)
      config$kernel_regularization_penalty_l2    <<- verify(config$kernel_regularization_penalty_l2,   'numeric', lengths = 1, default = 0.0)
      config$activity_regularization_penalty_l1  <<- verify(config$activity_regularization_penalty_l1, 'numeric', lengths = 1, default = 0.0)
      config$activity_regularization_penalty_l2  <<- verify(config$activity_regularization_penalty_l2, 'numeric', lengths = 1, default = 0.0)
      if(!inherits(config$kernel_initializer, 'character')){
        if(!inherits(config$kernel_initializer, 'keras.initializers.Initializer')){
          config$initializer_minval <<- verify(config$initializer_minval, 'numeric', lengths = 1, default = - 0.1)
          config$initializer_maxval <<- verify(config$initializer_maxval, 'numeric', lengths = 1, default =   0.1)
          config$initializer_seed   <<- verify(config$initializer_seed  , c('integer', 'numeric'), lengths = 1, default = 42)  %>% as.integer
          config$kernel_initializer <<- initializer_random_uniform(minval = config$initializer_minval, maxval = config$initializer_maxval, seed = config$initializer_seed)
        }
      }
      config$num_layers         <<- verify(config$num_layers, c('integer', 'numeric'), lengths = 1, domain = c(1, 25), default = 1)   %>% as.integer
      config$first_layer_nodes  <<- verify(config$first_layer_nodes, c('integer', 'numeric'), lengths = 1, default = 128) %>% as.integer
      config$layer_nodes_ratio  <<- verify(config$layer_nodes_ratio, 'numeric', lengths = 1, default = 0.4)
      config$layers_activation  <<- verify(config$layers_activation, c('character', 'function'), lengths = 1, default = 'relu')
      config$layers_dropout     <<- verify(config$layers_dropout, 'numeric', lengths = 1, default = 0.25)
      # config$output_activation  <<- config$output_activation %>% verify(c('character', 'function'), lengths = 1, default = 'softmax')

      if(is.null(config$layers)){config$layers <<- create_keras_layers(config)}

      config$epochs     <<- verify(config$epochs,     c('numeric', 'integer'), lengths = 1, default = 5) %>% as.integer
      config$callback   <<- verify(config$callback,   'function', default = function(epoch, logs){
        cat('Epoch:', epoch, ' Loss:', logs$loss, 'Validation Loss:', logs$val_loss, '\n')})
      config$batch_size <<- verify(config$batch_size, c('numeric', 'integer'), lengths = 1, default = 32) %>% as.integer
      config$optimizer  <<- verify(config$optimizer, c('character', 'function'), lengths = 1, default = 'adam',
          domain = c('adadelta', 'adagrad', 'adam', 'adamax', 'nadam', 'rmsprop', 'sgd'))
      config$learning_rate  <<- verify(config$learning_rate, 'numeric', lengths = 1, default = 0.0001)
      if(!inherits(config$loss, 'function')){
        config$loss <<- verify(config$loss, 'character', lengths = 1, default = 'categorical_crossentropy', varname = "'loss'",
                 domain = c('categorical_crossentropy', 'mean_squared_error', 'mean_absolute_error', 'mean_absolute_percentage_error', 'mean_squared_logarithmic_error', 'squared_hinge', 'hinge', 'categorical_hinge', 'logcosh', 'huber_loss', 'sparse_categorical_crossentropy', 'binary_crossentropy', 'kullback_leibler_divergence', 'poisson', 'cosine_proximity'))
      }
    },

    model.fit = function(X, y){
      objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
      X = X[objects$features$fname]

      if(is.empty(gradient_transformers)){
        target = to_categorical(y, 2)
      } else {
        target = y %>% as.integer %>% as.matrix %>% cbind(NA)
      }

      # Build the NN:
      if(is.null(objects$model)){
        input_layer = layer_input(shape = ncol(X), name = 'input')
        # objects$model <<- keras_model_sequential()

        sequential = input_layer
        for(i in config$layers %>% length %>% sequence){
          lyr = config$layers[[i]]
          sequential %<>% add_keras_layer_dense(lyr)
        }

        if(is.empty(gradient_transformers)){
          output_layer = sequential %>% layer_dense(name = 'output', units = 2, activation = 'softmax')
          objects$model <<- keras_model(inputs = input_layer, outputs = output_layer)
        } else {
          config$loss <<- function(y_true, y_pred){
            K <- backend()
            x <- y_pred[,1]
            g <- y_pred[,2]
            y <- y_true[,1]
            K$mean(y*K$log(1.0 + K$exp(- x - g)) + (1.0 - y)*K$log(1.0 + K$exp(x + g)))
          }
          grad_input   = layer_input(shape = 1, name = 'gradient')
          output_layer = sequential %>% layer_dense(name = 'output', units = 1, activation = 'linear')
          output_layer = layer_concatenate(list(output_layer, grad_input))
          objects$model <<- keras_model(inputs = list(grad_input, input_layer), outputs = output_layer)
        }

        # Compile the NN:
        optmzr = parse(text = paste('optimizer', config$optimizer, sep = '_') %>% paste0('(lr = ', config$learning_rate, ')')) %>% eval
        objects$model <<- objects$model %>%
          compile(loss = config$loss, optimizer = optmzr, metrics = list('categorical_accuracy'))
      }

      if(inherits(X, 'WIDETABLE')){X = rbig::as.matrix(X)}
      if(is.empty(gradient_transformers)){
        objects$model$fit(X %>% data.matrix, target,
                          epochs = config$epochs,
                          batch_size = config$batch_size,
                          validation_split = 0.2,
                          callbacks = list(callback_lambda(on_epoch_end = config$callback))) ->> objects$history
      } else {
        objects$model$fit(list(attr(y, 'gradient') %>% data.matrix, X %>% data.matrix), target,
                          epochs = config$epochs,
                          batch_size = config$batch_size,
                          validation_split = 0.2,
                          callbacks = list(callback_lambda(on_epoch_end = config$callback))) ->> objects$history
        }
    },

    model.predict = function(X){
      if(inherits(X, 'WIDETABLE')){X = rbig::as.matrix(X)}
      if(is.empty(gradient_transformers)){
        XOUT = objects$model$predict(X %>% data.matrix) %>% as.data.frame %>% {.[,2, drop = F]}
      } else {
        XOUT = objects$model$predict(list(rep(0, nrow(X)) %>% data.matrix, X %>% data.matrix))[,1] %>% logit_inv %>% as.data.frame
      }
    },

    model.save = function(path = getwd()){
      callSuper(path)
      keras::save_model_hdf5(object = objects$model, filepath = paste0(path, '/', name, '.hdf5'))
    },

    model.load = function(path = getwd()){
      callSuper(path)
      fn   = paste0(path, '/', name, '.hdf5')
      pass = file.exists(fn)
      warnif(!pass, paste0('File ', fn , ' does not exist!'))
      if(pass){
        objects$model <<- keras::load_model_hdf5(filepath = fn)
      }
    }
))

#' @export CLS.SPARKLYR.GBT
CLS.SPARKLYR.GBT = setRefClass('CLS.SPARKLYR.GBT', contains = 'CLASSIFIER', methods = list(
  initialize = function(...){
    callSuper(...)
    description      <<- 'Gradient Boosted Tree on Spark'
    package          <<- 'sparklyr'
    package_language <<- 'R'
    packages_required <<- c(packages_required, 'sparklyr')
    
    if(is.empty(name)){name <<- 'SPRGBT' %>% paste0(sample(1000:9999, 1))}
    config$spark_home <<- verify(config$spark_home, 'character', default = Sys.getenv('SPARK_HOME'))
    if(is.null(config$spark_config)){config$spark_config <<- sparklyr::spark_config()}
    if(is.null(config$connection)){
      config$connection <<- sparklyr::spark_connect(
        master = "local",
        spark_home = config$spark_home,
        spark_config = config$spark_config)}
  },

  model.fit = function(X, y){
      X_TBL    = sparklyr::sdf_copy_to(sc = config$connection, x = cbind(X, label = y), name = 'X_TBL', memory = T, repartition = 10)
      features = colnames(X)
      formula  = paste0('label ~ ', paste(features, collapse = ' + ')) %>% as.formula
      objects$model <<- do.call(sparklyr::ml_gbt_classifier, list(X = X_TBL, formula = formula) %<==>% (config %>% list.remove(reserved_words)))
      imp = try(objects$model$model$feature_importances() %>% as.numeric, silent = T)
      if(inherits(imp, 'numeric')) objects$features$importance <<- imp
  },

  model.predict = function(X){
    X_TBL = sparklyr::sdf_copy_to(sc = config$connection, x = X, name = 'X_TBL', memory = T, repartition = 10)
    sparklyr::ml_predict(objects$model, X_TBL) %>% pull(prediction) %>% as.data.frame
  }
))


CLS.E1071.NB = setRefClass('CLS.E1071.NB', contains = 'CLASSIFIER', methods = list(
    initialize = function(...){
      callSuper(...)
      description <<- 'Naive Bayes'
      package <<- 'e1071'
      package_language <<- 'R'
      packages_required <<- c(packages_required, 'e1071')
      

      if(is.empty(name)){name <<- 'E1071NB' %>% paste0(sample(10000:99999, 1))}
      library(e1071)
    },

    model.fit = function(X, y){
      require(package, character.only = T) %>% assert(paste('Package', package, 'not found!'))
      naiveBayes(Y ~ ., data = cbind(X, Y = y)) ->> objects$model
    },

    model.predict = function(X){
      require(package, character.only = T) %>% assert(paste('Package', package, 'not found!'))
      stats::predict(objects$model, X) %>% as.data.frame
    }

  ))

# Not complete:
CLS.RPART.DT = setRefClass('CLS.RPART.DT', contains = 'MODEL', methods = list(
    initialize = function(...){
      callSuper(...)
      description <<- 'Decision Tree'
      package     <<- 'rpart'
      package_language <<- 'R'
      packages_required <<- c(packages_required, 'rpart')
      
      if(is.empty(name)){name <<- 'RPRTDT' %>% paste0(sample(10000:99999, 1))}
      library(rpart)
    },

    model.fit = function(X, y){
      # naiveBayes(Y ~ ., data = cbind(X, Y = y)) ->> objects$model
    },

    model.predict = function(X){
      stats::predict(objects$model, X) %>% as.data.frame
    }
  ))

# Multinominal Naive Bayes Classifier:
CLS.SKLEARN.MNB = setRefClass('CLS.SKLEARN.MNB', contains = 'CLS.SKLEARN', methods = list(
    initialize = function(...){
      callSuper(model.module = 'naive_bayes', model.class = 'MultinomialNB', ...)
      description <<- 'Multinominal Naive Bayes'
      
      if(is.empty(name)){name <<- 'SKMNB' %>% paste0(sample(10000:99999, 1))}
    },

    model.fit = function(X, y){
        objects$features <<- objects$features %>% filter(fclass == 'integer')
        X = X[objects$features$fname]
        if(ncol(X) == 0){stop('No integer columns left!')}
        
        callSuper(X, y)
    }
  )
)

# pmg_accuracy: Performance Metric with Gradient: Accuracy
# Accuracy as loss
#' @export
pmg_accuracy <- function(preds, dtrain){
  labels <- getinfo(dtrain, "label")
  lgrads <- attr(dtrain, 'gradient')
  preds  <- preds + lgrads
  tp     <- sum(labels & (preds > 0))
  tn     <- sum((!labels) & (preds < 0))
  prf    <- (tp + tn)/length(labels)
  return(list(metric = "accuracy", value = prf))
}

#' @export
pmg_gini <- function(preds, dtrain){
  labels <- getinfo(dtrain, "label")
  lgrads <- attr(dtrain, 'gradient')
  preds  <- preds + lgrads
  prf    <- correlation(preds, labels, 'gini')
  return(list(metric = "gini", value = prf))
}

#' @export
pmg_logloss <- function(preds, dtrain){
  labels <- getinfo(dtrain, "label")
  lgrads <- attr(dtrain, 'gradient')
  preds  <- preds + lgrads
  prf    <- correlation(preds, labels, 'loss')
  return(list(metric = "loss", value = prf))
}

#' @export
pmg_lift <- function(preds, dtrain){
  labels <- getinfo(dtrain, "label")
  lgrads <- attr(dtrain, 'gradient')
  preds  <- preds + lgrads
  prf    <- correlation(preds, labels, 'lift')
  return(list(metric = "lift", value = prf))
}

#' @export CLS.XGBOOST
CLS.XGBOOST = setRefClass('CLS.XGBOOST', contains = 'CLASSIFIER', methods = list(
  initialize = function(...){
    callSuper(...)
    package          <<- 'xgboost'
    package_language <<- 'R'
    packages_required <<- c(packages_required, 'xgboost')
    
    description      <<- 'Extreme Gradient Boosting'
    if(is.empty(name)){name <<- 'XGB' %>% paste0(sample(10000:99999, 1))}
    assert(require(xgboost), "Package 'xgboost' needs to be installed!")

    reserved_words <<- c(reserved_words, 'nrounds', 'watchlist', 'obj', 'feval', 'verbose', 'print_every_n', 'early_stopping_rounds',
                       'maximize', 'save_period', 'save_name', 'xgb_model', 'callbacks', 'nthread', 'show_progress')
    
    # parameter 'nrounds' is equivalent to 'n_estimators' in CLS.SKLEARN.XGB
    config$nrounds       <<- verify(config$nrounds       , c('numeric', 'integer'), lengths = 1, domain = c(0,Inf), default = 100)
    config$nthread       <<- verify(config$nthread       , c('numeric', 'integer'), lengths = 1, domain = c(0,1024), default = 1)
    config$show_progress <<- verify(config$show_progress , 'logical',               lengths = 1, domain = c(T, F) , default = F)
    config$verbose       <<- verify(config$verbose       , c('numeric', 'integer', 'logical'), lengths = 1, domain = c(0,1)  , default = 1) %>% as.integer
    config$print_every_n <<- verify(config$print_every_n , c('numeric', 'integer'), lengths = 1, domain = c(0,Inf), default = 1) %>% as.integer
    config$save_name     <<- verify(config$save_name     , 'character',             lengths = 1, default = "xgboost.model")
    config$callbacks     <<- verify(config$callbacks     ,  'list', default = list())
  },

  model.fit = function(X, y){
    X = X[objects$features$fname]
    if(ncol(X) == 0){stop('No columns in the input dataset!')}

    dtrain = xgb.DMatrix(as.matrix(X), label = y)

    need_eval = config$show_progress | !is.null(config$early_stopping_rounds)
    if(need_eval){
      dvalidation = list(train = dtrain)
      # vp: Validation Pack
      for(vp in config$cv.set){
        # nvs: Number of Validation Sets
        nvs = length(dvalidation)
        grd = y_gradient(X = vp$X, y = vp$y)
        dvalidation[["Validation_" %++% nvs]] <- vp$X[objects$features$fname] %>% as.matrix %>% xgb.DMatrix(label = vp$y)
        if(sum(abs(grd)) > .Machine$double.eps){
          attr(dvalidation[["Validation_" %++% nvs]], 'gradient') <- grd
        }
        # todo: check for neural net, check for transformers
      }
    } else {dvalidation = list()}

    if(!is.null(attr(y, 'gradient'))){
      attr(dtrain, 'gradient') <- attr(y, 'gradient')
      config$obj   <<- function(preds, dtrain){
        # now you can access the attribute in customized function
        labels <- getinfo(dtrain, 'label')
        lgrads <- attr(dtrain, 'gradient')
        preds  <- preds + lgrads
        preds  <- 1/(1 + exp(-preds))
        grad   <- preds - labels
        hess   <- preds * (1 - preds)
        return(list(grad = grad, hess = hess))
      }
      if(is.null(config$feval)){
        config$feval <<- pmg_gini
        config$maximize <<- T
      }
    }

    if(need_eval & is.null(config$feval) & is.null(config$eval_metric)){
      config$eval_metric <<- 'auc'
      config$maximize <<- T
    }

    objects$model <<- xgb.train(
      params    = config %>% list.remove(reserved_words),
      data      = dtrain,
      nrounds   = config$nrounds,
      nthread   = config$nthread,
      watchlist = dvalidation,
      obj       = config[['obj']],
      feval     = config$feval,
      maximize  = config$maximize,
      verbose   = config$verbose,
      print_every_n = config$print_every_n,
      early_stopping_rounds = config$early_stopping_rounds,
      save_period = config$save_period,
      save_name = config$save_name,
      xgb_model = config$xgb_model,
      callbacks = config$callbacks)

    imp = try(xgb.importance(model = objects$model) %>% select(fname = Feature, importance = Gain), silent = T)
    if(!inherits(imp, 'try-error')){
      if(!is.null(objects$features$importance)) objects$features$importance <<- NULL
      objects$features %>% left_join(imp, by = 'fname') %>% na2zero ->> objects$features
    } else if(is.null(objects$features$importance)){
      objects$features$importance <<- 1.0/nrow(objects$features)
    }
  },

  model.predict = function(X){
    X = X[objects$model$feature_names]
    stats::predict(objects$model, xgb.DMatrix(as.matrix(X), label = rep(NA, nrow(X)))) %>% logit_inv %>% as.data.frame
  }
))
