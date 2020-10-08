
#' @export
CLASSIFIER = setRefClass('CLASSIFIER', contains = "MODEL",
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Binary Classifier'
      config$sig_level <<- verify(config$sig_level, 'numeric', domain = c(0,1), default = 0.1)
      config$return    <<- verify(config$return, 'character', domain = c('probs', 'logit', 'class'), default = 'probs')
      config$decision_threshold <<- verify(config$decision_threshold, 'numeric', lengths = 1, domain = c(0,1), default = 0.5)
      config$threshold_determination <<- verify(config$threshold_determination, 'character', lengths = 1,
                                                domain = c('set_as_config', 'maximum_f1', 'maximum_chi'), default = 'set_as_config')
      # todo: add 'target_precision', 'target_recall', 'ratio_quantile'
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

        set_decision_threshold(X, y)
        objects$n_output <<- as.integer(1)
      }
    },

    transform_yout = function(X, Y = NULL){
      has_gradient = length(gradient_transformers) > 0
      if (config$return == 'class'){for(i in sequence(ncol(Y))) {Y[,i] = as.numeric(Y[,i] > config$decision_threshold)}} else
      if (config$return == 'logit' | has_gradient){Y %<>% as.matrix %>% apply(2, logit_fwd) %>% as.data.frame}
      Y = callSuper(X, Y)
      if (config$return == 'probs' & has_gradient){Y %<>% as.matrix %>% apply(2, logit_inv) %>% as.data.frame}
      colnames(Y) <- config$return
      return(Y)
    },

    set_decision_threshold = function(X, y){
      if(fitted){
        if(config$threshold_determination == 'maximum_f1'){
          y_prob = predict(X) %>% pull(name %>% paste('out', sep = '_'))
          res = data.frame(y_pred = y_prob, y_true = y) %>% optSplit.f1('y_pred', 'y_true')
          config$decision_threshold <<- res$split
        }
        else if(config$threshold_determination == 'maximum_chi'){
          y_prob = predict(X) %>% pull(name %>% paste('out', sep = '_'))
          res = data.frame(y_pred = y_prob, y_true = y) %>% optSplit.chi('y_pred', 'y_true')
          config$decision_threshold <<- res$split
        }
        else if(config$threshold_determination == 'ratio_quantile'){
          y_prob = predict(X) %>% pull(name %>% paste('out', sep = '_'))
          config$decision_threshold <<- quantile(y_prob, probs = 1 - mean(y, na.rm = T))
        }
        #todo: do for other options
      }
    },

    performance = function(X, y, metric = c('gini', 'aurc', 'precision', 'recall', 'f1', 'sensitivity', 'specificity', 'accuracy', 'lift', 'loss'), ratio = NULL){
      metric = match.arg(metric)
      # cutoff_free = metric %in% c('aurc', 'gini', 'lift')
      yp = predict(X)[, 1]

      correlation(yp, y, metric = metric, ratio = ratio)
    }
  )
)

#' @export CLS.SCIKIT
CLS.SCIKIT = setRefClass('CLS.SCIKIT', contains = "CLASSIFIER",
   methods = list(
     initialize = function(...){
       callSuper(...)
       if(!require(reticulate)) stop("Package 'reticulate' is not installed!")
       if(!is.null(config$python_address)){
         use_python(config$python_address)
       }
       package <<- 'sklearn'
       package_language <<- 'python'
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
         objects$model <<- joblib$load(fn)
       }
     },

     model.predict = function(X){
       objects$model$predict_proba(X %>% data.matrix)[,2, drop = F] %>% as.data.frame
     },

     model.fit = function(X, y){
       objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
       X = X[objects$features$fname]
       objects$model$fit(X %>% data.matrix, y)
     },

     get.feature.weights = function(){
       if(fitted){
         return(objects$model$feature_importances_/sum(objects$model$feature_importances_))
       }
     }
   )
)

#' @export CLS.SCIKIT.KNN
CLS.SCIKIT.KNN = setRefClass('CLS.SCIKIT.KNN', contains = "CLS.SCIKIT",
  methods = list(
    initialize = function(...){
      callSuper(...)
      description <<- 'K-Nearest Neighbors'

      config$num_neighbors <<- verify(config$num_neighbors, c('numeric', 'integer'), default = 100) %>% as.integer
      module_knn = reticulate::import('sklearn.neighbors')
      objects$model <<- module_knn$KNeighborsClassifier(n_neighbors = config$num_neighbors)
    }
  )
)

#' @export CLS.HDPENREG.FLASSO
CLS.HDPENREG.FLASSO = setRefClass('CLS.FLASSO', contains = 'CLASSIFIER', methods = list(
  initialize = function(...){
    callSuper(...)
    package     <<- 'HDPenReg'
    package_language <<- 'R'
    description <<- 'Logistic Regression with Fusion Lasso'
    if(is.empty(name)){name <<- 'FLS' %>% paste0(sample(10000:99999, 1))}

    config$lambda1 <<- verify(config$lambda1, 'numeric', default = 1)
    config$lambda2 <<- verify(config$lambda2, 'numeric', default = 1)
    config$model   <<- verify(config$model,   'character', default = 'logistic')
  },

  model.fit = function(X, y){
    objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
    X = X[objects$features$fname]
    objects$model <<- do.call(HDPenReg::EMfusedlasso, list(X = X %>% as.matrix, y = y) %<==>% (config %>% list.remove(maler_words)))
    objects$features$importance <<- objects$model$coefficient
  },

  model.predict = function(X){
    X %>% as.matrix %*% objects$model$coefficient %>% logit.inv %>% as.data.frame
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

# A simple logistic regression classifier from scikit python package:
# It extracts only numeric features, does no dummification for categorical columns.
#' @export SCIKIT.LR
CLS.SCIKIT.LR = setRefClass('CLS.SCIKIT.LR', contains = "CLS.SCIKIT",
    methods = list(
      initialize = function(...){
        callSuper(...)
        config$sig_level <<- verify(config$sig_level, 'numeric', domain = c(0,1), default = 0.1)
        description             <<- 'Logistic Regression'
        if(is.empty(name)){name <<- 'SKLR' %>% paste0(sample(10000:99999, 1))}
        module_lm = reticulate::import('sklearn.linear_model')
        objects$model <<- do.call(module_lm$LogisticRegression, config %>% list.remove(maler_words))
      },

      model.fit = function(X, y){
          callSuper(X, y)
          X = X[objects$features$fname]
          objects$model$coef_ %>% abs %>% as.numeric -> weights
          # objects$features$importance <<- (weights/(X %>% apply(2, sd))) %>% na2zero %>% {./geomean(.[.>0])} %>% as.numeric
          objects$features$importance <<- (weights/(X %>% apply(2, sd))) %>% na2zero %>% {./geomean(.[.>0])} %>% vect.normalise %>% as.numeric

      },

      get.function = function(...){
        assert(fitted, 'Model not fitted!')
        build_lincomb(name = name, length(objects$features$fname), features = objects$features$fname, parameter_values = c(as.numeric(objects$model$intercept_), as.numeric(objects$model$coef_)), ...)
      }
   )
)

# A simple logistic regression classifier from scikit python package:
# It extracts only numeric features, does no dummification for categorical columns.
#' @export CLS.SCIKIT.DT
CLS.SCIKIT.DT = setRefClass('CLS.SCIKIT.DT', contains = "CLS.SCIKIT",
  methods = list(
    initialize = function(...){
      callSuper(...)
      description <<- 'Decision Tree'
      if(is.empty(name)){name <<- 'SKDT' %>% paste0(sample(10000:99999, 1))}
      module_dt = reticulate::import('sklearn.tree')
      objects$model <<- do.call(module_dt$DecisionTreeClassifier, config %>% list.remove(maler_words))
    }
  )
)

#' @export CLS.SCIKIT.XGB
CLS.SCIKIT.XGB = setRefClass('CLS.SCIKIT.XGB', contains = "CLS.SCIKIT",
    methods = list(
      initialize = function(...){
        callSuper(...)
        description <<- 'Extreme Gradient Boosting'
        if(is.empty(name)){name <<- 'SKXGB' %>% paste0(sample(10000:99999, 1))}
        if(!is.null(config$n_jobs)){config$n_jobs <<- as.integer(config$n_jobs)}
        module_xgb = reticulate::import('xgboost')
        objects$model     <<- do.call(module_xgb$XGBClassifier, config %>% list.remove(maler_words))

      },

      model.fit = function(X, y){
          objects$model$fit(X %>% data.matrix, y)
          imp = try(objects$model$feature_importances_ %>% as.numeric, silent = T)
          if(inherits(imp, 'numeric')) objects$features$importance <<- imp
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

#' @export CLS.SCIKIT.SVM
CLS.SCIKIT.SVM = setRefClass('CLS.SCIKIT.SVM', contains = "CLS.SCIKIT",
                         methods = list(
                           initialize = function(...){
                             callSuper(...)
                             description <<- 'Support Vector Machine'
                             if(is.empty(name)){name <<- 'SKSVM' %>% paste0(sample(10000:99999, 1))}
                             module_svm = reticulate::import('sklearn.svm')
                             objects$model <<- module_svm$SVC(gamma = 'scale', probability = T)
                           }
                         )
)


#' @export CLS.KERAS.DNN
CLS.KERAS.DNN = setRefClass('CLS.KERAS.DNN', contains = 'CLASSIFIER',
  methods = c(
    initialize = function(...){
      callSuper(...)
      description      <<- 'Deep Neural Network'
      package          <<- 'keras'
      package_language <<- 'python'

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
      objects$model <<- do.call(sparklyr::ml_gbt_classifier, list(X = X_TBL, formula = formula) %<==>% (config %>% list.remove(maler_words)))
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
CLS.SCIKIT.MNB = setRefClass('CLS.SCIKIT.MNB', contains = 'CLS.SCIKIT', methods = list(
    initialize = function(...){
      callSuper(...)
      description <<- 'Multinominal Naive Bayes'
      if(is.empty(name)){name <<- 'MNB' %>% paste0(sample(10000:99999, 1))}
      module_mnb = reticulate::import('sklearn.naive_bayes')
    },

    model.fit = function(X, y){
        objects$features <<- objects$features %>% filter(fclass == 'integer')
        X = X[objects$features$fname]
        if(ncol(X) == 0){stop('No integer columns left!')}
        objects$model <<- do.call(module_mnb$MultinomialNB, config %>% list.remove(maler_words))
        objects$model$fit(X %>% data.matrix, y)
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
    description      <<- 'Extreme Gradient Boosting'
    if(is.empty(name)){name <<- 'XGB' %>% paste0(sample(10000:99999, 1))}
    assert(require(xgboost), "Package 'xgboost' needs to be installed!")

    # parameter 'nrounds' is equivalent to 'n_estimators' in CLS.SCIKIT.XGB
    config$nrounds       <<- verify(config$nrounds       , c('numeric', 'integer'), lengths = 1, domain = c(0,Inf), default = 100)
    config$nthread       <<- verify(config$nthread       , c('numeric', 'integer'), lengths = 1, domain = c(0,1024), default = 1)
    config$show_progress <<- verify(config$show_progress , 'logical',               lengths = 1, domain = c(T, F) , default = F)
    config$verbose       <<- verify(config$verbose       , c('numeric', 'integer', 'logical'), lengths = 1, domain = c(0,1)  , default = 1) %>% as.integer
    config$print_every_n <<- verify(config$print_every_n , c('numeric', 'integer'), lengths = 1, domain = c(0,Inf), default = 1) %>% as.integer
    config$save_name     <<- verify(config$save_name     , 'character',             lengths = 1, default = "xgboost.model")
    config$callbacks     <<- verify(config$callbacks     ,  'list', default = list())
  },

  model.fit = function(X, y){
    reserved_words = c('nrounds', 'watchlist', 'obj', 'feval', 'verbose', 'print_every_n', 'early_stopping_rounds',
                       'maximize', 'save_period', 'save_name', 'xgb_model', 'callbacks', 'nthread', 'show_progress')
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
      params    = config %>% list.remove(c(maler_words, reserved_words)),
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
