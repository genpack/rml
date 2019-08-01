
CLASSIFIER = setRefClass('CLASSIFIER', contains = "MODEL",
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Abstract Classifier'
      config$sig_level <<- config$sig_level %>% verify('numeric', domain = c(0,1), default = 0.1)
      config$predict_probabilities <<- config$predict_probabilities %>% verify('logical', domain = c(T,F), default = T)
      config$decision_threshold <<- config$decision_threshold %>% verify('numeric', lengths = 1, domain = c(0,1), default = 0.5)
      config$threshold_determination <<- config$threshold_determination %>%
        verify('character', lengths = 1, domain = c('set_as_threshold', 'maximum_f1', 'maximum_chi','target_precision', 'target_recall', 'ratio_quantile'), default = 'maximum_f1')
      if(is.null(config$metric)){
        config$metric <<- chif(config$predict_probabilities, function(y_pred, y_test) {data.frame(prob = y_pred, actual = y_test) %>% optSplit.f1('prob', 'actual')->aa;aa$f1}, function(y1, y2) mean(xor(y1, y2), na.rm = T))
      }
      config$return_logit <<- config$return_logit %>% verify('logical', domain = c(T,F), default = F)
    },

    fit = function(X, y){
      if(!fitted){
        callSuper(X, y)
        set_decision_threshold(X, y)
      }
    },

    predict = function(X, prob = config$predict_probabilities){
      XORG = callSuper(X)
      XFET = XORG[objects$features$fname]
      XOUT = .self$model.predict(XFET)
      if(!prob){XOUT[,1] = as.numeric(XOUT[,1] > config$decision_threshold)}
      # if(prob & config$return_logit){for (i in 1:ncol(XOUT)){XOUT[,i] <- log(XOUT[,i]/(1 - XOUT[,i]))}}
      if(prob & config$return_logit){XOUT %<>% as.matrix %>% apply(2, function(x) log(x/(1-x))) %>% as.data.frame}

      if(ncol(XOUT) > 1){
        colnames(XOUT) <- name %>% paste(colnames(XOUT), sep = '_')
      } else {
        colnames(XOUT) <- name %>% paste('out', sep = '_')
      }
      treat(XOUT, XFET, XORG)
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

    performance = function(X, y, metric = c('aurc', 'gini', 'precision', 'recall', 'f1', 'sensitivity', 'specificity', 'accuracy')){
      metric = match.arg(metric)
      cutoff_free = metric %in% c('aurc', 'gini')
      yp = predict(X, cutoff_free) %>% pull(name %>% paste('out', sep = '_'))
      if(metric %in% c('aurc', 'gini')){
        aurc = AUC::auc(AUC::roc(yp, y %>% as.factor))
        if(metric == 'aurc') return(aurc) else return(2*aurc - 1)
      }
      if(metric %in% c('precision', 'recall', 'f1', 'accuracy', 'sensitivity', 'specificity', 'fp', 'fn', 'tp', 'tn')){
        if(metric == 'sensitivity') metric = 'recall'
        score = data.frame(y_pred = yp, y_true = y) %>% scorer('y_pred', 'y_true')
        return(score[[metric]])
      }
    }
  )
)

CLS.SCIKIT = setRefClass('CLS.SCIKIT', contains = "CLASSIFIER",
   methods = list(
     initialize = function(...){
       callSuper(...)
       if(!require(reticulate)) stop("Package 'reticulate' is not installed!")
       if(!is.null(config$python_address)){
         use_python(config$python_address)
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

CLS.SCIKIT.KNN = setRefClass('SCIKIT.KNN', contains = "CLS.SCIKIT",
  methods = list(
    initialize = function(...){
      callSuper(...)
      type              <<- 'K Nearest Neighbors'

      config$num_neighbors <<- config$num_neighbors %>% verify(c('numeric', 'integer'), default = 100) %>% as.integer
      module_knn = reticulate::import('sklearn.neighbors')
      objects$model <<- module_knn$KNeighborsClassifier(n_neighbors = config$num_neighbors)
    }
  )
)

CLS.FLASSO = setRefClass('CLS.FLASSO', contains = 'CLASSIFIER', methods = list(
  initialize = function(...){
    callSuper(...)
    type              <<- 'Logistic Regression with Fusion Lasso'

    config$lambda1 <<- config$lambda1 %>% verify('numeric', default = 1)
    config$lambda2 <<- config$lambda2 %>% verify('numeric', default = 1)
    config$epochs  <<- config$epochs  %>% verify(c('numeric', 'integer'), default = 100)
  },

  model.fit = function(X, y){
    objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
    X = X[objects$features$fname]
    objects$model <<- HDPenReg::EMfusedlasso(X %>% as.matrix, y, lambda1 = config$lambda1, lambda2 = config$lambda2,
                                             maxSteps = config$epochs, burn = 50, intercept = TRUE, model = "logistic",
                                             eps = 1e-05, eps0 = 1e-08, epsCG = 1e-08)

  }
))


suppressWarnings({mlr.classification.models = mlr::listLearners('classif')})

#' @export CLASSIFIER.MLR
cls.MLR = setRefClass('CLS.MLR', contains = "CLASSIFIER",
    methods = list(
      initialize = function(...){
        library(mlr)
        callSuper(...)
        type              <<- 'MLR Classifier'
        if(is.empty(name)){name <<- 'MLR' %>% paste0(sample(1000:9999, 1))}
        config$model_type <<- config$model_type %>% verify('character', domain = mlr.classification.models %>% pull(class), default = 'classif.gbm')
        objects$model     <<- mlr::makeLearner(cl = config$model_type, predict.type = chif(config$predict_probabilities, "prob", "response"))
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
        config$sig_level <<- config$sig_level %>% verify('numeric', domain = c(0,1), default = 0.1)
        type             <<- 'Logistic Regression'
        if(is.empty(name)){name <<- 'SKLR' %>% paste0(sample(1000:9999, 1))}
        module_lm = reticulate::import('sklearn.linear_model')
        objects$model <<- do.call(module_lm$LogisticRegression, config %>% list.remove(maler_words))
      },

      model.fit = function(X, y){
          callSuper(X, y)
          objects$model$coef_ %>% abs %>% as.numeric -> weights
          objects$features$importance <<- (weights/(X %>% apply(2, sd))) %>% na2zero %>% {./geomean(.[.>0])} %>% as.numeric
      }
   )
)

# A simple logistic regression classifier from scikit python package:
# It extracts only numeric features, does no dummification for categorical columns.
#' @export SCIKIT.DT
CLS.SCIKIT.DT = setRefClass('SCIKIT.DT', contains = "CLS.SCIKIT",
  methods = list(
    initialize = function(...){
      callSuper(...)
      type               <<- 'Decision Tree'
      if(is.empty(name)){name <<- 'SKDT' %>% paste0(sample(1000:9999, 1))}
      module_dt = reticulate::import('sklearn.tree')
      objects$model <<- do.call(module_dt$DecisionTreeClassifier, config %>% list.remove(maler_words))
    }

  )
)

#' @export SCIKIT.XGB
CLS.SCIKIT.XGB = setRefClass('CLS.SCIKIT.XGB', contains = "CLS.SCIKIT",
    methods = list(
      initialize = function(...){
        callSuper(...)
        type               <<- 'Extreme Gradient Boosting'
        if(is.empty(name)){name <<- 'SKXGB' %>% paste0(sample(1000:9999, 1))}
        module_xgb = reticulate::import('xgboost')
        objects$model     <<- do.call(module_xgb$XGBClassifier, config %>% list.remove(maler_words))
      },

      model.fit = function(X, y){
          objects$model$fit(X %>% data.matrix, y)
          imp = try(objects$model$feature_importances_ %>% as.numeric, silent = T)
          if(inherits(imp, 'numeric')) objects$features$importance <<- imp
      }

    )
)

#' @export SCIKIT.SVM
CLS.SCIKIT.SVM = setRefClass('CLS.SCIKIT.SVM', contains = "CLS.SCIKIT",
                         methods = list(
                           initialize = function(...){
                             callSuper(...)
                             type               <<- 'Support Vector Machine'
                             if(is.empty(name)){name <<- 'SKSVM' %>% paste0(sample(1000:9999, 1))}
                             module_svm = reticulate::import('sklearn.svm')
                             objects$model <<- module_svm$SVC(gamma = 'scale', probability = config$predict_probabilities)
                           }
                         )
)

#' @export KERAS
CLS.KERAS = setRefClass('KERAS', contains = 'CLASSIFIER',
  methods = c(
    initialize = function(...){
      callSuper(...)
      type <<- 'Neural Network'
      library(tensorflow)
      library(keras)
      if(is.empty(name)){name <<- 'KERASNN' %>% paste0(sample(1000:9999, 1))}
      ki = initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 42)
      kr = regularizer_l2(l = 0.001)
      config$layers <<- config$layers %>%
        verify('list', default = list(
          list(name = 'dense1', units = 128, activation = 'relu', dropout = 0.25,
               kernel_initializer = ki),
          list(name = 'dense2', units = 32 , activation = 'relu', dropout = 0.25,
               kernel_initializer = ki,
                kernel_regularizer = kr),
          list(name = 'dense3', units = 8, activation = 'relu',
               kernel_initializer = ki,
               kernel_regularizer = kr
               )))
      config$outputs <<- config$outputs %>% verify(c('numeric', 'integer'), lengths = 1, default = 2)
      config$output_activation <<- config$output_activation %>% verify(c('character', 'function'), lengths = 1, default = 'softmax')
      config$epochs  <<- config$epochs %>% verify(c('numeric', 'integer'), lengths = 1, default = 5)
      config$callback <<- config$callback %>% verify('function', default = function(epoch, logs){
        cat('Epoch:', epoch, ' Loss:', logs$loss, 'Validation Loss:', logs$val_loss, '\n')
      config$batch_size <<- config$batch_size %>% verify(c('numeric', 'integer'), lengths = 1, default = 32) %>% as.integer
      config$optimizer  <<- optimizer_adam(lr = 0.001)
      })
    },

    model.fit = function(X, y){
      y = to_categorical(y, config$outputs)
      objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
      X = X[objects$features$fname]
      # Build the NN:
      if(is.null(objects$model)){
        objects$model <<- keras_model_sequential()
        for(i in config$layers %>% length %>% sequence){
          lyr = config$layers[[i]]
          if(i == 1){
            objects$model <<- objects$model %>% add_keras_layer_dense(lyr, input_shape = ncol(X))
          } else {
            objects$model <<- objects$model %>% add_keras_layer_dense(lyr)
          }
          if(!is.null(lyr$dropout)){
            objects$model <<- objects$model %>%
              layer_dropout(rate = lyr$dropout)
          }
        }
        objects$model <<- objects$model %>%
          layer_dense(name = 'output', units = as.integer(config$outputs), activation = config$output_activation, kernel_initializer = initializer_random_uniform(seed = 42))

        # Compile the NN:
        objects$model <<- objects$model %>%
          # compile(loss = 'mse', optimizer = optimizer_rmsprop(), metrics = list('mean_absolute_error'))
          compile(loss = 'categorical_crossentropy', optimizer = optimizer_adam(lr = 0.001), metrics = list('categorical_accuracy'))
      }

      objects$model$fit(X %>% data.matrix, y,
                        epochs = as.integer(config$epochs),
                        batch_size = config$batch_size,
                        validation_split = 0.2,
                        callbacks = list(callback_lambda(on_epoch_end = config$callback))) ->> objects$history
    },

    model.predict = function(X){
      XOUT = objects$model$predict_proba(X %>% data.matrix) %>% as.data.frame %>% {.[,2, drop = F]}
    }
))

#' @export SPARKLYR.GBT
CLS.SPARKLYR.GBT = setRefClass('SPARKLYR.GBT', contains = 'CLASSIFIER', methods = list(
  initialize = function(...){
    callSuper(...)
    type               <<- 'Gradient Boost Tree on Spark'
    if(is.empty(name)){name <<- 'SPRGBT' %>% paste0(sample(1000:9999, 1))}
    library(sparklyr)
  },

  model.fit = function(X, y){
      X_TBL = sdf_copy_to(sc = config$connection, x = cbind(X, label = y), name = 'X_TBL', memory = T, repartition = 10)
      features = colnames(X)
      formula  = paste0('label ~ ', paste(features, collapse = ' + ')) %>% as.formula
      objects$model <<- ml_gbt_classifier(X_TBL, formula)
      imp = try(objects$model$model$feature_importances() %>% as.numeric, silent = T)
      if(inherits(imp, 'numeric')) objects$features$importance <<- imp
  },

  model.predict = function(X){
    X_TBL = sdf_copy_to(sc = config$connection, x = X, name = 'X_TBL', memory = T, repartition = 10)
    ml_predict(objects$model, X_TBL) %>% pull(prediction) %>% as.data.frame
  }

)
                           )
