
CLASSIFIER = setRefClass('CLASSIFIER', contains = "MODEL",
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Abstract Classifier'
      config$sig_level <<- config$sig_level %>% verify('numeric', domain = c(0,1), default = 0.1)
      config$predict_probabilities <<- config$predict_probabilities %>% verify('logical', domain = c(T,F), default = T)
      config$decision_threshold <<- config$decision_threshold %>% verify('numeric', lengths = 1, domain = c(0,1), default = 0.5)
      config$threshold_determination <<- config$threshold_determination %>%
        verify('character', lengths = 1, domain = c('set_as_threshold', 'maximum_f1', 'target_precision', 'target_recall'), default = 'maximum_f1')
      if(is.null(config$metric)){
        config$metric <<- chif(config$predict_probabilities, function(y_pred, y_test) {data.frame(prob = y_pred, actual = y_test) %>% optSplit.f1('prob', 'actual')->aa;aa$f1}, function(y1, y2) mean(xor(y1, y2), na.rm = T))
      }
    },

    set_decision_threshold = function(X, y){
      if(fitted){
        if(config$threshold_determination == 'maximum_f1'){
          y_prob = predict(X) %>% pull(name %>% paste('out', sep = '_'))
          res = data.frame(y_pred = y_prob, y_true = y) %>% optSplit.f1('y_pred', 'y_true')
          config$decision_threshold <<- res$split
        }
        #todo: do for other options
      }
    }
  )
)

CLASSIFIER.SCIKIT = setRefClass('CLASSIFIER.SCIKIT', contains = "CLASSIFIER",
   methods = list(
     initialize = function(...){
       callSuper(...)
       if(!require(reticulate)) stop("Package 'reticulate' is not installed!")
       if(!is.null(config$python_address)){
         use_python(config$python_address)
       }
     },

     predict = function(X, prob = T){
       XORG = callSuper(X)
       XFET = XORG[objects$features$fname]
       XOUT = objects$model$predict_proba(XFET %>% data.matrix)[,2, drop = F] %>% as.data.frame

       if(!prob){XOUT[,1] = as.numeric(XOUT[,1] > config$decision_threshold)}
       # else                            {XOUT = objects$model$predict(XFET %>% data.matrix) %>% as.data.frame}
       colnames(XOUT) <- name %>% paste('out', sep = '_')
       treat(XOUT, XFET, XORG)
     }
   )
)

SCIKIT.KNN = setRefClass('SCIKIT.KNN', contains = "CLASSIFIER.SCIKIT",
  methods = list(
    initialize = function(...){
      callSuper(...)
      type              <<- 'K Nearest Neighbors'
      
      config$num_neighbors <<- config$num_neighbors %>% verify(c('numeric', 'integer'), default = 100) %>% as.integer
      module_knn = reticulate::import('sklearn.neighbors')
      objects$model <<- module_knn$KNeighborsClassifier(n_neighbors = config$num_neighbors)
    },
    
    fit = function(X, y){
      if(!fitted){
        Xy  <- callSuper(X, y)
        XT = Xy$X; yt = Xy$y
        objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
        XT = XT[objects$features$fname]
        objects$model$fit(XT %>% data.matrix, yt)
        fitted <<- T
        set_decision_threshold(X, y)
      }
    }
  )
)

FLASSO = setRefClass('FLASSO', contains = 'CLASSIFIER', methods = list(
  initialize = function(...){
    callSuper(...)
    type              <<- 'Logistic Regression with Fusion Lasso'
    
    config$lambda1 <<- config$lambda1 %>% verify('numeric', default = 1)
    config$lambda2 <<- config$lambda2 %>% verify('numeric', default = 1)
    config$epochs  <<- config$epochs  %>% verify(c('numeric', 'integer'), default = 100)
  },
  
  fit = function(X, y){
    if(!fitted){
      Xy  <- callSuper(X, y)
      XT = Xy$X; yt = Xy$y
      objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
      XT = XT[objects$features$fname]
      objects$model <<- HDPenReg::EMfusedlasso(XT %>% as.matrix, yt, lambda1 = config$lambda1, lambda2 = config$lambda2, 
                                     maxSteps = config$epochs, burn = 50, intercept = TRUE, model = "logistic",
                                     eps = 1e-05, eps0 = 1e-08, epsCG = 1e-08)
      fitted <<- T
      set_decision_threshold(X, y)
    }
  }
  
))


suppressWarnings({mlr.classification.models = mlr::listLearners('classif')})

#' @export CLASSIFIER.MLR
CLASSIFIER.MLR = setRefClass('CLASSIFIER.MLR', contains = "CLASSIFIER",
    methods = list(
      initialize = function(...){
        library(mlr)
        callSuper(...)
        type              <<- 'MLR Classifier'
        if(is.empty(name)){name <<- 'MLR' %>% paste0(sample(1000:9999, 1))}
        config$model_type <<- config$model_type %>% verify('character', domain = mlr.classification.models %>% pull(class), default = 'classif.gbm')
        objects$model     <<- mlr::makeLearner(cl = config$model_type, predict.type = chif(config$predict_probabilities, "prob", "response"))
      },

      fit = function(X, y){
        if(!fitted){
          Xy  <- callSuper(X, y)
          XT = Xy$X; yt = Xy$y
          if(!inherits(yt, 'factor')){yt %<>% as.factor; assert(length(levels(yt)) == 2)}

          tsk = mlr::makeClassifTask(data = cbind(XT, label = yt), target = 'label')
          mlr::train(objects$model, tsk) ->> objects$model
          fitted <<- T
          set_decision_threshold(X, y)
        }
      },

      predict = function(X, prob = F){
        XORG  = callSuper(X)
        XFET  = XORG[objects$features$fname]
        stats::predict(objects$model, newdata = XFET) -> pred
        if(prob){
          XOUT = getPredictionProbabilities(pred)
        } else {
          XOUT = getPredictionResponse(pred) %>% as.numeric() - 1
        }
        XOUT %<>% as.data.frame
        names(XOUT) <- name %>% paste('out', sep = '_')

        treat(XOUT, XFET, XORG)
      }
    )
)

# A simple logistic regression classifier from scikit python package:
# It extracts only numeric features, does no dummification for categorical columns.
#' @export SCIKIT.LR
SCIKIT.LR = setRefClass('SCIKIT.LR', contains = "CLASSIFIER.SCIKIT",
    methods = list(
      initialize = function(...){
        callSuper(...)
        config$sig_level <<- config$sig_level %>% verify('numeric', domain = c(0,1), default = 0.1)
        type             <<- 'Logistic Regression'
        if(is.empty(name)){name <<- 'SKLR' %>% paste0(sample(1000:9999, 1))}
        module_lm = reticulate::import('sklearn.linear_model')
        config$penalty <<- config$penalty %>% verify('character', lengths = 1, domain = c('l1', 'l2'), default = 'l2')
        config$dual    <<- config$dual %>% verify('logical', lengths = 1, domain = c(F,T), default = F)
        config$tol     <<- config$tol %>% verify('numeric', lengths = 1, domain = c(0,1), default = 0.0001)
        config$C       <<- config$C %>% verify('numeric', lengths = 1, domain = c(0,1), default = 1)
        config$fit_intercept <<- config$fit_intercept %>% verify('logical', lengths = 1, domain = c(F,T), default = T)
        # config$class_weight  <<- config$class_weight %>% verify('list', default = list(0.5, 0.5))
        config$solver   <<- config$solver %>% verify('character', lengths = 1, domain =c('newton-cg', 'lbfgs', 'liblinear', 'sag', 'saga'), default = 'liblinear')
        config$max_iter <<- config$max_iter %>% verify(c('integer', 'numeric'), lengths = 1, domain =c(0, Inf), default = 100) %>% as.integer
        # todo: add multi_class, verbose, warm_start, n_jobs
        objects$model <<- module_lm$LogisticRegression(penalty = config$penalty, dual = config$dual, tol = config$tol,
                                                       C = config$C, fit_intercept = config$fit_intercept, solver = config$solver,
                                                       random_state = config$random_state,
                                                       max_iter = config$max_iter)
        # todo: define hyper parameters in config

      },

      reset = function(...){
        callSuper(...)
      },

      fit = function(X, y){
        if(!fitted){
          Xy  <- callSuper(X, y)
          XT = Xy$X; yt = Xy$y
          objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
          XT = XT[objects$features$fname]
          objects$model$fit(XT %>% data.matrix, yt)
          objects$model$coef_ %>% abs %>% as.numeric -> weights
          objects$features$importance <<- (weights/(XT %>% apply(2, sd))) %>% na2zero %>% {./geomean(.[.>0])} %>% as.numeric
          fitted <<- T
          set_decision_threshold(X, y)
        }
      }
   )
)

FE = setRefClass('FE', contains = 'MODEL',
                 methods = list(
                   initialize = function(...){
                     callSuper(...)
                     if(is.null(objects$mother)){
                       objects$mother <<- SCIKIT.XGB(predict_probabilities = T)
                     }
                   },
                   predict = function(X){
                     XORG = callSuper(X)
                     XFET = XORG[objects$features$fname]
                     XOUT = objects$model$predict(XFET)
                     names(XOUT) <- name %>% paste(objects$model$name, 'out', sep = '_')
                     treat(XOUT, XFET, XORG)
                   },

                   fit = function(X, y){
                     if(!fitted){
                       Xy  <- callSuper(X, y)
                       XT = Xy$X; yt = Xy$y
                       objects$mother$fit(XT, yt)
                       objects$mother$objects$features$importance %>% order(decreasing = T) %>% head(5) -> w
                       objects$features <<- objects$mother$objects$features[w,]
                       objects$model    <<- objects$mother$copy()
                       objects$model$reset(F)

                       XT = XT[objects$features$fname]
                       objects$model$fit(XT, yt)
                       objects$features <<- objects$model$objects$features
                     }
                     fitted <<- T      
                     set_decision_threshold(X, y)
                   }
                 ))

# # Scikit Logistic Regression with feature elimination:
# SCIKIT.LR.WFE = setRefClass('SCIKIT.LR.WFE', contains = "SCIKIT.LR",
#   methods = list(
#     fit = function(X, y, do_transform = T){
#       callSuper(X, y, do_transform = do_transform)
#
#       features <- get.features() %>% pull(fname)
#       reset(T)
#       if(do_transform) X = transform(X, y) %>% remove_invariant_features
#       callSuper(X[, features], y, do_transform = F)
#     },
#
#     get.features = function(){
#       objects$features <<- callSuper() %>% filter(importance > 0)
#       return(objects$features)
#     }
#
#   ))

# A simple logistic regression classifier from scikit python package:
# It extracts only numeric features, does no dummification for categorical columns.
#' @export SCIKIT.DT
SCIKIT.DT = setRefClass('SCIKIT.DT', contains = "CLASSIFIER.SCIKIT",
  methods = list(
    initialize = function(...){
      callSuper(...)
      type               <<- 'Decision Tree'
      if(is.empty(name)){name <<- 'SKDT' %>% paste0(sample(1000:9999, 1))}
      module_dt = reticulate::import('sklearn.tree')
      objects$model <<- module_dt$DecisionTreeClassifier()

    },

    fit = function(X, y){
      if(!fitted){
        Xy  <- callSuper(X, y)
        XT = Xy$X; yt = Xy$y
        
        objects$model$fit(XT %>% data.matrix, yt)
        imp = try(objects$model$feature_importances_ %>% as.numeric, silent = T)
        if(inherits(imp, 'numeric')) objects$features$importance <<- imp
        ## todo: feature importances
        fitted <<- T
        set_decision_threshold(X, y)

      }
    },

    reset = function(...){
      callSuper(...)
    },

    get.feature.weights = function(){
      if(fitted){
        return(objects$model$feature_importances_/sum(objects$model$feature_importances_))
      }
    }
  )
)

#' @export SCIKIT.XGB
SCIKIT.XGB = setRefClass('SCIKIT.XGB', contains = "CLASSIFIER.SCIKIT",
    methods = list(
      initialize = function(...){
        callSuper(...)
        type               <<- 'Extreme Gradient Boosting'
        if(is.empty(name)){name <<- 'SKXGB' %>% paste0(sample(1000:9999, 1))}
        module_xgb = reticulate::import('xgboost')
        config$max_depth        <<- config$max_depth %>% verify(c('numeric', 'integer'), default = 4) %>% as.integer
        config$min_child_weight <<- config$min_child_weight %>% verify(c('numeric', 'integer'), default = 40) %>% as.numeric
        config$subsample  <<- config$subsample %>% verify('numeric', domain = c(0,1), lengths = 1, default = 0.4)
        config$lambda     <<- config$lambda %>% verify('numeric', lengths = 1, default = 20000)
        config$alpha      <<- config$alpha %>% verify('numeric', lengths = 1, default = 2000)
        config$gamma      <<- config$gamma %>% verify('numeric', lengths = 1, default = 1.3)
        config$partial    <<- config$partial %>% verify('logical', lengths = 1, domain = c(F,T), default = TRUE)
        objects$model     <<- module_xgb$XGBClassifier(max_depth = config$max_depth, min_child_weight = config$min_child_weight, subsample = config$subsample, lambda = config$lambda, alpha = config$alpha, gamma = config$gamma, partial = config$partial)
      },

      fit = function(X, y){
        if(!fitted){
          Xy  <- callSuper(X, y)
          XT = Xy$X; yt = Xy$y
          
          objects$model$fit(XT %>% data.matrix, yt)
          imp = try(objects$model$feature_importances_ %>% as.numeric, silent = T)
          if(inherits(imp, 'numeric')) objects$features$importance <<- imp
          ## todo: feature importances
          fitted <<- T
          set_decision_threshold(X, y)
        }
      },

      reset = function(...){
        callSuper(...)
      }
    )
)

#' @export SCIKIT.SVM
SCIKIT.SVM = setRefClass('SCIKIT.SVM', contains = "CLASSIFIER.SCIKIT",
                         methods = list(
                           initialize = function(...){
                             callSuper(...)
                             type               <<- 'Support Vector Machine'
                             if(is.empty(name)){name <<- 'SKSVM' %>% paste0(sample(1000:9999, 1))}
                             module_svm = reticulate::import('sklearn.svm')
                             objects$model <<- module_svm$SVC(gamma = 'scale', probability = config$predict_probabilities)
                           },

                           fit = function(X, y){
                             if(!fitted){
                               Xy  <- callSuper(X, y)
                               XT = Xy$X; yt = Xy$y
                               
                               objects$model$fit(XT %>% data.matrix, yt)
                               imp = try(objects$model$feature_importances_ %>% as.numeric, silent = T)
                               if(inherits(imp, 'numeric')) objects$features$importance <<- imp
                               ## todo: feature importances
                               fitted <<- T
                               set_decision_threshold(X, y)
                               
                             }
                           },

                           reset = function(...){
                             callSuper(...)

                           }
                         )
)

#' @export KERAS
KERAS = setRefClass('KERAS', contains = 'CLASSIFIER',
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
      })
    },

    fit = function(X, y){
    if(!fitted){
      Xy  <- callSuper(X, y)
      XT = Xy$X; yt = Xy$y
      yt = to_categorical(yt, config$outputs)
      objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
      XT = XT[objects$features$fname]
      # Build the NN:
      objects$model <<- keras_model_sequential()
      for(i in config$layers %>% length %>% sequence){
        lyr = config$layers[[i]]
        if(i == 1){
          objects$model <<- objects$model %>%
            layer_dense(units = as.integer(lyr$units), activation = lyr$activation, input_shape = ncol(XT))
        } else {
          objects$model <<- objects$model %>%
            layer_dense(units = as.integer(lyr$units), activation = lyr$activation)
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


      objects$model$fit(XT %>% data.matrix, yt,
                        epochs = as.integer(config$epochs),
                        batch_size = as.integer(10),
                        validation_split = 0.2,
                        callbacks = list(callback_lambda(on_epoch_end = config$callback))) ->> objects$history
      fitted <<- T
      set_decision_threshold(X, y)
      
    }
  },

    predict = function(X){
    XORG = callSuper(X)
    XFET = XORG[objects$features$fname]
    if(config$predict_probabilities){XOUT = objects$model$predict_proba(XFET %>% data.matrix) %>% as.data.frame %>% {.[,2, drop = F]}}
    else                            {XOUT = objects$model$predict_classes(XFET %>% data.matrix) %>% as.data.frame}
    colnames(XOUT) <- name %>% paste('out', sep = '_')
    treat(XOUT, XFET, XORG)
  }
))

#' @export SPARKLYR.GBT
SPARKLYR.GBT = setRefClass('SPARKLYR.GBT', contains = 'CLASSIFIER', methods = list(
  initialize = function(...){
    callSuper(...)
    type               <<- 'Gradient Boost Tree on Spark'
    if(is.empty(name)){name <<- 'SPRGBT' %>% paste0(sample(1000:9999, 1))}
    library(sparklyr)
  },

  fit = function(X, y){
    if(!fitted){
      Xy  <- callSuper(X, y)
      XT = Xy$X; yt = Xy$y
      X_TBL = sdf_copy_to(sc = config$connection, x = cbind(XT, label = yt), name = 'X_TBL', memory = T, repartition = 10)
      features = colnames(X)
      formula  = paste0('label ~ ', paste(features, collapse = ' + ')) %>% as.formula
      objects$model <<- ml_gbt_classifier(X_TBL, formula)
      imp = try(objects$model$model$feature_importances() %>% as.numeric, silent = T)
      if(inherits(imp, 'numeric')) objects$features$importance <<- imp
      fitted <<- T
      set_decision_threshold(X, y)
      
    }
  },

  predict = function(X){
    XORG = callSuper(X)
    XFET = XORG[objects$features$fname]
    X_TBL = sdf_copy_to(sc = config$connection, x = XFET, name = 'X_TBL', memory = T, repartition = 10)
    if(config$predict_probabilities){XOUT = ml_predict(objects$model, X_TBL) %>% pull(probability_1) %>% as.data.frame}
    else                            {XOUT = ml_predict(objects$model, X_TBL) %>% pull(prediction) %>% as.data.frame}
    colnames(XOUT) <- name %>% paste('out', sep = '_')
    treat(XOUT, XFET, XORG)
  }

)
                           )
