
CLASSIFIER = setRefClass('CLASSIFIER', contains = "MODEL",
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Abstract Classifier'
      config$sig_level <<- config$sig_level %>% verify('numeric', domain = c(0,1), default = 0.1)
      config$predict_probabilities <<- config$predict_probabilities %>% verify('logical', domain = c(T,F), default = F)
      if(is.null(config$metric)){
        config$metric <<- chif(config$predict_probabilities, function(y_pred, y_test) {data.frame(prob = y_pred, actual = y_test) %>% optSplit.f1('prob', 'actual')->aa;aa$f1}, function(y1, y2) mean(xor(y1, y2), na.rm = T))
      }
      reset()
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
     
     predict = function(X){
       XORG = callSuper(X)
       XFET = XORG[objects$features$fname]
       if(config$predict_probabilities){XOUT = objects$model$predict_proba(XFET %>% data.matrix)[,2, drop = F] %>% as.data.frame}
       else                            {XOUT = objects$model$predict(XFET %>% data.matrix) %>% as.data.frame}
       colnames(XOUT) <- name %>% paste('out', sep = '_')
       treat(XOUT, XFET, XORG)
     }
   )
)

suppressWarnings({mlr.classification.models = mlr::listLearners('classif')})

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
          X = callSuper(X, y)
          if(!inherits(y, 'factor')){y %<>% as.factor; assert(length(levels(y)) == 2)}
          
          tsk = mlr::makeClassifTask(data = cbind(X, label = y), target = 'label')
          mlr::train(objects$model, tsk) ->> objects$model
          fitted <<- T
        }
      },
      
      predict = function(X){
        XORG  = callSuper(X)
        XFET  = XORG[objects$features$fname]
        stats::predict(objects$model, newdata = XFET) -> pred
        if(config$predict_probabilities){
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
SCIKIT.LR = setRefClass('SCIKIT.LR', contains = "CLASSIFIER.SCIKIT",
    methods = list(
      initialize = function(...){
        callSuper(...)
        config$sig_level <<- config$sig_level %>% verify('numeric', domain = c(0,1), default = 0.1)
        type             <<- 'Logistic Regression'
        if(is.empty(name)){name <<- 'SKLR' %>% paste0(sample(1000:9999, 1))}
      },

      reset = function(...){
        callSuper(...)
        module_lm = reticulate::import('sklearn.linear_model')
        objects$model <<- module_lm$LogisticRegression(penalty = 'l1',solver = 'liblinear')
        # todo: define hyper parameters in config
      },

      fit = function(X, y){
        if(!fitted){
          X %<>% callSuper(y)
          objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
          X = X[objects$features$fname]
          objects$model$fit(X %>% data.matrix, y)
          objects$model$coef_ %>% abs %>% as.numeric -> weights
          objects$features$importance <<- (weights/(X %>% apply(2, sd))) %>% na2zero %>% {./geomean(.[.>0])} %>% as.numeric
          fitted <<- T
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
                       X = transform(X, y)
                       objects$mother$fit(X, y)
                       objects$mother$objects$features$importance %>% order(decreasing = T) %>% head(5) -> w
                       objects$features <<- objects$mother$objects$features[w,]
                       objects$model    <<- objects$mother$copy()
                       objects$model$reset(F)
                       
                       X = X[objects$features$fname]
                       objects$model$fit(X, y)
                       objects$features <<- objects$model$objects$features
                     }
                     fitted <<- T                   }
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
SCIKIT.DT = setRefClass('SCIKIT.DT', contains = "CLASSIFIER.SCIKIT",
  methods = list(
    initialize = function(...){
      callSuper(...)
      type               <<- 'Decision Tree'
      if(is.empty(name)){name <<- 'SKDT' %>% paste0(sample(1000:9999, 1))}
    },

    fit = function(X, y){
      if(!fitted){
        X = callSuper(X, y)
        
        objects$model$fit(X %>% data.matrix, y)
        imp = try(objects$model$feature_importances_ %>% as.numeric, silent = T)
        if(inherits(imp, 'numeric')) objects$features$importance <<- imp
        ## todo: feature importances
        fitted <<- T
      }  
    },
    
    reset = function(...){
      callSuper(...)
      module_dt = reticulate::import('sklearn.tree')
      objects$model <<- module_dt$DecisionTreeClassifier()
    },

    get.feature.weights = function(){
      if(fitted){
        return(objects$model$feature_importances_/sum(objects$model$feature_importances_))
      }
    }
  )
)

SCIKIT.XGB = setRefClass('SCIKIT.XGB', contains = "CLASSIFIER.SCIKIT",
    methods = list(
      initialize = function(...){
        callSuper(...)
        type               <<- 'Extreme Gradient Boosting'
        if(is.empty(name)){name <<- 'SKXGB' %>% paste0(sample(1000:9999, 1))}
      },
      
      fit = function(X, y){
        if(!fitted){
          X = callSuper(X, y)
          
          objects$model$fit(X %>% data.matrix, y)
          imp = try(objects$model$feature_importances_ %>% as.numeric, silent = T)
          if(inherits(imp, 'numeric')) objects$features$importance <<- imp
          ## todo: feature importances
          fitted <<- T
        }  
      },
      
      reset = function(...){
        callSuper(...)
        module_xgb = reticulate::import('xgboost')
        objects$model <<- module_xgb$XGBClassifier(max_depth = as.integer(4), min_child_weight = 40, subsample = 0.4, lambda = 20000, alpha = 2000, gamma = 13, partial = TRUE, recall_max = 0.3)
      }    
    )
) 

SCIKIT.SVM = setRefClass('SCIKIT.SVM', contains = "CLASSIFIER.SCIKIT",
                         methods = list(
                           initialize = function(...){
                             callSuper(...)
                             type               <<- 'Support Vector Machine'
                             if(is.empty(name)){name <<- 'SKSVM' %>% paste0(sample(1000:9999, 1))}
                           },
                           
                           fit = function(X, y){
                             if(!fitted){
                               X = callSuper(X, y)
                               
                               objects$model$fit(X %>% data.matrix, y)
                               imp = try(objects$model$feature_importances_ %>% as.numeric, silent = T)
                               if(inherits(imp, 'numeric')) objects$features$importance <<- imp
                               ## todo: feature importances
                               fitted <<- T
                             }  
                           },
                           
                           reset = function(...){
                             callSuper(...)
                             module_svm = reticulate::import('sklearn.svm')
                             objects$model <<- module_svm$SVC(gamma = 'scale', probability = config$predict_probabilities)
                           }    
                         )
)
KERAS = setRefClass('KERAS', contains = 'MODEL',
                    methods = c(
                      initialize = function(...){
                        callSuper(...)
                      }
                      ## ... Under Construction ...
                    ))
