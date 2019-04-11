
CLASSIFIER = setRefClass('CLASSIFIER', contains = "MODEL",
  methods = list(
    initialize = function(settings = list(), ...){
      callSuper(settings = settings, ...)
      config$sig_level <<- settings$sig_level %>% verify('numeric', domain = c(0,1), default = '0.1')
      config$predict_probabilities <<- settings$predict_probabilities %>% verify('logical', domain = c(T,F), default = F)
      type             <<- 'Abstract Classifier'
      if(is.null(settings$cross_validation$performance_meter)){
        config$cross_validation$performance_meter <<- function(y1, y2) mean(xor(y1, y2), na.rm = T)
      }
      reset()
    },
    
    fit = function(X, y, do_transform = T){
      if(!fitted){
        if(do_transform) X = transform(X, y) %>% remove_invariant_features
        
        objects$model$fit(X, y)
        fitted <<- T
      }
    }
    
  )
)

CLASSIFIER.SCIKIT = setRefClass('CLASSIFIER.SCIKIT', contains = "CLASSIFIER",
   methods = list(
     initialize = function(settings = list(), ...){
       callSuper(settings = settings, ...)
       if(!require(reticulate)) stop("Package 'reticulate' is not installed!")
       if(!is.null(settings$python_address)){
         use_python(settings$python_address)
       }
     },
     
     fit = function(X, y, do_transform = T){
       if(!fitted){
         if(do_transform) X = transform(X, y) %>% remove_invariant_features
         
         objects$model$fit(X %>% data.matrix, y)
         objects$features <<- data.frame(name = colnames(X))
         ## todo: feature importances
         fitted <<- T
       }  
     },

     predict = function(X){
       X = callSuper(X)
       if(config$predict_probabilities){objects$model$predict_proba(X %>% data.matrix)[,2]}
       else                            {objects$model$predict(X %>% data.matrix)}
     }
   )
)

suppressWarnings({mlr.classification.models = mlr::listLearners('classif')})

CLASSIFIER.MLR = setRefClass('CLASSIFIER.MLR', contains = "CLASSIFIER",
    methods = list(
      initialize = function(settings = list(), ...){
        library(mlr)
        callSuper(settings = settings, ...)
        type              <<- 'Extreme Gradient Boost'
        config$model_type <<- settings$model_type %>% verify('character', domain = mlr.classification.models %>% pull(class), default = 'classif.gbm')
        objects$model     <<- mlr::makeLearner(cl = config$model_type, predict.type = chif(config$predict_probabilities, "prob", "response"))
      },
      
      fit = function(X, y, do_transform = T){
        if(!fitted){
          if(!inherits(y, 'factor')){y %<>% as.factor; assert(length(levels(y)) == 2)}
          if(do_transform) X = transform(X, y) %>% remove_invariant_features
          featnames <- colnames(X)
          tsk = mlr::makeClassifTask(data = cbind(X, label = y), target = 'label')
          mlr::train(objects$model, tsk) ->> objects$model
          objects$features <<- data.frame(name = featnames, stringsAsFactors = F)
          fitted <<- T
        }
      },
      
      predict = function(X){
        X = callSuper(X)
        stats::predict(objects$model, newdata = X) -> pred
        if(config$predict_probabilities){
          return(getPredictionProbabilities(pred))
        } else {
          return(getPredictionResponse(pred) %>% as.numeric() - 1)
        }
      }
    )
)
                                
# A simple logistic regression classifier from scikit python package:
# It extracts only numeric features, does no dummification for categorical columns.
SCIKIT.LR = setRefClass('SCIKIT.LR', contains = "CLASSIFIER.SCIKIT",
    methods = list(
      initialize = function(settings = list(), ...){
        callSuper(settings = settings, ...)
        config$sig_level <<- settings$sig_level %>% verify('numeric', domain = c(0,1), default = '0.1')
        type               <<- 'Logistic Regression'
      },

      reset = function(...){
        callSuper(...)
        module_lm = reticulate::import('sklearn.linear_model')
        objects$model <<- module_lm$LogisticRegression(penalty = 'l1',solver = 'liblinear')
        # todo: define hyper parameters in settings
      },

      fit = function(X, y, do_transform = T){
        if(!fitted){
          if(do_transform) X = transform(X, y) %>% remove_invariant_features
          featnames <- gener::numerics(X)
          X = X[featnames]
          objects$model$fit(X %>% data.matrix, y)
          objects$model$coef_ %>% abs %>% as.numeric -> weights
          objects$features <<- data.frame(name = featnames, importance = (weights/(X %>% apply(2, sd))) %>% na2zero %>% {./geomean(.[.>0])} %>% as.numeric, stringsAsFactors = F)
          fitted <<- T
        }
      }
   )
)

# Scikit Logistic Regression with feature elimination:
SCIKIT.LR.WFE = setRefClass('SCIKIT.LR.WFE', contains = "SCIKIT.LR", 
  methods = list(
    fit = function(X, y, do_transform = T){
      callSuper(X, y, do_transform = do_transform)
      
      features <- get.features() %>% pull(name)
      reset(T)
      if(do_transform) X = transform(X, y) %>% remove_invariant_features
      callSuper(X[, features], y, do_transform = F)
    },
    
    get.features = function(){
      objects$features <<- callSuper() %>% filter(importance > 0)
      return(objects$features)
    }
    
  ))

# A simple logistic regression classifier from scikit python package:
# It extracts only numeric features, does no dummification for categorical columns.
SCIKIT.DT = setRefClass('SCIKIT.DT', contains = "CLASSIFIER.SCIKIT",
  methods = list(
    initialize = function(settings = list(), ...){
      callSuper(settings = settings, ...)
      type               <<- 'Decision Tree'
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
    },
    
    get.performance.fit = function(){
      # todo ...
    },

    # lazy
    get.parameters = function(){
      # todo ...
    },

    # lazy
    get.predictor = function(){
      # todo ...
    },

    get.expert.predictor = function(X, y){
    }
  )
)

SCIKIT.XGB = setRefClass('SCIKIT.XGB', contains = "CLASSIFIER.SCIKIT",
    methods = list(
      initialize = function(settings = list(), ...){
        callSuper(settings = settings, ...)
        type               <<- 'Extreme Gradient Boosting'
      },
      
      reset = function(...){
        callSuper(...)
        module_xgb = reticulate::import('xgboost')
        objects$model <<- module_xgb$XGBClassifier()
      }    
    )
) 

