
CLASSIFIER = setRefClass('CLASSIFIER', contains = "MODEL",
  methods = list(
    initialize = function(settings = list(), ...){
      callSuper(settings = settings, ...)
      config$sig_level <<- settings$sig_level %>% verify('numeric', domain = c(0,1), default = '0.1')
      type             <<- 'Abstract Classifier'
      if(is.null(settings$cross_validation$performance_meter)){
        config$cross_validation$performance_meter <<- function(y1, y2) mean(xor(y1, y2), na.rm = T)
      }
    },

    fit = function(X, y){
        if(!fitted){
          X = transform(X, y)
          objects$model$fit(X, y)
          fitted <<- T
        }
    },

    predict = function(X){
      callSuper()
      X = transform(X)
      objects$model$predict(X)
    },

  )
)


# A simple logistic regression classifier from scikit python package:
# It extracts only numeric features, does no dummification for categorical columns.
SCIKIT.LR = setRefClass('SCIKIT.LR', contains = "CLASSIFIER",
    methods = list(
      initialize = function(settings = list(), ...){
        callSuper(settings = settings, ...)
        config$sig_level <<- settings$sig_level %>% verify('numeric', domain = c(0,1), default = '0.1')
        type               <<- 'Logistic Regression'
        if(!require(reticulate)) stop("Package 'reticulate' is not installed!")
        if(!is.null(settings$python_address)){
          use_python(settings$python_address)
        }
        reset()
        # pandas = reticulate::import('pandas')
      },

      reset = function(...){
        callSuper(...)
        module_lm = reticulate::import('sklearn.linear_model')
        objects$model <<- module_lm$LogisticRegression(penalty = 'l1',solver = 'liblinear')
      },

      fit = function(X, y){
        if(!fitted){
          X = transform(X, y)
          X = X[gener::numerics(X)]
          objects$model$fit(X, y)
          fitted <<- T
        }
      },

      get.feature.weights = function(){
        if(is.null(objects$model.summary)){objects$model.summary <<- summary(objects$model)}
        pv   = objects$model.summary$coefficients[-1, 'Pr(>|t|)']
        keep = (pv < 0.1)
        weights = pv
        weights[!keep] <- 0
        weights[keep]  <- 1.0 - weights[keep]/0.1
        return(weights/sum(weights))
      },

      predict = function(X){
        callSuper()
        X = transform(X)
        X = X[gener::numerics(X)]
        objects$model$predict(X)
      },

      get.performance.fit = function(){
        if(is.null(objects$model.summary)){objects$model.summary <<- summary(objects$model)}
        return(objects$model.summary$adj.r.squared)
      },

      # lazy
      get.parameters = function(){
        list(model = objects$model)
      },

      # lazy
      get.predictor = function(){
        function(inputs, params){
          params$model$predict(inputs %>% as.data.frame)
        }
      },

      get.predictor.gradient = function(){
        function(inputs, params, wrt){
          params$coeff_[wrt]
        }
      },

      get.expert.predictor = function(X, y){
      }
    )
)


# A simple logistic regression classifier from scikit python package:
# It extracts only numeric features, does no dummification for categorical columns.
SCIKIT.DT = setRefClass('SCIKIT.DT', contains = "CLASSIFIER",
  methods = list(
    initialize = function(settings = list(), ...){
      callSuper(settings = settings, ...)
      config$sig_level <<- settings$sig_level %>% verify('numeric', domain = c(0,1), default = '0.1')
      type               <<- 'Decision Tree'
      if(!require(reticulate)) stop("Package 'reticulate' is not installed!")
      if(!is.null(settings$python_address)){
        use_python(settings$python_address)
      }
      reset()
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
