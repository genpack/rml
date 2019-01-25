# A simple logistic regression classifier from scikit python package:
# It extracts only numeric features, does no dummification for categorical columns.
SCIKIT.LR = setRefClass('SCIKIT.LR', contains = "MODEL",
    methods = list(
      initialize = function(settings = list(), ...){
        callSuper(...)
        config$sig_level <<- settings$sig_level %>% verify('numeric', domain = c(0,1), default = '0.1')
        type               <<- 'Logistic Regression'
        if(!require(reticulate)) stop("Package 'reticulate' is not installed!")
        if(!is.null(settings$python_address)){
          use_python(settings$python_address)
        }
        module_lm = reticulate::import('sklearn.linear_model')
        objects$model <<- module_lm$LogisticRegression(penalty = 'l1',solver = 'liblinear')
        # pandas = reticulate::import('pandas')
      },

      fit = function(X, y){
        X = transform(X)
        objects$model$fit(X, y)
        fitted <<- T
      },

      get.features.name = function(){
        #names(objects$model$coefficients[-1])
      },

      get.features.weight = function(){
        if(is.null(objects$model.summary)){objects$model.summary <<- summary(objects$model)}
        pv   = objects$model.summary$coefficients[-1, 'Pr(>|t|)']
        keep = (pv < 0.1)
        weights = pv
        weights[!keep] <- 0
        weights[keep]  <- 1.0 - weights[keep]/0.1
        return(weights/sum(weights))
      },

      predict = function(X){
        objects$model$predict(transform(X))
      },

      get.performance.fit = function(){
        if(is.null(objects$model.summary)){objects$model.summary <<- summary(objects$model)}
        return(objects$model.summary$adj.r.squared)
      },

      # todo: add k-fold, chronological shuffle, chronological split
      get.performance.cv = function(X, y, ntest = 20, split = 0.3, method = 'shuffle'){
        method = match.arg(method)
        keep   = objects$model

        X  = transform(X)
        N  = nrow(X)
        scores = c()

        for (i in sequence(ntest)){
          trindex = N %>% sequence %>% sample(size = floor((1 - config$split_ratio)*N))

          X_train = X[trindex, ]
          y_train = y[trindex]
          X_test  = X[- trindex,]
          y_test  = y[- trindex]

          objects$model$fit(X_train, y_train)
          scores = c(scores, objects$model$score(X_test, y_test))
        }
        objects$model <<- keep
        return(scores)
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
