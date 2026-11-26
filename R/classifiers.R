#' @include transformers.R

#' @title Abstract base class of binary classifiers
#' @name CLASSIFIER-class
#' @aliases CLASSIFIER
#' @description
#' All rml classifiers are binary: \code{model.predict()} returns the probability of class 1, and the
#' \code{return} setting converts it to log-odds or 0/1 classes. When the classifier has gradient
#' transformers their output (log-odds) is added before that conversion. Inherit from this class to write
#' your own classifier: implement \code{model.fit(X, y)} and \code{model.predict(X)}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{return}}{\code{'probs'} (default), \code{'logit'} or \code{'class'}.}
#'   \item{\code{return_type_in_output}}{Add the return type to the output column name. Default TRUE.}
#'   \item{\code{threshold}}{Probability threshold for \code{return = 'class'}. Default 0.5.}
#'   \item{\code{threshold_determination}}{\code{'set_as_config'} (default), \code{'maximum_f1'}, \code{'maximum_chi'} or \code{'label_rate_quantile'}: how the threshold is chosen on the training data.}
#'   \item{\code{metric}}{Main metric, default \code{'gini'} (\code{'f1'} for class output).}
#'   \item{\code{metrics}}{Default metrics of \code{performance()}, default \code{'pearson'}.}
#'   \item{\code{sig_level}}{Significance level used by some subclasses. Default 0.1.}
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{predict_probs(X)}}{Probabilities of class 1 as a vector, whatever \code{return} is set to.}
#'   \item{\code{set_threshold(X, y)}}{Chooses \code{threshold} according to \code{threshold_determination}.}
#' }
#' @seealso \code{\link{MODEL-class}}, \code{\link{classifier_transform_yout}}
#' @export CLASSIFIER
#' @exportClass CLASSIFIER
CLASSIFIER = setRefClass('CLASSIFIER', contains = "MODEL",
  methods = list(
    initialize = function(...){
      callSuper(...)
      initialize_classifier()
    },
    
    # Since this class is being used as a second parent class, it requires another initialize_classifier method.
    initialize_classifier = function(){
      type             <<- 'Binary Classifier'
      config[['sig_level']] <<- verify(config[['sig_level']], 'numeric', domain = c(0,1), default = 0.1)
      config[['return']]    <<- verify(config[['return']], 'character', domain = c('probs', 'logit', 'class'), default = 'probs')
      config[['return_type_in_output']] <<- verify(config[['return_type_in_output']], 'logical', lengths = 1, domain = c(T,F), default = T)
      config[['threshold']] <<- verify(config[['threshold']], 'numeric', lengths = 1, domain = c(0,1), default = 0.5)
      config[['threshold_determination']] <<- verify(config[['threshold_determination']], 'character', lengths = 1,
                                                domain = c('set_as_config', 'maximum_f1', 'maximum_chi', 'label_rate_quantile'), default = 'set_as_config')
      # todo: add 'target_precision', 'target_recall', 'ratio_quantile'
      reserved_words <<- c(reserved_words, 'sig_level', 'return', 'return_type_in_output', 'threshold', 'threshold_determination')
      packages_required <<- c(packages_required, 'stats')
      if(is.null(config[['metric']])){
        config[['metric']] <<- chif(config[['return']] == 'class', 'f1', 'gini')
      }
      if(is.null(config$metrics)){config$metrics <<- 'pearson'}
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
      classifier_transform_yout(.self, X, Y)
    },

    # Returns predicted probabilities of class 1 as a numeric vector, whatever the configured 'return' type
    predict_probs = function(X){
      keep = list(return = config$return, name_in_output = config$name_in_output, keep_columns = config$keep_columns, keep_features = config$keep_features)
      config$return <<- 'probs'; config$name_in_output <<- FALSE; config$keep_columns <<- FALSE; config$keep_features <<- FALSE
      on.exit({for(cn in names(keep)){config[[cn]] <<- keep[[cn]]}})
      predict(X)[[1]]
    },

    set_threshold = function(X, y){
      if(fitted & (config$threshold_determination != 'set_as_config')){
        y_prob = predict_probs(X)
        if(config$threshold_determination == 'maximum_f1'){
          res = data.frame(y_pred = y_prob, y_true = y) %>% optSplit.f1('y_pred', 'y_true')
          config$threshold <<- res$split
        }
        else if(config$threshold_determination == 'maximum_chi'){
          res = data.frame(y_pred = y_prob, y_true = y) %>% optSplit.chi('y_pred', 'y_true')
          config$threshold <<- res$split
        }
        else if(config$threshold_determination == 'label_rate_quantile'){
          config$threshold <<- quantile(y_prob, probs = 1 - mean(y, na.rm = T), names = F)
        }
      }
    },

    performance = function(X, y, metrics = config$metrics, ...){
      yp = predict(X) %>% {.[[ncol(.)]]}
      # we transform y only if yin has a transformer function and yout has not. This means the y in training set has been transformed
      # so the input y needs to have the same transformation to be comparable to the original label.
      # if yout has transformer function as well, then yp is already transformed and should be compared to y directly not to the transformed y
      if(!is.null(config$yin_transformer_function) & is.null(config$yout_transformer_function)){
        y = do.call(what = config$yin_transformer_function, args = list(y, config$yin_transformer_arguments))
      }
      correlation(yp, y, metrics = metrics, ...)
    }
  )
)

# Shared output transformation of all binary classifiers.
# Y holds predicted probabilities of class 1. If the model has gradient transformers, their (logit) output is added in log-odds space.
# The result is then converted to the configured 'return' type: 'probs', 'logit' or 'class'.
#' @title Output transformation of binary classifiers
#' @description
#' Adds gradient transformer output (log-odds) to the predicted probabilities, applies the output function and converts to the configured \code{return} type. Used by all classifiers.
#' @param model A \code{\link{CLASSIFIER-class}} object.
#' @param X Input data.
#' @param Y \code{data.frame} of predicted probabilities.
#' @return \code{data.frame}.
#' @keywords internal
#' @export
classifier_transform_yout = function(model, X, Y){
  cfg = model$config
  if(length(model$gradient_transformers) > 0){
    grad = model$y_gradient(X)
    for(i in numerics(Y)){Y[[i]] <- logit_inv(logit_fwd(Y[[i]]) + grad)}
  }
  if(!is.null(cfg$yout_transformer_function)){
    Y = do.call(what = cfg$yout_transformer_function, args = list(Y, cfg$yout_transformer_arguments))
  }
  if(cfg[['return']] == 'logit'){
    for(i in numerics(Y)){Y[[i]] <- logit_fwd(Y[[i]])}
  } else if (cfg[['return']] == 'class'){
    for(i in numerics(Y)){Y[[i]] <- as.numeric(Y[[i]] > cfg[['threshold']])}
  }
  if(isTRUE(cfg[['return_type_in_output']])){
    colnames(Y) <- cfg[['return']]
  } else {colnames(Y) <- ''}
  return(Y)
}

#' @title Wrapper for scikit-learn binary classifiers
#' @name CLS.SKLEARN-class
#' @aliases CLS.SKLEARN
#' @description
#' Superclass of scikit-learn classifiers: fits \code{sklearn.<model.module>.<model.class>} and returns \code{predict_proba()[, 2]}.
#' Requires Python with the module named below, reachable through reticulate. Set \code{python_address} to choose the Python executable; see \code{\link{python_setup}} and \code{vignette("rml-troubleshooting")} if the module is not found.
#'
#' Inherits from \code{\link{CLASSIFIER-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{model.module}}{sklearn sub-module.}
#'   \item{\code{model.class}}{sklearn classifier class.}
#' }
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = CLS.SKLEARN(model.module = 'naive_bayes', model.class = 'GaussianNB')
#' m$fit(X, y)
#' head(m$predict(X))
#' }
#' @export CLS.SKLEARN
CLS.SKLEARN = setRefClass('CLS.SKLEARN', contains = c('TRM.SKLEARN', "CLASSIFIER"),
   methods = list(
     initialize = function(...){
       callSuper(...)
       initialize_classifier()
     },
     
     model.predict = function(X){
       if(inherits(X, 'WIDETABLE')){X = wt_as_matrix(X)}
       retrieve_model()
       objects$model$predict_proba(X %>% data.matrix)[,2, drop = F] %>% as.data.frame
     },
     
     # Methods of the second parent (CLASSIFIER) are hidden by the first parent (TRM.SKLEARN) when both define them, so repeat them here:
     transform_yout = function(X, Y = NULL){
       classifier_transform_yout(.self, X, Y)
     }
  )
)

#' @title K-nearest neighbours classifier (scikit-learn)
#' @name CLS.SKLEARN.KNN-class
#' @aliases CLS.SKLEARN.KNN
#' @description
#' Wraps \code{sklearn.neighbors.KNeighborsClassifier}. Any of its arguments (\code{n_neighbors}, \code{weights}, ...) can be passed; whole numbers are sent to Python as integers.
#' Requires Python with the module named below, reachable through reticulate. Set \code{python_address} to choose the Python executable; see \code{\link{python_setup}} and \code{vignette("rml-troubleshooting")} if the module is not found.
#'
#' Inherits from \code{\link{CLS.SKLEARN-class}}.
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = CLS.SKLEARN.KNN(n_neighbors = 15L)
#' m$fit(X, y)
#' head(m$predict(X))
#' }
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

#' @title Fused lasso logistic regression (HDPenReg)
#' @name CLS.HDPENREG.FLASSO-class
#' @aliases CLS.HDPENREG.FLASSO
#' @description
#' Logistic regression with fused lasso penalty from \code{HDPenReg::EMfusedlasso}. Numeric features only.
#'
#' Inherits from \code{\link{CLASSIFIER-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{lambda1}}{Lasso penalty. Default 1.}
#'   \item{\code{lambda2}}{Fusion penalty. Default 1.}
#' }
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = CLS.HDPENREG.FLASSO(lambda1 = 0.1, lambda2 = 0.1)
#' m$fit(X, y)
#' head(m$predict(X))
#' }
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
    if(inherits(X, 'WIDETABLE')){X = wt_as_matrix(X)}
    
    objects$model <<- do.call(HDPenReg::EMfusedlasso, list(X = X %>% as.matrix, y = y) %<==>% (config %>% list.remove(reserved_words)))
    objects$features$importance <<- objects$model$coefficient
  },

  model.predict = function(X){
    if(inherits(X, 'WIDETABLE')){X = wt_as_matrix(X)}
    
    X %>% as.matrix %*% objects$model$coefficient %>% logit_inv %>% as.data.frame
  }
))

#' @title Any mlr classification learner
#' @name CLS.MLR-class
#' @aliases CLS.MLR
#' @description
#' Wraps an mlr learner (\code{mlr::makeLearner(model_type, predict.type = 'prob')}).
#'
#' Inherits from \code{\link{CLASSIFIER-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{model_type}}{mlr learner name, e.g. \code{'classif.randomForest'}. Default \code{'classif.gbm'}.}
#' }
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = CLS.MLR(model_type = 'classif.logreg')
#' m$fit(X, y)
#' head(m$predict(X))
#' }
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
#' @title Logistic regression (scikit-learn)
#' @name CLS.SKLEARN.LR-class
#' @aliases CLS.SKLEARN.LR
#' @description
#' Wraps \code{sklearn.linear_model.LogisticRegression} (arguments \code{C}, \code{penalty}, \code{solver}, ...). Uses numeric features only. Feature importance is the absolute coefficient scaled by the feature's standard deviation.
#' Requires Python with the module named below, reachable through reticulate. Set \code{python_address} to choose the Python executable; see \code{\link{python_setup}} and \code{vignette("rml-troubleshooting")} if the module is not found.
#'
#' \code{max_iter} defaults to 1000 (scikit-learn's default is 100). If scikit-learn still warns that \code{lbfgs failed to converge},
#' scale the features, for example with \code{transformers = list(MAP.RML.ZFS())}, or raise \code{max_iter}.
#'
#' Inherits from \code{\link{CLS.SKLEARN-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{max_iter}}{Maximum solver iterations. Default 1000.}
#'   \item{other}{Any argument of \code{LogisticRegression}. Whole numbers are passed to Python as integers, so \code{max_iter = 500} works.}
#' }
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = CLS.SKLEARN.LR(C = 0.5)
#' m$fit(X, y)
#' head(m$predict(X))
#' }
#' @export CLS.SKLEARN.LR
CLS.SKLEARN.LR = setRefClass('CLS.SKLEARN.LR', contains = "CLS.SKLEARN",
    methods = list(
      initialize = function(...){
        callSuper(model.module = 'linear_model', model.class = 'LogisticRegression', ...)
        description <<- 'Logistic Regression'
        if(is.empty(name)){name <<- 'SKLR' %>% paste0(sample(10000:99999, 1))}
        # scikit-learn's default (100) is often too few for unscaled data
        config$max_iter <<- verify(config$max_iter, c('numeric', 'integer'), lengths = 1, domain = c(1, Inf), default = 1000) %>% as.integer
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
        rfun::build_lincomb(name = name, length(objects$features$fname), features = objects$features$fname, parameter_values = c(as.numeric(objects$model$intercept_), as.numeric(objects$model$coef_)), ...)
      }
   )
)

# A simple decision tree classifier from SKLEARN python package:
#' @title Decision tree classifier (scikit-learn)
#' @name CLS.SKLEARN.DT-class
#' @aliases CLS.SKLEARN.DT
#' @description
#' Wraps \code{sklearn.tree.DecisionTreeClassifier} (arguments \code{max_depth}, \code{min_samples_leaf}, ...).
#' Requires Python with the module named below, reachable through reticulate. Set \code{python_address} to choose the Python executable; see \code{\link{python_setup}} and \code{vignette("rml-troubleshooting")} if the module is not found.
#'
#' Inherits from \code{\link{CLS.SKLEARN-class}}.
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = CLS.SKLEARN.DT(max_depth = 3L)
#' m$fit(X, y)
#' head(m$predict(X))
#' }
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

#' @title Logistic regression (stats::glm)
#' @name CLS.STATS.LR-class
#' @aliases CLS.STATS.LR
#' @description
#' Binomial \code{glm} on all input columns. Feature importance is derived from coefficient p-values (1 for p = 0, 0 for p >= 0.1).
#' Supports gradient transformers: their log-odds enter the model as an offset, so the regression learns the correction on top of them.
#' Other \code{glm} arguments (\code{weights}, \code{control}, ...) can be passed.
#'
#' Inherits from \code{\link{CLASSIFIER-class}}.
#' @examples
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = CLS.STATS.LR(name = 'lr')
#' m$fit(X, y)
#' head(m$predict(X))
#' # boosted by xgboost (requires the xgboost package):
#' if(requireNamespace('xgboost', quietly = TRUE)){
#'   m2 = CLS.STATS.LR(gradient_transformers = list(CLS.XGBOOST(nrounds = 10, verbose = 0)))
#'   m2$fit(X, y)
#' }
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
                             pv   = objects$model.summary$coefficients[-1, 'Pr(>|z|)'] %>% na2zero
                             names(pv) <- gsub('`', '', names(pv))
                             # Usually NA p-values appear when there is a perfect fit (100% R-squared), so each feature shall be considerd as important!?
                             keep = (pv < 0.1)
                             weights = pv
                             weights[!keep] <- 0
                             weights[keep]  <- 1.0 - weights[keep]/0.1
                             return(weights)
                           },
                           
                           model.fit = function(X, y){
                             arguments = config %>% list.remove(c(reserved_words, 'family'))
                             data = as.data.frame(X)
                             data$.rml_y <- as.numeric(y)
                             rhs = paste0('`', colnames(X), '`', collapse = ' + ')
                             # Output of gradient transformers (log-odds) enters as an offset, so the regression learns the correction on top of it:
                             grad = attr(y, 'gradient')
                             if(!is.null(grad)){
                               data$.rml_offset <- as.numeric(grad)
                               rhs = paste(rhs, 'offset(.rml_offset)', sep = ' + ')
                             }
                             arguments$data <- data
                             arguments$formula <- as.formula(paste('.rml_y ~', rhs))
                             arguments$family <- stats::binomial
                             objects$model <<- do.call(stats::glm, args = arguments)
                             # Feature Importances:
                             gw = get.features.weight()
                             objects$features$importance <<- gw[objects$features$fname]
                           },
                           
                           model.predict = function(X){
                             if(inherits(X, 'WIDETABLE')){X = wt_as_data_frame(X)}
                             X = as.data.frame(X) %>% na2zero
                             # Prediction of the regression alone; gradient transformer output is added later in transform_yout:
                             X$.rml_offset <- 0
                             objects$model %>% stats::predict(X, type = "response") %>% unname %>% as.data.frame
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
                           #                 if(inherits(X, 'WIDETABLE')){X = wt_as_matrix(X)}
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


#' @title XGBoost classifier (Python xgboost)
#' @name CLS.SKLEARN.XGB-class
#' @aliases CLS.SKLEARN.XGB
#' @description
#' Wraps \code{xgboost.XGBClassifier} through reticulate (arguments \code{n_estimators}, \code{max_depth}, \code{learning_rate}, ...). For the R xgboost package use \code{\link{CLS.XGBOOST-class}}.
#' Requires Python with the module named below, reachable through reticulate. Set \code{python_address} to choose the Python executable; see \code{\link{python_setup}} and \code{vignette("rml-troubleshooting")} if the module is not found.
#'
#' Inherits from \code{\link{CLASSIFIER-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{n_jobs}}{Number of threads (converted to integer).}
#' }
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = CLS.SKLEARN.XGB(n_estimators = 50L, max_depth = 3L)
#' m$fit(X, y)
#' head(m$predict(X))
#' }
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
        save_model_object(paste0(path, '/', name, '.joblib'))
        release_model()
      },
      
      model.load = function(path = getwd()){
        callSuper(path)
        fn   = paste0(path, '/', name, '.joblib')
        pass = file.exists(fn)
        rutils::assert(pass, paste0('File ', fn , ' does not exist!'))
        if(pass){load_model_object(fn)}
      },
      
      save_model_object = function(filename){
        joblib = reticulate::import('joblib')
        joblib$dump(objects$model, filename)
      },
      
      load_model_object = function(filename){
        joblib = reticulate::import('joblib')
        objects$module <<- reticulate::import('xgboost')
        objects$model  <<- joblib$load(filename)
      },
      
      # save model object in a tempfile temporarily
      keep_model = function(filename){
        callSuper()
        objects$model_filename <<- tempfile() %>% gsub(pattern = "\\\\", replacement = "/")
        save_model_object(objects$model_filename)
      },
      
      retrieve_model = function(){
        callSuper()
        if(!is.null(objects$model_filename)){
          if(file.exists(objects$model_filename)){
            load_model_object(objects$model_filename)
          }
        }
      },
      
      model.fit = function(X, y){
        objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
        X = X[objects$features$fname]
        if(inherits(X, 'WIDETABLE')){X = wt_as_matrix(X)}
        objects$module <<- reticulate::import('xgboost')

        objects$model <<- do.call(objects$module$XGBClassifier, python_args(config, reserved_words))
        objects$model$fit(X %>% data.matrix, y)
        imp = try(objects$model$feature_importances_ %>% as.numeric, silent = T)
        if(inherits(imp, 'numeric')) objects$features$importance <<- imp
      },

      model.predict = function(X){
        if(inherits(X, 'WIDETABLE')){X = wt_as_matrix(X)}
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
                      if(inherits(X, 'WIDETABLE')){X = wt_as_matrix(X)}
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

#' @title Support vector machine classifier (scikit-learn)
#' @name CLS.SKLEARN.SVM-class
#' @aliases CLS.SKLEARN.SVM
#' @description
#' Wraps \code{sklearn.svm.SVC} with \code{probability = TRUE} (arguments \code{C}, \code{kernel}, \code{gamma}, ...).
#' Requires Python with the module named below, reachable through reticulate. Set \code{python_address} to choose the Python executable; see \code{\link{python_setup}} and \code{vignette("rml-troubleshooting")} if the module is not found.
#'
#' Inherits from \code{\link{CLS.SKLEARN-class}}.
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = CLS.SKLEARN.SVM(C = 1)
#' m$fit(X, y)
#' head(m$predict(X))
#' }
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

#' @title Deep neural network classifier (keras)
#' @name CLS.KERAS.DNN-class
#' @aliases CLS.KERAS.DNN
#' @description
#' Dense feed-forward network built with keras/tensorflow. Supports gradient transformers: their log-odds are fed as a
#' second input and added to the network's linear output inside a custom logistic loss.
#'
#' Inherits from \code{\link{CLASSIFIER-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{num_layers}}{Number of hidden layers (1 to 25). Default 1.}
#'   \item{\code{first_layer_nodes}}{Units in the first hidden layer. Default 128.}
#'   \item{\code{layer_nodes_ratio}}{Each layer has this share of the previous layer's units. Default 0.4.}
#'   \item{\code{layers_activation}}{Activation of hidden layers. Default \code{'relu'}.}
#'   \item{\code{layers_dropout}}{Dropout rate after each hidden layer. Default 0.25.}
#'   \item{\code{layers}}{Custom list of layer specifications; overrides the five settings above.}
#'   \item{\code{kernel_regularization_penalty_l1, kernel_regularization_penalty_l2}}{Weight penalties. Default 0.}
#'   \item{\code{activity_regularization_penalty_l1, activity_regularization_penalty_l2}}{Activity penalties. Default 0.}
#'   \item{\code{kernel_initializer}}{Initializer; by default uniform between \code{initializer_minval} (-0.1) and \code{initializer_maxval} (0.1) with \code{initializer_seed} (42).}
#'   \item{\code{epochs}}{Training epochs. Default 5.}
#'   \item{\code{batch_size}}{Default 32.}
#'   \item{\code{optimizer}}{One of \code{'adam'} (default), \code{'adadelta'}, \code{'adagrad'}, \code{'adamax'}, \code{'nadam'}, \code{'rmsprop'}, \code{'sgd'}.}
#'   \item{\code{learning_rate}}{Default 1e-4.}
#'   \item{\code{loss}}{Keras loss. Default \code{'categorical_crossentropy'}.}
#'   \item{\code{callback}}{Function called at the end of each epoch.}
#' }
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = CLS.KERAS.DNN(num_layers = 2, epochs = 10)
#' m$fit(X, y)
#' head(m$predict(X))
#' }
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

      if(inherits(X, 'WIDETABLE')){X = wt_as_matrix(X)}
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
      if(inherits(X, 'WIDETABLE')){X = wt_as_matrix(X)}
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

#' @title Gradient boosted trees on Spark (sparklyr)
#' @name CLS.SPARKLYR.GBT-class
#' @aliases CLS.SPARKLYR.GBT
#' @description
#' Trains \code{sparklyr::ml_gbt_classifier} on a Spark connection. Other arguments of that function can be passed.
#'
#' Inherits from \code{\link{CLASSIFIER-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{spark_home}}{Spark installation. Default \code{Sys.getenv('SPARK_HOME')}.}
#'   \item{\code{spark_config}}{Spark configuration. Default \code{sparklyr::spark_config()}.}
#'   \item{\code{connection}}{An existing Spark connection; a local one is created if missing.}
#' }
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = CLS.SPARKLYR.GBT(max_iter = 20)
#' m$fit(X, y)
#' head(m$predict(X))
#' }
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


#' @title Naive Bayes classifier (e1071)
#' @name CLS.E1071.NB-class
#' @aliases CLS.E1071.NB
#' @description
#' Wraps \code{e1071::naiveBayes}.
#'
#' Inherits from \code{\link{CLASSIFIER-class}}.
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = CLS.E1071.NB()
#' m$fit(X, y)
#' head(m$predict(X))
#' }
#' @export CLS.E1071.NB
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
#' @title Multinomial naive Bayes classifier (scikit-learn)
#' @name CLS.SKLEARN.MNB-class
#' @aliases CLS.SKLEARN.MNB
#' @description
#' Wraps \code{sklearn.naive_bayes.MultinomialNB}. Uses integer (count) columns only.
#' Requires Python with the module named below, reachable through reticulate. Set \code{python_address} to choose the Python executable; see \code{\link{python_setup}} and \code{vignette("rml-troubleshooting")} if the module is not found.
#'
#' Inherits from \code{\link{CLS.SKLEARN-class}}.
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' X$c = rpois(300, 3)
#' m = CLS.SKLEARN.MNB()
#' m$fit(X, y)
#' head(m$predict(X))
#' }
#' @export CLS.SKLEARN.MNB
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
#' @rdname xgboost_eval_functions
#' @export
pmg_accuracy <- function(preds, dtrain){
  labels <- xgboost::getinfo(dtrain, "label")
  lgrads <- attr(dtrain, 'gradient')
  # base margin (gradient transformer output) is already included in preds; old-style gradient attribute is added if present:
  if(!is.null(lgrads)) preds <- preds + lgrads
  tp     <- sum(labels & (preds > 0))
  tn     <- sum((!labels) & (preds < 0))
  prf    <- (tp + tn)/length(labels)
  return(list(metric = "accuracy", value = prf))
}

#' @title Custom xgboost evaluation functions
#' @description
#' Evaluation functions for \code{feval} of \code{\link{CLS.XGBOOST-class}}: gini, accuracy, log loss and lift of the predicted log-odds.
#' @param preds Predicted log-odds.
#' @param dtrain \code{xgb.DMatrix} with labels.
#' @return \code{list(metric = , value = )}.
#' @rdname xgboost_eval_functions
#' @export
pmg_gini <- function(preds, dtrain){
  labels <- xgboost::getinfo(dtrain, "label")
  lgrads <- attr(dtrain, 'gradient')
  # base margin (gradient transformer output) is already included in preds; old-style gradient attribute is added if present:
  if(!is.null(lgrads)) preds <- preds + lgrads
  prf    <- correlation(preds, labels, 'gini')
  return(list(metric = "gini", value = prf))
}

#' @rdname xgboost_eval_functions
#' @export
pmg_logloss <- function(preds, dtrain){
  labels <- xgboost::getinfo(dtrain, "label")
  lgrads <- attr(dtrain, 'gradient')
  # base margin (gradient transformer output) is already included in preds; old-style gradient attribute is added if present:
  if(!is.null(lgrads)) preds <- preds + lgrads
  prf    <- correlation(preds, labels, 'log_loss')
  return(list(metric = "log_loss", value = prf))
}

#' @rdname xgboost_eval_functions
#' @export
pmg_lift <- function(preds, dtrain){
  labels <- xgboost::getinfo(dtrain, "label")
  lgrads <- attr(dtrain, 'gradient')
  # base margin (gradient transformer output) is already included in preds; old-style gradient attribute is added if present:
  if(!is.null(lgrads)) preds <- preds + lgrads
  prf    <- correlation(preds, labels, 'lift')
  return(list(metric = "lift", value = prf))
}

#' @title XGBoost classifier (R xgboost)
#' @name CLS.XGBOOST-class
#' @aliases CLS.XGBOOST
#' @description
#' Gradient boosted trees with \code{xgboost::xgb.train}. Any booster parameter (\code{max_depth}, \code{eta}, \code{subsample},
#' \code{colsample_bytree}, \code{scale_pos_weight}, ...) can be passed. Works with xgboost 1.x and 3.x.
#' Supports gradient transformers: their log-odds are set as the base margin of the training data.
#'
#' Inherits from \code{\link{CLASSIFIER-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{nrounds}}{Number of boosting rounds. Default 100.}
#'   \item{\code{objective}}{Default \code{'binary:logistic'}.}
#'   \item{\code{nthread}}{Threads. Default 1.}
#'   \item{\code{early_stopping_rounds}}{Stop when the validation metric has not improved for this many rounds (uses \code{cv.set}).}
#'   \item{\code{show_progress}}{Print evaluation progress. Default FALSE.}
#'   \item{\code{print_every_n}}{Default 1.}
#'   \item{\code{eval_metric}}{Evaluation metric when progress or early stopping is on. Default \code{'auc'}.}
#'   \item{\code{obj}}{Custom objective function.}
#'   \item{\code{feval}}{Custom evaluation function, e.g. \code{\link{pmg_gini}}.}
#'   \item{\code{maximize}}{Whether \code{feval} is maximised.}
#'   \item{\code{xgb_model}}{Model to continue training from.}
#'   \item{\code{callbacks}}{List of xgboost callbacks.}
#'   \item{\code{save_period, save_name}}{Periodic saving during training.}
#' }
#' @examples
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' if(requireNamespace('xgboost', quietly = TRUE)){
#'   m = CLS.XGBOOST(nrounds = 20, max_depth = 3, eta = 0.2, verbose = 0)
#'   m$fit(X, y)
#'   head(m$predict(X))
#' }
#' @export CLS.XGBOOST
CLS.XGBOOST = setRefClass('CLS.XGBOOST', contains = 'CLASSIFIER', methods = list(
  initialize = function(...){
    callSuper(...)
    package          <<- 'xgboost'
    package_language <<- 'R'
    packages_required <<- c(packages_required, 'xgboost')
    
    description      <<- 'Extreme Gradient Boosting'
    if(is.empty(name)){name <<- 'XGB' %>% paste0(sample(10000:99999, 1))}
    assert(requireNamespace('xgboost', quietly = TRUE), "Package 'xgboost' needs to be installed!")
    # Default objective is 'binary:logistic'. Pass config 'objective' to change it (for example 'binary:logitraw'),
    # or a custom objective function as config 'obj'.

    reserved_words <<- c(reserved_words, 'nrounds', 'watchlist', 'obj', 'feval', 'verbose', 'print_every_n', 'early_stopping_rounds',
                       'maximize', 'save_period', 'save_name', 'xgb_model', 'callbacks', 'nthread', 'show_progress', 'python_address')
    
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
    objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
    X = X[objects$features$fname]
    if(ncol(X) == 0){stop('No numeric columns in the input dataset!')}
    if(inherits(X, 'WIDETABLE')){X = wt_as_matrix(X)}

    # Output of gradient transformers (log-odds) is passed to xgboost as base margin,
    # so the trees learn a correction on top of it with the standard logistic loss:
    grad = attr(y, 'gradient')
    dtrain = xgb_dmatrix(X, label = as.numeric(y), base_margin = grad)

    need_eval = config$show_progress | !is.null(config$early_stopping_rounds)
    dvalidation = list()
    if(need_eval){
      dvalidation = list(train = dtrain)
      # vp: Validation Pack
      for(vp in config$cv.set){
        nvs = length(dvalidation)
        grd = chif(length(gradient_transformers) > 0, y_gradient(X = vp$X), NULL)
        dvalidation[[paste0("Validation_", nvs)]] <- xgb_dmatrix(vp$X[objects$features$fname], label = as.numeric(vp$y), base_margin = grd)
      }
    }

    params = config %>% list.remove(reserved_words)
    if(is.null(config[['obj']]) & is.null(params[['objective']])){params$objective <- 'binary:logistic'}
    if(!is.null(config[['obj']])){params$objective <- NULL}
    # With a base margin, base_score must be neutral (probability 0.5 = margin 0) so that predictions without the margin are the trees' correction only:
    if(!is.null(grad) && is.null(params$base_score)){params$base_score <- 0.5}
    if(need_eval & is.null(config$feval) & is.null(params$eval_metric)){
      params$eval_metric <- 'auc'
    }
    if(!is.null(config$feval)){params$eval_metric <- NULL}
    params$nthread <- as.integer(config$nthread)

    objects$model <<- xgb_train(params = params, data = dtrain, nrounds = config$nrounds, evals = dvalidation,
      obj = config[['obj']], feval = config$feval, maximize = config$maximize, verbose = as.integer(config$show_progress & (config$verbose > 0)),
      print_every_n = config$print_every_n, early_stopping_rounds = config$early_stopping_rounds,
      save_period = config$save_period, save_name = config$save_name, xgb_model = config$xgb_model, callbacks = config$callbacks)

    imp = try(xgboost::xgb.importance(model = objects$model) %>% as.data.frame %>% select(fname = Feature, importance = Gain), silent = T)
    if(!inherits(imp, 'try-error')){
      if(!is.null(objects$features$importance)) objects$features$importance <<- NULL
      objects$features %>% dplyr::left_join(imp, by = 'fname') %>% na2zero ->> objects$features
    } else if(is.null(objects$features$importance)){
      objects$features$importance <<- 1.0/nrow(objects$features)
    }
  },

  # Returns probabilities of class 1 from the trees only (gradient transformer output is added later in transform_yout)
  model.predict = function(X){
    X = X[objects$features$fname]
    if(inherits(X, 'WIDETABLE')){X = wt_as_matrix(X)}
    stats::predict(objects$model, xgb_dmatrix(X), outputmargin = TRUE) %>% logit_inv %>% as.data.frame
  }
))
