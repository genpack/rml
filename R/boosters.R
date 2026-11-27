
# Given 'X_train' should contain features of the given 'model'
# If you want the models to be trained with all rows of the training dataset (X,y) in cross validation, then:
# - set cv.train_ratio = 1
# - set cv.ntrain = 1
# - specify a separate validation set in parameter: cv.set
#' @title Try adding features to a model
#' @description
#' Builds \code{n_experiment} copies of \code{base_model}, each with a random subset of extra columns, cross-validates them and returns the best one if it beats the base model.
#' @param base_model Model object.
#' @param X Features (all candidate columns).
#' @param y Target.
#' @param n_experiment Number of copies.
#' @param subset_size Extra columns per copy.
#' @return The best model, or \code{NULL} if none improved.
#' @export
feature_booster = function(base_model, X, y, n_experiment = 10, subset_size = 10){
  # Verifications:
  columns = wt_colnames(X)
  basefet = model_features(base_model)
  assert(basefet %<% columns, "All features of base_model must be columns of X")
  
  # performance.cv() returns one score per test set; models are compared on the mean score
  prf_base = mean(unlist(base_model$performance.cv(X, y)), na.rm = TRUE)
    
  remain  = columns %-% basefet

  # build model bag:
  bag = list()
  for(i in sequence(n_experiment)){
    model = base_model$deep_copy(i)
    model$reset()
    model$name <- base_model$name %>% paste('boost', i, sep = '_')
    # fetsubset: random subset of features to be added to the exisiting features:
    fetsubset = remain %>% sample(size = min(subset_size, length(remain)))
    newfetset = basefet %>% union(fetsubset)  
    
    nt = length(model$transformers)
    if(nt == 0){
      model$config$features.include = newfetset
    } else {
      model$transformers[[nt + 1]] <- MAP.RML.IDT(features.include = newfetset)
    }
    
    model$reset()
    
    bag[[model$name]] <- model 
  }
  
  ## Evaluate model bag and get cross-validation performances:
  bag <- evaluate_models(bag, X, y)
  
  prf <- bag %>% sapply(function(v) {p = v$objects$performance.cv; if(is.character(p)) NA_real_ else mean(unlist(p), na.rm = TRUE)})
  
  ord = order(prf, decreasing = T) %>% first
  prf_best = prf[ord]
  
  if(prf_best > prf_base){
    cat('\n', sprintf('Boosting Succeeded. Performance improved from %s to %s', prf_base, prf_best))
    boosted = bag[[ord]]
    
  } else {
    cat('\n', sprintf('Boosting Failed. Performance declined from %s to %s', prf_base, prf_best))
    boosted = NULL
  }
  ##
  
  out = list(boosted_model = boosted, models = bag)
  # todo: variable 'out' should be returned rather than 'boosted'
  return(boosted)
}



#' Try values of one hyper-parameter
#'
#' Makes one copy of \code{base} for each value of \code{parameter} and compares them with \code{\link{service_models}}.
#' @param base Model object.
#' @param parameter \code{character} name of a config argument of \code{base}.
#' @param values Vector of values to try.
#' @param ... Arguments of \code{\link{service_models}}: \code{X_train}, \code{y_train}, \code{X_test}, \code{y_test}, \code{metrics}, ...
#' @return Output of \code{\link{service_models}}: \code{list(best_model, best_performance, results)}.
#' @export
hp_booster = function(base, parameter, values, ...){
  
  bag = list()
  for(i in sequence(length(values))){
    model = base$deep_copy(i)
    model$config[[parameter]] <- values[[i]]
    model$name <- paste0(base$name, '_HPB', i)
    model$reset()
    bag[[model$name]] <- model
  }
  
  service_models(bag, ...)
}


# t_boost: transformer booster (Adds transformers one by one from a list of given transformers)
# gt_boost: Gradient Transformer boost (switches one-by-one among a given list of models)
