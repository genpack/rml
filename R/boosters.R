
# Given 'X_train' should contain features of the given 'model'
# If you want the models to be trained with all rows of the training dataset (X,y) in cross validation, then:
# - set cv.train_ratio = 1
# - set cv.ntrain = 1
# - specify a separate validation set in parameter: cv.set
#' @export
feature_booster = function(base_model, X, y, n_experiment = 10, subset_size = 10){
  # Verifications:
  columns = rbig::colnames(X)
  basefet = model_features(base_model)
  assert(basefet %<% columns, "todo")
  
  prf_base = base_model$performance.cv(X, y)
    
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
  
  prf <- bag %>% lapply(function(v) v$objects$performance.cv) %>% unlist
  
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



# hp_boost: Changes hyper-parameters one-by-one or randomly from a list of given hyper-parameters
hp_booster = function(base, parameter, values, ...){
  
  bag = list()
  for(i in sequenc(length(values))){
    model = base&deep_copy(i)
    model$config[[parameter]] <- values[i]
    model$name %<>% paste('_HPB', i)
    model$reset()
    bag[[model$name]] <- model
  }
  
  service_models(bag, ...)
}


# t_boost: transformer booster (Adds transformers one by one from a list of given transformers)
# gt_boost: Gradient Transformer boost (switches one-by-one among a given list of models)
