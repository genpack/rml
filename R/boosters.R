
#' @export
train_models = function(models, X, y){
  for(i in names(models)){
    cat('Training model: ', i, ' ...')
    res = try(models[[i]]$fit(X, y), silent = T)
    cat('DONE!', '\n')
    if(inherits(res, 'try-error')){
      cat('\n', 'Model ', i, ' training failed!', '\n', res %>% as.character, '\n')
    }
  }

  for(i in names(models)){
    if(!models[[i]]$fitted){
      models[[i]] <- NULL
    }
  }

  return(models)
}

# Runs cross validation for each model in the baglist:
#' @export
evaluate_models = function(models, X, y){
  for(i in names(models)){
    cat('Evaluating model: ', i, ' ...')
    res = try(models[[i]]$performance.cv(X, y), silent = T)
    cat('DONE!', '\n')
    if(inherits(res, 'try-error')){
      models[[i]]$objects$performance.cv <- sprintf("Model %s evaluation failed! %s", model$name, res %>% as.character)
    } else {
      models[[i]]$objects$performance.cv <- res
    }
  }
  
  return(models)
}


#' @export
evaluate_models_parallel = function(models, X, y, n_jobs = 8){
  library(doParallel)
  cl = makeCluster(n_jobs)
  registerDoParallel(cl)
  actual_njobs = getDoParWorkers()
  warnif(actual_njobs < n_jobs, 
         sprintf('Parallel run is working with %s cores rather than %s. It may take longer than expected!', actual_njobs, n_jobs))
  
  foreach(i = names(models), .combine = c, .packages = c('magrittr', 'dplyr','rutils', 'reticulate', 'rml', 'rbig', 'rfun'), .errorhandling = 'remove') %dopar% {
    model = models[[i]]
    res = try(model$performance.cv(X, y), silent = T)
    if(inherits(res, 'try-error')){
      model$objects$performance.cv = sprintf("Model %s evaluation failed! %s", model$name, res %>% as.character)
    } else {
      model$objects$performance.cv <- res
    }
    model
  }
}

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
    model = base_model$copy()
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

# t_boost: transformer booster (Adds transformers one by one from a list of given transformers)
# gt_boost: Gradient Transformer boost (switches one-by-one among a given list of models)
