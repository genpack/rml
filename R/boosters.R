# Given 'X_train' and 'X_valid' should contain features of the given 'model'
ft_boost = function(model, X_train, y_train, X_valid, y_valid){
  newcols = rbig::colnames(X_train)
  assert(newcols %<% rbig::colnames(X_valid), 'todo: write error message')
  if(model$fitted){
    warnif(model$objects$features %>% filter(importance > 0) %>% pull(fname) %<% newcols, 'todo')
  }
}



# hp_boost: Changes hyper-parameters one-by-one or randomly from a list of given hyper-parameters

# t_boost: transformer booster (Adds transformers one by one from a list of given transformers)
# gt_boost: Gradient Transformer boost (switches one-by-one among a given list of models)
