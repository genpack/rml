# Given 'X_train' and 'X_valid' should contain features of the given 'model'
boost_sfs = function(model, X_train, y_train, X_valid, y_valid){
  newcols = rbig::colnames(X_train)
  assert(newcols %<% rbig::colnames(X_valid), 'todo: write error message')
  if(model$fitted){
    warnif(model$objects$features %>% filter(importance > 0) %>% pull(fname) %<% newcols, 'todo')
  }
}