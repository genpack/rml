# Tools for genetic algorithm of feature building:

# nf features are born by random parents:
createFeatures.multiplicative = function(flist, nf, prefix = 'Feat'){
  features = rownames(flist)
  flist %>% rbind(
    data.frame(
      name = prefix %>% paste(nrow(flist) + sequence(nf)),
      father = features %>% sample(nf, replace = T),
      mother = features %>% sample(nf, replace = T),
      correlation = NA,
      safety = 0, stringsAsFactors = F) %>% column2Rownames('name'))
}


createFeatures.logical = function(flist, nf, prefix = 'Feat', actions = c('AND', 'OR', 'XOR')){
  features = rownames(flist)
  flist %>% rbind(
    data.frame(
      name    = prefix %>% paste(nrow(flist) + sequence(nf)),
      father  = features %>% sample(nf, replace = T),
      mother  = features %>% sample(nf, replace = T),
      action  = actions %>% sample(nf, replace = T),
      correlation = NA,
      safety = 0, stringsAsFactors = F) %>% column2Rownames('name'))
}
# immunes a subset of features to the highest safety level
immune = function(flist, features, level, columns){
  flist[features, 'safety'] = level
  have_parents = which(!(features %in% columns))
  if(length(have_parents) > 0){
    flist %<>% immune(flist[features[have_parents], 'father'], level, columns)
    flist %<>% immune(flist[features[have_parents], 'mother'], level, columns)
  }
  return(flist)
}


getFeatureValue.logical = function(flist, name, dataset){
  if(name %in% colnames(dataset)){return(dataset[, name])}
  
  if(name %in% rownames(flist)){
    if(flist[name, 'father'] %in% names(dataset)) {father = dataset[, flist[name, 'father']]} else {father = getFeatureValue.logical(flist, flist[name, 'father'], dataset)}
    if(flist[name, 'mother'] %in% names(dataset)) {mother = dataset[, flist[name, 'mother']]} else {mother = getFeatureValue.logical(flist, flist[name, 'mother'], dataset)}
    return(switch(flist[name, 'action'], 'AND' = {father & mother}, 'OR' = {father | mother}, 'XOR' = {xor(father, mother)}))
  } else {stop('Feature name is not in the list!')}
}


getFeatureValue.multiplicative = function(flist, name, dataset){
  if(name %in% colnames(dataset)){return(dataset[, name])}
  if(name %in% rownames(flist)){
    if(flist[name, 'father'] %in% names(dataset)) {father = dataset[, flist[name, 'father']]} else {father = getFeatureValue.multiplicative(flist, flist[name, 'father'], dataset)}
    if(flist[name, 'mother'] %in% names(dataset)) {mother = dataset[, flist[name, 'mother']]} else {mother = getFeatureValue.multiplicative(flist, flist[name, 'mother'], dataset)}
    return(father*mother)
  } else {stop('Feature name is not in the list!')}
}

evaluateFeatures.multiplicative = function(flist, X, y, top = 100, cor_fun = cor){
  ns   = rownames(flist)
  
  keep = is.na(flist$correlation) & (flist$father %in% columns) & (flist$mother %in% columns)
  if(sum(keep) > 0){
    flist$correlation[keep] <- cor_fun(X[, flist$father[keep]]*X[, flist$mother[keep]], y) %>% as.numeric %>% abs
  }
  keep = is.na(flist$correlation) %>% which
  
  for(i in keep){
    flist$correlation[i] <- cor_fun(getFeatureValue.multiplicative(flist, ns[i], X), y)
  }  
  
  high_level = max(flist$safety) + 1
  # ord = flist$correlation %>% order(decreasing = T) %>% intersect(which(!duplicated(flist$correlation)))
  ord = flist$correlation %>% order(decreasing = T)
  
  top  = min(top, length(ord) - 1)
  
  flist %<>% immune(ns[ord[sequence(top)]], level = high_level, columns = colnames(X)) 
  
  # keep = which(flist$safety == high_level | (is.na(flist$father) & is.na(flist$mother)))
  keep = which(flist$safety == high_level)
  return(flist[keep, ])
}

evaluateFeatures.logical = function(flist, X, y, top = 100, cor_fun = cross_enthropy){
  columns = colnames(X)
  ns      = rownames(flist)
  top     = min(top, length(ns) - 1)
  keep    = is.na(flist$correlation) & (flist$father %in% columns) & (flist$mother %in% columns)
  if(sum(keep) > 0){
    flist$correlation[keep] <- cor_fun(X[, flist$father[keep]]*X[, flist$mother[keep]], y) %>% as.numeric %>% abs
  }
  keep = is.na(flist$correlation) %>% which
  
  for(i in keep){
    flist$correlation[i] <- cor_fun(getFeatureValue.logical(flist, ns[i], X), y)
  }  
  
  high_level = max(flist$safety) + 1
  ord = flist$correlation %>% order(decreasing = T)
  flist %<>% immune(ns[ord[sequence(top)]], level = high_level, columns = colnames(X)) 
  
  keep = which(flist$safety == high_level)
  return(flist[keep, ])
}

# Optimal Genetic Binary Feature Combiner
genBinFeatBoost.fit = function(X, y, target = 0.9, epochs = 10, cycle_survivors = 500, cycle_births = 2000, final_survivors = 5, metric = cross_enthropy){
  columns = colnames(X)
  flist   = data.frame(name = columns, father = NA, mother = NA, action = NA, correlation = metric(X[, columns], y) %>% as.numeric %>% abs, safety = 0) %>% column2Rownames('name')
  flist   = flist[!is.na(flist$correlation),]
  # nf features are born by random parents:
  i = 0
  while((i < epochs) & (max(flist$correlation) < target)){
    i = i + 1
    flist = createFeatures.logical(flist, cycle_births)
    flist %<>% evaluateFeatures.logical(X, y, cor_fun = metric, top = chif(i == epochs, final_survivors, cycle_survivors))
    
    cat('Iteration: ', i, ': Best Correlation = ', max(flist$correlation), ' nrow(flist) = ', nrow(flist), '\n')
  }
  
  return(flist)
}
