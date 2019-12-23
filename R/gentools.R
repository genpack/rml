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
  if(length(name) > 1){
    out = NULL
    for(nm in name){
      out = cbind(out, getFeatureValue.logical(flist, nm, dataset))
    }
    names(out) <- name
    return(out)
  }

  if(name %in% colnames(dataset)){return(dataset[, name])}

  if(name %in% rownames(flist)){
    if(flist[name, 'father'] %in% names(dataset)) {father = dataset[, flist[name, 'father']]} else {father = getFeatureValue.logical(flist, flist[name, 'father'], dataset)}
    if(flist[name, 'mother'] %in% names(dataset)) {mother = dataset[, flist[name, 'mother']]} else {mother = getFeatureValue.logical(flist, flist[name, 'mother'], dataset)}
    return(switch(flist[name, 'action'], 'AND' = {father & mother}, 'OR' = {father | mother}, 'XOR' = {xor(father, mother)}))
  } else {stop('Feature name is not in the list!')}
}


getFeatureValue.multiplicative = function(flist, name, dataset){
  if(length(name) > 1){
    out = NULL
    for(nm in name){
      out = cbind(out, getFeatureValue.multiplicative(flist, nm, dataset))
    }
    names(out) <- name
    return(out)
  }

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

evaluateFeatures.logical = function(flist, X, y, top = 100, cor_fun = cross_accuracy){
  columns  = colnames(X)
  ns       = rownames(flist)
  top      = min(top, length(ns) - 1)

  keep_and = which(is.na(flist$correlation) & (flist$father %in% columns) & (flist$mother %in% columns) & (flist$action == 'AND'))
  if(length(keep_and) > 0){
    flist$correlation[keep_and] <- cor_fun(X[, flist$father[keep_and]]*X[, flist$mother[keep_and]], y) %>%
      as.numeric %>% na2zero %>% abs
  }
  keep_or = which(is.na(flist$correlation) & (flist$father %in% columns) & (flist$mother %in% columns) & (flist$action == 'OR'))
  if(length(keep_or) > 0){
    flist$correlation[keep_or] <- (X[flist$father[keep_or]] | X[, flist$mother[keep_or]]) %>%
      cor_fun(y) %>% as.numeric %>% na2zero %>% abs
  }
  keep_xor = which(is.na(flist$correlation) & (flist$father %in% columns) & (flist$mother %in% columns) & (flist$action == 'XOR'))
  if(length(keep_xor) > 0){
    flist$correlation[keep_xor] <- xor(X[, flist$father[keep_xor]], X[, flist$mother[keep_xor]]) %>%
      cor_fun(y) %>% as.numeric %>% na2zero %>% abs
  }

  keep = is.na(flist$correlation) %>% which

  if(length(keep) > 0){
    flist$correlation[keep] <- getFeatureValue.logical(flist, ns[keep], X) %>% cor_fun(y) %>%
      as.numeric %>% na2zero %>% abs
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


#
#
# xgb = SCIKIT.XGB()
# dm  = DUMMIFIER()
# ob  = OPTBINNER()


GENETIC = setRefClass(
  'GENETIC',
  fields = list(featlist  = 'data.frame',
                modelbag  = 'list',
                models    = 'list',
                functions = 'list',
                config    = 'list'),

  methods = list(
    initialize = function(){
      config$cycle_births <<- 10
      featlist <<- data.frame(
        name   = character(),
        mother = character(),
        correlation = numeric(),
        safety = numeric(), stringsAsFactors = F)
      models <<- list(xgb, dm)
    },

    createFeatures = function(X,y){
      colnames = colnames()
      if(is.empty(featlist)){
        featlist <<- data.frame(
          name   = colnames,
          mother = NA,
          correlation = NA,
          safety = 0, stringsAsFactors = F) %>% column2Rownames('name')
      }
      features = rownames(featlist)

      for(i in sequence(config$cycle_births)){
        # pick a subset of rows
        N   = nrow(X)
        n1  = floor(N*0.3)
        trindex = N %>% sequence %>% sample(n1, replace = F)

        # pick a subset of features
        nf = sequence(length(features))
        features %>% sample()
        # pick a transformer from modelbag
        # pick a model class and create an abstract model with transformer
        i     = models %>% length %>% sequence %>% sample(1)
        model = models[[i]]
        model$fit(X, y)
        modelbag[[model$name]] <<- model
        # train the new built model
        # add the model to modelbag and model output to featlist
        featlist <<- rbind(featlist,
                           data.frame(
                              name   = model$name %>% paste('out', sep = '_'),
                              mother = model$name,
                              correlation = NA,
                              safety = 0, stringsAsFactors = F) %>% column2Rownames('name')
        )
      }
    }



  ))


########## GENERIC GENETIC ##############
transtypes = c(IDENTITY = 3, NORMALIZER = 2, DUMMIFIER = 1, GROUPER = 0.2, ENCODER.JAMESSTEIN = 1, OPTBINNER = 1, ENCODER.CATBOOST = 1, ENCODER.HELMERT = 1,
              SMBINNING = 1, LOGGER = 1, KMEANS = 1, PRCOMP = 1, CLS.SCIKIT.XGB = 1, CLS.SCIKIT.KNN = 0.2, CLS.SCIKIT.LR = 1, CLS.SCIKIT.SVM = 0.2)

createFeatures = function(flist, nf, prefix = 'FEAT', X, y){
  features = names(flist)
  lenflist = length(flist)
  for(i in sequence(nf)){
    fname   = prefix %>% paste(lenflist + i)
    parents = features %>% sample(5, replace = F)
    feature_parents = parent %^% colnames(X)
    trans_parents   = parents %-% feature_parents
    translist       = flist %>% list.extract(trans_parents)

    if(!is.empty(feature_parents)){
      idt = IDENTITY(features.include = feature_parents)
      translist[[idt$name]] <- idt
    }

    transname = pick(transtypes)
    model     = new(transname, transformers = translist)
    res       = try(model$fit(X, y), silent = T)
    if(!inherits(res, 'try-error')){
      flist[[fname]] <- list(
        name = fname,
        parents = parents,
        action  = transname,
        performance = NA,
        safety = 0
      )
    }
  }
}

getFeatureValue = function(flist, name, X){
  if(length(name) > 1){
    out = NULL
    for(nm in name){
      out = cbind(out, getFeatureValue.multiplicative(flist, nm, X))
    }
    names(out) <- name
    return(out)
  }

  if(name %in% colnames(X)){return(X[, name])}
  if(name %in% names(flist)){
    return(flist[[name]]$model$predict(X))
  } else {
    stop('feature name not in the list!')
  }
}


########### SUPERVISOR GENETIC ####################

createFeatures.supervisor = function(flist, nf, prefix = 'Feat'){
  features = rownames(flist)
  flist %>% rbind(
    data.frame(
      name = prefix %>% paste(nrow(flist) + sequence(nf)),
      father = features %>% sample(nf, replace = T),
      mother = features %>% sample(nf, replace = T),
      m_type = 'CLS.SCIKIT.XGB',
      correlation = NA,
      safety = 0, stringsAsFactors = F) %>% column2Rownames('name'))
}










