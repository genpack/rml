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
      safety = 0, stringsAsFactors = F) %>% distinct(father, mother, .keep_all = T) %>%
      column2Rownames('name'))
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
      safety = 0, stringsAsFactors = F) %>% distinct(father, mother, action, .keep_all = T) %>%
      column2Rownames('name'))
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
genBinFeatBoost.fit = function(X, y, target = 0.9, epochs = 10, max_fail = 2, cycle_survivors = 500, cycle_births = 2000, final_survivors = 5, metric = cross_enthropy){
  columns = colnames(X)
  flist   = data.frame(name = columns, father = NA, mother = NA, action = NA, correlation = metric(X[, columns], y) %>% as.numeric %>% abs, safety = 0) %>% column2Rownames('name')
  flist   = flist[!is.na(flist$correlation),]
  # nf features are born by random parents:
  i = 0; j = 0; prev_best = -Inf
  while((i < epochs) & (max(flist$correlation) < target) & (j < max_fail)){
    i = i + 1
    flist = createFeatures.logical(flist, cycle_births)
    flist %<>% evaluateFeatures.logical(X, y, cor_fun = metric, top = chif(i == epochs, final_survivors, cycle_survivors))
    best = max(flist$correlation)
    cat('Iteration: ', i, ': Best Correlation = ', best, ' population = ', nrow(flist), '\n')
    if(best > prev_best){
      prev_best = best
    } else {
      j = j + 1
    }
  }

  return(flist)
}


#
#
# xgb = SKLEARN.XGB()
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

createFeatures = function(flist, nf, types, prefix = 'FEAT', X, y){
  features = names(flist)
  lenflist = length(flist)
  for(i in sequence(nf)){
    fname   = prefix %>% paste(lenflist + i, sep = '_')
    parents = features %>% sample(5, replace = F)
    feature_parents = parents %^% colnames(X)
    trans_parents   = parents %-% feature_parents
    translist       = flist %>% list.extract(trans_parents) %>% list.pull('model', do_unlist = F)

    if(!is.empty(feature_parents)){
      idt = MAP.RML.IDT(features.include = feature_parents)
      translist[[idt$name]] <- idt
    }

    transname = pick(transtypes)
    model     = new(transname, transformers = translist)
    res       = try(model$fit(X, y), silent = T)
    if(!inherits(res, 'try-error')){
      flist[[fname]] <- list(
        name    = fname,
        parents = parents,
        action  = transname,
        model   = model,
        correlation = NA,
        safety = 0
      )
    }
  }
  return(flist)
}

getFeatureValue = function(flist, fnames, X){
  if(length(fnames) > 1){
    out = NULL
    for(nm in fnames){
      out = cbind(out, getFeatureValue(flist, nm, X))
    }
    names(out) <- fnames
    return(out)
  }

  if(fnames %in% colnames(X)){return(X[, fnames])}
  if(fnames %in% names(flist)){
    res = try(flist[[fnames]]$model$predict(X), silent = T)
    if(inherits(res, 'try-error')) res = numeric(nrow(X)) %>% as.data.frame
    return(res)
  } else {
    stop('feature name not in the list!')
  }
}

getFeatureCorrelations = function(flist, X, y, metric = 'pearson_correlation'){
  ns   = names(flist)
  keep = c()
  for(item in flist){
    keep %<>% c(is.na(item$correlation))
  }
  for(i in which(keep)){
    flist[[i]]$correlation <- correlation(getFeatureValue(flist, ns[i], X), y, metric = metric) %>% max
  }
  return(flist)
}

reduceFeatures = function(flist, X, y, metric = 'pearson_correlation', top = 100){
  ns   = names(flist)
  flist %<>% getFeatureCorrelations(X, y, metric)

  high_level = max(flist %>% list.pull('safety')) + 1
  ord = flist %>% list.pull('correlation') %>% order(decreasing = T)

  top  = min(top, length(ord) - 1)

  flist %<>% immuneFeatures(ns[ord[sequence(top)]], level = high_level, columns = colnames(X))

  keep = which(flist %>% list.pull('safety') == high_level)
  return(flist %>% list.extract(ns[keep]))
}

immuneFeatures = function(flist, features, level, columns){
  for(ft in features){
    flist[[ft]]$safety = level
  }

  have_parents = which(!(features %in% columns))
  if(length(have_parents) > 0){
    flist %<>% immuneFeatures(flist %>% list.extract(features[have_parents]) %>% list.pull('parents') %>% unique, level, columns)
  }
  return(flist)
}


########### SUPERVISOR GENETIC ####################

createFeatures.supervisor = function(flist, nf, prefix = 'Feat'){
  features = rownames(flist)
  flist %>% rbind(
    data.frame(
      name = prefix %>% paste(nrow(flist) + sequence(nf)),
      father = features %>% sample(nf, replace = T),
      mother = features %>% sample(nf, replace = T),
      m_type = 'CLS.SKLEARN.XGB',
      correlation = NA,
      safety = 0, stringsAsFactors = F) %>% column2Rownames('name'))
}


########### GREEDY GENETIC ####################
default_greedy_templates = list(
  xgb1 = list(class = 'CLS.SKLEARN.XGB', weight = 0.20, n_num = c(30:60, 40:80), n_cat = c(0:10, 5:15),    n_jobs = as.integer(7), return_logit = c(T, T, F)),
  lr1  = list(class = 'CLS.SKLEARN.LR' , weight = 0.20, n_num = c(20:60)       , n_cat = c(0:10),           penalty = 'l1', return_logit = c(T, T, T, F), transformers = "MAP.RML.MMS()"),
  svm1 = list(class = 'CLS.SKLEARN.SVM', weight = 0.05, n_num = c(5:20)        , n_cat = c(0:10)))

create_transformer = function(X, y, types = default_greedy_templates, name = NULL){
  colnames(X) %>% sapply(function(i) X[,i] %>% class) -> features
  num_features = names(features)[features == 'numeric']
  cat_features = names(features)[features == 'integer']

  types %>% list.pull('weight') %>% {names(.) <- types %>% length %>% sequence;.} -> weights

  sn  = pick(weights) %>% as.integer
  types[[sn]]$n_num = ifelse(types[[sn]]$n_num > length(num_features), length(num_features), types[[sn]]$n_num)
  types[[sn]]$n_cat = ifelse(types[[sn]]$n_cat > length(cat_features), length(cat_features), types[[sn]]$n_cat)
  nnf = types[[sn]]$n_num %>% sample(1)
  ncf = types[[sn]]$n_cat %>% sample(1)

  tr  = try(types[[sn]]$class %>% new(features.include = c(num_features %>% sample(nnf), cat_features %>% sample(ncf))), silent = T)
  if(inherits(tr, 'try-error')) {
    cat( '\n', as.character(tr), '\n')
    return(NULL)
  }
  if(!is.null(name)) tr$name = name
  configs = names(types[[sn]]) %-% c('class', 'weight', 'n_num', 'n_cat', 'transformers')
  for(cfg in configs){
    if(length(types[[sn]][[cfg]]) == 1){
      tr$config[[cfg]] <- types[[sn]][[cfg]]
    } else {
      tr$config[[cfg]] <- types[[sn]][[cfg]] %>% sample(1)
    }
  }
  if(!is.null(types[[sn]][['transformers']])){
    for(scr in types[[sn]][['transformers']]){
      sub_tr = try(parse(text = scr) %>% eval, silent = T)
      if(inherits(sub_tr, 'try-error')){
        cat( '\n Sub_transformer failed: ', as.character(sub_tr), '\n')
      } else {
        tr$transformers[[sub_tr$name]] <- sub_tr
      }
    }
  }

  res = try(tr$fit(X, y), silent = T)
  if(inherits(res, 'try-error')) {
    cat( '\n', as.character(res), '\n')
    return(NULL)
  }

  return(tr)
}

addTransformer = function(model, transformer, X_train, y_train, X_val, y_val, benchmark = NULL){
  if(is.null(benchmark)){
    model$fit(X_train, y_train)
    benchmark = model$performance(X_val, y_val, 'gini')
  }
  if(length(model$transformers) == 0){
    idt = MAP.RML.IDT(name = 'I', features.include = model$config$features.include, features.exclude = model$config$features.exclude)
    model$transformers[['I']]   <- idt
  }
  model$transformers[[transformer$name]] <- transformer
  model$reset(reset_transformers = F)
  res = try(model$fit(X_train, y_train), silent = T)
  if(!inherits(res, 'try-error')){
    new_perf = model$performance(X_val, y_val, metric = 'gini')
    if(new_perf <= benchmark){
      cat('\n', 'Transformer ', transformer$name, ' Failed!', '\n')
      model$transformers[[transformer$name]] <- NULL
      model$reset(reset_transformers = F)
    } else {
      cat('\n', 'Transformer ', transformer$name, ' successfully improved performance to: ', new_perf, '\n')
      return(new_perf)
    }
  } else {
    cat('\n', res %>% as.character, '\n')
    model$transformers[[transformer$name]] <- NULL
    model$reset(reset_transformers = F)
  }
}

join_features = function(father, mother, X_train, y_train, X_val, y_val, benchmark = NULL){
  if(is.null(benchmark)){
    model$fit(X_train, y_train)
    benchmark = model$performance(X_val, y_val, 'gini')
  }
  if(length(model$transformers) == 0){
    idt = MAP.RML.IDT(name = 'I', features.include = model$config$features.include, features.exclude = model$config$features.exclude)
    model$transformers[['I']]   <- idt
  }
  model$transformers[[transformer$name]] <- transformer
  model$reset(reset_transformers = F)
  res = try(model$fit(X_train, y_train), silent = T)
  if(!inherits(res, 'try-error')){
    new_perf = model$performance(X_val, y_val, metric = 'gini')
    if(new_perf <= benchmark){
      cat('\n', 'Transformer ', transformer$name, ' Failed!', '\n')
      model$transformers[[transformer$name]] <- NULL
      model$reset(reset_transformers = F)
    } else {
      cat('\n', 'Transformer ', transformer$name, ' successfully improved performance to: ', new_perf, '\n')
      return(new_perf)
    }
  } else {
    cat('\n', res %>% as.character, '\n')
    model$transformers[[transformer$name]] <- NULL
    model$reset(reset_transformers = F)
  }
}


########### EXPERT GENETIC ####################
classifiers   =  c("CLS.SKLEARN.XGB", "CLS.SKLEARN.LR", "CLS.SKLEARN.SVM", "CLS.SKLEARN.KNN", "CLS.SKLEARN.DT")
encoders      =  c("ENC.CATEGORY_ENCODERS.HLMRT", "ENCODER.CATBOOST", "ENCODER.JAMESSTEIN", "ENCODER.TARGET", "ENCODER.MODEL")
binners       =  c('OPTBINNER', 'SMBINNING')

models_pass   = c("DUMMIFIER", "MAP.RML.MMS", "numeric", classifiers, encoders)
encoders_pass = c('integer', 'GROUPER', 'BIN.KMEANS.KMC', 'SKMEANS')
binners_pass  = c('integer', 'numeric', classifiers, encoders)
free_numerics = c('integer', 'numeric')
bound_numerics = c("MAP.RML.MMS", "SCALER", classifiers, encoders, 'numeric')

default_expert_templates = list(
  cls.xgb.01 = list(class = 'CLS.SKLEARN.XGB', weight = 0.1, n_jobs = as.integer(7), return_logit = c(T, T, F), max_depth = 3:15, min_child_weight = 1:5, n_estimators = 50*(1:6), feature_transformer = 'MAP.RML.IDT'),
  cls.xgb.02 = list(class = 'CLS.XGBOOST', weight = 0.1, n_jobs = as.integer(7), return_logit = c(T, T, F),
                    colsample_bytree = as.integer(1:10),
                    gamma = list(fun = runif, min = 1, max = 10),
                    eta = list(fun = runif, min = 0.05, max = 0.5),
                    nrounds = 5:100,
                    max_depth = 2:20,
                    min_child_weight = 2:10,
                    scale_pos_weight = 2:10,
                    subsample = list(fun = runif, min = 0, max = 1)),
  cls.lr.01  = list(class = 'CLS.SKLEARN.LR' , weight = 0.05, penalty = c(rep('l1',5), 'l2'), return_logit = c(T, T, T, F), pass = models_pass, feature_transformer = 'MAP.RML.MMS'),
  cls.svm.01 = list(class = 'CLS.SKLEARN.SVM', weight = 0.05, pass = models_pass, max_train = 5000:10000, return_logit = c(T, T, F), feature_transformer = 'SCALER'),
  cls.knn.01 = list(class = 'CLS.SKLEARN.KNN', weight = 0.05, pass = models_pass, max_train = 5000:10000, return_logit = c(T, T, F), feature_transformer = 'MAP.RML.MMS'),
  cls.dt.01  = list(class = 'CLS.SKLEARN.DT', weight = 0.05, pass = models_pass, return_logit = c(T, T, F), feature_transformer = 'MAP.RML.IDT'),
  cls.flasso.01 = list(class = 'CLS.FLASSO', weight = 0.05, pass = models_pass, lambda1 = 0.1*(0:50), lambda2 = 0.1*(0:50), return_logit = c(T, T, F), feature_transformer = 'MAP.RML.MMS'),
  cls.gbt.01 = list(class = 'CLS.SPARKLYR.GBT', weight = 0.05, pass = models_pass, return_logit = c(T, T, F),
       max_iter  = 20:50, max_depth = 2:20, subsampling_rate = 0.1*(1:10),
       max_bins  = c(16, 32, 64, 128), min_info_gain = 0,
       step_size = c(0.001*(1:10), 0.01*(1:10), 0.1*(1:10)), feature_transformer = 'MAP.RML.IDT'),
  cls.dnn.01 = list(class = 'CLS.KERAS.DNN', weight = 0.05, pass = models_pass, return_logit = c(T, T, F),
       num_layers = 1:5,
       first_layer_nodes = 1:1024,
       layer_nodes_ratio = 0.1*(1:20),
       layers_activation = c('relu', 'linear'),
       layers_dropout = 0.01*(1:75),
       initializer_seed = 1:100000,
       kernel_regularization_penalty_l1 = c(rep(0, 20), 0.001*(1:1000)),
       kernel_regularization_penalty_l2 = c(rep(0, 20), 0.001*(1:1000)),
       learning_rate = 0.0001*(1:1000),
       optimizer = c('adadelta', 'adagrad', 'adam', 'adamax', 'nadam', 'rmsprop', 'sgd'),
       feature_transformer = 'MAP.RML.MMS'),
  # list(class = 'SCALER', weight = 0.01, pass = 'numeric'),
  # list(class = 'MAP.RML.MMS', weight = 0.05, pass = 'numeric'),
  bin.obb.01   = list(class = 'BIN.RML.OBB', weight = 0.02, pass = binners_pass, feature_transformer = 'MAP.RML.MMS'),
  enc.jstn.01  = list(class = 'ENC.CATEGORY_ENCODERS.JSTN', weight = 0.01, pass = encoders_pass, feature_transformer = 'MAP.RML.IDT'),
  enc.catb.01  = list(class = 'ENC.CATEGORY_ENCODERS.CATB', weight = 0.01, pass = encoders_pass, feature_transformer = 'MAP.RML.IDT'),
  enc.hlmrt.01 = list(class = 'ENC.CATEGORY_ENCODERS.HLMRT', weight = 0.01, pass = encoders_pass, feature_transformer = 'MAP.RML.IDT'),
  enc.te.01    = list(class = 'ENC.RML.TE', weight = 0.01, pass = encoders_pass, feature_transformer = 'MAP.RML.IDT'),
  fnt.inv.01   = list(class = 'FNT.RML.INV', weight = 0.01, pass = 'numeric', trim = 100, feature_transformer = 'MAP.RML.MMS'),
  fet.d2mul.01 = list(class = 'FET.RML.D2MUL', weight = 0.01, pass = setdiff(models_pass, 'DUMMIFIER'), feature_transformer = 'MAP.RML.MMS'),
  fnt.log.01   = list(class = 'FNT.RML.LOG', weight = 0.01, pass = c('MAP.RML.MMS', 'numeric', classifiers), intercept = 0.1*(0:100), feature_transformer = 'MAP.RML.MMS'),
  enc.ohe.01   = list(class = 'ENC.FASTDUMMIES.OHE', weight = 0.01, pass = encoders_pass, feature_transformer = 'MAP.RML.IDT'),
  list(class = 'FET.RML.MGB', weight = 0.01, pass = free_numerics, n_survivors = 2, max_fail = 2:3, feature_transformer = 'MAP.RML.MMS'),
  list(class = 'GENETIC.BOOSTER.LOGICAL', weight = 0.01, pass = c('OPTBINNER', 'DUMMIFIER'), feature_transformer = 'MAP.RML.IDT'),
  list(class = 'BIN.KMEANS.KMC', weight = 0.01, pass = c(free_numerics, bound_numerics), feature_transformer = 'MAP.RML.MMS'),
  list(class = 'MAP.PYLMNN.LMNN', weight = 0.01, pass = c(free_numerics, bound_numerics), max_train = 5000:10000, feature_transformer = 'MAP.RML.MMS'),
  list(class = 'MAP.STATS.PCA', weight = 0.01, pass = free_numerics, num_components = 5:30, feature_transformer = 'MAP.RML.MMS'))

# names(default_expert_templates) <- default_expert_templates %>% list.pull('class') %>% unname

read_exlist = function(path){
  exl = list()
  for(fn in list.files(path)){
    exl[[fn]] <- model_load(fn, path)
  }
  return(exl)
}

create_experts_from_dataset = function(dataset){
  cls = colnames(dataset) %>% sapply(function(i) dataset[,i] %>% class) %>% unname
  list(exlog = data.frame(exname = colnames(dataset), father = as.character(NA), mother = as.character(NA), action = as.character(NA), class = cls, correlation = as.numeric(NA), safety = 0, stringsAsFactors = F) %>%
    column2Rownames('exname'), exlist = list())
}

develop_exlog_from_exlist = function(exlog, exlist){
  exnames = exlist %>% list.pull('name') %>% unname
  exclass = exlist %>% lapply(function(x) class(x)) %>% unlist %>% unname

  data.frame(exname = exnames, father = as.character(NA), mother = as.character(NA), action = as.character(NA), class = exclass, correlation = as.numeric(NA), safety = 0, stringsAsFactors = F) %>%
    column2Rownames('exname') %>% rbind(exlog)
}



grow_exlog    = function(exlog, ne, prefix = 'EX', template_set = default_expert_templates, action_set = c('<<==>>', '<<==', '==>>')){
  originals = rownames(exlog)[exlog$class %in% c('numeric', 'integer')]
  features  = rownames(exlog)
  exnames   = features %>% charFilter(prefix %++% '_', match_case = T)
  if(is.empty(exnames)){start_nn = 0} else {
    start_nn  =  exnames %>% stringr::str_remove(prefix %++% '_') %>% as.integer %>% max
  }
  exlog %>% rbind(
    data.frame(
      name   = prefix %>% paste(start_nn + sequence(ne), sep = '_'),
      father = features %>% sample(ne, replace = T),
      mother = features %>% sample(ne, replace = T),
      action = action_set %>% sample(ne, replace = T),
      class  = template_set %>% list.pull('class') %>% sample(ne, replace = T),
      correlation = NA,
      safety = 0, stringsAsFactors = F) %>%
      column2Rownames('name'))
      # {.$action[.$father %in% originals] <- '<<==>>';.}) %>%
      # {.$class[which(.$action == '<<==')] <- .[.$father[which(.$action == '<<==')], 'class'];.}
}

# Example (for test):
# testlog = data.frame(exname = LETTERS, father = NA, mother = NA, action = NA, class = c('numeric', 'integer') %>% sample(size = 26, replace = T), correlation = NA, safety = 0, stringsAsFactors = F) %>%
#   column2Rownames('exname')
# testlog %>% grow_exlog(200) %>% View
# exlog %>%
consistent_exlog = function(exlog){
  exnames = rownames(exlog)
  tbd     = which(!exlog$father %in% c(exnames, NA)) %U% which(!exlog$mother %in% c(exnames, NA))
  while(length(tbd) > 0){
    exlog   = exlog[- tbd, ]
    exnames = rownames(exlog)
    tbd     = which(!exlog$father %in% c(exnames, NA)) %U% which(!exlog$mother %in% c(exnames, NA))
  }
  return(exlog)
}

# Removes infeasible experts
correct_exlog = function(exlog, template_set = default_expert_templates){
  # expert_classes = exlog %>% rownames2Column('exname') %>% distince()
  exlog_copy <- exlog %>% rownames2Column('exname')
  originals  <- exlog_copy$exname[is.na(exlog_copy$father) | is.na(exlog_copy$mother)]

  exlog_copy %<>%
    left_join(exlog_copy %>% select(exname, class) %>% rename(father = exname, father_class = class), by = 'father') %>%
    left_join(exlog_copy %>% select(exname, class) %>% rename(mother = exname, mother_class = class), by = 'mother') %>%
    mutate(class = as.character(class), mother_class = as.character(mother_class), father_class = as.character(father_class))

  exlog_copy %<>%
    filter(!is.na(father) & !is.na(mother)) %>%
    filter(!father_class %in% c('numeric', 'integer') | action != '<<==') %>%
    filter(!mother_class %in% c('numeric', 'integer') | action != '==>>')

  keep = rep(TRUE, nrow(exlog_copy))
  for(i in sequence(nrow(exlog_copy))){
    if(!is.null(template_set[[exlog_copy$class[i]]]$pass)){
      keep[i] = (exlog_copy$father_class[i] %in% template_set[[exlog_copy$class[i]]]$pass) & (exlog_copy$mother_class[i] %in% template_set[[exlog_copy$class[i]]]$pass)
    }
  }

  exlog_copy = exlog_copy[keep, ]

  exnames = originals %U% exlog_copy$exname
  exlog   = exlog[exnames, ] %>% consistent_exlog

  exlog[which(exlog$action == '<<=='), 'class'] <- exlog[exlog[which(exlog$action == '<<=='), 'father'], 'class']
  exlog[which(exlog$action == '==>>'), 'class'] <- exlog[exlog[which(exlog$action == '==>>'), 'mother'], 'class']

  return(exlog)
}

grow_experts = function(experts, n_births = 100, n_target = 20, prefix = 'EX', template_set = default_expert_templates, action_set = c('<<==>>', '<<==', '==>>')){
  nexp = nrow(experts$exlog)
  while (nrow(experts$exlog) < n_target + nexp){
    experts$exlog %<>% grow_exlog(n_births, prefix, template_set, action_set) %>%
      correct_exlog(template_set = template_set)
  }
  experts$exlog = experts$exlog[sequence(nexp + n_target),] %>% correct_exlog(template_set = template_set)
  experts %>% build_experts(template_set = template_set)
}

build_expert_from_template = function(exname = NULL, template){
  stopifnot(inherits(template, 'list'))
  tr  = try(template$class %>% new, silent = T)
  if(inherits(tr, 'try-error')){
    cat('\n', 'Building expert ', tr, ' failed!', '\n',  as.character(tr), '\n')
    return(NULL)
  }
  if(!is.null(exname)) tr$name = exname
  configs = names(template) %-% c('class', 'weight', 'n_num', 'n_cat', 'transformers','feature_transformer', 'pass')
  for(cfg in configs){
    if (inherits(template[[cfg]], 'list')){
      stopifnot(inherits(template[[cfg]]$fun, 'function'))
      tr$config[[cfg]] <- do.call(template[[cfg]]$fun, template[[cfg]] %>% list.add(n = 1) %>% list.remove('fun'))
    } else {
      if(length(template[[cfg]]) == 1){
        tr$config[[cfg]] <- template[[cfg]]
      } else {
        tr$config[[cfg]] <- template[[cfg]] %>% sample(1)
      }
    }
  }
  return(tr)
}

build_experts = function(experts, template_set){

  originals = rownames(experts$exlog)[experts$exlog$class %in% c('numeric', 'integer')]
  exnames   = rownames(experts$exlog) %-% names(experts$exlist) %-% originals
  for(i in exnames){
    cat('Building expert: ', i, ' ... ')

    tr = build_expert_from_template(exname = i, template = template_set[[experts$exlog[i, 'class']]])

    root_father = experts$exlog[i,'father'] %in% originals
    root_mother = experts$exlog[i,'mother'] %in% originals

    if(root_father & root_mother){
      if(experts$exlog[i, 'action'] == '<<==>>'){
        expert_transformer = list(new(template_set[[experts$exlog[i, 'class']]]$feature_transformer, features.include = c(experts$exlog[i,'father'], experts$exlog[i,'mother']) %>% unique))}
      else {stop('Impossible! Check expert generator engine.')}}
    else if(root_father){
      if(experts$exlog[i, 'action'] == '<<==>>'){
        expert_transformer = list(new(template_set[[experts$exlog[i, 'class']]]$feature_transformer, features.include = c(experts$exlog[i,'father'])), experts$exlist[[experts$exlog[i,'mother']]])}
      else if (experts$exlog[i, 'action'] == '==>>'){
        expert_transformer = experts$exlist[[experts$exlog[i,'mother']]]$transformers
        j = 0; permit = F
        while(j < length(expert_transformer) & !permit){
          j = j + 1
          if(expert_transformer[[j]]$type == 'Identity Transformer'){
            expert_transformer[[j]] = expert_transformer[[j]]$copy()
            expert_transformer[[j]]$config$features.include %<>% c(experts$exlog[i,'father'])
            expert_transformer[[j]]$reset()
            permit = T
          }
        }
        if(!permit){
          expert_transformer[[length(expert_transformer) + 1]] <- new(template_set[[experts$exlog[i, 'class']]]$feature_transformer, features.include = c(experts$exlog[i,'father']))
        }}
      else {stop('Impossible! Check expert generator engine.')}}
    else if(root_mother){
      if(experts$exlog[i, 'action'] == '<<==>>'){
        expert_transformer = list(new(template_set[[experts$exlog[i, 'class']]]$feature_transformer, features.include = c(experts$exlog[i,'mother'])), experts$exlist[[experts$exlog[i,'father']]])}
      else if (experts$exlog[i, 'action'] == '<<=='){
        expert_transformer = experts$exlist[[experts$exlog[i,'father']]]$transformers
        j = 0; permit = F
        while(j < length(expert_transformer) & !permit){
          j = j + 1
          if(expert_transformer[[j]]$type == 'Identity Transformer'){
            expert_transformer[[j]] = expert_transformer[[j]]$copy()
            expert_transformer[[j]]$config$features.include %<>% c(experts$exlog[i,'mother'])
            expert_transformer[[j]]$reset()
            permit = T
          }
        }
        if(!permit){
          expert_transformer[[length(expert_transformer) + 1]] <- new(template_set[[experts$exlog[i, 'class']]]$feature_transformer, features.include = c(experts$exlog[i,'mother']))
        }}
      else {stop('Impossible! Check expert generator engine.')}}
    else{
      if(experts$exlog[i, 'action'] == '<<==>>'){
        expert_transformer = list(experts$exlist[[experts$exlog[i,'father']]], experts$exlist[[experts$exlog[i,'mother']]])
      } else if (experts$exlog[i, 'action'] == '==>>'){
        expert_transformer = experts$exlist[[experts$exlog[i,'mother']]]$transformers
        expert_transformer[[length(expert_transformer) + 1]] <- experts$exlist[[experts$exlog[i,'father']]]
      } else if (experts$exlog[i, 'action'] == '<<=='){
        expert_transformer = experts$exlist[[experts$exlog[i,'father']]]$transformers
        expert_transformer[[length(expert_transformer) + 1]] <- experts$exlist[[experts$exlog[i,'mother']]]
      } else {stop('Impossible! Check expert generator engine.')}
    }

    tr$transformers     <- expert_transformer
    experts$exlist[[i]] <- tr
    cat('Done!', '\n')
  }
  return(experts)
}

train_experts = function(experts, X, y){
  for(i in names(experts$exlist)){
    cat('Training expert: ', i, ' ...')
    res = try(experts$exlist[[i]]$fit(X, y), silent = T)
    cat('DONE!', '\n')
    if(inherits(res, 'try-error')){
      cat('\n', 'Expert ', i, ' training failed!', '\n', res %>% as.character, '\n')
    }
  }
  ftbd = c()
  for(i in names(experts$exlist)){
    if(!experts$exlist[[i]]$fitted){
      experts$exlist[[i]] <- NULL
      ftbd = c(ftbd, i)
    }
  }
  tbd = rownames(experts$exlog) %in% ftbd
  experts$exlog = experts$exlog[!tbd,]

  return(experts)
}

get_expert_value = function(exlist, exnames, dataset){
  if(length(exnames) > 1){
    out = NULL
    nms = c()
    for(nm in exnames){
      res = try(get_expert_value(exlist, nm, dataset), silent = T)
      if(inherits(res, 'try-error')) {
        cat('\n', exnames, ' prediction failed!', '\n', res %>% as.character)
      } else {
        out = cbind(out, res)
        nms = c(nms, nm)
      }
    }
    names(out) <- nms
    return(out)
  }

  if(exnames %in% colnames(dataset)){return(dataset[, exnames])}
  if(exnames %in% names(exlist)){
    return(exlist[[exnames]]$predict(dataset))
  } else {
    stop('feature name not in the list!')
  }
}

get_expert_correlations = function(experts, X, y, metric = 'gini'){
  exnames   = rownames(experts$exlog)
  originals = rownames(experts$exlog)[experts$exlog$class %in% c('numeric', 'integer')]
  assert((exnames %-% originals) %==% names(experts$exlist))
  tbc  = is.na(experts$exlog$correlation)
  for(i in which(tbc)){
    val = try(get_expert_value(experts$exlist, exnames[i], X), silent = T)
    if(!inherits(val, 'try-error')){
      experts$exlog[i, 'correlation'] <- correlation(val, y, metric = metric) %>% max
    }
  }
  return(experts)
}

reduce_experts = function(experts, X, y, metric = 'gini', top = 10){
  exnames   = rownames(experts$exlog)
  originals = rownames(experts$exlog)[experts$exlog$class %in% c('numeric', 'integer')]
  assert((exnames %-% originals) %==% names(experts$exlist))

  experts %<>% get_expert_correlations(X, y, metric)
  experts$exlog = experts$exlog[!is.na(experts$exlog$correlation),]
  experts$exlist %<>% list.extract(rownames(experts$exlog))

  experts$exlog$father_score = experts$exlog[experts$exlog$father, 'correlation']
  experts$exlog$mother_score = experts$exlog[experts$exlog$mother, 'correlation']
  experts$exlog$parent_score = ifelse(experts$exlog$father_score > experts$exlog$mother_score, experts$exlog$father_score, experts$exlog$mother_score)
  experts$exlog$parent_score[is.na(experts$exlog$parent_score)] <- -Inf
  experts$exlog = experts$exlog[experts$exlog$correlation > experts$exlog$parent_score, colnames(experts$exlog) %-% c('father_score', 'mother_score', 'parent_score')] %>%
    consistent_exlog

  exnames    = rownames(experts$exlog)
  high_level = max(experts$exlog %>% pull('safety')) + 1
  ord = experts$exlog %>% pull('correlation') %>% order(decreasing = T)

  top  = min(top, length(ord) - 1)

  experts %<>% immune_experts(exnames[ord[sequence(top)]], level = high_level)
  experts$exlog = experts$exlog[experts$exlog$safety == high_level,] %>% consistent_exlog
  experts$exlist %<>% list.extract(rownames(experts$exlog))
  return(experts)
}

immune_experts = function(experts, exnames, level){
  originals = rownames(experts$exlog)[which(experts$exlog$class %in% c('numeric', 'integer'))]
  experts$exlog[exnames, 'safety'] <- level

  have_parents = which(!(exnames %in% originals))
  if(length(have_parents) > 0){
    experts %<>% immune_experts(experts$exlog[exnames[have_parents], 'father'] %U% experts$exlog[exnames[have_parents], 'mother'] %-% NA, level)
  }
  return(experts)
}

save_experts = function(experts, path = getwd()){
  experts$exlog %>% write.csv(path %>% paste('exlog.csv', sep = '/'), row.names = F)
  for(fn in names(experts$exlist) %-% list.files(path)){
    save_model(experts$exlist[[fn]], path)
  }
}

load_experts = function(experts, path = getwd()){
  experts = list(exlog = data.frame(), exlist = list())
  experts$exlog <- read.csv(path %>% paste('exlog.csv', sep = '/'), as.is = T)
  for(fn in list.files(path) %^% rownames(experts$exlog)){
    experts$exlist[[fn]] <- model_load(fn, path)
  }
  for(fn in list.files(path) %-% names(exlist)){
    unlink(path %>% paste(fn, spe = '/'), recursive=TRUE)
  }
  return(experts)
}

########### FUNCTIONAL GENETIC ####################
# default_function_set = c(mul2, lincomb2, hyperbola2, binbin2, hyperpoly_d1, binbin)
# for(i in 2:5){
#   default_function_set = c(default_function_set, build_lincomb(i))
#   default_function_set = c(default_function_set, build_binbin(i))
#   default_function_set = c(default_function_set, build_poly(i, 2))
#   default_function_set = c(default_function_set, build_poly(i, 3))
# }
pick_from_list = function(lst){
  lst[[lst %>% length %>% sequence %>% sample(size = 1)]]
}

grow_funlist = function(funlist = NULL, features = NULL, n_births = 100, function_set = default_function_set, prefix = 'FN'){
  if(is.empty(funlist)){
    assert(!is.empty(features), 'Both funlist and features are empty!')
    funlist = features %>% as.list; names(funlist) <- features}
  funcnames = names(funlist) %>% charFilter(prefix)
  if(is.empty(funcnames)){start_nn = 0} else {
    start_nn  = funcnames %>% stringr::str_remove(prefix %++% '_') %>% as.integer %>% max
  }
  for(i in sequence(n_births)){
    model = pick_from_list(function_set)$deep_copy()
    model$name <- prefix %>% paste(i + start_nn, sep = '_')
    for(j in names(model$inputs)){
      inp_object = funlist[[names(funlist) %>% sample(1)]]
      if(!inherits(inp_object, 'FUNCTION')){
        model$inputs[[j]] <- inp_object
      } else {
        model$inputs[[j]] <- inp_object$deep_copy()
        model$inputs[[j]]$name = inp_object$name %>% paste0(c(letters, LETTERS) %>% sample(1))
      }
    }
    funlist[[model$name]] <- model
  }
  return(funlist)
}

reset_funlist = function(funlist){
  for(fn in funlist){
    if(inherits(fn, 'FUNCTION')) {fn$reset()}
  }
}

evaluate_funlist = function(funlist, X, y, metric = logloss_sum){
  loss_value = numeric()
  ofun       = metric$copy()
  ofun$inputs$y <- y

  funset = names(funlist)
  if(is.null(funset)){funset = sequence(length(funlist))}
  for(fn in funset){
    ofun$inputs$x  <- funlist[[fn]]
    ofun$reset()
    loss_value[fn] <- ofun$get.output.agg(data = X)
  }
  return(loss_value)
}

# computes the output of each function and removes functions when
# any value in any of their inputs are not in the domain: returns NA, Inf or -Inf
# clean_funlist = function(funlist){
#   for(fn in funlist){
#     if(inherits(fn, 'FUNCTION')){
#
#     }
#   }
# }


boost_funlist_parallel = function(funlist, X, y, metric = logloss_sum, n_jobs = 8, ...){


  library(doParallel)
  cl = makeCluster(n_jobs)
  registerDoParallel(cl)
  warnif(getDoParWorkers() < n_jobs, 'Parallel run is not working. It may take too long!')

  boostlist = funlist %>% lapply(function(ff) {out = inherits(ff, 'FUNCTION'); if(out) out = length(ff$list.parameters()) > 0; out}) %>% unlist %>% which %>% names
  bflist    = foreach(fn = boostlist, .combine = c, .packages = c('magrittr', 'dplyr'), .errorhandling = 'stop') %dopar% {
    source('~/Documents/software/R/packages/rfun/R/funclass.R')
    source('~/Documents/software/R/packages/rfun/R/funlib.R')
    source('~/Documents/software/R/packages/rfun/R/funlib2.R')
    source('~/Documents/software/R/packages/rfun/R/builders.R')
    source('~/Documents/software/R/packages/rfun/R/solvers.R')
    source('~/Documents/software/R/packages/rutils/R/rutils.R')
    source('~/Documents/software/R/packages/rml/R/mltools.R')

    ofun          <- metric$copy()
    ofun$inputs$y <- y
    ofun$inputs$x <- funlist[[fn]]
    ofun$reset()

    prm = funlist[[fn]]$list.parameters()
    keep = ofun$get.param(prm); fval = ofun$get.output.agg(data = X)

    if(ofun$inputs$x$type %>% substr(1,13) == 'binary binner'){
      sel = prm %>% sample(min(2, length(prm)))
      res = try(minimize.ccd(ofun, parameters = sel, data = X, silent = silent), silent = T)
      ofun$reset();if(ofun$get.output.agg(data = X) > fval) ofun$set.param(keep) else {keep = ofun$get.param(prm); fval = ofun$get.output.agg(data = X)}

      sel = prm %>% sample(min(2, length(prm)))
      res = try(minimize.walk(ofun, parameters = sel, data = X, silent = silent), silent = T)
      ofun$reset();if(ofun$get.output.agg(data = X) > fval) ofun$set.param(keep) else {keep = ofun$get.param(prm); fval = ofun$get.output.agg(data = X)}
    }

    sel     = prm %>% sample(min(16, length(prm)))
    stepdir = ofun$get.gradients.agg(wrt = sel, data = X) %>% unlist %>% vect.normalize %>% {-1.0*.}
    success = try(step_forward(ofun, parameters = sel, direction = stepdir, data = X, silent = F, ...), silent = T)

    ofun$reset();if(ofun$get.output.agg(data = X) > fval) ofun$set.param(keep) else {keep = ofun$get.param(prm); fval = ofun$get.output.agg(data = X)}
    ofun$reset()
    gc()
    ofun$inputs$x
  }
  stopCluster(cl)
  gc()
  names(bflist) = boostlist
  for(fn in boostlist){
    funlist[[fn]] <- bflist[[fn]]
  }
  return(funlist)
}

boost_funlist = function(funlist, X, y, metric = logloss_sum, silent = F, ...){
  ofun          = metric$copy()
  ofun$inputs$y <- y

  funames <- funlist %>% lapply(function(i) inherits(i, 'FUNCTION')) %>% unlist %>% which %>% names

  for(fn in funames){
    prm = funlist[[fn]]$list.parameters()
    if(!is.empty(prm)){
      ofun$reset()
      ofun$inputs$x <- funlist[[fn]]

      prm = funlist[[fn]]$list.parameters()
      keep = ofun$get.param(prm); fval = ofun$get.output.agg(data = X)
      if(!silent){cat('\n', 'Minimizing function ', fn, ' with loss: ', fval, ' ... ')}


      if(ofun$inputs$x$type %>% substr(1,13) == 'binary binner'){
        sel = prm %>% sample(min(2, length(prm)))
        res = try(minimize.ccd(ofun, parameters = sel, data = X, silent = silent), silent = T)
        ofun$reset();if(ofun$get.output.agg(data = X) > fval) ofun$set.param(keep) else {keep = ofun$get.param(prm); fval = ofun$get.output.agg(data = X)}

        sel = prm %>% sample(min(2, length(prm)))
        res = try(minimize.walk(ofun, parameters = sel, data = X, silent = silent), silent = T)
        ofun$reset();if(ofun$get.output.agg(data = X) > fval) ofun$set.param(keep) else {keep = ofun$get.param(prm); fval = ofun$get.output.agg(data = X)}
      }

      sel     = prm %>% sample(min(16, length(prm)))
      stepdir = ofun$get.gradients.agg(wrt = sel, data = X) %>% unlist %>% vect.normalize %>% {-1.0*.}
      success = try(step_forward(ofun, parameters = sel, direction = stepdir, data = X, silent = silent, ...), silent = T)

      ofun$reset();if(ofun$get.output.agg(data = X) > fval) ofun$set.param(keep) else {keep = ofun$get.param(prm); fval = ofun$get.output.agg(data = X)}
    }
  }
}

clean_funlist = function(funlist, X, y, metric = loss_sse){
  err = funlist %>% evaluate_funlist(X, y, metric = metric)
  funlist %>% list.remove(names(err)[which(is.na(err))])
}

get_function_correlations = function(funlist, X, ...){
  corl = numeric()
  for(fn in names(funlist)){
    if(inherits(funlist[[fn]], 'character')){
      x = X[, fn]
    } else if (inherits(funlist[[fn]], 'FUNCTION')){
      x = funlist[[fn]]$get.output(data = X)
    } else {stop('Unexpected element found in the list.')}
    corl[fn] <- correlation(x, ...)
  }
  return(corl)
}

reduce_funlist = function(funlist, X, y, top = 50, metric = logloss_sum){
  classes   = funlist %>% lapply(function(item) class(item)[1]) %>% unlist
  features  = names(classes)[which(classes == 'character')]
  functions = names(classes)[which(classes == 'FUNCTION')]

  errors = funlist %>% list.extract(functions) %>% evaluate_funlist(X, y, metric = metric)
  funlist %>% list.extract(
    names(errors)[order(errors)[min(length(errors), top) %>% sequence]]) %<==>%
    (funlist %>% list.extract(features))
}

# save_funlist = function(funlist, path = getwd()){
#   for(i in )
#   dataset = funlist
# }

train_funlist = function(flist = NULL, champions = list(), X_train, y_train, X_test, y_test,
                         depth = 5, iters = 3, trials = 5, n_births = 20, n_survivors = 10, silent = T,
                         loss_function = loss_sse_gb, function_set = default_function_set){
  best_train = Inf
  best_test  = Inf
  features   = colnames(X_train)
  assert(features %<% colnames(X_test), 'columns of X_train must be subset of X_test!')

  for(jj in sequence(depth)){
    # Build loss functions:
    loss_train = loss_function$deep_copy()
    loss_train$inputs$y = y_train
    loss_train$inputs$x = 0
    loss_train$inputs$z = 0
    for(ch in champions){
      if(inherits(ch, 'FUNCTION')){
        ch$reset()
        loss_train$inputs$z = loss_train$inputs$z + ch$get.output(data = X_train)
      } else {
        loss_train$inputs$z = loss_train$inputs$z + X_train[, ch]
      }
    }
    loss_train$reset()
    best_train = loss_train$get.output.agg(data = X_train)
    ##
    loss_test = loss_function$deep_copy()
    loss_test$inputs$y = y_test
    loss_test$inputs$x = 0
    loss_test$inputs$z = 0
    for(ch in champions){
      if(inherits(ch, 'FUNCTION')){
        ch$reset()
        loss_test$inputs$z = loss_test$inputs$z + ch$get.output(data = X_test)
      } else {
        loss_test$inputs$z = loss_test$inputs$z + X_test[, ch]
      }
    }
    loss_test$reset()
    best_test = loss_test$get.output.agg(data = X_test)

    # Reset champions:
    reset_funlist(champions)

    # Run loop
    cnt = 0; success = F; chance = T; enough = F
    while(!enough | (!success & chance)){
      grow_funlist(flist, features = features, n_births = n_births, function_set = function_set) -> flist
      # boost_funlist(flist, X = X_train, y = y_train, metric = loss_train, silent = silent)
      flist = boost_funlist_parallel(flist, X = X_train, y = y_train, metric = loss_train)
      flist %<>% clean_funlist(X = X_train, y = y_train, metric = loss_train)
      evaluate_funlist(flist, X = X_train, y = y_train, metric = loss_train) -> err_train
      reset_funlist(flist)
      evaluate_funlist(flist, X = X_test, y = y_test, metric = loss_test) -> err_test
      reset_funlist(flist)

      err_train = err_train[!is.na(err_train)]
      err_test  = err_test[!is.na(err_test)]
      err_test  = err_test[err_test < best_test - 0.01]
      err_train = err_train[err_train < best_train - 0.01]
      selected  = names(err_test) %^% names(err_train)
      err_test  = err_test[selected]
      err_train = err_train[selected]
      cat('\n', 'best_train: ', best_train, ' min err_train: ', min(err_train), ' min err_test: ', min(err_test), 'selected: ', length(selected))
      if(length(selected) > 0){
        if(cnt > 0){success = T}
        err_train %<>% sort
        top = min(n_survivors, length(selected))
        flist %<>% list.extract(err_train[sequence(top)] %>% names %>% union(features))
        champions_name = names(err_train)[1]
        best_train     = err_train[1]
        best_test      = err_test[champions_name]
        cat('\n', 'iter ', cnt, ' --> Success: Best Test: ', best_test, ' Best Train: ', best_train)
      } else {
        cat('\n', 'iter ', cnt, ' --> Failed')
      }

      cnt = cnt + 1; chance = cnt < trials; enough = cnt > iters
    }

    # Find the champion:
    if(champions_name %in% features){
      champions[[length(champions) + 1]] = champions_name
    } else if(champions_name %in% names(flist)){
      lnch = length(champions) + 1
      champions[[lnch]] = flist[[champions_name]]$deep_copy()
      champions[[lnch]]$name = paste0('C', lnch) %>% paste(flist[[champions_name]]$name, sep = '_')
    } else stop('Unknown Champion!')

  }
  return(champions)
}




### Permanent Genetic:

#

# 1- Pick a classifier template
# 2- Take a bunch of features
# 3- Improve Models
# 3-1 For DNNs, train with more rows
# 3-2 For Function Models, run another iteration of optimization
# 3-3 For Other classifiers chnage

#### Genetic Base & Boost: ####

# Read Existing Models and put them in a list:
read_models = function(path){
  mlist = list()
  for(dn in list.files(path)){
    mdl = model_load(dn, path)
    mlist[[mdl$name]] <- mdl$copy()
  }
  return(mlist)
}

# Internal Function, Not to Export
add_model_to_modlog = function(modlog = data.frame(), model = NULL, performance){
  if(is.null(model)) return(modlog)

  modlog[model$name, 'name'] <- model$name
  modlog[model$name, 'description'] <- model$description
  modlog[model$name, 'package'] <- model$package
  modlog[model$name, 'numTransformers'] <- length(model$transformers)
  modlog[model$name, 'numGradientTransformers'] <- length(model$gradient_transformers)
  modlog[model$name, 'numFeatures'] <- nrow(model$objects$features)
  modlog[model$name, 'bestFeature'] <- try(model$objects$features$fname[order(model$objects$features$importance, decreasing = T)[1]], silent = T)

  for(mtrc in names(performance)){
    modlog[model$name, mtrc] <- performance[mtrc]
  }
  return(modlog)
}

evaluate_models = function(modlist, X, y){
  mlog = data.frame()
  for(mdl in modlist){
    mlog %<>% add_model_to_modlog(model = mdl, X = X, y = y)
  }
  return(mlog)
}

evaluate_features = function(X, y, metrics = 'gini', ...){
  if((ncol(X) == 0) | (nrow(X) == 0)) return(NULL)
  if(inherits(X, 'WIDETABLE')){
    ef = NULL
    for(tn in unique(X$meta$table)){
      cols = X$meta %>% filter(table == tn) %>% pull(column) %>% unique
      ef = X[cols] %>% as.data.frame %>% evaluate_features(y = y, metrics = metrics) %>% rbind(ef)
    }
    return(ef)
  }
  features <- colnames(X) %>% sapply(function(i) X %>% pull(i) %>% class) %>% as.data.frame %>% {colnames(.)<-'fclass';.}
  features$n_unique  <- colnames(X) %>% sapply(function(x) X %>% pull(x) %>% unique %>% length) %>% unlist
  features$n_missing <- colnames(X) %>% sapply(function(x) X %>% pull(x) %>% is.na %>% sum) %>% unlist

  for(cn in colnames(X)){
    for(mtrc in metrics){
      metricval <- try(correlation(X %>% pull(cn), y, metric = mtrc, ...), silent = T)
      if(inherits(metricval, 'numeric')){
        features[cn, mtrc] <- metricval
      }
    }
  }
  features$type = ifelse(features$fclass == 'numeric', 'numeric', ifelse(features$fclass == 'integer', 'ordinal', 'nominal'))
  features$sumScores <- features[[metrics[1]]]
  features$numScores <- 1
  features$avgScores <- features$sumScores

  return(features)
}

gb_supporting_classifiers = c('CLS.XGBOOST', 'CLS.KERAS.DNN')
##
add_classifier = function(input = list(fetlog = NULL, modlog = data.frame(), modlist = list()),
                          templates = default_templates,
                          X_train, y_train, X_valid, y_valid,
                          classifiers = c(CLS.SKLEARN.XGB = 1, CLS.XGBOOST = 1, CLS.SKLEARN.LR = 1),
                          boosting_rate = 0.5, gradient_boosting_rate = 0.5,
                          path = NULL, metrics = c('gini', 'lift', 'loss'), ...){
  modlog   = input$modlog
  modlist  = input$modlist
  features = input$fetlog

  if(is.null(features)){
    features = evaluate_features(X_valid, y_valid, metrics[1])
  }

  # Building a base model:
  base = build_from_template(template_name = pick(classifiers), features = features, templates = templates)

  # Should I boost it with transformers?
  if(nrow(modlog) > 0){
    # Transformer Boosting
    modnames = rownames(modlog)
    while((runif(1) < boosting_rate) & (length(modnames) > 0)){
      modname  <- modnames %>% sample(size = 1, prob = modlog[modnames, metrics[1]] %>% vect.map %>% vect.normalise)
      modnames %<>% setdiff(modname)

      nt = length(base$transformers)
      if(nt > 0){
        base$transformers[[nt + 1]] <- modlist[[modname]]$copy()
      } else {
        base$transformers[[1]] <- new('MAP.RML.IDT', name = 'I', features.include = base$config$features.include)
        base$transformers[[2]] <- modlist[[modname]]$copy()
        base$config$features.include <- NULL
      }
    }

    # Gradient Transformer Boosting
    if((runif(1) < gradient_boosting_rate) & inherits(base, gb_supporting_classifiers)){
      modname  <- rownames(modlog) %>% sample(size = 1, prob = modlog[[metrics[1]]] %>% vect.map %>% vect.normalise)

      base$gradient_transformers[[1]] <- modlist[[modname]]$copy()
      base$gradient_transformers[[1]]$config$return <- 'logit'
    }

  }

  try(base$fit(X_train, y_train), silent = T) -> res
  if(!inherits(res, 'try-error')){
    modlist[[base$name]] <- base

    modperf = c()
    yp = base$predict(X_valid)[,1]
    for(mtrc in metrics){
      modperf[mtrc] <- try(correlation(yp, y_valid, metric = mtrc), silent = T)
    }

    modlog %<>% add_model_to_modlog(base, performance = modperf)
    features %<>% add_model_to_fetlog(base, performance = modperf)
    if(!is.null(path)){
      save_model(base, path = path)
    }
  } else cat('\n', 'Model fitting failed: ', res)


  return(list(fetlog = features, modlog = modlog, modlist = modlist))
}

# Internal function
# Adds feature importances of the given model to the feature log
add_model_to_fetlog = function(fetlog, model, performance){
  performance = performance[1]
  modfet      = model$objects$features %>% column2Rownames('fname')
  fet         = rownames(modfet) %^% rownames(fetlog)

  fetlog[fet, 'sumScores'] = fetlog[fet, 'sumScores'] + performance*(modfet[fet, 'importance'] %>% vect.map)
  fetlog[fet, 'numScores'] = fetlog[fet, 'numScores'] + 1
  fetlog[fet, 'avgScores'] = fetlog[fet, 'sumScores']/fetlog[fet, 'numScores']

  return(fetlog)
}

#### End ####








