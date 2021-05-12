FET.SKLEARN.MFG = setRefClass(
  'FET.SKLEARN.MFG',
  contains = 'TRM.SKLEARN',
  methods = list(
    initialize = function(...){
      callSuper(model.module = 'preprocessing', model.class = 'PolynomialFeatures', ...)
      type             <<- 'Feature Generator'
      description      <<- 'Multiplicative Feature Generator'

      if(is.empty(name)){name <<- 'SKMFG' %>% paste0(sample(10000:99999, 1))}
    }
))



# previously : MULTIPLIER.BINARY
FET.RML.D2MUL = setRefClass('FET.RML.D2MUL', contains = 'MODEL', methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Feature Generator'
    description      <<- 'Degree 2 Multiplier'
    package          <<- 'rml'
    package_language <<- 'R'
    if(is.empty(name)){name <<- 'D2MUL' %>% paste0(sample(10000:99999, 1))}
  },
  
  model.fit = function(X, y = NULL){
    objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
    actions = NULL
    for(ft in objects$features$fname){
      actions %<>% rbind(data.frame(father = ft, mother = objects$features$fname %-% actions$father))
    }
    objects$actions <<- actions
  },
  
  model.predict = function(X){
    out = X[, c()]
    for(i in 1:nrow(objects$actions)){
      out[, paste(objects$actions$father[i], objects$actions$mother[i], sep = '_X_')] <- X[, objects$actions$father[i]]*X[, objects$actions$mother[i]]
    }
    return(out)
  }
))


# Previously GENETIC.BOOSTER.GEOMETRIC
FET.RML.MGB = setRefClass('FET.RML.MGB', contains = 'MODEL',
  methods = list(
    initialize = function(...){
      type             <<- 'Feature Generator'
      description      <<- 'Multiplicative Genetic Booster'
      package          <<- 'rml'
      package_language <<- 'R'
      if(is.empty(name)){name <<- 'MGB' %>% paste0(sample(10000:99999, 1))}
      
      callSuper(...)
      if(is.null(config$epochs)) {config$epochs <<- 10}
      if(is.null(config$max_fails)) {config$max_fails <<- 2}
      if(is.null(config$cycle_births)) {config$cycle_births <<- 1000}
      if(is.null(config$cycle_survivors)) {config$cycle_survivors <<- 100}
      if(is.null(config$final_survivors)) {config$final_survivors <<- 5}
      if(is.null(config$metric)){config$metric <<- 'gini'}
    },
    
    root_parents = function(fn){
      originals = rownames(objects$model)[is.na(objects$model$father) | is.na(objects$model$mother)]
      if(is.null(objects$root_parents)) {objects$root_parents <<- list()}
      if(!is.null(objects$root_parents[[fn]])){rp = objects$root_parents[[fn]]} else {
        if(fn %in% originals){
          rp = fn
        } else {
          rp = c()
          if(objects$model[fn, 'father'] %in% originals){rp = c(rp, objects$model[fn, 'father'])} else {rp = c(rp, root_parents(fn = objects$model[fn, 'father']))}
          if(objects$model[fn, 'mother'] %in% originals){rp = c(rp, objects$model[fn, 'mother'])} else {rp = c(rp, root_parents(fn = objects$model[fn, 'mother']))}
        }
        objects$root_parents[[fn]] <<- rp
      }
      return(objects$root_parents[[fn]])
    },
    
    # nf features are born by random parents:
    createFeatures = function(prefix = 'F'){
      features  = rownames(objects$model)
      
      objects$model <<- objects$model %>% rbind(
        data.frame(
          fname  = prefix %>% paste0(nrow(objects$model) + sequence(config$cycle_births)),
          father = features %>% sample(config$cycle_births, replace = T),
          mother = features %>% sample(config$cycle_births, replace = T),
          correlation = NA,
          safety = 0, rps = as.character(NA), stringsAsFactors = F) %>%
          mutate(parents = paste(ifelse(father <= mother, father, mother), ifelse(mother <= father, father, mother))) %>%
          distinct(parents, .keep_all = T) %>% select(-parents) %>%
          column2Rownames('fname'))
      for(fn in rownames(objects$model)){
        if(is.na(objects$model[[fn, 'rps']])){
          objects$model[[fn, 'rps']] <<- root_parents(fn) %>% sort %>% paste(collapse = ',')
        }
      }
      objects$model <<- objects$model[!duplicated(objects$model$rps),]
      objects$root_parents <<- objects$root_parents %>% list.extract(rownames(objects$model))
    },
    
    evaluateFeatures = function(X, y){
      top     = config$cycle_survivors
      columns = rbig:colnames(X)
      flist   = objects$model
      ns      = rownames(flist)
      
      keep = is.na(flist$correlation) & (flist$father %in% columns) & (flist$mother %in% columns)
      if(sum(keep) > 0){
        flist$correlation[keep] <- correlation(X[, flist$father[keep]]*X[, flist$mother[keep]], y, metric = config$metric) %>% as.numeric %>% abs
      }
      keep = is.na(flist$correlation) %>% which
      
      if(length(keep) > 0){
        flist$correlation[keep] <- correlation(getFeatureValue.multiplicative(flist, ns[keep], X), y, metric = config$metric)
      }
      
      # wna = flist$correlation %>% is.na
      # wdp = flist$correlation %>% duplicated
      # flist = flist[wna | !wdp, ]
      
      high_level = max(flist$safety) + 1
      # ord = flist$correlation %>% order(decreasing = T) %>% intersect(which(!duplicated(flist$correlation)))
      ord = flist$correlation %>% order(decreasing = T)
      
      top  = min(top, length(ord) - 1)
      
      flist %<>% immune(ns[ord[sequence(top)]], level = high_level, columns = rbig::colnames(X))
      
      # keep = which(flist$safety == high_level | (is.na(flist$father) & is.na(flist$mother)))
      keep = which(flist$safety == high_level)
      objects$model <<- flist[keep, ]
    },
    
    model.predict = function(X){
      top = objects$model %>% rownames2Column('fname') %>% distinct(correlation, .keep_all = T) %>%
        arrange(desc(correlation)) %>% head(config$final_survivors)
      
      XOUT <- getFeatureValue.multiplicative(objects$model, top$fname, X)
      return(XOUT %>% as.data.frame)
    },
    
    model.fit = function(X, y){
      objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
      X = X[objects$features$fname]
      objects$model <<- data.frame(fname = objects$features$fname, father = as.character(NA), mother = as.character(NA), correlation = correlation(X, y, metric = config$metric), safety = 0, rps = as.character(NA), stringsAsFactors = F) %>% column2Rownames('fname')
      objects$fdata <<- X[objects$features$fname]
      
      i = 0;j = 0;prev_best = -Inf
      while((i < config$epochs) & (j < config$max_fails)){
        i = i + 1
        createFeatures()
        evaluateFeatures(X = X[objects$features$fname], y = y)
        best = max(objects$model$correlation)
        cat('Iteration:', i, ': Best Correlation = ', best, ' population = ', nrow(objects$model), '\n')
        if(best > prev_best){
          prev_best = best
        } else {
          j = j + 1
        }
      }
    }
  )
)


# previously : MULTIPLIER.BINARY.COMP
FET.RML.D2MULC = setRefClass('FET.RML.D2MULC', contains = 'FET.RML.D2MUL', methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Feature Generator'
    description      <<- 'Degree 2 Multiplier Components'
    package          <<- 'rml'
    package_language <<- 'R'
    
    if(is.empty(name)){name <<- 'D2MC' %>% paste0(sample(10000:99999, 1))}
    config$num_components   <<- verify(config$num_components, c('numeric', 'integer'), default = 5)
    config$model_class      <<- verify(config$model_class, 'character' , default = 'SKLEARN.XGB')
    config$model_config     <<- verify(config$model_config, 'character', default = list())
  },
  
  model.fit = function(X, y){
    callSuper(X)
    objects$model <<- new(config$model_class, config = config$model_config)
    feature_subset_scorer(objects$model, predict(X), y) -> scores
    objects$features <<- scores %>% rownames2Column('fname') %>%
      select(fname, importance = score) %>% filter(importance > 0) %>% arrange(desc(importance)) %>% head(config$num_components)
  },
  
  model.predict = function(X){
    X
  }
))

FNT.RML.POLY = setRefClass('FNT.RML.POLY', contains = 'MODEL', methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Function Transformer'
    description      <<- 'Polynomial'
    package          <<- 'rml'
    package_language <<- 'R'
    if(is.empty(name)){name <<- 'PLY' %>% paste0(sample(10000:99999, 1))}
    
    # Sigma{k = 1}{n_terms} (gain*x + intercept)^k
    config$apply_abs <<- verify(config$apply_abs, 'logical', default = F)
    config$intercept <<- verify(config$intercept, 'numeric', default = 0)
    config$gain      <<- verify(config$gain, 'numeric', default = 1)
    config$n_terms   <<- verify(config$n_terms, 'numeric', default = 3) %>% as.integer
  },
  
  model.fit = function(X, y = NULL){
    objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
  },
  
  model.predict = function(X){
    XOUT = NULL; ns = character()
    for(i in objects$features$fname){
      v = X[,i] %>% as.numeric
      v = config$gain*v + config$intercept
      if(config$apply_abs){v = abs(v)}
      for(j in sequence(config$n_terms)){
        XOUT %<>% cbind(v^j)
        ns   %<>% c(name %>% paste('_', i, '_T', j))
      }
    }
    rbig:colnames(XOUT) <- ns
    return(XOUT %>% as.data.frame)
  }
  
))


# previously GENETIC.BOOSTER.LOGICAL
FET.RML.LGB = setRefClass('FET.RML.LGB', contains = 'MODEL', methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Feature Generator'
    description      <<- 'Logical Genetic Booster'
    package          <<- 'rml'
    package_language <<- 'R'
    
    if(is.empty(name)){name <<- 'GBL' %>% paste0(sample(10000:99999, 1))}
    
    if(is.null(config$metric)){config$metric <<- 'f1'}
    if(is.null(config$epochs)) {config$epochs <<- 10}
    if(is.null(config$max_fails)) {config$max_fails <<- 2}
    if(is.null(config$cycle_births)) {config$cycle_births <<- 1000}
    if(is.null(config$cycle_survivors)) {config$cycle_survivors <<- 100}
    if(is.null(config$final_survivors)) {config$final_survivors <<- 5}
    if(is.null(config$target)) {config$target <<- 0.9}
  },
  
  getFeatureValue = function(fname, dataset){
    if(!fitted) stop(paste('from', fname, 'of type', type, ':', 'Model not fitted!', '\n'))
    getFeatureValue.logical(objects$model, fname, dataset)
  },
  
  model.fit = function(X, y){
    objects$model    <<- genBinFeatBoost.fit(X, y, target = config$target, epochs = config$epochs, max_fails = config$max_fails, cycle_survivors = config$cycle_survivors, final_survivors = config$final_survivors, cycle_births = config$cycle_births, metric = config$metric)
    objects$features <<- objects$model %>%
      rownames2Column('fname') %>%
      filter(is.na(father), is.na(mother)) %>%
      select(fname, importance = correlation)
  },
  
  model.predict = function(X){
    top = objects$model %>% rownames2Column('fname') %>% distinct(correlation, .keep_all = T) %>%
      arrange(desc(correlation)) %>% head(config$final_survivors)
    XOUT = NULL
    for (i in top %>% nrow %>% sequence){
      XOUT %<>% cbind(getFeatureValue(top$fname[i], X))
    }
    rbig:colnames(XOUT) <- top$fname
    return(XOUT %>% as.data.frame %>% numerize_columns)
  }
))


FET.RML.SFS = setRefClass('FET.RML.SFS', contains = 'MODEL', methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Feature Generator'
    description      <<- 'Stepwise Feature Selector'
    package          <<- 'rml'
    package_language <<- 'R'
    if(is.empty(name)){name <<- 'SFS' %>% paste0(sample(10000:99999, 1))}
    config$model_class      <<- verify(config$model_class, 'character', default = 'CLS.SKLEARN.XGB')
    config$model_config     <<- verify(config$model_config, 'list', default = list())
    config$return_features  <<- config$return_features %>% verify('logical', lengths = 1, domain = c(T,F), default = T)
    config$feature_subset_size <<- verify(config$feature_subset_size, c('integer', 'numeric'), default = 200)
  },
  
  model.fit = function(X, y){
    if(is.null(config$model_config$cv.set) & !is.null(config$cv.set)){
      config$model_config$cv.set <<- config$cv.set
    }
    cv.set.transformed = config$model_config$cv.set
    if(!is.null(config$model_config$cv.set) & !is.empty(transformers)){
      for(i in sequence(length(cv.set.transformed))){cv.set.transformed[[i]]$X %<>% transform}
    }
    
    if(is.null(config$feature_order)){
      mdl <- new(config$model_class, config = config$model_config)
      mdl$config$cv.set = cv.set.transformed
      
      feature_subset_scorer(mdl, X, y, subset_size = config$feature_subset_size) -> scores
      objects$features <<- scores %>% rownames2Column('fname') %>%
        filter(score > 0) %>% arrange(desc(score)) %>% mutate(fname = as.character(fname)) %>% select(fname)
    } else {
      config$feature_order <<- config$feature_order %>% intersect(rbig:colnames(X))
      objects$features <<- data.frame(fname = config$feature_order, stringsAsFactors = F)
    }
    
    # Bild best model with best predictor:
    fetbag = c()
    perf   = -Inf
    
    remaining_features = objects$features$fname
    
    while(length(remaining_features) > 0){
      ft = remaining_features %>% head(n = min(config$feature_subset_size, length(remaining_features)))
      
      mdl = new(config$model_class, config = config$model_config)
      mdl$config$cv.set = cv.set.transformed
      
      new_perf = mdl$performance.cv(X[c(fetbag, ft)], y) %>% median
      if(new_perf > perf){
        cat('\n', 'New feature added: ', ft, ' New performance: ', new_perf)
        fetbag = mdl$objects$features %>% filter(importance > 0) %>% pull(fname)
        perf   = new_perf
        if(!config$return_features){
          objects$model <<- mdl$copy()
        }
      }
      remaining_features %<>% setdiff(ft)
    }
    
    # Train the final model with selected features or just keep selected features?!
    objects$features <<- objects$features %>% filter(fname %in% fetbag)
  },
  
  model.pedict = function(X){
    if(config$return_features) {return(X)} else {
      return(objects$model$predict(X))
    }
  }
))

