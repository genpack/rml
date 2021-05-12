
#' @export ENC.FASTDUMMIES.OHE
ENC.FASTDUMMIES.OHE = setRefClass('ENC.FASTDUMMIES.OHE', contains = "MODEL",
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Encoder'
      description      <<- 'One Hot Encoder'
      package          <<- 'fastDummies'
      packages_required <<- c(packages_required, 'fastDummies')
      
      package_language <<- 'R'
      reserved_words   <<- c(reserved_words, 'max_domain')
      
      if(is.empty(name)){name <<- 'DMFR' %>% paste0(sample(10000:99999, 1))}
      config$max_domain <<- verify(config$max_domain, c('numeric', 'integer'), default = 25) %>% as.integer
    },
    
    model.fit = function(X, y = NULL){
      objects$features     <<- objects$features %>% filter(fname %in% nominals(X))
      
      warnif(is.empty(objects$features), 'No categorical columns found!', wrn_src = name)
      dummies = character(); nbin = character()
      for(cat in objects$features %>% pull(fname)){
        uval = X %>% pull(cat) %>% unique
        nduv = length(uval) # number of distinct ubique values
        warnif(nduv < 3, 'Feature ' %++% cat %++% ' has less than 3 distinct values and will not be encoded!', wrn_src = name)
        warnif(nduv > config$max_domain, 'Feature ' %++% cat %++% ' has too many distinct values and will not be encoded!', wrn_src = name)
        if((length(uval) > 2) & (length(uval) < config$max_domain)){
          nbin = c(nbin, cat)
          dummies %<>% c(cat %>% paste(uval, sep = '_'))
        }
      }
      objects$features     <<- objects$features %>% filter(fname %in% nbin)
      assert(!is.empty(objects$features), 'No categorical columns for dummification!', err_src = name)
      
      ## todo: take care of remove_first_dummy and remove_most_frequent_dummy arguments
      objects$dummy_columns <<- dummies
      # todo: rename to output_features and add importance(performance)
    },
    
    model.predict = function(X){
      X %>%
        fastDummies::dummy_cols(objects$features$fname, remove_first_dummy = FALSE, remove_most_frequent_dummy = FALSE) %>%
        {.[, -(sequence(length(objects$features$fname))), drop = F]} -> res
      # We need to make sure the column names of the output is exactly the same as the table given in fitting
      comm_cols  = colnames(res) %^% objects$dummy_columns
      less_cols  = objects$dummy_columns %-% colnames(res)
      extra      = data.frame.na(nrow = nrow(res), ncol = length(less_cols), col_names = less_cols)
      
      res[comm_cols] %>% cbind(extra) %>% na2zero %>% {.[, objects$dummy_columns, drop = F]}
    }
  )
)


#' @export ENC.SKLEARN.OHE
ENC.SKLEARN.OHE = setRefClass(
  'ENC.SKLEARN.OHE',
  contains = 'TRM.SKLEARN',
  methods = list(
    initialize = function(...){
      callSuper(model.module = 'preprocessing', model.class = 'OneHotEncoder', ...)
      type             <<- 'Encoder'
      description      <<- 'One-Hot Encoder'
      reserved_words   <<- c(reserved_words, 'max_domain')
      
      config$pp.remove_nominal_features <<- F
      config$pp.remove_numeric_features <<- T
      if(is.empty(name)){name <<- 'SKOHE' %>% paste0(sample(10000:99999, 1))}
      config$max_domain <<- verify(config$max_domain, c('numeric', 'integer'), default = 25) %>% as.integer
    },
    
    model.fit = function(X, y){
      if(!fitted){
        if(!'n_unique' %in% colnames(objects$features)){
          objects$features$n_unique <<- rbig::colnames(X) %>% sapply(function(x) X %>% pull(x) %>% unique %>% length) %>% unlist
        }
        objects$features <<- objects$features %>% filter(fname %in% nominals(X), n_unique <= config$max_domain)
        
        X = X[objects$features$fname]
        callSuper()
      }
    },
    
    model.predict = function(X){
      objects$model$transform(X) %>% as.matrix %>% as.data.frame
    }
))


#' @export ENC.CATEGORY_ENCODERS
ENC.CATEGORY_ENCODERS = setRefClass(
  'ENC.CATEGORY_ENCODERS',
  contains = 'MODEL',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Encoder'
      description      <<- 'Superclass Wrapper for encoders from Python package category_encoders'
      package          <<- 'category_encoders'
      package_language <<- 'Python'
      config$pp.remove_nominal_features <<- F
      config$pp.remove_numeric_features <<- T
      
      config$model.module <<- verify(config$model.module, 'character', lengths = 1, null_allowed = F)
      config$model.class  <<- verify(config$model.class, 'character' , lengths = 1, null_allowed = F)

      if(is.empty(name)){name <<- 'CEENC' %>% paste0(sample(10000:99999, 1))}
      
      objects$module <<- reticulate::import(paste('category_encoders', config[['model.module']], sep = '.'))
    },
    
    model.fit = function(X, y){
      if(!fitted){
        module = reticulate::import('category_encoders')
        objects$model <<- do.call(objects$module[[config$model.class]],
                                  config %>% list.remove(reserved_words) %>% list.add(cols = objects$features$fname))
        objects$model$fit(X, y)
      }
    },
    
    model.predict = function(X){
      objects$model$transform(X)
    }
))


####

#' @export ENC.CATEGORY_ENCODERS.CATB
ENC.CATEGORY_ENCODERS.CATB = setRefClass(
  'ENC.CATEGORY_ENCODERS.CATB',
  contains = 'ENC.CATEGORY_ENCODERS',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Encoder'
      description      <<- 'CatBoost Encoder'
      package          <<- 'category_encoders'
      package_language <<- 'Python'
      config$model.module     <<- 'cat_boost'
      config$model.class      <<- 'CatBoostEncoder'
      
      if(is.empty(name)){name <<- 'CECATB' %>% paste0(sample(10000:99999, 1))}
    }
))

ENC.CATEGORY_ENCODERS.JSTN = setRefClass(
  'ENC.CATEGORY_ENCODERS.JSTN',
  contains = 'ENC.CATEGORY_ENCODERS',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Encoder'
      description      <<- 'James Stein Encoder'
      package          <<- 'category_encoders'
      package_language <<- 'Python'
      config$model.module     <<- 'james_stein'
      config$model.class      <<- 'JamesSteinEncoder'
      
      if(is.empty(name)){name <<- 'CEJSTN' %>% paste0(sample(10000:99999, 1))}
    }
))


#' @export ENC.CATEGORY_ENCODERS.HLMRT
ENC.CATEGORY_ENCODERS.HLMRT = setRefClass(
  'ENC.CATEGORY_ENCODERS',
  contains = 'MODEL',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Encoder'
      description      <<- 'Helmert Encoder'
      package          <<- 'category_encoders'
      package_language <<- 'Python'
      config$model.module     <<- 'helmert'
      config$model.class      <<- 'HelmertEncoder'
      
      if(is.empty(name)){name <<- 'CEHLMRT' %>% paste0(sample(10000:99999, 1))}
    }
))

ENC.RML.FE = setRefClass(
  'ENC.RML.FE', contains = 'MODEL',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Encoder'
      description      <<- 'Feature Encoder'
      package          <<- 'rml'
      package_language <<- 'R'
      reserved_words   <<- c(reserved_words, 'action_by_original')
      
      config$aggregator <<- verify(config$aggregator, 'character', default = 'mean')
      
      if(is.empty(name)){name <<- 'FE' %>% paste0(sample(10000:99999, 1))}
    },
    
    model.fit = function(X, y = NULL){
      if(!fitted){
        catfigs = objects$features %>% filter(fclass %in% c('character', 'integer', 'factor')) %>% pull(fname) %>% intersect(rbig::colnames(X))
        if(!is.null(config$categoricals)) {catfigs = catfigs %^% config$categoricals}
        
        numfigs = objects$features %>% filter(fclass %in% c('numeric', 'integer')) %>% pull(fname) %>% intersect(rbig::colnames(X)) %>% setdiff(catfigs)
        assert(length(catfigs) > 0, 'No categorical feature for encoding')
        assert(length(numfigs) > 0, 'No numerical features for feature encoding')
        objects$categoricals <<- catfigs
        
        objects$features <<- objects$features %>% filter(fname %in% c(catfigs, numfigs))
        X = X[objects$features$fname]
        
        objects$model <<- list()
        for(ccol in catfigs){
          objects$model[[ccol]] <<- list()
          for(ncol in numfigs){
            objects$model[[ccol]][[ncol]] <<- parse(
              text = sprintf("summarise(group_by(select(X, c(%s, %s)), %s), %s_%s = %s(%s))", ccol, ncol, ccol, ccol, ncol, config$aggregator, ncol)) %>% eval
          }
        }
      }
    },
    
    model.predict = function(X){
      XOUT = X[c()]
      
      for(ccol in names(objects$model)){
        for(ncol in names(objects$model[[ccol]])){
          cn   = paste(ccol, ncol, sep = '_')
          XOUT = X %>% left_join(objects$model[[ccol]][[ncol]], by = ccol) %>% {.[cn]} %>% cbind(XOUT)
          if(inherits(config$action_by_original, 'function')){
            XOUT[, cn] <- do.call(config$action_by_original, list(XOUT[, cn], X[, ncol])) %>% verify(c('numeric', 'integer'))
          }
        }
      }
      return(XOUT)
    }
  )
)

# Replaces categorical features with class ratios associated with each category
#' @export ENC.RML.TE
ENC.RML.TE = setRefClass('ENC.RML.TE', contains = 'MODEL',
                         methods = list(
                           initialize = function(...){
                             callSuper(...)
                             type             <<- 'Encoder'
                             description      <<- 'Target Encoder'
                             package          <<- 'rml'
                             package_language <<- 'R'
                             
                             if(is.empty(name)){name <<- 'TE' %>% paste0(sample(10000:99999, 1))}
                           },
                           
                           model.fit = function(X, y){
                             if(!fitted){
                               objects$features <<- objects$features %>% filter(fname %in% nominals(X))
                               X = X[objects$features$fname]
                               stopifnot(ncol(X) > 0, 'No categorical feature for encoding')
                               objects$model <<- list()
                               for(col in rbig::colnames(X)){
                                 objects$model[[col]] <<- cbind(X, label = y) %>% group_by_(col) %>% summarise(ratio = mean(label))
                               }
                             }
                           },
                           
                           model.predict = function(X){
                             XOUT  = X
                             for(col in objects$features$fname){
                               cn = 'ratio' %>% {names(.) <- name %>% paste(col, sep = '_');.}
                               XOUT %<>% left_join(objects$model[[col]], by = col) %>% spark.rename(cn) %>% spark.unselect(col) %>% na2median
                             }
                             return(XOUT)
                           }
                         )
)

# Todo: does not save joblib for Python segment models
#' @export ENC.RML.ME
ENC.RML.ME = setRefClass('ENC.RML.ME', contains = 'MODEL',
                         methods = list(
                           initialize = function(...){
                             callSuper(...)
                             type             <<- 'Encoder'
                             description      <<- 'Model Encoder'
                             package          <<- 'rml'
                             package_language <<- 'R'
                             reserved_words   <<- c(reserved_words, 'segmentation_features')
                             
                             config$model.class  <<- verify(config$model.class, 'character', default = 'CLS.SKLEARN.XGB')
                             config$model.config <<- verify(config$model.config, 'list', default = list(predict_probabilities = T))
                             config$min_rows     <<- verify(config$min_rows, c('numeric', 'integer'), lengths = 1, default = 100)
                             if(is.empty(name)){name <<- 'ME' %>% paste0(sample(10000:99999, 1))}
                             objects$model <<- list()
                           },
                           
                           model.fit = function(X, y){
                             if(is.null(config$segmentation_features)){
                               objects$categoricals <<- nominals(X)
                             } else {
                               objects$categoricals <<- config$segmentation_features %^% nominals(X)
                             }
                             assert(!is.empty(objects$categoricals), 'No descrete features found!')
                             
                             if(is.null(objects$model[['__global__']])){
                               objects$model[['__global__']] <<- new(config$model.class, config = config$model.config)
                             }
                             
                             objects$model[['__global__']]$fit(X, y)
                             for(col in objects$categoricals){
                               objects$model[[col]] <<- list()
                               Xcol = X %>% pull(col)
                               uval = Xcol %>% unique
                               for(val in uval){
                                 vlc = as.character(val)
                                 www = which(Xcol == val)
                                 if(length(www) > config$min_rows){
                                   objects$model[[col]][[vlc]] <<- new(config$model.class, config = config$model.config)
                                   objects$model[[col]][[vlc]]$fit(X[www,], y[www])
                                 }
                               }
                             }
                           },
                           
                           model.predict = function(X){
                             XOUT  = NULL
                             X['__rowid__'] = nrow(X) %>% sequence
                             for(col in objects$categoricals){
                               bibi = function(dot){
                                 dot %<>% as.data.frame
                                 NNN = nrow(dot)
                                 mdl = objects$model[[col]][[dot[1,col] %>% as.character]]
                                 if(is.null(mdl)) {mdl = objects$model[['__global__']]}
                                 dot$value = mdl$predict(dot)[,1]
                                 dot[c('__rowid__', 'value')]
                               }
                               cn = 'value' %>% {names(.) <- col;.}
                               df = X['__rowid__'] %>%
                                 left_join(X %>% group_by_(col) %>% do({bibi(.)}), by = '__rowid__') %>% select(value) %>% spark.rename(cn)
                               
                               if(is.null(XOUT)){XOUT = df[,1, drop = F]} else {XOUT %<>% cbind(df[,1, drop = F])}
                             }
                             # colnames(XOUT) <- NULL
                             return(XOUT)
                           }
                         ))

# previously: SEGMENTER.MODEL.BOOSTER
# Model Encoder Booster
ENC.RML.MEB =
  setRefClass('ENC.RML.MEB',
              contains = 'ENC.RML.ME',
              
              methods = list(
                model.fit = function(X, y){
                  nn = nrow(X)
                  indtrain = nn %>% sequence %>% sample(floor(0.7*nn))
                  X_train  = X[indtrain,]
                  y_train  = y[indtrain]
                  X_test   = X[- indtrain,]
                  y_test   = y[- indtrain]
                  
                  callSuper(X_train, y_train)
                  
                  colnms = objects$model %>% names %>% setdiff("__global__")
                  glb    =  objects$model[['__global__']]
                  for(col in colnms){
                    nms = names(objects$model[[col]])
                    for(valc in nms){
                      mdl = objects$model[[col]][[valc]]
                      ind = which(X_test[,col] == valc)
                      if(length(ind) > config$min_rows){
                        pm = mdl$performance(X_test[ind,], y_test[ind], metric = 'gini')
                        pg = glb$performance(X_test[ind,], y_test[ind], metric = 'gini')
                        if(pg > pm){
                          objects$model[[col]][[valc]] <<- NULL
                        }
                      }
                    }
                  }
                }
              )
  )
