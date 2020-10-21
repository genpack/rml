#' @export MAP.SKLEARN.NRM
MAP.SKLEARN.NRM = setRefClass(
  'MAP.SKLEARN.NRM',
  contains = 'TRM.SKLEARN',
  methods = list(
    initialize = function(model.module = 'preprocessing', model.class = 'Normalizer', ...){
      callSuper(...)
      type             <<- 'Mapper'
      description      <<- 'Normalizer'

      if(is.empty(name)){name <<- 'SKNRM' %>% paste0(sample(10000:99999, 1))}
    }
))


#' @export MAP.SKLEARN.ZFS
MAP.SKLEARN.ZFS = setRefClass(
  'MAP.SKLEARN.ZFS',
  contains = 'MODEL',
  methods = list(
    initialize = function(model.module = 'preprocessing', model.class = 'StandardScaler', ...){
      callSuper(...)
      type             <<- 'Mapper'
      description      <<- 'Z-Factor Standard Scaler'

      if(is.empty(name)){name <<- 'SKZFS' %>% paste0(sample(10000:99999, 1))}
    }
))

#' @export MAP.SKLEARN.MMS
MAP.SKLEARN.MMS = setRefClass(
  'MAP.SKLEARN.MMS',
  contains = 'TRM.SKLEARN',
  methods = list(
    initialize = function(model.module = 'preprocessing', model.class = 'MinMaxScaler', ...){
      callSuper(...)
      type             <<- 'Mapper'
      description      <<- 'MinMax Scaler'
      if(is.empty(name)){name <<- 'SKMMS' %>% paste0(sample(10000:99999, 1))}
    }
))


#' @export MAP.STATS.PCA
MAP.STATS.PCA = setRefClass('MAP.STATS.PCA', contains = 'MODEL', methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Mapper'
    description      <<- 'Principal Component'
    package          <<- 'stats'
    package_language <<- 'R'
    if(is.empty(name)){name <<- 'PRC' %>% paste0(sample(10000:99999, 1))}
    config$num_components <<- verify(config$num_components, c('numeric', 'integer'), default = 5)
    config$scale  <<- verify(config$scale, 'logical', domain = c(F,T), default = T)
    config$center <<- verify(config$center, 'logical', domain = c(F,T), default = T)
  },
  
  model.fit = function(X, y = NULL){
    objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
    X = X[objects$features$fname]
    objects$model <<- prcomp(X, center = config$center, scale. = config$scale, rank. = config$rank)
    
    # Remove x object to reduce memory allocation. (Not needed for transformation:)
    config$keep_x %<>% verify('logical', lengths = 1, domain = c(T, F), default = F)
    if(!config$keep_x){
      objects$model$x <<- NULL
    }
  },
  
  model.predict = function(X){
    XOUT = stats::predict(objects$model, X %>% as.matrix)
    XOUT = XOUT[,sequence(min(ncol(XOUT), config$num_components))] %>% as.data.frame
    # colnames(XOUT) <- name %>% paste(colnames(XOUT), sep = '_')
    return(XOUT)
  }
))

# previously PYLMNN
MAP.PYLMNN.LMNN = setRefClass('MAP.PYLMNN.LMNN', contains = "MODEL", methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Mapper'
    description      <<- 'Large Margin Nearest Neighbors'
    package          <<- 'pylmnn'
    package_language <<- 'Python'
    if(is.empty(name)){name <<- 'LMNN' %>% paste0(sample(10000:99999, 1))}
    config$num_components <<- verify(config$num_components, c('numeric', 'integer'), default = 5) %>% as.integer
    config$num_neighbors  <<- verify(config$num_neighbors , c('numeric', 'integer'), default = 10) %>% as.integer
    config$epochs         <<- verify(config$epochs, c('numeric', 'integer'), default = 100) %>% as.integer
    lmnn_module   <-  reticulate::import('pylmnn')
    objects$model <<- lmnn_module$LargeMarginNearestNeighbor(n_neighbors = config$num_neighbors, max_iter = config$epochs, n_components = config$num_components, init = 'pca')
  },
  
  model.fit = function(X, y){
    objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
    X = X[objects$features$fname]
    objects$model$fit(X %>% data.matrix, y)
  },
  
  model.predict = function(X){
    XOUT = objects$model$transform(X %>% data.matrix) %>% as.data.frame
    return(XOUT)
  }
))


# previously: IDENTITY
#' @export MAP.RML.IDT
MAP.RML.IDT = setRefClass('MAP.RML.IDT' , contains = "MODEL",
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Mapper'
      description      <<- 'Identity'
      package          <<- 'rml'
      package_language <<- 'R'
      if(is.empty(name)){name <<-'IDT' %>% paste0(sample(10000:99999, 1))}
    },
    
    model.fit = function(X, y){
      # do nothing
    },
    
    model.predict = function (X){
      # Do nothing
      X
    }
))


# previously NORMALIZER
#' @export MAP.RML.MMS
MAP.RML.MMS = setRefClass('MAP.RML.MMS', contains = 'MODEL',
                          methods = list(
                            initialize = function(...){
                              callSuper(...)
                              type             <<- 'Mapper'
                              description      <<- 'MinMax Scaler'
                              package          <<- 'rml'
                              package_language <<- 'R'
                              config$suffix    <<- verify(config$suffix, 'character', default = 'NRM')
                              if(is.empty(name)){name <<- 'MMS' %>% paste0(sample(10000:99999, 1))}
                            },
                            
                            model.fit = function(X, y = NULL){
                              objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
                              X = X[objects$features$fname] %>% as.matrix
                              X %>% apply(2, function(v) min(v, na.rm = T)) ->> objects$features$min
                              X %>% apply(2, function(v) max(v, na.rm = T)) ->> objects$features$max
                            },
                            
                            model.predict = function(X){
                              feat  = objects$features %>% column2Rownames('fname')
                              objects$features$fname %>% sapply(function(i) (X[,i] -  feat[i,'min'])/(feat[i,'max'] - feat[i,'min'])) %>% na2zero %>% as.data.frame
                            }
                          ))

# previously SCALER
#' @export MAP.RML.ZFS
MAP.RML.ZFS = setRefClass('MAP.RML.ZFS', contains = "MODEL", methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Mapper'
    description      <<- 'ZFactor Standard Scaler'
    package          <<- 'rml'
    package_language <<- 'R'
    
    config$suffix    <<- verify(config$suffix, 'character', default = 'SCALED')
    if(is.empty(name)){name <<- 'ZFS' %>% paste0(sample(10000:99999, 1))}
  },
  
  model.fit = function(X, y = NULL){
    objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
    X = X[objects$features$fname] %>% as.matrix
    X %>% apply(2, function(v) mean(v, na.rm = T)) ->> objects$features$mean
    X %>% apply(2, function(v)   sd(v, na.rm = T)) ->> objects$features$stdv
  },
  
  model.predict = function(X){
    feat  = objects$features %>% column2Rownames('fname')
    objects$features$fname %>% sapply(function(i) (X[,i] -  feat[i,'mean'])/feat[i,'stdv']) %>% na2zero %>% as.data.frame
  }
))

MAP.SKLEARN.QT = setRefClass(
  'MAP.SKLEARN.QT',
  contains = 'MODEL',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Mapper'
      description      <<- 'Quantile'
      package          <<- 'sklearn'
      package_language <<- 'Python'
      
      if(is.empty(name)){name <<- 'QT' %>% paste0(sample(10000:99999, 1))}
    },
    
    model.fit = function(X, y){
      if(!fitted){
        objects$features <<- objects$features %>% filter(fname %in% c('numeric', 'integer'))
        X = X[objects$features$fname]
        
        module = reticulate::import('sklearn.preprocessing')
        objects$model <<- do.call(module$QuantileTransformer, config %>% list.remove(reserved_words))
        objects$model$fit(X, y)
      }
    },
    
    model.predict = function(X){
      objects$model$transform(X)
    }
  ))

