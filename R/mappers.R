#' @title Row normaliser (scikit-learn)
#' @name MAP.SKLEARN.NRM-class
#' @aliases MAP.SKLEARN.NRM
#' @description
#' Wraps \code{sklearn.preprocessing.Normalizer}. Its arguments can be passed.
#' Requires Python with the module named below, reachable through reticulate. Set \code{python_address} to choose the Python executable; see \code{\link{python_setup}} and \code{vignette("rml-troubleshooting")} if the module is not found.
#'
#' Inherits from \code{\link{TRM.SKLEARN-class}}.
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = MAP.SKLEARN.NRM()
#' m$fit(X, y)
#' head(m$predict(X))
#' }
#' @export MAP.SKLEARN.NRM
MAP.SKLEARN.NRM = setRefClass(
  'MAP.SKLEARN.NRM',
  contains = 'TRM.SKLEARN',
  methods = list(
    initialize = function(...){
      callSuper(model.module = 'preprocessing', model.class = 'Normalizer', ...)
      type             <<- 'Mapper'
      description      <<- 'Normalizer'

      if(is.empty(name)){name <<- 'SKNRM' %>% paste0(sample(10000:99999, 1))}
    }
))


#' @title Standard scaler (scikit-learn)
#' @name MAP.SKLEARN.ZFS-class
#' @aliases MAP.SKLEARN.ZFS
#' @description
#' Wraps \code{sklearn.preprocessing.StandardScaler}. Its arguments can be passed.
#' Requires Python with the module named below, reachable through reticulate. Set \code{python_address} to choose the Python executable; see \code{\link{python_setup}} and \code{vignette("rml-troubleshooting")} if the module is not found.
#'
#' Inherits from \code{\link{TRM.SKLEARN-class}}.
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = MAP.SKLEARN.ZFS()
#' m$fit(X, y)
#' head(m$predict(X))
#' }
#' @export MAP.SKLEARN.ZFS
MAP.SKLEARN.ZFS = setRefClass(
  'MAP.SKLEARN.ZFS',
  contains = 'TRM.SKLEARN',
  methods = list(
    initialize = function(...){
      callSuper(model.module = 'preprocessing', model.class = 'StandardScaler', ...)
      type             <<- 'Mapper'
      description      <<- 'Z-Factor Standard Scaler'

      if(is.empty(name)){name <<- 'SKZFS' %>% paste0(sample(10000:99999, 1))}
    }
))

#' @title Min-max scaler (scikit-learn)
#' @name MAP.SKLEARN.MMS-class
#' @aliases MAP.SKLEARN.MMS
#' @description
#' Wraps \code{sklearn.preprocessing.MinMaxScaler}. Its arguments can be passed.
#' Requires Python with the module named below, reachable through reticulate. Set \code{python_address} to choose the Python executable; see \code{\link{python_setup}} and \code{vignette("rml-troubleshooting")} if the module is not found.
#'
#' Inherits from \code{\link{TRM.SKLEARN-class}}.
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = MAP.SKLEARN.MMS()
#' m$fit(X, y)
#' head(m$predict(X))
#' }
#' @export MAP.SKLEARN.MMS
MAP.SKLEARN.MMS = setRefClass(
  'MAP.SKLEARN.MMS',
  contains = 'TRM.SKLEARN',
  methods = list(
    initialize = function(...){
      callSuper(model.module = 'preprocessing', model.class = 'MinMaxScaler', ...)
      type             <<- 'Mapper'
      description      <<- 'MinMax Scaler'
      if(is.empty(name)){name <<- 'SKMMS' %>% paste0(sample(10000:99999, 1))}
    }
))


#' @title Principal component analysis (stats::prcomp)
#' @name MAP.STATS.PCA-class
#' @aliases MAP.STATS.PCA
#' @description
#' Projects the numeric columns on their first principal components.
#'
#' Inherits from \code{\link{MODEL-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{num_components}}{Number of components returned. Default 5.}
#'   \item{\code{center}}{Center columns. Default TRUE.}
#'   \item{\code{scale}}{Scale columns to unit variance. Default TRUE.}
#' }
#' @examples
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = MAP.STATS.PCA(num_components = 2)
#' m$fit(X, y)
#' head(m$predict(X))
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
    XOUT = XOUT[, sequence(min(ncol(XOUT), config$num_components))] %>% as.data.frame
    colnames(XOUT) <- paste0('CMP', sequence(ncol(XOUT)))
    return(XOUT)
  }
))

# previously PYLMNN
#' @title Large margin nearest neighbour projection (pylmnn)
#' @name MAP.PYLMNN.LMNN-class
#' @aliases MAP.PYLMNN.LMNN
#' @description
#' Learns a linear projection with the Python package pylmnn.
#' Requires Python with the module named below, reachable through reticulate. Set \code{python_address} to choose the Python executable; see \code{\link{python_setup}} and \code{vignette("rml-troubleshooting")} if the module is not found.
#'
#' Inherits from \code{\link{MODEL-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{num_components}}{Output dimension. Default 5.}
#'   \item{\code{num_neighbors}}{Neighbours. Default 10.}
#'   \item{\code{epochs}}{Maximum iterations. Default 100.}
#' }
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = MAP.PYLMNN.LMNN(num_components = 2)
#' m$fit(X, y)
#' head(m$predict(X))
#' }
#' @export MAP.PYLMNN.LMNN
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
#' @title Identity mapper
#' @name MAP.RML.IDT-class
#' @aliases MAP.RML.IDT
#' @description
#' Returns its input columns unchanged. Use it as a transformer to give the parent model the raw columns alongside the
#' outputs of other transformers; combine with \code{features.include} to pass only some columns.
#'
#' Inherits from \code{\link{MODEL-class}}.
#' @examples
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = MAP.RML.IDT(features.include = 'a')
#' m$fit(X, y)
#' head(m$predict(X))
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
#' @title Min-max scaler
#' @name MAP.RML.MMS-class
#' @aliases MAP.RML.MMS
#' @description
#' Maps each numeric column to \code{(x - min)/(max - min)} with the training minimum and maximum. Missing and undefined values become 0.
#'
#' Inherits from \code{\link{MODEL-class}}.
#' @examples
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = MAP.RML.MMS()
#' m$fit(X, y)
#' head(m$predict(X))
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
                              objects$features$min <<- unname(apply(X, 2, min, na.rm = T))
                              objects$features$max <<- unname(apply(X, 2, max, na.rm = T))
                            },
                            
                            model.predict = function(X){
                              f = objects$features
                              scale_columns(X, f$fname, f$min, f$max - f$min)
                            }
                          ))

# previously SCALER
#' @title Z-factor (standard) scaler
#' @name MAP.RML.ZFS-class
#' @aliases MAP.RML.ZFS
#' @description
#' Maps each numeric column to \code{(x - mean)/sd} with the training mean and standard deviation. Missing and undefined values become 0.
#'
#' Inherits from \code{\link{MODEL-class}}.
#' @examples
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = MAP.RML.ZFS()
#' m$fit(X, y)
#' head(m$predict(X))
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
    objects$features$mean <<- unname(colMeans(X, na.rm = T))
    objects$features$stdv <<- unname(apply(X, 2, sd, na.rm = T))
  },
  
  model.predict = function(X){
    f = objects$features
    scale_columns(X, f$fname, f$mean, f$stdv)
  }
))

#' @title Quantile transformer (scikit-learn)
#' @name MAP.SKLEARN.QT-class
#' @aliases MAP.SKLEARN.QT
#' @description
#' Wraps \code{sklearn.preprocessing.QuantileTransformer}. Its arguments can be passed.
#' Requires Python with the module named below, reachable through reticulate. Set \code{python_address} to choose the Python executable; see \code{\link{python_setup}} and \code{vignette("rml-troubleshooting")} if the module is not found.
#'
#' Inherits from \code{\link{MODEL-class}}.
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = MAP.SKLEARN.QT(n_quantiles = 100L)
#' m$fit(X, y)
#' head(m$predict(X))
#' }
#' @export MAP.SKLEARN.QT
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
        objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
        X = X[objects$features$fname]
        
        module = reticulate::import('sklearn.preprocessing')
        objects$model <<- do.call(module$QuantileTransformer, config %>% list.remove(reserved_words))
        objects$model$fit(X %>% data.matrix, y)
      }
    },
    
    model.predict = function(X){
      out = objects$model$transform(X[objects$features$fname] %>% data.matrix) %>% as.data.frame()
      colnames(out) <- objects$features$fname
      out
    }
  ))

