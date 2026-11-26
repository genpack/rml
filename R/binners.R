#' @include transformers.R

# previously KMEANS
#' @title K-means cluster binner (stats::kmeans)
#' @name BIN.KMEANS.KMC-class
#' @aliases BIN.KMEANS.KMC
#' @description
#' Clusters the rows on the numeric columns with \code{kmeans} and returns the number of the nearest cluster centre.
#'
#' Inherits from \code{\link{MODEL-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{num_clusters}}{Number of clusters. Default 5.}
#' }
#' @examples
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = BIN.KMEANS.KMC(num_clusters = 3)
#' m$fit(X, y)
#' head(m$predict(X))
#' @export BIN.KMEANS.KMC
BIN.KMEANS.KMC = setRefClass('BIN.KMEANS.KMC', contains = 'MODEL', methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Binner'
    description      <<- 'KMeans Clustering'
    package          <<- 'rml'
    package_language <<- 'R'
    type     <<- ' from package kmeans'
    if(is.empty(name)){name <<- 'KMC' %>% paste0(sample(10000:99999, 1))}
    config$num_clusters <<- verify(config$num_clusters, c('numeric', 'integer'), default = 5)
    
  },
  
  model.fit = function(X, y = NULL){
    objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
    X = X[objects$features$fname]
    objects$model <<- kmeans(X, centers = config$num_cluster)
  },
  
  model.predict = function(X){
    # number of the nearest cluster centre (squared euclidean distance), vectorised over rows:
    M  = as.matrix(X[objects$features$fname])
    C  = objects$model$centers
    D2 = outer(rowSums(M^2), rowSums(C^2), '+') - 2*M %*% t(C)
    data.frame(cluster = max.col(-D2, ties.method = 'first'))
  }
))

# previously GROUPER
#' @title Categorical feature grouper
#' @name BIN.RML.GROUPER-class
#' @aliases BIN.RML.GROUPER
#' @description
#' Groups the values of categorical columns into fewer groups by their target rate.
#'
#' Inherits from \code{\link{MODEL-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{encoding}}{\code{'target_ratio'} (default) or \code{'flasso'}.}
#' }
#' @export BIN.RML.GROUPER
BIN.RML.GROUPER = setRefClass('BIN.RML.GROUPER', contains = "MODEL", methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Binner'
    description      <<- 'Categorical Feature Grouper'
    package          <<- 'rml'
    package_language <<- 'R'
    if(is.empty(name)){name <<- 'GRP' %>% paste0(sample(10000:99999, 1))}
    config$encoding <<- verify(config$encoding, 'character', domain =c('target_ratio', 'flasso'), default = 'target_ratio')
    #config$num_components <<- config$num_components %>% verify(c('numeric', 'integer'), default = 5) %>% as.integer
  },
  
  model.fit = function(X, y){
    objects$features <<- objects$features %>% filter(fclass %in% c('character','factor','logical','integer'))
    X = X[objects$features$fname]
    assert(ncol(X) > 0, 'No nominal features found!')
    objects$model <<- fit_map_new(X, y, objects$features$fname, encoding = config$encoding)
  },
  
  model.predict = function(X){
    predict_map(X, objects$model) %>% as.data.frame %>% {colnames(.) <- NULL;.}
  }
))


# This model, finds all numeric features in X and uses smbinning R package to optimally bin them to categorical columns and returns the table with additional features
# original features are NOT maintained in the output table
# This model is using stupid package smbinning. It only divides to 5 buckets, so it's not really optimal binning but better than nothing!
# Also, make sure there is no '.' character in column labels because the package author has problem with dots!!!!!!
# previously SMBINNING
#' @title Optimal binner (smbinning)
#' @name BIN.SMBINNING.OB-class
#' @aliases BIN.SMBINNING.OB
#' @description
#' Bins each numeric column with \code{smbinning::smbinning} (conditional inference trees).
#'
#' Inherits from \code{\link{MODEL-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{percentageOfRecords}}{Minimum share of rows per bin (0 to 0.5). Default 0.05.}
#' }
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = BIN.SMBINNING.OB()
#' m$fit(X, y)
#' head(m$predict(X))
#' }
#' @export BIN.SMBINNING.OB
BIN.SMBINNING.OB = setRefClass('BIN.SMBINNING.OB', contains = "MODEL",
   methods = list(
     initialize = function(...){
       callSuper(...)
       # refer to help page for package smbinning (?smbinning::smbinning)
       config$percentageOfRecords <<- verify(config$percentageOfRecords, 'numeric', domain = c(0, 0.5), default = '0.05')
       config$suffix <<- verify(config$suffix, 'character', default = 'BIN')
       type          <<- 'Binner'
       description   <<- 'Optimal Binner'
       package       <<- 'smbinning'
       package_language <<- 'R'
       packages_required <<- c(packages_required, 'smbinning')
       if(is.empty(name)){name <<- 'SMB' %>% paste0(sample(10000:99999, 1))}
     },
     
     model.fit = function(X, y){
       library(smbinning)
       objects$features <<- objects$features %>% filter(fclass == 'numeric')
       X = X[objects$features$fname]
       
       objects$model <<- list()
       # This model currently only works for categorical y
       if(inherits(y, 'character')){y %<>% as.factor %>% as.integer}
       if(inherits(y, c('logical', 'numeric'))){y %<>% as.integer}
       
       columns = numerics(X)
       if(length(columns) > 0){
         ds = cbind(X[, columns], Y = y)
         for(col in columns){
           res = try(smbinning::smbinning(ds, y = 'Y', x = col), silent = T)
           
           if(inherits(res, 'list')){
             ns = names(objects$model)
             res$col_id = length(objects$model) + 1
             objects$model <<- c(objects$model, list(res))
             names(objects$model) <<- c(ns, col)
             cat('Column ', col, ': Successfully binned to 5 buckets!', '\n')
           } else {
             cat('Column ', col, ': ', res, '\n')
           }
         }
       }
       objects$features <<- data.frame(fname = columns, stringsAsFactors = F)
       
       # Remove ctree objects to save space (They are not needed for prediction):
       config$keep_ctrees <<- verify(config$keep_ctrees, 'logical', lengths = 1, domain = c(T,F), default = F)
       
       for(i in sequence(nrow(objects$features))){
         mdl = objects$model[[objects$features$fname[i]]]
         if(!is.null(mdl)){
           objects$features[i, 'importance'] <<- mdl$iv
           if(!config$keep_ctrees){
             objects$model[[objects$features$fname[i]]]$ctree <<- NULL
           }
         }
       }
     },
     
     # predict acts like transformer
     model.predict = function(X){
       ns    = names(objects$model)
       ds    = X[, ns]
       for(ft in ns){
         ds  %<>% smbinning::smbinning.gen(objects$model[[ft]], chrname = ft %>% paste(config$suffix, sep = '.'))
       }
       columns = colnames(ds) %>% setdiff(colnames(X))
       ds[, columns, drop = F]
     }
   )
)


#' @title Optimal binary binner
#' @name BIN.RML.OBB-class
#' @aliases BIN.RML.OBB
#' @description
#' Splits each numeric column at the cut point that maximises the chi-square association with the binary target and
#' returns 0/1 per column. Importance is the strength of that association.
#'
#' Inherits from \code{\link{MODEL-class}}.
#' @examples
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = BIN.RML.OBB()
#' m$fit(X, y)
#' head(m$predict(X))
#' @export BIN.RML.OBB
BIN.RML.OBB = setRefClass('BIN.RML.OBB', contains = "MODEL", methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Binner'
    description      <<- 'Optimal Binary Binner'
    package          <<- 'rml'
    package_language <<- 'R'
    
    # refer to help page for package smbinning (?smbinning::smbinning)
    config$suffix    <<- verify(config$suffix, 'character', default = 'BIN')
    config$basis     <<- verify(config$basis, 'character', domain = c('f1', 'chi'), default = 'chi')
    if(is.empty(name)){name <<- 'OBB' %>% paste0(sample(10000:99999, 1))}
  },
  
  model.fit = function(X, y){
    objects$features <<- objects$features %>% filter(fclass == 'numeric')
    X = X[objects$features$fname]
    if(length(objects$features$fname) > 0){
      ds = cbind(X, Y = y)
      objects$model <<- ds %>% optSplitColumns.chi(objects$features$fname, label_col = 'Y') %>% mutate(Column = as.character(Column))
      # objects$model <<- ds %>% optSplitColumns.f1(objects$features$fname, label_col = 'Y') %>% mutate(Column = as.character(Column))
      # todo: use 'metric' property of the config (must be class function) and change default functions for f1 and chi in mltools to return same column names
    }
    objects$features$importance <<- objects$model$correlation %>% vect.map(0,1)
  },
  
  # predict acts like transformer
  model.predict = function(X){
    X %>% ncol %>% sequence %>% sapply(function(i) as.integer(X[,i] > objects$model$split[i])) %>% as.data.frame %>% {colnames(.)<-NULL;.}
  }
))


#' @title K-means cluster binner (scikit-learn)
#' @name BIN.SKLEARN.KMC-class
#' @aliases BIN.SKLEARN.KMC
#' @description
#' Wraps \code{sklearn.cluster.KMeans} on the numeric columns and returns the cluster label of each row.
#' Requires Python with the module named below, reachable through reticulate. Set \code{python_address} to choose the Python executable; see \code{\link{python_setup}} and \code{vignette("rml-troubleshooting")} if the module is not found.
#'
#' Inherits from \code{\link{TRM.SKLEARN-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{n_clusters}}{Number of clusters. Default 3.}
#' }
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = BIN.SKLEARN.KMC(n_clusters = 4)
#' m$fit(X, y)
#' head(m$predict(X))
#' }
#' @export BIN.SKLEARN.KMC
BIN.SKLEARN.KMC = setRefClass(
  'BIN.SKLEARN.KMC',
  contains = 'TRM.SKLEARN',
  methods = list(
    initialize = function(...){
      callSuper(model.module = 'cluster', model.class = 'KMeans', ...)
      type             <<- 'Binner'
      description      <<- 'KMeans Clustering'

      if(is.empty(name)){name <<- 'KMC' %>% paste0(sample(10000:99999, 1))}
      config$n_clusters <<- verify(config$n_clusters, c('integer', 'numeric'), lengths = 1, domain = c(1,1000), default = 3) %>% as.integer
    },

    model.fit = function(X, y = NULL){
      objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
      callSuper(X[objects$features$fname], y)
    },

    # Returns the cluster number of each row
    model.predict = function(X){
      if(inherits(X, 'WIDETABLE')){X = wt_as_matrix(X)}
      retrieve_model()
      data.frame(cluster = as.integer(objects$model$predict(X %>% data.matrix)))
    }
))
