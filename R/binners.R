#' @include transformers.R

# previously KMEANS
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
    bibi = function(u){
      objects$model$centers %>% apply(1, function(v) rutils::difference(u, v)) %>% order %>% first
    }
    X %>% as.matrix %>% apply(1, bibi) %>% as.data.frame %>% {colnames(.)<-NULL;.}
  }
))

# previously GROUPER
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


BIN.SKLEARN.KMC = setRefClass(
  'BIN.SKLEARN.KMC',
  contains = 'MODEL',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Binner'
      description      <<- 'KMeans Clustering'
      package          <<- 'sklearn'
      package_language <<- 'Python'
      
      if(is.empty(name)){name <<- 'KMC' %>% paste0(sample(10000:99999, 1))}
      config$n_clusters <<- verify(config$n_clusters, c('integer', 'numeric'), lengths = 1, domain = c(1,1000), default = 3)
    },
    
    model.fit = function(X, y){
      if(!fitted){
        objects$features <<- objects$features %>% filter(fname %in% c('numeric', 'integer'))
        X = X[objects$features$fname]
        
        module = reticulate::import('sklearn.cluster')
        objects$model <<- do.call(module$k_means, config %>% list.remove(rml_words))
        objects$model$fit(X, y)
      }
    },
    
    model.predict = function(X){
      objects$model$transform(X)
    }
  ))

