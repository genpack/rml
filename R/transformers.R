
TRANSFORMER = setRefClass('TRANSFORMER', contains = "MODEL", methods = list(
  
  predict = function(X){
    XORG = callSuper(X) %>% as.data.frame
    XFET = XORG[objects$features$fname]
    XOUT = .self$model.predict(XFET)
    if(is.null(colnames(XOUT))){
      if(ncol(XOUT) == nrow(objects$features)){
        colnames(XOUT) <- name %>% paste(objects$features$fname, sep = '_')
      } else if (ncol(XOUT) == 1){
        colnames(XOUT) <- name %>% paste('out', sep = '_')
      } else{
        colnames(XOUT) <- name %>% paste('out', sequence(ncol(XOUT)), sep = '_')
      }
    } else {
      colnames(XOUT) <- name %>% paste(colnames(XOUT), sep = '_')
    }
    
    treat(XOUT, XFET, XORG)
  }
  
))

# This model, finds all numeric features in X and uses smbinning R package to optimally bin them to categorical columns and returns the table with additional features
# original features are NOT maintained in the output table
# This model is using stupid package smbinning. It only divides to 5 buckets, so it's not really optimal binning but better than nothing!
# Also, make sure there is no '.' character in column labels because the package author has problem with dots!!!!!!
#' @export SMBINNING
SMBINNING = setRefClass('SMBINNING', contains = "TRANSFORMER",
    methods = list(
      initialize = function(...){
        callSuper(...)
        # refer to help page for package smbinning (?smbinning::smbinning)
        config$percentageOfRecords <<- config$percentageOfRecords %>% verify('numeric', domain = c(0, 0.5), default = '0.05')
        config$suffix              <<- config$suffix %>% verify('character', default = 'BIN')
        type                       <<- 'Optimal Binner'
        if(is.empty(name)){name <<- 'SMBIN' %>% paste0(sample(1000:9999, 1))}
      },

      model.fit = function(X, y){
          objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
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
          # todo: add importance
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

#' @export NORMALIZER
NORMALIZER = setRefClass('NORMALIZER', contains = 'TRANSFORMER',
  methods = list(
    initialize = function(...){
      callSuper(...)
      config$suffix <<- config$suffix %>% verify('character', default = 'NRM')
      type          <<- 'Normalizer'
      if(is.empty(name)){name <<- 'NRMR' %>% paste0(sample(1000:9999, 1))}
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

#' @export SCALER
SCALER = setRefClass('SCALER', contains = "TRANSFORMER", methods = list(
  initialize = function(...){
    callSuper(...)
    config$suffix              <<- config$suffix %>% verify('character', default = 'SCALED')
    type                       <<- 'ZFactor Scaler'
    if(is.empty(name)){name <<- 'SCLR' %>% paste0(sample(1000:9999, 1))}
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

#' @export OPTBINNER
OPTBINNER = setRefClass('OPTBINNER', contains = "TRANSFORMER", methods = list(
  initialize = function(...){
    callSuper(...)
    # refer to help page for package smbinning (?smbinning::smbinning)
    config$suffix              <<- config$suffix %>% verify('character', default = 'BIN')
    config$basis               <<- config$basis %>% verify('character', domain = c('f1', 'chi'), default = 'chi')
    type                       <<- 'Optimal Binner'
    if(is.empty(name)){name <<- 'OBIN' %>% paste0(sample(1000:9999, 1))}
  },

  model.fit = function(X, y){
      objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
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

# Replaces categorical features with class ratios associated with each category
#' @export SEGMENTER.RATIO
SEGMENTER.RATIO = setRefClass('SEGMENTER.RATIO', contains = 'TRANSFORMER',
   methods = list(
     initialize = function(...){
       callSuper(...)
       type     <<- 'Class-Ratio Segmenter'
       if(is.empty(name)){name <<- 'SEGRAT' %>% paste0(sample(1000:9999, 1))}

     },

     model.fit = function(X, y){
       if(!fitted){
         objects$features <<- objects$features %>% filter(fname %in% nominals(X))
         X = X[objects$features$fname]
         objects$model <<- list()
         for(col in colnames(X)){
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

#' @export SEGMENTER.MODEL
SEGMENTER.MODEL = setRefClass('SEGMENTER.MLR', contains = 'TRANSFORMER',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type     <<- 'Model Segmenter'
      config$model_class  <<- config$model_class %>% verify('character', default = 'SCIKIT.XGB')
      config$model_config <<- config$model_config %>% verify('list', default = list(predict_probabilities = T))
      config$min_rows     <<- config$min_rows %>% verify(c('numeric', 'integer'), lengths = 1, default = 25)
      if(is.empty(name)){name <<- 'SEGMOD' %>% paste0(sample(1000:9999, 1))}
    },

    model.fit = function(X, y){
        objects$categoricals <<- nominals(X)
        #X = X[objects$categoricals]
        objects$model <<- list()
        for(col in objects$categoricals){
          objects$model[[col]] <<- list()
          Xcol = X %>% pull(col)
          uval = Xcol %>% unique
          objects$model[[col]][['__global__']] <<- new(config$model_class, config = config$model_config)
          for(val in uval){
            vlc = as.character(val)
            www = which(Xcol == val)
            if(length(www) > config$min_rows){
              objects$model[[col]][[vlc]] <<- new(config$model_class, config = config$model_config)
              objects$model[[col]][[vlc]]$fit(X[www,], y[www])
            } else {
              objects$model[[col]][['__global__']]$fit(X %>% spark.unselect(col), y)
            }
          }
        }
    },

    model.predict = function(X){
      XOUT  = NULL

      for(col in objects$categoricals){
        bibi = function(dot){
          dot %<>% as.data.frame
          NNN = nrow(dot)
          mdl = objects$model[[col]][[dot[1,col] %>% as.character]]
          if(is.null(mdl)) {mdl = objects$model[[col]][['__global__']]}
          data.frame(value = mdl$predict(dot), stringsAsFactors = F)
        }
        cn = 'value' %>% {names(.) <- name %>% paste(col, sep = '_');.}
        df = X %>% group_by_(col) %>% do({bibi(.)}) %>% spark.rename(cn)
        if(is.null(XOUT)){XOUT = df[,2, drop = F]} else {XOUT %<>% cbind(df[,2, drop = F])}
      }
      colnames(XOUT) <- NULL
      return(XOUT)
    }
  ))

#' @export CATCONCATER
CATCONCATER = setRefClass('CATCONCATER', contains = "TRANSFORMER",
   methods = list(
     initialize = function(...){
       callSuper(...)
       type     <<- 'Categorical Features Concater'
       if(is.empty(name)){name <<- 'CATCON' %>% paste0(sample(1000:9999, 1))}
     },

     model.fit = function(X, y){
         objects$features <<- data.frame(fname = nominals(X), stringsAsFactors = F)
     },

     model.predict = function(X){
       X %>% apply(1, function(x) paste(x, collapse = '-')) %>% as.data.frame
     }
   )
)

       
LOGGER = setRefClass('LOGGER', contains = "TRANSFORMER", methods = list(
  initialize = function(...){
    callSuper(...)
    type     <<- 'Logger Transformer'
    if(is.empty(name)){name <<- 'LOG' %>% paste0(sample(1000:9999, 1))}
    config$apply_abs <<- config$apply_abs %>% verify(c('logical'), default = T)
    config$intercept <<- config$intercept %>% verify(c('numeric'), default = 1)
  },
  
  model.fit = function(X, y){
    objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
    X = X[objects$features$fname]
  },
  
  model.predict = function(X){
    if(config$apply_abs){X = abs(X)}
    XOUT = log(X + config$intercept)
    return(XOUT)
  }
))

#' @export DUMMIFIER
DUMMIFIER = setRefClass('DUMMIFIER', contains = "TRANSFORMER",
   methods = list(
     initialize = function(...){
       callSuper(...)
       type     <<- 'Categorical Feature Dummifier'
       if(is.empty(name)){name <<- 'DMFR' %>% paste0(sample(1000:9999, 1))}
     },

     model.fit = function(X, y = NULL){
       objects$features     <<- objects$features %>% filter(fname %in% nominals(X))
       
       warnif(is.empty(objects$features), 'No categorical columns found!')
       dummies = character(); nbin = character()
       for(cat in objects$features %>% pull(fname)){
         uval = unique(X %>% pull(cat))
         if(length(uval) > 2){
           nbin = c(nbin, cat)              
           dummies %<>% c(cat %>% paste(uval, sep = '_'))
         }               
       }
       objects$features     <<- objects$features %>% filter(fname %in% nbin)
       warnif(is.empty(objects$features), 'No categorical columns with more than two unique values found!')
       
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

#' @export GENETIC.BOOSTER.GEOMETRIC
GENETIC.BOOSTER.GEOMETRIC = setRefClass('GENETIC.BOOSTER.GEOMETRIC', contains = 'TRANSFORMER',
    methods = list(
      initialize = function(...){
        callSuper(...)
        if(is.null(config$num_top_features)) {config$num_top_features <<- 1}
        if(is.null(config$epochs)) {config$epochs <<- 10}
        if(is.null(config$cycle_births)) {config$cycle_births <<- 1000}
        if(is.null(config$cycle_survivors)) {config$cycle_survivors <<- 100}
        if(is.null(config$final_survivors)) {config$final_survivors <<- 1}
        if(is.null(config$metric)){config$metric <<- cor}
      },

      getFeatureValue = function(flist, fname, dataset){
        if(fname %in% colnames(dataset)){return(dataset[, fname])}
        if(fname %in% rownames(flist)){
          if(flist[fname, 'father'] %in% names(dataset)) {father = dataset[, flist[fname, 'father']]} else {father = getFeatureValue.multiplicative(flist, flist[fname, 'father'], dataset)}
          if(flist[fname, 'mother'] %in% names(dataset)) {mother = dataset[, flist[fname, 'mother']]} else {mother = getFeatureValue.multiplicative(flist, flist[fname, 'mother'], dataset)}
          return(father*mother)
        } else {stop('Feature fname is not in the list!')}
      },

      # nf features are born by random parents:
      createFeatures = function(flist, prefix = 'F'){
        features = rownames(flist)
        flist %>% rbind(
          data.frame(
            fname  = prefix %>% paste0(nrow(flist) + sequence(config$cycle_births)),
            father = features %>% sample(config$cycle_births, replace = T),
            mother = features %>% sample(config$cycle_births, replace = T),
            correlation = NA,
            safety = 0, stringsAsFactors = F) %>% column2Rownames('fname'))
      },
      
      evaluateFeatures = function(X, y){
        cor_fun = config$metric
        top     = config$cycle_survivors
        columns = colnames(X)
        flist   = objects$model
        ns      = rownames(flist)
        
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
        objects$model <<- flist[keep, ]
      },
      
      model.predict = function(X){
        top = objects$model %>% rownames2Column('fname') %>% distinct(correlation, .keep_all = T) %>%
          arrange(desc(correlation)) %>% head(config$num_top_features)
        XOUT = NULL
        for (i in top %>% nrow %>% sequence){
          XOUT %<>% cbind(getFeatureValue(objects$model, top$fname[i], X))
        }
        colnames(XOUT) <- top$fname
        return(XOUT)
      },
      

      model.fit = function(X, y){
          cor_fun = config$metric
          objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
          X = X[objects$features$fname]
          objects$model <<- data.frame(fname = objects$features$fname, father = NA, mother = NA, correlation = cor_fun(X, y) %>% as.numeric %>% abs, safety = 0) %>% column2Rownames('fname')
          objects$fdata <<- X[objects$features$fname]

          i = 0
          while(i < config$epochs){
            i = i + 1
            objects$model <<- createFeatures(objects$model)
            evaluateFeatures(X = X[objects$features$fname], y = y)

            cat('Iteration:', i, ': Best Correlation = ', 100*max(objects$model$correlation), ' nrow(flist) = ', nrow(objects$model), '\n')
          }
      }
      
      # todo: add model.predict
    )
)

#' @export GENETIC.BOOSTER.LOGICAL
GENETIC.BOOSTER.LOGICAL = setRefClass('GENETIC.BOOSTER.LOGICAL', contains = 'TRANSFORMER', methods = list(
  initialize = function(...){
    callSuper(...)
    if(is.null(config$metric)){config$metric <<- cross_accuracy}
    if(is.null(config$num_top_features)) {config$num_top_features <<- 1}
    if(is.null(config$epochs)) {config$epochs <<- 10}
    if(is.null(config$cycle_births)) {config$cycle_births <<- 1000}
    if(is.null(config$cycle_survivors)) {config$num_top_features <<- 100}
    if(is.null(config$final_survivors)) {config$final_survivors <<- 1}
  },
  
  getFeatureValue = function(fname, dataset){
    if(!fitted) stop(paste('from', fname, 'of type', type, ':', 'Model not fitted!', '\n'))
    getFeatureValue.logical(objects$model, fname, dataset)
  },
  
  model.fit = function(X, y){
    objects$model    <<- genBinFeatBoost.fit(X, y, target = 0.9, epochs = config$epochs, cycle_survivors = config$cycle_survivors, final_survivors = config$final_survivors, cycle_births = config$cycle_births, metric = config$metric)
    objects$features <<- data.frame(fname = objects$model$father %U% objects$model$father, stringsAsFactors = F)
  },
  
  model.predict = function(X){
    top = objects$model %>% rownames2Column('fname') %>% distinct(correlation, .keep_all = T) %>%
      arrange(desc(correlation)) %>% head(config$num_top_features)
    XOUT = NULL
    for (i in top %>% nrow %>% sequence){
      XOUT %<>% cbind(getFeatureValue(top$fname[i], X))
    }
    colnames(XOUT) <- top$fname
    return(XOUT)
  }
))

#' @export SUPERVISOR
SUPERVISOR = setRefClass('SUPERVISOR', contains = "TRANSFORMER",
  methods = list(
    initialize = function(...){
      callSuper(...)
      type     <<- 'Model Supervisor'
      if(is.empty(name)){name <<- 'SUP' %>% paste0(sample(1000:9999, 1))}
      config$model_class  <<- config$model_class  %>% verify('character', default = 'SCIKIT.XGB')
      config$model_config <<- config$model_config %>% verify('character', default = list())
      objects$model <<- new(config$model_class, config = config$model_config)
    },

    model.fit = function(X, y){
        Xt = X %>% spark.unselect(objects$pupils)
        pp = X %>% spark.select(objects$pupils) %>% as.matrix
        pp %>% apply(1, which.max) -> highest
        pp %>% apply(1, which.min) -> lowest

        yt = ifelse(y, highest, lowest) - 1
        objects$model$fit(Xt, yt)
    },

    model.predict = function(X){
      pp   = X %>% spark.select(objects$pupils)
      yout = objects$model$predict(X) %>% as.matrix
      pp %>% nrow %>% sequence %>% sapply(function(i) pp[i, yout[i,1] + 1]) %>% as.data.frame
    }
  )
)

#' @export KMEANS
KMEANS = setRefClass('KMEANS', contains = 'TRANSFORMER', methods = list(
  initialize = function(...){
    callSuper(...)
    type     <<- 'KMeans Clustering Transformer'
    if(is.empty(name)){name <<- 'KMNS' %>% paste0(sample(1000:9999, 1))}
    config$num_clusters <<- config$num_clusters %>% verify(c('numeric', 'integer'), default = 5)

  },
  model.fit = function(X, y = NULL){
      objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
      X = X[objects$features$fname]
      objects$model <<- kmeans(X, centers = config$num_cluster)
  },

  model.predict = function(X){
    bibi = function(u){
      objects$model$centers %>% apply(1, function(v) gener::difference(u, v)) %>% order %>% first
    }
    X %>% as.matrix %>% apply(1, bibi) %>% as.data.frame %>% {colnames(.)<-NULL;.}
  }
))

#' @export PRCOMP
PRCOMP = setRefClass('PRCOMP', contains = 'TRANSFORMER', methods = list(
  initialize = function(...){
    callSuper(...)
    type     <<- 'Principal Component Transformer'
    if(is.empty(name)){name <<- 'PRC' %>% paste0(sample(1000:9999, 1))}
    config$num_components <<- config$num_components %>% verify(c('numeric', 'integer'), default = 5)
    config$scale  <<- config$scale  %>% verify('logical', domain = c(F,T), default = T)
    config$center <<- config$center %>% verify('logical', domain = c(F,T), default = T)
  },

  model.fit = function(X, y = NULL){
      objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
      X = X[objects$features$fname]
      objects$model <<- prcomp(X, center = config$center, scale. = config$scale, rank. = config$rank)
  },

  model.predict = function(X){
    XOUT = stats::predict(objects$model, X %>% as.matrix)
    XOUT = XOUT[,sequence(min(ncol(XOUT), config$num_components))] %>% as.data.frame
    colnames(XOUT) <- name %>% paste(colnames(XOUT), sep = '_')
    return(XOUT)
  }
))

PYLMNN = setRefClass('PYLMNN', contains = "TRANSFORMER", methods = list(
  initialize = function(...){
    callSuper(...)
    type     <<- 'Large Margin Nearest Neighbors'
    if(is.empty(name)){name <<- 'LMNN' %>% paste0(sample(1000:9999, 1))}
    config$num_components <<- config$num_components %>% verify(c('numeric', 'integer'), default = 5) %>% as.integer
    config$num_neighbors  <<- config$neighbors %>% verify(c('numeric', 'integer'), default = 10) %>% as.integer
    config$epochs         <<- config$epochs %>% verify(c('numeric', 'integer'), default = 100) %>% as.integer
    lmnn_module   <-  reticulate::import('pylmnn')
    objects$model <<- lmnn_module$LargeMarginNearestNeighbor(n_neighbors = config$num_neighbors, max_iter = config$epochs, n_components = config$num_components, init = 'pca')
  },
  
  model.fit = function(X, y){
      objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
      X = X[objects$features$fname]
      objects$model$fit(X %>% data.matrix, y)
  },
  
  model.predict = function(X){
    XOUT = objects$model$transform(XF %>% data.matrix) %>% as.data.frame
    colnames(XOUT) <- name %>% paste(colnames(XOUT), sep = '_')
    return(XOUT)
  }
))

GROUPER = setRefClass('GROUPER', contains = "TRANSFORMER", methods = list(
  initialize = function(...){
    callSuper(...)
    type     <<- 'Categorical Feature Grouper'
    if(is.empty(name)){name <<- 'GRP' %>% paste0(sample(1000:9999, 1))}
    #config$num_components <<- config$num_components %>% verify(c('numeric', 'integer'), default = 5) %>% as.integer
  },
  
  model.fit = function(X, y){
      objects$features <<- objects$features %>% filter(fclass %in% c('character','factor','logical', 'integer'))
      X = X[objects$features$fname]
      # objects$model <<- fit_map_new(X, y, objects$features$fname)
      objects$model <<- fit_map(X, y, objects$features$fname)
  },
  
  model.predict = function(X){
    predict_map(X, objects$model) %>% as.data.frame %>% {colnames(.)<-NULL;.}
  }
))
