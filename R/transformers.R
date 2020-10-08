
# TRANSFORMER = setRefClass('TRANSFORMER', contains = "MODEL", methods = list(
#
#
#   # predict = function(X){
#   #   if(is.null(colnames(XOUT))){
#   #     if(ncol(XOUT) == nrow(objects$features)){
#   #       colnames(XOUT) <- name %>% paste(objects$features$fname, sep = '_')
#   #     } else if (ncol(XOUT) == 1){
#   #       colnames(XOUT) <- name
#   #     } else{
#   #       colnames(XOUT) <- name %>% paste(sequence(ncol(XOUT)), sep = '_')
#   #     }
#   #   } else {
#   #     colnames(XOUT) <- name %>% paste(colnames(XOUT), sep = '_')
#   #   }
#   #
#   #   treat(XOUT, XFET, XORG)
#   # }
#
# ))


## Legend for transformers

# CLS: Classifiers
  # XGB: XGBoost
  # LR:  Logistic Regression
  # DT:  Decision Tree
  # KNN: K Nearest Neighbours
  # DNN: Deep Neural Net
  # SVM: Support Vector Machine
  # NB: Naive Bayes
  # GBM: Gradient Boost Model


# REG: Regressors
  # LM: Linear Model (Linear Regrtession)
  # RT: Regression Tree
  # XGB: XGBoost
  # DNN: Deep Neural Net

# FET: Feature Engineering Transformers (Booster):
#   D2MUL: Degree 2 Multiplier
#   D2MULC: Degree 2 Multiplier with Feature Selector
#
# FNT: Function transformer
#   INV: Inverser
#   LOG: Logarithm
#   POLY: Polynomial
#
# ENC: Encoders:
#   OHE: One Hot Encoder
#   TE: Target Encoder
#   HLMRT: Helmert Encoder
#   CATB: Catboost Encoder
#   JSTN: James Stein Encoder
#
# BIN: Binner Transformers:
#   OBB: Optimla Binary Binner
#   OB: Optimal Binner
#   KM: KMeans Clustering
#   SKM: Spherical KMeans Clustering
#   HCL: Hierarchical Clustering
#
# MAP: One to one mappers:
#   IDT: Identity Transformer
#   RNK: ranker
#   QT: Quantile Transformer
#   MMS: MinMaxScaler (Normalizer)
#   ZFS: Z Factor Scaler
#   PCA: Principal Component Analysis Mapper
#   NRM: Normalizer

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

# previously NORMALIZER
#' @export MAP.MALER.MMS
MAP.MALER.MMS = setRefClass('MAP.MALER.MMS', contains = 'MODEL',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Mapper'
      description      <<- 'MinMax Scaler'
      package          <<- 'maler'
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
#' @export MAP.MALER.ZFS
MAP.MALER.ZFS = setRefClass('MAP.MALER.ZFS', contains = "MODEL", methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Mapper'
    description      <<- 'ZFactor Standard Scaler'
    package          <<- 'maler'
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

#' @export BIN.MALER.OBB
BIN.MALER.OBB = setRefClass('BIN.MALER.OBB', contains = "MODEL", methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Binner'
    description      <<- 'Optimal Binary Binner'
    package          <<- 'maler'
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

#' @export ENC.CATEGORY_ENCODERS.HLMRT
ENC.CATEGORY_ENCODERS.HLMRT = setRefClass(
  'ENC.CATEGORY_ENCODERS.HLMRT',
  contains = 'MODEL',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Encoder'
      description      <<- 'Helmert Encoder'
      package          <<- 'category_encoders'
      package_language <<- 'Python'

      if(is.empty(name)){name <<- 'HLMRT' %>% paste0(sample(10000:99999, 1))}
    },

    model.fit = function(X, y){
      if(!fitted){
        objects$features <<- objects$features %>% filter(fname %in% nominals(X))
        X = X[objects$features$fname]

        module = reticulate::import('category_encoders')
        objects$model <<- do.call(module$helmert$HelmertEncoder,
                                  config %>% list.remove(maler_words) %>% list.add(cols = objects$features$fname))
        objects$model$fit(X, y)
      }
    },

    model.predict = function(X){
      objects$model$transform(X)
    }
  ))

MAP.SCIKIT.QT = setRefClass(
  'MAP.SCIKIT.QT',
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
        objects$model <<- do.call(module$QuantileTransformer, config %>% list.remove(maler_words))
        objects$model$fit(X, y)
      }
    },

    model.predict = function(X){
      objects$model$transform(X)
    }
  ))

BIN.SCIKIT.KMC = setRefClass(
  'BIN.SCIKIT.KMC',
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
        objects$model <<- do.call(module$k_means, config %>% list.remove(maler_words))
        objects$model$fit(X, y)
      }
    },

    model.predict = function(X){
      objects$model$transform(X)
    }
  ))

FET.SCIKIT.MFG = setRefClass(
  'MAP.SCIKIT.NRM',
  contains = 'MODEL',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Feature Generator'
      description      <<- 'Multiplicative Feature Generator'
      package          <<- 'sklearn'
      package_language <<- 'Python'

      if(is.empty(name)){name <<- 'MFG' %>% paste0(sample(10000:99999, 1))}
    },

    model.fit = function(X, y){
      if(!fitted){
        objects$features <<- objects$features %>% filter(fname %in% c('numeric', 'integer'))
        X = X[objects$features$fname]

        module = reticulate::import('sklearn.preprocessing')
        objects$model <<- do.call(PolynomialFeatures, config %>% list.remove(maler_words))
        objects$model$fit(X, y)
      }
    },

    model.predict = function(X){
      objects$model$transform(X)
    }
  ))

MAP.SCIKIT.NRM = setRefClass(
  'MAP.SCIKIT.NRM',
  contains = 'MODEL',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Mapper'
      description      <<- 'Normalizer'
      package          <<- 'sklearn'
      package_language <<- 'Python'

      if(is.empty(name)){name <<- 'NRM' %>% paste0(sample(10000:99999, 1))}
    },

    model.fit = function(X, y){
      if(!fitted){
        objects$features <<- objects$features %>% filter(fname %in% c('numeric', 'integer'))
        X = X[objects$features$fname]

        module = reticulate::import('sklearn.preprocessing')
        objects$model <<- do.call(module$Normalizer, config %>% list.remove(maler_words))
        objects$model$fit(X, y)
      }
    },

    model.predict = function(X){
      objects$model$transform(X)
    }
  ))


ENC.SCIKIT.OHE = setRefClass(
  'ENC.SCIKIT.OHE',
  contains = 'MODEL',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Encoder'
      description      <<- 'One-Hot Encoder'
      package          <<- 'sklearn'
      package_language <<- 'Python'
      type     <<-
      if(is.empty(name)){name <<- 'SKOHE' %>% paste0(sample(10000:99999, 1))}
      config$max_domain <<- verify(config$max_domain, c('numeric', 'integer'), default = 25) %>% as.integer
    },

    model.fit = function(X, y){
      if(!fitted){
        if(!'n_unique' %in% colnames(objects$features)){
          objects$features$n_unique <<- colnames(X) %>% sapply(function(x) X %>% pull(x) %>% unique %>% length) %>% unlist
        }
        objects$features <<- objects$features %>% filter(fname %in% nominals(X), n_unique <= config$max_domain)
        X = X[objects$features$fname]

        module = reticulate::import('sklearn.preprocessing')
        warnif(is.empty(objects$features), 'Either no categorical columns found or all categorical columns have too many distict values!', wrn_src = name)

        objects$model <<- do.call(module$OneHotEncoder, config %>% list.remove(maler_words))
        objects$model$fit(X, y)
      }
    },

    model.predict = function(X){
      objects$model$transform(X) %>% as.matrix %>% as.data.frame
    }
  ))

MAP.SCIKIT.ZFS = setRefClass(
  'MAP.SCIKIT.ZFS',
  contains = 'MODEL',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Mapper'
      description      <<- 'Z-Factor Standard Scaler'
      package          <<- 'sklearn'
      package_language <<- 'Python'
      if(is.empty(name)){name <<- 'ZFS' %>% paste0(sample(10000:99999, 1))}
    },

    model.fit = function(X, y){
      if(!fitted){
        objects$features <<- objects$features %>% filter(fname %in% c('numeric', 'integer'))
        X = X[objects$features$fname]

        module = reticulate::import('sklearn.preprocessing')
        objects$model <<- do.call(module$StandardScaler, config %>% list.remove(maler_words))
        objects$model$fit(X, y)
      }
    },

    model.predict = function(X){
      objects$model$transform(X)
    }
  ))

MAP.SCIKIT.MMS = setRefClass(
  'MAP.SCIKIT.MMS',
  contains = 'MODEL',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Mapper'
      description      <<- 'MinMax Scaler'
      package          <<- 'sklearn'
      package_language <<- 'Python'
      if(is.empty(name)){name <<- 'MMS' %>% paste0(sample(10000:99999, 1))}
    },

    model.fit = function(X, y){
      if(!fitted){
        objects$features <<- objects$features %>% filter(fname %in% c('numeric', 'integer'))
        X = X[objects$features$fname]

        module = reticulate::import('sklearn.preprocessing')
        objects$model <<- do.call(module$MinMaxScaler, config %>% list.remove(maler_words))
        objects$model$fit(X, y)
      }
    },

    model.predict = function(X){
      objects$model$transform(X)
    }
  ))


ENC.CATEGORY_ENCODERS.CATB = setRefClass(
  'ENC.CATEGORY_ENCODERS.CATB',
  contains = 'MODEL',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Encoder'
      description      <<- 'CatBoost Encoder'
      package          <<- 'category_encoders'
      package_language <<- 'Python'

      if(is.empty(name)){name <<- 'CATB' %>% paste0(sample(10000:99999, 1))}
    },

    model.fit = function(X, y){
      if(!fitted){
        objects$features <<- objects$features %>% filter(fname %in% nominals(X))
        X = X[objects$features$fname]

        module = reticulate::import('category_encoders')
        objects$model <<- do.call(module$cat_boost$CatBoostEncoder,
                                  config %>% list.remove(maler_words) %>% list.add(cols = objects$features$fname))
        objects$model$fit(X, y)
      }
    },

    model.predict = function(X){
      objects$model$transform(X)
    }
  ))

ENC.CATEGORY_ENCODERS.JSTN = setRefClass(
  'ENC.CATEGORY_ENCODERS.JSTN',
  contains = 'MODEL',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Encoder'
      description      <<- 'James Stein Encoder'
      package          <<- 'category_encoders'
      package_language <<- 'Python'

      if(is.empty(name)){name <<- 'JSTNENC' %>% paste0(sample(10000:99999, 1))}
    },

    model.fit = function(X, y){
      if(!fitted){
        objects$features <<- objects$features %>% filter(fname %in% nominals(X))
        X = X[objects$features$fname]

        module = reticulate::import('category_encoders')
        objects$model <<- do.call(module$james_stein$JamesSteinEncoder,
                                  config %>% list.remove(maler_words) %>% list.add(cols = objects$features$fname))
        objects$model$fit(X, y)
      }
    },

    model.predict = function(X){
      objects$model$transform(X)
    }
  ))

ENC.MALER.FE = setRefClass(
  'ENC.MALER.FE', contains = 'MODEL',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Encoder'
      description      <<- 'Feature Encoder'
      package          <<- 'maler'
      package_language <<- 'R'

      config$aggregator <<- verify(config$aggregator, 'character', default = 'mean')

      if(is.empty(name)){name <<- 'FE' %>% paste0(sample(10000:99999, 1))}
    },

    model.fit = function(X, y = NULL){
      if(!fitted){
        catfigs = objects$features %>% filter(fclass %in% c('character', 'integer', 'factor')) %>% pull(fname) %>% intersect(colnames(X))
        if(!is.null(config$categoricals)) {catfigs = catfigs %^% config$categoricals}

        numfigs = objects$features %>% filter(fclass %in% c('numeric', 'integer')) %>% pull(fname) %>% intersect(colnames(X)) %>% setdiff(catfigs)
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
#' @export ENC.MALER.TE
ENC.MALER.TE = setRefClass('ENC.MALER.TE', contains = 'MODEL',
   methods = list(
     initialize = function(...){
       callSuper(...)
       type             <<- 'Encoder'
       description      <<- 'Target Encoder'
       package          <<- 'maler'
       package_language <<- 'R'

       if(is.empty(name)){name <<- 'TE' %>% paste0(sample(10000:99999, 1))}
     },

     model.fit = function(X, y){
       if(!fitted){
         objects$features <<- objects$features %>% filter(fname %in% nominals(X))
         X = X[objects$features$fname]
         stopifnot(ncol(X) > 0, 'No categorical feature for encoding')
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

# Todo: does not save joblib for Python segment models
#' @export ENC.MALER.ME
ENC.MALER.ME = setRefClass('ENC.MALER.ME', contains = 'MODEL',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Encoder'
      description      <<- 'Model Encoder'
      package          <<- 'maler'
      package_language <<- 'R'

      config$model_class  <<- verify(config$model_class, 'character', default = 'CLS.SCIKIT.XGB')
      config$model_config <<- verify(config$model_config, 'list', default = list(predict_probabilities = T))
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
          objects$model[['__global__']] <<- new(config$model_class, config = config$model_config)
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
              objects$model[[col]][[vlc]] <<- new(config$model_class, config = config$model_config)
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
ENC.MALER.MEB =
  setRefClass('ENC.MALER.MEB',
              contains = 'ENC.MALER.ME',

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

FNT.MALER.INV = setRefClass('FNT.MALER.INV', contains = 'MODEL', methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Function Transformer'
    description      <<- 'Geometric'
    package          <<- 'maler'
    package_language <<- 'R'

    if(is.empty(name)){name <<- 'INV' %>% paste0(sample(10000:99999, 1))}
    config$trim <<- verify(config$trim, 'numeric', lengths = 1, default = 10000)
  },

  model.fit = function(X, y = NULL){
    objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
  },

  model.predict = function(X){
    out = X[, c()]
    for(i in objects$features$fname){
      col = 1.0/X[, i]
      maxtrim = max(col[col <   config$trim])
      mintrim = min(col[col > - config$trim])

      out[, i] <- ifelse(col > config$trim, maxtrim, ifelse(col < - config$trim, mintrim, col))
    }
    return(out)
  }

))

# previously : MULTIPLIER.BINARY
FET.MALER.D2MUL = setRefClass('FET.MALER.D2MUL', contains = 'MODEL', methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Feature Generator'
    description      <<- 'Degree 2 Multiplier'
    package          <<- 'maler'
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

# previously : MULTIPLIER.BINARY.COMP
FET.MALER.D2MULC = setRefClass('FET.MALER.D2MULC', contains = 'FET.MALER.D2MUL', methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Feature Generator'
    description      <<- 'Degree 2 Multiplier Components'
    package          <<- 'maler'
    package_language <<- 'R'

    if(is.empty(name)){name <<- 'D2MC' %>% paste0(sample(10000:99999, 1))}
    config$num_components   <<- verify(config$num_components, c('numeric', 'integer'), default = 5)
    config$model_class      <<- verify(config$model_class, 'character' , default = 'SCIKIT.XGB')
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

FNT.MALER.POLY = setRefClass('FNT.MALER.POLY', contains = 'MODEL', methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Function Transformer'
    description      <<- 'Polynomial'
    package          <<- 'maler'
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
    colnames(XOUT) <- ns
    return(XOUT %>% as.data.frame)
  }

))



#' @export CATCONCATER
CATCONCATER = setRefClass('CATCONCATER', contains = "MODEL",
   methods = list(
     initialize = function(...){
       callSuper(...)
       type             <<- 'Feature Generator'
       description      <<- 'Categorical Features Concater'
       package          <<- 'maler'
       package_language <<- 'R'
       if(is.empty(name)){name <<- 'CATCON' %>% paste0(sample(10000:99999, 1))}
     },

     model.fit = function(X, y){
         objects$features <<- data.frame(fname = nominals(X), stringsAsFactors = F)
     },

     model.predict = function(X){
       X %>% apply(1, function(x) paste(x, collapse = '-')) %>% as.data.frame
     }
   )
)


FNT.MALER.LOG = setRefClass('FNT.MALER.LOG', contains = "MODEL", methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Function Transformer'
    description      <<- 'Logarythm'
    package          <<- 'maler'
    package_language <<- 'R'
    if(is.empty(name)){name <<- 'LOG' %>% paste0(sample(10000:99999, 1))}
    config$apply_abs <<- verify(config$apply_abs, c('logical'), default = T)
    config$intercept <<- verify(config$intercept, c('numeric'), default = 1)
  },

  model.fit = function(X, y){
    objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
  },

  model.predict = function(X){
    if(config$apply_abs){X = abs(X)}
    XOUT = log(as.matrix(X) + config$intercept)
    return(XOUT %>% as.data.frame)
  }
))

FNT.MALER.LOGIT = setRefClass('FNT.MALER.LOGIT', contains = "MODEL", methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Function Transformer'
    description      <<- 'Logit'
    package          <<- 'maler'
    package_language <<- 'R'
    if(is.empty(name)){name <<- 'LGT' %>% paste0(sample(10000:99999, 1))}
    config$apply_mms <<- verify(config$apply_mms, c('logical'), default = T)
  },

  model.fit = function(X, y){
    objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
  },

  model.predict = function(X){
    X = as.matrix(X)
    if(config$apply_mms){X = X %>% apply(2, vect.normalise)}
    X[X < .Machine$double.eps] <- .Machine$double.eps
    X[X > 1.0 - .Machine$double.eps] <- 1.0 - .Machine$double.eps
    XOUT = log(X/(1 - X))
    return(XOUT %>% as.data.frame)
  }
))

#' @export ENC.FASTDUMMIES.OHE
ENC.FASTDUMMIES.OHE = setRefClass('ENC.FASTDUMMIES.OHE', contains = "MODEL",
   methods = list(
     initialize = function(...){
       callSuper(...)
       type             <<- 'Encoder'
       description      <<- 'One Hot Encoder'
       package          <<- 'fastDummies'
       package_language <<- 'R'
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


# Previously GENETIC.BOOSTER.GEOMETRIC
#' @export FET.MALER.MGB
FET.MALER.MGB = setRefClass('FET.MALER.MGB', contains = 'MODEL',
    methods = list(
      initialize = function(...){
        type             <<- 'Feature Generator'
        description      <<- 'Multiplicative Genetic Booster'
        package          <<- 'maler'
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
        columns = colnames(X)
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

        flist %<>% immune(ns[ord[sequence(top)]], level = high_level, columns = colnames(X))

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

# previously GENETIC.BOOSTER.LOGICAL
#' @export GENETIC.BOOSTER.LOGICAL
FET.MALER.LGB = setRefClass('FET.MALER.LGB', contains = 'MODEL', methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Feature Generator'
    description      <<- 'Logical Genetic Booster'
    package          <<- 'maler'
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
    colnames(XOUT) <- top$fname
    return(XOUT %>% as.data.frame %>% numerize_columns)
  }
))

# previously ENS.MALER.BS
#' @export ENS.MALER.BS
ENS.MALER.BS = setRefClass('ENS.MALER.BS', contains = "MODEL",
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Ensembler'
      description      <<- 'Binary Supervisor Ensembler'
      package          <<- 'maler'
      package_language <<- 'R'

      if(is.empty(name)){name <<- 'BSUP' %>% paste0(sample(10000:99999, 1))}
      config$model_class  <<- verify(config$model_class, 'character', default = 'CLS.SCIKIT.XGB')
      config$model_config <<- verify(config$model_config, 'character', default = list())
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

#' @export ENS.MALER.AGGR
ENS.MALER.AGGR = setRefClass('ENS.MALER.AGGR', contains = "CLASSIFIER",
   methods = list(
     initialize = function(...){
       callSuper(...)
       type             <<- 'Ensembler'
       description      <<- 'Aggregator Ensembler'
       package          <<- 'maler'
       package_language <<- 'R'

       if(is.empty(name)){name <<- 'AGGR' %>% paste0(sample(10000:99999, 1))}

       config$aggregator <<- verify(config$aggregator, 'character', lengths = 1, default = 'mean')
       assert(match.fun(config$aggregator) %>% inherits('function'), paste(config$aggregator, 'is not a function!'))
     },

     model.fit = function(X, y){
       objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
       X = X[objects$features$fname]
     },

     model.predict = function(X){
       aggrfun = match.fun(config$aggregator)

       X %>% as.matrix %>% apply(1, aggrfun) %>% as.data.frame
     }
   )
)

# previously KMEANS
#' @export BIN.KMEANS.KMC
BIN.KMEANS.KMC = setRefClass('BIN.KMEANS.KMC', contains = 'MODEL', methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Binner'
    description      <<- 'KMeans Clustering'
    package          <<- 'maler'
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
      objects$model$centers %>% apply(1, function(v) gener::difference(u, v)) %>% order %>% first
    }
    X %>% as.matrix %>% apply(1, bibi) %>% as.data.frame %>% {colnames(.)<-NULL;.}
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
    colnames(XOUT) <- name %>% paste(colnames(XOUT), sep = '_')
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

# previously GROUPER
BIN.MALER.GROUPER = setRefClass('BIN.MALER.GROUPER', contains = "MODEL", methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Binner'
    description      <<- 'Categorical Feature Grouper'
    package          <<- 'maler'
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


FET.MALER.SFS = setRefClass('FET.MALER.SFS', contains = 'MODEL', methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Feature Generator'
    description      <<- 'Stepwise Feature Selector'
    package          <<- 'maler'
    package_language <<- 'R'
    if(is.empty(name)){name <<- 'SFS' %>% paste0(sample(10000:99999, 1))}
    config$model_class      <<- verify(config$model_class, 'character', default = 'CLS.SCIKIT.XGB')
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
      config$feature_order <<- config$feature_order %>% intersect(colnames(X))
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


# previously: IDENTITY
MAP.MALER.IDT = setRefClass('MAP.MALER.IDT' , contains = "MODEL",
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Mapper'
      description      <<- 'Identity'
      package          <<- 'maler'
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
