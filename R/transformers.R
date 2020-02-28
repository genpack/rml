
TRANSFORMER = setRefClass('TRANSFORMER', contains = "MODEL", methods = list(

  predict = function(X){
    XORG = callSuper(X) %>% as.data.frame
    XFET = XORG[objects$features$fname]
    if(canuse_saved_prediction(XFET)){
      XOUT = objects$saved_pred$XOUT
    } else {
      XOUT = .self$model.predict(XFET)
      keep_prediction(XFET, XOUT)
    }
    if(is.null(colnames(XOUT))){
      if(ncol(XOUT) == nrow(objects$features)){
        colnames(XOUT) <- name %>% paste(objects$features$fname, sep = '_')
      } else if (ncol(XOUT) == 1){
        colnames(XOUT) <- name
      } else{
        colnames(XOUT) <- name %>% paste(sequence(ncol(XOUT)), sep = '_')
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
        if(is.empty(name)){name <<- 'SMBIN' %>% paste0(sample(10000:99999, 1))}
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
          config$keep_ctrees %<>% verify('logical', lengths = 1, domain = c(T,F), default = F)

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

#' @export NORMALIZER
NORMALIZER = setRefClass('NORMALIZER', contains = 'TRANSFORMER',
  methods = list(
    initialize = function(...){
      callSuper(...)
      config$suffix <<- config$suffix %>% verify('character', default = 'NRM')
      type          <<- 'Normalizer'
      if(is.empty(name)){name <<- 'NRMR' %>% paste0(sample(10000:99999, 1))}
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
    if(is.empty(name)){name <<- 'SCLR' %>% paste0(sample(10000:99999, 1))}
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
    if(is.empty(name)){name <<- 'OBIN' %>% paste0(sample(10000:99999, 1))}
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

ENCODER.HELMERT = setRefClass(
  'ENCODER.HELMERT',
  contains = 'TRANSFORMER',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type     <<- 'Helmert Categorical Encoder'
      if(is.empty(name)){name <<- 'HLMRTENC' %>% paste0(sample(10000:99999, 1))}
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

ENCODER.CATBOOST = setRefClass(
  'ENCODER.CATBOOST',
  contains = 'TRANSFORMER',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type     <<- 'CatBoost Categorical Encoder'
      if(is.empty(name)){name <<- 'CTBSTENC' %>% paste0(sample(10000:99999, 1))}
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

ENCODER.JAMESSTEIN = setRefClass(
  'ENCODER.JAMESSTEIN',
  contains = 'TRANSFORMER',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type     <<- 'James Stein Categorical Target Encoder'
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


# Replaces categorical features with class ratios associated with each category
#' @export ENCODER.TARGET
ENCODER.TARGET = setRefClass('ENCODER.TARGET', contains = 'TRANSFORMER',
   methods = list(
     initialize = function(...){
       callSuper(...)
       # type     <<- 'Class-Ratio Categorical Target Encoder'
       type     <<- 'Class-Ratio Segmenter (Target Encoder)'
       if(is.empty(name)){name <<- 'SMR' %>% paste0(sample(10000:99999, 1))}

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

#' @export ENCODER.MODEL
ENCODER.MODEL = setRefClass('ENCODER.MODEL', contains = 'TRANSFORMER',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type     <<- 'Model Segmenter'
      config$model_class  <<- config$model_class %>% verify('character', default = 'CLS.SCIKIT.XGB')
      config$model_config <<- config$model_config %>% verify('list', default = list(predict_probabilities = T))
      config$min_rows     <<- config$min_rows %>% verify(c('numeric', 'integer'), lengths = 1, default = 100)
      if(is.empty(name)){name <<- 'SEGMOD' %>% paste0(sample(10000:99999, 1))}
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



ENCODER.MODEL.BOOSTER =
  setRefClass('ENCODER.MODEL.BOOSTER',
              contains = 'ENCODER.MODEL',

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

INVERTER = setRefClass('INVERTER', contains = 'TRANSFORMER', methods = list(
  initialize = function(...){
    callSuper(...)
    type     <<- 'Inverter Transformer'
    if(is.empty(name)){name <<- 'INV' %>% paste0(sample(10000:99999, 1))}
    config$trim <<- config$trim %>% verify('numeric', lengths = 1, default = 10000)
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
GEOPOLY = setRefClass('GEOPOLY', contains = 'TRANSFORMER', methods = list())
MULTIPLIER.BINARY = setRefClass('MULTIPLIER.BINARY', contains = 'TRANSFORMER', methods = list(
  initialize = function(...){
    callSuper(...)
    type     <<- 'Binary Multiplier'
    if(is.empty(name)){name <<- 'BMUL' %>% paste0(sample(10000:99999, 1))}
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

MULTIPLIER.BINARY.COMP = setRefClass('MULTIPLIER.BINARY.COMP', contains = 'MULTIPLIER.BINARY', methods = list(
  initialize = function(...){
    callSuper(...)
    type     <<- 'Binary Multiplier Components'
    if(is.empty(name)){name <<- 'BMC' %>% paste0(sample(10000:99999, 1))}
    config$num_components <<- config$num_components %>% verify(c('numeric', 'integer'), default = 5)
    config$model_class  <<- config$model_class  %>% verify('character', default = 'SCIKIT.XGB')
    config$model_config <<- config$model_config %>% verify('character', default = list())
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

POLYNOMIAL = setRefClass('POLYNOMIAL', contains = 'TRANSFORMER', methods = list(
  initialize = function(...){
    callSuper(...)
    type     <<- 'Polynomial Feature Transformer'
    if(is.empty(name)){name <<- 'PLYG' %>% paste0(sample(10000:99999, 1))}

    # Sigma{k = 1}{n_terms} (gain*x + intercept)^k
    config$apply_abs <<- config$apply_abs %>% verify('logical', default = F)
    config$intercept <<- config$intercept %>% verify('numeric', default = 0)
    config$gain      <<- config$gain %>% verify('numeric', default = 1)
    config$n_terms   <<- config$n_terms %>% verify('numeric', default = 3) %>% as.integer
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
CATCONCATER = setRefClass('CATCONCATER', contains = "TRANSFORMER",
   methods = list(
     initialize = function(...){
       callSuper(...)
       type     <<- 'Categorical Features Concater'
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


LOGGER = setRefClass('LOGGER', contains = "TRANSFORMER", methods = list(
  initialize = function(...){
    callSuper(...)
    type     <<- 'Logger Transformer'
    if(is.empty(name)){name <<- 'LOG' %>% paste0(sample(10000:99999, 1))}
    config$apply_abs <<- config$apply_abs %>% verify(c('logical'), default = T)
    config$intercept <<- config$intercept %>% verify(c('numeric'), default = 1)
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

#' @export DUMMIFIER
DUMMIFIER = setRefClass('DUMMIFIER', contains = "TRANSFORMER",
   methods = list(
     initialize = function(...){
       callSuper(...)
       type     <<- 'Categorical Feature Dummifier'
       if(is.empty(name)){name <<- 'DMFR' %>% paste0(sample(10000:99999, 1))}
       config$max_domain <<- config$max_domain %>% verify(c('numeric', 'integer'), default = 25) %>% as.integer
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

#' @export GENETIC.BOOSTER.GEOMETRIC
GENETIC.BOOSTER.GEOMETRIC = setRefClass('GENETIC.BOOSTER.GEOMETRIC', contains = 'TRANSFORMER',
    methods = list(
      initialize = function(...){
        type     <<- 'Geometric Genetic Booster'
        if(is.empty(name)){name <<- 'GBG' %>% paste0(sample(10000:99999, 1))}

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

#' @export GENETIC.BOOSTER.LOGICAL
GENETIC.BOOSTER.LOGICAL = setRefClass('GENETIC.BOOSTER.LOGICAL', contains = 'TRANSFORMER', methods = list(
  initialize = function(...){
    callSuper(...)
    type     <<- 'Logical Genetic Booster'
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

#' @export SUPERVISOR
SUPERVISOR = setRefClass('SUPERVISOR', contains = "TRANSFORMER",
  methods = list(
    initialize = function(...){
      callSuper(...)
      type     <<- 'Model Supervisor'
      if(is.empty(name)){name <<- 'SUP' %>% paste0(sample(10000:99999, 1))}
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
    if(is.empty(name)){name <<- 'KMNS' %>% paste0(sample(10000:99999, 1))}
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
    if(is.empty(name)){name <<- 'PRC' %>% paste0(sample(10000:99999, 1))}
    config$num_components <<- config$num_components %>% verify(c('numeric', 'integer'), default = 5)
    config$scale  <<- config$scale  %>% verify('logical', domain = c(F,T), default = T)
    config$center <<- config$center %>% verify('logical', domain = c(F,T), default = T)
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

PYLMNN = setRefClass('PYLMNN', contains = "TRANSFORMER", methods = list(
  initialize = function(...){
    callSuper(...)
    type     <<- 'Large Margin Nearest Neighbors'
    if(is.empty(name)){name <<- 'LMNN' %>% paste0(sample(10000:99999, 1))}
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
    XOUT = objects$model$transform(X %>% data.matrix) %>% as.data.frame
    return(XOUT)
  }
))

GROUPER = setRefClass('GROUPER', contains = "TRANSFORMER", methods = list(
  initialize = function(...){
    callSuper(...)
    type     <<- 'Categorical Feature Grouper'
    if(is.empty(name)){name <<- 'GRP' %>% paste0(sample(10000:99999, 1))}
    config$encoding <<- config$encoding %>% verify('character', domain =c('target_ratio', 'flasso'), default = 'target_ratio')
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

SFS = setRefClass('SFS', contains = 'TRANSFORMER', methods = list(
  initialize = function(...){
    callSuper(...)
    type     <<- 'Stepwise Feature Selector'
    if(is.empty(name)){name <<- 'SFS' %>% paste0(sample(10000:99999, 1))}
    config$model_class  <<- config$model_class %>% verify('character', default = 'CLS.SCIKIT.XGB')
    config$model_config <<- config$model_config %>% verify('list', default = list())
    config$return_features <<- config$return_features %>% verify('logical', lengths = 1, domain = c(T,F), default = T)
  },

  model.fit = function(X, y){
    cv.set.transformed = config$model_config$cv.set
    if(!is.null(config$model_config$cv.set) & !is.empty(transformers)){
      for(i in sequence(length(cv.set.transformed))){cv.set.transformed[[i]]$X %<>% transform}
    }

    if(is.null(config$feature_order)){
      mdl <- new(config$model_class, config = config$model_config)
      mdl$config$cv.set = cv.set.transformed

      feature_subset_scorer(mdl, X, y) -> scores
      objects$features <<- scores %>% rownames2Column('fname') %>%
        filter(score > 0) %>% arrange(desc(score)) %>% mutate(fname = as.character(fname)) %>% select(fname)
    } else {
      objects$features <<- data.frame(fname = config$feature_order, stringsAsFactors = F)
    }

    # Bild best model with best predictor:
    fetbag = c()
    perf   = -Inf

    for(ft in objects$features$fname){
      mdl = new(config$model_class, config = config$model_config)
      mdl$config$cv.set = cv.set.transformed

      new_perf = mdl$get.performance.cv(X[, c(fetbag, ft), drop = F], y) %>% median
      if(new_perf > perf){
        cat('\n', 'New feature added: ', ft, ' New performance: ', new_perf)
        fetbag = mdl$objects$features %>% filter(importance > 0) %>% pull(fname)
        perf   = new_perf
        if(!config$return_features){
          objects$model <<- mdl$copy()
        }
      }
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



IDENTITY = setRefClass('IDENTITY' , contains = "TRANSFORMER",
  methods = list(
    initialize = function(...){
      callSuper(...)
      type <<- 'Identity Transformer'
      if(is.empty(name)){name <<-'IDTY' %>% paste0(sample(10000:99999, 1))}
    },

    model.fit = function(X, y){
      # do nothing
    },

    model.predict = function (X){
      # Do nothing
      X
    }
  ))
