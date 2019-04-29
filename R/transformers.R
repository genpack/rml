
# This model, finds all numeric features in X and uses smbinning R package to optimally bin them to categorical columns and returns the table with additional features
# original features are NOT maintained in the output table
# This model is using stupid package smbinning. It only divides to 5 buckets, so it's not really optimal binning but better than nothing!
# Also, make sure there is no '.' character in column labels because the package author has problem with dots!!!!!!
SMBINNING = setRefClass('SMBINNING', contains = "MODEL",
    methods = list(
      initialize = function(...){
        callSuper(...)
        # refer to help page for package smbinning (?smbinning::smbinning)
        config$percentageOfRecords <<- config$percentageOfRecords %>% verify('numeric', domain = c(0, 0.5), default = '0.05')
        config$suffix              <<- config$suffix %>% verify('character', default = 'BIN')
        type                       <<- 'Optimal Binner'
      },

      fit = function(X, y){
        if(!fitted){
          objects$binners <<- list()
          X = transform(X, y)
          # This model currently only works for categorical y
          if(inherits(y, 'character')){y %<>% as.factor %>% as.integer}
          if(inherits(y, c('logical', 'numeric'))){y %<>% as.integer}

          columns = numerics(X)
          if(length(columns) > 0){
            ds = cbind(X[, columns], Y = y)
            for(col in columns){
              res = try(smbinning::smbinning(ds, y = 'Y', x = col), silent = T)
              
              if(inherits(res, 'list')){
                ns = names(objects$binners)
                res$col_id = length(objects$binners) + 1
                objects$binners <<- c(objects$binners, list(res))
                names(objects$binners) <<- c(ns, col)
                cat('Column ', col, ': Successfully binned to 5 buckets!', '\n')
              } else {
                cat('Column ', col, ': ', res, '\n')
              }
            }
          }
          objects$features <<- data.frame(fname = columns, stringsAsFactors = F)
          # todo: add importance
          fitted <<- TRUE
        }
      },

      get.features.name = function(){
        names(objects$binners)
      },

      get.features.weight = function(){
      },

      # predict acts like transformer
      predict = function(X){
        XORG  = callSuper(X)
        XFET  = XORG[objects$features$fname]
        ns    = names(objects$binners)
        ds    = XFET[, ns]
        for(ft in ns){
          ds  %<>% smbinning::smbinning.gen(objects$binners[[ft]], chrname = ft %>% paste(config$suffix, sep = '.'))
        }
        columns = colnames(ds) %>% setdiff(colnames(XFET))
        XOUT    = ds[, columns, drop = F]
        treat(XOUT, XFET, XORG)
      }
    )
)

NORMALIZER = setRefClass('NORMALIZER', contains = 'MODEL', 
  methods = list(
    initialize = function(...){
      callSuper(...)
      config$suffix <<- config$suffix %>% verify('character', default = 'NRM')
      type          <<- 'Normalizer'
      
    },
    fit = function(X, y = NULL){
      if(!fitted){
        X = callSuper(X, y)
        objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
        X = X[objects$features$fname] %>% as.matrix
        X %>% apply(2, function(v) min(v, na.rm = T)) ->> objects$features$min
        X %>% apply(2, function(v) max(v, na.rm = T)) ->> objects$features$max
        fitted <<- TRUE
      }
    },
      
      predict = function(X){
        XORG  = callSuper(X)
        XFET  = XORG[objects$features$fname] %>% as.data.frame
        feat  = objects$features %>% column2Rownames('fname')
        
        XOUT  = objects$features$fname %>% sapply(function(i) (XFET[,i] -  feat[i,'min'])/(feat[i,'max'] - feat[i,'min'])) %>% na2zero %>% as.data.frame
        colnames(XOUT) <- objects$features$fname %>% paste(config$suffix, sep = '_')
        treat(XOUT, XFET, XORG)
      }  
      
  ))

SCALER = setRefClass('SCALER', contains = "MODEL", methods = list(
  initialize = function(...){
    callSuper(...)
    config$suffix              <<- config$suffix %>% verify('character', default = 'SCALED')
    type                       <<- 'ZFactor Scaler'
  },
  
  fit = function(X, y = NULL){
    if(!fitted){
      X = callSuper(X, y)
      objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
      X = X[objects$features$fname] %>% as.matrix
      X %>% apply(2, function(v) mean(v, na.rm = T)) ->> objects$features$mean
      X %>% apply(2, function(v)   sd(v, na.rm = T)) ->> objects$features$stdv
      fitted <<- TRUE
    }
  },
  
  predict = function(X){
    XORG  = callSuper(X)
    XFET  = XORG[objects$features$fname] %>% as.data.frame
    feat  = objects$features %>% column2Rownames('fname')
    
    XOUT  = objects$features$fname %>% sapply(function(i) (XFET[,i] -  feat[i,'mean'])/feat[i,'stdv']) %>% na2zero %>% as.data.frame
    colnames(XOUT) <- objects$features$fname %>% paste(config$suffix, sep = '_')
    treat(XOUT, XFET, XORG)
  }  
))

OPTBINNER = setRefClass('OPTBINNER', contains = "MODEL",
                        methods = list(
                          initialize = function(...){
                            callSuper(...)
                            # refer to help page for package smbinning (?smbinning::smbinning)
                            config$suffix              <<- config$suffix %>% verify('character', default = 'BIN')
                            type                       <<- 'Optimal Binner'
                          },
                          
                          fit = function(X, y){
                            if(!fitted){

                              X       = transform(X, y)
                              columns = numerics(X)
                              if(length(columns) > 0){
                                ds = cbind(X[columns], Y = y)
                                objects$model <<- ds %>% optSplitColumns.f1(columns %-% 'Y', label_col = 'Y') %>% mutate(Column = as.character(Column))
                              }
                              objects$features <<- objects$model %>% mutate(f1 = f1 %>% vect.map(0,1)) %>% rename(fname = Column, importance = f1) %>% select(fname, importance)
                              # todo: add importance
                              fitted <<- TRUE
                            }
                          },
                          
                          # predict acts like transformer
                          predict = function(X){
                            XORG  = callSuper(X)
                            XFET  = XORG[objects$features$fname] %>% as.data.frame
                            XOUT  = XFET %>% ncol %>% sequence %>% sapply(function(i) as.integer(XFET[,i] > objects$model$split[i]))
                            colnames(XOUT) <- objects$features$fname %>% paste(config$suffix, sep = '.')
                            treat(XOUT, XFET, XORG)
                          }
                        )
)


KMEANS = setRefClass('KMEANS', contains = "MODEL", methods = list(
  initialize = function(...){
    callSuper(...)
    type <<- "Kmeans Clustering"
    config$number_of_clusters <<- config$number_of_clusters %>% verify(c('numeric', 'integer'), lengths = 1, default = 5)
  }
))

# Replaces categorical features with class ratios associated with each category
SEGMENTER.RATIO = setRefClass('SEGMENTER.RATIO', contains = 'MODEL',
   methods = list(
     initialize = function(...){
       callSuper(...)
       type     <<- 'Class-Ratio Segmenter'
     },
     
     fit = function(X, y, do_transform = T){
       if(!fitted){
         if(do_transform){X = transform(X, y)}
         objects$features <<- data.frame(fname = nominals(X), stringsAsFactors = F)
         X = X[objects$features$fname]
         objects$model <<- list()
         for(col in colnames(X)){
           objects$model[[col]] <<- cbind(X, label = y) %>% group_by_(col) %>% summarise(ratio = mean(label))
         }
         fitted <<- T
       }
     },
     
     predict = function(X){
       XORG  = callSuper(X)
       XFET  = XORG[objects$features$fname]
       XOUT  = XFET
       for(col in objects$features$fname){
         cn = 'ratio' %>% {names(.)<-'ratio' %>% paste(col, sep = '_');.}
         XOUT %<>% left_join(objects$model[[col]], by = col) %>% spark.rename(cn) %>% spark.unselect(col)
       }
       treat(XOUT, XFET, XORG)
     }
   )
)


SEGMENTER.MODEL = setRefClass('SEGMENTER.MLR', contains = 'MODEL',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type     <<- 'Model Segmenter'
      config$model_class  <<- config$model_class %>% verify('character', default = 'SCIKIT.XGB')
      config$model_config <<- config$model_config %>% verify('list', default = list(predict_probabilities = T))
      config$min_rows     <<- config$min_rows %>% verify(c('numeric', 'integer'), lengths = 1, default = 25)
      if(is.empty(name)){name <<- 'SEGMOD' %>% paste0(sample(1000:9999, 1))}
    },
    
    fit = function(X, y){
      if(!fitted){
        X = callSuper(X, y)
        objects$categoricals <<- nominals(X)
        #X = X[objects$categoricals]
        objects$model <<- list()
        for(col in objects$categoricals){
          objects$model[[col]] <<- list()
          Xcol = X %>% pull(col)
          uval = Xcol %>% unique
          objects$model[[col]][['__global__']] <<- new(config$model_class, config$model_config)
          for(val in uval){
            vlc = as.character(val)
            www = which(Xcol == val)
            if(length(www) > config$min_rows){
              objects$model[[col]][[vlc]] <<- new(config$model_class, config$model_config)
              objects$model[[col]][[vlc]]$fit(X[www,], y[www])
            } else {
              objects$model[[col]][['__global__']]$fit(X %>% spark.unselect(col), y)
            }
          }
        }
        fitted <<- T
      }
    },
    
    predict = function(X){
      XORG  = callSuper(X)
      XFET  = XORG[objects$features %>% pull(fname)]
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
        df = XFET %>% group_by_(col) %>% do({bibi(.)}) %>% spark.rename(cn)
        if(is.null(XOUT)){XOUT = df[,2, drop = F]} else {XOUT %<>% cbind(df[,2, drop = F])}
      }
      treat(XOUT, XFET, XORG)
    }
  ))

CATCONCATER = setRefClass('CATCONCATER', contains = "MODEL",
   methods = list(
     initialize = function(...){
       callSuper(...)
       type     <<- 'Categorical Features Concater'
       if(is.empty(name)){name <<- 'CATCON' %>% paste0(sample(1000:9999, 1))}
     },
     
     fit = function(X, y){
       if(!fitted){
         X = callSuper(X, y)
         objects$features <<- data.frame(fname = nominals(X), stringsAsFactors = F)
         fitted <<- TRUE
       }
     },
     
     predict = function(X){
       XORG = callSuper(X)
       XFET = XORG[objects$features$fname]
       bibi = function(x) paste(x, collapse = '-')
       XOUT = XFET %>% apply(1, bibi)
       treat(XOUT, XFET, XORG)
     }
   )
)


DUMMIFIER = setRefClass('DUMMIFIER', contains = "MODEL",
                         methods = list(
                           initialize = function(...){
                             callSuper(...)
                             type     <<- 'Categorical Feature Dummifier'
                           },

                           fit = function(X, y = NULL){
                             X = callSuper(X)
                             objects$features     <<- objects$features %>% filter(class %in% nominals(X))
                             warnif(is.empty(objects$features), 'No categorical columns found for dummification!')
                             dummies = character()
                             for(cat in objects$features %>% pull(fname)){
                               dummies %<>% c(cat %>% paste(unique(X %>% pull(cat)), sep = '_'))
                             }
                             ## todo: take care of remove_first_dummy and remove_most_frequent_dummy arguments
                             fitted <<- TRUE
                             objects$dummy_columns <<- dummies
                             # todo: rename to output_features and add importance(performance)
                           },

                           predict = function(X){
                             XORG  = callSuper(X)
                             XFET  = XORG[objects$features$fname]
                             XFET %>% 
                               fastDummies::dummy_cols(objects$features$fname, remove_first_dummy = FALSE, remove_most_frequent_dummy = TRUE) %>% 
                               {.[, -(sequence(length(objects$features$fname))), drop = F]} -> res
                             # We need to make sure the column names of the output is exactly the same as the table given in fitting
                             comm_cols  = colnames(res) %^% objects$dummy_columns
                             less_cols  = objects$dummy_columns %-% colnames(res)
                             extra      = data.frame.na(nrow = nrow(res), col_names = less_cols)
                             
                             XOUT = res[comm_cols] %>% cbind(extra) %>% na2zero %>% {.[, objects$dummy_columns, drop = F]}
                             treat(XOUT, XFET, XORG)
                           }
                         )
)

GENETIC.BOOSTER.GEOMETRIC = setRefClass('GENETIC.BOOSTER.GEOMETRIC', contains = 'MODEL',
    methods = list(
      initialize = function(...){
        callSuper(...)
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
      createFeatures = function(flist, nf, prefix = 'Feat'){
        features = rownames(flist)
        flist %>% rbind(
          data.frame(
            fname  = prefix %>% paste(nrow(flist) + sequence(nf)),
            father = features %>% sample(nf, replace = T),
            mother = features %>% sample(nf, replace = T),
            correlation = NA,
            safety = 0, stringsAsFactors = F) %>% column2Rownames('fname'))
      },

      fit = function(X, y){
        if(!fitted){
          objects$columns <<- numerics(X)
          X = X[objects$columns]
          objects$model <<- data.frame(fname = columns, father = NA, mother = NA, correlation = cor(X, y) %>% as.numeric %>% abs, safety = 0) %>% column2Rownames('fname')
          objects$fdata <<- X[columns]

          i = 0
          while(i < 10){
            i = i + 1
            flist = createFeatures(flist, 1000)
            flist %<>% evaluateFeatures.multiplicative(X = dataset[,columns], y = dataset[,'Y'], top = 100)

            cat('Iteration:', i, ': Best Correlation = ', 100*max(flist$correlation), ' nrow(flist) = ', nrow(flist), '\n')
          }

        }
      }
    )
                                        )


GENETIC.BOOSTER.LOGICAL = setRefClass('GENETIC.BOOSTER.LOGICAL', contains = 'MODEL',
                                        methods = list(
                                          initialize = function(...){
                                            callSuper(...)
                                            if(is.null(config$metric)){config$metric <<- cross_enthropy}
                                            if(is.null(config$num_top_features)) {config$num_top_features <<- 1}
                                            if(is.null(config$cycle_births)) {config$num_top_features <<- 1000}
                                            if(is.null(config$cycle_survivors)) {config$num_top_features <<- 100}
                                            if(is.null(config$final_survivors)) {config$final_survivors <<- 1}
                                          },
                                          
                                          getFeatureValue = function(fname, dataset){
                                            if(!fitted) stop(paste('from', fname, 'of type', type, ':', 'Model not fitted!', '\n'))
                                            getFeatureValue.logical(objects$model, fname, dataset)
                                          },
                                          
                                          fit = function(X, y){
                                            if(!fitted){
                                              X                <- transform(X, y)
                                              objects$model    <<- genBinFeatBoost.fit(X, y, target = 0.9, epochs = 10, cycle_survivors = config$cycle_survivors, final_survivors = config$final_survivors, cycle_births = config$cycle_births, metric = config$metric)
                                              objects$features <<- data.frame(fname = objects$model$father %U% objects$model$father, stringsAsFactors = F)
                                              fitted           <<- TRUE
                                            }
                                          },
                                          
                                          predict = function(X){
                                            XORG   = callSuper(X)
                                            XFET   = XORG[objects$features$fname]
                                            top = objects$model %>% rownames2Column('fname') %>% distinct(correlation, .keep_all = T) %>% 
                                              arrange(desc(correlation)) %>% head(config$num_top_features)
                                            XOUT = NULL
                                            for (i in top %>% nrow %>% sequence){
                                              XOUT %<>% cbind(getFeatureValue(top$fname[i], XFET))  
                                            }
                                            colnames(XOUT) <- top$fname
                                            treat(XOUT, XFET, XORG)
                                          }
                                        )
)


SUPERVISOR = setRefClass('SUPERVISOR', contains = "MODEL",
  methods = list(
    initialize = function(...){
      callSuper(...)
      type     <<- 'Model Supervisor'
      if(is.empty(name)){name <<- 'SUP' %>% paste0(sample(1000:9999, 1))}
      config$model_class  <<- config$model_class  %>% verify('character', default = 'SCIKIT.XGB')
      config$model_config <<- config$model_config %>% verify('character', default = list())
      objects$model <<- new(config$model_class, config$model_config)
    },
    
    fit = function(X, y){
      if(!fitted){
        X  = callSuper(X, y)
        Xt = X %>% spark.unselect(objects$pupils)
        pp = X %>% spark.select(objects$pupils) %>% as.matrix
        pp %>% apply(1, which.max) -> highest
        pp %>% apply(1, which.min) -> lowest
        
        yt = ifelse(y, highest, lowest) - 1
        objects$model$fit(Xt, yt)
        fitted <<- TRUE
      }
    },
    
    predict = function(X){
      XORG = callSuper(X)
      XFET = XORG[objects$features$fname]
      pp   = XFET %>% spark.select(objects$pupils)
      yout = objects$model$predict(XFET) %>% as.matrix
      XOUT = pp %>% nrow %>% sequence %>% sapply(function(i) pp[i, yout[i,1] + 1]) %>% as.data.frame
      colnames(XOUT) <- name %>% paste('out', sep = '_')
      treat(XOUT, XFET, XORG)
    }
  )
)

KMEANS = setRefClass('KMEANS', contains = 'MODEL', methods = list(
  initialize = function(...){
    callSuper(...)
    type     <<- 'KMeans Clustering Transformer'
    if(is.empty(name)){name <<- 'KMNs' %>% paste0(sample(1000:9999, 1))}
    config$num_cluster <<- config$num_cluster %>% verify(c('numeric', 'integer'), default = 5)
    
  },
  fit = function(X, y = NULL){
    if(!fitted){
      X = callSuper(X, y)
      objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
      X = X[objects$features$fname]
      objects$model <<- kmeans(X, centers = config$num_clusters)
    }
  },
  
  predict = function(X){
    XORG = callSuper(X)
    XFET = XORG[objects$features$fname]
    bibi = function(u){
      objects$model$centers %>% apply(1, function(v) gener::difference(u, v)) %>% order %>% first
    }
    XOUT = XFET %>% as.matrix %>% apply(2, bibi)
    colnames(XOUT) <- name %>% paste('out', sep = '_')
    treat(XOUT, XFET, XORG)
  }
))
