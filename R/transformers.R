
# This model, finds all numeric features in X and uses smbinning R package to optimally bin them to categorical columns and returns the table with additional features
# original features are NOT maintained in the output table
# This model is using stupid package smbinning. It only divides to 5 buckets, so it's not really optimal binning but better than nothing!
# Also, make sure there is no '.' character in column labels because the package author has problem with dots!!!!!!
SMBINNING = setRefClass('SMBINNING', contains = "MODEL",
    methods = list(
      initialize = function(settings = list(), ...){
        callSuper(...)
        # refer to help page for package smbinning (?smbinning::smbinning)
        settings$percentageOfRecords %<>% verify('numeric', domain = c(0, 0.5), default = '0.05')
        settings$suffix              %<>% verify('character', default = 'BIN')
        config <<- settings
        type   <<- 'Optimal Binner'
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
              res = smbinning::smbinning(ds, y = 'Y', x = col)
              if(!inherits(res, 'character')){
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
        X  = callSuper(X)
        ns = names(objects$binners)
        ds = X[, ns]
        for(ft in ns){
          ds  %<>% smbinning::smbinning.gen(objects$binners[[ft]], chrname = ft %>% paste(config$suffix, sep = '.'))
        }
        columns = colnames(ds) %>% setdiff(colnames(X))
        return(ds[, columns])
      }
    )
)

# Replaces categorical features with class ratios associated with each category
SEGMENTER.RATIO = setRefClass('SEGMENTER.RATIO', contains = 'MODEL',
   methods = list(
     initialize = function(settings = list(), ...){
       callSuper(settings = settings, ...)
       type     <<- 'Class-Ratio Segmenter'
     },
     fit = function(X, y, do_transform = T){
       if(!fitted){
         if(do_transform){X = transform(X, y)}
         objects$features <<- data.frame(name = nominals(X), stringsAsFactors = F)
         X = X[objects$features$name]
         objects$model <<- list()
         for(col in colnames(X)){
           objects$model[[col]] <<- cbind(X, label = y) %>% group_by_(col) %>% summarise(ratio = mean(label))
         }
         fitted <<- T
       }
     },
     
     predict = function(X){
       X = callSuper(X)
       for(col in objects$features$name){
         cn = 'ratio' %>% {names(.)<-'ratio' %>% paste(col, sep = '_');.}
         X %<>% left_join(objects$model[[col]], by = col) %>% spark.rename(cn)
       }
       return(X)
     }
   )
)

SEGMENTER.MODEL = setRefClass('SEGMENTER.MLR', contains = 'MODEL',
  methods = list(
    initialize = function(settings = list(), ...){
      callSuper(settings = settings, ...)
      type     <<- 'Model Segmenter'
      config$model_class  <<- settings$model_class %>% verify('character', default = 'SCIKIT.XGB')
      config$model_config <<- settings$model_config %>% verify('list', default = list())
      if(is.empty(name)){name <<- 'SEGMOD' %>% paste0(sample(1000:9999, 1))}
    },
    
    fit = function(X, y, do_transform = T){
      if(!fitted){
        if(do_transform){X = transform(X, y)}
        objects$features <<- data.frame(name = nominals(X), stringsAsFactors = F)
        X = X[objects$features$name]
        objects$model <<- list()
        for(col in colnames(X)){
          objects$model[[col]] <<- list()
          Xcol = X %>% pull(col)
          uval = Xcol %>% unique
          for(val in uval){
            vlc = as.character(val)
            objects$model[[col]][[as.character(vlc)]] <<- new(config$model_class, settings = config$model_config)
            wwww = which(Xcol == val)
            if(length(wwww) > 0){
              objects$model[[col]][[vlc]]$fit(X[wwww,], y[wwww])
            }
          }
        }
        fitted <<- T
      }
    },
    
    predict = function(X){
      X = callSuper(X)
      for(col in objects$features$name){
        bibi = function(dot){
          dot %<>% as.data.frame
          data.frame(value = objects$model[[col]][[dot[1,col] %>% as.character]]$predict(dot), stringsAsFactors = F)
        }
        cn = 'value' %>% {names(.) <-name %>% paste(col, sep = '_');.}
        df = X %>% group_by_(col) %>% do({bibi(.)}) %>% spark.rename(cn)
        X %<>% cbind(df[,2])
      }
      return(X)
    }
  ))

CATCONCATER = setRefClass('CATCONCATER', contains = "MODEL",
   methods = list(
     initialize = function(settings = list(), ...){
       callSuper(settings = settings, ...)
       type     <<- 'Categorical Features Concater'
       if(is.empty(name)){name <<- 'CATCON' %>% paste0(sample(1000:9999, 1))}
     },
     
     fit = function(X, y){
       if(!fitted){
         objects$features <<- data.frame(name = nominals(X), stringsAsFactors = F)
         fitted <<- TRUE
       }
     },
     
     predict = function(X){
       X   = callSuper(X)
       X[objects$features$name]
       bibi = function(x) paste(x, collapse = '-')
       X[objects$features$name] %>% apply(1, bibi) 
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
                             X = transform(X, y)
                             objects$features     <<- data.frame(name = colnames(X), stringsAsFactors = F)
                             objects$categoricals <<- nominals(X)
                             assert(!is.empty(objects$categoricals), 'No categorical columns found for dummification!')
                             dummies = character()
                             for(cat in objects$categoricals){
                               dummies %<>% c(cat %>% paste(unique(X[, cat]), sep = '_'))
                             }
                             ## todo: take care of remove_first_dummy and remove_most_frequent_dummy arguments
                             fitted <<- TRUE
                             objects$dummy_columns <<- dummies
                           },

                           predict = function(X){
                             X = callSuper(X)
                             X[objects$categoricals] %>% 
                               fastDummies::dummy_cols(objects$categoricals, remove_first_dummy = FALSE, remove_most_frequent_dummy = TRUE) %>% 
                               {.[, -(sequence(length(objects$categoricals))), drop = F]} -> res
                             # We need to make sure the column names of the output is exactly the same as the table given in fitting
                             comm_cols  = colnames(res) %^% objects$dummy_columns
                             less_cols  = objects$dummy_columns %-% colnames(res)
                             extra      = data.frame.na(nrow = nrow(res), col_names = less_cols)
                             
                             res[, comm_cols] %>% cbind(extra) %>% na2zero %>% {.[, objects$dummy_columns, drop = F]}
                           }
                         )
)

GENETIC.BOOSTER.GEOMETRIC = setRefClass('GENETIC.BOOSTER.GEOMETRIC', contains = 'MODEL',
    methods = list(
      initialize = function(settings = list(), ...){
        callSuper(settings = settings, ...)
        if(is.null(settings$metric)){config$metric <<- cor}
      },

      getFeatureValue = function(flist, name, dataset){
        if(name %in% colnames(dataset)){return(dataset[, name])}
        if(name %in% rownames(flist)){
          if(flist[name, 'father'] %in% names(dataset)) {father = dataset[, flist[name, 'father']]} else {father = getFeatureValue.multiplicative(flist, flist[name, 'father'], dataset)}
          if(flist[name, 'mother'] %in% names(dataset)) {mother = dataset[, flist[name, 'mother']]} else {mother = getFeatureValue.multiplicative(flist, flist[name, 'mother'], dataset)}
          return(father*mother)
        } else {stop('Feature name is not in the list!')}
      },

      # nf features are born by random parents:
      createFeatures = function(flist, nf, prefix = 'Feat'){
        features = rownames(flist)
        flist %>% rbind(
          data.frame(
            name = prefix %>% paste(nrow(flist) + sequence(nf)),
            father = features %>% sample(nf, replace = T),
            mother = features %>% sample(nf, replace = T),
            correlation = NA,
            safety = 0, stringsAsFactors = F) %>% column2Rownames('name'))
      },

      fit = function(X, y){
        if(!fitted){
          objects$columns <<- numerics(X)
          X = X[objects$columns]
          objects$flist <<- data.frame(name = columns, father = NA, mother = NA, correlation = cor(X, y) %>% as.numeric %>% abs, safety = 0) %>% column2Rownames('fname')
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
                                          initialize = function(settings = list(), ...){
                                            callSuper(settings = settings, ...)
                                            if(is.null(settings$metric)){config$metric <<- cross_enthropy}
                                            if(is.null(settings$num_top_features)) {config$num_top_features <<- 1}
                                            if(is.null(settings$cycle_births)) {config$num_top_features <<- 1000}
                                            if(is.null(settings$cycle_survivors)) {config$num_top_features <<- 100}
                                            if(is.null(settings$final_survivors)) {config$final_survivors <<- 1}
                                          },
                                          
                                          getFeatureValue = function(name, dataset){
                                            if(!fitted) stop(paste('from', name, 'of type', type, ':', 'Model not fitted!', '\n'))
                                            getFeatureValue.logical(objects$flist, name, dataset)
                                          },
                                          
                                          fit = function(X, y){
                                            if(!fitted){
                                              X               <- transform(X, y)
                                              objects$flist   <<- genBinFeatBoost.fit(X, y, target = 0.9, epochs = 10, cycle_survivors = config$cycle_survivors, final_survivors = config$final_survivors, cycle_births = config$cycle_births, metric = config$metric)
                                              objects$features <<- data.frame(name = objects$flist$father %U% objects$flist$father, stringsAsFactors = F)
                                              fitted          <<- TRUE
                                            }
                                          },
                                          
                                          predict = function(X){
                                            X   = callSuper(X)
                                            top = objects$flist %>% rownames2Column('name') %>% distinct(correlation, .keep_all = T) %>% 
                                              arrange(desc(correlation)) %>% head(config$num_top_features)
                                            df = NULL
                                            for (i in top %>% nrow %>% sequence){
                                              df %<>% cbind(getFeatureValue(top$name[i], X))  
                                            }
                                            colnames(df) <- top$name
                                            df
                                          }
                                        )
)
