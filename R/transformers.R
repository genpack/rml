
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
        callSuper()
        X  = transform(X)
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


CATREMOVER = setRefClass('CATREMOVER', contains = "MODEL",
    methods = list(
      initialize = function(settings = list(), ...){
        callSuper(...)
        type     <<- 'Categorical Feature Remover'
      },

      fit = function(X, y){
        if(!fitted){
          objects$model <<- numerics(X)
          fitted <<- TRUE
        }
      },

      predict = function(X){
        callSuper()
        X[objects$model]
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
                             objects$categoricals <<- nominals(X)
                             fitted <<- TRUE
                           },

                           predict = function(X){
                             # callSuper()
                             X = transform(X)
                             X[objects$categoricals] %>% fastDummies::dummy_cols(objects$categoricals)
                           }
                         )
)
