
#' @title One-hot encoder (fastDummies)
#' @name ENC.FASTDUMMIES.OHE-class
#' @aliases ENC.FASTDUMMIES.OHE
#' @description
#' Creates one 0/1 column per value of each categorical column with between 3 and \code{max_domain} distinct values.
#'
#' Inherits from \code{\link{MODEL-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{max_domain}}{Maximum number of distinct values of an encoded column. Default 25.}
#' }
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), city = sample(c('A', 'B', 'C'), 300, replace = TRUE))
#' y = as.numeric(X$a + (X$city == 'A') + rnorm(300) > 0)
#' m = ENC.FASTDUMMIES.OHE()
#' m$fit(X, y)
#' head(m$predict(X))
#' }
#' @export ENC.FASTDUMMIES.OHE
ENC.FASTDUMMIES.OHE = setRefClass('ENC.FASTDUMMIES.OHE', contains = "MODEL",
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Encoder'
      description      <<- 'One Hot Encoder'
      package          <<- 'fastDummies'
      packages_required <<- c(packages_required, 'fastDummies')
      
      package_language <<- 'R'
      reserved_words   <<- c(reserved_words, 'max_domain')
      
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


# Converts categorical columns to a character matrix (same text values at fit and predict time)
#' @title Text matrix for one-hot encoding
#' @param X \code{data.frame} of categorical columns.
#' @return Character matrix.
#' @keywords internal
#' @export
ohe_input = function(X){
  if(inherits(X, 'WIDETABLE')){X = wt_as_data_frame(X)}
  X = as.data.frame(X)
  for(i in sequence(ncol(X))){X[[i]] <- as.character(X[[i]])}
  as.matrix(X)
}

#' @title One-hot encoder (scikit-learn)
#' @name ENC.SKLEARN.OHE-class
#' @aliases ENC.SKLEARN.OHE
#' @description
#' Wraps \code{sklearn.preprocessing.OneHotEncoder} for categorical columns with at most \code{max_domain} distinct values.
#' Values are encoded as text, values not seen in training give all-zero rows (\code{handle_unknown = 'ignore'}), and
#' output columns are named \code{<column>_<value>}.
#' Requires Python with the module named below, reachable through reticulate. Set \code{python_address} to choose the Python executable; see \code{\link{python_setup}} and \code{vignette("rml-troubleshooting")} if the module is not found.
#'
#' Inherits from \code{\link{TRM.SKLEARN-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{max_domain}}{Maximum number of distinct values of an encoded column. Default 25.}
#' }
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), city = sample(c('A', 'B', 'C'), 300, replace = TRUE))
#' y = as.numeric(X$a + (X$city == 'A') + rnorm(300) > 0)
#' m = ENC.SKLEARN.OHE()
#' m$fit(X, y)
#' head(m$predict(X))
#' }
#' @export ENC.SKLEARN.OHE
ENC.SKLEARN.OHE = setRefClass(
  'ENC.SKLEARN.OHE',
  contains = 'TRM.SKLEARN',
  methods = list(
    initialize = function(...){
      callSuper(model.module = 'preprocessing', model.class = 'OneHotEncoder', ...)
      type             <<- 'Encoder'
      description      <<- 'One-Hot Encoder'
      reserved_words   <<- c(reserved_words, 'max_domain', 'model.module', 'model.class')
      
      if(is.empty(name)){name <<- 'SKOHE' %>% paste0(sample(10000:99999, 1))}
      config$max_domain <<- verify(config$max_domain, c('numeric', 'integer'), default = 25) %>% as.integer
    },
    
    model.fit = function(X, y){
      if(!fitted){
        if(!'n_unique' %in% colnames(objects$features)){
          objects$features$n_unique <<- vapply(objects$features$fname, function(x) length(unique(X[[x]])), integer(1), USE.NAMES = F)
        }
        objects$features <<- objects$features %>% filter(fname %in% nominals(X), n_unique <= config$max_domain)
        assert(nrow(objects$features) > 0, 'No categorical columns with few enough distinct values for one-hot encoding!')
        args = python_args(config, reserved_words)
        if(is.null(args$handle_unknown)){args$handle_unknown <- 'ignore'}
        if(is.null(args$sparse_output)){args$sparse_output <- FALSE}
        objects$module <<- reticulate::import('sklearn.preprocessing')
        objects$model  <<- do.call(objects$module$OneHotEncoder, args)
        objects$model$fit(ohe_input(X[objects$features$fname]))
      }
    },

    model.predict = function(X){
      retrieve_model()
      out = objects$model$transform(ohe_input(X[objects$features$fname])) %>% as.matrix %>% as.data.frame
      colnames(out) <- as.character(objects$model$get_feature_names_out(objects$features$fname))
      return(out)
    }
))


#' @title Wrapper for encoders of the Python package category_encoders
#' @name ENC.CATEGORY_ENCODERS-class
#' @aliases ENC.CATEGORY_ENCODERS
#' @description
#' Superclass of the category_encoders based classes. Encodes categorical (character, factor, integer) columns.
#' Requires Python with the module named below, reachable through reticulate. Set \code{python_address} to choose the Python executable; see \code{\link{python_setup}} and \code{vignette("rml-troubleshooting")} if the module is not found.
#'
#' Inherits from \code{\link{MODEL-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{model.module}}{category_encoders sub-module.}
#'   \item{\code{model.class}}{Encoder class.}
#' }
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), city = sample(c('A', 'B', 'C'), 300, replace = TRUE))
#' y = as.numeric(X$a + (X$city == 'A') + rnorm(300) > 0)
#' m = ENC.CATEGORY_ENCODERS(model.module = 'target_encoder', model.class = 'TargetEncoder')
#' m$fit(X, y)
#' head(m$predict(X))
#' }
#' @export ENC.CATEGORY_ENCODERS
ENC.CATEGORY_ENCODERS = setRefClass(
  'ENC.CATEGORY_ENCODERS',
  contains = 'MODEL',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Encoder'
      description      <<- 'Superclass Wrapper for encoders from Python package category_encoders'
      package          <<- 'category_encoders'
      package_language <<- 'Python'

      config$model.module <<- verify(config$model.module, 'character', lengths = 1, null_allowed = F)
      config$model.class  <<- verify(config$model.class, 'character' , lengths = 1, null_allowed = F)
      
      config$column_filters <<- config$column_filters %>% 
        rutils::list.add(list(column = "fclass", filter = " %in% c('character', 'factor', 'integer')"))
      
      if(is.empty(name)){name <<- 'CEENC' %>% paste0(sample(10000:99999, 1))}
      
      objects$module <<- reticulate::import(paste('category_encoders', config[['model.module']], sep = '.'))
    },
    
    model.fit = function(X, y){
      if(!fitted){
        objects$module <<- reticulate::import(paste('category_encoders', config[['model.module']], sep = '.'))
        objects$model  <<- do.call(objects$module[[config$model.class]],
                                  python_args(config, reserved_words) %>% list.add(cols = objects$features$fname))
        objects$model$fit(X, y)
      }
    },
    
    model.predict = function(X){
      objects$model$transform(X)
    },
    
    save_model_object = function(filename){
      pickle = reticulate::import('pickle')
      handle = "open('%s', 'wb')" %>% sprintf(filename) %>% reticulate::py_eval()
      pickle$dump(objects$model, handle)
    },
    
    load_model_object = function(filename){
      pickle = reticulate::import('pickle')
      handle = "open('%s', 'rb')" %>% sprintf(filename) %>% reticulate::py_eval()
      objects$module <<- reticulate::import(paste('category_encoders', config[['model.module']], sep = '.'))
      objects$model  <<- pickle$load(handle)
    },

    model.save = function(path = getwd()){
      callSuper(path)
      save_model_object(paste0(path, '/', name, '.cem'))
      release_model()
    },
    
    model.load = function(path = getwd()){
      callSuper(path)
      fn   = paste0(path, '/', name, '.cem')
      pass = file.exists(fn)
      warnif(!pass, paste0('File ', fn , ' does not exist!'))
      if(pass){load_model_object(fn)}
    },
    
    # save model object in a tempfile temporarily
    keep_model = function(filename){
      callSuper()
      objects$model_filename <<- tempfile() %>% gsub(pattern = "\\\\", replacement = "/")
      save_model_object(objects$model_filename)
    },
    
    retrieve_model = function(){
      callSuper()
      if(!is.null(objects$model_filename)){
        if(file.exists(objects$model_filename)){
          load_model_object(objects$model_filename)
        }
      }
    }
    
    
))


####

#' @title CatBoost encoder (category_encoders)
#' @name ENC.CATEGORY_ENCODERS.CATB-class
#' @aliases ENC.CATEGORY_ENCODERS.CATB
#' @description
#' Wraps \code{category_encoders.cat_boost.CatBoostEncoder}. Its arguments can be passed.
#' Requires Python with the module named below, reachable through reticulate. Set \code{python_address} to choose the Python executable; see \code{\link{python_setup}} and \code{vignette("rml-troubleshooting")} if the module is not found.
#'
#' Inherits from \code{\link{ENC.CATEGORY_ENCODERS-class}}.
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), city = sample(c('A', 'B', 'C'), 300, replace = TRUE))
#' y = as.numeric(X$a + (X$city == 'A') + rnorm(300) > 0)
#' m = ENC.CATEGORY_ENCODERS.CATB()
#' m$fit(X, y)
#' head(m$predict(X))
#' }
#' @export ENC.CATEGORY_ENCODERS.CATB
ENC.CATEGORY_ENCODERS.CATB = setRefClass(
  'ENC.CATEGORY_ENCODERS.CATB',
  contains = 'ENC.CATEGORY_ENCODERS',
  methods = list(
    initialize = function(...){
      callSuper(model.module = 'cat_boost', model.class = 'CatBoostEncoder', ...)
      type             <<- 'Encoder'
      description      <<- 'CatBoost Encoder'
      package          <<- 'category_encoders'
      package_language <<- 'Python'
      config$model.module     <<- 'cat_boost'
      config$model.class      <<- 'CatBoostEncoder'
      
      if(is.empty(name)){name <<- 'CECATB' %>% paste0(sample(10000:99999, 1))}
    }
))

#' @title Target encoder (category_encoders)
#' @name ENC.CATEGORY_ENCODERS.TE-class
#' @aliases ENC.CATEGORY_ENCODERS.TE
#' @description
#' Wraps \code{category_encoders.target_encoder.TargetEncoder}. Its arguments can be passed.
#' Requires Python with the module named below, reachable through reticulate. Set \code{python_address} to choose the Python executable; see \code{\link{python_setup}} and \code{vignette("rml-troubleshooting")} if the module is not found.
#'
#' Inherits from \code{\link{ENC.CATEGORY_ENCODERS-class}}.
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), city = sample(c('A', 'B', 'C'), 300, replace = TRUE))
#' y = as.numeric(X$a + (X$city == 'A') + rnorm(300) > 0)
#' m = ENC.CATEGORY_ENCODERS.TE()
#' m$fit(X, y)
#' head(m$predict(X))
#' }
#' @export ENC.CATEGORY_ENCODERS.TE
ENC.CATEGORY_ENCODERS.TE = setRefClass(
  'ENC.CATEGORY_ENCODERS.TE',
  contains = 'ENC.CATEGORY_ENCODERS',
  methods = list(
    initialize = function(...){
      callSuper(model.module = 'target_encoder', model.class = 'TargetEncoder', ...)
      type             <<- 'Encoder'
      description      <<- 'Target Encoder'
      package          <<- 'category_encoders'
      package_language <<- 'Python'

      if(is.empty(name)){name <<- 'CETE' %>% paste0(sample(10000:99999, 1))}
    }
  ))



#' @title James-Stein encoder (category_encoders)
#' @name ENC.CATEGORY_ENCODERS.JSTN-class
#' @aliases ENC.CATEGORY_ENCODERS.JSTN
#' @description
#' Wraps \code{category_encoders.james_stein.JamesSteinEncoder}. Its arguments can be passed.
#' Requires Python with the module named below, reachable through reticulate. Set \code{python_address} to choose the Python executable; see \code{\link{python_setup}} and \code{vignette("rml-troubleshooting")} if the module is not found.
#'
#' Inherits from \code{\link{ENC.CATEGORY_ENCODERS-class}}.
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), city = sample(c('A', 'B', 'C'), 300, replace = TRUE))
#' y = as.numeric(X$a + (X$city == 'A') + rnorm(300) > 0)
#' m = ENC.CATEGORY_ENCODERS.JSTN()
#' m$fit(X, y)
#' head(m$predict(X))
#' }
#' @export ENC.CATEGORY_ENCODERS.JSTN
ENC.CATEGORY_ENCODERS.JSTN = setRefClass(
  'ENC.CATEGORY_ENCODERS.JSTN',
  contains = 'ENC.CATEGORY_ENCODERS',
  methods = list(
    initialize = function(...){
      callSuper(model.module = 'james_stein', model.class = 'JamesSteinEncoder', ...)
      type             <<- 'Encoder'
      description      <<- 'James Stein Encoder'
      package          <<- 'category_encoders'
      package_language <<- 'Python'

      if(is.empty(name)){name <<- 'CEJSTN' %>% paste0(sample(10000:99999, 1))}
    }
))


#' @title Helmert encoder (category_encoders)
#' @name ENC.CATEGORY_ENCODERS.HLMRT-class
#' @aliases ENC.CATEGORY_ENCODERS.HLMRT
#' @description
#' Wraps \code{category_encoders.helmert.HelmertEncoder}. Its arguments can be passed.
#' Requires Python with the module named below, reachable through reticulate. Set \code{python_address} to choose the Python executable; see \code{\link{python_setup}} and \code{vignette("rml-troubleshooting")} if the module is not found.
#'
#' Inherits from \code{\link{ENC.CATEGORY_ENCODERS-class}}.
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), city = sample(c('A', 'B', 'C'), 300, replace = TRUE))
#' y = as.numeric(X$a + (X$city == 'A') + rnorm(300) > 0)
#' m = ENC.CATEGORY_ENCODERS.HLMRT()
#' m$fit(X, y)
#' head(m$predict(X))
#' }
#' @export ENC.CATEGORY_ENCODERS.HLMRT
ENC.CATEGORY_ENCODERS.HLMRT = setRefClass(
  'ENC.CATEGORY_ENCODERS.HLMRT',
  contains = 'ENC.CATEGORY_ENCODERS',
  methods = list(
    initialize = function(...){
      callSuper(model.module = 'helmert', model.class = 'HelmertEncoder', ...)
      type             <<- 'Encoder'
      description      <<- 'Helmert Encoder'
      package          <<- 'category_encoders'
      package_language <<- 'Python'

      if(is.empty(name)){name <<- 'CEHLMRT' %>% paste0(sample(10000:99999, 1))}
    }
))

#' @title Feature encoder: numeric aggregates per category
#' @name ENC.RML.FE-class
#' @aliases ENC.RML.FE
#' @description
#' For each categorical column and each numeric column, outputs the aggregate (e.g. mean) of the numeric column within
#' the row's category, as learned in training. Output columns are named \code{<categorical>_<numeric>}.
#'
#' Inherits from \code{\link{MODEL-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{aggregator}}{Name of the aggregate function. Default \code{'mean'}.}
#'   \item{\code{categoricals}}{Categorical columns to use. Default all.}
#'   \item{\code{action_by_original}}{Optional function of (aggregate, original value), e.g. a difference, applied to each output.}
#' }
#' @examples
#' X = data.frame(a = rnorm(300), city = sample(c('A', 'B', 'C'), 300, replace = TRUE))
#' y = as.numeric(X$a + (X$city == 'A') + rnorm(300) > 0)
#' m = ENC.RML.FE(aggregator = 'mean')
#' m$fit(X, y)
#' head(m$predict(X))
#' @export ENC.RML.FE
ENC.RML.FE = setRefClass(
  'ENC.RML.FE', contains = 'MODEL',
  methods = list(
    initialize = function(...){
      callSuper(...)
      type             <<- 'Encoder'
      description      <<- 'Feature Encoder'
      package          <<- 'rml'
      package_language <<- 'R'
      reserved_words   <<- c(reserved_words, 'action_by_original')
      
      config$aggregator <<- verify(config$aggregator, 'character', default = 'mean')
      
      if(is.empty(name)){name <<- 'FE' %>% paste0(sample(10000:99999, 1))}
    },
    
    model.fit = function(X, y = NULL){
      if(!fitted){
        catfigs = objects$features %>% filter(fclass %in% c('character', 'integer', 'factor')) %>% pull(fname) %>% intersect(wt_colnames(X))
        if(!is.null(config$categoricals)) {catfigs = catfigs %^% config$categoricals}
        
        numfigs = objects$features %>% filter(fclass %in% c('numeric', 'integer')) %>% pull(fname) %>% intersect(wt_colnames(X)) %>% setdiff(catfigs)
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
#' @title Target encoder
#' @name ENC.RML.TE-class
#' @aliases ENC.RML.TE
#' @description
#' Replaces each categorical (character, factor or integer) column by the rate of class 1 within its category in the training
#' data. Categories not seen in training get the median rate.
#'
#' Inherits from \code{\link{MODEL-class}}.
#' @section Configuration:
#' \describe{
#'   \item{\code{smoothing}}{Pulls the rate of small categories towards the overall rate: the encoded value is
#'     \code{(sum of y + smoothing * overall rate) / (count + smoothing)}. Default 0 (the plain rate). Values around
#'     10 to 50 reduce overfitting when many categories have few rows.}
#' }
#' @examples
#' X = data.frame(a = rnorm(300), city = sample(c('A', 'B', 'C'), 300, replace = TRUE))
#' y = as.numeric(X$a + (X$city == 'A') + rnorm(300) > 0)
#' m = ENC.RML.TE()
#' m$fit(X, y)
#' head(m$predict(X))
#' @export ENC.RML.TE
ENC.RML.TE = setRefClass('ENC.RML.TE', contains = 'MODEL',
                         methods = list(
                           initialize = function(...){
                             callSuper(...)
                             type             <<- 'Encoder'
                             description      <<- 'Target Encoder'
                             package          <<- 'rml'
                             package_language <<- 'R'
                             
                             if(is.empty(name)){name <<- 'TE' %>% paste0(sample(10000:99999, 1))}
                           },
                           
                           model.fit = function(X, y){
                             if(!fitted){
                               # todo: remove this conversion when widetable supports cbind, group_by and summarise
                               if(inherits(X, 'WIDETABLE')){X = wt_as_data_frame(X)}
                               objects$features <<- objects$features %>% filter(fclass %in% c('integer', 'character', 'factor'))
                               X = X[objects$features$fname]
                               rutils::assert(ncol(X) > 0, "No nominal or ordinal features left for encoding!")
                               objects$model <<- list()
                               # smoothing m: ratio = (sum of y + m * overall rate) / (count + m); 0 gives the plain rate
                               m     = verify(config$smoothing, c('numeric', 'integer'), lengths = 1, domain = c(0, Inf), default = 0)
                               prior = mean(y)
                               for(col in colnames(X)){
                                 objects$model[[col]] <<- cbind(X, label = y) %>% group_by(!!sym(col)) %>%
                                   summarise(ratio = (sum(label) + m*prior)/(length(label) + m))
                               }
                             }
                           },
                           
                           model.predict = function(X){
                             XOUT  = X
                             for(col in objects$features$fname){
                               cn = 'ratio' %>% {names(.) <- col;.}
                               XOUT %<>% left_join(objects$model[[col]], by = col) %>% column_drop(col) %>% column_rename(cn) %>% na2median
                             }
                             return(XOUT)
                           }
                         )
)

# Todo: does not save joblib for Python segment models
#' @title Model encoder: one model per category
#' @name ENC.RML.ME-class
#' @aliases ENC.RML.ME
#' @description
#' For each categorical column, fits a separate model (of class \code{model.class}) on the rows of each category value
#' with more than \code{min_rows} rows, plus a global model. Each row is scored by the model of its category (or the global one).
#' Outputs one column per categorical column.
#'
#' Inherits from \code{\link{MODEL-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{model.class}}{Class name of the segment models, e.g. \code{'CLS.STATS.LR'}.}
#'   \item{\code{model.config}}{List of arguments for the segment models.}
#'   \item{\code{min_rows}}{Minimum rows for a category to get its own model. Default 100.}
#'   \item{\code{segmentation_features}}{Categorical columns to segment by. Default all.}
#' }
#' @examples
#' X = data.frame(a = rnorm(300), city = sample(c('A', 'B', 'C'), 300, replace = TRUE))
#' y = as.numeric(X$a + (X$city == 'A') + rnorm(300) > 0)
#' m = ENC.RML.ME(model.class = 'CLS.STATS.LR', model.config = list(features.include = 'a', verbose = 0), min_rows = 50)
#' m$fit(X, y)
#' head(m$predict(X))
#' @export ENC.RML.ME
ENC.RML.ME = setRefClass('ENC.RML.ME', contains = 'MODEL',
                         methods = list(
                           initialize = function(...){
                             callSuper(...)
                             type             <<- 'Encoder'
                             description      <<- 'Model Encoder'
                             package          <<- 'rml'
                             package_language <<- 'R'
                             reserved_words   <<- c(reserved_words, 'segmentation_features')
                             
                             config$model.class  <<- verify(config$model.class, 'character', lengths = 1, default = 'CLS.STATS.LR')
                             config$model.config <<- verify(config$model.config, 'list', default = list(verbose = 0))
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
                               objects$model[['__global__']] <<- do.call(what = config$model.class, args = config$model.config)
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
                                   objects$model[[col]][[vlc]] <<- do.call(what = config$model.class, args = config$model.config)
                                   # the segment column is constant within a segment, so it is left out
                                   objects$model[[col]][[vlc]]$fit(X[www, setdiff(colnames(X), col), drop = FALSE], y[www])
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
                                 left_join(X %>% group_by(!!sym(col)) %>% do({bibi(.)}), by = '__rowid__') %>% select(value) %>% column_rename(cn)
                               
                               if(is.null(XOUT)){XOUT = df[,1, drop = F]} else {XOUT %<>% cbind(df[,1, drop = F])}
                             }
                             # colnames(XOUT) <- NULL
                             return(XOUT)
                           }
                         ))

# previously: SEGMENTER.MODEL.BOOSTER
# Model Encoder Booster
#' @title Model encoder with fallback to the global model
#' @name ENC.RML.MEB-class
#' @aliases ENC.RML.MEB
#' @description
#' Like \code{\link{ENC.RML.ME-class}}, but holds out 30\% of the training rows and drops a category's model when the global
#' model scores better (gini) on that category.
#'
#' Inherits from \code{\link{ENC.RML.ME-class}}.
#' @export ENC.RML.MEB
ENC.RML.MEB =
  setRefClass('ENC.RML.MEB',
              contains = 'ENC.RML.ME',
              
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
