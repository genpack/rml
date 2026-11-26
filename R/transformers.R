#' @include abstract.R


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

#' @title Wrapper for scikit-learn transformers and models
#' @name TRM.SKLEARN-class
#' @aliases TRM.SKLEARN
#' @description
#' Superclass of all scikit-learn based classes. It creates \code{sklearn.<model.module>.<model.class>} with the
#' non-reserved config arguments, fits it on the numeric matrix of the input and calls its \code{transform()} method
#' at prediction. Python models are saved with joblib by \code{model_save()}.
#' Requires Python with the module named below, reachable through reticulate. Set \code{python_address} to choose the Python executable.
#'
#' Inherits from \code{\link{MODEL-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{model.module}}{sklearn sub-module, e.g. \code{'preprocessing'}.}
#'   \item{\code{model.class}}{sklearn class, e.g. \code{'StandardScaler'}.}
#'   \item{\code{python_address}}{Path of the Python executable.}
#' }
#' @examples
#' \dontrun{
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = TRM.SKLEARN(model.module = 'preprocessing', model.class = 'StandardScaler')
#' m$fit(X, y)
#' head(m$predict(X))
#' }
#' @export TRM.SKLEARN
TRM.SKLEARN = setRefClass(
  'TRM.SKLEARN',
  contains = 'MODEL',
  methods = list(
    initialize = function(...){
      callSuper(...)
      
      if(!require(reticulate)) stop("Package 'reticulate' is not installed!")
      if(!is.null(config$python_address)){
        use_python(config$python_address)
      }
      
      type             <<- 'Transformer'
      description      <<- 'Superclass Wrapper for sklearn modules'
      package          <<- 'sklearn'
      package_language <<- 'Python'

      config$model.module <<- verify(config$model.module, 'character', lengths = 1, null_allowed = F)
      objects$module <<- reticulate::import(paste('sklearn', config[['model.module']], sep = '.'))
    },
    
    model.save = function(path = getwd()){
      callSuper(path)
      save_model_object(paste0(path, '/', name, '.joblib'))
      release_model()
    },
    
    model.load = function(path = getwd()){
      callSuper(path)
      fn   = paste0(path, '/', name, '.joblib')
      pass = file.exists(fn)
      rutils::assert(pass, paste0('File ', fn , ' does not exist!'))
      if(pass){load_model_object(fn)}
    },
    
    save_model_object = function(filename){
      joblib = reticulate::import('joblib')
      joblib$dump(objects$model, filename)
    },
    
    load_model_object = function(filename){
      joblib = reticulate::import('joblib')
      objects$module <<- reticulate::import(paste('sklearn', config[['model.module']], sep = '.'))
      objects$model  <<- joblib$load(filename)
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
    },

    model.fit = function(X, y){
      if(!fitted){
        objects$module <<- reticulate::import(paste('sklearn', config[['model.module']], sep = '.'))
        objects$model <<- do.call(objects$module[[config[['model.class']]]], args = config %>% rutils::list.remove(reserved_words))
        if(inherits(X, 'WIDETABLE')){X = wt_as_matrix(X)}
        objects$model$fit(X %>% data.matrix, y)
      }
    },
    
    model.predict = function(X){
      if(inherits(X, 'WIDETABLE')){X = wt_as_matrix(X)}
      retrieve_model()
      out = objects$model$transform(X %>% data.matrix) %>% as.data.frame()
      
      # Important note: column headers in the output are default for mappers (same as input column names). 
      # If your naming strategy is different, write your own model.predict() method for any wrapper inheriting from this class.
      if(ncol(out) == nrow(objects$features)){
        colnames(out) <- objects$features$fname
      } else {
        # transformers that create new columns (e.g. PolynomialFeatures) name them with get_feature_names_out()
        ns = try(as.character(objects$model$get_feature_names_out(objects$features$fname)), silent = TRUE)
        if(!inherits(ns, 'try-error') && length(ns) == ncol(out)){
          colnames(out) <- gsub(' ', '_X_', ns)
        }
      }
      return(out)
    }
))



#' @title Categorical concatenator
#' @name CATCONCATER-class
#' @aliases CATCONCATER
#' @description
#' Outputs one column joining the values of all categorical columns of a row with \code{'-'}.
#'
#' Inherits from \code{\link{MODEL-class}}.
#' @export CATCONCATER
CATCONCATER = setRefClass('CATCONCATER', contains = "MODEL",
   methods = list(
     initialize = function(...){
       callSuper(...)
       type             <<- 'Feature Generator'
       description      <<- 'Categorical Features Concater'
       package          <<- 'rml'
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

