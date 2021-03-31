#' @include abstract.R

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
      joblib = reticulate::import('joblib')
      joblib$dump(objects$model, paste0(path, '/', name, '.joblib'))
    },
    
    model.load = function(path = getwd()){
      callSuper(path)
      fn   = paste0(path, '/', name, '.joblib')
      pass = file.exists(fn)
      warnif(!pass, paste0('File ', fn , ' does not exist!'))
      if(pass){
        joblib = reticulate::import('joblib')
        objects$model  <<- joblib$load(fn)
        objects$module <<- reticulate::import(paste('sklearn', config[['model.module']], sep = '.'))
      }
    },

    model.fit = function(X, y){
      if(!fitted){
        objects$model <<- do.call(objects$module[[config[['model.class']]]], args = config %>% list.remove(reserved_words))
        if(inherits(X, 'WIDETABLE')){X = rbig::as.matrix(X)}
        objects$model$fit(X, y)
      }
    },
    
    model.predict = function(X){
      if(inherits(X, 'WIDETABLE')){X = rbig::as.matrix(X)}
      objects$model$transform(X)
    }
))



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

