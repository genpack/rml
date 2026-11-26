# 25 Oct 2019 'fitted', 'features.include' and 'transformer' added to reserved_words
# Todos:
# 1- Add imputer module to config to impute for missing values
# look at Recipes: https://conf20-intro-ml.netlify.app/slides/06-recipes.html#2
# 2- Add upsampling and downsampling modules

library(magrittr)

#' @title Abstract base class of all rml models
#' @name MODEL-class
#' @aliases MODEL
#' @description
#' \code{MODEL} is the root of the rml class hierarchy. Every classifier, regressor and transformer
#' (encoder, mapper, binner, feature generator, function transformer, ensembler) inherits from it.
#' Subclasses only implement \code{model.fit(X, y)} and \code{model.predict(X)}; \code{MODEL} provides
#' everything else: input column selection, transformers, gradient transformers, row sampling,
#' preprocessing, feature elimination, output naming, cross-validation, saving and copying.
#'
#' Arguments passed to the constructor (or inside \code{config = list(...)}) are stored in the
#' \code{config} field. Names listed in the \code{reserved_words} field are rml settings; any other name
#' is passed to the underlying algorithm (for example \code{max_depth} to xgboost).
#'
#' @field name \code{character} model name. Used as prefix of output columns and as file name when saving.
#' @field type \code{character} model type, e.g. \code{'Binary Classifier'}, \code{'Encoder'}.
#' @field package \code{character} name of the package providing the algorithm.
#' @field package_language \code{character} \code{'R'} or \code{'Python'}.
#' @field packages_required \code{character} packages loaded on parallel workers.
#' @field description \code{character} short description of the algorithm.
#' @field reserved_words \code{character} config names used by rml and not passed to the algorithm.
#' @field config \code{list} all settings of the model.
#' @field fitted \code{logical} TRUE once the model is trained.
#' @field transformers \code{list} of models whose outputs are the input features of this model.
#' @field gradient_transformers \code{list} of base models (or column names of \code{X}) this model boosts:
#'   they are boosted by this model, which learns to correct their output. Several outputs are combined by \code{gradient_transformers_aggregator}.
#' @field yin_transformers,yout_transformers \code{list} reserved for target transformation functions.
#' @field objects \code{list} fitted state: \code{model} (the trained object), \code{features} (table of the
#'   features used, with class and importance), \code{fitting_time} (when fitting finished), \code{fitting_duration} (seconds), \code{n_output}.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{fit(X, y)}}{Trains the model and all its transformers and gradient transformers. Does nothing if already fitted.}
#'   \item{\code{predict(X)}}{Returns a data.frame of model outputs, named \code{<name>_<output>}.}
#'   \item{\code{reset(reset_transformers = TRUE, reset_gradient_transformers = TRUE, set_features.include = TRUE)}}{Makes the model unfitted.
#'     With no transformers and \code{set_features.include = TRUE}, the features used so far are written to \code{features.include},
#'     so refitting uses the same features.}
#'   \item{\code{performance(X, y, metrics)}}{Score of the predictions against \code{y} (classifiers and regressors), see \code{\link{correlation}}.}
#'   \item{\code{performance.cv(X, y, metrics)}}{Repeated train/test cross-validation on a deep copy of the model.}
#'   \item{\code{deep_copy(name_suffix = NULL)}}{Copy of the model, its transformers and gradient transformers.}
#'   \item{\code{info.features()}, \code{info.model()}, \code{info.transformers()}, \code{info.edge_list()}}{Information about the features and the network of models.}
#'   \item{\code{info.network()}}{\code{list(nodes, links)} of the model network: transformers feed their parent, gradient transformers are boosted by it.}
#'   \item{\code{plot.network(plotter)}}{Draws the network: \code{'visNetwork'} (interactive, through rvis if installed, otherwise visNetwork) or \code{'igraph'} (static).}
#'   \item{\code{model.save(path)}, \code{model.load(path)}}{Save or load native model files; use \code{\link{model_save}} and \code{\link{model_load}}.}
#'   \item{\code{supports_gradient_transformers()}}{TRUE if training uses the output of gradient transformers.}
#' }
#'
#' @section Configuration:
#' Input columns:
#' \describe{
#'   \item{\code{features.include}, \code{features.exclude}}{Input columns to use or drop, before transformers run.}
#'   \item{\code{features.include.at}, \code{features.exclude.at}}{Columns to keep or drop after the transformers ran.}
#'   \item{\code{column_filters}}{List of filters on the feature table, each \code{list(column = , filter = )} with \code{filter} written
#'     as R code. Default \code{list(list(column = 'n_unique', filter = ' > 1'))} removes constant columns.}
#'   \item{\code{pp.coerce_integer_features}}{Convert numeric columns holding whole numbers to integer. Default FALSE.}
#' }
#' Output:
#' \describe{
#'   \item{\code{name_in_output}}{Prefix output column names with the model name. Default TRUE.}
#'   \item{\code{keep_columns}}{Also return the input columns the model did not use. Default FALSE.}
#'   \item{\code{keep_features}}{Also return the input columns the model used. Default FALSE.}
#'   \item{\code{pass_columns}, \code{remove_columns}}{Restrict which input columns are passed when either switch above is on.}
#'   \item{\code{save_predictions}}{Reuse the last prediction when \code{predict()} is called again with the same data. Default FALSE.}
#' }
#' Preprocessing:
#' \describe{
#'   \item{\code{pp.mask_missing_values}}{Value replacing missing values, in training and prediction.}
#'   \item{\code{pp.trim_outliers}}{Clip outliers of each column in the training data. Default FALSE. Related:
#'     \code{pp.trim_outliers.sd_threshold} (4), \code{pp.trim_outliers.adaptive} (FALSE), \code{pp.trim_outliers.recursive} (FALSE).}
#'   \item{\code{eda.enabled}}{Add moments, quantiles, missing and unique counts to \code{objects$features}. Default FALSE.}
#' }
#' Row sampling (binary targets):
#' \describe{
#'   \item{\code{smp.enabled}}{Turn sampling on. Default FALSE.}
#'   \item{\code{smp.method}}{\code{'upsample'} (default), \code{'downsample'} or \code{'smote'} (Python package smote_variants).}
#'   \item{\code{smp.class_ratio}}{Target share of class 1 after balancing.}
#'   \item{\code{smp.sample_ratio}, \code{smp.num_rows}}{Share or maximum number of rows kept.}
#'   \item{\code{smp.config}}{Arguments of the SMOTE oversampler; \code{model} selects the smote_variants class.}
#' }
#' Feature elimination:
#' \describe{
#'   \item{\code{fe.enabled}}{Drop features with importance not above the threshold and retrain. Default FALSE.}
#'   \item{\code{fe.importance_threshold}}{Threshold, default 0. \code{fe.quantile} sets it as a quantile of the importances instead.}
#'   \item{\code{fe.recursive}}{Repeat until no feature is below the threshold. Default FALSE.}
#' }
#' Target transformation and boosting:
#' \describe{
#'   \item{\code{yin_transformer_function}, \code{yin_transformer_arguments}}{Function (and extra argument) applied to \code{y} before training.}
#'   \item{\code{yout_transformer_function}, \code{yout_transformer_arguments}}{Function (and extra argument) applied to the output.}
#'   \item{\code{gradient_transformers_aggregator}}{Function combining several gradient transformer outputs per row. Default \code{mean}.}
#' }
#' Cross-validation and metrics:
#' \describe{
#'   \item{\code{metrics}}{Default metrics of \code{performance()}.}
#'   \item{\code{cv.ntrain}, \code{cv.ntest}}{Number of training samples (1) and test samples per training sample (5).}
#'   \item{\code{cv.train_ratio}, \code{cv.test_ratio}}{Share of rows for training (0.5) and of the remaining rows for testing (0.2).}
#'   \item{\code{cv.set}}{List of validation sets \code{list(list(X = , y = ))} used instead of random test samples.}
#'   \item{\code{cv.reset_transformer}}{Refit transformers in each cross-validation round. Default TRUE.}
#' }
#' Runtime:
#' \describe{
#'   \item{\code{verbose}}{0 silences progress messages. Default 1, or the value of \code{options(rml.verbose = )} so that \code{options(rml.verbose = 0)} silences all models of a session.}
#'   \item{\code{mc.num_cores}}{Cores used to fit and run transformers in parallel. Default 1.}
#'   \item{\code{remove_failed_transformers}}{Drop transformers that fail to fit instead of stopping. Default TRUE.}
#' }
#' @seealso \code{\link{CLASSIFIER-class}}, \code{\link{REGRESSOR-class}}, \code{vignette('rml-reference', package = 'rml')}
#' @examples
#' X = data.frame(a = rnorm(200), b = rnorm(200))
#' y = as.numeric(X$a + rnorm(200) > 0)
#' model = CLS.STATS.LR(name = 'lr', transformers = list(MAP.RML.ZFS(name = 'z')), verbose = 0)
#' model$fit(X, y)
#' head(model$predict(X))
#' model$objects$features
#' @export MODEL
#' @exportClass MODEL
MODEL = setRefClass('MODEL',
  fields = list(name = "character", type = "character", package = "character", package_language = "character", packages_required = "character", description = "character", reserved_words = "character",
                config = "list", fitted = 'logical', transformers = 'list', gradient_transformers = 'list', yin_transformers = 'list', yout_transformers = 'list', objects = "list"),

  methods = list(
    initialize          = function(..., name = character(), transformers = list(), gradient_transformers = list(), mother = NULL, features = NULL, pupils = NULL){
      callSuper(name = name)
      settings = list(...)
      ns       = names(settings)
      for (sn in ns){
        set = settings[[sn]]
        if(inherits(set, 'list') &  sn == 'config') {
          settings = settings %<==>% set
          settings[[sn]] <- NULL
        }
      }
      
      
      # Packages loaded on parallel workers (rml loads its own imports):
      packages_required <<- c('rml')

      reserved_words <<- c('keep_columns', 'keep_features', 
                         'name_in_output', 'metrics', 
                         'cv.ntrain', 'cv.ntest', 'cv.test_ratio','cv.train_ratio', 'cv.split_method', 
                         'cv.reset_transformer', 'cv.restore_model', 'cv.set', 
                         'sfs.enabled', 
                         'fe.enabled','fe.recursive', 'fe.importance_threshold', 'fe.quantile',
                         'mc.enabled', 'mc.num_cores',
                         'remove_failed_transformers',
                         'pp.coerce_integer_features',
                         'pp.trim_outliers', 'pp.trim_outliers.adaptive', 'pp.trim_outliers.recursive', 'pp.trim_outliers.sd_threshold',
                         'pp.mask_missing_values',
                         'model.class', 'model.config', 'model.module',
                         'eda.enabled', 
                         'verbose', 'pass_columns', 'remove_columns', 'name', 'column_filters',
                         # smp.enabled: boolean parameter. default = FALSE. Should I do any sampling of the training rows at all? If FALSE (Default) 
                         #              model will be trained by the entire training data table (X) without any changes.
                         'smp.enabled', 'smp.class_ratio', 'smp.sample_ratio', 'smp.num_rows', 'smp.method', 'smp.config',
                         'features.include', 'features.include.at', 'features.exclude', 'features.exclude.at',
                         'ts.enabled', 'ts.id_col', 'ts.time_col', 
                         'return_features', 'feature_subset_size', 'gradient_transformers_aggregator', 'save_predictions',
                         'yin_transformer_function', 'yin_transformer_arguments', 'yout_transformer_function', 'yout_transformer_arguments',
                         'metric', 'transformers', 'fitted', 'python_address')
      
      for(pn in c('keep_columns', 'keep_features', 'cv.restore_model', 'fe.enabled', 'mc.enabled', 
                  'pp.coerce_integer_features', 'pp.trim_outliers', 
                  'eda.enabled', 'smp.enabled', 'ts.enabled', 'save_predictions')){
        settings[[pn]] <- verify(settings[[pn]], 'logical', lengths = 1, domain = c(T,F), default = F)
      }
      
      # Note: if a transformer fails to fit, config parameter 'remove_failed_transformer' determines if the failed transformer should be removed or not.
      # if it is set to FALSE, then failed transformer remains among transformers. 
      # Then as part of the fitting procedure, the predict() method of the failed transformer will be called to transform the training set.
      # This will fail, but it does not stop the process but no columns will be generated in the transformed output.
      # Now, if there is another successfully fit transformer, there will be some columns in the transformed table and the main model will be fit based on that.
      # Now, for test, since the failed transformer still exists and is not fit, it's fit method is called as part of the predict procedure of the main model.
      # The failed transformer now is being fitted with the test set which is not right, however, 
      # if it fits successfully, it generates columns in the final output anyway, 
      # but those columns will not be read by the main model at prediction because it only reads columns which are specified 
      # in objects$features table. So no problem will happen.
      # The advantage of setting this parameter to False is that after resetting the model and fitting it with a different dataset
      # the failed transformer may not fail this time and be used in the main model.
      
      for(pn in c('cv.reset_transformer', 'name_in_output', 'remove_failed_transformers')){
        settings[[pn]] <- verify(settings[[pn]], 'logical', lengths = 1, domain = c(T,F), default = T)
      }
      
      settings$verbose <- verify(settings$verbose, c('numeric', 'integer'), lengths = 1, default = getOption('rml.verbose', 1)) %>% as.integer
      settings$column_filters  <- verify(settings$column_filters, 'list', default = list(list(column = 'n_unique', filter = ' > 1')))

      # Default metrics are set by subclasses (CLASSIFIER: 'pearson', REGRESSOR: 'mae')
      settings$metrics   <- verify(settings$metrics,  'character', null_allowed = T)
      settings$cv.split_method <- verify(settings$cv.split_method, lengths = 1, 'character', default = 'shuffle')
      
      settings$cv.ntest  <- verify(settings$cv.ntest , c('numeric', 'integer'), lengths = 1, domain = c(1, Inf), default = 5) %>% as.integer
      settings$cv.ntrain <- verify(settings$cv.ntrain, c('numeric', 'integer'), lengths = 1, domain = c(1, Inf), default = 1) %>% as.integer
      settings$cv.train_ratio <- verify(settings$cv.train_ratio, c('numeric', 'integer'), lengths = 1, domain = c(0, 1), default = 0.5) %>% as.numeric
      settings$cv.test_ratio  <- verify(settings$cv.test_ratio, c('numeric', 'integer') , lengths = 1, domain = c(0, 1), default = 0.2) %>% as.numeric
      settings$fe.importance_threshold  <- verify(settings$fe.importance_threshold, c('numeric', 'integer') , lengths = 1, default = 0) %>% as.numeric

      # if(is.null(settings$ts.id_col)){settings$ts.id_col = 'caseID'}
      # if(is.null(settings$ts.time_col)){settings$ts.time_col = 'time'}
      if(is.null(settings$gradient_transformers_aggregator)){settings$gradient_transformers_aggregator = mean}

      config       <<- settings
      fitted       <<- FALSE
      transformers <<- transformers %>% {if(inherits(.,'list')) . else list(.)}
      gradient_transformers <<- gradient_transformers %>% {if(inherits(.,'list')) . else list(.)}
      objects$mother        <<- mother
      objects$features      <<- features
      objects$pupils        <<- pupils
    },
    
    retrieve_model = function(){
      for(tr in transformers){tr$retrieve_model()}
      for(tr in gradient_transformers){tr$retrieve_model()}
    },
    
    keep_model = function(){
      for(tr in transformers){tr$keep_model()}
      for(tr in gradient_transformers){tr$keep_model()}
    },

    # deletes temporary file if model object is temporarily saved
    release_model = function(){
      if(!is.null(objects$model_filename)){
        if(file.exists(objects$model_filename)){
          unlink(objects$model_filename)
        }
        objects$model_filename <<- NULL
      }
      for(tr in transformers){tr$release_model()}
      for(tr in gradient_transformers){tr$release_model()}
    },
    
    reset               = function(reset_transformers = T, reset_gradient_transformers = T, set_features.include = T){
      fitted <<- FALSE
      # A fitted model without transformers keeps the features it used (after elimination) for the next fit.
      # An unfitted model has no feature table, so its features.include is left as it is
      # (otherwise a second reset() would set it to NULL and the model would use all columns).
      if(set_features.include & (length(transformers) == 0) & !is.null(objects$features$fname)){
        config$features.include <<- objects$features$fname
      }
      objects$features <<- NULL
      objects$model    <<- NULL
      if(!is.null(objects$model_filename)){
        unlink(objects$model_filename)
        objects$model_filename <<- NULL
      }
      objects$saved_pred <<- NULL
      if (reset_transformers & !is.empty(transformers)){
        for (transformer in transformers) transformer$reset(reset_transformers = T, reset_gradient_transformers = reset_gradient_transformers, set_features.include = set_features.include)
      }
      if (reset_gradient_transformers & !is.empty(gradient_transformers)){
        for (transformer in gradient_transformers) transformer$reset(reset_transformers = reset_transformers, reset_gradient_transformers = T, set_features.include = set_features.include)
      }
    },

    info.features       = function(include_transformers = T){
      fet = objects$features
      if(include_transformers){
        for(tr in transformers){
          fet %<>% rbind(tr$info.features())
        }
      }
      return(fet)
    },

    canuse_saved_prediction = function(X){
      permit = F
      if(!is.null(objects$saved_pred)){
        X      = X[numerics(X)]
        permit = identical(colSums(X), objects$saved_pred$CSUMS)
        permit = permit & identical(rowSums(X), objects$saved_pred$RSUMS)
        permit = permit & identical(X[objects$saved_pred$RNDM, ], objects$saved_pred$XSAMP)
      }
      return(permit)
    },

    keep_prediction      = function(X, Y){
      X    = X[numerics(X)]
      rnd  = sequence(nrow(X)) %>% sample(size = ncol(X), replace = T)
      objects$saved_pred <<- list(CSUMS = colSums(X), RSUMS = rowSums(X), XOUT = Y, RNDM = rnd, XSAMP = X[rnd,])
    },

    predict              = function(X){
      if(!fitted) stop(paste('from', name, 'of type', type, ':', 'Model not fitted!', '\n'))
      if(inherits(X, 'matrix')){X %<>% as.data.frame}
      XORG = transform_x(X)
      zero = objects$features$fname %-% wt_colnames(XORG)
      if(length(zero) > 0){
        for(z in zero) XORG[,z] <- 0
        cat('Warning from ', name, ': Features ', paste(zero, collapse = ','), ' were not generated by transformers! Filled with zero.', '\n')
      }
      XFET = XORG[objects$features$fname]

      cusp = config$save_predictions
      if(cusp){
        cusp = canuse_saved_prediction(XFET)
      }
      if(cusp){
        XOUT = objects$saved_pred$XOUT
      } else {
        # .self$retrieve_model()
        
        if(!is.null(config$pp.mask_missing_values)){
          # This will edit the table, so WideTables cannot be used yet.
          if(inherits(XFET, 'WIDETABLE')){XFET = wt_as_data_frame(XFET)}
          for(i in sequence(ncol(XFET))){
            wna = which(is.na(XFET[[i]]))
            if(length(wna) > 0){
              XFET[wna, i] <- config$pp.mask_missing_values[1] %>% rutils::coerce(class(XFET[wna, i])[1])
            }
          }
        }
        
        XOUT = .self$model.predict(XFET)
        if(config$save_predictions){
          keep_prediction(XFET, XOUT)
        }
      }

      XOUT = transform_yout(X, XOUT)
      if(is.null(colnames(XOUT))){
        colnames(XOUT) <- chif(ncol(XOUT) == nrow(objects$features), objects$features$fname, chif(ncol(XOUT) == 1, '', as.character(sequence(ncol(XOUT)))))
      }
      colnames(XOUT)[which(colnames(XOUT) %in% c('.'))] <- ''
      if((ncol(XOUT) > 0) & config$name_in_output) colnames(XOUT) <- name %>% paste(colnames(XOUT), sep = ifelse(colnames(XOUT) == '', '', '_'))

      objects$n_output <<- ncol(XOUT)
      treat(XOUT, XFET, XORG)
    },

    treat                = function(out, fet, org){
      if(!is.null(config$pass_columns)) org = org[config$pass_columns]
      if(!is.null(config$remove_columns)) org = org[colnames(org) %-% config$remove_columns]

      if(config$keep_columns)
        if(config$keep_features) return(cbind(org, out))
        else return(cbind(org %>% column_drop(colnames(fet)), out))
      else if (config$keep_features){
        # if(!is.null(config$features.include)){
        #   extra = config$features.include %-% colnames(fet)
        #   return(cbind(fet, org[extra], out))
        # } else return(cbind(fet, out))
        return(cbind(fet, out))
      }
           else return(out)
    },

    # Fitting with feature elimination (FE). Works only for model fitters which generate feature importance
    fit.fe = function(X, y){
      .self$model.fit(X, y)
      if('importance' %in% colnames(objects$features)){
        if(!is.null(config$fe.quantile)){
          threshold = objects$features$importance %>% quantile(probs = config$fe.quantile)
        } else {
          threshold = config$fe.importance_threshold
        }
        fns = objects$features$fname
        ftk = fns[which(objects$features$importance > threshold)] # features to keep
        fte = fns %-% ftk
        while(length(fte) > 0){
          objects$features <<- objects$features %>% filter(fname %in% ftk)
          if(is.empty(objects$features)){
            # .self$fit.distribution(X, y); fte = c()
            stop('No features left for training the model!')
          } else {
            .self$model.fit(X[objects$features$fname], y)
            fns = objects$features$fname
            ftk = fns[which(objects$features$importance > threshold)] # features to keep
            fte = chif(config$fe.recursive, fns %-% ftk, c())
          }
        }

        if(is.empty(objects$features)){
          cat(name, ': ', 'No features left after elimination! Distribution fitted for output variable.', '\n')
        }
      }
    },

    fit.quad = function(X, y){
      fit.fe(X, y)
      clmns = wt_colnames(X)
      XP    = X[objects$features$fname]
      for(i in 1:ncol(X)){
        v  = X[,i]
        Xi <- X %>% as.matrix %>% apply(2, function(u) u*v) %>% as.data.frame %>% {colnames(.) <- clmns %>% paste(clmns[i], sep = 'X');.}
        XP = XP %>% cbind(Xi)
        objects$features <<- wt_colnames(XP) %>% sapply(function(i) XP %>% pull(i) %>% class) %>% as.data.frame %>% {colnames(.)<-'fclass';.} %>% rownames2Column('fname') %>% mutate(fname = as.character(fname), fclass = as.character(fclass))
        fit.fe(XP, y)
        XP  = XP[objects$features$fname]
      }
    },

    fit.distribution = function(X = NULL, y){
      out = outliers(y); while(!is.empty(out)){y = y[-out]; out = outliers(y)}
      objects$model <<- list(family = 'normal', mean = mean(y, na.rm = T), sd = sd(y, na.rm = T))
    },

    predict.distribution = function(X){
      N = try(nrow(X), silent = T)
      if(!inherits(N, 'integer')) N = as.integer(N)
      # rnorm(N, objects$model$mean, objects$model$sd) %>% as.data.frame
      rep(objects$model$mean, N) %>% as.data.frame
    },

    # TRUE if the model's training uses the output of its gradient transformers.
    # Regressors and non-classifier models always can (the residual is computed in transform_yin).
    # Classifiers need algorithm-specific loss changes; override this method in your own class once supported.
    supports_gradient_transformers = function(){
      !inherits(.self, 'CLASSIFIER') || inherits(.self, gb_supporting_classifiers)
    },

    fit = function(X, y = NULL){
      fit_start = Sys.time(); was_fitted = fitted
      if(!fitted){
        if(length(gradient_transformers) > 0){
          assert(supports_gradient_transformers(), sprintf("Model %s of class %s does not support gradient transformers yet. Supported classifiers: %s", name, class(.self)[1], paste(gb_supporting_classifiers, collapse = ', ')))
        }
        if(inherits(X, 'matrix')){X %<>% as.data.frame}
        y = transform_yin(X, y)
        if(!is.null(config[['features.include']])){X = X[config$features.include %^% wt_colnames(X)]}
        if(!is.null(config[['features.exclude']])){X = X[wt_colnames(X) %-% config$features.exclude]}
        X = transform_x(X, y)
        if(!is.null(config[['features.include.at']])){X = X[config$features.include.at %^% wt_colnames(X)]}
        if(!is.null(config[['features.exclude.at']])){X = X[wt_colnames(X) %-% config$features.exclude.at]}
        if(config$smp.enabled){
          # Row sampling must keep the gradient attribute (output of gradient transformers) aligned with y:
          y_grad = attr(y, 'gradient')
          smp_method <- verify(config$smp.method, 'character', domain = c('upsample', 'downsample', 'smote'), default = 'upsample') 

          actual_ratio = mean(y)
          n_train_rows = length(y)
          
          if(!is.null(verify(config$smp.class_ratio, 'numeric', domain = c(.Machine$double.eps, 1 - .Machine$double.eps), null_allowed = T))){
            if(smp_method == 'downsample'){
              if(config$smp.class_ratio >= actual_ratio){
                # downsample negative class, keep all positive class:
                w1 = which(y == 1)
                n1 = length(w1)
                rr = config$smp.class_ratio
                w0 = which(y == 0) %>% sample(as.integer(n1*(1.0 - rr)/rr))
              } else {
                # downsample positive class, keep all negative class:
                w0 = which(y == 0)
                n0 = length(w0)
                rr = config$smp.class_ratio
                w1 = which(y == 1) %>% sample(as.integer(n0*(1.0 - rr)/rr))
              }
            } else if(smp_method == 'upsample'){
              if(config$smp.class_ratio >= actual_ratio){
                # upsample positive class, keep all negative class:
                w0 = which(y == 0)
                n0 = length(w0)
                rr = config$smp.class_ratio
                w1 = which(y == 1) %>% sample(as.integer(n0*rr/(1.0 - rr)), replace = T)
              } else {
                # upsample negative class, keep all positive class:
                w1 = which(y == 1)
                n1 = length(w1)
                rr = config$smp.class_ratio
                w0 = which(y == 0) %>% sample(as.integer(n1*(1.0 - rr)/rr), replace = T)
              }
            } else if (smp_method == 'smote'){
              assert(is.null(y_grad), "SMOTE sampling cannot be used with gradient transformers: synthetic rows have no gradient transformer output. Use 'upsample' or 'downsample' instead.")
              sv = reticulate::import('smote_variants')
              smote_model = verify(config$smp.config[['model']], 'character', domain = names(sv), lengths = 1, default = 'distance_SMOTE')
              config$smp.config[['proportion']] <<- verify(config$smp.config[['proportion']], 'numeric', lengths = 1, default = config$smp.class_ratio/(1.0 - config$smp.class_ratio))
              
              oversampler = do.call(sv[[smote_model]], config$smp.config %>% list.remove('model'))
              res = oversampler$sample(X %>% as.matrix, y)
              X  = res[[1]] %>% as.data.frame %>% {colnames(.) <- colnames(X);.}
              y  = res[[2]] %>% as.numeric
              w0 = which(y == 0)
              w1 = which(y == 1)
            } else {stop('Unknown sampling method specified!')}
            ind = c(w0, w1) %>% sample(length(c(w0, w1))) 
            # X = X[ind,]; y = y[ind]
            n_train_rows = length(ind)
          } else {ind = sequence(length(y))}
          
          n_rows = verify(config$smp.num_rows, c('numeric', 'integer'), lengths = 1, domain = c(1, Inf), default = n_train_rows) %>% 
            min(n_train_rows) %>% {.*verify(config$smp.sample_ratio, 'numeric', lengths = 1, domain = c(0, 1), default = 1.0)} %>% as.integer
          
          ind_2 = n_train_rows %>% sequence %>% sample(n_rows)
          X = X[ind[ind_2],]; y = y[ind[ind_2]]
          if(!is.null(y_grad)){attr(y, 'gradient') <- y_grad[ind[ind_2]]}
        }
        
        # This code chunk must only be here. Please don't relocate it from here!
        if(config$pp.coerce_integer_features){
          X %<>% int_ordinals
        }
        assert(ncol(X) > 0, 'No column found in the training dataset!')

        if(inherits(X, 'WIDETABLE')){
          objects$features <<- X$meta %>% distinct(column, .keep_all = T) %>% select(fname = column, fclass = class, n_unique = n_unique)
        } else {
          objects$features <<- data.frame(fname = colnames(X), fclass = vapply(as.list(X), function(v) class(v)[1], character(1)), stringsAsFactors = F, row.names = NULL)
        }
        nums = objects$features$fclass %in% c('numeric', 'integer')
        cats = objects$features$fclass %in% c('character', 'integer', 'factor')
        if((sum(nums) > 0) & (config$eda.enabled)){
          numinfo = feature_info_numeric(X)
          objects$features <<- objects$features %>% left_join(numinfo, by = 'fname')
        }
        if((sum(cats) > 0) & (config$eda.enabled)){
          catinfo = feature_info_categorical(X)
          objects$features <<- objects$features %>% merge(catinfo, all = T)
        }
        
        if(inherits(config$column_filters, 'list')){
          for(fitem in config$column_filters){
            verify(fitem, 'list', names_include = c('column', 'filter'))
            assert(is.character(fitem$column), "Element 'column' must be character. %s was of class %s!" %>% sprintf(fitem$column, class(fitem$column)[1]))
            assert(is.character(fitem$filter), "Element 'filter' must be character. %s was of class %s!" %>% sprintf(fitem$filter, class(fitem$filter)[1]))
            if((fitem$column == 'n_unique') & !('n_unique' %in% colnames(objects$features))){
              objects$features$n_unique <<- vapply(objects$features$fname, function(x) length(unique(X[[x]])), integer(1), USE.NAMES = F)
            }
            script = 'objects$features'
            script %<>% paste("filter(%s %s)" %>% sprintf(fitem$column, fitem$filter), sep = " %>% ")
            objects$features <<- parse(text = script) %>% eval
          }
          X = X[objects$features$fname]
        }
        assert(nrow(objects$features) > 0, 'No features left for training!')
        
        # Apply preprocessing: (Don't take preprocessing to the transform_x method again. It causes problems with WideTables. Furthermore, they should not be applied on test. mask_missing_values is an exception which is applied to test in predict method)
        # todo: missing values should be treated differently for each column or groups of columns or classes of columns
        # todo: missing values could be imputed by aggregations of non-missing values like mean, median or most_frequent
        if(!is.null(config$pp.mask_missing_values)){
          # This will edit the table, so WideTables cannot be used yet.
          if(inherits(X, 'WIDETABLE')){X = wt_as_data_frame(X)}
          for(i in sequence(ncol(X))){
            wna = which(is.na(X[[i]]))
            if(length(wna) > 0){
              X[wna, i] <- config$pp.mask_missing_values[1] %>% rutils::coerce(class(X[wna, i])[1])
            }
          }
        }
        
        # todo: remove outliers can be considered as a transformer as well. 
        # Add transformers (transformer type: preprocessor)
        # This way of outlier trimming is applied only on the training set and not on the test set
        # If you use outlier trimmer as a transformer, it will be applied on both test and train
        if(config$pp.trim_outliers){
          adapt = verify(config$pp.trim_outliers.adaptive, 'logical', default = F)
          recur = verify(config$pp.trim_outliers.recursive, 'logical', default = F)
          sdcut = verify(config$pp.trim_outliers.sd_threshold, 'numeric', domain = c(0,Inf), default = 4)
          X %<>% trim_outliers(sd_threshold = sdcut, adaptive = adapt, recursive = recur)
        }

        if(config$fe.enabled) {
          config$fe.recursive <<- verify(config$fe.recursive, 'logical', lengths = 1, domain = c(T,F), default = F)
          fit.fe(X, y)
        } else {.self$model.fit(X, y)}
        # todo: if(config$quad.enabled) {fit.quad(X, y)}
      }
      fitted <<- TRUE
      objects$fitting_time <<- Sys.time()
      # seconds spent fitting this model, including its transformers and gradient transformers
      if(!was_fitted){objects$fitting_duration <<- as.numeric(difftime(objects$fitting_time, fit_start, units = 'secs'))}
    },

    transform_x = function(X, y = NULL){
      
      nt = length(transformers)
      if(nt > 0){
        ## Fitting:
        transformers <<- fit_models(transformers, X = X, y = y, num_cores = config$mc.num_cores, verbose = config$verbose, 
                                      remove_failed_models = config$remove_failed_transformers)
        # # (uft: unfitted transformers)
        # uft = transformers %>% lapply(function(x) {!x$fitted}) %>% unlist %>% which
        # num_cores = 1
        # if(config$mc.enabled){
        #   ncores_available = parallel::detectCores()
        #   num_cores <- verify(config$mc.num_cores, c('numeric', 'integer'), lengths = 1, domain = c(1, ncores_available), default = ncores_available - 1) %>% as.integer
        # }
        # if(config$mc.enabled & (num_cores > 1) & (length(uft) > 1)){
        #   requirements = transformers %>% lapply(function(x) x$packages_required) %>% unlist %>% unique
        #   cl  = rutils::setup_multicore(n_jobs = num_cores)
        #   if(!config$silent){cat('\n', 'Fitting  %s transformers ... ' %>% sprintf(length(uft)))}
        #   transformers <<- foreach(transformer = transformers, 
        #                            .combine = c, .packages = requirements, 
        #                            .errorhandling = rutils::chif(config$remove_failed_transformers, 'remove', 'stop')) %dopar% {
        #     if(!transformer$fitted){
        #       transformer$fit(X, y)
        #     }
        #     gc()
        #     list(transformer)
        #   }
        #   stopCluster(cl)
        #   gc()
        #   if(!config$silent){cat('Done!')}
        # } else {
        #   for(i in uft){
        #       transformer = transformers[[i]]
        #       if(!config$silent){
        #         cat('\n', 'Fitting transformer %s of type %s: %s ... ' %>% sprintf(transformer$name, transformer$type, transformer$description))
        #       }
        #       res = try(transformer$fit(X, y), silent = config$remove_failed_transformers)
        #       if(!config$silent){
        #         rutils::warnif(inherits(res, 'try-error'), sprintf("Transformer %s failed to fit!", transformer$name), as.character(res))
        #         cat('Done!')
        #       }
        #     }  
        # }
        # ## Remove unfitted transformers:
        # # ft: fitted transformers
        # ft = transformers %>% lapply(function(x) {x$fitted}) %>% unlist %>% which
        # nt = length(ft)
        # warnif(nt < length(transformers), sprintf("%s transformers failed to fit and removed!", length(transformers) - nt))
        # transformers <<- transformers %>% list.extract(ft)
        # assert(nt == length(transformers))
        nt = length(transformers)
      }
      if(nt > 0){
        ## Prediction:
        XT = predict_models(transformers, X = X, num_cores = config$mc.num_cores, verbose = config$verbose)
        # if(config$mc.enabled & (num_cores > 1) & (nt > 1)){
        #   requirements = transformers %>% lapply(function(x) x$packages_required) %>% unlist %>% unique
        #   cl = rutils::setup_multicore(n_jobs = num_cores)
        #   if(!config$silent){cat('\n', 'Generate transformed columns from %s transformers ... ' %>% sprintf(nt))}
        #   XT = foreach(transformer = transformers, .combine = cbind, .packages = requirements, 
        #                .errorhandling = rutils::chif(config$remove_failed_transformers, 'remove', 'stop')) %dopar% {
        #     gc()
        #     transformer$predict(X)
        #   }
        #   stopCluster(cl)
        #   gc()
        #   if(!config$silent){
        #     cat('Done!')
        #   }
        # } else {
        #   for(i in sequence(nt)){
        #     transformer = transformers[[i]]
        #     if(!config$silent){
        #       cat('\n', 'Generate transformed columns from transformer %s ... ' %>% sprintf(transformer$name))
        #     }
        #     if(i == 1){
        #       XT = transformer$predict(X)
        #     } else {
        #       XT = cbind(XT, transformer$predict(X) %>% {.[colnames(.) %-% colnames(XT)]})
        #     }
        #     if(!config$silent){
        #       cat('Done!')
        #     }
        #   }
      } else {XT = X}
      
      return(XT)
    },

    # Returns the aggregated output of gradient transformers (a numeric vector with one value per row of X), or 0 if there is none.
    # Each gradient transformer can be a column name in X (an offset column) or a model inheriting from MODEL.
    # Only the transformer's own output columns are used (columns it passes through via keep_columns/keep_features are ignored).
    # Classifier outputs are always used on the logit scale, because classifiers boost in log-odds space.
    y_gradient = function(X, y = NULL){
      nt = length(gradient_transformers)
      if(nt == 0){return(0)}
      YT = list()
      for(i in sequence(nt)){
        transformer = gradient_transformers[[i]]
        if(inherits(transformer, 'character')){
          assert(transformer %in% wt_colnames(X), sprintf("Gradient transformer column '%s' not found in the data!", transformer))
          YT[[i]] <- as.numeric(X[[transformer]])
        } else if (inherits(transformer, 'MODEL')){
          if(!transformer$fitted) {transformer$fit(X, y)}
          out = transformer$predict(X)
          nout = transformer$objects$n_output
          if(is.null(nout)){nout = ncol(out)}
          out = out[, ncol(out) - rev(sequence(nout)) + 1, drop = F]
          v = as.numeric(out[[1]])
          if(inherits(transformer, 'CLASSIFIER')){
            ret = transformer$config[['return']]
            assert(ret != 'class', sprintf("Gradient transformer %s must not return classes. Set its config 'return' to 'logit'.", transformer$name))
            if(ret == 'probs'){v = logit_fwd(v)}
          }
          YT[[i]] <- v
        } else {stop(sprintf("Gradient transformer %s must be a column name or a MODEL object!", i))}
      }
      if(nt == 1){return(YT[[1]])}
      do.call(cbind, YT) %>% apply(1, config$gradient_transformers_aggregator)
    },

    transform_yin = function(X, y){
      if(!is.null(config$yin_transformer_function)){
        y = do.call(what = config$yin_transformer_function, args = list(y, config$yin_transformer_arguments))
      }
      if(length(gradient_transformers) > 0){
        attr(y, 'gradient') <- y_gradient(X, y)
      }
      return(y)
    },

    # Adds the output of gradient transformers to the model output (the model was trained on the residual),
    # then applies the output transformer function if one is configured.
    transform_yout = function(X, Y){
      if(length(gradient_transformers) > 0){
        grad = y_gradient(X)
        for(i in numerics(Y)){
          Y[[i]] <- Y[[i]] + grad
        }
      }
      if(!is.null(config$yout_transformer_function)){
        Y = do.call(what = config$yout_transformer_function, args = list(Y, config$yout_transformer_arguments))
      }
      return(Y)
    },

    performance.fit  = function(){},

    info.transformer_count = function(){
      cnt = 1
      for(tr in transformers){
        cnt = cnt + tr$info.transformer_count()
      }
      return(cnt)
    },

    info.transformer_names = function(){
      mdlns = name
      for(tr in transformers){
        mdlns = c(mdlns, tr$info.transformer_names())
      }
      return(mdlns %>% unique)
    },

    info.edge_list = function(first = T){
      edgelist = NULL
      if(is.empty(transformers)){
        edgelist %<>% rbind(c('INPUT', name))
      } else {
        for(tr in transformers){
          edgelist %<>% rbind(c(tr$name, name))
          edgelist %<>% rbind(tr$info.edge_list(first = F))
        }
      }
      if(first){
        edgelist %<>% rbind(c(name, 'OUTPUT'))
      }
      return(edgelist)
    },

    info.model = function(){
      info = list(name = name, type = type, class = class(.self)[1], description = description, package = package,
                  language = package_language, outputs = objects$n_output, fitted = fitted) %<==>%
       list.extract(config, c('keep_columns', 'keep_features', 'smp.enabled', 'smp.class_ratio', 'smp.sample_ratio',
                                  'smp.method', 'fe.enabled', 'metric')) %>% list.clean
      return(info)
    },

    # One row per model in the network (this model and all its transformers and gradient transformers, recursively)
    info.transformers = function(){
      tbl = do.call(data.frame, info.model() %>% list.add(stringsAsFactors = F))
      for(tr in c(transformers, gradient_transformers)){
        if(inherits(tr, 'MODEL')){tbl = dplyr::bind_rows(tbl, tr$info.transformers())}
      }
      return(tbl %>% dplyr::distinct(name, .keep_all = TRUE))
    },

    # Nodes and links of the model network, used by plot.network().
    # Links: 'feeds' from a transformer (or INPUT) to the model using its output as features,
    # 'boosted by' from a gradient transformer (base model) to the model that boosts it.
    info.network = function(){
      edge = function(s, t, r) data.frame(source = s, target = t, relation = r, stringsAsFactors = FALSE)
      collect = function(m){
        out = list()
        if(is.empty(m$transformers)){out[[length(out) + 1]] <- edge('INPUT', m$name, 'feeds')}
        for(tr in m$transformers){
          out[[length(out) + 1]] <- edge(tr$name, m$name, 'feeds')
          out[[length(out) + 1]] <- collect(tr)
        }
        for(gt in m$gradient_transformers){
          if(inherits(gt, 'MODEL')){
            out[[length(out) + 1]] <- edge(gt$name, m$name, 'boosted by')
            out[[length(out) + 1]] <- collect(gt)
          } else {
            out[[length(out) + 1]] <- edge('INPUT', m$name, paste('offset', gt))
          }
        }
        do.call(rbind, out)
      }
      links = collect(.self) %>% rbind(data.frame(source = name, target = 'OUTPUT', relation = 'prediction', stringsAsFactors = FALSE)) %>% dplyr::distinct()

      tbl   = info.transformers()
      nodes = data.frame(id = tbl$name, class = tbl$class, type = tbl$type,
                         description = tbl$description, package = tbl$package,
                         outputs = if(is.null(tbl$outputs)) NA_integer_ else as.integer(tbl$outputs),
                         fitted = tbl$fitted, stringsAsFactors = FALSE)
      nodes = rbind(data.frame(id = 'INPUT', class = 'INPUT', type = 'Data', description = 'Input features', package = '', outputs = NA_integer_, fitted = TRUE, stringsAsFactors = FALSE),
                    nodes,
                    data.frame(id = 'OUTPUT', class = 'OUTPUT', type = 'Prediction', description = 'Final prediction', package = '', outputs = NA_integer_, fitted = TRUE, stringsAsFactors = FALSE))
      nodes$label = ifelse(nodes$id %in% c('INPUT', 'OUTPUT'), nodes$id, paste0(nodes$id, '\n', nodes$class))
      nodes$title = paste0('<b>', nodes$id, '</b><br>', nodes$class, '<br>', nodes$description,
                           ifelse(is.na(nodes$outputs), '', paste0('<br>outputs: ', nodes$outputs)))
      links$outputs = nodes$outputs[match(links$source, nodes$id)]
      list(nodes = nodes, links = links)
    },

    # Draws the network of models.
    # plotter = 'visNetwork': interactive htmlwidget, through package rvis if installed, otherwise package visNetwork.
    # plotter = 'igraph': static plot with package igraph.
    # Arguments in ... are passed to rvis as config (or to visNetwork::visNetwork / igraph's plot()).
    plot.network = function(plotter = c('visNetwork', 'igraph'), ...){
      plotter = match.arg(plotter)
      net = info.network()
      link_colors = c(feeds = '#7f8c8d', 'boosted by' = '#e67e22', prediction = '#2c3e50')
      net$links$color = ifelse(net$links$relation %in% names(link_colors), link_colors[net$links$relation], '#3498db')
      if(plotter == 'visNetwork' && !requireNamespace('visNetwork', quietly = TRUE) && requireNamespace('igraph', quietly = TRUE)){
        message("Package 'visNetwork' is not installed: drawing a static igraph chart instead.")
        plotter = 'igraph'
      }
      if(plotter == 'visNetwork'){
        if(requireNamespace('rvis', quietly = TRUE) && requireNamespace('visNetwork', quietly = TRUE)){
          # shape 'box' makes every node a rectangle sized to its own label
          net$nodes$shape = 'box'
          return(rvis::rvisPlot(type = 'graph', plotter = 'visNetwork', obj = net, key = 'id', label = 'label', color = 'type',
                                shape = 'shape', tooltip = 'title', source = 'source', target = 'target', linkLabel = 'relation',
                                linkColor = 'color', config = list(...)) %>%
                   visNetwork::visNodes(margin = 10))
        }
        assert(requireNamespace('visNetwork', quietly = TRUE), "plot.network() needs package 'visNetwork' (or use plotter = 'igraph')")
        nodes = net$nodes %>% dplyr::mutate(group = type, shape = 'box')
        edges = net$links %>% dplyr::transmute(from = source, to = target, label = relation, color = color, arrows = 'to',
                                               dashes = relation == 'boosted by')
        return(visNetwork::visNetwork(nodes, edges, ...) %>%
                 visNetwork::visNodes(margin = 10) %>%
                 visNetwork::visHierarchicalLayout(direction = 'LR', sortMethod = 'directed') %>%
                 visNetwork::visLegend(useGroups = TRUE) %>%
                 visNetwork::visOptions(highlightNearest = TRUE))
      }
      assert(requireNamespace('igraph', quietly = TRUE), "plotter = 'igraph' needs package 'igraph'")
      g = igraph::graph_from_data_frame(net$links[c('source', 'target', 'relation')], vertices = net$nodes[c('id', 'type', 'label')])
      types = unique(net$nodes$type)
      pal   = grDevices::hcl.colors(max(3, length(types)), 'Set 2')[seq_along(types)]
      # left to right: column = longest path from INPUT, rows ordered by igraph's Sugiyama layout to reduce crossings
      sorted = igraph::topo_sort(g, mode = 'out')
      depth  = stats::setNames(rep(0, igraph::vcount(g)), igraph::V(g)$name)
      for(v in igraph::V(g)$name[sorted]){
        for(w in igraph::neighbors(g, v, mode = 'out')$name){depth[w] <- max(depth[w], depth[v] + 1)}
      }
      lay = igraph::layout_with_sugiyama(g, layers = depth + 1)$layout[seq_len(igraph::vcount(g)), , drop = FALSE]
      xs  = unname(depth); ys = lay[, 1]
      # one row unit per node in each column, keeping Sugiyama's order, centred vertically
      ys  = stats::ave(ys, xs, FUN = function(u) rank(u, ties.method = 'first') - (length(u) + 1)/2)
      args = list(...)
      cex  = if(is.null(args$cex)) 0.85 else args$cex
      labels = igraph::V(g)$label
      # draw with base graphics so every box is sized to fit its own label; legends go in the bottom margin
      op = graphics::par(mar = c(4, 0.5, ifelse(is.null(args$main), 0.5, 2.5), 0.5))
      on.exit(graphics::par(op), add = TRUE)
      graphics::plot.new()
      graphics::plot.window(xlim = c(-0.3, max(xs) + 0.3), ylim = c(min(ys) - 0.55, max(ys) + 0.55))
      if(!is.null(args$main)) graphics::title(main = args$main)
      measure = function(cex){
        list(w = graphics::strwidth(labels, cex = cex) + 1.0*graphics::strwidth('M', cex = cex),
             h = graphics::strheight(labels, cex = cex) + 0.9*graphics::strheight('M', cex = cex))
      }
      m = measure(cex)
      # shrink the text if the widest box would overlap the next column or the tallest box the next row
      shrink = min(1, 0.93/max(m$w), 0.9/max(m$h))
      if(shrink < 1){cex = cex*shrink; m = measure(cex)}
      src = match(net$links$source, igraph::V(g)$name); trg = match(net$links$target, igraph::V(g)$name)
      graphics::arrows(x0 = xs[src] + m$w[src]/2, y0 = ys[src], x1 = xs[trg] - m$w[trg]/2, y1 = ys[trg],
                       length = 0.08, col = net$links$color, lwd = 1.3,
                       lty = ifelse(net$links$relation == 'boosted by', 2, 1))
      graphics::rect(xs - m$w/2, ys - m$h/2, xs + m$w/2, ys + m$h/2,
                     col = pal[match(igraph::V(g)$type, types)], border = 'grey30')
      graphics::text(xs, ys, labels, cex = cex)
      lcex = 0.8
      xmid = graphics::grconvertX(0.5, 'nfc')
      graphics::legend(xmid, graphics::grconvertY(0, 'nfc'), xjust = 0.5, yjust = 0, legend = types, fill = pal, text.width = graphics::strwidth(types, cex = 0.8),
                       bty = 'n', cex = lcex, horiz = TRUE, xpd = NA)
      graphics::legend(xmid, graphics::grconvertY(0.045, 'nfc'), xjust = 0.5, yjust = 0,
                       legend = c('feeds (transformer)', 'boosted by (gradient transformer)'),
                       col = link_colors[c('feeds', 'boosted by')], lty = c(1, 2), lwd = 1.3, bty = 'n', cex = lcex,
                       horiz = TRUE, xpd = NA)
      invisible(net)
    },

    # info.transformers = function(){
    #   translist = data.frame()
    #   translist
    #   for(tr in transformers){
    #     mdlns = c(mdlns, tr$info.transformer_names())
    #   }
    #   return(mdlns %>% unique)
    # },

    model.save = function(path = getwd()){
      if(!file.exists(path)) {dir.create(path)}
      for(tr in transformers){
        tr$model.save(path)
      }
      for(gtr in gradient_transformers){
        gtr$model.save(path)
      }
    },

    model.load = function(path = getwd()){
      for(tr in transformers){
        tr$model.load(path)
      }
      for(gtr in gradient_transformers){
        gtr$model.load(path)
      }
    },

    # if name_suffix is specified, it will be added to the model names and all its transformers and gradient transformers
    deep_copy = function(name_suffix = NULL){
      obj = .self$copy()
      if(!is.null(name_suffix)){obj$name = obj$name %>% paste0(name_suffix)}
      for (i in sequence(length(transformers))){
        obj$transformers[[i]] <- transformers[[i]]$deep_copy(name_suffix = name_suffix)
      }
      for (i in sequence(length(gradient_transformers))){
        obj$gradient_transformers[[i]] <- gradient_transformers[[i]]$deep_copy(name_suffix = name_suffix)
      }
      return(obj)
    },
    
    # todo: add k-fold, chronological shuffle, chronological split
    performance.cv = function(X, y, metrics = config$metrics, ...){
      cvmodel = .self$deep_copy()

      # Split by shuffling: todo: support other splitting methods(i.e.: chronological)
      N       = nrow(X)

      scores = list()

      for (i in sequence(cvmodel$config$cv.ntrain)){
        ind_train = N %>% sequence %>% sample(size = floor(config$cv.train_ratio*N), replace = F)

        X_train = X[ind_train, ,drop = F]
        y_train = y[ind_train]

        cvmodel$reset(config$cv.reset_transformer)
        cvmodel$fit(X_train, y_train)

        if(is.null(config$cv.set)){
          for(j in sequence(config$cv.ntest)){
            N2       = N - length(ind_train)
            ind_test = sequence(N) %>% setdiff(ind_train) %>% sample(size = floor(config$cv.test_ratio*N2), replace = F)
            X_test   = X[ind_test, , drop = F]
            y_test   = y[ind_test]
            scores[[length(scores) + 1]]  <- cvmodel$performance(X_test, y_test, metrics = metrics, ...)
          }
        } else {
          for(vset in config$cv.set){
            scores[[length(scores) + 1]] <- cvmodel$performance(vset$X, vset$y, metrics = metrics, ...)
          }
        }
      }

      # while((length(scores) == 1) & inherits(scores, 'list')) scores = scores[[1]]
      # if(inherits(scores, 'list')) {
      #   cls = scores %>% lapply(class) %>% unlist
      #   if(length(unique(cls)) == 1) {scores %<>% unlist}
      # }
      return(scores)
    },

    # get.performance = function(X_train, y_train, X_test, y_test){
    #
    #
    #   reset(config$cv.reset_transformer)
    #   .self$fit(X_train, y_train)
    #   yhat   = predict(X_test) # this has error! Fix it!
    #   objects$model <<- keep
    #   return(list(y_pred = yhat, y_true = y_test))
    # },

    info.size             = function(){
      t_size = list(config = 0, objects = 0, transformers = 0)
      for(tr in transformers){
        slist          = tr$info.size()
        t_size$config  = t_size$config  + slist$config
        t_size$objects = t_size$objects + slist$objects
        t_size$transformers = t_size$transformers + slist$transformers
      }
      list(config  = object.size(config),
           objects = object.size(objects),
           transformers = object.size(transformers) + t_size$config + t_size$objects + t_size$transformers)

    },

    get.parameters       = function(){},
    get.expert.predictor = function(){},
    get.expert.features  = function(){}
))

