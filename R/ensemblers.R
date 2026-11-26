
# previously ENS.RML.BS
#' @title Binary supervisor ensembler
#' @name ENS.RML.BS-class
#' @aliases ENS.RML.BS
#' @description
#' Learns which of its \code{pupils} (model outputs among the input columns) to trust for each row: a supervisor model
#' predicts the best pupil, and that pupil's output is returned.
#'
#' Inherits from \code{\link{MODEL-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{model_class}}{Class of the supervisor model. Default \code{'CLS.SKLEARN.XGB'}.}
#'   \item{\code{model_config}}{Arguments of the supervisor model.}
#' }
#' @export ENS.RML.BS
ENS.RML.BS = setRefClass('ENS.RML.BS', contains = "MODEL",
   methods = list(
     initialize = function(...){
       callSuper(...)
       type             <<- 'Ensembler'
       description      <<- 'Binary Supervisor Ensembler'
       package          <<- 'rml'
       package_language <<- 'R'
       
       if(is.empty(name)){name <<- 'BSUP' %>% paste0(sample(10000:99999, 1))}
       config$model_class  <<- verify(config$model_class, 'character', default = 'CLS.SKLEARN.XGB')
       config$model_config <<- verify(config$model_config, 'character', default = list())
       objects$model <<- new(config$model_class, config = config$model_config)
     },
     
     model.fit = function(X, y){
       Xt = X %>% column_drop(objects$pupils)
       pp = X[objects$pupils] %>% as.matrix
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

#' @title Aggregating ensembler
#' @name ENS.RML.AGGR-class
#' @aliases ENS.RML.AGGR
#' @description
#' Outputs a row-wise aggregate (default the mean) of its numeric inputs. Give it classifiers as transformers to average
#' their probabilities. It is a classifier, so \code{return}, thresholds and \code{performance()} apply.
#'
#' Inherits from \code{\link{CLASSIFIER-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{aggregator}}{Name of the aggregate function. Default \code{'mean'}.}
#' }
#' @examples
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = ENS.RML.AGGR(verbose = 0, transformers = list(CLS.STATS.LR(name = 'a', features.include = 'a'), CLS.STATS.LR(name = 'b')))
#' m$fit(X, y)
#' head(m$predict(X))
#' @export ENS.RML.AGGR
ENS.RML.AGGR = setRefClass('ENS.RML.AGGR', contains = "CLASSIFIER",
   methods = list(
     initialize = function(...){
       callSuper(...)
       type             <<- 'Ensembler'
       description      <<- 'Aggregator Ensembler'
       package          <<- 'rml'
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
