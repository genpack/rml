
# previously ENS.RML.BS
#' @title Binary supervisor ensembler
#' @name ENS.RML.BS-class
#' @aliases ENS.RML.BS
#' @description
#' Combines two binary classifiers (the \code{pupils}) by learning, row by row, which one to trust.
#' For each training row the pupil to follow is the one with the higher probability if \code{y = 1} and the
#' one with the lower probability if \code{y = 0}. A supervisor classifier learns this choice from the
#' other input columns, and at prediction the output of the chosen pupil is returned.
#'
#' The pupils are normally given as transformers, together with a \code{\link{MAP.RML.IDT-class}} transformer
#' that passes the raw features the supervisor learns from.
#'
#' Inherits from \code{\link{CLASSIFIER-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{pupils}}{Names of the two input columns holding the pupils' probabilities. Default: the output columns of the classifier transformers.}
#'   \item{\code{model_class}}{Class of the supervisor classifier. Default \code{'CLS.XGBOOST'}.}
#'   \item{\code{model_config}}{\code{list} of arguments of the supervisor classifier.}
#' }
#' @examples
#' set.seed(1)
#' X = data.frame(a = rnorm(1000), b = rnorm(1000))
#' y = as.numeric(runif(1000) < plogis(ifelse(X$b > 0, 2*X$a, -X$a)))
#' m = ENS.RML.BS(transformers = list(CLS.STATS.LR(name = 'p1', features.include = 'a'),
#'                                    CLS.XGBOOST(name = 'p2', nrounds = 20),
#'                                    MAP.RML.IDT(name = 'raw')),
#'                model_config = list(nrounds = 20, max_depth = 2))
#' m$fit(X, y)
#' head(m$predict(X))
#' @export ENS.RML.BS
ENS.RML.BS = setRefClass('ENS.RML.BS', contains = "CLASSIFIER",
   methods = list(
     initialize = function(...){
       callSuper(...)
       type             <<- 'Ensembler'
       description      <<- 'Binary Supervisor Ensembler'
       package          <<- 'rml'
       package_language <<- 'R'
       
       if(is.empty(name)){name <<- 'BSUP' %>% paste0(sample(10000:99999, 1))}
       config$model_class  <<- verify(config$model_class, 'character', lengths = 1, default = 'CLS.XGBOOST')
       config$model_config <<- verify(config$model_config, 'list', default = list())
       config$pupils       <<- verify(config$pupils, 'character')
       reserved_words      <<- c(reserved_words, 'model_class', 'model_config', 'pupils')
     },
     
     # Output columns of the classifier transformers
     find_pupils = function(columns){
       pupils = c()
       for(tr in transformers){
         if(inherits(tr, 'CLASSIFIER')){
           pupils = c(pupils, columns[columns == tr$name | startsWith(columns, paste0(tr$name, '_'))])
         }
       }
       return(pupils)
     },
     
     model.fit = function(X, y){
       pupils = config$pupils
       if(is.null(pupils)){pupils = find_pupils(colnames(X))}
       assert(length(pupils) == 2, sprintf('ENS.RML.BS needs exactly two pupil columns, found %s: %s', length(pupils), paste(pupils, collapse = ', ')))
       objects$pupils <<- pupils
       
       Xt = X[setdiff(colnames(X), pupils)]
       if(ncol(Xt) == 0){Xt = X[pupils]}
       objects$supervisor_features <<- colnames(Xt)
       pp = as.matrix(X[pupils])
       
       # 1 if the second pupil should be followed, 0 for the first one
       yt = as.numeric(ifelse(y == 1, pp[,2] > pp[,1], pp[,2] < pp[,1]))
       args = c(config$model_config, list(name = paste0(name, '_supervisor'), verbose = 0))
       objects$model <<- do.call(new, c(list(config$model_class), args))
       objects$model$fit(Xt, yt)
     },
     
     model.predict = function(X){
       pp     = as.matrix(X[objects$pupils])
       follow = objects$model$predict_probs(X[objects$supervisor_features]) > 0.5
       data.frame(prob = ifelse(follow, pp[,2], pp[,1]))
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
