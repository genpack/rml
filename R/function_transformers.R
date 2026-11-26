
#' @title Inverse transformer
#' @name FNT.RML.INV-class
#' @aliases FNT.RML.INV
#' @description
#' Outputs \code{1/x} for each numeric column, with values beyond \code{trim} replaced by the largest value within it.
#'
#' Inherits from \code{\link{MODEL-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{trim}}{Absolute limit of the output. Default 10000.}
#' }
#' @examples
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = FNT.RML.INV()
#' m$fit(X, y)
#' head(m$predict(X))
#' @export FNT.RML.INV
FNT.RML.INV = setRefClass('FNT.RML.INV', contains = 'MODEL', methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Function Transformer'
    description      <<- 'Geometric'
    package          <<- 'rml'
    package_language <<- 'R'
    
    if(is.empty(name)){name <<- 'INV' %>% paste0(sample(10000:99999, 1))}
    config$trim <<- verify(config$trim, 'numeric', lengths = 1, default = 10000)
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

#' @title Logarithm transformer
#' @name FNT.RML.LOG-class
#' @aliases FNT.RML.LOG
#' @description
#' Outputs \code{log(x + intercept)} for each numeric column.
#'
#' Inherits from \code{\link{MODEL-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{intercept}}{Added before taking the log. Default 1.}
#'   \item{\code{apply_abs}}{Take the absolute value first. Default TRUE.}
#' }
#' @examples
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = FNT.RML.LOG()
#' m$fit(X, y)
#' head(m$predict(X))
#' @export FNT.RML.LOG
FNT.RML.LOG = setRefClass('FNT.RML.LOG', contains = "MODEL", methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Function Transformer'
    description      <<- 'Logarythm'
    package          <<- 'rml'
    package_language <<- 'R'
    if(is.empty(name)){name <<- 'LOG' %>% paste0(sample(10000:99999, 1))}
    config$apply_abs <<- verify(config$apply_abs, c('logical'), default = T)
    config$intercept <<- verify(config$intercept, c('numeric'), default = 1)
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

#' @title Logit transformer
#' @name FNT.RML.LOGIT-class
#' @aliases FNT.RML.LOGIT
#' @description
#' Outputs the logit of each numeric column, after min-max scaling to [0, 1] by default.
#'
#' Inherits from \code{\link{MODEL-class}}.
#' @section Configuration:
#' Arguments specific to this class (see \code{\link{MODEL-class}} for arguments shared by all models):
#' \describe{
#'   \item{\code{apply_mms}}{Min-max scale columns first. Default TRUE.}
#' }
#' @examples
#' X = data.frame(a = rnorm(300), b = rnorm(300))
#' y = as.numeric(X$a - X$b + rnorm(300) > 0)
#' m = FNT.RML.LOGIT()
#' m$fit(X, y)
#' head(m$predict(X))
#' @export FNT.RML.LOGIT
FNT.RML.LOGIT = setRefClass('FNT.RML.LOGIT', contains = "MODEL", methods = list(
  initialize = function(...){
    callSuper(...)
    type             <<- 'Function Transformer'
    description      <<- 'Logit'
    package          <<- 'rml'
    package_language <<- 'R'
    if(is.empty(name)){name <<- 'LGT' %>% paste0(sample(10000:99999, 1))}
    config$apply_mms <<- verify(config$apply_mms, c('logical'), default = T)
  },
  
  model.fit = function(X, y){
    objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
  },
  
  model.predict = function(X){
    X = as.matrix(X)
    if(config$apply_mms){X = X %>% apply(2, vect.normalise)}
    X[X < .Machine$double.eps] <- .Machine$double.eps
    X[X > 1.0 - .Machine$double.eps] <- 1.0 - .Machine$double.eps
    XOUT = log(X/(1 - X))
    return(XOUT %>% as.data.frame)
  }
))
