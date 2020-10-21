
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
