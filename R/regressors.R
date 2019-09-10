REGRESSOR = setRefClass('REGRESSOR', contains = "MODEL",
 methods = list(
   initialize = function(...){
     callSuper(...)
     type               <<- 'Abstract Regressor'
     config$sig_level   <<- config$sig_level   %>% verify('numeric', domain = c(0,1), default = 0.05)
     config$sfs.enabled <<- config$sfs.enabled %>% verify('logical', domain = c(F,T), default = F)
     if(is.null(config$metric)){
       config$metric <<- function(y1, y2){
         err = (y1 - y2)^2 %>% sum
         # den = (y_test - mean(y_test))^2 %>% sum
         den = (y2 - mean(y2))^2 %>% sum
         return(1.0 - min(err/den, 1.0))
       }
     }
   },
   
   predict = function(X){
     XORG = callSuper(X)
     XFET = XORG[objects$features$fname]
     if(ncol(XFET) == 0){XOUT = predict.distribution(X)} else {XOUT = .self$model.predict(XFET)}
     colnames(XOUT) <- name %>% paste('out', sep = '_')
     treat(XOUT, XFET, XORG)
   },
   
   performance = function(X, y, metric = c('rmse', 'mae', 'medae')){
     metric = match.arg(metric)
     yp = predict(X) %>% pull(name %>% paste('out', sep = '_')) 
     if(metric == 'rmse'){
       return(rmse(yp, y))
     }
     if(metric == 'mae'){
       return(mae(yp, y))
     }
     if(metric == 'medae'){
       return(medae(yp, y))
     }
   }
 )
)


# A simple linear regression model. Features with linear dependency to others, will be removed to avoid singularity.
# feature importances are based on p-values of coefficients.
#' @export REG.LM
REG.LM = setRefClass('REG.LM', contains = "REGRESSOR",
   methods = list(
     initialize = function(...){
       callSuper(...)
       config$sig_level <<- config$sig_level %>% verify('numeric', domain = c(0,1), default = '0.1')
       type             <<- 'Linear Regression'
       if(is.empty(name)){name <<- 'LREG' %>% paste0(sample(1000:9999, 1))}
     },

     model.fit = function(X, y){
       objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
       X = X[objects$features$fname] %>% na2zero

       if(ncol(X) == 0){fit.distribution(X, y); return(NULL)}
       if(config$sfs.enabled){
         D   = cbind(X, Y = y) %>% as.matrix
         res = evaluate(D)
         objects$features <<- objects$features %>% filter(fname %in% res$sig.feature.names)
       }
       # forml = as.formula('y ~ ' %>% paste(paste(colnames(X), collapse = ' + ')))
       objects$model <<- stats::lm(y ~ ., data = cbind(X, y))
       singulars = is.na(objects$model$coefficients) %>% which %>% names
       while(length(singulars) > 0){
         objects$features <<- objects$features %>% filter(!(fname %in% singulars))
         model.fit(X, y)
         singulars = is.na(objects$model$coefficients) %>% which %>% names
       }
       gw = get.features.weight()
       objects$features$importance <<- gw[objects$features$fname]
     },

     get.features.weight = function(){
       objects$model.summary <<- summary(objects$model)
       pv   = objects$model.summary$coefficients[-1, 'Pr(>|t|)'] %>% na2zero
       # Usually NA p-values appear when there is a perfect fit (100% R-squared), so each feature shall be considerd as important!?
       keep = (pv < 0.1)
       weights = pv
       weights[!keep] <- 0
       weights[keep]  <- 1.0 - weights[keep]/0.1
       return(weights)
     },

     model.predict = function(X){
       objects$model %>% stats::predict(X %>% na2zero) %>% as.data.frame
     },

     get.performance.fit = function(){
       if(is.null(objects$model.summary)){objects$model.summary <<- summary(objects$model)}
       return(objects$model.summary$adj.r.squared)
     },

     # lazy
     get.parameters = function(){},

     # lazy
     get.predictor = function(){
       function(inputs, params){
         params$model$predict(inputs %>% as.data.frame)
       }
     },

     get.predictor.gradient = function(){
       function(inputs, params, wrt){
         params$coefficient[wrt]
       }
     },

     get.expert.predictor = function(X, y){
       # select random subset from the big dataset
       expert = new('FUNCTION', name = name %>% paste('predictor', sep = '.'), inputs = get.features.name() %>% {names(.) <- .;.})
       # congrats: baby is now born! Now its time for the kid to be trained in a school:

       # train the expert and find input weights and performances:
       fit(X, y)
       expert$params          = get.parameters()
       expert$objects$weights = get.features.weight()
       expert$objects$parents = get.features.name()
       expert$objects$performance.fit = get.performance.fit(X, y)
       expert$objects$performance.cv  = get.performance.cv(X, y)

       expert$rule.output   =
         function(inputs, params){
           params$model$predict(inputs %>% as.data.frame)
         }

       expert$rule.gradient =
         function(inputs, params, wrt){
           params$coefficient[wrt]
         }
       return(expert)
     }
   )
)


#' @export REG.XGB
REG.XGB = setRefClass('REG.XGB', contains = "REGRESSOR",
                     methods = list(
                       initialize = function(...){
                         callSuper(...)
                         type             <<- 'XGBoost Regression'
                         if(is.empty(name)){name <<- 'XGBREG' %>% paste0(sample(1000:9999, 1))}
                         if(is.null(config$nrounds)){config$nrounds <<- 100}
                       },
                       
                       model.fit = function(X, y){
                         objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
                         X = X[objects$features$fname] %>% na2zero
                         
                         if(ncol(X) == 0){fit.distribution(X, y); return(NULL)}
                         if(config$sfs.enabled){
                           # Not supported yet
                         }
                         
                         objects$model <<- xgboost::xgb.train(data = xgboost::xgb.DMatrix(X %>% as.matrix, label = y), nrounds = config$nrounds, params = config %>% list.remove(maler_words))
                       },

                       model.predict = function(X){
                         objects$model %>% stats::predict(X %>% as.matrix) %>% as.data.frame
                       }
                       
                     )
)


#' @export REG.SCIKIT.XGB
REG.SCIKIT.XGB = setRefClass('REG.SCIKIT.XGB', contains = "REGRESSOR",
                             methods = list(
                               initialize = function(...){
                                 callSuper(...)
                                 type               <<- 'Extreme Gradient Boosting for Regression'
                                 if(is.empty(name)){name <<- 'SKGBREG' %>% paste0(sample(1000:9999, 1))}
                                 module_xgb = reticulate::import('xgboost')
                                 objects$model     <<- do.call(module_xgb$XGBRegressor, config %>% list.remove(maler_words))
                               },
                               
                               model.fit = function(X, y){
                                 objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
                                 X = X[objects$features$fname]
                                 
                                 objects$model$fit(X %>% data.matrix, y)
                                 imp = try(objects$model$feature_importances_ %>% as.numeric, silent = T)
                                 if(inherits(imp, 'numeric')) objects$features$importance <<- imp
                               },

                               model.predict = function(X){
                                 objects$model$predict(X %>% data.matrix) %>% as.data.frame
                               }
                               
                             )
)
# REG.TAYLOR = setRefClass('REG.TAYLOR', contains = 'MODEL', methods = list(
#   initialize = function(...){
#     callSuper(...)
#     type               <<- 'Taylor Booster'
#     config$model_class <<- 'REG.LM'
#   },
#   
#   model.fit = function(X, y){
#     objects$features <<- objects$features %>% filter(fclass %in% c('numeric', 'integer'))
#     X = X[objects$features$fname]
#     
#     mdl <- new(config$model_class, config = config$model_config, rfe.enabled = T, sfs.enabled = T)
#     mdl$fit(X, y)
#     Xp = X[mdl$objects$features$fname]
#     for(col in mdl$objects$features$fname){
#       Xi = X %>% as.matrix %>% apply(2, function(v) X[,col]*v) %>% {colnames(.) <- colnames(.) %>% paste('X', col, sep = '_')}
#       Xi = cbind(Xp, Xi)
#       mdl <- new(config$model_class, config = config$model_config, rfe.enabled = T, sfs.enabled = T)
#       mdl$fit(Xi, y)
#       Xp = Xi[mdl$objects$features$fname]
#     }
#     object
#   }
# ))
