# A simple linear regression model. Features with linear dependency to others, will be removed to avoid singularity.
# feature importances are based on p-values of coefficients.
STATS.LM = setRefClass('STATS.LM', contains = "MODEL",
   methods = list(
     initialize = function(...){
       callSuper(...)
       config$sig_level <<- config$sig_level %>% verify('numeric', domain = c(0,1), default = '0.1')
       type             <<- 'Linear Regression'
       if(is.null(config$metric)){
         settings$metric <- function(y1, y2){
           err = (y1 - y2)^2 %>% sum
           # den = (y_test - mean(y_test))^2 %>% sum
           den = (y2 - mean(y2))^2 %>% sum
           return(1.0 - min(err/den, 1.0))
         }
       }
       
     },

     fit = function(X, y){
       X     = transform(X, y)
       forml = as.formula('y ~ ' %>% paste(paste(names(X), collapse = ' + ')))
       objects$model <<- lm(forml, data = cbind(X, y))
       singulars = is.na(objects$model$coefficients) %>% which %>% names
       if(length(singulars) > 0){
         X = X[, colnames(X) %>% setdiff(singulars)]
         fit(X, y)
       }
       fitted <<- TRUE
     },

     get.features.name = function(){
       names(objects$model$coefficients[-1])
     },

     get.features.weight = function(){
       if(is.null(objects$model.summary)){objects$model.summary <<- summary(objects$model)}
       pv   = objects$model.summary$coefficients[-1, 'Pr(>|t|)']
       keep = (pv < 0.1)
       weights = pv
       weights[!keep] <- 0
       weights[keep]  <- 1.0 - weights[keep]/0.1
       return(weights/sum(weights))
     },

     predict = function(X){
       X = transform(X)
       objects$model %>% stats::predict(X)
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
