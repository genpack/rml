##### Untility Functions #####
# Generates a random integer number
#' @export
randint = function(n = 1, min = 0, max = 100){
  min + as.integer(runif(n = n)*(max - min + 1))
}


##### Builders #####

#' @export
build_model_instance_from_template = function(model_name = NULL, template){
  stopifnot(inherits(template, 'list'))
  tr  = try(template$class %>% new, silent = T)
  if(inherits(tr, 'try-error')){
    cat('\n', 'Building model instance ', tr, ' failed!', '\n',  as.character(tr), '\n')
    return(NULL)
  }
  if(!is.null(model_name)) tr$name = model_name
  configs = names(template) %-% c('class', 'transformers', 'feature_sample_ratio', 'feature_sample_size', 'pass')
  for(cfg in configs){
    if (inherits(template[[cfg]], 'list')){
      funobj = match.fun(template[[cfg]]$fun)
      stopifnot(inherits(funobj, 'function'))
      tr$config[[cfg]] <- do.call(funobj, template[[cfg]] %>% list.add(n = 1) %>% list.remove('fun'))
    } else {
      if(length(template[[cfg]]) > 1){
        if(inherits(template[[cfg]], c('numeric', 'integer')) & (length(names(template[[cfg]])) == length(template[[cfg]]))){
          tr$config[[cfg]] <- pick(template[[cfg]])
        } else {tr$config[[cfg]] <- template[[cfg]] %>% sample(size = 1)}
      } else if (length(template[[cfg]]) == 1) (tr$config[[cfg]] <- template[[cfg]])
    }
  }
  return(tr)
}

# 

#' builds a model object from a template which is given in a list of templates.
#'
#' @param template_name \code{character}: Specify template name. Must be among the names of the list given via argument \code{templates}
#' @param model_name \code{character}: Specify the name of the model being generated. Every model object can have a name. If name is not specified, a random name will be generated.
#' @param features \code{character or data.frame}: Specify feature names from which model will be trained. 
#' The models's config parameter \code{features.include} will be set accordingly to extract specified features from the training dataset.
#' features can also be a data frame of features (output of function evaluate_features)
#' @param templates \code{list}: A named list of templates. Argument \code{template_name} must be among the names of this list.
#' @return A object inheriting from class \code{MODEL}
#' @export
build_model_from_template = function(template_name, model_name = NULL, features = NULL, templates = default_templates, metric = 'gini'){
  stopifnot(inherits(templates, 'list'))
  temp_names = names(templates)
  verify(template_name, 'character', domain = temp_names)

  model = build_model_instance_from_template(model_name = model_name, template = templates[[template_name]])
  # Add transformers to the model:
  for(item in templates[[template_name]]$transformers){
    chance = runif(1)
    if(chance < item$probability){
      nt = length(model$transformers)
      tn = item$templates %>% sample(size = 1)
      model$transformers[[nt + 1]] <- build_model_from_template(template_name = tn, features = features, templates = templates)
    }
  }

  if(length(model$transformers) == 0){
    
    if(!is.null(features)){
      if(inherits(features, 'character')){model$config$features.include <- features} else {model$config$features.include <- rownames(features)}
    }

    if(!is.null(templates[[template_name]]$pass)){
      # Have you specified which features can pass in the template?
      if(inherits(templates[[template_name]]$pass, 'list') & inherits(features, 'data.frame')){
        ind = sequence(nrow(features))
        for(filtn in names(templates[[template_name]]$pass)){
          if(filtn %in% colnames(features)){
            val = features[[filtn]]
            if(inherits(val, c('character', 'factor'))){
              ind = ind %^% which(val %in% templates[[template_name]]$pass[[filtn]])
            } else {
              # First element of range specifies the minimum value
              ind = ind %^% which(val > templates[[template_name]]$pass[[filtn]][1])
              if(length(templates[[template_name]]$pass[[filtn]]) > 1){
                # Second element of range specifies the maximum value
                ind = ind %^% which(val < templates[[template_name]]$pass[[filtn]][2])
              }
            }
          }
        }
        model$config$features.include = rownames(features)[ind]
      }
      else if(inherits(templates[[template_name]]$pass, 'character')){
        model$config$features.include <- chif(is.null(model$config$features.include, templates[[template_name]]$pass, model$config$features.include %^% templates[[template_name]]$pass))
      }
    }

    # Template properties 'feature_sample_ratio' and 'feature_sample_size' work only when
    # 'model$config$features.include' is not null and this happens when either
    # template property 'pass' is a charachter and directly specifies feature names or
    # argument 'features' is a data.frame with rownames specifiying feature names.
    # 'feature_sample_ratio' is always overwritten with 'feature_sample_size'. So don't specify both.
    if(!is.null(model$config$features.include)){
      if(length(model$config$features.include) == 0){return(NULL)}

      num_feat = NULL
      # Respecting property 'feature_sample_ratio'
      if(!is.null(templates[[template_name]]$feature_sample_ratio)){
        if(inherits(templates[[template_name]]$feature_sample_ratio, 'list')){
          ratio = do.call(templates[[template_name]]$feature_sample_ratio$fun, args = templates[[template_name]]$feature_sample_ratio %>% list.remove('fun') %>% list.add(n = 1))
        } else {ratio = templates[[template_name]]$feature_sample_ratio %>% sample(size = 1)}
        num_feat = ceiling(length(model$config$features.include)*ratio)
      }  

      # Respecting property 'feature_sample_size'
      if(!is.null(templates[[template_name]]$feature_sample_size)){
        num_feat = templates[[template_name]]$feature_sample_size %>% sample(size = 1)
        if(num_feat > length(model$config$features.include)){num_feat = length(model$config$features.include)}
      }
      if(!is.null(num_feat)){
        if(inherits(features, 'character')){
          model$config$features.include <- model$config$features.include %>% sample(size = num_feat)
        } else {
          model$config$features.include <- model$config$features.include %>%
          sample(size = num_feat, prob = features[model$config$features.include, metric] %>% vect.normalise)
        }
      }
    }
  }
  return(model)
}


