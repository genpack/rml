# Header
# Filename:       config.R
# Description:    Builds models from yaml or json configs: model_from_config() and the internal helpers of MODEL$from_config().
# Version History:
# 0.1.0 (27 November 2026) - Initial version.

# Keys of a model config that are not passed to the model's config
config_structure_keys = c('class', 'name', 'features', 'transformers', 'gradient_transformers')
# Keys whose content is moved up to the model's level (model hyper-parameters may be grouped under one of them)
config_parameter_keys = c('hyper_parameters', 'hyperparameters', 'parameters', 'config')
# dplyr verbs (and head/tail) allowed in the 'operations' of a feature file
config_feature_operations = c('filter', 'select', 'pull', 'arrange', 'slice', 'slice_head', 'slice_tail', 'slice_max', 'slice_min',
                              'distinct', 'mutate', 'rename', 'head', 'tail')

# Lists of unnamed scalars (json arrays, mixed yaml sequences) become vectors, recursively.
simplify_config = function(x){
  if(!is.list(x)) return(x)
  x = lapply(x, simplify_config)
  if(length(x) > 0 && is.null(names(x)) && all(sapply(x, function(e) is.atomic(e) && length(e) == 1))){
    types = unique(sapply(x, function(e) class(e)[1]))
    if(length(types) == 1 || all(types %in% c('integer', 'numeric'))) return(unlist(x))
  }
  x
}

# Reads a config given as a list, a file path (.yml, .yaml or .json) or yaml/json text.
# Returns the config list with attribute 'root': the folder of the config file, against which relative paths are resolved.
read_model_config = function(config, root = NULL){
  if(is.character(config)){
    assert(length(config) == 1, 'config must be a list, a file path or a yaml/json string')
    if(file.exists(config)){
      ext = tolower(tools::file_ext(config))
      if(ext == 'json'){
        assert(requireNamespace('jsonlite', quietly = TRUE), "Reading json configs needs package 'jsonlite'")
        cfg = jsonlite::read_json(config, simplifyVector = FALSE)
      } else {
        assert(requireNamespace('yaml', quietly = TRUE), "Reading yaml configs needs package 'yaml'")
        cfg = yaml::read_yaml(config)
      }
      if(is.null(root)) root = dirname(normalizePath(config))
    } else if(grepl('^\\s*[\\{\\[]', config)){
      assert(requireNamespace('jsonlite', quietly = TRUE), "Reading json configs needs package 'jsonlite'")
      cfg = jsonlite::parse_json(config, simplifyVector = FALSE)
    } else {
      assert(requireNamespace('yaml', quietly = TRUE), "Reading yaml configs needs package 'yaml'")
      cfg = yaml::yaml.load(config)
      assert(is.list(cfg), sprintf("config '%s' is neither an existing file nor a yaml/json text", config))
    }
  } else {
    cfg = config
  }
  assert(is.list(cfg), 'config must be a list, a file path or a yaml/json string')
  cfg = simplify_config(cfg)
  attr(cfg, 'root') <- root
  cfg
}

# Returns the config of the master model: the 'model' block if there is one, with root-level features and
# hyper-parameter blocks merged into it.
extract_model_config = function(cfg){
  root = attr(cfg, 'root')
  if(!is.null(cfg$model)){
    m = cfg$model
    assert(is.list(m), "Key 'model' must hold the model's config")
    others = names(cfg) %-% c('model', 'features', config_parameter_keys)
    if(length(others) > 0) warning(sprintf('Keys ignored at the root of the config: %s', paste(others, collapse = ', ')))
    for(k in c('features', config_parameter_keys) %^% names(cfg)){
      m[[k]] <- merge_config_block(m[[k]], cfg[[k]], k)
    }
  } else {
    m = cfg
  }
  attr(m, 'root') <- root
  m
}

merge_config_block = function(a, b, key){
  if(is.null(a)) return(b)
  if(key == 'features'){return(c(as.list(a), as.list(b)) %>% simplify_config)}
  dup = names(a) %^% names(b)
  assert(length(dup) == 0, sprintf("Parameters %s are given twice under '%s'", paste(dup, collapse = ', '), key))
  c(a, b)
}

# Moves the content of hyper-parameter blocks up to the model's level
flatten_model_config = function(cfg){
  for(k in config_parameter_keys %^% names(cfg)){
    block = cfg[[k]]
    cfg[[k]] <- NULL
    if(is.null(block)) next
    assert(is.list(block) && !is.null(names(block)), sprintf("Key '%s' must hold named parameters", k))
    dup = names(block) %^% names(cfg)
    assert(length(dup) == 0, sprintf("Parameters %s are given both under '%s' and at the model's level", paste(dup, collapse = ', '), k))
    cfg = c(cfg, block)
  }
  cfg
}

# Finds the class of a model: exact name, then with '-' read as '_', then ignoring case.
resolve_model_class = function(cls){
  assert(is.character(cls) && length(cls) == 1, "Each model needs one 'class'")
  is_model = function(x) {
    def = methods::getClassDef(x)
    !is.null(def) && methods::extends(def, 'MODEL')
  }
  for(cand in unique(c(cls, gsub('-', '_', cls)))){if(is_model(cand)) return(cand)}
  known = getNamespaceExports('rml') %>% {.[grepl('^[A-Z]{3}\\.', .)]} %>% {.[sapply(., is_model)]}
  m = known[toupper(known) == toupper(gsub('-', '_', cls))]
  if(length(m) == 1) return(m)
  d = utils::adist(toupper(gsub('-', '_', cls)), toupper(known))[1, ]
  near = known[order(d)][seq_len(min(3, length(known)))]
  stop(sprintf("Unknown model class '%s'. Did you mean %s?", cls, paste(sQuote(near, FALSE), collapse = ', ')), call. = FALSE)
}

# Resolves the 'features' of a config: names, or files of names
resolve_config_features = function(features, root = NULL){
  if(is.null(features)) return(NULL)
  if(is.character(features)) return(unique(features))
  if(is.list(features) && !is.null(features$file_path)) features = list(features)
  out = character()
  for(f in features){
    if(is.character(f)){
      out = c(out, f)
    } else if(is.list(f) && !is.null(f$file_path)){
      out = c(out, features_from_file(f, root))
    } else {
      stop("Each element of 'features' must be a feature name or a list with 'file_path' (and optional 'operations')", call. = FALSE)
    }
  }
  unique(out)
}

# Reads feature names from a file: a table (csv, tsv, rds) narrowed down by dplyr operations, or a list of names (txt, yaml, json).
features_from_file = function(spec, root = NULL){
  path = spec$file_path
  if(!is.null(root) && !grepl('^(/|~|[A-Za-z]:)', path)) path = file.path(root, path)
  assert(file.exists(path), sprintf("Feature file '%s' not found", path))
  ext = tolower(tools::file_ext(path))
  tbl = switch(ext,
               csv  = utils::read.csv(path, stringsAsFactors = FALSE),
               tsv  = utils::read.delim(path, stringsAsFactors = FALSE),
               rds  = readRDS(path),
               yml  = , yaml = yaml::read_yaml(path),
               json = jsonlite::read_json(path, simplifyVector = TRUE),
               readLines(path) %>% trimws %>% {.[. != '']})
  for(op in spec$operations){tbl = apply_config_operation(tbl, op)}
  if(is.data.frame(tbl)){
    column = if(!is.null(spec$column)) spec$column else if(ncol(tbl) == 1) colnames(tbl)[1] else if('fname' %in% colnames(tbl)) 'fname' else NULL
    assert(!is.null(column), sprintf("Feature file '%s' gives a table: end the operations with 'pull' or set 'column'", spec$file_path))
    tbl = tbl[[column]]
  }
  as.character(unlist(tbl))
}

apply_config_operation = function(tbl, op){
  fun = op$fun
  assert(is.character(fun) && length(fun) == 1 && fun %in% config_feature_operations,
         sprintf("Operation '%s' is not supported. Use one of: %s", paste(fun, collapse = ' '), paste(config_feature_operations, collapse = ', ')))
  f = if(fun %in% c('head', 'tail')) getExportedValue('utils', fun) else getExportedValue('dplyr', fun)
  args = op$arguments
  if(is.null(args) || length(args) == 0){return(f(tbl))}
  call = parse(text = sprintf('.f(.tbl, %s)', paste(args, collapse = ', ')))[[1]]
  eval(call, envir = list(.f = f, .tbl = tbl), enclos = baseenv())
}

# Builds one model (and its transformers and gradient transformers, recursively) from its config
build_model_from_config = function(cfg, root = NULL, counter = new.env()){
  if(is.character(cfg)) return(cfg)  # a gradient transformer given as a column of offsets
  assert(is.list(cfg) && !is.null(names(cfg)), 'Each model config must be a set of named parameters')
  cls  = resolve_model_class(cfg$class)
  cfg  = flatten_model_config(cfg)
  name = cfg$name
  if(is.null(name)){
    base = tolower(sub('^.*\\.', '', cls))
    counter[[base]] <- if(is.null(counter[[base]])) 1 else counter[[base]] + 1
    name = paste0(base, '_', counter[[base]])
  }
  feats = resolve_config_features(cfg$features, root)
  # yaml and json give whole numbers as integers; rml checks most numeric parameters as doubles
  # (Python-based models convert whole numbers back to integers, see python_args()).
  args  = cfg[names(cfg) %-% config_structure_keys] %>% rapply(as.numeric, classes = 'integer', how = 'replace')
  if(!is.null(feats)) args$features.include = unique(c(args$features.include, feats))
  trs = lapply(cfg$transformers, build_model_from_config, root = root, counter = counter)
  gts = lapply(cfg$gradient_transformers, build_model_from_config, root = root, counter = counter)
  assert(all(vapply(trs, inherits, logical(1), 'MODEL')), sprintf("Every transformer of '%s' must be a model config with a 'class'", name))
  methods::new(cls, name = name, transformers = trs, gradient_transformers = gts, config = args)
}

model_names_recursive = function(model){
  subs = c(model$transformers, model$gradient_transformers)
  subs = subs[vapply(subs, inherits, logical(1), 'MODEL')]
  c(model$name, unlist(lapply(subs, model_names_recursive)))
}

#' @title Build a model from a yaml or json config
#'
#' @description
#' Creates a model, with its transformers and gradient transformers, from a config: a yaml or json file, a yaml or
#' json string, or a \code{list}. Use \code{vignette('rml-config-gallery')} for examples.
#'
#' @section Config format:
#' A model is a set of parameters:
#' \describe{
#'   \item{\code{class}}{Class of the model, e.g. \code{CLS.XGBOOST}. Required. A \code{-} is read as \code{_}.}
#'   \item{\code{name}}{Name of the model. Default: the class's short name and a number, e.g. \code{xgboost_1}.}
#'   \item{\code{features}}{Input columns of the model (its \code{features.include}). Without them, the model's own feature
#'     selection applies, e.g. an encoder takes the categorical columns. Each element is a column name or a file of names:
#'     \code{file_path} (csv, tsv, rds, txt, yaml or json; relative to the config file) and optional \code{operations}, a
#'     list of \code{fun} (a dplyr verb such as \code{filter} or \code{pull}, or \code{head}) and \code{arguments}
#'     (R code, as text).}
#'   \item{\code{transformers}, \code{gradient_transformers}}{Lists of model configs, built the same way. A gradient
#'     transformer is the base model the model boosts; it can also be the name of a column of offsets.}
#'   \item{\code{hyper_parameters}}{Optional block of parameters (also \code{hyperparameters}, \code{parameters} or
#'     \code{config}). Its content is merged with the other parameters, so a parameter can be written in the block or
#'     next to \code{class}, but not in both.}
#'   \item{any other key}{A parameter of the model, e.g. \code{nrounds}, \code{fe.enabled} or \code{column_filters}.}
#' }
#' The master model is either the root of the config or the block under key \code{model}. With a \code{model} block,
#' root-level \code{features} and \code{hyper_parameters} belong to the master model.
#'
#' @param config \code{character} path of a .yml, .yaml or .json file, yaml or json text, or a \code{list}.
#' @param root \code{character} folder against which relative feature file paths are resolved. Default: the config
#'   file's folder, or the working directory.
#' @return An unfitted model.
#' @seealso \code{\link{MODEL-class}} (method \code{from_config()})
#' @examples
#' cfg = "
#' model:
#'   class: CLS.STATS.LR
#'   name: lr_on_encodings
#'   transformers:
#'   - class: ENC.RML.TE
#'     name: te
#'   - class: MAP.RML.ZFS
#'     name: z
#'     features: [a, b]
#' "
#' model = model_from_config(cfg)
#' X = data.frame(a = rnorm(300), b = rnorm(300), g = sample(letters[1:5], 300, replace = TRUE))
#' y = as.numeric(X$a + (X$g == 'a') + rnorm(300) > 0.5)
#' model$fit(X, y)
#' head(model$predict(X))
#' @export
model_from_config = function(config, root = NULL){
  cfg = read_model_config(config, root) %>% extract_model_config
  model = build_model_from_config(cfg, root = attr(cfg, 'root'))
  dup = model_names_recursive(model) %>% {.[duplicated(.)]} %>% unique
  if(length(dup) > 0){
    warning(sprintf("Model names used more than once: %s. Network charts (plot.network) draw models of the same name as one node.", paste(dup, collapse = ', ')))
  }
  model
}
