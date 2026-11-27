# Header
# Filename:       rml.R
# Description:    A pipeline and toolbox for R programmers useful for building products for machine learning and prediction
# Version History:
# 0.0.0 (05 August 2013)    - Initial Issue
# 0.1.0 (23 February 2020)  - Second Issue
# 0.1.2 (06 August 2020)    - Third Issue
# 0.1.3 (13 October 2020)   - Forth Issue
# 0.2.2 (15 October 2020)   - Fundamental Changes: transformers transferred to separate files. Super classes created for sklearn and category_encoders packages.
# 0.2.3 (21 October 2020)   - reserved_words added to class MODEL
# 0.2.4 (21 October 2020)   - All sklearn modules inherit from TRM.SKLEARN
# 0.2.6 (22 October 2020)   - function model_update added to mltools, exported and added to namespace
# 0.2.8 (22 October 2020)   - argument update added to function model_load
# 0.2.11 (22 October 2020)  - minor changes in function scorer
# 0.3.1 (30 October 2020)   - Sampling parameters added to the config: smp.enabled, smp.ratio, smp.num_rows, smp.fix_class_ratio
# 0.3.3 (05 November 2020)  - WIDETABLE is converted to matrix for all sklearn transformers
# 0.3.5 (11 November 2020)  - templates.R updated: minor issues fixed
# 0.3.7 (25 November 2020)  - abstract.R updated: upsampling and downsampling added
# 0.4.0 (05 January 2021)   - abstract.R updated: smote upsampling with python package smote_variant added
# 0.4.5 (06 January 2021)   - abstract.R updated: upsampling engine moved to after transformation. fix_class_ratio renamed to class_ratio
# 0.5.1 (12 January 2021)   - classifier.R updated: CLS.SKLEARN calls initialize method of its second parent: CLASSIFIER
# 0.5.2 (13 January 2021)   - abstract.R updated: logical parameter 'name_in_output' added to config
# 0.6.0 (01 February 2021)  - mltools.R updated: functions scorer() and correlation() modified.
# 0.6.2 (11 February 2021)  - Module mlvis.R added with two functions: binary_class_bar() and binary_class_pie()
# 0.6.5 (16 March 2021)     - function evaluate_features() in gentools.R updated and exported.
# 0.6.6 (17 March 2021)     - functions outlier() and plot_bindensity() added.
# 0.6.8 (18 March 2021)     - function correlation() changed: optsplit.chi and optsplit.f1 added
# 0.6.10 (31 March 2021)    - Method model.load() changed for SKLearn models. Module is reloaded.
# 0.7.1 (1 April 2021)      - Boosters initiated: Function feature_booster(), train_model() and evaluate_models() added.
# 0.7.2 (12 April 2021)     - Minor fix in function correlation(). NULL is passed to argument 'threshold' 
# 0.7.4 (27 April 2021)     - clustering.R transferred from rutils, function cluster_tree() added. 
# 0.8.5 (06 May 2021)       - multicore transformer training added. 
# 0.8.7 (07 May 2021)       - function trim_outliers() added and exported. 
# 0.9.0 (07 May 2021)       - config properties: pp.mask_missing_values and pp.trim_outliers added
# 0.9.6 (10 May 2021)       - issues in multicore transformer fitting functionality was fixed. method transform_x()
# 0.10.1 (12 May 2021)      - Class CLS.STATS.LR added.
# 1.0.6 (21 May 2021)       - Major changes: functions fit_models() and predict_models() and service_models() added supporting multicore computation. Method transform_x() updated and calls these functions. 
# 1.0.8 (25 May 2021)       - Minor change in xgb classifiers, method fit.model(): Conversion from WideTable to matrix moved to after feature selection. 
# 1.0.9 (02 June 2021)      - Minor change in SKLearn Transformers: Converts X to data.matrix in the superClass. 
# 1.0.10 (02 June 2021)     - config property column_filters updated. 
# 1.1.1 (02 June 2021)      - Method deep_copy() added to abstract. 
# 1.1.3 (03 June 2021)      - Minor bug in mappers and encoders fixed. 
# 1.2.1 (04 June 2021)      - preprocessing relocated from method fit() to transform_x(). They are now called  for both fit and predict.
# 1.2.4 (07 June 2021)      - Minor bugs in encoders fixed.
# 1.2.6 (08 June 2021)      - reset now can set features.include in the config when there are no transformers. (todo: feature elimination embedding to transformers does not work yet!)
# 1.2.8 (09 June 2021)      - sklearn transformer modules are now re-created in fit method to avoid errors in parallel runs
# 1.3.5 (11 June 2021)      - Saves category_encoder modules with pickle, methods keep_model(), retrieve_model(), release_model() added and customized for python-based models.
# 1.3.8 (15 June 2021)      - preprocessing relocated back from transform_x() to method fit(). They are now called only for fit. only mask_missing_values is called in predict as well.
# 1.4.2 (18 June 2021)      - parameter 'return_type_in_output' added.
# 1.4.3 (29 June 2021)      - CLS.XGBOOST() updated: xgb.train replaced by xgboost::xgb.train and config$eval_metric becomes null if config$feval exists
# 1.4.5 (02 December 2022)  - function bucket_moments() in mltools.R modified.
# 1.4.6 (06 March 2023)     - Minor bug fixed in mappers. Used to destroy feature names when a tibble was passed for training.
# 1.4.7 (08 June 2023)      - clustering.R changed to version 1.2.7
# 2.0.0 (26 November 2026)  - New features, documentation and many bug fixes:
#                             * Packaging: DESCRIPTION declares all dependencies; NAMESPACE, man/ generated by roxygen2; works with
#                               rwidetable (formerly rbig) and with xgboost 1.x and 3.x; AUC and rfun dependencies removed.
#                             * Gradient boosting: CLS.XGBOOST uses the gradient transformer output as base margin with
#                               objective 'binary:logistic' (default); CLS.STATS.LR supports gradient transformers (glm offset);
#                               classifier gradient transformers are read as logits; unsupported classifiers raise an error;
#                               row sampling keeps the gradient.
#                             * New: info.network(), plot.network() rewritten ('boosted by' links; rvis::rvisPlot, visNetwork or igraph),
#                               hp_booster() exported, options(rml.verbose), objects$fitting_duration, python_setup help page.
#                             * Python: whole numbers are passed to Python as int (python_args()); CLS.SKLEARN.LR max_iter = 1000;
#                               python_address no longer passed to scikit-learn.
#                             * Rewritten or repaired: ENS.RML.BS, ENC.SKLEARN.OHE, ENC.RML.TE, ENC.RML.ME, BIN.SKLEARN.KMC,
#                               BIN.KMEANS.KMC, BIN.RML.OBB, MAP.SKLEARN.ZFS, MAP.SKLEARN.QT, FNT.RML.LOGIT, FNT.RML.POLY,
#                               CLS.E1071.NB (returned classes), feature_booster, service_models, predict_models, optSplit.chi
#                               p-value, templates, reset() keeping features.include, verbose, threshold determination.
#                             * Speed: MAP.RML.ZFS/MMS 4-6x faster; vectorised EDA and BIN.KMEANS.KMC prediction.
#                             * Documentation: roxygen help for every class and function; vignettes 1-8 (getting started,
#                               pipelines, gradient boosting, reference, transformers, ensembles and tools, troubleshooting,
#                               showcase); testthat suite (66 tests).
#                             * FET.RML.D2MULC no longer exported (depends on functions that do not exist).
# 2.0.1 (27 November 2026)  - Documentation: boosting terminology corrected. If A is a gradient transformer of B, A is the base
#                             model and is boosted by B. Vignettes, help pages and the 'boosted by' links of plot.network() updated.
#
# 2.0.2 (27 November 2026)  - Visualisation fixes. plot_bindensity() no longer prints a default title under the given one and
#                             gains main, xlab, labels and legend arguments. plot.network(plotter = 'igraph') sizes each box to
#                             its label; the visNetwork chart uses box nodes. Showcase vignette: hover details on the Gini against
#                             fitting time chart and an interactive ROC chart (plotly).
#
# 2.1.0 (27 November 2026)  - New: plot.network() draws with DiagrammeR (Graphviz layout) by default: boxes fitted to labels,
#                             colours by model type, hover a model for details, legend under the chart. It falls back to
#                             visNetwork, then igraph, when a package is missing. New argument legend. DiagrammeR added to
#                             Suggests. Showcase vignette and its Shiny app use the DiagrammeR chart.
#
# 2.2.0 (27 November 2026)  - New: model_from_config() and method MODEL$from_config() build a model network from a yaml or json
#                             file, yaml or json text, or a list: transformers and gradient transformers nest, features can be
#                             listed or read from files with dplyr operations, hyper-parameters can sit in their own block.
#                             New: CLS.RPART.DT (rpart decision tree) completed and exported. ENC.RML.TE gains 'smoothing'.
#                             plot_bindensity() gains remove_outliers. plot.network: fixed colours per model type; the igraph
#                             chart routes links around boxes. New vignette 'rml-config-gallery': eleven networks as configs
#                             (inst/extdata/gallery) with their networks, score densities, Gini and ROC curves.
#
# 2.2.1 (27 November 2026)  - Vignette 'rml-config-gallery' stops with a clear message when knitted with an installed rml older
#                             than 2.2.0, instead of 'could not find function model_from_config'.
#
# 2.2.2 (27 November 2026)  - Vignette 'rml-config-gallery': a network is skipped only for the Python modules it needs, the reason
#                             is shown in its card, and the lessons no longer print NA for networks that did not run.
#
# Versioning: x.y.z - increase z for a minor change or bug fix, y for a major change, x when features or packages are added.

# Description for Roxygen

#' rml: machine learning pipelines built from models and transformers
#'
#' rml wraps algorithms from R and Python packages (stats, xgboost, keras, scikit-learn, category_encoders and
#' others) in reference classes with one interface: \code{model$fit(X, y)} and \code{model$predict(X)}.
#' Models can be chained: a model's \code{transformers} produce its input features, and its
#' \code{gradient_transformers} are its base models, which it boosts by learning to correct their output.
#'
#' Start with \code{vignette('rml-getting-started', package = 'rml')} and see \code{\link{MODEL-class}} for
#' the settings shared by all models.
#' Python-based classes need a Python setup; see \code{\link{python_setup}} and \code{vignette('rml-troubleshooting')}.
#' @author Nicholas Berta
#' @keywords internal
#' @import magrittr
#' @import rutils
#' @rawNamespace import(dplyr, except = c(pick))
#' @importFrom methods new setRefClass
#' @importFrom foreach foreach %dopar%
#' @importFrom parallel stopCluster detectCores
#' @importFrom stats sd quantile median cor predict binomial glm lm as.formula runif rnorm rgeom setNames prcomp kmeans chisq.test dist hclust cutree na.omit var pchisq pf as.dist density
#' @importFrom utils head tail object.size read.csv write.csv txtProgressBar setTxtProgressBar
#' @importFrom graphics lines
"_PACKAGE"

#' Default model templates
#'
#' Named list of templates used by \code{\link{build_model_from_template}}. Each template gives a model
#' \code{class} and sets or ranges of config values to draw from.
#' @format Named \code{list}.
"default_templates"

#' @include abstract.R
#' @include mltools.R
#' @include transformers.R
#' @include encoders.R
#' @include mappers.R
#' @include binners.R
#' @include feature_generators.R
#' @include function_transformers.R
#' @include classifiers.R
#' @include clustering.R
#' @include gentools.R
#' @include ensemblers.R
#' @include boosters.R
NULL

