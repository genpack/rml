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

# Description for Roxygen

#' rml
#'
#' This package is a tool-box for machine learning and prediction.
#' @author Nicholas Berta
#'
#' @docType package
#' @name rml

# If you changed default_templates, run this before committing:
# default_templates %>% yaml::write_yaml('data/default_templates.yaml')

# Run these lines before building the package:
# yaml::read_yaml('data/default_templates.yaml') -> default_templates
# save(default_templates, file = 'data/default_templates.RData')

#' @import magrittr
#' @import rutils
#' @import rbig
#' 
#' @include abstract.R
#' @include mltools.R
#' @include transformers.R
#' @include encoders.R
#' @include mappers.R
#' @include binners.R
#' @include mappers.R
#' @include feature_generators.R
#' @include function_transformers.R
#' @include classifiers.R
#' @include clustering.R
#' @include gentools.R
#' @include ensemblers.R
#' @include boosters.R
#' 
#' 

