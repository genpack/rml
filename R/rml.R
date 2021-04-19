# Header
# Filename:       rml.R
# Description:    A pipeline and toolbox for R programmers useful for building products for machine learning and prediction
# Version History:
# 0.0.0 (05 August 2013)    - Initial Issue
# 0.1.0 (23 February 2014)  - Second Issue
# 0.1.2 (06 August 2014)    - Third Issue
# 0.1.3 (13 October 2014)   - Forth Issue
# 0.2.2 (15 October 2014)   - Fundamental Changes: transformers transferred to separate files. Super classes created for sklearn and category_encoders packages.
# 0.2.3 (21 October 2014)   - reserved_words added to class MODEL
# 0.2.4 (21 October 2014)   - All sklearn modules inherit from TRM.SKLEARN
# 0.2.6 (22 October 2014)   - function model_update added to mltools, exported and added to namespace
# 0.2.8 (22 October 2014)   - argument update added to function model_load
# 0.2.11 (22 October 2014)  - minor changes in function scorer
# 0.3.1 (30 October 2014)   - Sampling parameters added to the config: smp.enabled, smp.ratio, smp.num_rows, smp.fix_class_ratio
# 0.3.3 (05 November 2014)  - WIDETABLE is converted to matrix for all sklearn transformers
# 0.3.5 (11 November 2014)  - templates.R updated: minor issues fixed
# 0.3.7 (25 November 2014)  - abstract.R updated: upsampling and downsampling added
# 0.4.0 (05 January 2015)   - abstract.R updated: smote upsampling with python package smote_variant added
# 0.4.5 (06 January 2015)   - abstract.R updated: upsampling engine moved to after transformation. fix_class_ratio renamed to class_ratio
# 0.5.1 (12 January 2015)   - classifier.R updated: CLS.SKLEARN calls initialize method of its second parent: CLASSIFIER
# 0.5.2 (13 January 2015)   - abstract.R updated: logical parameter 'name_in_output' added to config
# 0.6.0 (01 February 2015)  - mltools.R updated: functions scorer() and correlation() modified.
# 0.6.2 (11 February 2015)  - Module mlvis.R added with two functions: binary_class_bar() and binary_class_pie()
# 0.6.5 (16 March 2015)     - function evaluate_features() in gentools.R updated and exported.
# 0.6.6 (17 March 2015)     - functions outlier() and plot_bindensity() added.
# 0.6.8 (18 March 2015)     - function correlation() changed: optsplit.chi and optsplit.f1 added
# 0.6.10 (31 March 2015)    - Method model.load() changed for SKLearn models. Module is reloaded.
# 0.7.1 (1 April 2015)      - Boosters initiated: Function feature_booster(), train_model() and evaluate_models() added.
# 0.7.2 (12 April 2015)     - Minor fix in function correlation(). NULL is passed to argument 'threshold' 


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
#' @include gentools.R
#' @include ensemblers.R
#' @include boosters.R
#' 
#' 

