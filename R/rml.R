# Header
# Filename:       rml.R
# Description:    A pipeline and toolbox for R programmers useful for building products for machine learning and prediction
# Version History:
# 0.0.0 (05 August 2013)   - Initial Issue
# 0.1.0 (23 February 2014) - Second Issue
# 0.1.2 (06 August 2014)   - Third Issue
# 0.1.3 (13 October 2014)  - Forth Issue
# 0.2.0 (15 October 2014)  - Fundamental Changes: transformers transferred to separate files. Super classes created for sklearn and category_encoders packages.


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

