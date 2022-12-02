# mlvis
# Gives an interactive plotly bar plot indicating counts of each binary class based on feature values
# Choose the feature from the dropdown on the top and use the slider to adjust your desired value range
# The barchart shows counts of each class for all cases for which the selected feature values 
# fall into the specified value range
# packages plotly and crossbar are required
#' @export 
binary_class_bar = function(X, y, ncuts = 1000, C0_tag = 'C0', C1_tag = 'C1', round_digits = 2, horizontal = T){
  
  cbind(X, label = y) %>% bucket_moments(colnames(X), 'label', ncuts = ncuts) -> res
  
  res %<>% mutate(C0 = cumulative_count - cumulative_sum_V2, cutpoint = round(cutpoint_V1, round_digits)) %>% 
    select(V1, cutpoint, C0, C1 = cumulative_sum_V2)
  colnames(res) <- c('feature', 'cutpoint', C0_tag, C1_tag)
  reshape2::melt(res, measure.vars = c(C0_tag, C1_tag)) %>% 
    crosstalk::SharedData$new() -> shared_features
  
  if(ncol(X) == 1){
    fig = crosstalk::bscols(
      list(
        crosstalk::filter_slider('cutpoint', 'Threshold', shared_features, ~cutpoint),
        shared_features %>% plotly::plot_ly(x = ~value, y = ~variable, type = 'bar')
      )  
    )
  } else {
    fig = crosstalk::bscols(
      list(
        list(
          crosstalk::filter_select('feature', 'Feature', shared_features, ~feature, multiple = F),
          crosstalk::filter_slider('cutpoint', 'Threshold', shared_features, ~cutpoint)),
        shared_features %>% plotly::plot_ly(x = ~value, y = ~variable, type = 'bar')
      )  
    )
  }
  return(fig)
}

#' @export 
binary_class_pie = function(X, y, ncuts = 1000, C0_tag = 'C0', C1_tag = 'C1', round_digits = 2, horizontal = T){
  
  cbind(X, label = y) %>% bucket_moments(colnames(X), 'label', ncuts = ncuts) -> res
  
  res %<>% mutate(C0 = bucket_count - bucket_sum, cutpoint = round(cutpoint, round_digits)) %>% 
    select(V1, cutpoint, C0, C1 = bucket_sum)
  colnames(res) <- c('feature', 'cutpoint', C0_tag, C1_tag)
  reshape2::melt(res, measure.vars = c(C0_tag, C1_tag)) %>% 
    crosstalk::SharedData$new() -> shared_features
  
  crosstalk::bscols(
    list(
      list(
        crosstalk::filter_select('feature', 'Feature', shared_features, ~feature, multiple = F),
        crosstalk::filter_slider('cutpoint', 'Threshold', shared_features, ~cutpoint)),
      shared_features %>% plotly::plot_ly(values = ~value, labels = ~variable, type = 'pie')
    )  
  )
  
}


#' This function plots density of vector \code{x} for elements for which \code{y} is 0 in black and
#' another density plot for elements for which \code{y} is 1 in red.
#' 
#' @export
plot_bindensity = function(x, y){
  rmv = which(x %>% outlier(recursive = T))
  if(length(rmv) > 0){
    x = x[- rmv]
    y = y[- rmv]
  }
  w0 = which(y == 0)
  w1 = which(y == 1)
  
  x[w0] %>% density %>% plot
  x[w1] %>% density %>% lines(col = 'red')
}


