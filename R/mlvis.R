# mlvis
# Gives an interactive plotly bar plot indicating counts of each binary class based on feature values
# Choose the feature from the dropdown on the top and use the slider to adjust your desired value range
# The barchart shows counts of each class for all cases for which the selected feature values 
# fall into the specified value range
# packages plotly and crossbar are required
#' @title Interactive class counts by feature threshold
#' @description
#' Interactive plotly bar or pie chart of the count of each binary class for rows whose feature value is below a threshold chosen with a slider (requires plotly, crosstalk, reshape2).
#' @param X \code{data.frame} of features.
#' @param y Binary target.
#' @param ncuts Number of thresholds.
#' @param C0_tag,C1_tag Class labels.
#' @param round_digits Rounding of thresholds.
#' @param horizontal \code{logical} horizontal bars.
#' @return A crosstalk widget.
#' @rdname binary_class_plots
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

#' @rdname binary_class_plots
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
#' Outliers are removed first, unless \code{remove_outliers = FALSE}. Both densities share one y axis, so neither curve is cut off.
#'
#' @param x \code{numeric} vector.
#' @param y binary vector of the same length.
#' @param main \code{character} chart title. Default is no title.
#' @param xlab \code{character} x axis label.
#' @param labels \code{character} names of class 0 and class 1, shown in the legend.
#' @param legend \code{logical} add a legend? Default \code{TRUE}.
#' @param remove_outliers \code{logical} drop outliers of \code{x} first? Default \code{TRUE}. Use \code{FALSE} for
#'   model scores, whose extreme values matter.
#' @param ... further arguments passed to \code{plot()}.
#' @export
plot_bindensity = function(x, y, main = '', xlab = '', labels = c('0', '1'), legend = TRUE, remove_outliers = TRUE, ...){
  rmv = if(remove_outliers) which(x %>% outlier(recursive = T)) else integer()
  if(length(rmv) > 0){
    x = x[- rmv]
    y = y[- rmv]
  }
  d0 = density(x[which(y == 0)], na.rm = TRUE)
  d1 = density(x[which(y == 1)], na.rm = TRUE)
  plot(d0, main = main, xlab = xlab, ylim = c(0, max(d0$y, d1$y)), ...)
  lines(d1, col = 'red')
  if(legend){
    graphics::legend('topright', legend = labels, col = c('black', 'red'), lty = 1, bty = 'n', cex = 0.8)
  }
  invisible(list(d0 = d0, d1 = d1))
}


