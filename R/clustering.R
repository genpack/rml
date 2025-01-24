# Header
# Filename:     clustering.R
# Version History:
# Version   Date                 Action
# ----------------------------------
# 1.0.0     16 September 2013    Initial Issue.
# 1.1.0     20 September 2017    function elbow.plot() renamed to elbow() and modified:
#                                returns wss, Argument doPlot added, if TRUE plots
# 1.2.0     21 September 2016    Function bnc() added: best number of clusters
# 1.2.1     21 September 2016    Function elbow() modified and exported: returns a list containing:
#                                wgss: within group sum of squares
#                                clst: clustering list: list of all kmenas or skmeans clusterings
#                                bnc: best number of clusters
# 1.2.2     21 September 2016    Argument bnc_threshold added to functions bnc() and elbow()
# 1.2.3     28 May 2019          function elbow() modified: for euclidean metric, max.num.cluster now can be equal to nrow(M)
# 1.2.4     26 March 2021        function elbow() modified: Argument max.num.clusters changed to num.clusters specifying a set of values for number of clusters to be tested
# 1.2.6     27 April 2021        function cluster_tree() added: runs a hierarchical clustering with optimal selection of depths
# 1.2.7     08 June 2023         function cluster_tree modified: gives full information for each number of cluster from 1 to number of points

cluster.distances <- function(M, SK){
  # Returns a vector containing the distances of each point from the center of
  # its cluster.
  N = nrow(M)
  distances = c()
  for (i in 1:N){
    distances = c(distances, spherical.distance(M[i,],SK$prototype[SK$cluster[i], ]))
  }
  return(distances)
}

cluster.moments <- function(M, SK){
  # Returns the within group sum of dissimilarities (moments) of the given spherical clustering
  #   Input 1: M (A n X m Matrix of data, where n is the number of points (vectors or objects) and
  #               m is the number of dimensions of the vectors. )
  #               (points are arranged as rows)
  #   Input 2: SK (A spherical k-means object. Output of function skmeans())
  #   Output: A vector of length k of real numbers containing sum of dissimilarities of each cluster,
  #           where k is the number of clusters.
  k = dim(SK$prototypes)[1]
  n = dim(M)[1]
  wgss = rep(0, k)
  for (i in 1:n){  # For each vector
    cluster.number = SK$cluster[i]
    wgss[cluster.number] =  wgss[cluster.number] + spherical.distance(M[i,], SK$prototype[cluster.number,])
  }
  return(wgss)
}

#' Use this function to find the best number of clusters
#'
#' @param M A matrix containing vectors for clustering. Each row defines a vector.
#' @param max.num.clusters maximum number of clusters
#' @param metric Either 'euclidean' or 'spherical' determining the metric used for clustering
#' @param bnc_threshold Specifies the threshold for reduction ratio in within group sum of squares
#' @param doPlot logical: Would you like to see the elbow plot to determine the number of clusters?
#' @param num.clusters set of values for number of clusters to test. 
#' @return list: containing three elements: wgss (Within group sum of squares), clst (list of clustering objects), bnc(best number of clusters)
#' @examples
#' a = elbow(iris[,1:4], num.clusters = c(2, 5, 10, 15, 20, 25), doPlot = T)
#'
#' a$wgss
#'       NC2       NC5      NC10      NC15      NC20      NC25 
#' 152.34795  46.46117  29.90776  21.67031  17.78198  11.90241 
#' (Your values may be different!)
#' 
#' a$clst[[a$bnc]]$cluster %>% table
#' 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
#' 15 10  7 13  8  8  1 10  4  6  3  5  8  9  5  5  5 14 10  4 
#' (Your values may be different!)
#' @export
elbow <- function(M, max.num.clusters = 25, metric = "euclidean", doPlot = T, num.clusters = NULL, bnc_method = "jump_threshold", ...) {
  if(!inherits(M, 'matrix')){M %<>% as.matrix}
  nr = nrow(M)
  assert(nr > 2, "Number of rows must be greater than 1", err_src = 'elbow')
  
  max.num.clusters = verify(max.num.clusters, domain= c(2, NA), default = nrow(M) - 1)
  
  if(is.null(num.clusters)){
    num.clusters = sequence(max.num.clusters) %-% 1
  }
  
  num.clusters = num.clusters[num.clusters < nr]
  if(!is.null(max.num.clusters)){num.clusters = num.clusters[num.clusters <= max.num.clusters]}
  
  assert(!(1 %in% num.clusters), 'Number of clusters must be greater than 1', err_src = 'elbow')
  
  wgss = c()
  clst = list()
  if (metric == "euclidean"){
    for (nc in num.clusters) {
      if(nc == nr){
        K = list(cluster = sequence(nr), tot.withinss = 0)
      } else {K = kmeans(M, nc)}
      
      wgss = c(wgss, K$tot.withinss)
      clst %<>% list.add(K)
    }
    names(wgss) <- paste0('NC', num.clusters)
    names(clst) <- paste0('NC', num.clusters)
    
    if(doPlot){plot(num.clusters, wgss, type="b")}
    out = list(wgss = wgss, clst = clst)
    return(list(wgss = wgss, clst = clst, bnc = wgss %>% best_num_clusters(method = bnc_method, ...)))
  }
  else if (metric == "spherical"){
    library("skmeans")
    for (nc in num.clusters){
      SK    = skmeans(M, nc)
      tot.withinss = sum(cluster.moments(M, SK))
      wgss = c(wgss, tot.withinss)
      clst %<>% list.add(K)
    }
    names(wgss) <- paste0('NC', num.clusters)
    names(clst) <- paste0('NC', num.clusters)
    
    if(doPlot){plot(num.clusters, wgss, type="b")}
    return(list(wgss = wgss, clst = clst, bnc = wgss %>% best_num_clusters(method = bnc_method, ...)))
  }
}

#' @export
best_num_clusters <- function(wss, method =c("jump_threshold", "gain_and_cost", "min_slope", "best_break", "angle_threshold") , jump_threshold = 0.2, angle_threshold = 70){
  method = match.arg(method)
  N = length(wss)
  ncls = names(wss) %>% gsub(pattern = "NC", replacement = "") %>% as.numeric
  if(is.empty(ncls)){ncls = len_seq(wss)}
  
  if(method == 'jump_threshold'){
    w   =  which((wss[-N] - wss[-1])/wss[-N] < jump_threshold)
    while(!is.empty(w)){
      wss = wss[- w - 1]
      N   = length(wss)
      w   =  which((wss[-N] - wss[-1])/wss[-N] < jump_threshold)
    }
    return(ncls[N] %>% as.integer)
  } else if (method == 'gain_and_cost'){
    gain = c(0, wss) %>% vect.map %>% {1-.[-1]}
    cost = c(0, ncls) %>% vect.map %>% {.[-1]}
    return(ncls[order(gain/cost) %>% {.[length(.)]}])
  } else if (method == 'min_slope'){
    bncs = c()
    for(i in sequence(N - 1)){
      a = (wss - wss[i])/(ncls - ncls[i])
      a = a[ - sequence(i)]
      b = ncls[ - sequence(i)]
      bncs = c(bncs, b[order(a) %>% first])
    }
    a = table(bncs)
    b = names(a) %>% as.integer
    return(b[which(a == max(a))] %>% max)
  } else if(method == "best_break"){
    ss = sequence(N-2) + 1
    nn = sequence(N)
    minr = Inf
    for(i in ss){
      for (j in nn[nn < i]){
        v = (wss[j] - wss[i])/(ncls[i] - ncls[j])
        if(v > 0){
          for (k in nn[nn>i]){
            u = (wss[i] - wss[k])/(ncls[k] - ncls[i])
            if(u > 0){
              r = u/v
              if(r < minr){
                minr = r
                mini = i
                minj = j
                mink = k
              }
            }
          }
        }
      }
    }
    return(ncls[mini])
  } else if(method == 'angle_threshold'){
    a = wss[-N] - wss[-1]
    b = ncls[-1] - ncls[-N]
    t = 180*atan(a/b)/pi
    w = which((t > 0) & (t < angle_threshold))
    return(best_num_clusters(wss, method = 'best_break'))
  }
}

#' @export
cluster_tree <- function(distmat, max_num_clusters = as.integer(nrow(distmat)/2), show_progress = T){
  # np: number of points
  np = nrow(distmat)
  assert(np == ncol(distmat), "distmat must be a square matrix!")
  
  distmat %>% as.dist %>% hclust -> hc
  ct = cutree(hc, k = sequence(np))

  cluster_size = list()

  within_group_mean_distance = list()
  within_group_median_distance = list()
  within_group_max_distance = list()

  between_group_mean_distance = list()
  between_group_median_distance = list()
  between_group_min_distance = list()
  
  cols = rownames(ct)
  if(show_progress){pb = txtProgressBar(min = 1, max = max_num_clusters, style = 3)}
  for(i in sequence(max_num_clusters)){
    itn = paste0('N', i)
    if(show_progress){setTxtProgressBar(pb, i)}
    
    cluster_size[[itn]] <- ct[,i] %>% table
    
    within_group_mean_distance[[itn]] <- numeric(i)
    within_group_median_distance[[itn]] <- numeric(i)
    within_group_max_distance[[itn]] <- numeric(i)
    for(cn in which(table(ct[,i]) > 1)){
      fet = cols[which(ct[, i] == cn)]
      within_group_mean_distance[[itn]][cn] <- mean(distmat[fet, fet], na.rm = T)
      within_group_median_distance[[itn]][cn] <- median(distmat[fet, fet], na.rm = T)
      within_group_max_distance[[itn]][cn] <- max(distmat[fet, fet], na.rm = T)
    }
    
    between_group_mean_distance[[itn]] <- matrix(rep(0, i*i), nrow = i)
    between_group_median_distance[[itn]] <- matrix(rep(0, i*i), nrow = i)
    between_group_min_distance[[itn]] <- matrix(rep(0, i*i), nrow = i)
    for(ii in sequence(i)){
      for(jj in sequence(i)){
        fet_ii = cols[which(ct[, i] == ii)]
        fet_jj = cols[which(ct[, i] == jj)]
        between_group_mean_distance[[itn]][ii,jj] <- mean(distmat[fet_ii, fet_jj])   
        between_group_median_distance[[itn]][ii,jj] <- median(distmat[fet_ii, fet_jj])   
        between_group_min_distance[[itn]][ii,jj] <- min(distmat[fet_ii, fet_jj])   
      }
    }
  }
  if(show_progress){close(pb)}
  out = list(hc_cutree = ct, 
             cluster_size = cluster_size,
             within_group_mean_distance = within_group_mean_distance,
             within_group_median_distance = within_group_median_distance,
             within_group_max_distance = within_group_max_distance,
             between_group_mean_distance = between_group_mean_distance,
             between_group_median_distance = between_group_median_distance,
             between_group_min_distance = between_group_min_distance,
             hc_object = hc)
  return(out)
}

# Initial results from RP seem to be too good: Gini: 0.66, prec_2: 0.6, lift_2: 16.5. Most important features are:
#   
#   originalApplicatioHasPersonalLoanFunded
# 
# originalDaysBetweenApplicationAndFirstFunding
# 
# I don’t know why these two features did not show up in Jon’s leaky feature detector notebook, but they are most probably still leaky.

