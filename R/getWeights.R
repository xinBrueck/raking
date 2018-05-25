#' Return weights calculated using raking algorism 
#'
#' This function takes in a dataframe of individual survey data, each row represents an individual.
#' a dataframe of census constrain data and perform raking algorism to get the weights for each
#' individual based on the census constrain
#'
#' @param inds a dataframe of individual survey data, in the form of indicator matrix, each row represent an individual
#' @param cons a dataframe of census constrain data, each row is a different census region, each column represent a census category. 
#' @param feat a numerical vector representing the corresponding columns for each feature in the inds dataframe 
#' @param tols tolerence for the stopping point of the iteration of the raking algorism
#' @return A dataframe weights
#' @export


getWeights <- function(inds, cons, feat, tols = 1.15e-02){
  n_zones <- nrow(cons) # number of zones
  n_inds <- nrow(inds) # number of individuals
  nFeats <- length(feat) # number of features 
  
  
  ##get initial weight matrix
  weights <- matrix(data = 1, nrow = n_inds, ncol = n_zones)
  ind_aggs <- colSums(inds)
  ind_aggs <- t(apply(cons, 1, function(x) 1 * ind_aggs))
  
  
  cnts <- 1
  while (cnts < (nFeats*50)) {
    ncons  <- ifelse(cnts %% nFeats == 0, nFeats, cnts %% nFeats)
    n_starts <- ifelse(ncons == 1, 1, feat[ncons-1]+1)
    weight <- rakeWeights(ind = inds, con=cons, n_zone=n_zones, n_ind=n_inds, n_start=n_starts,
                          n_cons = feat[ncons], wt = weights, ind_agg = ind_aggs, tol = tols, count = cnts)
    cnts <- weight[[3]] + 1
    weights <- weight[[1]]
    ind_aggs <- weight[[2]]
  }
  
  return(weights)
  
}