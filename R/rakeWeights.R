#' Get weights for a single feature 
#'
#' This function takes in the indicator matrix for one feature and the census data.
#' using the raking algorism to calculate the weights for the feature according to the census constrain
#'
#' @return A list of the resulting weights, number of total iteration
#' each iteration will compare the difference between the current weights and the previous weights, if there is
#' practically no difference the number of iteration will be overwrote to 1000000001 to get out ot the iteration
rakeWeights <- function(ind, con, n_zone, n_ind, n_start, n_cons, wt, ind_agg, count, tol=1.15e-02){
  colnames(ind_agg) <- names(con)
  
  weight0 <- wt
  for(j in 1:n_zone){
    for(i in n_start:(n_start+n_cons-1)){
      wt[ind[, i] == 1, j] <- con[j, i] / ind_agg[j, i]
    }
  }
  
  weights <- weight0 * wt
  
  for(i in 1:n_zone){
    ind_agg[i, ] <- colSums(ind * weights[, i])
  }
  
  count <- ifelse(all.equal(weight0, weights, tolerance = tol) == TRUE, 1000000001, count)
  
  mylist <- list(weights, ind_agg, count)
  print(sprintf("Final Weight Matrix"))
  print(weights[c(1:10),])
  print(sprintf("Survey Input Totals"))
  print(colSums(ind))
  print(sprintf("Census Input Totals"))
  print(con)
  print(sprintf("colSum(surveyInput x weights)"))
  print(ind_agg)
  
  print(sprintf("# of interations: %d", count))
  print(sprintf("# of levels in this category: %d", n_cons))
  return(mylist)
}
