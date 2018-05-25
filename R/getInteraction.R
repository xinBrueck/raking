#' Helper function for shinyapp
#' 
#'
#' This function get the interactions of two variables in the logic form
#'
getInteraction <- function(dat, inters){
  ####get the interactions
  datIs <- vector("list", length(inters))
  for (inter in inters){
    datI <- vector("list", length(inter))
    for (n in 1:length(inter)){
      datI[[inter[n]]] <- dat[[inter[n]]]
    }
    nm <- paste(inter, collapse = "By")
    datIs[[nm]] <- do.call("left_join", datI)
  }
  
  return(datIs)
}
