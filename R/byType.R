#' Return indicator matrix 
#'
#' This function takes in a dataframe representing a single categorical/numerical group.
#' based on the different types of data the input dataframe representing, 
#' it will perform different processure like cut, recode and etc and return a indicator matirx
#'
#' @param dat a dataframe representing a single categorical or numerical group to be processed
#' @param types representing the types of the input variable, possible values: "factor", "numeric","logic". 
#' if factor: recode the factors into different groups
#' if numeric: cut the numeric variables into factors
#' if logic(indicator matrix): group different factors if needed 
#' @param reqs rules as string for recode/cut/group based on the different types of input variable 
#' if factor: examples as "c(0)='No';c(1,2,3,4)='Yes'", recode 0 into 'No', recode 1,2,3,4 into 'Yes'
#' if numeric: input the cuts
#' if logic(indicator matrix): input the categories to be grouped together
#' @return A dataframe of the indicator matrix
#' @export
byType <- function(dat, types, reqs){
  
  if(types == "factor"){
    pDT <- car::recode(dat[,1], reqs)
    
  }else if(types == "numeric"){
    pDT <- cut(dat[,1],reqs, right=FALSE)
    
  }else{
    dtL <- dat
    len <- length(reqs)
    
    dtTemp <- vector("list", length(reqs)) 
    for(j in 1:(length(reqs)-1)){
      
      dtTem <- dtL %>% select(one_of(reqs[[j]])) %>% rowSums()
      dtTem <- as.data.frame(dtTem)
      colnames(dtTem) <- reqs[[len]][j]
      dtTemp[[j]] <- dtTem
      
      dtL <- dtL %>% select(-one_of(reqs[[j]]))
    }
    
    dtTemp[[len]] <- dtL
    pDT <- do.call("cbind", dtTemp)
  }
  pDT <- as.data.frame(pDT)
  return(pDT)
}

