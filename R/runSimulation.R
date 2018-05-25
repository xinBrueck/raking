#' use getSim to run the simulation
#' 
#'
#' This function will run 2 simulations and print out the weighted mean/variance/CIs of the different levels of education and citybelongs 
#' simulation 1: take a random sample of 1000 individuals
#' simulation 2: take a biased sample of 1000 individuals, with 70% femals and 30% males
#'
#' @export
runSimulation <- function() {
  ################################round 1. random sample#################################################
  ##get the simulation
  simResult_ran <- getSim(surveyInput = surveyDt, censusInput = censusDt, sample = 1000, seed = 1208, biased = 0)
  
  
  ######################round 2. biased sample, with the prob of getting female == 0.7###################
  ##get the simulation
  simResult_biased <- getSim(surveyInput = surveyDt, censusInput = censusDt, sample = 1000, seed = 1208, biased = 0.7)
  
  
  ##getting the census data for education to compare 
  # eduVar <- "B06009_005E,B06009_006E,B06009_003E,B06009_002E,B06009_004E"
  # edu <- CensusAPI2010s(trimws(unlist(strsplit(eduVar,','))), statefip, levels, keys, "ACS")
  # edu <- edu %>% filter(edu$fips %in% counties)
  # eduCensus <- edu[,-1]
  eduCensusPct <- eduCensus/sum(eduCensus)
  names(eduCensusPct) <- c("Bachelor", "Graduate", "High School", "Less Than HS", "Some College")
  
  ##compare
  ##show the reweight results for education levels
  ####education levels####
  print("Census percentage for education levels")
  eduCensusPct   ## Census percentage for education levels
  
  print("random simulation mean for education")
  simResult_ran[[3]]
  print("random simulation CI for education")
  simResult_ran[[4]]   ##CI
  print("random simulation variance for education")
  simResult_ran[[2]]   ##variance
  
  print("biased simulation mean for education")
  simResult_biased[[3]]   ##mean
  print("biased simulation CI for education")
  simResult_biased[[4]]   ##CI
  print("biased simulation variance for education")
  simResult_biased[[2]]   ##variance
  
  
  ########city belongings######
  ##Simple Random Sample Result
  print("random simulation mean for city belonging")
  simResult_ran[[7]]   #mean
  print("random simulation CI for city belonging")
  simResult_ran[[8]]   #CI
  print("random simulation variance for city belonging")
  simResult_ran[[6]]   #variance
  
  ##Biased Sample Result
  print("biased simulation mean for city belonging")
  simResult_biased[[7]]   #mean
  print("biased simulation CI for city belonging")
  simResult_biased[[8]]   #CI
  print("biased simulation variance for city belonging")
  simResult_biased[[6]]   #variance
  
}