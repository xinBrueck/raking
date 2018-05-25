#' Return resulting percents/variance/mean/CIs for education level and citybelonging levels
#' 
#'
#' This function takes in a dataframe of individuals survey data containing gender, race, age info as in indicator matrix form.
#' takes n samples, perform raking algorism on each sample to get the weights, use the weights to get the percentage for each levels
#' in education and citybelongings.Finally get the mean, variance, CIs for the levels percentages using the samples. 
#'
#' @param surveyInput dataframe of individuals survey data containing gender, race, age info
#' @param censusInput dataframe of census constrain data, each column represents a census category
#' @param sample number of samples to take
#' @param seed seeds to be used for sampling
#' @param biased if biased > 0, use biased as the prob of sampling female individuals
#' @return A list of the resulting percentages in the samples, the mean/variance/CIs for education levels and citybelonging levels
#' @export

getSim <- function(surveyInput,censusInput,sample, seed, biased){
  ##get a dataframe to hold the 1000*5 percentages, 5 is for there are 5 levels in education level
  edupercents <- matrix(NA, sample, 5)
  cityBelongingpercents <- matrix(NA, sample, 10)
  set.seed(seed)
  for (i in 1:sample){
    ##get the sample, 10% of the file
    ##here the surveyDt is already Processed and have gender, marital status, hispanic,race, education, age for reweighting
    ##if biased > 0, use biased as the prob of getting female, put more weight on female when sampling, female with 70% male with 30%
    if(biased > 0 && biased < 1){
      ##get the weight variable
      surveyInput$genderWt <- ifelse(surveyInput$gender.pDT.Female==1,biased/sum(surveyInput$gender.pDT.Female), (1-biased)/sum(surveyInput$gender.pDT.Male))
      sample <- sample(seq_len(nrow(surveyInput)), 135, prob=surveyInput$genderWt,replace = TRUE)
    }else{
      sample <- sample(seq_len(nrow(surveyInput)), 135,replace = TRUE)
    }
    sampleDt <- surveyInput[sample,]
    
    ##after sample need to separate the sampleDt into reweight and test group
    ##use gender, race, age for reweight
    #feat <- c(2, 5,2,6,5,15)
    reweightDt <- sampleDt %>% select(c(1:2, 10:15, 31:45))
    ##education for test now
    testEdu <- sampleDt %>% select(c(16:20))
    testCityBelonging <- sampleDt %>% select(c(21:30))
    
    ##censusInput
    ##get the weight
    ##gender:2 marital status 5
    feat <- c(2,6,15)
    
    ##get the weights
    weights <- getWeights(reweightDt, censusInput, feat)
    
    ##get the edu percentage
    eduMargins  <- colSums(testEdu* weights)
    eduPct <- eduMargins/sum(eduMargins)
    edupercents[i,] <- eduPct
    
    ##get the citybelonging percentage
    cityBelongingMargins  <- colSums(testCityBelonging* weights)
    cityBelongingPct <- cityBelongingMargins/sum(cityBelongingMargins)
    cityBelongingpercents[i,] <- cityBelongingPct
  }
  
  table(sampleDt$gender.pDT.Female)  ##check if the resample weight is applied 
  table(sampleDt$gender.pDT.Male)
  ##get the mean
  eduMean <- colMeans(edupercents)
  cityBelongingMean <- colMeans(cityBelongingpercents)
  
  ##get the variance
  eduVariance <- colSums((sweep(edupercents,2,eduMean))^2)/999
  cityVariance <- colSums((sweep(cityBelongingpercents,2,cityBelongingMean))^2)/999
  ##get the CI
  quants <- c(0.025,0.975)
  eduCIs <- apply( edupercents, 2 , quantile , probs = quants)
  cityCIs <- apply( cityBelongingpercents, 2 , quantile , probs = quants)
  
  result <-  vector("list", 8)
  
  result[[1]] <- edupercents
  result[[2]] <- eduVariance
  result[[3]] <- eduMean
  result[[4]] <- eduCIs
  
  result[[5]] <- cityBelongingpercents
  result[[6]] <- cityVariance
  result[[7]] <- cityBelongingMean
  result[[8]] <- cityCIs
  return(result)
  
}













