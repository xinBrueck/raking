This project is to re-weight survey data by applying `Raking Algorithm`.    

People take surveys to learn about the whole population. But often times, the   
survey sample is not a good representation of the whole population, possibly due to:     
- Sophisticated Sampling Schemas
- Low Response Rate

By using `Raking Algorithm`, we can adjust the weights for each respondent in the survey based on known population totals, for instance census totals. That way, we can force the survey totals to match the known population totals, and the re-weighted sample would be a better representation of the whole population

## Main Analysis Steps

- Develop `Rshiny App` for:
  * Preprocess survey data
  * Get US Census 2010 data

- Apply `Raking Algorithm`:
  Iteratively estimate weights across target variables in turn until weights converge

- Sensitivity Analysis to evaluate raking results for biased sampling schemas against simple random sampling schemas
  * From the survey data, take 1000 samples for each sampling schemas
    1. Simple Ramdom Sampling
    2. Biased Sampling

  * Use US Census 2010 totals to adjust weights for the 1000 biased samples
    1. weights adjusted by age, gender and race

  * Get point estimate for target variables(for instance: pct of each education levels) for the 2000 samples

  * Get `Mean`,`Variance`, `Confidence Interval` for target variables for each sampling schemas and compare the results


## Next Steps

- Adjust weights using more variables
- Implement ways to account for survey instrument with multiple choices

## Main Language/Framework Used

- [Rshiny](https://shiny.rstudio.com/)
- [Rmarkdown](https://rmarkdown.rstudio.com/)
- [Roxygen2](http://r-pkgs.had.co.nz/man.html)
