#' US Mortality table from 2007
#'
#' Mortality table from the US Social Security Adminstration 
#' issued in 2007.
#' 
#' @format A data frame with 240 rows. Each row corresponds
#' to one age year from 0 to 120. 
#' - `Age`: The age year. 0 corresponds to birth through one year of age.
#' - `Sex`: The sex for which the row is relevant.
#' - `Mortality`: The fraction of people of that age who died in 2007.
#' - `LifeExpectancy`: The calculated "life expectancy" at that age for that sex.
#' 
#' @details Life expectancy is a measure constructed from 
#' a simulation. Start with 100,000 people at birth. Use the 
#' mortality at each age to follow the living through successive
#' ages. The "life expectancy at birth" (age 0) is the average
#' age at death of those 100,000 people. For the life expectancy in
#' year n, consider only that fraction of the original 100,000 who survived to
#' year n. The life expectancy will be the average time until death for those survivors.
#' 
#' @docType data
"SSA_2007"
