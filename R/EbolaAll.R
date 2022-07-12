#' Case numbers in an Ebola outbreak in 2014
#' 
#' In December 2013, an 18-month-old boy from a village in Guinea suffered fatal diarrhea.  Over the next months a broader outbreak was discovered, and in mid-March 2014, the Pasteur Institute in France confirmed the illness as Ebola-Virus Disease caused by the Zaire ebolavirus. 

#' Although the outbreak was first recognized in Guinea, 
#' it eventually encompassed Liberia and Sierra Leone 
#' as well.  By July 2014, the outbreak spread to the 
#' capitals of all three countries.  
#' 
#' - `EbolaAll` gives daily reports for Guinea, Liberia, and Sierra Leone.
#' - `EbolaGuinea` contains just the Guinea data, but adds
#' columns containing a 7-day moving average.
#' 
#' 
#' @source US Centers for Disease Control (CDC). 
#'
#' @format A dataframe with 182 rows. Each row is a daily report 
#' during a period of over 18 months during 2014 and 2015. 
#' Each report gives the number of new cases and disease-related 
#' deaths since the last report in each or three countries: 
#' Sierra Leon, Liberia, and Guinea. These values have been calculated 
#' from the raw, cumulative data. The data have been scrubbed to remove 
#' obvious errors. 
#' - `Date`: Date when the World Health Organization issued the report 
#' - `Gcases`:  Number of new cases in Guinea 
#' - `Gdeaths`: Number of new deaths in Guinea 
#' - `Lcases`: Number of new cases in Liberia 
#' - `Ldeaths`: Number of new deaths in Liberia 
#' - `SLcases`: Number of new cases in Sierra Leone 
#' - `SLdeaths`: Number of new deaths in Sierra Leone
#' - `TotCases`: Cumulative number of cases across all three countries 
#' - `TotDeaths`: Cumulative number of deaths across all three countries 
#' Added in `EbolaGuinea`
#' - `Days`: When the report was issued in terms of a count of days from the initial report.
#' - `G7Rcases`:  Number of new cases in Guinea averaged across 7 reports 
#' -  `G7Rdeaths`:  Number of new deaths in Guinea averaged across 7 reports 
#'
#' @docType data
"EbolaAll"
#' @rdname EbolaAll
"EbolaGuinea"

