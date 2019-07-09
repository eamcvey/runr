
# group_age ---------------------------------------------------------------

#' Categories ages in age groups
#'
#' @param ages A numeric vector of ages
#' 
#' @return A character vector of age groups

group_age <- function(ages) {
  
  as.character(
    cut(ages, breaks = c(0, 18, 25, 30, 35, 40, 45, 50, 55, 60, 65, 100),
                  labels = c('under 18', '18-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65 and up'))
  )

} 

