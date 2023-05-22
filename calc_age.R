
#creating a function that calculate age by sustracting this year from  the date of birth year.
#birth takes numeric vector/ birth year
#this year takes single numeric value/ current year or year of data collection
calc_age <- function(birth, thisyear = 2020) {
  stopifnot(is.numeric(birth))
  thisyear - birth
}