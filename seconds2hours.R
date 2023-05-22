# creating a function that convert second to hour
#one second is 1/3600 hr 
# x is a numeric vector
seconds2hours <- function(x) {
  stopifnot(is.numeric(x))
  x / 3600
}
