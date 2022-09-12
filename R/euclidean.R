#' Euclidean Algorithm
#'
#' `euclidean()` Calculate the greatest common divisor between numbers a and b.
#'
#' @param scalar number a
#' @param scalar number b
#' @returns the greatest common divisor of a and b
#' @seealso https://en.wikipedia.org/wiki/Euclidean_algorithm
#' @examples
#' euclidean(a,b)
#'
#' @export
#' 
euclidean <-
function(a,b){
  if(a<b){
    exc=b
    b=a
    a=exc
  }
  while(b!=0){
    t=b
    b=a%%b
    a=t
  }
  return(a)
}
