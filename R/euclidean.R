#' Euclidean Algorithm
#'
#' `euclidean()` Calculate the greatest common divisor between numbers a and b.
#'
#' @param a number a
#' @param b number b
#' @returns the greatest common divisor of a and b
#' @seealso https://en.wikipedia.org/wiki/Euclidean_algorithm
#' @examples
#' euclidean(9,27)
#'
#' @export
#' 
euclidean <-
function(a,b){
  stopifnot(length(a)==1 & is.numeric(a))
  stopifnot(length(b)==1 & is.numeric(b))
  a=abs(a)
  b=abs(b)
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
