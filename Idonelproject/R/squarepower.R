#' testing example square number
#'
#' input new number to square it.
#' @param x A numeric number to be squared
#' @return the number square from input
#' @export

square <- function(x){
  return(x^2)
}

#' @title finding compund interest
#'
#'@name compund
#'@descrption finding compund interst for CD IRA
#'@param p is principal amount 
#'@param i is annual interest rate 
#' @param n is number of compound periods for a year
#' @return the compund interest 
#' 
#' 
#' @export

interest <- function( p, i, n){
  return(p*(1+i)^n-p)
  
}


