#' BMI 3
#'
#' @param x Numeric Vector
#'
#' @returns Factor Variable
#' @export
#'
#' @examples
#' bmi.vals = rnorm(n=20, mean = 25, sd = 3)
#' bmi3(bmi.vals)
bmi3 <- function(x) {
  bmi.groups <- cut(x, breaks = c(0,25,30, Inf),right = FALSE)
return( bmi.groups) }
