#' myFirstFunction
#'
#' @param x A number
#'
#' @returns A list of x and y, y is x^2
#'
#' @examples
#' myFirstFunction(1:20000)
#'
#' @export
#'
#'
myFirstFunction <- function(x)
{
  y = x^2
  plot(y~x)

  list(x = x, y = y)
}
