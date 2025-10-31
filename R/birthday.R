#' Birthday
#'
#' @param n a quantiative vector
#'
#' @returns The probability that two people share the same birthday
#' @export
#'
#' @examples birthday(1:20)
birthday = function(n)
{
  1 - exp(lfactorial(365) - lfactorial(365-n) - n*log(365))
}
