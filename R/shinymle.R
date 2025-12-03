#' shinyMLE
#'
#' @returns A Shiny APP to find MLE
#' @export
#'
#' @examples
#' \dontrun{shinylme()}
shinymle = function()
{
  shiny::runApp(system.file("SHINY", package = "MATH4753F25matthewhouston"), launch.browser = TRUE)
}
