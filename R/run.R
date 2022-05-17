#' Run a local instance of protoshiny
#'
#' @export
#' @importFrom shiny runApp
#' @examples
#' \dontrun{
#'     visualize_hc()
#' }
#' @return No return value, launches the protoshiny Shiny application.
#' @importFrom shiny shinyApp
visualize_hc <- function() {
  runApp(shinyApp(get_ui(), get_server()))
}
