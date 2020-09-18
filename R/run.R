#' Run a local instance of protoshiny
#'
#' @export
#' @importFrom shiny runApp
#' @examples
#' \dontrun{
#'     visualize_hc()
#' }
#' @importFrom shiny shinyApp
visualize_hc <- function() {
  runApp(shinyApp(get_ui(), get_server()))
}
