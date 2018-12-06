
#' Start DiscoveR
#' @title This function will start DiscoveR
#' @return Nothing
#' @description An interactive Shiny application for exploring data.
#' @details This starts the DiscoveR application on the user's local computer.
#' @keywords DiscoveR
#' @examples
#' \dontrun{
#'  if(interactive()){
#'    init_discover()
#'  }
#'}
init_discover <- function() {
  rm(envir = .GlobalEnv, list = ls(envir = .GlobalEnv))
  Sys.setenv("LANGUAGE"="ES")
  options(encoding = "utf8")
  shiny::runApp(appDir = system.file("application", package = "DiscoveR"), 
                launch.browser = TRUE)
}

