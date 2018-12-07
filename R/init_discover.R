
#' Start discoveR
#' @title This function will start discoveR
#' @return Nothing
#' @description An interactive Shiny application for exploring data.
#' @details This starts the discoveR application on the user's local computer.
#' @keywords discoveR
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
  shiny::runApp(appDir = system.file("application", package = "discoveR"), 
                launch.browser = TRUE)
}

