#' The application server-side
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#' DO NOT REMOVE.
#' @import shiny
#' @import shinycustomloader
#' @import plotly
#' @importFrom DT tableHeader formatStyle
#' @keywords internal
#' 
app_server <- function( input, output, session ) {
  
  ##################################  Options  ################################
  options(shiny.maxRequestSize = 200*1024^2)
  options(
    DT.options = list(
      aLengthMenu = c(10, 30, 50), iDisplayLength = 10,
      scrollX = TRUE, language = list(
        search = shiny::HTML('<i class="fa fa-search"></i>'),
        info = "", emptyTable = "", zeroRecords = "",
        paginate = list(
          "previous" = shiny::HTML('<i class="fa fa-backward"></i>'),
          "next"     = shiny::HTML('<i class="fa fa-forward"></i>'),
          "first"    = shiny::HTML('<i class="fa fa-fast-backward"></i>'), 
          "last"     = shiny::HTML('<i class="fa fa-fast-forward"></i>')))
    )
  )
  
  onStop(function() stopApp())
  
  ##################################  Variables  ##############################
  updateData <- rv(datos = NULL, originales = NULL, idioma = NULL)
  
  
  ###################################  Update  ################################
  #' Update on Language
  observeEvent(input$idioma, {
    updateData$idioma = input$idioma
    updateLabelInput(session, cambiar.labels(), tr(cambiar.labels(), input$idioma))
  })
  
  #' Enable/disable on load data
  observe({
    element <- "#sidebarItemExpanded li"
    menu.values <- c(
      "[class^=treeview]",  " a[data-value=acp]", " a[data-value=cj]",
      " a[data-value=kmedias]", " a[data-value=reporte]")

    lapply(menu.values, function(i) {
      if(is.null(updateData$datos) || ncol(updateData$datos) < 1) {
        addClass(class = "disabled", selector = paste0(element, i))
      } else {
        removeClass(class = "disabled", selector = paste0(element, i))
      }
    })
  })
  
  ###################################  Modules  ###############################
  callModule(mod_carga_datos_server,    "carga_datos_ui_1",    updateData)
  callModule(mod_r_numerico_server,     "r_numerico_ui_1",     updateData)
  callModule(mod_normal_server,         "normal_ui_1",         updateData)
  callModule(mod_dispersion_server,     "dispersion_ui_1",     updateData)
  callModule(mod_distribuciones_server, "distribuciones_ui_1", updateData)
  callModule(mod_correlacion_server,    "correlacion_ui_1",    updateData)
  callModule(mod_acp_server,            "acp_ui_1",            updateData)
  callModule(mod_cj_server,             "cj_ui_1",             updateData)
  callModule(mod_kmedias_server,        "kmedias_ui_1",        updateData)
}
