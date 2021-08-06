#' correlacion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_correlacion_ui <- function(id){
  ns <- NS(id)
  
  opts_cor <- tabsOptions(heights = c(70, 50), tabs.content = list(
    list(
      options.run(ns("run_cor")), tags$hr(style = "margin-top: 0px;"),
      colourpicker::colourInput(
        ns("col_max"), labelInput("selcolor"), "#2E86C1", 
        allowTransparent = T),
      colourpicker::colourInput(
        ns("col_med"), labelInput("selcolor"), "#F8F5F5", 
        allowTransparent = T),
      colourpicker::colourInput(
        ns("col_min"), labelInput("selcolor"), "#FF5733", 
        allowTransparent = T)
      
    ),
    list(codigo.monokai(ns("fieldCodeCor"),  height = "30vh"))
  ))
  
  tagList(
    tabBoxPrmdt(
      id = ns("tabCor"), opciones = opts_cor, title = NULL,
      tabPanel(
        title = labelInput("correlacion"), value = "correlacion",
        echarts4rOutput(ns('plot_cor'), height = "70vh")),
      tabPanel(
        title = labelInput("resultados"), value = "cor.salida",
        div(style = "height: 75vh;overflow-y: scroll;",
            withLoader(verbatimTextOutput(ns("txt_cor")), 
                       type = "html", loader = "loader4")))
    )
  )
}
    
#' correlacion Server Function
#' @keywords internal
mod_correlacion_server <- function(input, output, session, updateData) {
  ns <- session$ns
  
  #' Gráfico de Correlaciones
  output$plot_cor <- renderEcharts4r({
    input$run_cor
    datos <- var.numericas(updateData$datos)
    colores <- list(isolate(input$col_min), isolate(input$col_med), isolate(input$col_max))
    
    tryCatch({
      cod <- code.cor(colores)
      updateAceEditor(session, "fieldCodeCor", value = cod)
      
      datos.plot <- round(cor(datos), 3)
      e_cor(datos.plot, colores)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  
  #' Resultados numéricos de Correlaciones
  output$txt_cor <- renderPrint(print(cor(var.numericas(updateData$datos))))
}
    
## To be copied in the UI
# mod_correlacion_ui("correlacion_ui_1")
    
## To be copied in the server
# callModule(mod_correlacion_server, "correlacion_ui_1")
 
