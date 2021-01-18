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
        ns("col_min"), labelInput("selcolor"), "#FF5733", 
        allowTransparent = T),
      colourpicker::colourInput(
        ns("col_med"), labelInput("selcolor"), "#F8F5F5", 
        allowTransparent = T),
      colourpicker::colourInput(
        ns("col_max"), labelInput("selcolor"), "#2E86C1", 
        allowTransparent = T)
    ),
    list(codigo.monokai(ns("fieldCodeCor"),  height = "30vh"))
  ))
  
  tagList(
    tabBoxPrmdt(
      id = ns("tabCor"), opciones = opts_cor, title = NULL,
      tabPanel(
        title = labelInput("correlacion"), value = "correlacion",
        withLoader(highchartOutput(ns('hc_cor'), height = "70vh"), 
                   type = "html", loader = "loader4")),
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
  output$hc_cor <- renderHighchart({
    input$run_cor
    datos <- var.numericas(updateData$datos)
    col_min <- isolate(input$col_min)
    col_med <- isolate(input$col_med)
    col_max <- isolate(input$col_max)
    colores <- list(list(0, col_min), list(0.5, col_med), list(1, col_max))
    
    tryCatch({
      cod <- code.cor(col_min, col_med, col_max)
      updateAceEditor(session, "fieldCodeCor", value = cod)
      
      label.js <- JS(
        "function() {return Highcharts.numberFormat(this.point.value, 2);}"
      )
      hchart(cor(datos)) %>% hc_chart(zoomType = "xy") %>%
        hc_exporting(enabled = T, filename = "correlaciones") %>%
        hc_colorAxis(stops = NULL) %>%
        hc_colorAxis(stops = colores, min = -1, max = 1) %>%
        hc_plotOptions(
          series = list(
            dataLabels = list(enabled = TRUE, formatter = label.js)
          )
        )
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
 
