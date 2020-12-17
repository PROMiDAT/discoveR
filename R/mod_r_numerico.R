#' r_numerico UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_r_numerico_ui <- function(id) {
  ns <- NS(id)
  tagList(
    col_7(box(
      title = labelInput("resumen"), status = "primary", width = 12,
      solidHeader = TRUE, collapsible = TRUE,
      DT::dataTableOutput(ns("resumen.completo")), hr(),
      codigo.monokai(ns("fieldCodeResum"), height = "8vh"))
    ),
    col_5(box(
      title = labelInput("resumenvar"), status = "primary",
      width = 12, solidHeader = TRUE, collapsible = TRUE,
      selectInput(inputId = ns("sel.resumen"), label = labelInput("selvar"),
                  choices =  ""), fluidRow(uiOutput(ns("resumen"))))
    )
  )
}
    
#' r_numerico Server Function
#' @keywords internal
mod_r_numerico_server <- function(input, output, session, updateData) {
  ns <- session$ns
  
  #' Update on load data
  observeEvent(updateData$datos, {
    datos <- updateData$datos
    updateSelectInput(session, "sel.resumen", choices = colnames(datos))
  })
  
  #' Resumen numérico
  output$resumen.completo <- DT::renderDataTable({
    datos  <- updateData$datos
    updateAceEditor(session, "fieldCodeResum", value = "summary(datos)")
    data.frame(
      unclass(summary(datos)), check.names = F, stringsAsFactors = F)
  }, options = list(dom = 'ft', scrollX = T), rownames = F)
  
  output$resumen = renderUI({
    datos <- isolate(updateData$datos)
    idioma <- isolate(updateData$idioma)
    if(input$sel.resumen %in% colnames(var.numericas(datos))) {
      resumen.numerico(datos, input$sel.resumen, idioma)
    } else {
      resumen.categorico(datos, input$sel.resumen)
    }
  })
}
    
## To be copied in the UI
# mod_r_numerico_ui("r_numerico_ui_1")
    
## To be copied in the server
# callModule(mod_r_numerico_server, "r_numerico_ui_1")
 
