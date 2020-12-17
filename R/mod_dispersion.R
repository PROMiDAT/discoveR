#' dispersion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dispersion_ui <- function(id) {
  ns <- NS(id)
  
  titulo_disp <- tags$div(
    class = "multiple-select-var",
    selectizeInput(
      ns("sel_disp"), NULL, multiple = T, choices = c(""),
      options = list(maxItems = 3))
  )
  
  opc_disp <- tabsOptions(
    heights = c(50, 40), tabs.content = list(
      list(h4(labelInput("opciones")), hr(),
           colourpicker::colourInput(
             ns("col_disp"), labelInput("selcolor"), value = "steelblue", 
             allowTransparent = T)),
      list(col_12(codigo.monokai(ns("fieldCodeDisp"), height = "25vh"))
      )
    )
  )
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxDisp"), opciones = opc_disp, title = titulo_disp,
      tabPanel(
        title = labelInput("dispersion"), value = "tabDisp", 
        withLoader(uiOutput(ns("plot_disp")), 
                   type = "html", loader = "loader4")
      )
    )
  )
}
    
#' dispersion Server Function
#' @keywords internal
mod_dispersion_server <- function(input, output, session, updateData) {
  ns <- session$ns
  
  #' Update on load data
  observeEvent(updateData$datos, {
    datos <- var.numericas(updateData$datos)
    updateSelectInput(session, "sel_disp", choices = colnames(datos))
  })
  
  #' Choose 2D or 3D plot
  output$plot_disp <- renderUI({
    vars  <- input$sel_disp
    
    if(length(vars) == 2) {
      highchartOutput(ns('disp_2D'), height = "70vh")
    } else if(length(vars) == 3) {
      plotlyOutput(ns('disp_3D'), height = "70vh")
    } else {
      highchartOutput(ns('NULL'), height = "70vh")
    }
  })
  
  #' Scatter Plot 2D
  output$disp_2D <- renderHighchart({
    datos <- updateData$datos
    vars  <- input$sel_disp
    color <- input$col_disp
    
    if(length(vars) == 2) {
      cod <- code.disp.2d(vars, color)
      updateAceEditor(session, "fieldCodeDisp", value = cod)
      datos <- data.frame(x = datos[[vars[1]]], y = datos[[vars[2]]])
      
      hchart(datos, "point", hcaes(x = x, y = y), color = color) %>%
        hc_chart(zoomType = "xy") %>% hc_xAxis(title = list(text = vars[1])) %>%
        hc_yAxis(title = list(text = vars[2])) %>% 
        hc_tooltip(
          pointFormat = paste0(vars[1], ": {point.x}<br>", vars[2], ": {point.y}"),
          headerFormat = ''
        ) %>% hc_exporting(enabled = T, filename = "dispersion")
    } else {
      return(NULL)
    }
  })
  
  #' Scatter Plot 3D
  output$disp_3D <- renderPlotly({
    datos <- updateData$datos
    vars  <- input$sel_disp
    color <- input$col_disp
    
    if(length(vars) == 3) {
      cod <- code.disp.3d(vars, color)
      updateAceEditor(session, "fieldCodeDisp", value = cod)
      
      datos <- data.frame(
        x = datos[[vars[1]]], y = datos[[vars[2]]], z = datos[[vars[3]]]
      )
      
      plot_ly(
        datos, x = ~x, y = ~y, z = ~z, type = 'scatter3d', 
        mode = 'markers', marker = list(color = color),
        hovertemplate = paste0(
          vars[1], ": %{x:}<br>", vars[2], ": %{y:}<br>",
          vars[3], ": %{z:}<extra></extra>"
        )) %>% config(displaylogo = F) %>%
        layout(paper_bgcolor = "black", scene = list(
          xaxis = list(title = vars[1], gridcolor = "white"), 
          yaxis = list(title = vars[2], gridcolor = "white"),
          zaxis = list(title = vars[3], gridcolor = "white")))
    } else {
      return(NULL)
    }
  })
}
    
## To be copied in the UI
# mod_dispersion_ui("dispersion_ui_1")
    
## To be copied in the server
# callModule(mod_dispersion_server, "dispersion_ui_1")


