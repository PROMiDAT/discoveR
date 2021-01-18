#' distribuciones UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_distribuciones_ui <- function(id){
  ns <- NS(id)
  
  titulo_dist <- tags$div(
    class = "multiple-select-var", conditionalPanel(
      condition = "input.tabDyA == 'numericas'",
      selectInput(inputId = ns("sel_dya_num"), label = NULL, choices = "")),
    conditionalPanel(
      condition = "input.tabDyA == 'categoricas'",
      selectInput(inputId = ns("sel_dya_cat"), label = NULL, choices = ""))
  )
  
  opc_dist <- tabsOptions(
    botones = list(icon("gear"), icon("info"), icon("code")),
    widths = c(50, 100, 100), heights = c(50, 50, 35),
    tabs.content = list(
      list(
        options.run(ns("run_dist")), tags$hr(style = "margin-top: 0px;"),
        conditionalPanel(
          condition = "input.tabDyA == 'numericas'",
          colourpicker::colourInput(
            ns("col_dist_bar"), labelInput("selcolbar"), value = "steelblue", 
            allowTransparent = T),
          colourpicker::colourInput(
            ns("col_dist_point"), labelInput("selcolline"), value = "red",
            allowTransparent = T)
        )
      ),
      list(DT::dataTableOutput(ns("mostrar.atipicos"))),
      list(
        conditionalPanel(
          condition = "input.tabDyA == 'numericas'",
          codigo.monokai(ns("fieldCodeNum"), height = "10vh")),
        conditionalPanel(
          condition = "input.tabDyA == 'categoricas'",
          codigo.monokai(ns("fieldCodeCat"), height = "20vh"))
      )
    )
  )
  
  tagList(
    tabBoxPrmdt(
      id = "tabDyA", title = titulo_dist, opciones = opc_dist,
      tabPanel(
        title = labelInput("numericas"), value = "numericas",
        withLoader(highchartOutput(ns('hc_num'), height = "75vh"), 
                   type = "html", loader = "loader4")),
      tabPanel(
        title = labelInput("categoricas"), value = "categoricas",
        withLoader(highchartOutput(ns('hc_cat'), height = "75vh"), 
                   type = "html", loader = "loader4"))
    )
  )
}
    
#' distribuciones Server Function
#' @keywords internal
mod_distribuciones_server <- function(input, output, session, updateData){
  ns <- session$ns
  
  #' Update on load data
  observeEvent(updateData$datos, {
    datos       <- updateData$datos
    numericos   <- var.numericas(datos)
    categoricos <- var.categoricas(datos)
    
    updateSelectInput(session, "sel_dya_num", choices = colnames(numericos))
    updateSelectInput(session, "sel_dya_cat", choices = colnames(categoricos))
  })
  
  #' Gráfico de Distribuciones (Númericas)
  output$hc_num = renderHighchart({
    input$run_dist
    datos      <- updateData$datos
    var        <- input$sel_dya_num
    colorBar   <- isolate(input$col_dist_bar)
    colorPoint <- isolate(input$col_dist_point)
    titulos <- c(
      tr("minimo", updateData$idioma),
      tr("q1", updateData$idioma),
      tr("mediana", updateData$idioma),
      tr("q3", updateData$idioma),
      tr("maximo", updateData$idioma)
    )
    
    tryCatch({
      cod <- paste0("hchistboxplot(datos[['", var, "']], 'distribucion_num', '", 
                    colorBar, "', '", colorPoint, "', '", var, "', c('", 
                    paste(titulos, collapse = "', '"), "'))\n")
      updateAceEditor(session, "fieldCodeNum", value = cod)
      hchistboxplot(datos[[var]], "distribucion_num", colorBar, 
                    colorPoint, var, titulos)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  
  output$mostrar.atipicos = DT::renderDataTable({
    datos <- updateData$datos
    var   <- input$sel_dya_num
    atipicos <- boxplot.stats(datos[, var])
    datos <- datos[datos[, var] %in% atipicos$out, var, drop = F]
    datos <- datos[order(datos[, var]), , drop = F]
    DT::datatable(datos, options = list(
      dom = 't', scrollX = TRUE, scrollY = "28vh", pageLength = nrow(datos))) %>%
      formatStyle(1, color = "white", backgroundColor = "#CBB051", target = "row")
  })
  
  #' Gráfico de Distribuciones (Categóricas)
  output$hc_cat = renderHighchart({
    var  <- input$sel_dya_cat
    validate(need(var != "", tr("errorcat", isolate(updateData$idioma))))
    
    tryCatch({
      data <- updateData$datos[, var]
      
      cod <- code.dist.cat(var)
      updateAceEditor(session, "fieldCodeCat", value = cod)
      
      datos <- data.frame (
        label = levels(data), 
        value = summary(data, maxsum = length(levels(data)))
      )
      hchart(datos, hcaes(x = label, y = value, color = label), type = "column") %>%
        hc_xAxis(title = list(text = "")) %>% hc_yAxis(title = list(text = "")) %>%
        hc_tooltip(pointFormat = "<b>{point.name}:</b> {point.y}",
                   headerFormat = "") %>%
        hc_exporting(enabled = T, filename = "distribucion_cat")
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
}
    
## To be copied in the UI
# mod_distribuciones_ui("distribuciones_ui_1")
    
## To be copied in the server
# callModule(mod_distribuciones_server, "distribuciones_ui_1")
 
