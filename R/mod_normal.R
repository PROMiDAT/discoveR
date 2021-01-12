#' normal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_normal_ui <- function(id) {
  ns <- NS(id)
  
  opc_hist <- tabsOptions(heights = c(70, 30), tabs.content = list(
    list(h4(labelInput("opciones")), hr(),
         conditionalPanel(
           "input.BoxNormal == 'tabNormalPlot'",
           colourpicker::colourInput(
             ns("col_hist_bar"), labelInput("selcolbar"),
             value = "steelblue", allowTransparent = T),
           colourpicker::colourInput(
             ns("col_hist_line"), labelInput("selcolline"),
             value = "#555555", allowTransparent = T)
         ),
         conditionalPanel(
           "input.BoxNormal == 'tabQPlot'",
           colourpicker::colourInput(
             ns("col_qq_point"), labelInput("selcolpoint"),
             value = "steelblue", allowTransparent = T),
           colourpicker::colourInput(
             ns("col_qq_line"), labelInput("selcolline"),
             value = "#555555", allowTransparent = T)
         )),
    list(
      conditionalPanel(
        "input.BoxNormal == 'tabNormalPlot'",
        codigo.monokai(ns("fieldCodeNormal"), height = "10vh")),
      conditionalPanel(
        "input.BoxNormal == 'tabQPlot'",
        codigo.monokai(ns("fieldCodeQplot"), height = "10vh")),
      conditionalPanel(
        "input.BoxNormal == 'tabNormalCalc'",
        codigo.monokai(ns("fieldCalcNormal"), height = "10vh")))
  ))
  
  tagList(
    tabBoxPrmdt(
      id = "BoxNormal", opciones = opc_hist, 
      title = tags$div(
        class = "multiple-select-var",
        selectInput(inputId = ns("sel_normal"), label = NULL, choices =  "")
      ), 
      tabPanel(
        title = labelInput("plotnormal"), value = "tabNormalPlot",
        withLoader(echarts4rOutput(ns('hc_normal'), height = "70vh"), 
                   type = "html", loader = "loader4")),
      tabPanel(
        title = "Qplot + Qline", value = "tabQPlot",
        withLoader(echarts4rOutput(ns('hc_qq'), height = "70vh"), 
                   type = "html", loader = "loader4")),
      tabPanel(
        title = labelInput("normalidad"), value = "tabNormalCalc",
        withLoader(DT::DTOutput(ns('calc_normal')), 
                   type = "html", loader = "loader4"))
    )
  )
}
    
#' normal Server Function
#' @keywords internal
mod_normal_server <- function(input, output, session, updateData) {
  ns <- session$ns
  
  #' Update on load data
  observeEvent(updateData$datos, {
    datos       <- updateData$datos
    numericos   <- var.numericas(datos)
    
    updateSelectInput(session, "sel_normal", choices = colnames(numericos))
  })
  
  #' Grafico Test de normalidad
  output$hc_normal <- renderEcharts4r({
    var       <- input$sel_normal
    datos     <- updateData$datos[, var]
    colorBar  <- input$col_hist_bar
    colorLine <- input$col_hist_line
    nombres   <- c(tr("histograma", updateData$idioma), 
                   tr("curvanormal", updateData$idioma))
    
    tryCatch({
      cod <- paste0("hchistnormal(datos[['", var, "']], 'normal', '", colorBar,
                    "', '", colorLine, "', c('", nombres[1], "', '", 
                    nombres[2], "'))")
      updateAceEditor(session, "fieldCodeNormal", value = cod)
      hchistnormal(datos, "normal", colorBar, colorLine, nombres)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  
  #' Grafico qqplot + qqline
  output$hc_qq <- renderEcharts4r({
    var        <- input$sel_normal
    datos      <- updateData$datos[, var]
    colorPoint <- input$col_qq_point
    colorLine  <- input$col_qq_line
    
    tryCatch({
      cod <- paste0("hcqq(datos[['", var, "']], 'qq', '", colorPoint,
                    "', '", colorLine, "')")
      updateAceEditor(session, "fieldCodeQplot", value = cod)
      hcqq(datos, "qq", colorPoint, colorLine)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  
  #' Resumen Test de normalidad
  output$calc_normal <- DT::renderDT({
    datos <- updateData$datos
    noms  <- c(tr('asimetria', isolate(updateData$idioma)),
               tr('normalidad', isolate(updateData$idioma)))
    
    tryCatch({
      updateAceEditor(session, "fieldCalcNormal", value = "dfnormal(datos)")
      res <- dfnormal(datos)
      
      sketch <- htmltools::withTags(table(
        tags$thead(
          tags$tr(
            tags$th(rowspan = 2, 'Variables'), 
            tags$th(colspan = 1, style = "text-align: center;", 
                    labelInput('asimetria', noms[1])),
            tags$th(colspan = 3, style = "text-align: center;", 
                    labelInput('normalidad', noms[2]))
          ),
          tags$tr(
            tags$th('Fisher'), tags$th('Pearson'), 
            tags$th('Lillie'), tags$th('Cvm')
          )
        )
      ))
      DT::datatable(
        res, selection = 'none', container = sketch,
        options = list(dom = 'frtip', scrollY = "50vh")
      )
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
}

## To be copied in the UI
# mod_normal_ui("normal_ui_1")

## To be copied in the server
# callModule(mod_normal_server, "normal_ui_1")