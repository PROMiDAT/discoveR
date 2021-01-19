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
    list(options.run(ns("run_normal")), tags$hr(style = "margin-top: 0px;"),
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
         ),
         conditionalPanel(
           "input.BoxNormal == 'tabNormalCalc'",
           sliderInput(ns("slide_inter"), labelInput("alfa"), 
                       min = 0, max = 0.2, step = 0.01, value = 0.05)
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
        withLoader(highchartOutput(ns('hc_normal'), height = "70vh"), 
                   type = "html", loader = "loader4")),
      tabPanel(
        title = "Qplot + Qline", value = "tabQPlot",
        withLoader(highchartOutput(ns('hc_qq'), height = "70vh"), 
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
  output$hc_normal <- renderHighchart({
    input$run_normal
    var       <- input$sel_normal
    datos     <- updateData$datos[, var]
    colorBar  <- isolate(input$col_hist_bar)
    colorLine <- isolate(input$col_hist_line)
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
  output$hc_qq <- renderHighchart({
    input$run_normal
    var        <- input$sel_normal
    datos      <- updateData$datos[, var]
    colorPoint <- isolate(input$col_qq_point)
    colorLine  <- isolate(input$col_qq_line)
    
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
    input$run_normal
    datos <- updateData$datos
    alfa <- isolate(as.numeric(input$slide_inter))
    noms  <- c(tr('asimetria', isolate(updateData$idioma)),
               tr('normalidad', isolate(updateData$idioma)),
               tr('sigue', isolate(updateData$idioma)),
               tr('pvalue', isolate(updateData$idioma)),
               tr('tasim', isolate(updateData$idioma)))
    
    tryCatch({
      updateAceEditor(session, "fieldCalcNormal", value = "dfnormal(datos)")
      res <- dfnormal(datos)
      
      res <- res[, c(1, 5)]
      res <- round(res, 3)
      res$asimetria <- res$fisher > 0
      res$asimetria <- ifelse(res$asimetria, '<i class="fa fa-plus" style="color: green;"></i>', 
                              '<i class="fa fa-minus" style="color: red;"></i>')
      res$normal <- res$shapiro > alfa
      res$normal <- ifelse(res$normal, '<i class="fa fa-check" style="color: green;"></i>', 
                           '<i class="fa fa-times" style="color: red;"></i>')
      res$shapiro <- paste0(res$shapiro, " > ", alfa)
      res <- res[, c(1, 3, 2, 4)]
      
      sketch <- htmltools::withTags(table(
        tags$thead(
          tags$tr(
            tags$th(rowspan = 2, 'Variables'), 
            tags$th(colspan = 2, style = "text-align: center;", 
                    labelInput('asimetria', noms[1])),
            tags$th(colspan = 2, style = "text-align: center;", 
                    labelInput('normalidad', noms[2]))
          ),
          tags$tr(
            tags$th(labelInput('tasim', noms[5])), tags$th(labelInput('asimetria', noms[1])),
            tags$th(labelInput('pvalue', noms[4])), tags$th(labelInput('sigue', noms[3]))
          )
        )
      ))
      DT::datatable(
        res, selection = 'none', container = sketch, escape = F,
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