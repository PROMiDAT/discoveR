#' acp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_acp_ui <- function(id) {
  ns <- NS(id)
  
  title_acp <- tags$div(
    class = "multiple-select-var", style = "width: 150px;",
    conditionalPanel(
      condition = paste0(
        "input.tabPCA == 'tabInd' || ", "input.tabPCA == 'tabVar' || ", 
        "input.tabPCA == 'tabBi'"),
      radioSwitch(ns("plotMode"), NULL, list("2D", "3D")))
  )
  
  opts_acp <- tabsOptions(
    widths = c(100, 100), heights = c(60, 40),
    tabs.content = list(
      list(
        options.run(ns("run_pca")), tags$hr(style = "margin-top: 0px;"),
        radioSwitch(ns("switch_scale"), NULL, c("centrar", "nocentrar")),
        sliderInput(ns("slider_ejes"), labelInput("selejes"), 1, 10, c(1, 2)),
        conditionalPanel(
          condition = paste0("input.tabPCA == 'tabInd' ||",
                             " input.tabPCA == 'tabBi'"),
          fluidRow(
            style = "margin-left: 0px; margin-right: 0px",
            col_6(sliderInput(ns("ind_cos"), labelInput("cosind"), 0, 100, 0)),
            col_3(
              colourpicker::colourInput(
                ns("col_pca_ind"), labelInput("colindbien"), "steelblue", 
                allowTransparent = T)
            ),
            col_3(
              colourpicker::colourInput(
                ns("col_ind_cos"), labelInput("colindmal"), "firebrick", 
                allowTransparent = T)
            )
          )
        ),
        conditionalPanel(
          condition = paste0(
            "input.tabPCA == 'tabVar' || ", "input.tabPCA == 'tabBi'"),
          fluidRow(
            style = "margin-left: 0px; margin-right: 0px",
            col_6(sliderInput(ns("var_cos"), labelInput("cosvar"), 0, 100, 0)),
            col_3(
              colourpicker::colourInput(
                ns("col_pca_var"), labelInput("colvarbien"), "forestgreen", 
                allowTransparent = T)
            ),
            col_3(
              colourpicker::colourInput(
                ns("col_var_cos"), labelInput("colvarmal"), "darkorchid", 
                allowTransparent = T)
            )
          )
        )
      ),
      list(
        codigo.monokai(ns("fieldCodePCAModelo"), height = "10vh"),
        lapply(c("Ind", "Var", "Bi"), function(i) {
          conditionalPanel(
              condition = paste0("input.tabPCA == 'tab", i, "'"),
              codigo.monokai(ns(paste0("fieldCode", i, "2D")), height = "10vh"),
              codigo.monokai(ns(paste0("fieldCode", i, "3D")), height = "10vh")
          )
        }),
        lapply(c('VEE', 'CCI', 'CCV', 'CVC', 'PC'), function(i) {
          conditionalPanel(
            condition = paste0("input.tabPCA == 'tab", i, "'"),
            codigo.monokai(ns(paste0("fieldCode", i)), height = "15vh"))
        })
      )
    )
  )
  
  tagList(
    tabBoxPrmdt(
      id = "tabPCA", title = title_acp, opciones = opts_acp,
      tabPanel(
        title = labelInput("individuos"), value = "tabInd",
        tags$div(
          id = ns("div_ind_2D"),
          withLoader(highchartOutput(ns("ind_2D"), height = "75vh"), 
                     type = "html", loader = "loader4")),
        tags$div(
          id = ns("div_ind_3D"),
          withLoader(plotlyOutput(ns("ind_3D"), height = "75vh"), 
                     type = "html", loader = "loader4"))
      ),
      tabPanel(
        title = labelInput("variables"), value = "tabVar",
        tags$div(
          id = ns("div_var_2D"),
          withLoader(highchartOutput(ns("var_2D"), height = "75vh"), 
                     type = "html", loader = "loader4")),
        tags$div(
          id = ns("div_var_3D"),
          withLoader(plotlyOutput(ns("var_3D"), height = "75vh"), 
                     type = "html", loader = "loader4"))
      ),
      tabPanel(
        title = labelInput("sobreposicion"), value = "tabBi",
        tags$div(
          id = ns("div_bi_2D"),
          withLoader(highchartOutput(ns("bi_2D"), height = "75vh"), 
                     type = "html", loader = "loader4")),
        tags$div(
          id = ns("div_bi_3D"),
          withLoader(plotlyOutput(ns("bi_3D"), height = "75vh"), 
                     type = "html", loader = "loader4"))
      ),
      navbarMenu(
        labelInput("ayudacp"),
        tabPanel(labelInput("vee"), value = "tabVEE", 
                 highchartOutput(ns("plotVEE"), height = "75vh")),
        tabPanel(labelInput("cci"), value = "tabCCI",
                 highchartOutput(ns("plotCCI"), height = "75vh")),
        tabPanel(labelInput("ccv"), value = "tabCCV",
                 highchartOutput(ns("plotCCV"), height = "75vh")),
        tabPanel(labelInput("cvc"), value = "tabCVC",
                 highchartOutput(ns("plotCVC"), height = "75vh")),
        tabPanel(labelInput("cp"), value = "tabPC",
                 highchartOutput(ns("plotPC"), height = "75vh"))
      ),
      tabPanel(
        title = labelInput("resultados"), value = "pca.salida",
        div(style = "height: 75vh;overflow-y: scroll;", 
            withLoader(verbatimTextOutput(ns("txtpca")), 
                       type = "html", loader = "loader4")))
    )
  )
}

#' acp Server Function
#' @keywords internal
mod_acp_server <- function(input, output, session, updateData) {
  ns <- session$ns
  
  #' Generate PCA on load data
  modelo.pca <- reactive({
    input$run_pca
    
    datos       <- var.numericas(updateData$datos)
    centrado    <- isolate(input$switch_scale)
    dimensiones <- 10
    if(ncol(datos) < 10) {
      dimensiones <- ncol(datos)
    }
    
    updateSliderInput(session, inputId = "slider_ejes", max = dimensiones)
    
    if(nrow(datos) == 0) {
      return(NULL)
    } else {
      cod <- paste0("modelo.pca <- PCA(var.numericas(datos), scale.unit = ",
                    centrado, ", ncp = ", dimensiones, ", graph = F)")
      updateAceEditor(session, "fieldCodePCAModelo", value = cod)
      return(PCA(datos, scale.unit = centrado, ncp = dimensiones, graph = F))
    }
  })
  
  #' Choose 2D or 3D plot
  observeEvent(input$plotMode, {
    if(input$plotMode) {
      shinyjs::show("fieldCodeInd2D")
      shinyjs::hide("fieldCodeInd3D")
      shinyjs::show("fieldCodeVar2D")
      shinyjs::hide("fieldCodeVar3D")
      shinyjs::show("fieldCodeBi2D")
      shinyjs::hide("fieldCodeBi3D")
      shinyjs::show("div_ind_2D")
      shinyjs::hide("div_ind_3D")
      shinyjs::show("div_var_2D")
      shinyjs::hide("div_var_3D")
      shinyjs::show("div_bi_2D")
      shinyjs::hide("div_bi_3D")
    } else {
      shinyjs::hide("fieldCodeInd2D")
      shinyjs::show("fieldCodeInd3D")
      shinyjs::hide("fieldCodeVar2D")
      shinyjs::show("fieldCodeVar3D")
      shinyjs::hide("fieldCodeBi2D")
      shinyjs::show("fieldCodeBi3D")
      shinyjs::hide("div_ind_2D")
      shinyjs::show("div_ind_3D")
      shinyjs::hide("div_var_2D")
      shinyjs::show("div_var_3D")
      shinyjs::hide("div_bi_2D")
      shinyjs::show("div_bi_3D")
    }
  })
  
  #' Plot PCA 2D (individuals)
  output$ind_2D <- renderHighchart({
    modelo <- modelo.pca()
    
    ejes    <- isolate(input$slider_ejes)
    ind.cos <- isolate(input$ind_cos) * 0.01
    ind.col <- isolate(input$col_pca_ind)
    cos.col <- isolate(input$col_ind_cos)
    titulos <- c(tr("bienr", updateData$idioma), tr("malr", updateData$idioma))
    
    if(is.null(modelo)) {
      return(NULL)
    } else {
      cod <- paste0("hcpcaind(modelo.pca, c(", paste(ejes, collapse = ", "), 
                    "), 'acp_ind', '", ind.col, "', ", ind.cos, ", '", 
                    cos.col, "', c('", titulos[1], "', '", titulos[2], "'))")
      updateAceEditor(session, "fieldCodeInd2D", value = cod)
      hcpcaind(modelo, ejes, "acp_ind", ind.col, ind.cos, cos.col, titulos)
    }
  })
  
  #' Plot PCA 3D (individuals)
  output$ind_3D <- renderPlotly({
    modelo <- modelo.pca()
    
    ejes    <- isolate(input$slider_ejes)
    ind.cos <- isolate(input$ind_cos) * 0.01
    ind.col <- isolate(input$col_pca_ind)
    cos.col <- isolate(input$col_ind_cos)
    titulos <- c(tr("bienr", updateData$idioma), tr("malr", updateData$idioma))
    
    if(is.null(modelo)) {
      return(NULL)
    } else {
      cod <- paste0("plotly_pcaind(modelo.pca, c(", paste(ejes, collapse = ", "), 
                    "), '", ind.col, "', ", ind.cos, ", '", cos.col, 
                    "', c('", titulos[1], "', '", titulos[2], "'))")
      updateAceEditor(session, "fieldCodeInd3D", value = cod)
      plotly_pcaind(modelo, c(1, 2, 3), ind.col, ind.cos, cos.col, titulos)
    }
  })
  
  #' Plot PCA 2D (variables)
  output$var_2D <- renderHighchart({
    modelo  <- modelo.pca()
    
    ejes    <- isolate(input$slider_ejes)
    var.cos <- isolate(input$var_cos) * 0.01
    var.col <- isolate(input$col_pca_var)
    cos.col <- isolate(input$col_var_cos)
    titulos <- c(tr("bienr", updateData$idioma), tr("malr", updateData$idioma))
    
    if(is.null(modelo)) {
      return(NULL)
    } else {
      cod <- paste0("hcpcavar(modelo.pca, c(", paste(ejes, collapse = ", "), 
                    "), 'acp_var', '", var.col, "', ", var.cos, ", '", 
                    cos.col, "', c('", titulos[1], "', '", titulos[2], "'))")
      updateAceEditor(session, "fieldCodeVar2D", value = cod)
      hcpcavar(modelo, ejes, "acp_var", var.col, var.cos, cos.col, titulos)
    }
  })
  
  #' Plot PCA 3D (variables)
  output$var_3D <- renderPlotly({
    modelo  <- modelo.pca()
    
    ejes    <- isolate(input$slider_ejes)
    var.cos <- isolate(input$var_cos) * 0.01
    var.col <- isolate(input$col_pca_var)
    cos.col <- isolate(input$col_var_cos)
    titulos <- c(tr("bienr", updateData$idioma), tr("malr", updateData$idioma))
    
    if(is.null(modelo)) {
      return(NULL)
    } else {
      cod <- paste0("plotly_pcavar(modelo.pca, c(", paste(ejes, collapse = ", "), 
                    "), '", var.col, "', ", var.cos, ", '", cos.col, 
                    "', c('", titulos[1], "', '", titulos[2], "'))")
      updateAceEditor(session, "fieldCodeVar3D", value = cod)
      plotly_pcavar(modelo, c(1, 2, 3), var.col, var.cos, cos.col, titulos)
    }
  })
  
  #' Plot PCA 2D (Biplot)
  output$bi_2D <- renderHighchart({
    modelo  <- modelo.pca()
    
    ejes        <- isolate(input$slider_ejes)
    ind.cos     <- isolate(input$ind_cos) * 0.01
    ind.col     <- isolate(input$col_pca_ind)
    ind.cos.col <- isolate(input$col_ind_cos)
    var.cos     <- isolate(input$var_cos) * 0.01
    var.col     <- isolate(input$col_pca_var)
    var.cos.col <- isolate(input$col_var_cos)
    titulos <- c(tr("bienr", updateData$idioma), tr("malr", updateData$idioma))
    
    if(is.null(modelo)) {
      return(NULL)
    } else {
      cod <- paste0("hcpcabi(modelo.pca, c(", paste(ejes, collapse = ", "), 
                    "), 'acp_bi', '", ind.col, "', '", var.col, "', ", 
                    ind.cos, ", ", var.cos, ", '", ind.cos.col, "', '", 
                    var.cos.col, "', c('", titulos[1], "', '", titulos[2], 
                    "'))")
      updateAceEditor(session, "fieldCodeBi2D", value = cod)
      hcpcabi(modelo, ejes, "acp_bi", ind.col, var.col, ind.cos, 
              var.cos, ind.cos.col, var.cos.col, titulos)
    }
  })
  
  #' Plot PCA 3D (Biplot)
  output$bi_3D <- renderPlotly({
    modelo  <- modelo.pca()
    
    ejes        <- isolate(input$slider_ejes)
    ind.cos     <- isolate(input$ind_cos) * 0.01
    ind.col     <- isolate(input$col_pca_ind)
    ind.cos.col <- isolate(input$col_ind_cos)
    var.cos     <- isolate(input$var_cos) * 0.01
    var.col     <- isolate(input$col_pca_var)
    var.cos.col <- isolate(input$col_var_cos)
    titulos <- c(tr("bienr", updateData$idioma), tr("malr", updateData$idioma))
    
    if(is.null(modelo)) {
      return(NULL)
    } else {
      cod <- paste0("plotly_pcabi(modelo.pca, c(", paste(ejes, collapse = ", "), 
                    "), '", ind.col, "', '", var.col, "', ", ind.cos, ", ", 
                    var.cos, ", '", ind.cos.col, "', '", var.cos.col, 
                    "', c('", titulos[1], "', '", titulos[2], "'))")
      updateAceEditor(session, "fieldCodeBi3D", value = cod)
      plotly_pcabi(modelo, c(1, 2, 3), ind.col, var.col, ind.cos, 
                   var.cos, ind.cos.col, var.cos.col, titulos)
    }
  })
  
  #' Varianza Explicada por cada Eje
  output$plotVEE <- renderHighchart({
    modelo <- modelo.pca()
    
    datos <- data.frame (x = row.names(modelo$eig), y = modelo$eig[, 2])
    if(nrow(datos) > 10) {
      datos <- datos[1:10, ]
    }
    
    cod <- paste0(
    'datos <- data.frame (x = row.names(modelo.pca$eig), y = modelo.pca$eig[, 2])\n\n',
    'hchart(datos, hcaes(x = x, y = y), type = "column") %>%\n',
    '  hc_xAxis(title = list(text = "")) %>% hc_yAxis(title = list(text = "")) %>%\n',
    '  hc_tooltip(pointFormat = "<b>{point.name}:</b> {point.y:.3f}",\n',
    '             headerFormat = "") %>%\n',
    '  hc_exporting(enabled = T, filename = "plot_vee")\n')
    updateAceEditor(session, "fieldCodeVEE", value = cod)
    
    hchart(datos, hcaes(x = x, y = y), type = "column") %>%
      hc_xAxis(title = list(text = "")) %>% hc_yAxis(title = list(text = "")) %>%
      hc_tooltip(pointFormat = "<b>{point.name}:</b> {point.y:.3f}",
                 headerFormat = "") %>%
      hc_exporting(enabled = T, filename = "plot_vee")
  })
  
  #' Cosenos Cuadrados de los Individuos
  output$plotCCI <- renderHighchart({
    modelo <- modelo.pca()
    ejes <- isolate(input$slider_ejes)
    
    datos <- data.frame (x = row.names(modelo$ind$cos2), 
                         y = apply(modelo$ind$cos2[, ejes], 1, sum))
    datos <- datos[order(datos$y, decreasing = T), ]
    if(nrow(datos) > 20) {
      datos <- datos[1:20, ]
    }
    
    cod <- paste0(
      'datos <- data.frame (x = row.names(modelo.pca$ind$cos2),\n', 
      '                     y = apply(modelo.pca$ind$cos2[, c(', paste(ejes, collapse = ","), ')], 1, sum))\n\n',
      'hchart(datos, hcaes(x = x, y = y), type = "column") %>%\n',
      '  hc_xAxis(title = list(text = "")) %>% hc_yAxis(title = list(text = "")) %>%\n',
      '  hc_tooltip(pointFormat = "<b>{point.name}:</b> {point.y:.3f}",\n',
      '             headerFormat = "") %>%\n',
      '  hc_exporting(enabled = T, filename = "plot_cci")\n')
    updateAceEditor(session, "fieldCodeCCI", value = cod)
    
    hchart(datos, hcaes(x = x, y = y), type = "column") %>%
      hc_xAxis(title = list(text = "")) %>% hc_yAxis(title = list(text = "")) %>%
      hc_tooltip(pointFormat = "<b>{point.name}:</b> {point.y:.3f}",
                 headerFormat = "") %>%
      hc_exporting(enabled = T, filename = "plot_cci")
  })
  
  #' Cosenos Cuadrados de las Variables
  output$plotCCV <- renderHighchart({
    modelo <- modelo.pca()
    ejes <- isolate(input$slider_ejes)
    
    datos <- data.frame (x = row.names(modelo$var$cos2), 
                         y = apply(modelo$var$cos2[, ejes], 1, sum))
    datos <- datos[order(datos$y, decreasing = T), ]
    if(nrow(datos) > 20) {
      datos <- datos[1:20, ]
    }
    
    cod <- paste0(
      'datos <- data.frame (x = row.names(modelo.pca$var$cos2),\n', 
      '                     y = apply(modelo.pca$var$cos2[, c(', paste(ejes, collapse = ","), ')], 1, sum))\n\n',
      'hchart(datos, hcaes(x = x, y = y), type = "column") %>%\n',
      '  hc_xAxis(title = list(text = "")) %>% hc_yAxis(title = list(text = "")) %>%\n',
      '  hc_tooltip(pointFormat = "<b>{point.name}:</b> {point.y:.3f}",\n',
      '             headerFormat = "") %>%\n',
      '  hc_exporting(enabled = T, filename = "plot_ccv")\n')
    updateAceEditor(session, "fieldCodeCCV", value = cod)
    
    hchart(datos, hcaes(x = x, y = y), type = "column") %>%
      hc_xAxis(title = list(text = "")) %>% hc_yAxis(title = list(text = "")) %>%
      hc_tooltip(pointFormat = "<b>{point.name}:</b> {point.y:.3f}",
                 headerFormat = "") %>%
      hc_exporting(enabled = T, filename = "plot_ccv")
  })
  
  #' Correlación Variables con los Componentes
  output$plotCVC <- renderHighchart({
    modelo <- modelo.pca()
    
    cod <- paste0(
      'label.js <- JS(\n', 
      '  "function() {return Highcharts.numberFormat(this.point.value, 2);}"\n',
      ')\n\n',
      'hchart(modelo.pca$var$cor) %>% hc_chart(zoomType = "xy") %>%\n',
      '  hc_exporting(enabled = T, filename = "plot_cvc") %>%\n',
      '  hc_plotOptions(\n',
      '    series = list(\n',
      '      dataLabels = list(enabled = TRUE, formatter = label.js)\n',
      '    )\n  )\n')
    updateAceEditor(session, "fieldCodeCVC", value = cod)
    
    label.js <- JS(
      "function() {return Highcharts.numberFormat(this.point.value, 2);}"
    )
    hchart(modelo$var$cor) %>% hc_chart(zoomType = "xy") %>%
      hc_exporting(enabled = T, filename = "plot_cvc") %>%
      hc_plotOptions(
        series = list(
          dataLabels = list(enabled = TRUE, formatter = label.js)
        )
      )
  })
  
  #' Contribución de las variables
  output$plotPC <- renderHighchart({
    modelo <- modelo.pca()
    
    ejes <- isolate(input$slider_ejes)
    
    datos <- data.frame (x = row.names(modelo$var$contrib), 
                         y = modelo$var$contrib[, ejes[1]],
                         z = modelo$var$contrib[, ejes[2]])
    
    if(nrow(datos) > 20) {
      datos <- datos[1:20, ]
    }
    
    cod <- paste0(
      'datos <- data.frame (x = row.names(modelo.pca$var$contrib),\n', 
      '                     y = modelo.pca$var$contrib[, ', ejes[1], '],\n',
      '                     z = modelo.pca$var$contrib[, ', ejes[2], '])\n\n',
      'highchart() %>%\n',
      '  hc_add_series(datos, hcaes(x = x, y = y), type = "column",\n',
      '                name = paste0("Comp ", ', ejes[1], ')) %>%\n',
      '  hc_add_series(datos, hcaes(x = x, y = z), type = "column",\n',
      '                name = paste0("Comp ", ', ejes[2], ')) %>%\n',
      '  hc_xAxis(categories = datos$x) %>%\n',
      '  hc_tooltip(pointFormat = "<b>{point.name}:</b> {point.y:.3f}",\n',
      '             headerFormat = "") %>%\n',
      '  hc_exporting(enabled = T, filename = "plot_pc")\n')
    updateAceEditor(session, "fieldCodePC", value = cod)
    
    highchart() %>%
      hc_add_series(datos, hcaes(x = x, y = y), type = "column",
                    name = paste0("Comp ", ejes[1])) %>%
      hc_add_series(datos, hcaes(x = x, y = z), type = "column",
                    name = paste0("Comp ", ejes[2])) %>%
      hc_xAxis(categories = datos$x) %>%
      hc_tooltip(pointFormat = "<b>{point.name}:</b> {point.y:.3f}",
                 headerFormat = "") %>%
      hc_exporting(enabled = T, filename = "plot_pc")
  })
  
  #' Modelo ACP (Resultados numéricos)
  output$txtpca <- renderPrint(print(modelo.pca()))
}
    
## To be copied in the UI
# mod_acp_ui("acp_ui_1")
    
## To be copied in the server
# callModule(mod_acp_server, "acp_ui_1")
 
