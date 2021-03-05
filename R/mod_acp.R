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
              codigo.monokai(ns(paste0("fieldCode", i)), height = "10vh")
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
        echarts4rOutput(ns('plot_ind'), height = "75vh")),
      tabPanel(
        title = labelInput("variables"), value = "tabVar",
        echarts4rOutput(ns('plot_var'), height = "75vh")),
      tabPanel(
        title = labelInput("sobreposicion"), value = "tabBi",
        echarts4rOutput(ns('plot_bi'), height = "75vh")),
      navbarMenu(
        labelInput("ayudacp"),
        tabPanel(labelInput("vee"), value = "tabVEE", 
                 echarts4rOutput(ns("plotVEE"), height = "75vh")),
        tabPanel(labelInput("cci"), value = "tabCCI",
                 echarts4rOutput(ns("plotCCI"), height = "75vh")),
        tabPanel(labelInput("ccv"), value = "tabCCV",
                 echarts4rOutput(ns("plotCCV"), height = "75vh")),
        tabPanel(labelInput("cvc"), value = "tabCVC",
                 echarts4rOutput(ns("plotCVC"), height = "75vh")),
        tabPanel(labelInput("cp"), value = "tabPC",
                 echarts4rOutput(ns("plotPC"), height = "75vh"))
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
  
  #' Plot PCA (individuals)
  output$plot_ind <- renderEcharts4r({
    modelo <- modelo.pca()
    
    ejes    <- isolate(input$slider_ejes)
    ind.cos <- isolate(input$ind_cos) * 0.01
    ind.col <- isolate(input$col_pca_ind)
    cos.col <- isolate(input$col_ind_cos)
    titulos <- c(tr("bienr", updateData$idioma), tr("malr", updateData$idioma))
    
    if(is.null(modelo)) {
      return(NULL)
    } else {
      if(input$plotMode) {
        cod <- paste0("e_pcaind(modelo.pca, c(", paste(ejes, collapse = ", "), 
                      "), '", ind.col, "', ", ind.cos, ", '", cos.col, "', c('", 
                      titulos[1], "', '", titulos[2], "'))")
        updateAceEditor(session, "fieldCodeInd", value = cod)
        e_pcaind(modelo, ejes, ind.col, ind.cos, cos.col, titulos)
      } else {
        cod <- paste0("e_pcaind_3D(modelo, c(", paste(ejes, collapse = ", "), 
                      "), '", ind.col, "', ", ind.cos, ", '", cos.col, 
                      "', c('", titulos[1], "', '", titulos[2], "'))")
        updateAceEditor(session, "fieldCodeInd", value = cod)
        e_pcaind_3D(modelo, c(1, 2, 3), ind.col, ind.cos, cos.col, titulos)
      }
    }
  })
  
  #' Plot PCA (variables)
  output$plot_var <- renderEcharts4r({
    modelo <- modelo.pca()
    
    ejes    <- isolate(input$slider_ejes)
    var.cos <- isolate(input$var_cos) * 0.01
    var.col <- isolate(input$col_pca_var)
    cos.col <- isolate(input$col_var_cos)
    titulos <- c(tr("bienr", updateData$idioma), tr("malr", updateData$idioma))
    
    if(is.null(modelo)) {
      return(NULL)
    } else {
      if(input$plotMode) {
        cod <- paste0("e_pcavar(modelo.pca, c(", paste(ejes, collapse = ", "), 
                      "), '", var.col, "', ", var.cos, ", '", cos.col, "', c('", 
                      titulos[1], "', '", titulos[2], "'))")
        updateAceEditor(session, "fieldCodeVar", value = cod)
        e_pcavar(modelo, ejes, var.col, var.cos, cos.col, titulos)
      } else {
        cod <- paste0("e_pcavar_3D(modelo, c(", paste(ejes, collapse = ", "), 
                      "), '", var.col, "', ", var.cos, ", '", cos.col, 
                      "', c('", titulos[1], "', '", titulos[2], "'))")
        updateAceEditor(session, "fieldCodeVar", value = cod)
        e_pcavar_3D(modelo, c(1, 2, 3), var.col, var.cos, cos.col, titulos)
      }
    }
  })
  
  #' Plot PCA (Biplot)
  output$plot_bi <- renderEcharts4r({
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
      if(input$plotMode) {
        cod <- paste0("e_pcabi(modelo.pca, c(", paste(ejes, collapse = ", "), 
                      "), '", ind.col, "', '", var.col, "', ", 
                      ind.cos, ", ", var.cos, ", '", ind.cos.col, "', '", 
                      var.cos.col, "', c('", titulos[1], "', '", titulos[2], 
                      "'))")
        updateAceEditor(session, "fieldCodeBi", value = cod)
        e_pcabi(modelo, ejes, ind.col, var.col, ind.cos, var.cos, 
                ind.cos.col, var.cos.col, titulos)
      } else {
        cod <- paste0("e_pcabi_3D(modelo.pca, c(", paste(ejes, collapse = ", "), 
                      "), '", ind.col, "', '", var.col, "', ", 
                      ind.cos, ", ", var.cos, ", '", ind.cos.col, "', '", 
                      var.cos.col, "', c('", titulos[1], "', '", titulos[2], 
                      "'))")
        updateAceEditor(session, "fieldCodeBi", value = cod)
        e_pcabi_3D(modelo, c(1, 2, 3), ind.col, var.col, ind.cos, var.cos, 
                ind.cos.col, var.cos.col, titulos)
      }
    }
  })
  
  #' Varianza Explicada por cada Eje
  output$plotVEE <- renderEcharts4r({
    modelo <- modelo.pca()
    
    datos.plot <- data.frame(x = row.names(modelo$eig), y = modelo$eig[, 2])
    if(nrow(datos.plot) > 10) {
      datos.plot <- datos.plot[1:10, ]
    }
    
    cod <- paste0(
    'datos.plot <- data.frame (x = row.names(modelo.pca$eig), y = modelo.pca$eig[, 2])\n\n',
    'datos.plot %>% e_charts(x) %>% e_bar(y) %>% e_legend(F) %>%\n',
    '  e_tooltip(formatter = htmlwidgets::JS(paste0(\n',
    '    "function(params) {",\n',
    '    "  return(params.value[0] + \': \' + parseFloat(params.value[1]).toFixed(3))",\n',
    '    "}"\n  )))\n')
    updateAceEditor(session, "fieldCodeVEE", value = cod)
    
    datos.plot %>% e_charts(x) %>% e_bar(y) %>% e_legend(F) %>% 
      e_tooltip(formatter = htmlwidgets::JS(paste0(
        "function(params) {\n",
        "  return(params.value[0] + ': ' + parseFloat(params.value[1]).toFixed(3))",
        "}"
      )))
  })
  
  #' Cosenos Cuadrados de los Individuos
  output$plotCCI <- renderEcharts4r({
    modelo <- modelo.pca()
    ejes <- isolate(input$slider_ejes)
    
    datos.plot <- data.frame (x = row.names(modelo$ind$cos2), 
                              y = apply(modelo$ind$cos2[, ejes], 1, sum))
    datos.plot <- datos.plot[order(datos.plot$y, decreasing = T), ]
    if(nrow(datos.plot) > 20) {
      datos.plot <- datos.plot[1:20, ]
    }
    
    cod <- paste0(
      'datos.plot <- data.frame (x = row.names(modelo$ind$cos2),\n',
      '                          y = apply(modelo$ind$cos2[, c(', paste(ejes, collapse = ","), ')], 1, sum))\n',
      'if(nrow(datos.plot) > 20) {\n',
      '  datos.plot <- datos.plot[1:20, ]\n}\n\n',
      'datos.plot %>% e_charts(x) %>% e_bar(y) %>% e_legend(F) %>%\n',
      '  e_tooltip(formatter = htmlwidgets::JS(paste0(\n',
      '    "function(params) {",\n',
      '    "  return(params.value[0] + \': \' + parseFloat(params.value[1]).toFixed(3))",\n',
      '    "}"\n  )))\n')
    updateAceEditor(session, "fieldCodeCCI", value = cod)
    
    datos.plot %>% e_charts(x) %>% e_bar(y) %>% e_legend(F) %>% 
      e_tooltip(formatter = htmlwidgets::JS(paste0(
        "function(params) {\n",
        "  return(params.value[0] + ': ' + parseFloat(params.value[1]).toFixed(3))",
        "}"
      )))
  })
  
  #' Cosenos Cuadrados de las Variables
  output$plotCCV <- renderEcharts4r({
    modelo <- modelo.pca()
    ejes <- isolate(input$slider_ejes)
    
    datos.plot <- data.frame (x = row.names(modelo$var$cos2), 
                              y = apply(modelo$var$cos2[, ejes], 1, sum))
    datos.plot <- datos.plot[order(datos.plot$y, decreasing = T), ]
    if(nrow(datos.plot) > 20) {
      datos.plot <- datos.plot[1:20, ]
    }
    
    cod <- paste0(
      'datos.plot <- data.frame (x = row.names(modelo$var$cos2),\n',
      '                          y = apply(modelo$var$cos2[, c(', paste(ejes, collapse = ","), ')], 1, sum))\n',
      'if(nrow(datos.plot) > 20) {\n',
      '  datos.plot <- datos.plot[1:20, ]\n}\n\n',
      'datos.plot %>% e_charts(x) %>% e_bar(y) %>% e_legend(F) %>%\n',
      '  e_tooltip(formatter = htmlwidgets::JS(paste0(\n',
      '    "function(params) {",\n',
      '    "  return(params.value[0] + \': \' + parseFloat(params.value[1]).toFixed(3))",\n',
      '    "}"\n  )))\n')
    updateAceEditor(session, "fieldCodeCCV", value = cod)
    
    datos.plot %>% e_charts(x) %>% e_bar(y) %>% e_legend(F) %>% 
      e_tooltip(formatter = htmlwidgets::JS(paste0(
        "function(params) {\n",
        "  return(params.value[0] + ': ' + parseFloat(params.value[1]).toFixed(3))",
        "}"
      )))
  })
  
  #' Correlación Variables con los Componentes
  output$plotCVC <- renderEcharts4r({
    modelo <- modelo.pca()
    
    cod <- paste0(
      'datos.plot <- round(modelo$var$cor, 3)\n', 
      'datos.plot %>% e_charts() %>%\n',
      '  e_correlations(\n',
      '    label = list(show = T),\n',
      '    itemStyle = list(borderWidth = 2, borderColor = "#fff")\n',
      '  ) %>% e_datazoom(show = F) %>% e_show_loading() %>% e_tooltip(\n',
      '    formatter = htmlwidgets::JS(paste0(\n',
      '      "function(params) {",\n',
      '      "  return(params.value[1] + \' ~ \' + params.value[0] + \': \' + params.value[2])",\n',
      '      "}"))\n  )\n'
    )
    updateAceEditor(session, "fieldCodeCVC", value = cod)
    
    datos.plot <- round(modelo$var$cor, 3)
    datos.plot %>% e_charts() %>%
      e_correlations(
        label = list(show = T),
        itemStyle = list(borderWidth = 2, borderColor = "#fff")
      ) %>% e_datazoom(show = F) %>% e_show_loading() %>% e_tooltip(
        formatter = htmlwidgets::JS(paste0(
          "function(params) {\n",
          "  return(params.value[1] + ' ~ ' + params.value[0] + ': ' + params.value[2])\n", 
          "}"))
      )
  })
  
  #' Contribución de las variables
  output$plotPC <- renderEcharts4r({
    modelo <- modelo.pca()
    
    ejes <- isolate(input$slider_ejes)
    
    datos.plot <- data.frame (
      x = row.names(modelo$var$contrib), 
      y = round(modelo$var$contrib[, ejes[1]], 3),
      z = round(modelo$var$contrib[, ejes[2]], 3))
    
    if(nrow(datos.plot) > 20) {
      datos.plot <- datos.plot[1:20, ]
    }
    
    cod <- paste0(
      'datos <- data.frame (x = row.names(modelo.pca$var$contrib),\n', 
      '                     y = modelo.pca$var$contrib[, ', ejes[1], '],\n',
      '                     z = modelo.pca$var$contrib[, ', ejes[2], '])\n',
      'if(nrow(datos.plot) > 20) {\n',
      '  datos.plot <- datos.plot[1:20, ]\n',
      '}\n\n',
      'datos.plot %>% e_charts(x) %>% e_bar(y, name = "', paste0("Comp ", ejes[1]), '") %>% \n',
      '  e_bar(z, name = "', paste0("Comp ", ejes[2]), '") %>% e_tooltip()\n'
    )
    updateAceEditor(session, "fieldCodePC", value = cod)
    
    datos.plot %>% e_charts(x) %>% e_bar(y, name = paste0("Comp ", ejes[1])) %>% 
      e_bar(z, name = paste0("Comp ", ejes[2])) %>% e_tooltip()
  })
  
  #' Modelo ACP (Resultados numéricos)
  output$txtpca <- renderPrint(print(modelo.pca()))
}
    
## To be copied in the UI
# mod_acp_ui("acp_ui_1")
    
## To be copied in the server
# callModule(mod_acp_server, "acp_ui_1")
 
