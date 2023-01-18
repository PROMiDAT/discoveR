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
    heights = 70, tabs.content = list(
      list(
        options.run(ns("run_pca")), tags$hr(style = "margin-top: 0px;"),
        radioSwitch(ns("switch_scale"), NULL, c("centrar", "nocentrar")),
        fluidRow(
          style = "margin-left: 0px; margin-right: 0px",
          col_6(sliderInput(ns("slider_ejes"), labelInput("selejes"), 1, 10, c(1, 2))),
          col_6(radioSwitch(ns("switch_label"), "selabelind", c("si", "no"), val.def = F))
        ),
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
        HTML("<center>"), 
        echarts4rOutput(ns('plot_var'), height = "75vh", width = "75vh"),
        HTML("</center>")),
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
mod_acp_server <- function(id, updateData, codedioma) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Generate PCA on load data
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
        cod <- paste0("### docpcamodel\nmodelo.pca <- PCA(var.numericas(datos), scale.unit = ",
                      centrado, ", ncp = ", dimensiones, ", graph = F)\n")
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        return(PCA(datos, scale.unit = centrado, ncp = dimensiones, graph = F))
      }
    })
    
    # Plot PCA (individuals)
    output$plot_ind <- renderEcharts4r({
      modelo <- modelo.pca()
      
      ejes    <- isolate(input$slider_ejes)
      etqs    <- isolate(input$switch_label)
      ind.cos <- isolate(input$ind_cos) * 0.01
      ind.col <- isolate(input$col_pca_ind)
      cos.col <- isolate(input$col_ind_cos)
      titulos <- c(tr("bienr", codedioma$idioma), tr("malr", codedioma$idioma))
      
      if(is.null(modelo)) {
        return(NULL)
      } else {
        if(input$plotMode) {
          cod <- paste0("### docpcaind2d\ne_pcaind(modelo.pca, c(", paste(ejes, collapse = ", "), 
                        "), '", ind.col, "', ", ind.cos, ", '", cos.col, "', c('", 
                        titulos[1], "', '", titulos[2], "'))\n")
          isolate(codedioma$code <- append(codedioma$code, cod))
          
          e_pcaind(modelo, ejes, ind.col, ind.cos, cos.col, titulos, etqs)
        } else {
          cod <- paste0("### docpcaind3d\ne_pcaind_3D(modelo.pca, c(", paste(ejes, collapse = ", "), 
                        "), '", ind.col, "', ", ind.cos, ", '", cos.col, 
                        "', c('", titulos[1], "', '", titulos[2], "'))\n")
          isolate(codedioma$code <- append(codedioma$code, cod))
          
          e_pcaind_3D(modelo, c(1, 2, 3), ind.col, ind.cos, cos.col, titulos, etqs)
        }
      }
    })
    
    # Plot PCA (variables)
    output$plot_var <- renderEcharts4r({
      modelo <- modelo.pca()
      
      ejes    <- isolate(input$slider_ejes)
      var.cos <- isolate(input$var_cos) * 0.01
      var.col <- isolate(input$col_pca_var)
      cos.col <- isolate(input$col_var_cos)
      titulos <- c(tr("bienr", codedioma$idioma), tr("malr", codedioma$idioma))
      
      if(is.null(modelo)) {
        return(NULL)
      } else {
        if(input$plotMode) {
          cod <- paste0("### docpcavar2d\ne_pcavar(modelo.pca, c(", paste(ejes, collapse = ", "), 
                        "), '", var.col, "', ", var.cos, ", '", cos.col, "', c('", 
                        titulos[1], "', '", titulos[2], "'))\n")
          isolate(codedioma$code <- append(codedioma$code, cod))
          
          e_pcavar(modelo, ejes, var.col, var.cos, cos.col, titulos)
        } else {
          cod <- paste0("### docpcavar3d\ne_pcavar_3D(modelo.pca, c(", paste(ejes, collapse = ", "), 
                        "), '", var.col, "', ", var.cos, ", '", cos.col, 
                        "', c('", titulos[1], "', '", titulos[2], "'))\n")
          isolate(codedioma$code <- append(codedioma$code, cod))
          
          e_pcavar_3D(modelo, c(1, 2, 3), var.col, var.cos, cos.col, titulos)
        }
      }
    })
    
    # Plot PCA (Biplot)
    output$plot_bi <- renderEcharts4r({
      modelo  <- modelo.pca()
      
      ejes        <- isolate(input$slider_ejes)
      etqs        <- isolate(input$switch_label)
      ind.cos     <- isolate(input$ind_cos) * 0.01
      ind.col     <- isolate(input$col_pca_ind)
      ind.cos.col <- isolate(input$col_ind_cos)
      var.cos     <- isolate(input$var_cos) * 0.01
      var.col     <- isolate(input$col_pca_var)
      var.cos.col <- isolate(input$col_var_cos)
      titulos <- c(tr("bienr", codedioma$idioma), tr("malr", codedioma$idioma))
      
      if(is.null(modelo)) {
        return(NULL)
      } else {
        if(input$plotMode) {
          cod <- paste0("### docpcabi2d\ne_pcabi(modelo.pca, c(", paste(ejes, collapse = ", "), 
                        "), '", ind.col, "', '", var.col, "', ", 
                        ind.cos, ", ", var.cos, ", '", ind.cos.col, "', '", 
                        var.cos.col, "', c('", titulos[1], "', '", titulos[2], 
                        "'))\n")
          isolate(codedioma$code <- append(codedioma$code, cod))
          
          e_pcabi(modelo, ejes, ind.col, var.col, ind.cos, var.cos, 
                  ind.cos.col, var.cos.col, titulos, etqs)
        } else {
          cod <- paste0("### docpcabi3d\ne_pcabi_3D(modelo.pca, c(", paste(ejes, collapse = ", "), 
                        "), '", ind.col, "', '", var.col, "', ", 
                        ind.cos, ", ", var.cos, ", '", ind.cos.col, "', '", 
                        var.cos.col, "', c('", titulos[1], "', '", titulos[2], 
                        "'))\n")
          isolate(codedioma$code <- append(codedioma$code, cod))
          
          e_pcabi_3D(modelo, c(1, 2, 3), ind.col, var.col, ind.cos, var.cos, 
                     ind.cos.col, var.cos.col, titulos, etqs)
        }
      }
    })
    
    # Varianza Explicada por cada Eje
    output$plotVEE <- renderEcharts4r({
      modelo <- modelo.pca()
      
      datos.plot <- data.frame(x = row.names(modelo$eig), y = modelo$eig[, 2])
      if(nrow(datos.plot) > 10) {
        datos.plot <- datos.plot[1:10, ]
      }
      
      cod <- paste0(
        '### docvee\n',
        'datos.plot <- data.frame (x = row.names(modelo.pca$eig), y = modelo.pca$eig[, 2])\n\n',
        'datos.plot |> e_charts(x) |> e_bar(y) |> e_legend(F) |>\n',
        '  e_tooltip(formatter = htmlwidgets::JS(paste0(\n',
        '    "function(params) {",\n',
        '    "  return(params.value[0] + \': \' + parseFloat(params.value[1]).toFixed(3))",\n',
        '    "}"\n  )))\n')
      isolate(codedioma$code <- append(codedioma$code, cod))
      
      datos.plot |> e_charts(x) |> e_bar(y) |> e_legend(F) |> 
        e_tooltip(formatter = htmlwidgets::JS(paste0(
          "function(params) {\n",
          "  return(params.value[0] + ': ' + parseFloat(params.value[1]).toFixed(3))",
          "}"
        )))
    })
    
    # Cosenos Cuadrados de los Individuos
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
        '### doccci\n',
        'datos.plot <- data.frame (x = row.names(modelo.pca$ind$cos2),\n',
        '                          y = apply(modelo.pca$ind$cos2[, c(', paste(ejes, collapse = ","), ')], 1, sum))\n',
        'if(nrow(datos.plot) > 20) {\n',
        '  datos.plot <- datos.plot[1:20, ]\n}\n\n',
        'datos.plot |> e_charts(x) |> e_bar(y) |> e_legend(F) |>\n',
        '  e_tooltip(formatter = htmlwidgets::JS(paste0(\n',
        '    "function(params) {",\n',
        '    "  return(params.value[0] + \': \' + parseFloat(params.value[1]).toFixed(3))",\n',
        '    "}"\n  )))\n')
      isolate(codedioma$code <- append(codedioma$code, cod))
      
      datos.plot |> e_charts(x) |> e_bar(y) |> e_legend(F) |> 
        e_tooltip(formatter = htmlwidgets::JS(paste0(
          "function(params) {\n",
          "  return(params.value[0] + ': ' + parseFloat(params.value[1]).toFixed(3))",
          "}"
        )))
    })
    
    # Cosenos Cuadrados de las Variables
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
        '### docccv\n',
        'datos.plot <- data.frame (x = row.names(modelo.pca$var$cos2),\n',
        '                          y = apply(modelo.pca$var$cos2[, c(', paste(ejes, collapse = ","), ')], 1, sum))\n',
        'if(nrow(datos.plot) > 20) {\n',
        '  datos.plot <- datos.plot[1:20, ]\n}\n\n',
        'datos.plot |> e_charts(x) |> e_bar(y) |> e_legend(F) |>\n',
        '  e_tooltip(formatter = htmlwidgets::JS(paste0(\n',
        '    "function(params) {",\n',
        '    "  return(params.value[0] + \': \' + parseFloat(params.value[1]).toFixed(3))",\n',
        '    "}"\n  )))\n')
      isolate(codedioma$code <- append(codedioma$code, cod))
      
      datos.plot |> e_charts(x) |> e_bar(y) |> e_legend(F) |> 
        e_tooltip(formatter = htmlwidgets::JS(paste0(
          "function(params) {\n",
          "  return(params.value[0] + ': ' + parseFloat(params.value[1]).toFixed(3))",
          "}"
        )))
    })
    
    # Correlación Variables con los Componentes
    output$plotCVC <- renderEcharts4r({
      modelo <- modelo.pca()
      
      cod <- paste0(
        '### doccvc\n',
        'datos.plot <- round(modelo.pca$var$cor, 3)\n', 
        'e_cor(datos.plot)\n'
      )
      isolate(codedioma$code <- append(codedioma$code, cod))
      
      datos.plot <- round(modelo$var$cor, 3)
      e_cor(datos.plot)
    })
    
    # Contribución de las variables
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
        '### docpc\n',
        'datos <- data.frame (x = row.names(modelo.pca$var$contrib),\n', 
        '                     y = modelo.pca$var$contrib[, ', ejes[1], '],\n',
        '                     z = modelo.pca$var$contrib[, ', ejes[2], '])\n',
        'if(nrow(datos.plot) > 20) {\n',
        '  datos.plot <- datos.plot[1:20, ]\n',
        '}\n\n',
        'datos.plot |> e_charts(x) |> e_bar(y, name = "', paste0("Comp ", ejes[1]), '") |> \n',
        '  e_bar(z, name = "', paste0("Comp ", ejes[2]), '") |> e_tooltip()\n'
      )
      isolate(codedioma$code <- append(codedioma$code, cod))
      
      datos.plot |> e_charts(x) |> e_bar(y, name = paste0("Comp ", ejes[1])) |> 
        e_bar(z, name = paste0("Comp ", ejes[2])) |> e_tooltip()
    })
    
    # Modelo ACP (Resultados numéricos)
    output$txtpca <- renderPrint({
      modelo <- modelo.pca()
      print(list(eig = modelo$eig, ind = modelo$ind, var = modelo$var))
    })
  })
}
    
## To be copied in the UI
# mod_acp_ui("acp_ui_1")
    
## To be copied in the server
# mod_acp_server("acp_ui_1", updateData, codedioma)
 
