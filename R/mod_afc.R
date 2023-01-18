#' afc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_afc_ui <- function(id){
  ns <- NS(id)
  
  title_afc <- tags$div(
    class = "multiple-select-var", style = "width: 150px;",
    conditionalPanel(
      condition = paste0(
        "input.tabAFC == 'tabInd' || ", "input.tabAFC == 'tabVar' || ", 
        "input.tabAFC == 'tabBi'"),
      radioSwitch(ns("plotMode"), NULL, list("2D", "3D")))
  )
  
  opts_afc <- tabsOptions(
    heights = 70, tabs.content = list(
      list(
        options.run(ns("run_afc")), tags$hr(style = "margin-top: 0px;"),
        fluidRow(
          style = "margin-left: 0px; margin-right: 0px",
          col_6(sliderInput(ns("slider_ejes"), labelInput("selejes"), 1, 10, c(1, 2))),
          col_6(radioSwitch(ns("switch_label"), "selabel", c("si", "no"), val.def = T))
        ),
        conditionalPanel(
          condition = paste0("input.tabAFC == 'tabInd' ||",
                             " input.tabAFC == 'tabBi'"),
          fluidRow(
            style = "margin-left: 0px; margin-right: 0px",
            col_6(sliderInput(ns("ind_cos"), labelInput("cosind"), 0, 100, 0)),
            col_3(
              colourpicker::colourInput(
                ns("col_afc_ind"), labelInput("colindbien"), "steelblue", 
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
            "input.tabAFC == 'tabVar' || ", "input.tabAFC == 'tabBi'"),
          fluidRow(
            style = "margin-left: 0px; margin-right: 0px",
            col_6(sliderInput(ns("var_cos"), labelInput("cosvar"), 0, 100, 0)),
            col_3(
              colourpicker::colourInput(
                ns("col_afc_var"), labelInput("colvarbien"), "forestgreen", 
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
      id = "tabAFC", title = title_afc, opciones = opts_afc,
      tabPanel(
        title = labelInput("tcruz"), value = "tabCruz",
        echarts4rOutput(ns('plot_cruz'), height = "75vh")),
      tabPanel(
        title = labelInput("prows"), value = "tabInd",
        echarts4rOutput(ns('plot_ind'), height = "75vh")),
      tabPanel(
        title = labelInput("pcols"), value = "tabVar",
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
        tabPanel(labelInput("cp"), value = "tabPC",
                 echarts4rOutput(ns("plotPC"), height = "75vh"))
      ),
      tabPanel(
        title = labelInput("resultados"), value = "afc.salida",
        div(style = "height: 75vh;overflow-y: scroll;", 
            withLoader(verbatimTextOutput(ns("txtafc")), 
                       type = "html", loader = "loader4")))
    )
  )
}
    
#' afc Server Functions
#'
#' @noRd 
mod_afc_server <- function(id, updateData, codedioma) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Plot Tabla Cruzada
    output$plot_cruz <- renderEcharts4r({
      datos <- var.numericas(updateData$datos)
      
      if(nrow(datos) == 0) {
        return(NULL)
      } else {
        cod <- paste0("### docballoon\ne_balloon(var.numericas(datos))\n")
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        return(e_balloon(datos))
      }
    })
    
    # Generate AFC on load data
    modelo.afc <- reactive({
      input$run_afc
      
      datos       <- var.numericas(updateData$datos)
      dimensiones <- 10
      if(ncol(datos) < 10) {
        dimensiones <- ncol(datos)
      }
      
      updateSliderInput(session, inputId = "slider_ejes", max = dimensiones)
      
      if(nrow(datos) == 0) {
        return(NULL)
      } else {
        cod <- paste0("### docafcmodel\nmodelo.afc <- CA(var.numericas(datos), ncp = ", 
                      dimensiones, ", graph = F)\n")
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        return(CA(datos, ncp = dimensiones, graph = F))
      }
    })
    
    # Plot AFC (individuals)
    output$plot_ind <- renderEcharts4r({
      modelo <- modelo.afc()
      
      ejes    <- isolate(input$slider_ejes)
      etqs    <- isolate(input$switch_label)
      ind.cos <- isolate(input$ind_cos) * 0.01
      ind.col <- isolate(input$col_afc_ind)
      cos.col <- isolate(input$col_ind_cos)
      titulos <- c(tr("bienr", codedioma$idioma), tr("malr", codedioma$idioma))
      
      if(is.null(modelo)) {
        return(NULL)
      } else {
        if(input$plotMode) {
          cod <- paste0("### docafcind2d\ne_afcind(modelo.afc, c(", paste(ejes, collapse = ", "), 
                        "), '", ind.col, "', ", ind.cos, ", '", cos.col, "', c('", 
                        titulos[1], "', '", titulos[2], "'))\n")
          isolate(codedioma$code <- append(codedioma$code, cod))
          
          e_afcrow(modelo, ejes, ind.col, ind.cos, cos.col, titulos, etqs)
        } else {
          cod <- paste0("### docafcind3d\ne_afcind_3D(modelo, c(", paste(ejes, collapse = ", "), 
                        "), '", ind.col, "', ", ind.cos, ", '", cos.col, 
                        "', c('", titulos[1], "', '", titulos[2], "'))\n")
          isolate(codedioma$code <- append(codedioma$code, cod))
          
          e_afcrow_3D(modelo, c(1, 2, 3), ind.col, ind.cos, cos.col, titulos, etqs)
        }
      }
    })
    
    # Plot AFC (variables)
    output$plot_var <- renderEcharts4r({
      modelo <- modelo.afc()
      
      ejes    <- isolate(input$slider_ejes)
      var.cos <- isolate(input$var_cos) * 0.01
      var.col <- isolate(input$col_afc_var)
      cos.col <- isolate(input$col_var_cos)
      titulos <- c(tr("bienr", codedioma$idioma), tr("malr", codedioma$idioma))
      
      if(is.null(modelo)) {
        return(NULL)
      } else {
        if(input$plotMode) {
          cod <- paste0("### docafcvar2d\ne_afcvar(modelo.afc, c(", paste(ejes, collapse = ", "), 
                        "), '", var.col, "', ", var.cos, ", '", cos.col, "', c('", 
                        titulos[1], "', '", titulos[2], "'))\n")
          isolate(codedioma$code <- append(codedioma$code, cod))
          
          e_afccol(modelo, ejes, var.col, var.cos, cos.col, titulos)
        } else {
          cod <- paste0("### docafcvar3d\ne_afcvar_3D(modelo, c(", paste(ejes, collapse = ", "), 
                        "), '", var.col, "', ", var.cos, ", '", cos.col, 
                        "', c('", titulos[1], "', '", titulos[2], "'))\n")
          isolate(codedioma$code <- append(codedioma$code, cod))
          
          e_afccol_3D(modelo, c(1, 2, 3), var.col, var.cos, cos.col, titulos)
        }
      }
    })
    
    # Plot PCA (Biplot)
    output$plot_bi <- renderEcharts4r({
      modelo  <- modelo.afc()
      
      ejes        <- isolate(input$slider_ejes)
      etqs        <- isolate(input$switch_label)
      ind.cos     <- isolate(input$ind_cos) * 0.01
      ind.col     <- isolate(input$col_afc_ind)
      ind.cos.col <- isolate(input$col_ind_cos)
      var.cos     <- isolate(input$var_cos) * 0.01
      var.col     <- isolate(input$col_afc_var)
      var.cos.col <- isolate(input$col_var_cos)
      titulos <- c(tr("bienr", codedioma$idioma), tr("malr", codedioma$idioma))
      
      if(is.null(modelo)) {
        return(NULL)
      } else {
        if(input$plotMode) {
          cod <- paste0("### docafcbi2d\ne_afcbi(modelo.afc, c(", paste(ejes, collapse = ", "), 
                        "), '", ind.col, "', '", var.col, "', ", 
                        ind.cos, ", ", var.cos, ", '", ind.cos.col, "', '", 
                        var.cos.col, "', c('", titulos[1], "', '", titulos[2], 
                        "'))\n")
          isolate(codedioma$code <- append(codedioma$code, cod))
          
          e_afcbi(modelo, ejes, ind.col, var.col, ind.cos, var.cos, 
                  ind.cos.col, var.cos.col, titulos, etqs)
        } else {
          cod <- paste0("### docafcbi3d\ne_afcbi_3D(modelo.afc, c(", paste(ejes, collapse = ", "), 
                        "), '", ind.col, "', '", var.col, "', ", 
                        ind.cos, ", ", var.cos, ", '", ind.cos.col, "', '", 
                        var.cos.col, "', c('", titulos[1], "', '", titulos[2], 
                        "'))\n")
          isolate(codedioma$code <- append(codedioma$code, cod))
          
          e_afcbi_3D(modelo, c(1, 2, 3), ind.col, var.col, ind.cos, var.cos, 
                     ind.cos.col, var.cos.col, titulos, etqs)
        }
      }
    })
    
    # Varianza Explicada por cada Eje
    output$plotVEE <- renderEcharts4r({
      modelo <- modelo.afc()
      
      datos.plot <- data.frame(x = row.names(modelo$eig), y = modelo$eig[, 2])
      if(nrow(datos.plot) > 10) {
        datos.plot <- datos.plot[1:10, ]
      }
      
      cod <- paste0(
        '### docvee\n',
        'datos.plot <- data.frame (x = row.names(modelo.afc$eig), y = modelo.afc$eig[, 2])\n\n',
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
      modelo <- modelo.afc()
      ejes <- isolate(input$slider_ejes)
      
      datos.plot <- data.frame (x = row.names(modelo$row$cos2), 
                                y = apply(modelo$row$cos2[, ejes], 1, sum))
      datos.plot <- datos.plot[order(datos.plot$y, decreasing = T), ]
      if(nrow(datos.plot) > 20) {
        datos.plot <- datos.plot[1:20, ]
      }
      
      cod <- paste0(
        '### doccci\n',
        'datos.plot <- data.frame (x = row.names(modelo.afc$row$cos2),\n',
        '                          y = apply(modelo.afc$row$cos2[, c(', paste(ejes, collapse = ","), ')], 1, sum))\n',
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
      modelo <- modelo.afc()
      ejes <- isolate(input$slider_ejes)
      
      datos.plot <- data.frame (x = row.names(modelo$col$cos2), 
                                y = apply(modelo$col$cos2[, ejes], 1, sum))
      datos.plot <- datos.plot[order(datos.plot$y, decreasing = T), ]
      if(nrow(datos.plot) > 20) {
        datos.plot <- datos.plot[1:20, ]
      }
      
      cod <- paste0(
        '### docccv\n',
        'datos.plot <- data.frame (x = row.names(modelo.afc$col$cos2),\n',
        '                          y = apply(modelo.afc$col$cos2[, c(', paste(ejes, collapse = ","), ')], 1, sum))\n',
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
    
    # Contribución de las variables
    output$plotPC <- renderEcharts4r({
      modelo <- modelo.afc()
      
      ejes <- isolate(input$slider_ejes)
      
      datos.plot <- data.frame (
        x = row.names(modelo$col$contrib), 
        y = round(modelo$col$contrib[, ejes[1]], 3),
        z = round(modelo$col$contrib[, ejes[2]], 3))
      
      if(nrow(datos.plot) > 20) {
        datos.plot <- datos.plot[1:20, ]
      }
      
      cod <- paste0(
        '### docpc\n',
        'datos <- data.frame (x = row.names(modelo.afc$col$contrib),\n', 
        '                     y = modelo.afc$col$contrib[, ', ejes[1], '],\n',
        '                     z = modelo.afc$col$contrib[, ', ejes[2], '])\n',
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
    
    # Modelo AFC (Resultados numéricos)
    output$txtafc <- renderPrint({
      modelo <- modelo.afc()
      print(list(eig = modelo$eig, col = modelo$col, row = modelo$row))
    })
  })
}
    
## To be copied in the UI
# mod_afc_ui("afc_1")
    
## To be copied in the server
# mod_afc_server("afc_1")
