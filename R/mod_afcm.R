#' afcm UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_afcm_ui <- function(id){
  ns <- NS(id)
  
  title_afcm <- tags$div(
    class = "multiple-select-var", style = "width: 150px;",
    conditionalPanel(
      condition = paste0(
        "input.tabAFCM == 'tabInd' || ", "input.tabAFCM == 'tabCat' || ", 
        "input.tabAFCM == 'tabBi'"),
      radioSwitch(ns("plotMode"), NULL, list("2D", "3D")))
  )
  
  opts_afcm <- tabsOptions(
    heights = 70, tabs.content = list(
      list(
        options.run(ns("run_afcm")), tags$hr(style = "margin-top: 0px;"),
        fluidRow(
          style = "margin-left: 0px; margin-right: 0px",
          col_6(sliderInput(ns("slider_ejes"), labelInput("selejes"), 1, 10, c(1, 2))),
          col_6(radioSwitch(ns("switch_label"), "selabel", c("si", "no"), val.def = T))
        ),
        conditionalPanel(
          condition = paste0("input.tabAFCM == 'tabInd' ||",
                             " input.tabAFCM == 'tabBi'"),
          fluidRow(
            style = "margin-left: 0px; margin-right: 0px",
            col_6(sliderInput(ns("ind_cos"), labelInput("cosind"), 0, 100, 0)),
            col_3(
              colourpicker::colourInput(
                ns("col_afcm_ind"), labelInput("colindbien"), "steelblue", 
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
            "input.tabAFCM == 'tabCat' || ", "input.tabAFCM == 'tabBi'"),
          fluidRow(
            style = "margin-left: 0px; margin-right: 0px",
            col_6(sliderInput(ns("cat_cos"), labelInput("cosvar"), 0, 100, 0)),
            col_3(
              colourpicker::colourInput(
                ns("col_afcm_cat"), labelInput("colcatbien"), "forestgreen", 
                allowTransparent = T)
            ),
            col_3(
              colourpicker::colourInput(
                ns("col_cat_cos"), labelInput("colcatmal"), "darkorchid", 
                allowTransparent = T)
            )
          )
        ),
        conditionalPanel(
          condition = "input.tabAFCM == 'tabVar'",
          colourpicker::colourInput(
            ns("col_afcm_var"), labelInput("colvarbien"), "forestgreen", 
            allowTransparent = T)
        )
      )
    )
  )
  
  tagList(
    tabBoxPrmdt(
      id = "tabAFCM", title = title_afcm, opciones = opts_afcm,
      tabPanel(
        title = labelInput("individuos"), value = "tabInd",
        echarts4rOutput(ns('plot_ind'), height = "75vh")),
      tabPanel(
        title = labelInput("variables"), value = "tabVar",
        echarts4rOutput(ns('plot_var'), height = "75vh")),
      tabPanel(
        title = labelInput("categorias"), value = "tabCat",
        echarts4rOutput(ns('plot_cat'), height = "75vh")),
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
        title = labelInput("resultados"), value = "afcm.salida",
        div(style = "height: 75vh;overflow-y: scroll;", 
            withLoader(verbatimTextOutput(ns("txtafcm")), 
                       type = "html", loader = "loader4")))
    )
  )
}
    
#' afcm Server Functions
#'
#' @noRd 
mod_afcm_server <- function(id, updateData, codedioma) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Generate AFCM on load data
    modelo.afcm <- reactive({
      input$run_afcm
      
      datos       <- var.categoricas(updateData$datos)
      dimensiones <- 10
      if(ncol(datos) < 10) {
        dimensiones <- ncol(datos)
      }
      
      updateSliderInput(session, inputId = "slider_ejes", max = dimensiones)
      
      if(nrow(datos) == 0) {
        return(NULL)
      } else {
        cod <- paste0("### docafcmmodel\nmodelo.afcm <- MCA(var.categoricas(datos), ncp = ", 
                      dimensiones, ", graph = F)\n")
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        return(MCA(datos, ncp = dimensiones, graph = F))
      }
    })
    
    # Plot AFCM (individuals)
    output$plot_ind <- renderEcharts4r({
      modelo <- modelo.afcm()

      ejes    <- isolate(input$slider_ejes)
      etqs    <- isolate(input$switch_label)
      ind.cos <- isolate(input$ind_cos) * 0.01
      ind.col <- isolate(input$col_afcm_ind)
      cos.col <- isolate(input$col_ind_cos)
      titulos <- c(tr("bienr", codedioma$idioma), tr("malr", codedioma$idioma))

      if(is.null(modelo)) {
        return(NULL)
      } else {
        if(input$plotMode) {
          cod <- paste0("### docafcmind2d\ne_afcmind(modelo.afcm, c(", paste(ejes, collapse = ", "),
                        "), '", ind.col, "', ", ind.cos, ", '", cos.col, "', c('",
                        titulos[1], "', '", titulos[2], "'))\n")
          isolate(codedioma$code <- append(codedioma$code, cod))

          e_afcmind(modelo, ejes, ind.col, ind.cos, cos.col, titulos, etqs)
        } else {
          cod <- paste0("### docafcmind3d\ne_afcmind_3D(modelo.afcm, c(", paste(ejes, collapse = ", "),
                        "), '", ind.col, "', ", ind.cos, ", '", cos.col,
                        "', c('", titulos[1], "', '", titulos[2], "'))\n")
          isolate(codedioma$code <- append(codedioma$code, cod))
          
          e_afcmind_3D(modelo, c(1, 2, 3), ind.col, ind.cos, cos.col, titulos, etqs)
        }
      }
    })
    
    # Plot AFCM (variables)
    output$plot_var <- renderEcharts4r({
      modelo <- modelo.afcm()
      
      ejes    <- isolate(input$slider_ejes)
      var.col <- isolate(input$col_afcm_var)
      titulos <- c(tr("bienr", codedioma$idioma), tr("malr", codedioma$idioma))
      
      if(is.null(modelo)) {
        return(NULL)
      } else {
        if(input$plotMode) {
          cod <- paste0("### docafcmvar2d\ne_afcmvar(modelo.afcm, c(", 
                        paste(ejes, collapse = ", "), "), '", var.col, "')\n")
          isolate(codedioma$code <- append(codedioma$code, cod))
          
          e_afcmvar(modelo, ejes, var.col)
        } else {
          cod <- paste0("### docafcmvar3d\ne_afcmvar_3D(modelo.afcm, c(", 
                        paste(ejes, collapse = ", "), "), '", var.col, "')\n")
          isolate(codedioma$code <- append(codedioma$code, cod))
          
          e_afcmvar_3D(modelo, c(1, 2, 3), var.col)
        }
      }
    })

    # Plot AFCM (categories)
    output$plot_cat <- renderEcharts4r({
      modelo <- modelo.afcm()

      ejes    <- isolate(input$slider_ejes)
      var.cos <- isolate(input$cat_cos) * 0.01
      var.col <- isolate(input$col_afcm_cat)
      cos.col <- isolate(input$col_cat_cos)
      titulos <- c(tr("bienr", codedioma$idioma), tr("malr", codedioma$idioma))

      if(is.null(modelo)) {
        return(NULL)
      } else {
        if(input$plotMode) {
          cod <- paste0("### docafcmvar2d\ne_afcmcat(modelo.afcm, c(", paste(ejes, collapse = ", "),
                        "), '", var.col, "', ", var.cos, ", '", cos.col, "', c('",
                        titulos[1], "', '", titulos[2], "'))\n")
          isolate(codedioma$code <- append(codedioma$code, cod))

          e_afcmcat(modelo, ejes, var.col, var.cos, cos.col, titulos)
        } else {
          cod <- paste0("### docafcmvar3d\ne_afcmcat_3D(modelo.afcm, c(", paste(ejes, collapse = ", "),
                        "), '", var.col, "', ", var.cos, ", '", cos.col,
                        "', c('", titulos[1], "', '", titulos[2], "'))\n")
          isolate(codedioma$code <- append(codedioma$code, cod))

          e_afcmcat_3D(modelo, c(1, 2, 3), var.col, var.cos, cos.col, titulos)
        }
      }
    })

    # Plot AFCM (Biplot)
    output$plot_bi <- renderEcharts4r({
      modelo  <- modelo.afcm()

      ejes        <- isolate(input$slider_ejes)
      etqs        <- isolate(input$switch_label)
      ind.cos     <- isolate(input$ind_cos) * 0.01
      ind.col     <- isolate(input$col_afcm_ind)
      ind.cos.col <- isolate(input$col_ind_cos)
      var.cos     <- isolate(input$cat_cos) * 0.01
      var.col     <- isolate(input$col_afcm_cat)
      var.cos.col <- isolate(input$col_cat_cos)
      titulos <- c(tr("bienr", codedioma$idioma), tr("malr", codedioma$idioma))

      if(is.null(modelo)) {
        return(NULL)
      } else {
        if(input$plotMode) {
          cod <- paste0("### docafcmbi2d\ne_afcmbi(modelo.afcm, c(", paste(ejes, collapse = ", "),
                        "), '", ind.col, "', '", var.col, "', ",
                        ind.cos, ", ", var.cos, ", '", ind.cos.col, "', '",
                        var.cos.col, "', c('", titulos[1], "', '", titulos[2],
                        "'))\n")
          isolate(codedioma$code <- append(codedioma$code, cod))

          e_afcmbi(modelo, ejes, ind.col, var.col, ind.cos, var.cos,
                  ind.cos.col, var.cos.col, titulos, etqs)
        } else {
          cod <- paste0("### docafcmbi3d\ne_afcmbi_3D(modelo.afcm, c(", paste(ejes, collapse = ", "),
                        "), '", ind.col, "', '", var.col, "', ",
                        ind.cos, ", ", var.cos, ", '", ind.cos.col, "', '",
                        var.cos.col, "', c('", titulos[1], "', '", titulos[2],
                        "'))\n")
          isolate(codedioma$code <- append(codedioma$code, cod))

          e_afcmbi_3D(modelo, c(1, 2, 3), ind.col, var.col, ind.cos, var.cos,
                     ind.cos.col, var.cos.col, titulos, etqs)
        }
      }
    })

    # Varianza Explicada por cada Eje
    output$plotVEE <- renderEcharts4r({
      modelo <- modelo.afcm()

      datos.plot <- data.frame(x = row.names(modelo$eig), y = modelo$eig[, 2])
      if(nrow(datos.plot) > 10) {
        datos.plot <- datos.plot[1:10, ]
      }

      cod <- paste0(
        '### docvee\n',
        'datos.plot <- data.frame (x = row.names(modelo.afcm$eig), y = modelo.afcm$eig[, 2])\n\n',
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
      modelo <- modelo.afcm()
      ejes <- isolate(input$slider_ejes)

      datos.plot <- data.frame (x = row.names(modelo$ind$cos2),
                                y = apply(modelo$ind$cos2[, ejes], 1, sum))
      datos.plot <- datos.plot[order(datos.plot$y, decreasing = T), ]
      if(nrow(datos.plot) > 20) {
        datos.plot <- datos.plot[1:20, ]
      }

      cod <- paste0(
        '### doccci\n',
        'datos.plot <- data.frame (x = row.names(modelo.afcm$ind$cos2),\n',
        '                          y = apply(modelo.afcm$ind$cos2[, c(', paste(ejes, collapse = ","), ')], 1, sum))\n',
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
      modelo <- modelo.afcm()
      ejes <- isolate(input$slider_ejes)

      datos.plot <- data.frame (x = row.names(modelo$var$cos2),
                                y = apply(modelo$var$cos2[, ejes], 1, sum))
      datos.plot <- datos.plot[order(datos.plot$y, decreasing = T), ]
      if(nrow(datos.plot) > 20) {
        datos.plot <- datos.plot[1:20, ]
      }

      cod <- paste0(
        '### docccv\n',
        'datos.plot <- data.frame (x = row.names(modelo.afcm$var$cos2),\n',
        '                          y = apply(modelo.afcm$var$cos2[, c(', paste(ejes, collapse = ","), ')], 1, sum))\n',
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
      modelo <- modelo.afcm()

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
        'datos <- data.frame (x = row.names(modelo.afcm$var$contrib),\n',
        '                     y = modelo.afcm$var$contrib[, ', ejes[1], '],\n',
        '                     z = modelo.afcm$var$contrib[, ', ejes[2], '])\n',
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

    # Modelo AFCM (Resultados numéricos)
    output$txtafcm <- renderPrint({
      modelo <- modelo.afcm()
      print(list(eig = modelo$eig, ind = modelo$ind, var = modelo$var))
    })
  })
}
    
## To be copied in the UI
# mod_afcm_ui("afc_1")
    
## To be copied in the server
# mod_afcm_server("afc_1")
