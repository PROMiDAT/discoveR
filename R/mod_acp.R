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
    radioSwitch(ns("plotMode"), NULL, list("2D", "3D")),
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
        ),
        conditionalPanel(
          condition = "input.tabPCA == 'tabCVC'",
          selectInput(
            inputId = ns("cvc.metodo"), label = labelInput("seltipo"),
            choices =  c("circle", "square", "ellipse",  "number",
                         "shade", "color", "pie"))
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
        lapply(c('VEE', 'CCI', 'CCV', 'CVC', 'PC1', 'PC2'), function(i) {
          conditionalPanel(
            condition = paste0("input.tabPCA == 'tab", i, "'"),
            codigo.monokai(paste0("fieldCode", i), height = "15vh"))
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
          echarts4rOutput(ns("ind_2D"), height = "75vh")),
        tags$div(
          id = ns("div_ind_3D"),
          withLoader(plotlyOutput(ns("ind_3D"), height = "75vh"), 
                     type = "html", loader = "loader4"))
      ),
      tabPanel(
        title = labelInput("variables"), value = "tabVar",
        tags$div(
          id = ns("div_var_2D"),
          echarts4rOutput(ns("var_2D"), height = "75vh")),
        tags$div(
          id = ns("div_var_3D"),
          withLoader(plotlyOutput(ns("var_3D"), height = "75vh"), 
                     type = "html", loader = "loader4"))
      ),
      tabPanel(
        title = labelInput("sobreposicion"), value = "tabBi",
        tags$div(
          id = ns("div_bi_2D"),
          echarts4rOutput(ns("bi_2D"), height = "75vh")),
        tags$div(
          id = ns("div_bi_3D"),
          withLoader(plotlyOutput(ns("bi_3D"), height = "75vh"), 
                     type = "html", loader = "loader4"))
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
      updateSliderInput(session, inputId = "slider_ejes", max = dimensiones)
    }
    
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
  output$ind_2D <- renderEcharts4r({
    modelo <- modelo.pca()
    
    ejes    <- isolate(input$slider_ejes)
    ind.cos <- isolate(input$ind_cos) * 0.01
    ind.col <- isolate(input$col_pca_ind)
    cos.col <- isolate(input$col_ind_cos)
    titulos <- c(tr("bienr", updateData$idioma), tr("malr", updateData$idioma))
    
    if(is.null(modelo)) {
      return(NULL)
    } else {
      cod <- paste0("hcpcaind(modelo, c(", paste(ejes, collapse = ", "), 
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
      cod <- paste0("plotly_pcaind(modelo, c(", paste(ejes, collapse = ", "), 
                    "), '", ind.col, "', ", ind.cos, ", '", cos.col, 
                    "', c('", titulos[1], "', '", titulos[2], "'))")
      updateAceEditor(session, "fieldCodeInd3D", value = cod)
      plotly_pcaind(modelo, c(1, 2, 3), ind.col, ind.cos, cos.col, titulos)
    }
  })
  
  #' Plot PCA 2D (variables)
  output$var_2D <- renderEcharts4r({
    modelo  <- modelo.pca()
    
    ejes    <- isolate(input$slider_ejes)
    var.cos <- isolate(input$var_cos) * 0.01
    var.col <- isolate(input$col_pca_var)
    cos.col <- isolate(input$col_var_cos)
    titulos <- c(tr("bienr", updateData$idioma), tr("malr", updateData$idioma))
    
    if(is.null(modelo)) {
      return(NULL)
    } else {
      cod <- paste0("hcpcavar(modelo, c(", paste(ejes, collapse = ", "), 
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
      cod <- paste0("plotly_pcavar(modelo, c(", paste(ejes, collapse = ", "), 
                    "), '", var.col, "', ", var.cos, ", '", cos.col, 
                    "', c('", titulos[1], "', '", titulos[2], "'))")
      updateAceEditor(session, "fieldCodeVar3D", value = cod)
      plotly_pcavar(modelo, c(1, 2, 3), var.col, var.cos, cos.col, titulos)
    }
  })
  
  #' Plot PCA 2D (Biplot)
  output$bi_2D <- renderEcharts4r({
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
      cod <- paste0("hcpcabi(modelo, c(", paste(ejes, collapse = ", "), 
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
      cod <- paste0("plotly_pcabi(modelo, c(", paste(ejes, collapse = ", "), 
                    "), '", ind.col, "', '", var.col, "', ", ind.cos, ", ", 
                    var.cos, ", '", ind.cos.col, "', '", var.cos.col, 
                    "', c('", titulos[1], "', '", titulos[2], "'))")
      updateAceEditor(session, "fieldCodeBi3D", value = cod)
      plotly_pcabi(modelo, c(1, 2, 3), ind.col, var.col, ind.cos, 
                   var.cos, ind.cos.col, var.cos.col, titulos)
    }
  })
  
  #' Plot K-medias (Resultados numéricos)
  output$txtpca <- renderPrint(print(modelo.pca()))
}
    
## To be copied in the UI
# mod_acp_ui("acp_ui_1")
    
## To be copied in the server
# callModule(mod_acp_server, "acp_ui_1")
 
