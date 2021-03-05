#' cj UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cj_ui <- function(id) {
  ns <- NS(id)
  
  title_cj <- tags$div(
    class = "multiple-select-var", style = "width: 550px;",
    conditionalPanel(
      condition = "input.tabjerar == 'tabBar'",
      fluidRow(
        col_5(radioSwitch(ns("scaleBar"), NULL, list("ori", "porc"), c(F, T))),
        col_7(selectInput(ns("selBar"), NULL, ""))
      )
    ),
    conditionalPanel(
      condition = "input.tabjerar == 'tabMapa'",
      radioSwitch(ns("plotModeCJ"), NULL, list("2D", "3D"))
    ),
    conditionalPanel(
      condition = "input.tabjerar == 'tabVert'",
      tags$div(
        style = "float: right;",
        radioSwitch(ns("scaleVert"), NULL, list("ori", "res"), c(F, T))
      )
    )
  )
  
  opts_cj <- tabsOptions(
    widths = c(100, 100), heights = c(70, 50),
    tabs.content = list(
      list(options.run(ns("run_hc")), tags$hr(style = "margin-top: 0px;"),
           fluidRow(
             style = "margin-left: 0px; margin-right: 0px",
             col_3(color.input(ns("hcColor"))),
             col_9(
               radioSwitch(ns("cj.scale"), NULL, c("centrar", "nocentrar")),
               sliderInput(
                 ns("cant.cluster"), labelInput("cantcluster"), 2, 10, 2),
               selectInput(
                 ns("sel.dist.method"),  labelInput("metododist"),
                 c("euclidean", "maximum", "manhattan", "canberra", 
                   "binary", "minkowski")),
               selectInput(
                 ns("sel.hc.method"), labelInput("selmetodo"),
                 c("ward.D2", "single", "complete", "average"))
             )
           ), hr(), actionButton(
             ns("CJbtn"), labelInput("agregarcluster"), width = "100%"), hr()
      ),
      list(
        codigo.monokai(ns("fieldCodeModelo"), height = "15vh"),
        lapply(c("Inercia", "Dendo", "Mapa", "Horiz", 
                 "Vert", "Radar", "Bar"), function(i) {
                   conditionalPanel(
                     condition = paste0("input.tabjerar == 'tab", i, "'"),
                     codigo.monokai(ns(paste0("fieldCode", i)), height = "15vh"))
                 })
      )
    )
  )
  
  tagList(
    tabBoxPrmdt(
      id = "tabjerar", title = title_cj, opciones = opts_cj,
      tabPanel(
        title = labelInput("inercia"), value = "tabInercia",
        echarts4rOutput(ns("cj_inercia"), height = "75vh")
      ),
      tabPanel(
        title = labelInput("dendograma"), value = "tabDendo",
        withLoader(plotlyOutput(ns("cj_dendrograma"), height = "75vh"), 
                   type = "html", loader = "loader4")
      ),
      tabPanel(
        title = labelInput("mapa"), value = "tabMapa",
        echarts4rOutput(ns("cj_mapa"), height = "75vh")
      ),
      tabPanel(
        title = labelInput("horizontal"), value = "tabHoriz",
        echarts4rOutput(ns("cj_horiz"), height = "75vh")
      ),
      tabPanel(
        title = labelInput("vertical"), value = "tabVert",
        echarts4rOutput(ns("cj_vert"), height = "75vh")
      ),
      tabPanel(
        title = labelInput("radar"), value = "tabRadar",
        echarts4rOutput(ns("cj_radar"), height = "75vh")
      ),
      tabPanel(
        title = labelInput("interpretacioncat"), value = "tabBar",
        echarts4rOutput(ns("cj_cat"), height = "75vh")
      ),
      tabPanel(
        title = labelInput("resultados"), value = "salida.cj",
        div(style = "height: 75vh;overflow-y: scroll;", 
            withLoader(verbatimTextOutput(ns("txtcj")), 
                       type = "html", loader = "loader4"))
      )
    )
  )
}
    
#' cj Server Function
#' @keywords internal
mod_cj_server <- function(input, output, session, updateData) {
  ns <- session$ns
  
  cj_colors <- rv(colors = NULL)
  
  #' Mostrar Colores (Cluster jerarquico)
  observeEvent(input$cant.cluster, {
    cant <- input$cant.cluster
    if(!is.null(cant)) {
      mostrar.colores("hcColor", cant)
    }
  })
  
  #' Generate hclust on load data
  modelo.cj <- reactive({
    input$run_hc
    data <- var.numericas(updateData$datos)
    
    centrar      <- isolate(input$cj.scale)
    cant.cluster <- isolate(input$cant.cluster)
    dist.method  <- isolate(input$sel.dist.method)
    hc.method    <- isolate(input$sel.hc.method)
    
    if(nrow(data) == 0) {
      return(NULL)
    } else if(nrow(data) > 8000) {
      showNotification(tr("longerror"), duration = 30, type = "warning")
      return(NULL)
    } else {
      if(centrar) {
        modelo <- hclust(dist(as.data.frame(scale(data)), method = dist.method), 
                         method = hc.method)
      } else {
        modelo <- hclust(dist(data, method = dist.method), method = hc.method)
      }
      clusters <- as.factor(cutree(modelo, k = cant.cluster))
      centros  <- calc.centros(data, clusters)
      cj_colors$colors <- unname(sapply(levels(clusters), function(i) 
        isolate(input[[paste0("hcColor", i)]])))
      
      cod <- code.cj(centrar, dist.method, hc.method, cant.cluster)
      updateAceEditor(session, "fieldCodeModelo", value = cod)
      return(list(modelo = modelo, clusters = clusters, centros = centros))
    }
  })
  
  #' Update on load data
  observeEvent(updateData$datos, {
    datos       <- updateData$datos
    numericos   <- var.numericas(datos)
    categoricos <- var.categoricas(datos)
    
    updateSelectInput(session, "selBar", choices = colnames(categoricos))
  })
  
  #' Inertia plot
  output$cj_inercia <- renderEcharts4r({
    if(is.null(modelo.cj())) return(NULL)
    
    centrar <- isolate(input$cj.scale)
    titulos <- c(
      tr("inercia", updateData$idioma), 
      tr("inerciainter", updateData$idioma),
      tr("inerciaintra", updateData$idioma)
    )
    
    tryCatch({
      data <- var.numericas(updateData$datos)
      if(centrar) {data <- as.data.frame(scale(data))}
      
      inercias <- data.frame(
        total = inercia.total(data),
        inter.clase = BP(data, modelo.cj()$clusters)
      )
      inercias$intra.clase <- inercias$total - inercias$inter.clase
      
      cod <- code.inercia(titulos)
      updateAceEditor(session, "fieldCodeInercia", value = cod)
      
      e_inercia(inercias, titulos)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  
  #' Generate dendrogram
  output$cj_dendrograma <- renderPlotly({
    if(is.null(modelo.cj())) return(NULL)
    
    tryCatch({
      modelo   <- modelo.cj()$modelo
      clusters <- modelo.cj()$clusters
      
      p <- gg_dendrograma(modelo, clusters, c("gray", isolate(cj_colors$colors)))
      
      cod <- code.dendro(isolate(cj_colors$colors))
      updateAceEditor(session, "fieldCodeDendo", value = cod)
      
      ggplotly(p, tooltip = c("y", "cluster", "clusters", "label")) %>% 
        layout(showlegend = F, xaxis = list(showline = F), yaxis = list(showline = F)) %>%
        style(textposition = "right") %>% config(displaylogo = F)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  
  #' Plot hclust (Mapa)
  output$cj_mapa <- renderEcharts4r({
    modelo <- PCA(var.numericas(updateData$datos))
    if(is.null(modelo)) return(NULL)
    
    if(input$plotModeCJ) {
      cod <- paste0(
        "modelo.pca <- PCA(var.numericas(datos))\n",
        "e_mapa(modelo.pca, modelo.cj$clusters, c('",
        paste(isolate(cj_colors$colors), collapse = "', '"), "'))\n")
      updateAceEditor(session, "fieldCodeMapa", value = cod)
      
      e_mapa(modelo, modelo.cj()$clusters, isolate(cj_colors$colors))
    } else {
      cod <- paste0(
        "modelo.pca <- PCA(var.numericas(datos))\n",
        "e_mapa_3D(modelo.pca, modelo.cj$clusters, c('",
        paste(isolate(cj_colors$colors), collapse = "', '"), "'))\n")
      updateAceEditor(session, "fieldCodeMapa", value = cod)
      
      e_mapa_3D(modelo, modelo.cj()$clusters, isolate(cj_colors$colors))
    }
  })
  
  #' Plot hclust (Horizontal)
  output$cj_horiz <- renderEcharts4r({
    if(is.null(modelo.cj())) return(NULL)
    
    centros <- modelo.cj()$centros$real
    cod.centros <- "modelo.cj$centros$real"
    tryCatch({
      cod <- paste0("e_horiz(", cod.centros, ", c('",
                    paste(isolate(cj_colors$colors), collapse = "', '"), "'))")
      updateAceEditor(session, "fieldCodeHoriz", value = cod)
      
      e_horiz(centros, isolate(cj_colors$colors))
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  
  #' Plot hclust (Vertical)
  output$cj_vert <- renderEcharts4r({
    if(is.null(modelo.cj())) return(NULL)
    
    centros <- data.frame(apply(modelo.cj()$centros$real, 2, function(x) x / max(abs(x)) * 100))
    cod.centros <- "data.frame(apply(modelo.cj$centros$real, 2, function(x) x / max(abs(x)) * 100))"
    if(input$scaleVert == "FALSE") {
      centros <- modelo.cj()$centros$real
      cod.centros <- "modelo.cj$centros$real"
    }
    
    tryCatch({
      cod <- paste0("e_vert(", cod.centros, ", c('",
                    paste(isolate(cj_colors$colors), collapse = "', '"), "'))")
      updateAceEditor(session, "fieldCodeVert", value = cod)
      
      e_vert(centros, isolate(cj_colors$colors))
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  
  #' Plot hclust (Radar)
  output$cj_radar <- renderEcharts4r({
    if(is.null(modelo.cj())) return(NULL)
    
    tryCatch({
      cod <- paste0("e_radar(modelo.cj$centros$porcentual, c('",
                    paste(isolate(cj_colors$colors), collapse = "', '"), "'))")
      updateAceEditor(session, "fieldCodeRadar", value = cod)
      
      e_radar(modelo.cj()$centros$porcentual, isolate(cj_colors$colors))
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  
  #' Plot hclust (Categórico)
  output$cj_cat <- renderEcharts4r({
    if(is.null(modelo.cj())) return(NULL)
    
    var <- input$selBar
    escalar <- input$scaleBar == "TRUE"
    
    validate(need(var != "", tr("errorcat", isolate(updateData$idioma))))
    
    tryCatch({
      cod <- paste0(
        "e_cat(modelo.cj$clusters, datos[['", var, "']], c('",
        paste(isolate(cj_colors$colors), collapse = "', '"), "'), ", escalar, ")")
      updateAceEditor(session, "fieldCodeBar", value = cod)
      
      e_cat(modelo.cj()$clusters, updateData$datos[, var], 
            isolate(cj_colors$colors), escalar)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  
  #' Plot K-medias (Resultados numéricos)
  output$txtcj <- renderPrint({
    if(is.null(modelo.cj())) return(NULL)
    print(modelo.cj())
  })
  
  #' Agregar Clusteres a tabla de datos (CJ)
  observeEvent(input$CJbtn, {
    clusters <- isolate(modelo.cj()$clusters)
    nom <- ifelse(updateData$idioma == "es", "CJ.", "HC.")
    cluster.var <- as.factor(paste0(nom, clusters))
    
    updateData$datos[[paste0(nom, length(unique(clusters)))]] <- cluster.var
    showNotification(tr("msjclusters"), duration = 5, type = "message")
  })
}
    
## To be copied in the UI
# mod_cj_ui("cj_ui_1")
    
## To be copied in the server
# callModule(mod_cj_server, "cj_ui_1")
 
