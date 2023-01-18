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
    heights = 70, tabs.content = list(
      list(options.run(ns("run_hc")), tags$hr(style = "margin-top: 0px;"),
           fluidRow(
             style = "margin-left: 0px; margin-right: 0px",
             col_3(color.input(ns("hcColor"))),
             col_9(
               radioSwitch(ns("cj.scale"), NULL, c("centrar", "nocentrar")),
               sliderInput(
                 ns("cant.cluster"), labelInput("cantcluster"), 2, 10, 2),
               fluidRow(
                 col_6(selectInput(
                   ns("sel.dist.method"),  labelInput("metododist"),
                   c("euclidean", "maximum", "manhattan", "canberra",
                     "binary", "minkowski"))),
                 col_6(selectInput(
                   ns("sel.hc.method"), labelInput("selmetodo"),
                   c("ward.D2", "single", "complete", "average")))
               ),
               conditionalPanel(
                 condition = paste0("input.tabjerar == 'tabMapa'"),
                 radioSwitch(ns("switch_label"), "selabelind", c("si", "no"), val.def = F))
             )
           ), hr(), actionButton(
             ns("CJbtn"), labelInput("agregarcluster"), width = "100%"), hr()
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
mod_cj_server <- function(id, updateData, codedioma) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    cj_colors <- rv(colors = NULL)
    
    # Mostrar Colores (Cluster jerarquico)
    observeEvent(input$cant.cluster, {
      cant <- input$cant.cluster
      if(!is.null(cant)) {
        mostrar.colores("hcColor", cant)
      }
    })
    
    # Generate hclust on load data
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
        
        cod <- paste0("### dochclustmodel\n",
                      code.cj(centrar, dist.method, hc.method, cant.cluster))
        isolate(codedioma$code <- append(codedioma$code, cod))
        return(list(modelo = modelo, clusters = clusters, centros = centros))
      }
    })
    
    # Update on load data
    observeEvent(updateData$datos, {
      datos       <- updateData$datos
      numericos   <- var.numericas(datos)
      categoricos <- var.categoricas(datos)
      
      updateSelectInput(session, "selBar", choices = colnames(categoricos))
    })
    
    # Inertia plot
    output$cj_inercia <- renderEcharts4r({
      if(is.null(modelo.cj())) return(NULL)
      
      centrar <- isolate(input$cj.scale)
      titulos <- c(
        tr("inercia", codedioma$idioma), 
        tr("inerciainter", codedioma$idioma),
        tr("inerciaintra", codedioma$idioma)
      )
      
      tryCatch({
        data <- var.numericas(updateData$datos)
        if(centrar) {data <- as.data.frame(scale(data))}
        
        inercias <- data.frame(
          total = inercia.total(data),
          inter.clase = BP(data, modelo.cj()$clusters)
        )
        inercias$intra.clase <- inercias$total - inercias$inter.clase
        
        cod <- paste0("### dochclustinercia\n", code.inercia(titulos))
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        e_inercia(inercias, titulos)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
    
    # Generate dendrogram
    output$cj_dendrograma <- renderPlotly({
      if(is.null(modelo.cj())) return(NULL)
      
      tryCatch({
        modelo <- modelo.cj()$modelo
        k      <- isolate(input$cant.cluster)
        
        p <- gg_dendrograma(modelo, k, isolate(cj_colors$colors))
        
        cod <- paste0("### dochclustdend\n", code.dendro(k, isolate(cj_colors$colors)))
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        ggplotly(p, tooltip = c("y", "cluster", "clusters", "label")) %>% 
          layout(showlegend = F, xaxis = list(showline = F), yaxis = list(showline = F)) %>%
          style(textposition = "right") %>% config(displaylogo = F)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
    
    # Plot hclust (Mapa)
    output$cj_mapa <- renderEcharts4r({
      modelo <- PCA(var.numericas(updateData$datos))
      if(is.null(modelo)) return(NULL)
      
      colores  <- isolate(cj_colors$colors)
      clusters <- modelo.cj()$clusters
      addetq   <- isolate(input$switch_label)
      
      if(input$plotModeCJ) {
        cod <- paste0(
          "### dochclustmapa2d\n",
          "modelo.pca <- PCA(var.numericas(datos))\n",
          "e_mapa(modelo.pca, modelo.cj$clusters, c('",
          paste(colores, collapse = "', '"), "'), etq = ", addetq, ")\n")
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        e_mapa(modelo, clusters, colores, etq = addetq)
      } else {
        cod <- paste0(
          "### dochclustmapa3d\n",
          "modelo.pca <- PCA(var.numericas(datos))\n",
          "e_mapa_3D(modelo.pca, modelo.cj$clusters, c('",
          paste(colores, collapse = "', '"), "'), etq = ", addetq, ")\n")
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        e_mapa_3D(modelo, clusters, colores, etq = addetq)
      }
    })
    
    # Plot hclust (Horizontal)
    output$cj_horiz <- renderEcharts4r({
      if(is.null(modelo.cj())) return(NULL)
      
      centros <- modelo.cj()$centros$real
      cod.centros <- "modelo.cj$centros$real"
      tryCatch({
        cod <- paste0("### dochclusthoriz\ne_horiz(", cod.centros, ", c('",
                      paste(isolate(cj_colors$colors), collapse = "', '"), "'))\n")
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        e_horiz(centros, isolate(cj_colors$colors))
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
    
    # Plot hclust (Vertical)
    output$cj_vert <- renderEcharts4r({
      if(is.null(modelo.cj())) return(NULL)
      
      centros <- data.frame(apply(modelo.cj()$centros$real, 2, function(x) x / max(abs(x)) * 100))
      cod.centros <- "data.frame(apply(modelo.cj$centros$real, 2, function(x) x / max(abs(x)) * 100))"
      if(input$scaleVert == "FALSE") {
        centros <- modelo.cj()$centros$real
        cod.centros <- "modelo.cj$centros$real"
      }
      
      tryCatch({
        cod <- paste0("### dochclustvert\ne_vert(", cod.centros, ", c('",
                      paste(isolate(cj_colors$colors), collapse = "', '"), "'))\n")
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        e_vert(centros, isolate(cj_colors$colors))
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
    
    # Plot hclust (Radar)
    output$cj_radar <- renderEcharts4r({
      if(is.null(modelo.cj())) return(NULL)
      
      tryCatch({
        cod <- paste0("### dochclustradar\ne_radar(modelo.cj$centros$porcentual, c('",
                      paste(isolate(cj_colors$colors), collapse = "', '"), "'))\n")
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        e_radar(modelo.cj()$centros$porcentual, isolate(cj_colors$colors))
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
    
    # Plot hclust (Categórico)
    output$cj_cat <- renderEcharts4r({
      if(is.null(modelo.cj())) return(NULL)
      
      var <- input$selBar
      escalar <- input$scaleBar == "TRUE"
      
      validate(need(var != "", tr("errorcat", isolate(codedioma$idioma))))
      
      tryCatch({
        cod <- paste0(
          "### dochclustcat\ne_cat(modelo.cj$clusters, datos[['", var, "']], c('",
          paste(isolate(cj_colors$colors), collapse = "', '"), "'), ", escalar, ")\n")
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        e_cat(modelo.cj()$clusters, updateData$datos[, var], 
              isolate(cj_colors$colors), escalar)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
    
    # Plot K-medias (Resultados numéricos)
    output$txtcj <- renderPrint({
      if(is.null(modelo.cj())) return(NULL)
      print(modelo.cj())
    })
    
    # Agregar Clusteres a tabla de datos (CJ)
    observeEvent(input$CJbtn, {
      clusters <- isolate(modelo.cj()$clusters)
      nom <- ifelse(codedioma$idioma == "es", "CJ.", "HC.")
      cluster.var <- as.factor(paste0(nom, clusters))
      
      updateData$datos[[paste0(nom, length(unique(clusters)))]] <- cluster.var
      showNotification(tr("msjclusters"), duration = 5, type = "message")
    })
  })
}
    
## To be copied in the UI
# mod_cj_ui("cj_ui_1")
    
## To be copied in the server
# mod_cj_server("cj_ui_1")
 
