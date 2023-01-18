#' kmedias UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_kmedias_ui <- function(id) {
  ns <- NS(id)
  
  title_k <- tags$div(
    class = "multiple-select-var", style = "width: 550px;",
    conditionalPanel(
      condition = "input.tabkmedias == 'tabKbar'",
      fluidRow(
        col_5(radioSwitch(ns("scaleKbar"), NULL, list("ori", "porc"), c(F, T))),
        col_7(selectInput(ns("selKbar"), NULL, ""))
      )
    ),
    conditionalPanel(
      condition = "input.tabkmedias == 'tabKmapa'",
      radioSwitch(ns("plotModeK"), NULL, list("2D", "3D"))
    ),
    conditionalPanel(
      condition = "input.tabkmedias == 'tabKvert'",
      tags$div(
        style = "float: right;",
        radioSwitch(ns("scaleKvert"), NULL, list("ori", "res"), c(F, T))
      )
    )
  )
  
  opts_k <- tabsOptions(
    heights = 70, tabs.content = list(
      list(options.run(ns("run_k")), tags$hr(style = "margin-top: 0px;"),
           fluidRow(
             style = "margin-left: 0px; margin-right: 0px",
             col_3(color.input(ns("kColor"))),
             col_9(
               radioSwitch(ns("k.scale"), NULL, c("centrar", "nocentrar")),
               sliderInput(
                 ns("cant.kmeans.cluster"), labelInput("cantcluster"), 2, 10, 2),
               fluidRow(
                 col_4(numericInput(ns("num.nstart"), labelInput("nstart"), 2, step = 10)),
                 col_4(numericInput(ns("num.iter"), labelInput("niter"), 100, step = 100)),
                 col_4(selectInput(ns("sel.algoritmo"), labelInput("algoritmo"), 
                                   c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")))
               ),
               conditionalPanel(
                 condition = paste0("input.tabkmedias == 'tabJambu'"),
                 radioSwitch(ns("radiojambu"), "metcluster", c("jambu", "sil"),
                             c("wss", "silhouette"))),
               conditionalPanel(
                 condition = paste0("input.tabkmedias == 'tabKmapa'"),
                 radioSwitch(ns("switch_label"), "selabelind", c("si", "no"), val.def = F))
             )
           ), hr(), actionButton(
             ns("Kbtn"), labelInput("agregarcluster"), width = "100%"), hr()
      )
    )
  )
  
  tagList(
    tabBoxPrmdt(
      id = "tabkmedias", title = title_k, opciones = opts_k,
      tabPanel(
        title = labelInput("inercia"), value = "tabKinercia",
        echarts4rOutput(ns("k_inercia"), height = "75vh")
      ),
      tabPanel(
        title = labelInput("numcluster"), value = "tabJambu",
        echarts4rOutput(ns('k_jambu'), height = "70vh")
      ),
      tabPanel(
        title = labelInput("mapa"), value = "tabKmapa",
        echarts4rOutput(ns("k_mapa"), height = "75vh")
      ),
      tabPanel(
        title = labelInput("horizontal"), value = "tabKhoriz",
        echarts4rOutput(ns("k_horiz"), height = "75vh")
      ),
      tabPanel(
        title = labelInput("vertical"), value = "tabKvert",
        echarts4rOutput(ns("k_vert"), height = "75vh")
      ),
      tabPanel(
        title = labelInput("radar"), value = "tabKradar",
        echarts4rOutput(ns("k_radar"), height = "75vh")
      ),
      tabPanel(
        title = labelInput("interpretacioncat"), value = "tabKbar",
        echarts4rOutput(ns("k_cat"), height = "75vh")
      ),
      tabPanel(
        title = labelInput("resultados"), value = "salida.k",
        div(style = "height: 75vh;overflow-y: scroll;", 
            withLoader(verbatimTextOutput(ns("txtk")), 
                       type = "html", loader = "loader4"))
      )
    )
  )
}
    
#' kmedias Server Function
#' @keywords internal
mod_kmedias_server <- function(id, updateData, codedioma) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    k_colors <- rv(colors = NULL)
    
    # Mostrar Colores (Cluster jerarquico)
    observeEvent(input$cant.kmeans.cluster, {
      cant <- input$cant.kmeans.cluster
      if(!is.null(cant)) {
        mostrar.colores("kColor", cant)
      }
    })
    
    # Generate K-medias on load data
    modelo.k <- reactive({
      input$run_k
      data <- var.numericas(updateData$datos)
      
      centrar       <- isolate(input$k.scale)
      cant.cluster  <- isolate(input$cant.kmeans.cluster)
      num.nstart    <- as.numeric(isolate(input$num.nstart))
      num.iter      <- as.numeric(isolate(input$num.iter))
      sel.algoritmo <- isolate(input$sel.algoritmo)
      
      if(nrow(data) == 0) {
        return(NULL)
      } else {
        if(centrar) {
          modelo   <- kmeans(
            as.data.frame(scale(data)), centers = cant.cluster, 
            iter.max = num.iter, nstart = num.nstart, algorithm = sel.algoritmo)
        } else {
          modelo   <- kmeans(
            data, centers = cant.cluster, iter.max = num.iter,
            nstart = num.nstart, algorithm = sel.algoritmo)
        }
        
        clusters <- as.factor(modelo$cluster)
        centros  <- calc.centros(data, clusters)
        k_colors$colors <- unname(sapply(levels(clusters), function(i) 
          isolate(input[[paste0("kColor", i)]])))
        
        cod <- paste0("### dockmodel\n",
                      code.k(centrar, cant.cluster, num.iter, num.nstart, sel.algoritmo))
        isolate(codedioma$code <- append(codedioma$code, cod))
        return(list(modelo = modelo, clusters = clusters, centros = centros))
      }
    })
    
    # Update on load data
    observeEvent(updateData$datos, {
      datos       <- updateData$datos
      numericos   <- var.numericas(datos)
      categoricos <- var.categoricas(datos)
      
      updateSelectInput(session, "selKbar", choices = colnames(categoricos))
    })
    
    # Inertia plot
    output$k_inercia <- renderEcharts4r({
      titulos <- c(
        tr("inercia", codedioma$idioma), 
        tr("inerciainter", codedioma$idioma),
        tr("inerciaintra", codedioma$idioma)
      )
      
      tryCatch({
        inercias <- data.frame(
          total       = modelo.k()$modelo$totss,
          inter.clase = modelo.k()$modelo$betweenss,
          intra.clase = modelo.k()$modelo$tot.withinss
        )
        
        cod <- paste0("### dockinertia\n", code.kinercia(titulos))
        isolate(codedioma$code <- append(codedioma$code, cod))
        e_inercia(inercias, titulos)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
    
    # Jambu Elbow
    output$k_jambu <- renderEcharts4r({
      input$run_k
      centrar <- isolate(input$k.scale)
      
      data <- var.numericas(updateData$datos)
      if(nrow(data) > 10000) {
        showNotification(tr("longerror"), duration = 30, type = "warning")
        return(NULL)
      }
      if(centrar) {data <- as.data.frame(scale(data))}
      
      tryCatch({
        if(input$radiojambu == "wss") {
          cod <- "### dockjambu\ne_jambu(datos.aux, 10)\n"
          isolate(codedioma$code <- append(codedioma$code, cod))
          e_jambu(data, 10)
        } else {
          cod <- "### docksilh\ne_silhouette(datos.aux, 10)\n"
          isolate(codedioma$code <- append(codedioma$code, cod))
          e_silhouette(data, 10)
        }
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
    
    # Plot K-medias (Mapa)
    output$k_mapa <- renderEcharts4r({
      modelo <- PCA(var.numericas(updateData$datos))
      if(is.null(modelo)) return(NULL)
      
      colores  <- isolate(k_colors$colors)
      clusters <- modelo.k()$clusters
      addetq   <- isolate(input$switch_label)
      
      if(input$plotModeK) {
        cod <- paste0(
          "### dockmapa2d\n",
          "modelo.pca <- PCA(var.numericas(datos))\n",
          "e_mapa(modelo.pca, modelo.k$clusters, c('",
          paste(colores, collapse = "', '"), "'), etq = ", addetq, ")\n")
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        e_mapa(modelo, clusters, colores, etq = addetq)
      } else {
        cod <- paste0(
          "### dockmapa3d\n",
          "modelo.pca <- PCA(var.numericas(datos))\n",
          "e_mapa_3D(modelo.pca, modelo.k$clusters, c('",
          paste(colores, collapse = "', '"), "'), etq = ", addetq, ")\n")
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        e_mapa_3D(modelo, clusters, colores, etq = addetq)
      }
    })
    
    # Plot K-medias (Horizontal)
    output$k_horiz <- renderEcharts4r({
      centros <- modelo.k()$centros$real
      cod.centros <- "modelo.k$centros$real"
      tryCatch({
        cod <- paste0("### dockhoriz\ne_horiz(", cod.centros, ", 'k_horiz', c('",
                      paste(isolate(k_colors$colors), collapse = "', '"), "'))\n")
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        e_horiz(centros, isolate(k_colors$colors))
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
    
    # Plot K-medias (Vertical)
    output$k_vert <- renderEcharts4r({
      centros <- data.frame(apply(modelo.k()$centros$real, 2, function(x) x / max(abs(x)) * 100))
      cod.centros <- "data.frame(apply(modelo.k$centros$real, 2, function(x) x / max(abs(x)) * 100))"
      if(input$scaleKvert == "FALSE") {
        centros <- modelo.k()$centros$real
        cod.centros <- "modelo.k$centros$real"
      }
      
      tryCatch({
        cod <- paste0("### dockvert\ne_vert(", cod.centros, ", 'k_vertical', c('",
                      paste(isolate(k_colors$colors), collapse = "', '"), "'))\n")
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        e_vert(centros, isolate(k_colors$colors))
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
    
    # Plot K-medias (Radar)
    output$k_radar <- renderEcharts4r({
      tryCatch({
        cod <- paste0("### dockradar\ne_radar(modelo.k$centros$porcentual, 'k_radar', c('",
                      paste(isolate(k_colors$colors), collapse = "', '"), "'))\n")
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        e_radar(modelo.k()$centros$porcentual, isolate(k_colors$colors))
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
    
    # Plot K-medias (Categórico)
    output$k_cat <- renderEcharts4r({
      var <- input$selKbar
      escalar <- input$scaleKbar
      
      validate(need(var != "", tr("errorcat", isolate(codedioma$idioma))))
      
      tryCatch({
        cod <- paste0(
          "### dockcat\ne_cat(modelo.k$clusters, datos[['", var, "']], 'k_cat', c('",
          paste(isolate(k_colors$colors), collapse = "', '"), "'), ", escalar, ")\n")
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        e_cat(modelo.k()$clusters, updateData$datos[, var], 
              isolate(k_colors$colors), escalar)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
    
    # Plot K-medias (Resultados numéricos)
    output$txtk <- renderPrint(print(modelo.k()))
    
    # Agregar Clusteres a tabla de datos (K-medias)
    observeEvent(input$Kbtn, {
      clusters <- isolate(modelo.k()$clusters)
      nom <- ifelse(codedioma$idioma == "es", paste0("Kmedias.", length(unique(clusters))),
                    paste0("Kmeans.", length(unique(clusters))))
      cluster.var <- as.factor(paste0("K", clusters))
      
      updateData$datos[[nom]] <- cluster.var
      showNotification(tr("msjclusters"), duration = 5, type = "message")
    })
  })
}
    
## To be copied in the UI
# mod_kmedias_ui("kmedias_ui_1")
    
## To be copied in the server
# mod_kmedias_server("kmedias_ui_1")
 
