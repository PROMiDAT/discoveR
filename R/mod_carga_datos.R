#' carga_datos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_carga_datos_ui <- function(id) {
  ns <- NS(id)
  tagList(
    col_5(tabBox(
      title = NULL, width = 12,
      tabPanel(
        title = labelInput("cargar"), width = 12, solidHeader = FALSE,
        collapsible = FALSE, collapsed = FALSE,
        checkboxInput(ns('header'), labelInput("header"), value = T),
        checkboxInput(ns('rowname'), labelInput("Rownames"), value = T),
        radioButtons(
          ns('sep'), labelInput("separador"), inline = T,
          choiceNames = c(';', ',', 'TAB'), choiceValues = c(';', ',', '\t')
        ),
        radioButtons(ns('dec'), labelInput("separadordec"), c(',', '.'), inline = T),
        radioSwitch(ns("deleteNA"), "eliminana", c("eliminar", "imputar")),
        fileInput(
          ns('archivo'), labelInput("cargarchivo"), width = "100%",
          placeholder = "", buttonLabel = labelInput("subir"),
          accept = c('text/csv', '.csv', '.txt')), hr(),
        actionButton(ns("loadButton"), labelInput("cargar"), width = "100%"),
        hr(), codigo.monokai(ns("fieldCodeData"), height = "10vh")),
      
      tabPanel(
        title = labelInput("trans"), width = 12, solidHeader = FALSE,
        collapsible = FALSE, collapsed = FALSE,
        uiOutput(ns('transData')), hr(),
        actionButton(ns('transButton'), labelInput("aplicar"), width = "100%"),
        hr(), codigo.monokai(ns("fieldCodeTrans"), height = "10vh"))
    )),
    
    col_7(
      box(
        title = labelInput("data"), status = "primary", width = 12,
        solidHeader = TRUE, collapsible = TRUE,
        withLoader(DT::dataTableOutput(ns('tabladatos')), 
                   type = "html", loader = "loader4"), hr(),
        downloadButton(ns("downloaDatos"), labelInput("descargar"), style = "width:100%")
      )
    )
  )
}
    
#' carga_datos Server Function
#' @keywords internal
mod_carga_datos_server <- function(input, output, session, updateData) {
  ns <- session$ns
  
  selectInputTrans <- function(datos, var, idioma = "es") {
    tags$select(
      id = ns(paste0("sel", var)),
      tags$option(value = "categorico", tr("categorico", idioma)),
      if(class(datos[, var]) %in% c("numeric", "integer")) {
        tags$option(value = "numerico", tr("numerico", idioma), 
                    selected = 'selected')
      } else {
        tags$option(value = "numerico", tr("numerico", idioma))
      },
      tags$option(value = "disyuntivo", tr("disyuntivo", idioma))
    )
  }
  
  #' Descarga tabla de datos
  output$downloaDatos <- downloadHandler(
    filename = function() {
      input$archivo$name
    },
    content = function(file) {
      write.csv(updateData$datos, file, row.names = input$rowname)
    }
  )
  
  #' Load Button Function
  observeEvent(input$loadButton, {
    rowname    <- isolate(input$rowname)
    ruta       <- isolate(input$archivo)
    sep        <- isolate(input$sep)
    dec        <- isolate(input$dec)
    encabezado <- isolate(input$header)
    deleteNA   <- isolate(input$deleteNA)
    tryCatch({
      codigo <- code.carga(rowname, ruta$name, sep, dec, encabezado, deleteNA)
      updateAceEditor(session, "fieldCodeData", value = codigo)
      
      updateData$originales <- carga.datos(
        rowname, ruta$datapath, sep, dec, encabezado, deleteNA)
      if(ncol(updateData$originales) <= 1) {
        showNotification(
          "ERROR: Check Separators", duration = 10, type = "error")
        updateData$originales <- NULL
        updateData$datos <- NULL
      } else {
        updateData$datos <- updateData$originales
      }
    }, error = function(e) {
      updateData$datos <- NULL
      updateData$originales <- NULL
      showNotification(paste0("ERROR al cargar datos: ", e), type = "error")
    })
  })
  
  #' Update data on table
  output$tabladatos <- DT::renderDataTable({
    datos  <- updateData$datos
    tipos  <- c(
      tr("numerico",   isolate(updateData$idioma)),
      tr("categorico", isolate(updateData$idioma))
    )

    tryCatch({
      nombre.columnas <- c("ID", colnames(datos))
      tipo.columnas <- sapply(colnames(datos), function(i)
        ifelse(class(datos[,i]) %in% c("numeric", "integer"),
               paste0("<span data-id='numerico'>", tipos[1], "</span>"),
               paste0("<span data-id='categorico'>", tipos[2], "</span>")))
      sketch = htmltools::withTags(table(
        tableHeader(nombre.columnas),
        tags$tfoot(
          tags$tr(tags$th(), lapply(tipo.columnas, function(i) 
            tags$th(shiny::HTML(i))))
        )
      ))
      DT::datatable(
        datos, selection = 'none', editable = TRUE,  container = sketch,
        options = list(dom = 'frtip', scrollY = "40vh")
      )
    }, error = function(e) {
      showNotification(paste0("ERROR al mostrar datos: ", e), type = "error")
      return(NULL)
    })
  }, server = T)
  
  #' Update Transform Table
  output$transData = renderUI({
    datos <- updateData$originales
    idioma <- updateData$idioma
    
    res <- list(fluidRow(
      column(4, tags$span(tags$b("Variable"))),
      column(5, tags$b(tr("tipo", idioma))),
      column(3, tags$b(tr("activa", idioma))),
    ), hr(style = paste0("margin-top: 10px; margin-bottom: 10px;", 
                         "border-top: 1px solid black;")))
    
    if(!is.null(datos) && ncol(datos) > 0) {
      res <- list(res, lapply(colnames(datos), function(x) {
        list(fluidRow(
          column(4, tags$span(x)),
          column(5, selectInputTrans(datos, x, idioma)),
          column(3, tags$input(type = "checkbox", id = ns(paste0("del", x)), 
                               checked = T))
        ), hr(style = "margin-top: 10px; margin-bottom: 10px"))
      }))
    }
    
    res <- tags$div(
      style = "height: 40vh; overflow-y: scroll;",
      do.call(tagList, res)
    )
    return(res)
  })
  
  #' Transform Button Function
  observeEvent(input$transButton, {
    datos <- updateData$originales
    cod = ""
    updateAceEditor(session, "fieldCodeTrans", value = cod)
    for (var in colnames(datos)) {
      if(!input[[paste0("del", var)]]) {
        datos[, var] <- NULL
        cod <- paste0(cod, "datos[['", var, "']] <- NULL\n")
        
      } else {
        if(input[[paste0("sel", var)]] == "categorico" &
           class(datos[, var]) %in% c("numeric","integer")) {
          datos[, var] <- as.factor(datos[, var])
          cod <- paste0(cod, code.trans(var, "categorico"))
        }
        if(input[[paste0("sel", var)]] == "numerico" &
           !(class(datos[, var]) %in% c("numeric","integer"))) {
          datos[, var] <- as.numeric(datos[, var])
          cod <- paste0(cod, code.trans(var, "numerico"))
        }
        if(input[[paste0("sel", var)]] == "disyuntivo") {
          datos <- datos.disyuntivos(datos, var)
          datos[, var] <- NULL
          cod <- paste0(cod, code.trans(var, "disyuntivo"))
        }
      }
    }
    
    updateAceEditor(session, "fieldCodeTrans", value = cod)
    updateData$datos <- datos
  })
}
    
## To be copied in the UI
# mod_carga_datos_ui("carga_datos_ui_1")
    
## To be copied in the server
# callModule(mod_carga_datos_server, "carga_datos_ui_1")
 
