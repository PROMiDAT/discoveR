#' The application server-side
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#' DO NOT REMOVE.
#' @import shiny
#' @import shinycustomloader
#' @import plotly
#' @importFrom DT tableHeader formatStyle
#' @keywords internal
#' 
app_server <- function( input, output, session ) {
  
  ##################################  Options  ################################
  options(shiny.maxRequestSize = 200*1024^2)
  options(
    DT.options = list(
      aLengthMenu = c(10, 30, 50), iDisplayLength = 10,
      language = list(
        search = shiny::HTML('<i class="fa fa-search"></i>'),
        info = "", emptyTable = "", zeroRecords = "",
        paginate = list(
          "previous" = shiny::HTML('<i class="fa fa-backward"></i>'),
          "next"     = shiny::HTML('<i class="fa fa-forward"></i>'),
          "first"    = shiny::HTML('<i class="fa fa-fast-backward"></i>'), 
          "last"     = shiny::HTML('<i class="fa fa-fast-forward"></i>')))
    )
  )
  
  onStop(function() stopApp())
  
  ##################################  Variables  ##############################
  updateData <- rv(datos = NULL, originales = NULL, idioma = NULL)
  codedioma  <- rv(idioma = "es", code = list())
  
  ###################################  Update  ################################
  # Update on Language
  observeEvent(input$idioma, {
    codedioma$idioma = input$idioma
    etiquetas <- names(translation)
    updateLabelInput(session, etiquetas, tr(etiquetas, input$idioma))
  })
  
  # Update Code
  observeEvent(c(codedioma$code, input$idioma), {
    codigo <- codedioma$code
    lg <- input$idioma
    
    keys <- names(translation)
    keys <- keys[grepl("doc", keys, fixed = TRUE)]
    
    for (k in keys) {
      codigo <- gsub(paste0(" ", k, "\n"),
                     paste0(" ", tr(k, idioma = lg), "\n"), codigo, fixed = T)
    }
    
    codigo.completo <- paste0(
      "library(XLConnect)\n", "library(caret)\n", "library(echarts4r)\n",
      "library(loadeR)\n", "library(discoveR)\n\n"
    )
    for (cod in codigo) {
      codigo.completo <- paste0(codigo.completo, "\n", cod)
    }
    updateAceEditor(session, "fieldCode", value = codigo.completo)
  })
  
  # Enable/disable on load data
  observe({
    datos <- updateData$datos
    n.neg <- sum(var.numericas(datos) < 0)
    n.num <- ncol(var.numericas(datos))
    n.cat <- ncol(var.categoricas(datos))
    
    element <- "#sidebarItemExpanded li"
    menu.num <- c(
      " a[data-value=acp]", " a[data-value=cj]", 
      " a[data-value=kmedias]", " a[data-value=reporte]")
    
    menu.cat <- c(" a[data-value=afcm]")
    
    if(is.null(datos)) {
      addClass(class = "disabled", selector = paste0(element, "[class^=treeview]"))
    } else {
      removeClass(class = "disabled", selector = paste0(element, "[class^=treeview]"))
    }
    
    if(is.null(datos) || n.num < 2 || n.neg > 0) {
      addClass(class = "disabled", selector = paste0(element, " a[data-value=afc]"))
    } else {
      removeClass(class = "disabled", selector = paste0(element, " a[data-value=afc]"))
    }

    lapply(menu.num, function(i) {
      if(is.null(datos) || n.num < 2) {
        addClass(class = "disabled", selector = paste0(element, i))
      } else {
        removeClass(class = "disabled", selector = paste0(element, i))
      }
    })
    
    lapply(menu.cat, function(i) {
      if(is.null(datos) || n.cat < 2) {
        addClass(class = "disabled", selector = paste0(element, i))
      } else {
        removeClass(class = "disabled", selector = paste0(element, i))
      }
    })
  })
  
  ###################################  Modules  ###############################
  
  mod_carga_datos_server("carga_datos_ui_1", updateData, NULL, codedioma, "discoveR")
  
  loadeR::mod_r_numerico_server(        "r_numerico_ui_1", updateData, codedioma)
  loadeR::mod_normal_server(                "normal_ui_1", updateData, codedioma)
  loadeR::mod_dispersion_server(        "dispersion_ui_1", updateData, codedioma)
  loadeR::mod_distribuciones_server("distribuciones_ui_1", updateData, codedioma)
  loadeR::mod_correlacion_server(      "correlacion_ui_1", updateData, codedioma)
  
  mod_acp_server(        "acp_ui_1", updateData, codedioma)
  mod_afc_server(        "afc_ui_1", updateData, codedioma)
  mod_afcm_server(      "afcm_ui_1", updateData, codedioma)
  mod_cj_server(          "cj_ui_1", updateData, codedioma)
  mod_kmedias_server("kmedias_ui_1", updateData, codedioma)
}
