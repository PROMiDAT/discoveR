#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import rlang
#' @import shiny
#' @import loadeR
#' @import shinyAce
#' @import FactoMineR
#' @import shinydashboardPlus
#' @importFrom shinydashboard dashboardBody menuItem menuSubItem sidebarMenu tabBox tabItem tabItems
#' @importFrom shinyjs useShinyjs show hide addClass removeClass
#' @importFrom stats cor cutree hclust kmeans median na.omit
#' @importFrom utils read.table write.csv
#' @noRd
#' 
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    dashboardPage(
      title = "PROMiDAT - discoveR",
      dashboardHeader(
        title = HTML(paste0(
          '<span class = "logo-lg">
            <a href = "https://promidat.com" target = "_blank">
              <img src = "img/logo.png" width = "100%" style = "padding-top:2px; padding-bottom:6px;">
            </a>
          </span>',
          '<img src= "img/logo_small.png" height = 50%, width = "120%">'
        )),
        controlbarIcon = icon("gears")
      ),
      
      dashboardSidebar(
        sidebarMenu(
          id = "principal",
          tags$div(style="padding-top:10px;"),
          menuItem(labelInput("data"), tabName = "cargar",
                   icon = icon("database")),
          menuItem(labelInput("basi"), tabName = "parte1",
                   icon = icon("table-list"),
            menuSubItem(labelInput("resu"), "resumen",
                        icon = icon("arrow-down-1-9")),
            menuSubItem(labelInput("norm"), "normalidad",
                        icon = icon("chart-bar")),
            menuSubItem(labelInput("disp"), "dispersion",
                        icon = icon("chart-line")),
            menuSubItem(labelInput("dist"), "distribucion",
                        icon = icon("chart-area")),
            menuSubItem(labelInput("corr"), "correlacion",
                        icon = icon("table"))
          ),
          menuItem(labelInput("acp"), tabName = "acp", 
                   icon = icon("chart-pie")),
          menuItem(labelInput("afc"), tabName = "afc", 
                   icon = icon("envelope")),
          menuItem(labelInput("afcm"), tabName = "afcm", 
                   icon = icon("envelopes-bulk")),
          menuItem(labelInput("jerarquico"), tabName = "cj",
                   icon = icon("sitemap")),
          menuItem(labelInput("kmedias"), tabName = "kmedias",
                   icon = icon("object-group")),
          menuItem(labelInput("acerca"), tabName = "acercaDe",
                   icon = icon("info")),
          hr(),
          menu.idioma(),
          hr(),
          img(src = "img/discoveR.png", style = 
                "margin-left: auto;margin-right: auto;display: block;width: 80%;"),
          tags$div(style = "display:none;",
                   sliderInput(inputId = "aux", min = 2, value = 2,
                               label = "Cantidad de Clusters", max = 10),
                   colourpicker::colourInput(
                     "auxColor", NULL, value = "red", allowTransparent = T)
          )
        )
      ),
      
      dashboardBody(
        tabItems(
          
          # Carga de Datos
          tabItem(tabName = "cargar", mod_carga_datos_ui(
            "carga_datos_ui_1", labelInput('data'), paquete = "discoveR")),
          
          # Resumen Numérico
          tabItem(tabName = "resumen", loadeR::mod_r_numerico_ui("r_numerico_ui_1")),
          
          # Test de Normalidad
          tabItem(tabName = "normalidad", loadeR::mod_normal_ui("normal_ui_1")),
          
          # Dispersión
          tabItem(tabName = "dispersion",
                  loadeR::mod_dispersion_ui("dispersion_ui_1")),
          
          # Distribuciones
          tabItem(tabName = "distribucion", 
                  loadeR::mod_distribuciones_ui("distribuciones_ui_1")),
          
          # Correlaciones
          tabItem(tabName = "correlacion", 
                  loadeR::mod_correlacion_ui("correlacion_ui_1")),
          
          # ACP
          tabItem(tabName = "acp", mod_acp_ui("acp_ui_1")),
          
          # AFC
          tabItem(tabName = "afc", mod_afc_ui("afc_ui_1")),
          
          # AFCM
          tabItem(tabName = "afcm", mod_afcm_ui("afcm_ui_1")),
          
          # Clusterización Jerarquica
          tabItem(tabName = "cj", mod_cj_ui("cj_ui_1")),
          
          # Kmedias
          tabItem(tabName = "kmedias", mod_kmedias_ui("kmedias_ui_1")),
          
          # Acerca De
          tabItem(tabName = "acercaDe", mod_acercade_ui("acercade_ui_1"))
        )
      ),
      
      dashboardControlbar(
        width = 500,
        div(
          style = "margin-right: 15px; margin-left: 15px;", 
          h3(labelInput('code')), hr(), 
          codigo.monokai("fieldCode", height = "70vh"),
          downloadButton("btn_code", NULL, style = "width: 100%;")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  
  add_resource_path('www', app_sys('app/www'))
  add_resource_path('img', app_sys('app/img'))
  add_resource_path('lang', app_sys('app/lang'))
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'discoveR'
    ),

    shinyjs::useShinyjs()
  )
}

