#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyAce
#' @import shinydashboard
#' @import shinydashboardPlus
#' @importFrom shinyjs useShinyjs show hide addClass removeClass
#' @importFrom stats cor cutree hclust kmeans median na.omit
#' @importFrom utils read.table write.csv
#' @noRd
#' 
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    shinydashboardPlus::dashboardPagePlus(
      title = "PROMiDAT - discoveR",
      shinydashboard::dashboardHeader(
        title = HTML(paste0(
          '<span class = "logo-lg">
            <a href = "https://promidat.com" target = "_blank">
              <img src = "img/logo.png" width = "100%" style = "padding-top:2px; padding-bottom:6px;">
            </a>
          </span>',
          '<img src= "img/logo_small.png" height = 50%, width = "120%">'
        ))
      ),
      
      dashboardSidebar(
        sidebarMenu(
          id = "principal",
          tags$div(style="padding-top:10px;"),
          menuItem(labelInput("data"), tabName = "cargar",
                   icon = icon("dashboard")),
          menuItem(labelInput("basico"), tabName = "parte1",
                   icon = icon("th-list"),
            menuSubItem(labelInput("resumen"), "resumen",
                        icon = icon("sort-numeric-asc")),
            menuSubItem(labelInput("normalidad"), "normalidad",
                        icon = icon("bar-chart")),
            menuSubItem(labelInput("dispersion"), "dispersion",
                        icon = icon("line-chart")),
            menuSubItem(labelInput("distribucion"), "distribucion",
                        icon = icon("area-chart")),
            menuSubItem(labelInput("correlacion"), "correlacion",
                        icon = icon("table"))
          ),
          menuItem(labelInput("acp"), tabName = "acp", 
                   icon = icon("pie-chart")),
          menuItem(labelInput("jerarquico"), tabName = "cj",
                   icon = icon("sitemap")),
          menuItem(labelInput("kmedias"), tabName = "kmedias",
                   icon = icon("object-group")),
          #menuItem(labelInput("reporte"), tabName = "reporte",
          #         icon = icon("save-file", lib = "glyphicon")),
          menuItem(labelInput("acercade"), tabName = "acercaDe",
                   icon = icon("info")),
          hr(),
          menu.idioma(),
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
          tabItem(tabName = "cargar",  mod_carga_datos_ui("carga_datos_ui_1")),
          
          # Resumen Numérico
          tabItem(tabName = "resumen", mod_r_numerico_ui("r_numerico_ui_1")),
          
          # Test de Normalidad
          tabItem(tabName = "normalidad", mod_normal_ui("normal_ui_1")),
          
          # Dispersión
          tabItem(tabName = "dispersion",
                  mod_dispersion_ui("dispersion_ui_1")),
          
          # Distribuciones
          tabItem(tabName = "distribucion", 
                  mod_distribuciones_ui("distribuciones_ui_1")),
          
          # Correlaciones
          tabItem(tabName = "correlacion", 
                  mod_correlacion_ui("correlacion_ui_1")),
          
          # ACP
          tabItem(tabName = "acp", mod_acp_ui("acp_ui_1")),
          
          # Clusterización Jerarquica
          tabItem(tabName = "cj", mod_cj_ui("cj_ui_1")),
          
          # Kmedias
          tabItem(tabName = "kmedias", mod_kmedias_ui("kmedias_ui_1")),
          
          # Acerca De
          tabItem(tabName = "acercaDe", mod_acercade_ui("acercade_ui_1"))
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

