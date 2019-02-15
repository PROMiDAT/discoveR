# UI Script

library(DT)
library(shiny)
library(future)
library(ggplot2)
library(shinyjs)
library(shinyAce)
library(rmarkdown)
library(factoextra)
library(rstudioapi)
library(colourpicker)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)

shinyUI(dashboardPagePlus(
  title="PROMiDAT - discoveR",
  shinydashboardPlus::dashboardHeaderPlus(
    title = tags$a(href="http://promidat.com", target = "_blank",
                   shiny::img(src="Logo2.png", height=55, width="100%",
                       style="padding-top:2px; padding-bottom:6px;"))
  ),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "principal",
      tags$div(style="padding-top:10px;"),
      shinydashboard::menuItem(labelInput("data"), tabName = "cargar",
                               icon = shiny::icon("dashboard")),
      shinydashboard::menuItem(
        labelInput("basico"), tabName = "parte1", 
        icon = shiny::icon("th-list"),
        shinydashboard::menuSubItem(
          labelInput("resumen"), tabName = "resumen", 
          icon = shiny::icon("sort-numeric-asc")),
        shinydashboard::menuSubItem(
          labelInput("normalidad"), tabName = "normalidad",
          icon = shiny::icon("bar-chart")),
        shinydashboard::menuSubItem(
          labelInput("dispersion"), tabName = "dispersion", 
          icon = shiny::icon("line-chart")),
        shinydashboard::menuSubItem(
          labelInput("distribucion"), tabName = "distribucion",
          icon = shiny::icon("area-chart")),
        shinydashboard::menuSubItem(
          labelInput("correlacion"), tabName = "correlacion",
          icon = shiny::icon("table"))
      ),
      shinydashboard::menuItem(
        labelInput("acp"), tabName = "acp", 
        icon = shiny::icon("pie-chart")),
      shinydashboard::menuItem(
        labelInput("jerarquico"), tabName = "agrupacion", 
        icon = shiny::icon("sitemap")),
      shinydashboard::menuItem(
        labelInput("kmedias"), tabName = "kmedias", 
        icon = shiny::icon("object-group")),
      shinydashboard::menuItem(
        labelInput("reporte"), tabName = "reporte",
        icon = shiny::icon("save-file", lib = "glyphicon")),
      shinydashboard::menuItem(
        labelInput("acercade"), tabName = "acercaDe",
        icon = shiny::icon("info")),
      shiny::hr(), 
      menu.idioma(),
      tags$div(style = "display:none;",
               shiny::sliderInput(inputId = "aux", min = 2, value = 2,
                                  label = "Cantidad de Clusters", max = 10),
               colourpicker::colourInput(
                 "auxColor", NULL, value = "red", allowTransparent = T)
      )
    )
  ),

  shinydashboard::dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style_promidat.css"),
      tags$link(rel = "shiny::icon", type = "image", href = paste0(
        "http://www.promidat.org/theme/image.php/", 
        "formal_white/theme/1438713216/favicon")),
      useShinyjs(),
      tags$script(src = "myscript.js")
    ),
    shiny::conditionalPanel(
      condition="($('html').hasClass('shiny-busy'))",
      div(id = "loaderWrapper", div(id="loader"))
    ),

    shinydashboard::tabItems(

      #Carga de Datos
      shinydashboard::tabItem(
        tabName = "cargar", shiny::column(width = 5, shinydashboard::tabBox(
          title = NULL, width = 12,
          shiny::tabPanel(
            title = labelInput("cargar"), width = 12, solidHeader = FALSE,
            collapsible = FALSE, collapsed = FALSE,
            shiny::checkboxInput('header', labelInput("header"), value = T),
            shiny::checkboxInput('rowname', labelInput("Rownames"), value = T),
            radioButtonsTr('sep', "separador", c(';', ',', '\t'), 
                           c("puntocoma", "coma", "tab")),
            radioButtonsTr('dec', "separadordec", c(',', '.'), c("coma", "punto")),
            switchInput(
              inputId = "deleteNA", onStatus = "success", offStatus = "danger", 
              value = T, labelWidth = "100px", label = labelInput("eliminana"), 
              onLabel = labelInput("si"), offLabel = labelInput("no")),
            shiny::fileInput(
              'file1', labelInput("cargarchivo"), width = "100%", 
              placeholder = "", buttonLabel = labelInput("subir"), 
              accept = c('text/csv', '.csv')),
            shiny::actionButton("loadButton", labelInput("cargar"), width = "100%"),
            shiny::hr(), shinyAce::aceEditor(
              "fieldCodeData", mode = "r", theme = "monokai", value = "", 
              height = "15vh", readOnly = T)),
          shiny::tabPanel(
            title = labelInput("trans"), width = 12, solidHeader = FALSE, 
            collapsible = FALSE, collapsed = FALSE, 
            DT::DTOutput('transData'), shiny::hr(), 
            shiny::actionButton("transButton", labelInput("aplicar"), 
                                width = "100%"),
            shiny::hr(), shinyAce::aceEditor(
              "fieldCodeTrans", mode = "r", theme = "monokai", value = "", 
              height = "10vh", readOnly = T))
        )),
        shiny::column(
          width = 7,
          shinydashboard::box(title = labelInput("data"), status = "primary", width = 12,
              solidHeader = TRUE, collapsible = TRUE, 
              DT::dataTableOutput('contents'), shiny::hr(),
              shiny::actionButton("downloaDatos", labelInput("descargar"),
                                  width = "100%")))
      ),

      #Resumen Numérico
      shinydashboard::tabItem(tabName = "resumen", shiny::column(
        shinydashboard::box(
          title = labelInput("resumen"), status = "primary", width = 12,
          solidHeader = TRUE, collapsible = TRUE, 
          DT::dataTableOutput("resumen.completo"), shiny::hr(),
          shinyAce::aceEditor(
            "fieldCodeResum", mode = "r", theme = "monokai", value = "", 
            height = "8vh", readOnly = T)), width = 7),
        shiny::column(
          shinydashboard::box(
            title = labelInput("resumenvar"), status = "primary",
            width = 12, solidHeader = TRUE, collapsible = TRUE,
            shiny::selectInput(
              inputId = "sel.resumen", label = labelInput("selvar"), 
              choices =  ""),
            shiny::fluidRow(uiOutput("resumen"))),  width = 5)
      ),

      #test de Normalidad
      shinydashboard::tabItem(tabName = "normalidad", shinydashboard::tabBox(
        id = "BoxNormal", width = NULL, title =
          tags$div(
            class = "multiple-select-var",
            shiny::selectInput(inputId = "sel.normal", label = NULL, choices =  "")
          ),
        shiny::tabPanel(
          title = labelInput("plotnormal"), value = "tabNormalPlot",
          shiny::plotOutput('plot.normal', height = "70vh")),
        shiny::tabPanel(
          title = labelInput("normalidad"), value = "tabNormalCalc",
          DT::DTOutput('calculo.normal')),
        tabsOptions(heights = c(50, 50, 100), tabs.content = list(
          list(shiny::h4(labelInput("opciones")), shiny::hr(),
               colourpicker::colourInput(
                 "col.normal", labelInput("selcolor"),
                 value = "#00FF22AA", allowTransparent = T)),
          list(
            shiny::conditionalPanel(
              "input.BoxNormal == 'tabNormalPlot'",
              campo.codigo("run.normal", "ref.normal", "fieldCodeNormal",
                           height = "25vh")),
            shiny::conditionalPanel(
              "input.BoxNormal == 'tabNormalCalc'",
              campo.codigo("run.calc.normal", "ref.calc.normal", 
                           "fieldCalcNormal", height = "20vh")))
          ))
        )
      ),

      #Dispersión
      shinydashboard::tabItem(
        tabName = "dispersion",
        shinydashboard::tabBox(
          id = "BoxDisp", width = NULL, title = 
            shiny::fluidRow(
              shiny::h4(
                style = "float:left;font-size: 20px;margin-right: 10px;",
                labelInput("selvars")),
              tags$div(
                class = "multiple-select-var",
                shiny::selectizeInput(
                  "select.var", NULL, multiple = T, choices = c(""),
                  options = list(maxItems = 3))
              )
            ),
          shiny::tabPanel(
            title = labelInput("dispersion"), value = "tabDisp",
            shiny::fluidRow(shiny::column(
              width = 8, 
              shiny::plotOutput(
                'plot.disp', height = "70vh", brush = 
                  brushOpts(id = "zoom.disp", resetOnNew = TRUE))
            ),
            shiny::column(
              width = 4, DT::dataTableOutput('mostrar.disp.zoom'), shiny::hr(),
              shiny::plotOutput('plot.disp.zoom', height = "41vh")
          ))),
          tabsOptions(
            heights = c(50, 40, 100), tabs.content = list(
              list(shiny::h4(labelInput("opciones")), shiny::hr(),
                   colourpicker::colourInput(
                     "col.disp", labelInput("selcolor"), value = "#FF0000AA", 
                     allowTransparent = T)),
              list(shiny::column(width = 12,
                          campo.codigo("run.disp", "ref.disp", 
                                       "fieldCodeDisp", height = "15vh"))
              )
            )
          )
        )
      ),

      #Distribuciones
      shinydashboard::tabItem(
        tabName = "distribucion",
        shinydashboard::tabBox(
          id = "tabDyA", width = NULL,
          title = tags$div(
            class = "multiple-select-var", shiny::conditionalPanel(
              condition = "input.tabDyA == 'numericas'",
              shiny::selectInput(inputId = "sel.distribucion.num", label = NULL,
                          choices =  "")),
            shiny::conditionalPanel(
              condition = "input.tabDyA == 'categoricas'",
              shiny::selectInput(inputId = "sel.distribucion.cat", label = NULL,
                          choices =  ""))),
          shiny::tabPanel(
            title = labelInput("numericas"), value = "numericas",
            shiny::plotOutput('plot.num', height = "70vh")),
          shiny::tabPanel(
            title = labelInput("categoricas"), value = "categoricas",
            shiny::plotOutput('plot.cat', height = "70vh")),
          tabsOptions(
            botones = list(shiny::icon("gear"), shiny::icon("terminal"), 
                           shiny::icon("info"), shiny::icon("code")),
            widths = c(50, 100, 100, 100), heights = c(50, 40, 50, 70),
            tabs.content = list(
              list(shiny::h4(labelInput("opciones")), shiny::hr(), 
                   colourpicker::colourInput(
                     "col.dist", labelInput("selcolor"), value = "#0D00FFAA",
                     allowTransparent = T)
              ),
              list(shiny::conditionalPanel(
                condition = "input.tabDyA == 'numericas'",
                campo.codigo("run.dya.num", "ref.dya.num",
                             "fieldCodeNum", height = "15vh")),
                shiny::conditionalPanel(
                  condition = "input.tabDyA == 'categoricas'",
                  campo.codigo("run.dya.cat", "ref.dya.cat",
                               "fieldCodeCat", height = "15vh"))),
              list(DT::dataTableOutput("mostrar.atipicos")),
              list(
                shiny::h4(labelInput("codigo")), shiny::hr(),
                shinydashboard::tabBox(
                  id = "tabCodeDyA", width = NULL, 
                  title = labelInput("codedist"),
                  shiny::tabPanel(title = labelInput("numericas"), 
                           shinyAce::aceEditor("fieldFuncNum", mode = "r", 
                                     theme = "monokai", value = "", 
                                     height = "300px", readOnly = T)),
                  shiny::tabPanel(title = labelInput("categoricas"), 
                           shinyAce::aceEditor("fieldFuncCat", mode = "r", 
                                     theme = "monokai", value = "", 
                                     height = "180px", readOnly = T))
                )
              )
            )
          )
        )
      ),

      #Correlaciones
      shinydashboard::tabItem(
        tabName = "correlacion",
        shinydashboard::tabBox(
          id = "tabCor", width = NULL,
          shiny::tabPanel(
            title = labelInput("correlacion"), value = "correlacion", 
            shiny::plotOutput('plot.cor', height = "70vh")),
          shiny::tabPanel(
            title = labelInput("resultados"), value = "cor.salida", 
            shiny::verbatimTextOutput("txtcor")),
          tabsOptions(heights = c(70, 50, 100), tabs.content = list(
            list(
              shiny::h4(labelInput("opciones")), shiny::hr(),
              shiny::selectInput(
                inputId = "cor.metodo", label = labelInput("selmetodo"),
                choices =  c("circle", "square", "ellipse", "number", 
                             "shade", "color", "pie")),
              shiny::selectInput(
                inputId = "cor.tipo", label = labelInput("seltipo"), 
                choices =  c("lower", "upper", "full"))),
            list(
              shinyAce::aceEditor("fieldModelCor", height = "6vh", mode = "r",
                        theme = "monokai", value = "", readOnly = T),
              campo.codigo("run.code.cor", "ref.code.cor",
                           "fieldCodeCor", height = "15vh"))
            )
          )
        )
      ),

      #PCA
      shinydashboard::tabItem(
        tabName = "acp",
        shinydashboard::tabBox(
          id = "tabPCA", width = NULL,
          shiny::tabPanel(
            title = labelInput("individuos"), value = "tabInd", shiny::fluidRow(
              shiny::column(
                width = 8,
                shiny::plotOutput(
                  'plot.ind', height = "70vh",
                  brush = brushOpts(id = "zoom.ind", resetOnNew = TRUE))
              ),
              shiny::column(width = 4, DT::dataTableOutput('mostrar.ind.zoom'),
                            shiny::hr(), shiny::plotOutput(
                              'plot.ind.zoom', height = "40vh")))
          ),
          shiny::tabPanel(
            title = labelInput("variables"), value = "tabVar",
            shiny::plotOutput('plot.var', height = "70vh")
          ),
          shiny::tabPanel(
            title = labelInput("sobreposicion"), value = "tabBi", shiny::fluidRow(
              shiny::column(
                width = 8,
                shiny::plotOutput('plot.biplot', height = "70vh",
                           brush = brushOpts(id = "zoom.bi", resetOnNew = TRUE))
              ),
              shiny::column(width = 4, DT::dataTableOutput('mostrar.bi.zoom'),
                     shiny::hr(), shiny::plotOutput(
                       'plot.bi.zoom', height = "40vh")))
          ),
          navbarMenu(
            labelInput("ayudacp"),
            shiny::tabPanel(labelInput("vee"), value = "tabVEE",
                     shiny::plotOutput("plotVEE", height = "70vh")),
            shiny::tabPanel(labelInput("cci"), value = "tabCCI",
                     shiny::plotOutput("plotCCI", height = "70vh")),
            shiny::tabPanel(labelInput("ccv"), value = "tabCCV",
                     shiny::plotOutput("plotCCV", height = "70vh")),
            shiny::tabPanel(labelInput("cvc"), value = "tabCVC",
                     shiny::plotOutput("plotCVC", height = "70vh")),
            shiny::tabPanel(labelInput("cp1"), value = "tabPC1",
                     shiny::plotOutput("plotPC1", height = "70vh")),
            shiny::tabPanel(labelInput("cp2"), value = "tabPC2",
                     shiny::plotOutput("plotPC2", height = "70vh"))),
          shiny::tabPanel(title = labelInput("resultados"), value = "pca.salida",
                   shiny::verbatimTextOutput("txtpca")),
          tabsOptions(
            tabs.content = list(
              list(
                shiny::h4(labelInput("opciones")), shiny::hr(),
                switchInput(
                  inputId = "switch.scale", value = T, onStatus = "success",
                  offStatus = "danger", label = labelInput("centrar"), 
                  onLabel = labelInput("si"), offLabel = labelInput("no"),
                  labelWidth = "100%"),
                shiny::sliderInput("slider.npc", labelInput("numerodim"), 
                            min = 2, max = 10, value = 5), 
                shiny::sliderInput(
                  "slider.ejes", labelInput("selejes"), min = 1,
                  max = 10, value = c(1,2)), 
                shiny::conditionalPanel(
                  condition = paste0("input.tabPCA == 'tabInd' ||",
                                     " input.tabPCA == 'tabBi'"),
                  shiny::sliderInput("ind.cos", label = labelInput("cosind"), 
                              min = 0, max = 100, value = 0),
                  colourpicker::colourInput(
                    "col.pca.ind", label = labelInput("selcolor"),
                    value = "#696969", allowTransparent = T)
                  ),
                shiny::conditionalPanel(
                  condition = paste0("input.tabPCA == 'tabVar' || ",
                                     "input.tabPCA == 'tabBi'"),
                  shiny::sliderInput(
                    "var.cos", label = labelInput("cosvar"), 
                    min = 0, max = 100, value = 0),
                  colourpicker::colourInput(
                    "col.pca.var", labelInput("selcolor"),
                    value = "steelblue", allowTransparent = T)
                  ),
                shiny::conditionalPanel(
                  condition = "input.tabPCA == 'tabCVC'",
                  shiny::selectInput(
                    inputId = "cvc.metodo", label = labelInput("seltipo"),
                    choices =  c("circle", "square", "ellipse",  "number",
                                 "shade", "color", "pie"))
                ), shiny::hr(),
                shiny::actionButton("ACPRun", labelInput("ejecutar"), 
                                    class = "btn-run", width = "100%"), 
                shiny::hr()
              ),
              list(
                shinyAce::aceEditor(
                  "fieldCodePCAModelo", height = "5vh", mode = "r",
                  theme = "monokai", value = "", readOnly = T),
                lapply(c("Ind", "Var", "Bi"), function(i) {
                  shiny::conditionalPanel(
                    condition = paste0("input.tabPCA == 'tab", i, "'"),
                    campo.codigo(paste0("run.pca", i), paste0("ref.pca", i),
                                 paste0("fieldCode", i), height = "15vh"))
                }),
                lapply(c('VEE', 'CCI', 'CCV', 'CVC', 'PC1', 'PC2'), function(i) {
                  shiny::conditionalPanel(
                    condition = paste0("input.tabPCA == 'tab", i, "'"),
                    shinyAce::aceEditor(
                      paste0("fieldCode", i), mode = "r", theme = "monokai",
                      value = "", height = "15vh", readOnly = T))
                })
              )
            )
          )
        )
      ),

      #Agrupaciones
      shinydashboard::tabItem(tabName = "agrupacion", shinydashboard::tabBox(
        id = "tabjerar", width = 12, title = tags$div(
          class = "multiple-select-var",
          lapply(c("Horiz", "Vert", "Bar"), function(i) {
            shiny::conditionalPanel(
              condition = paste0("input.tabjerar == 'tab", i, "'"),
              shiny::selectInput(inputId = paste0("sel", i), 
                          label = NULL, choices =  ""))
          })
        ),
        shiny::tabPanel(
          title = labelInput("inercia"), value = "tabInercia",
          shiny::wellPanel(
            shiny::fluidRow(uiOutput('inercia.cj')), style="height: 65vh;")
        ),
        shiny::tabPanel(title = labelInput("dendograma"), value = "tabDendo",
                 shiny::plotOutput('plot.diag', height = "70vh")
        ),
        shiny::tabPanel(
          title = labelInput("mapa"), value = "tabMapa", shiny::fluidRow(
            shiny::column(
              width = 8,
              shiny::plotOutput('plot.mapa', height = "70vh",
                         brush = brushOpts(id = "zoom.mapa", resetOnNew = TRUE))
            ),
            shiny::column(
              width = 4, DT::dataTableOutput('mostrar.mapa.zoom'), shiny::hr(),
              shiny::plotOutput('plot.mapa.zoom', height = "41vh")))
        ),
        shiny::tabPanel(title = labelInput("horizontal"), value = "tabHoriz",
                 shiny::plotOutput('plot.horiz', height = "70vh")
        ),
        shiny::tabPanel(title = labelInput("vertical"), value = "tabVert",
                 shiny::plotOutput('plot.vert', height = "70vh")
        ),
        shiny::tabPanel(title = labelInput("radar"), value = "tabRadar",
                 shiny::plotOutput('plot.radar', height = "70vh")
        ),
        shiny::tabPanel(title = labelInput("interpretacioncat"), value = "tabBar",
                 shiny::plotOutput('plot.bar.cat', height = "70vh")
        ),
        shiny::tabPanel(title = labelInput("resultados"), value = "salida.hc",
                 shiny::verbatimTextOutput("txthc"), shiny::hr(),
                 shiny::verbatimTextOutput("txtcentros")
        ),
        tabsOptions(
          botones = list(shiny::icon("gear"), shiny::icon("terminal"), 
                         shiny::icon("code")),
          widths = c(33.3, 100, 100), heights = c(100, 50, 70),
          tabs.content = list(
            list(shiny::h4(labelInput("opciones")), shiny::hr(),
                 shiny::sliderInput(inputId = "cant.cluster", min = 2, max = 10,
                             label = labelInput("cantcluster"), value = 2),
                 shiny::selectInput(
                   inputId = "sel.hc.method", label = labelInput("selmetodo"),
                   selectize = T, choices =  c("ward.D2", "single", 
                                               "complete", "average")),
                 shiny::selectInput(inputId = "sel.dist.method", 
                             label = labelInput("metododist"), selectize = T,
                             choices =  c("euclidean", "maximum", "manhattan",
                                          "canberra", "binary", "minkowski")),
                 tags$label(class='control-label', labelInput("selcolores")),
                 shiny::fluidRow(
                   lapply(1:10, function(i)
                     tags$div(class = "select-color", colourpicker::colourInput(
                       paste0("hcColor", i), NULL, value = def.colors[i],
                       allowTransparent = T)))), shiny::hr(),
                 shiny::actionButton("CJRun", labelInput("ejecutar"), 
                                     class = "btn-run", width = "100%"), 
                 shiny::hr(),
                 shiny::actionButton("HCbutton", labelInput("agregarcluster"), 
                              width = "100%"), shiny::hr()),
            list(
              shinyAce::aceEditor("fieldCodeModelo", height = "8vh",mode = "r",
                        theme = "monokai", value = "", readOnly = T),
              lapply(c("Dendo", "Mapa", "Horiz", 
                       "Vert", "Radar", "Bar"), function(i) {
                shiny::conditionalPanel(
                  condition = paste0("input.tabjerar == 'tab", i, "'"),
                  campo.codigo(paste0("run.hc", i), paste0("ref.hc", i),
                               paste0("fieldCode", i), height = "13vh"))
              })
            ),
            list(shiny::h4(labelInput("codigo")), shiny::hr(),
                 shinydashboard::tabBox(
                   id = "tabCodejerar", width = NULL,
                   shiny::tabPanel(title = labelInput("codecentros"), 
                            shinyAce::aceEditor(
                              "fieldCodeCentr", mode = "r", theme = "monokai",
                              value = "", height = "25vh", readOnly = T)),
                   shiny::tabPanel(title = labelInput("codehoriz"), 
                            shinyAce::aceEditor(
                              "fieldFuncHoriz", mode = "r", theme = "monokai",
                              value = "", height = "25vh", readOnly = T)),
                   shiny::tabPanel(title = labelInput("codevert"), 
                            shinyAce::aceEditor(
                              "fieldFuncVert", mode = "r", theme = "monokai",
                              value = "", height = "25vh", readOnly = T)),
                   shiny::tabPanel(title = labelInput("coderadar"), 
                            shinyAce::aceEditor(
                              "fieldFuncRadar", mode = "r", theme = "monokai",
                              value = "", height = "25vh", readOnly = T))
                 )
              )
            )
          )
        )
      ),

      #K-means
      shinydashboard::tabItem(
        tabName = "kmedias", shinydashboard::tabBox(
          id = "tabkmedias", width = 12, title =
            tags$div(
              class = "multiple-select-var",
              lapply(c("Khoriz", "Kvert", "Kbar"), function(i) {
                shiny::conditionalPanel(
                  condition = paste0("input.tabkmedias == 'tab", i, "'"),
                  shiny::selectInput(inputId = paste0("sel.", i),
                              label = NULL, choices = ""))
              })
            ),
          shiny::tabPanel(
            title = labelInput("inercia"), value = "tabKinercia",
            shiny::wellPanel(
              shiny::fluidRow(uiOutput('inercia.k')), style="height: 65vh;")
          ),
          shiny::tabPanel(title = labelInput("jambu"), value = "tabJambu",
                   shiny::plotOutput('plot.jambu', height = "70vh")
          ),
          shiny::tabPanel(
            title = labelInput("mapa"), value = "tabKmapa", shiny::fluidRow( 
              shiny::column(
                width = 8,
                shiny::plotOutput(
                  'plot.kmapa', height = "70vh", brush = brushOpts(
                    id = "zoom.kmapa", resetOnNew = TRUE))),
              shiny::column(
                width = 4, DT::dataTableOutput('mostrar.kmapa.zoom'), shiny::hr(),
                shiny::plotOutput('plot.kmapa.zoom', height = "41vh")))
          ),
          shiny::tabPanel(
            title = labelInput("horizontal"), value = "tabKhoriz",
            shiny::plotOutput('plot.khoriz', height = "70vh")
          ),
          shiny::tabPanel(
            title = labelInput("vertical"), value = "tabKvert",
            shiny::plotOutput('plot.kvert', height = "70vh")
          ),
          shiny::tabPanel(
            title = labelInput("radar"), value = "tabKradar",
            shiny::plotOutput('plot.kradar', height = "70vh")
          ),
          shiny::tabPanel(
            title = labelInput("interpretacioncat"), value = "tabKbar",
            shiny::plotOutput('plot.kcat', height = "70vh")
          ),
          shiny::tabPanel(
            title = labelInput("resultados"), value = "salida.k",
            shiny::verbatimTextOutput("txtk")
          ),
          tabsOptions(
            botones = list(shiny::icon("gear"), shiny::icon("terminal"), 
                           shiny::icon("code")),
            widths = c(33.3, 100, 100), heights = c(100, 50, 80),
            tabs.content = list(
              list(shiny::h4(labelInput("opciones")), shiny::hr(),
                   shiny::conditionalPanel(
                     condition = "input.tabkmedias == 'tabJambu'",
                     shiny::sliderInput(
                       inputId = "iteracionesK", min = 2, value = 20,
                       label = labelInput("kiter"), max = 20)),
                   shiny::sliderInput(
                     inputId = "cant.kmeans.cluster", min = 2, value = 2,
                     label = labelInput("cantcluster"), max = 10),
                   shiny::numericInput(
                     "num.nstart", step = 10, value = 1,
                     label = labelInput("nstart")),
                   shiny::numericInput(
                     "num.iter", step = 100, value = 10, 
                     label = labelInput("niter")),
                   shiny::selectInput(
                     inputId = "sel.algoritmo", label = labelInput("algoritmo"), 
                     selectize = T, choices =  c("Hartigan-Wong", "Lloyd", 
                                                 "Forgy", "MacQueen")),
                   tags$label(class='control-label', labelInput("selcolores")),
                   shiny::fluidRow(
                     lapply(1:10, function(i)
                       tags$div(class = "select-color", colourpicker::colourInput(
                         paste0("kColor", i), NULL, value = def.colors[i],
                         allowTransparent = T)))
                   ), shiny::hr(),
                   shiny::actionButton("KRun", labelInput("ejecutar"), 
                                       class = "btn-run", width = "100%"),
                   shiny::hr(),
                   shiny::actionButton("Kbutton", labelInput("agregarcluster"), 
                                width = "100%"), shiny::hr()
              ),
              list(
                shinyAce::aceEditor("fieldCodeKModelo", height = "5vh", mode = "r",
                          theme = "monokai", value = "", readOnly = T),
                lapply(c("Jambu", "Kmapa", "Khoriz", "Kvert",
                         "Kradar", "Kbar"), function(i) {
                  shiny::conditionalPanel(
                    condition = paste0("input.tabkmedias == 'tab", i, "'"),
                    campo.codigo(paste0("run.", i), paste0("ref.", i),
                                 paste0("fieldCode", i), height = "15vh"))
                })
              ),
              list(shiny::h4(labelInput("codigo")), shiny::hr(),
                   lapply(list(
                     list(titulo = labelInput("codejambu"), id = "Jambu"),
                     list(titulo = labelInput("codehoriz"), id = "Khoriz"),
                     list(titulo = labelInput("codevert"), id = "Kvert"),
                     list(titulo = labelInput("coderadar"), id = "Kradar")),
                     function(i) {
                       shiny::conditionalPanel(
                         condition = paste0("input.tabkmedias == 'tab", i$id, "'"),
                         shiny::h5(i$titulo),
                         shinyAce::aceEditor(
                           paste0("fieldFunc", i$id), mode = "r", theme = "monokai",
                           value = "", height = "50vh", readOnly = T))
                   })
              )
            )
          )
        )
      ),

      #Generar Reporte
      shinydashboard::tabItem(
        tabName = "reporte",
        shiny::column(
          width = 5, 
          shinydashboard::box(
            title = labelInput("reporte"), width = 12,
            shiny::textInput(
              "textTitulo", value = "Sin Titulo", width = "100%",
              label = labelInput("titulo")),
            shiny::textInput(
              "textNombre", value = "PROMiDAT", width = "100%",
              label = labelInput("nombre")), shiny::hr(),
            shiny::downloadButton(
              "descargar", labelInput("descargar"), class = "center-button"))
        ),
        shiny::column(
          width = 7,
          shinydashboard::box(
            title = labelInput("codreporte"), width = 12, height = "50vh",
            status = "primary", solidHeader = TRUE, collapsible = TRUE,
            shinyAce::aceEditor("fieldCodeReport", mode="markdown", 
                                value='', height = "43vh"))),
        shiny::fluidRow(shiny::column(
          width = 12, shinydashboard::box(
            title = labelInput("salida"), width = 12, height = "35vh",
            shiny::verbatimTextOutput("txtreport")))
        )
      ),

      shinydashboard::tabItem(
        tabName = "acercaDe",
        shiny::img(src="Logo.png",
            style = paste0("padding-bottom:20px;margin-left: auto;",
                         "margin-right: auto;display: block;width: 50%;")),
        infoBoxPROMiDAT(
          labelInput("copyright"), "PROMiDAT S.A.", 
          icono = shiny::icon("copyright")
        ),
        infoBoxPROMiDAT(
          labelInput("info"), tags$a(
            href="https://www.promidat.com/", style = "color:white;",
            target = "_blank", "https://www.promidat.com"), 
          icono = shiny::icon("info")
        ),
        infoBoxPROMiDAT(
          labelInput("version"), "1.0.4", icono = shiny::icon("file-code-o"))
      )
    ) #shinydashboard::tabItems
  ) #dashboardBody
)) #UI
