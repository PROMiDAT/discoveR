# UI Script

library(DT)
library(zip)
library(shiny)
library(knitr)
library(rgdal)
library(raster)
library(future)
library(reshape)
library(ggplot2)
library(shinyjs)
library(stringr)
library(stringi)
library(corrplot)
library(shinyAce)
library(promises)
library(ggdendro)
library(rmarkdown)
library(dendextend)
library(factoextra)
library(rstudioapi)
library(FactoMineR)
library(colourpicker)
library(shinyWidgets)
library(scatterplot3d)
library(shinydashboard)
library(shinydashboardPlus)

shinyUI(dashboardPagePlus(
  title="PROMiDAT - discoveR",
  dashboardHeaderPlus(
    title = tags$a(href="http://promidat.com", target = "_blank",
                   img(src="Logo2.png", height=55, width="100%",
                       style="padding-top:2px; padding-bottom:6px;"))
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "principal",
      tags$div(style="padding-top:10px;"),
      menuItem(labelInput("data"), tabName = "cargar",
               icon = icon("dashboard")),
      menuItem(
        labelInput("basico"), tabName = "parte1", icon = icon("th-list"),
        menuSubItem(labelInput("resumen"), tabName = "resumen",
                    icon = icon("sort-numeric-asc")),
        menuSubItem(labelInput("normalidad"), tabName = "normalidad",
                    icon = icon("bar-chart")),
        menuSubItem(labelInput("dispersion"), tabName = "dispersion", 
                    icon = icon("line-chart")),
        menuSubItem(labelInput("distribucion"), tabName = "distribucion",
                    icon = icon("area-chart")),
        menuSubItem(labelInput("correlacion"), tabName = "correlacion",
                    icon = icon("table"))),
      menuItem(labelInput("acp"), tabName = "acp", 
               icon = icon("pie-chart")),
      menuItem(labelInput("jerarquico"), tabName = "agrupacion",
               icon = icon("sitemap")),
      menuItem(labelInput("kmedias"), tabName = "kmedias",
               icon = icon("object-group")),
      menuItem(labelInput("reporte"), tabName = "reporte",
               icon = icon("save-file", lib = "glyphicon")),
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
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style_promidat.css"),
      tags$link(rel = "icon", type = "image", href = paste0(
        "http://www.promidat.org/theme/image.php/", 
        "formal_white/theme/1438713216/favicon")),
      useShinyjs(),
      tags$script(src = "myscript.js")
    ),
    conditionalPanel(
      condition="($('html').hasClass('shiny-busy'))",
      div(id = "loaderWrapper", div(id="loader"))
    ),

    tabItems(

      #Carga de Datos
      tabItem(tabName = "cargar", column(width = 5, tabBox(
        title = NULL, width = 12,
        tabPanel(
          title = labelInput("cargar"), width = 12, solidHeader = FALSE,
          collapsible = FALSE, collapsed = FALSE,
          checkboxInput('header', labelInput("header"), value = T),
          checkboxInput('rowname', labelInput("Rownames"), value = T),
          radioButtonsTr('sep', "separador", c(';', ',', '\t'), 
                         c("puntocoma", "coma", "tab")),
          radioButtonsTr('dec', "separadordec", c(',', '.'), c("coma", "punto")),
          switchInput(
            inputId = "deleteNA", onStatus = "success", offStatus = "danger", 
            value = T, labelWidth = "100px", label = labelInput("eliminana"), 
            onLabel = labelInput("si"), offLabel = labelInput("no")),
          fileInput(
            'file1', labelInput("cargarchivo"), width = "100%", 
            placeholder = "", buttonLabel = labelInput("subir"), 
            accept = c('text/csv', '.csv')),
          actionButton("loadButton", labelInput("cargar"), width = "100%"),
          hr(), aceEditor("fieldCodeData", mode = "r", theme = "monokai",
                          value = "", height = "15vh", readOnly = T)),
        tabPanel(
          title = labelInput("trans"), width = 12, solidHeader = FALSE, 
          collapsible = FALSE, collapsed = FALSE, 
          DT::DTOutput('transData'), hr(), 
          actionButton("transButton", labelInput("aplicar"), width = "100%"),
          hr(), aceEditor("fieldCodeTrans", mode = "r", theme = "monokai",
                          value = "", height = "10vh", readOnly = T))
        )),
        column(
          width = 7,
          box(title = labelInput("data"), status = "primary", width = 12,
              solidHeader = TRUE, collapsible = TRUE, 
              DT::dataTableOutput('contents'), hr(),
              actionButton("downloaDatos", labelInput("descargar"),
                           width = "100%")))
      ),

      #Resumen Numérico
      tabItem(tabName = "resumen", column(
        box(title = labelInput("resumen"), status = "primary", width = 12,
            solidHeader = TRUE, collapsible = TRUE,
            DT::dataTableOutput("resumen.completo"), hr(),
            aceEditor("fieldCodeResum", mode = "r", theme = "monokai",
                      value = "", height = "8vh", readOnly = T)), width = 7),
        column(
          box(title = labelInput("resumenvar"), status = "primary",
              width = 12, solidHeader = TRUE, collapsible = TRUE,
              selectInput(inputId = "sel.resumen", label = labelInput("selvar"), 
                          choices =  ""),
              fluidRow(uiOutput("resumen"))),  width = 5)
      ),

      #test de Normalidad
      tabItem(tabName = "normalidad", tabBox(
        id = "BoxNormal", width = NULL, title =
          tags$div(
            class = "multiple-select-var",
            selectInput(inputId = "sel.normal", label = NULL, choices =  "")
          ),
        tabPanel(title = labelInput("plotnormal"), value = "tabNormalPlot",
                 plotOutput('plot.normal', height = "70vh")),
        tabPanel(title = labelInput("normalidad"), value = "tabNormalCalc",
                 DT::DTOutput('calculo.normal')),
        tabsOptions(heights = c(50, 50, 100), tabs.content = list(
          list(h4(labelInput("opciones")), hr(),
               colourpicker::colourInput(
                 "col.normal", labelInput("selcolor"),
                 value = "#00FF22AA", allowTransparent = T)),
          list(
            conditionalPanel(
              "input.BoxNormal == 'tabNormalPlot'",
              campo.codigo("run.normal", "ref.normal", "fieldCodeNormal",
                           height = "25vh")),
            conditionalPanel(
              "input.BoxNormal == 'tabNormalCalc'",
              campo.codigo("run.calc.normal", "ref.calc.normal", 
                           "fieldCalcNormal", height = "20vh")))
          ))
        )
      ),

      #Dispersión
      tabItem(
        tabName = "dispersion",
        tabBox(
          id = "BoxDisp", width = NULL, title = 
            fluidRow(
              h4(style = "float:left;font-size: 20px;", labelInput("selvars")),
              tags$div(
                class = "multiple-select-var", style = "width:60%;",
                selectizeInput(
                  "select.var", NULL, multiple = T, choices = c(""),
                  options = list(maxItems = 3))
              )
            ),
          tabPanel(
            title = labelInput("dispersion"), value = "tabDisp",
            fluidRow(column(
              width = 8, 
              plotOutput(
                'plot.disp', height = "70vh", brush = 
                  brushOpts(id = "zoom.disp", resetOnNew = TRUE))
            ),
            column(
              width = 4, DT::dataTableOutput('mostrar.disp.zoom'), hr(), 
              plotOutput('plot.disp.zoom', height = "41vh")
          ))),
          tabsOptions(
            heights = c(50, 40, 100), tabs.content = list(
              list(h4(labelInput("opciones")), hr(),
                   colourpicker::colourInput(
                     "col.disp", labelInput("selcolor"), value = "#FF0000AA", 
                     allowTransparent = T)),
              list(column(width = 12,
                          campo.codigo("run.disp", "ref.disp", 
                                       "fieldCodeDisp", height = "15vh"))
              )
            )
          )
        )
      ),

      #Distribuciones
      tabItem(
        tabName = "distribucion",
        tabBox(
          id = "tabDyA", width = NULL,
          title = tags$div(
            class = "multiple-select-var", conditionalPanel(
              condition = "input.tabDyA == 'numericas'",
              selectInput(inputId = "sel.distribucion.num", label = NULL, 
                          choices =  "")),
            conditionalPanel(
              condition = "input.tabDyA == 'categoricas'",
              selectInput(inputId = "sel.distribucion.cat", label = NULL, 
                          choices =  ""))),
          tabPanel(title = labelInput("numericas"), value = "numericas",
                   plotOutput('plot.num', height = "70vh")),
          tabPanel(title = labelInput("categoricas"), value = "categoricas",
                   plotOutput('plot.cat', height = "70vh")),
          tabsOptions(
            botones = list(icon("gear"), icon("terminal"), 
                           icon("info"), icon("code")),
            widths = c(50, 100, 100, 100), heights = c(50, 40, 50, 70),
            tabs.content = list(
              list(h4(labelInput("opciones")), hr(), colourpicker::colourInput(
                "col.dist", labelInput("selcolor"), value = "#0D00FFAA", 
                allowTransparent = T)),
              list(conditionalPanel(
                condition = "input.tabDyA == 'numericas'",
                campo.codigo("run.dya.num", "ref.dya.num",
                             "fieldCodeNum", height = "15vh")),
                conditionalPanel(
                  condition = "input.tabDyA == 'categoricas'",
                  campo.codigo("run.dya.cat", "ref.dya.cat",
                               "fieldCodeCat", height = "15vh"))),
              list(DT::dataTableOutput("mostrar.atipicos")),
              list(
                h4(labelInput("codigo")), hr(),
                tabBox(
                  id = "tabCodeDyA", width = NULL, 
                  title = labelInput("codedist"),
                  tabPanel(title = labelInput("numericas"), 
                           aceEditor("fieldFuncNum", mode = "r", 
                                     theme = "monokai", value = "", 
                                     height = "300px", readOnly = T)),
                  tabPanel(title = labelInput("categoricas"), 
                           aceEditor("fieldFuncCat", mode = "r", 
                                     theme = "monokai", value = "", 
                                     height = "180px", readOnly = T))
                )
              )
            )
          )
        )
      ),

      #Correlaciones
      tabItem(
        tabName = "correlacion",
        tabBox(
          id = "tabCor", width = NULL,
          tabPanel(
            title = labelInput("correlacion"), value = "correlacion", 
            plotOutput('plot.cor', height = "70vh")),
          tabPanel(
            title = labelInput("resultados"), value = "cor.salida", 
            verbatimTextOutput("txtcor")),
          tabsOptions(heights = c(70, 50, 100), tabs.content = list(
            list(
              h4(labelInput("opciones")), hr(),
              selectInput(
                inputId = "cor.metodo", label = labelInput("selmetodo"),
                choices =  c("circle", "square", "ellipse", "number", 
                             "shade", "color", "pie")),
              selectInput(
                inputId = "cor.tipo", label = labelInput("seltipo"), 
                choices =  c("lower", "upper", "full"))),
            list(
              aceEditor("fieldModelCor", height = "6vh", mode = "r", 
                        theme = "monokai", value = "", readOnly = T),
              campo.codigo("run.code.cor", "ref.code.cor",
                           "fieldCodeCor", height = "15vh"))
            )
          )
        )
      ),

      #PCA
      tabItem(
        tabName = "acp",
        tabBox(
          id = "tabPCA", width = NULL,
          tabPanel(title = labelInput("individuos"), value = "tabInd", fluidRow(
            column(
              width = 8,
              plotOutput('plot.ind', height = "70vh",
                         brush = brushOpts(id = "zoom.ind", resetOnNew = TRUE))
            ),
            column(width = 4, DT::dataTableOutput('mostrar.ind.zoom'),
                   hr(), plotOutput('plot.ind.zoom')))
          ),
          tabPanel(title = labelInput("variables"), value = "tabVar",
                   plotOutput('plot.var', height = "70vh")
          ),
          tabPanel(
            title = labelInput("sobreposicion"), value = "tabBi", fluidRow(
              column(
                width = 8,
                plotOutput('plot.biplot', height = "70vh",
                           brush = brushOpts(id = "zoom.bi", resetOnNew = TRUE))
              ),
              column(width = 4, DT::dataTableOutput('mostrar.bi.zoom'),
                     hr(), plotOutput('plot.bi.zoom')))
          ),
          navbarMenu(
            labelInput("ayudacp"),
            tabPanel(labelInput("vee"), value = "tabVEE",
                     plotOutput("plotVEE", height = "70vh")),
            tabPanel(labelInput("cci"), value = "tabCCI",
                     plotOutput("plotCCI", height = "70vh")),
            tabPanel(labelInput("ccv"), value = "tabCCV",
                     plotOutput("plotCCV", height = "70vh")),
            tabPanel(labelInput("cvc"), value = "tabCVC",
                     plotOutput("plotCVC", height = "70vh")),
            tabPanel(labelInput("cp1"), value = "tabPC1",
                     plotOutput("plotPC1", height = "70vh")),
            tabPanel(labelInput("cp2"), value = "tabPC2",
                     plotOutput("plotPC2", height = "70vh"))),
          tabPanel(title = labelInput("resultados"), value = "pca.salida",
                   verbatimTextOutput("txtpca")),
          tabsOptions(
            tabs.content = list(
              list(
                h4(labelInput("opciones")), hr(),
                switchInput(
                  inputId = "switch.scale", value = T, onStatus = "success",
                  offStatus = "danger", label = labelInput("centrar"), 
                  onLabel = labelInput("si"), offLabel = labelInput("no"),
                  labelWidth = "100%"),
                sliderInput("slider.npc", labelInput("numerodim"), 
                            min = 2, max = 10, value = 5),
                sliderTextInput("slider.ejes", labelInput("selejes"), grid = T, 
                                choices = c(1:10), selected = c(1,2)),
                conditionalPanel(
                  condition = paste0("input.tabPCA == 'tabInd' ||",
                                     " input.tabPCA == 'tabBi'"),
                  sliderInput("ind.cos", label = labelInput("cosind"), 
                              min = 0, max = 100, value = 0),
                  colourpicker::colourInput(
                    "col.pca.ind", label = labelInput("selcolor"),
                    value = "#696969", allowTransparent = T)
                  ),
                conditionalPanel(
                  condition = paste0("input.tabPCA == 'tabVar' || ",
                                     "input.tabPCA == 'tabBi'"),
                  sliderInput(
                    "var.cos", label = labelInput("cosvar"), 
                    min = 0, max = 100, value = 0),
                  colourpicker::colourInput(
                    "col.pca.var", labelInput("selcolor"),
                    value = "steelblue", allowTransparent = T)
                  ),
                conditionalPanel(
                  condition = "input.tabPCA == 'tabCVC'",
                  selectInput(
                    inputId = "cvc.metodo", label = labelInput("seltipo"),
                    choices =  c("circle", "square", "ellipse",  "number",
                                 "shade", "color", "pie"))
                ), hr()
              ),
              list(
                aceEditor("fieldCodePCAModelo", height = "5vh", mode = "r",
                          theme = "monokai", value = "", readOnly = T),
                lapply(c("Ind", "Var", "Bi"), function(i) {
                  conditionalPanel(
                    condition = paste0("input.tabPCA == 'tab", i, "'"),
                    campo.codigo(paste0("run.pca", i), paste0("ref.pca", i),
                                 paste0("fieldCode", i), height = "15vh"))
                }),
                lapply(c('VEE', 'CCI', 'CCV', 'CVC', 'PC1', 'PC2'), function(i) {
                  conditionalPanel(
                    condition = paste0("input.tabPCA == 'tab", i, "'"),
                    aceEditor(paste0("fieldCode", i), mode = "r", theme = "monokai",
                              value = "", height = "15vh", readOnly = T))
                })
              )
            )
          )
        )
      ),

      #Agrupaciones
      tabItem(tabName = "agrupacion", tabBox(
        id = "tabjerar", width = 12, title = tags$div(
          class = "multiple-select-var",
          lapply(c("Horiz", "Vert", "Bar"), function(i) {
            conditionalPanel(
              condition = paste0("input.tabjerar == 'tab", i, "'"),
              selectInput(inputId = paste0("sel", i), 
                          label = NULL, choices =  ""))
          })
        ),
        tabPanel(
          title = labelInput("inercia"), value = "tabInercia",
          wellPanel(fluidRow(uiOutput('inercia.cj')), style="height: 65vh;")
        ),
        tabPanel(title = labelInput("dendograma"), value = "tabDendo",
                 plotOutput('plot.diag', height = "70vh")
        ),
        tabPanel(
          title = labelInput("mapa"), value = "tabMapa", fluidRow(
            column(
              width = 8,
              plotOutput('plot.mapa', height = "70vh",
                         brush = brushOpts(id = "zoom.mapa", resetOnNew = TRUE))
            ),
            column(width = 4, DT::dataTableOutput('mostrar.mapa.zoom'),
                   hr(), plotOutput('plot.mapa.zoom', height = "41vh")))
        ),
        tabPanel(title = labelInput("horizontal"), value = "tabHoriz",
                 plotOutput('plot.horiz', height = "70vh")
        ),
        tabPanel(title = labelInput("vertical"), value = "tabVert",
                 plotOutput('plot.vert', height = "70vh")
        ),
        tabPanel(title = labelInput("radar"), value = "tabRadar",
                 plotOutput('plot.radar', height = "70vh")
        ),
        tabPanel(title = labelInput("interpretacioncat"), value = "tabBar",
                 plotOutput('plot.bar.cat', height = "70vh")
        ),
        tabPanel(title = labelInput("resultados"), value = "salida.hc",
                 verbatimTextOutput("txthc"), hr(),
                 verbatimTextOutput("txtcentros")
        ),
        tabsOptions(
          botones = list(icon("gear"), icon("terminal"), icon("code")),
          widths = c(33.3, 100, 100), heights = c(100, 50, 70),
          tabs.content = list(
            list(h4(labelInput("opciones")), hr(),
                 sliderInput(inputId = "cant.cluster", min = 2, max = 10,
                             label = labelInput("cantcluster"), value = 2),
                 selectInput(inputId = "sel.hc.method", 
                             label = labelInput("selmetodo"), selectize = T,
                             choices =  c("ward.D2", "single", "complete", "average")),
                 selectInput(inputId = "sel.dist.method", 
                             label = labelInput("metododist"), selectize = T,
                             choices =  c("euclidean", "maximum", "manhattan",
                                          "canberra", "binary", "minkowski")),
                 tags$label(class='control-label', labelInput("selcolores")),
                 fluidRow(
                   lapply(1:10, function(i)
                     tags$div(class = "select-color", colourpicker::colourInput(
                       paste0("hcColor", i), NULL, value = def.colors[i],
                       allowTransparent = T)))), hr(),
                 actionButton("HCbutton", labelInput("agregarcluster"), 
                              width = "100%"), hr()),
            list(
              aceEditor("fieldCodeModelo", height = "8vh",mode = "r",
                        theme = "monokai", value = "", readOnly = T),
              lapply(c("Dendo", "Mapa", "Horiz", "Vert", "Radar", "Bar"), function(i) {
                conditionalPanel(
                  condition = paste0("input.tabjerar == 'tab", i, "'"),
                  campo.codigo(paste0("run.hc", i), paste0("ref.hc", i),
                               paste0("fieldCode", i), height = "13vh"))
              })
            ),
            list(h4(labelInput("codigo")), hr(),
                 tabBox(
                   id = "tabCodejerar", width = NULL,
                   tabPanel(title = labelInput("codecentros"), 
                            aceEditor(
                              "fieldCodeCentr", mode = "r", theme = "monokai",
                              value = "", height = "25vh", readOnly = T)),
                   tabPanel(title = labelInput("codehoriz"), 
                            aceEditor(
                              "fieldFuncHoriz", mode = "r", theme = "monokai",
                              value = "", height = "25vh", readOnly = T)),
                   tabPanel(title = labelInput("codevert"), 
                            aceEditor(
                              "fieldFuncVert", mode = "r", theme = "monokai",
                              value = "", height = "25vh", readOnly = T)),
                   tabPanel(title = labelInput("coderadar"), 
                            aceEditor(
                              "fieldFuncRadar", mode = "r", theme = "monokai",
                              value = "", height = "25vh", readOnly = T))
                 )
              )
            )
          )
        )
      ),

      #K-means
      tabItem(
        tabName = "kmedias", tabBox(
          id = "tabkmedias", width = 12, title =
            tags$div(
              class = "multiple-select-var",
              lapply(c("Khoriz", "Kvert", "Kbar"), function(i) {
                conditionalPanel(
                  condition = paste0("input.tabkmedias == 'tab", i, "'"),
                  selectInput(inputId = paste0("sel.", i),
                              label = NULL, choices = ""))
              })
            ),
          tabPanel(
            title = labelInput("inercia"), value = "tabKinercia",
            wellPanel(fluidRow(uiOutput('inercia.k')), style="height: 65vh;")
          ),
          tabPanel(title = labelInput("jambu"), value = "tabJambu",
                   plotOutput('plot.jambu', height = "70vh")
          ),
          tabPanel(title = labelInput("mapa"), value = "tabKmapa", fluidRow( column(
            width = 8,
            plotOutput('plot.kmapa', height = "70vh", brush =
                         brushOpts(id = "zoom.kmapa", resetOnNew = TRUE))),
            column(width = 4, DT::dataTableOutput('mostrar.kmapa.zoom'), hr(),
                   plotOutput('plot.kmapa.zoom', height = "41vh")))
          ),
          tabPanel(title = labelInput("horizontal"), value = "tabKhoriz",
                   plotOutput('plot.khoriz', height = "70vh")
          ),
          tabPanel(title = labelInput("vertical"), value = "tabKvert",
                   plotOutput('plot.kvert', height = "70vh")
          ),
          tabPanel(title = labelInput("radar"), value = "tabKradar",
                   plotOutput('plot.kradar', height = "70vh")
          ),
          tabPanel(title = labelInput("interpretacioncat"), value = "tabKbar",
                   plotOutput('plot.kcat', height = "70vh")
          ),
          tabPanel(title = labelInput("resultados"), value = "salida.k",
                   verbatimTextOutput("txtk")
          ),
          tabsOptions(
            botones = list(icon("gear"), icon("terminal"), icon("code")),
            widths = c(33.3, 100, 100), heights = c(100, 50, 80),
            tabs.content = list(
              list(h4(labelInput("opciones")), hr(),
                   conditionalPanel(
                     condition = "input.tabkmedias == 'tabJambu'",
                     sliderInput(inputId = "iteracionesK", min = 2, value = 20,
                                 label = labelInput("kiter"), max = 20)),
                   sliderInput(inputId = "cant.kmeans.cluster", min = 2, value = 2,
                               label = labelInput("cantcluster"), max = 10),
                   numericInput("num.nstart", step = 10, value = 1,
                                label = labelInput("nstart")),
                   numericInput("num.iter", step = 100, value = 10, 
                                label = labelInput("niter")),
                   selectInput(
                     inputId = "sel.algoritmo", label = labelInput("algoritmo"), 
                     selectize = T, choices =  c("Hartigan-Wong", "Lloyd", 
                                                 "Forgy", "MacQueen")),
                   tags$label(class='control-label', labelInput("selcolores")),
                   fluidRow(
                     lapply(1:10, function(i)
                       tags$div(class = "select-color", colourpicker::colourInput(
                         paste0("kColor", i), NULL, value = def.colors[i],
                         allowTransparent = T)))
                   ), hr(),
                   actionButton("Kbutton", labelInput("agregarcluster"), 
                                width = "100%"), hr()
              ),
              list(
                aceEditor("fieldCodeKModelo", height = "5vh", mode = "r",
                          theme = "monokai", value = "", readOnly = T),
                lapply(c("Jambu", "Kmapa", "Khoriz", "Kvert",
                         "Kradar", "Kbar"), function(i) {
                  conditionalPanel(
                    condition = paste0("input.tabkmedias == 'tab", i, "'"),
                    campo.codigo(paste0("run.", i), paste0("ref.", i),
                                 paste0("fieldCode", i), height = "15vh"))
                })
              ),
              list(h4(labelInput("codigo")), hr(),
                   lapply(list(
                     list(titulo = labelInput("codejambu"), id = "Jambu"),
                     list(titulo = labelInput("codehoriz"), id = "Khoriz"),
                     list(titulo = labelInput("codevert"), id = "Kvert"),
                     list(titulo = labelInput("coderadar"), id = "Kradar")),
                     function(i) {
                       conditionalPanel(
                         condition = paste0("input.tabkmedias == 'tab", i$id, "'"),
                         h5(i$titulo),
                         aceEditor(paste0("fieldFunc", i$id), mode = "r", theme = "monokai",
                                   value = "", height = "50vh", readOnly = T))
                   })
              )
            )
          )
        )
      ),

      #Generar Reporte
      tabItem(
        tabName = "reporte",
        column(
          width = 5, 
          box(
            title = labelInput("reporte"), width = 12,
            textInput("textTitulo", value = "Sin Titulo", width = "100%", 
                      label = labelInput("titulo")),
            textInput("textNombre", value = "PROMiDAT", width = "100%",
                      label = labelInput("nombre")), hr(),
            downloadButton("descargar", labelInput("descargar"), 
                           class = "center-button"))
        ),
        column(
          width = 7,
          box(title = labelInput("codreporte"), width = 12, height = "50vh",
              status = "primary", solidHeader = TRUE, collapsible = TRUE,
              aceEditor("fieldCodeReport", mode="markdown", 
                        value='', height = "43vh"))),
        fluidRow(column(
          width = 12,
          box(title = labelInput("salida"), width = 12, height = "35vh", 
              verbatimTextOutput("txtreport"))))
      ),

      tabItem(
        tabName = "acercaDe",
        img(src="Logo.png",
            style=paste0("padding-bottom:20px;margin-left: auto;",
                         "margin-right: auto;display: block;width: 50%;")),
        infoBoxPROMiDAT(
          labelInput("copyright"), "PROMiDAT S.A.", icono = icon("copyright")),
        infoBoxPROMiDAT(
          labelInput("info"), tags$a(
            href="https://www.promidat.com/", style = "color:white;",
            target = "_blank", "https://www.promidat.com"), icono = icon("info")),
        infoBoxPROMiDAT(
          labelInput("version"), "1.0.1", icono = icon("file-code-o"))
      )
    ) #tabItems
  ) #dashboardBody
)) #UI
