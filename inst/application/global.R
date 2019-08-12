source('biblioteca.R', local = T)

###################### Variables ##############################################
#' Carga de Variables
#' @author Diego
#' @return
#' @export
#'
contador <<- 0
datos.originales <<- NULL
nombre.datos <<- ""
datos.reporte <<- list()
env.report <<- new.env()
env.report$codigo.reporte <- list()
def.colors <<- c("#F8766D", "#00BFC4", "#00BA38", "#C77CFF", "#00B0F6",
                 "#EEEE00", "#CD661D", "#006400","#EE82EE", "#000080")

###################### Funciones R ############################################
#' Funciones R
#' @author Diego
#' @return functions
#' @export
#'

codigo.prueba <- function() {
  return(paste0(
    "datos[, 'Survived'] <- as.factor(datos[, 'Survived'])\n", 
    "datos <- datos.disyuntivos(datos, 'Survived')\n", 
    "datos[, 'Pclass'] <- as.factor(datos[, 'Pclass'])\n",
    "datos <- subset(datos, select = -Name)\n",
    "datos <- datos.disyuntivos(datos, 'Sex')\n",
    "datos <- subset(datos, select = -Ticket)\n",
    "datos <- subset(datos, select = -Cabin)"))
}

tr <- function(text) {
  sapply(text, function(s) {
    elem <- ifelse(is.null(translation[[s]][[input$idioma]]), s, 
                   translation[[s]][[input$idioma]])
    Encoding(elem) <- enc
    elem
  }, USE.NAMES = F)
}

close.menu <- function(valor = T) {
  select <- 'a[href^="#shiny-tab-parte1"]'
  if(valor) {
    shinyjs::hide(selector = "ul.menu-open");
    shinyjs::disable(selector = select)
  } else {
    shinyjs::enable(selector = select)
  }
}

overwrite.cat <- function(){
  unlockBinding("cat", .BaseNamespaceEnv)

  .BaseNamespaceEnv$cat <- function(..., file = "", sep = " ", fill = FALSE,
                                    labels = NULL, append = FALSE) {
    file <- stderr()
    sep <- ""

    msg <- .makeMessage(..., domain = NULL, appendLF = TRUE)
    call <- sys.call()
    cond <- simpleMessage(msg, call)

    if (is.character(file))
      if (file == "")
        file <- stdout()
    else if (substring(file, 1L, 1L) == "|") {
      file <- pipe(substring(file, 2L), "w")
      on.exit(close(file))
    }
    else {
      file <- file(file, ifelse(append, "a", "w"))
      on.exit(close(file))
    }
    defaultHandler <- function(c) {
      base:::.Internal(cat(as.list(conditionMessage(c)), file, sep, fill, labels, append))
    }
    withRestarts({
      signalCondition(cond)
      defaultHandler(cond)
    }, muffleMessage = function() NULL)
    invisible()
  }

  lockBinding("cat",.BaseNamespaceEnv)
}

recover.cat <- function() {
  unlockBinding("cat", .BaseNamespaceEnv)

  .BaseNamespaceEnv$cat <- function (..., file = "", sep = " ", fill = FALSE, labels = NULL,
                                     append = FALSE)
  {
    if (is.character(file))
      if (file == "")
        file <- stdout()
      else if (substring(file, 1L, 1L) == "|") {
        file <- pipe(substring(file, 2L), "w")
        on.exit(close(file))
      }
      else {
        file <- file(file, ifelse(append, "a", "w"))
        on.exit(close(file))
      }
      .Internal(cat(list(...), file, sep, fill, labels, append))
  }

  lockBinding("cat",.BaseNamespaceEnv)
}

mostrarError <-  function(msg, n.num = NULL, n.cat = NULL) {
  if(!is.null(n.num)) {
    if(n.num < 1) {
      return(error.plot(tr("errornum")))
    }
  } 
  if (!is.null(n.cat)) {
    if(n.cat < 1) {
      return(error.plot(tr("errorcat")))
    }
  }
  shiny::showNotification(paste0("ERROR: ", msg), duration = 10, type = "error")
  return(NULL)
}

error.plot <- function(msg) {
  res <- ggplot(data.frame(x = c(2, 2.5, 3), y = c(2 ,3 ,2))) + 
    geom_polygon(mapping=aes(x=x, y=y), col="gold", fill="gold", alpha=0.3) +
    annotate("rect", xmin = 2.47, xmax = 2.53, ymin = 2.4, ymax = 2.8) +
    annotate("rect", xmin = 2.47, xmax = 2.53, ymin = 2.25, ymax = 2.35) +
    annotate("text", x = 2.5, y = 2.1, label = paste0("bold('", msg, "')"), 
             size = 8, parse = T) + 
    theme(
      panel.background = element_rect(fill = "transparent"),
      axis.title = element_blank(), axis.ticks = element_blank(),
      axis.text = element_blank()
    )
  return(res)
}

###################### Funciones Shiny ########################################
#' Funciones shiny
#' @author Diego
#' @return functions
#' @export
#'

dropNulls <- function (x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

labelInput <- function(inputId, value = ""){
  tags$span(`data-id` = inputId, value)
}

updateLabelInput <- function (session, labelid, value = NULL) {
  message <- dropNulls(list(labelid = labelid))
  if(length(labelid) == 1) {
    labelid <- list(labelid)
  }
  ifelse(
    is.null(value), sentvalue <- tr(labelid), 
    ifelse(length(value) == 1, sentvalue <- list(value), 
           sentvalue <- value))
  session$sendCustomMessage(
    type = 'updateLabel', 
    message = list(ids = labelid, values = sentvalue))
}

extract.code <- function(funcion) {
  code <- paste(head(eval(parse(text = funcion)), 100), collapse = "\n")
  code <- paste(funcion, "<-", code)
  return(code)
}

inputRadio <- function(inputId, value, isSelected) {
  res <- tags$input(type = "radio", name = inputId, value = value)
  if(isSelected) {
    res$attribs$checked <- "checked"
  }
  return(res)
}

checkSwitch <- function(id, label = NULL, name) {
  tags$div(
    class = "form-group", `data-shinyjs-resettable-type`="RadioButtons", 
    `data-shinyjs-resettable-value` = name,
    if(!is.null(label)) {
      tags$label(class = "control-label", `for` = id, `data-id` = label)
    },
    tags$div(
      `data-toggle`="buttons",
      tags$div(
        class = "btn-radiogroup",
        tags$button(
          class = "btn radiobtn btn-radioswitch",
          tags$span(class = "radio-btn-icon-yes", tags$i(class="glyphicon glyphicon-ok")),
          tags$span(class = "radio-btn-icon-no", tags$i(class="glyphicon glyphicon-remove")),
          tags$input(id=id, type="checkbox", style = 
                       "position: absolute;clip: rect(0,0,0,0);pointer-events: none;"),
          labelInput(name)
        )
      )
    )
  )
}

radioSwitch <- function(id, label = NULL, names, values = NULL, val.def = T) {
  if(is.null(values)) values <- c(TRUE, FALSE) 
  tags$div(
    class = "form-group", `data-shinyjs-resettable-type`="RadioButtons", 
    `data-shinyjs-resettable-value` = names[1],
    if(!is.null(label)) {
      tags$label(class = "control-label", `for` = id, `data-id` = label)
    },
    tags$div(
      class = "radioGroupButtons btn-group-container-sw", id = id, `data-toggle`="buttons",
      tags$div(
        class = "btn-radiogroup",
        tags$button(
          class = ifelse(val.def, "btn radiobtn btn-radioswitch active", 
                 "btn radiobtn btn-radioswitch"),
          tags$span(class = "radio-btn-icon-yes", tags$i(class="glyphicon glyphicon-ok")),
          tags$span(class = "radio-btn-icon-no", tags$i(class="glyphicon glyphicon-remove")),
          
          if(val.def) {
            tags$input(type="radio", autocomplete="off", name=id, value=values[1], checked = "checked",
                       style = "position: absolute;clip: rect(0,0,0,0);pointer-events: none;")
          } else {
            tags$input(type="radio", autocomplete="off", name=id, value=values[1],
                       style = "position: absolute;clip: rect(0,0,0,0);pointer-events: none;")
          },
          
          labelInput(names[1])
        )
      ),
      tags$div(
        class = "btn-radiogroup", role = "group", 
        tags$button(
          class = ifelse(val.def,"btn radiobtn btn-radioswitch", 
                 "btn radiobtn btn-radioswitch active"),
          tags$span(class = "radio-btn-icon-yes", tags$i(class="glyphicon glyphicon-ok")),
          tags$span(class = "radio-btn-icon-no", tags$i(class="glyphicon glyphicon-remove")),
          
          if(val.def) {
            tags$input(type="radio", autocomplete="off", name=id, value=values[2],
                       style = "position: absolute;clip: rect(0,0,0,0);pointer-events: none;")
          } else {
            tags$input(type="radio", autocomplete="off", name=id, value=values[2], checked = "checked",
                       style = "position: absolute;clip: rect(0,0,0,0);pointer-events: none;")
          },
          
          labelInput(names[2])
        )
      )
    )
  )
}

radioButtonsTr <- function(inputId, label, values, names){
  tags$div(
    id=inputId, class="form-group shiny-input-radiogroup shiny-input-container",
    tags$label(class = "control-label", `for` = inputId, labelInput(label)),
    tags$div(
      class="shiny-options-group",
      lapply(1:length(values), function(i) {
        tags$div(
          class="radio", 
          tags$label(
            inputRadio(inputId, values[i], i == 1),
            tags$span(labelInput(names[i]))
          )
        )
      })
    )
  )
}

options.run <- function(runid) {
  tags$div(
    style = "display: inline-block; width: 100%", 
    shiny::h4(labelInput("opciones"), 
              style = "float: left;margin-bottom: 0px;margin-top: 0px;"),
    tags$button(
      id = runid, type = "button", class = "run-button action-button", 
      icon("play"), tags$a(labelInput("ejecutar"), style = "color:white")))
}

tabsOptions <- function(
  botones = list(icon("gear"), icon("terminal")), widths = c(50, 100),
  heights = c(100, 50), tabs.content = list("", "")
  ) {
  res <- ""
  codeButtons <- ""
  cant <- length(botones)
  if(cant == 2) {widgets <- c("left", "right")}
  if(cant == 3) {widgets <- c("left", "center", "right")}
  if(cant == 4) {widgets <- c("left", "centerleft", "centeright", "right")}
  if(cant == 5) {
    widgets <- c("left", "centerleft", "center", "centeright", "right")}
  for (i in 1:cant) {
    res <- paste0(
      res, tags$div(
        class = paste0("box-option box-option-", widgets[i]),
        style = paste0("width:", widths[i], "%;height:", heights[i], "%;"),
        tabs.content[[i]]), "\n")
    codeButtons <- paste0(
      codeButtons, "<button style='width:", 100/cant, "%' data-widget='", 
      widgets[i], "'>", botones[[i]], "</button>\n")
  }
  res <- paste0(
    res, tags$div(
      class = "btn-options", style = "position:relative;", 
      width = "100%", HTML(codeButtons))
  )
  return(tags$div(HTML(res)))
}

codigo.monokai <- function(id, height) {
  shinyAce::aceEditor(id, mode = "r", theme = "monokai", 
                      value = "", readOnly = T, height = height)
}

infoBoxPROMiDAT <- function(titulo, valor, icono) {
  tags$div(
    class = "info-box bg-promidat",
    tags$span(class = "info-box-icon", icono),
    tags$div(class="info-box-content", 
             tags$span(class = "info-box-text", titulo),
             tags$span(class = "info-box-number", valor)
    )
  )
}

menu.idioma <- function() {
  tags$li(
    class = "nodisabled treeview", 
    tags$a(
      href = "#shiny-tab-tabdioma", 
      tags$i(class="fa fa-language"),
      labelInput("idioma"),
      tags$i(class="fa fa-angle-left pull-right")),
    tags$ul(
      class="treeview-menu", style="display: none;", `data-expanded`="Idioma",
      radioButtons('idioma', labelInput("selidioma"), 
                   c('Español'='es', 'English'='en')),
      tags$br()))
}

updateSelects <- function(datos) {
  vars <- list("numericas" = colnames(var.numericas(datos)),
               "categoricas" = colnames(var.categoricas(datos)),
               "todas" = colnames(datos))
  selects <- list(
    list(id = "sel.normal", choices = "numericas", tipo = "Selectize"),
    list(id = "select.var", choices = "numericas", tipo = "Selectize"),
    list(id = "sel.distribucion.num", choices = "numericas", tipo = "Select"),
    list(id = "sel.distribucion.cat", choices = "categoricas", tipo = "Select"),
    list(id = "sel.resumen", choices = "todas", tipo = "Select"),
    list(id = "selVert", choices = "numericas+", tipo = "Select"),
    list(id = "sel.Kvert", choices = "numericas+", tipo = "Select"),
    list(id = "sel.Kbar", choices = "categoricas", tipo = "Select"),
    list(id = "selBar", choices = "categoricas", tipo = "Select"))
  lapply(selects, function(sel) {
    if(sel$tipo == "Selectize"){
      updateSelectizeInput(session, sel$id, choices = vars[[sel$choices]])
    } else {
      updateSelectInput(session, sel$id, choices = vars[[sel$choices]])
    }
  })
  updateinitSelects("selVert", colnames(var.numericas(datos)))
  updateinitSelects("sel.Kvert", colnames(var.numericas(datos)))
  cant.num <- ncol(var.numericas(datos))
  ndef <- ifelse(cant.num < 5, cant.num, 5)
  updateSliderInput(session, "slider.npc", max = cant.num, value = ndef)
}

updateinitSelects <- function(id, choices) {
  vars <- c()
  vars[tr("todos")] <- "todos"
  vars <- c(vars, choices)
  updateSelectInput(session, id, choices = vars)
}

updateMenu <- function(datos = NULL, init = F) {
  element <- "#sidebarItemExpanded li"
  menu.values <- c(
    "[class^=treeview]",  " a[data-value=acp]", " a[data-value=agrupacion]",
    " a[data-value=kmedias]", " a[data-value=reporte]")
  
  lapply(menu.values, function(i){
    if(init || (is.null(datos) || ncol(datos) < 1)){
      addClass(class = "disabled", selector = paste0(element, i))
    } else {
      removeClass(class = "disabled", selector = paste0(element, i))
    }
  })
}

color.input <- function(id) {
  tags$div(
    tags$label(class='control-label', labelInput("selcolores")),
    shiny::fluidRow(
      lapply(1:10, function(i)
        tags$div(class = "select-color", colourpicker::colourInput(
          paste0(id, i), NULL, value = def.colors[i], allowTransparent = T))))
  )
}

mostrar.colores <- function(id, n) {
  for (i in 1:10) {
    if(i <= n) {
      shinyjs::show(paste0(id, i))
    } else {
      shinyjs::hide(paste0(id, i))
    }
  }
}

###################### Carga de Datos #########################################
#' Funciones Carga de Datos
#' @author Diego
#' @return functions
#' @export
#'
colnames.empty <- function(res) {
  res <- colnames(res)
  if (is.null(res)) {
    return("")
  }
  return(res)
}

var.numericas <- function(data){
  if(is.null(data)) {
    return(NULL)
  }
  res <- base::subset(data, select =
                        sapply(data, class) %in% c('numeric', 'integer'))
  return(res)
}

var.categoricas <- function(data){
  if(is.null(data)) {
    return(NULL)
  }
  res <- base::subset(data, select =
                        !sapply(data, class) %in% c('numeric', 'integer'))
  return(res)
}

datos.disyuntivos <- function(data, vars) {
  if(is.null(data)) {
    return(NULL)
  }
  cualitativas <- base::subset(data, select = colnames(data) %in% c(vars))
  data <- data[, !colnames(data) %in% vars]
  for (variable in colnames(cualitativas)) {
    for (categoria in unique(cualitativas[, variable])) {
      nueva.var <- as.numeric(cualitativas[, variable] == categoria)
      data <- cbind(data, nueva.var)
      colnames(data)[length(colnames(data))] <-
        paste0(variable, '.', categoria)
    }
  }
  return(data)
}

code.NA <- function(deleteNA = T) {
  res <- ifelse(
    deleteNA, "datos.originales <<- na.omit(datos.originales)\n",
    paste0(
      "Mode <- function(x) {\n  x[which.max(summary(x))]\n}\n",
      "for (variable in colnames(datos.originales)) {\n",
      "  if(any(is.na(datos.originales[, variable]))){\n",
      "    ifelse(class(datos.originales[, variable]) %in% c('numeric', 'integer'),\n",
      "      datos.originales[, variable][is.na(datos.originales[, variable])] <<- \n",
      "        mean(datos.originales[, variable], na.rm = T),\n",
      "      datos.originales[, variable][is.na(datos.originales[, variable])] <<- \n",
      "        Mode(datos.originales[, variable]))",
      "\n  }\n}"))
  return(res)
}

code.carga <- function(nombre.filas = T, ruta = NULL, separador = ";",
                       sep.decimal = ",", encabezado = T, incluir.NA = F) {
  if(!is.null(ruta)) {
    ruta <- gsub("\\", "/", ruta, fixed = T)
  }
  res <- paste0(
    "datos.originales <<- read.table('", ruta, "', header=", encabezado, 
    ", sep='", separador, "', dec = '", sep.decimal, "'", 
    ifelse(nombre.filas, ", row.names = 1", ""), ")")
  res <- paste0(res, "\n", code.NA(incluir.NA))
  return(res)
}

code.trans <- function(variable, nuevo.tipo) {
  if(nuevo.tipo == "categorico"){
    return(paste0(
      "datos[, '", variable, "'] <- as.factor(datos[, '", variable, "'])"))
  } else if(nuevo.tipo == "numerico") {
    return(paste0(
      "datos[, '", variable, "'] <- as.numeric(sub(',', '.', datos[, '",
      variable, "'], fixed = TRUE))"))
  } else {
    es.factor <- ifelse(
      class(datos.originales[, variable]) %in% c('numeric', 'integer'),
      paste0("datos[, '", variable, "'] <- as.factor(datos[, '", variable, 
             "']) \n"), "")
    return(paste0(
      es.factor, "datos <- datos.disyuntivos(datos, '", variable,"')"))
  }
}

code.desactivar <- function(variable){
  return(paste0("datos <- subset(datos, select = -", variable, ")"))
}

###################### Estadisticas Basicas ###################################
#' Funciones Resumen Numérico
#' @author Diego
#' @return functions
#' @export
#'
resumen.numerico <- function(data, variable) {
  datos.numericos <- list(
    Q1 = list(
      id = "q1", Label = tags$span(`data-id`="q1", tr("q1")), color = "green",
      Value = format(round(quantile(data[, variable], .25), 3), scientific = F)
    ),
    Mediana = list(
      id = "mediana", Label = tags$span(`data-id`="mediana", tr("mediana")),
      Value = format(round(median(data[, variable]), 3), scientific = F), 
      color = "orange"),
    Q3 = list(
      id = "q3", Label = tags$span(`data-id`="q3", tr("q3")), color = "maroon",
      Value = format(round(quantile(data[, variable], .75), 3), scientific = F)
    ),
    Minimo = list(
      id = "minimo", Label = tags$span(`data-id`="minimo", tr("minimo")), 
      Value = format(round(min(data[, variable]), 3), scientific = F), 
      color = "red"),
    Promedio = list(
      id = "promedio", Label = tags$span(`data-id`="promedio", tr("promedio")),
      Value = format(round(mean(data[, variable]), 3), scientific = F), 
      color = "blue"),
    Maximo = list(
      id = "maximo", Label = tags$span(`data-id`="maximo", tr("maximo")), 
      Value = format(round(max(data[, variable]), 3), scientific = F), 
      color = "purple"),
    DS <- list(
      id = "ds", Label = tags$span(`data-id`="ds", tr("ds")), color = "yellow",
      Value = format(round(sd(data[, variable]), 3), scientific = FALSE, nsmall = 3)
    )
  )

  res <- lapply(datos.numericos, function(i) {
    tags$div(
      class='shiny-html-output col-sm-6 shiny-bound-output', id=i$id,
      tags$div(
        class=paste0('small-box bg-', i$color),
        tags$div(class='inner', tags$h3(i$Value), tags$p(i$Label)),
        tags$div(class='icon-large', tags$i(class=i$icon))
      )
    )
  })
  return(res)
}

resumen.categorico <- function(data, variable){
  color <- c("red","yellow","aqua","navy","teal","olive","purple","maroon",
             "black","blue","lime","orange","light-blue","green","fuchsia")
  datos.categoricos <- levels(data[, variable])
  res <- lapply(datos.categoricos, function(i) {
    tags$div(
      class='shiny-html-output col-sm-6 shiny-bound-output', id=paste0(variable, i),
      tags$div(
        class=paste0('small-box bg-', sample(color, 1)),
        tags$div(class='inner', tags$h3(summary(data[, variable])[i]), tags$p(i))
      )
    )
  })
  return(res)
}

#' Funciones Normal
#' @author Diego
#' @return functions
#' @export
#'
default.normal <- function(vars = NULL, color = "#00FF22AA", 
                           labelcurva = "Curva Normal") {
  if(is.null(vars)){
    return(NULL)
  } else {
    return(paste0(
      "promedio <- mean(datos[, '", vars, "']) \n",
      "desviacion <- sd(datos[, '", vars, "']) \n",
      "values <- dnorm(datos[, '", vars, "'], mean = promedio, sd = desviacion) \n",
      "values <- c(values, hist(datos", "[, '", vars, "'],  plot = F)$density) \n",
      "hist(datos[, '", vars, "'], col = '", color, "', border=F, axes=F,\n",
      "  freq = F, ylim = range(0, max(values)), ylab = '', \n",
      "  main = '", vars, "') \n",
      "axis(1, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025) \n",
      "axis(2, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025) \n",
      "curve(dnorm(x, mean = promedio, sd = desviacion), add=T, col='blue', lwd=2)\n",
      "legend('bottom', legend = '", labelcurva, "', col = 'blue', lty=1, cex=1.5)"))
  }
}

default.calc.normal <- function(
  labelsi = "Positiva", labelno = "Negativa", 
  labelsin = "Sin Asimetría") {
  return(paste0(
    "calc <- lapply(var.numericas(datos), function(i) fisher.calc(i)[1]) \n",
    "calc <- as.data.frame(calc) \n",
    "calc <- rbind(calc, lapply(calc, function(i) ifelse(i > 0, '", labelsi,
    "',\n  ifelse(i < 0, '", labelno, "', '", labelsin, "')))) \n",
    "calc <- t(calc)\ncalc"))
}

#' Funciones Dispersión
#' @author Diego
#' @return functions
#' @export
#'
default.disp <- function(vars = NULL, color = "#FF0000AA") {
  if(length(vars) < 2) {
    return("")
  } else if(length(vars) == 2) {
    return(paste0(
      "ggplot(data = datos, aes(x = ", vars[1], ", y = ", vars[2],
      ", label = rownames(datos))) +\n", "geom_point(color = '",
      color, "', size = 3) + geom_text(vjust = -0.7) + theme_minimal()"))
  } else{
    return(paste0(
      "scatterplot3d(datos[, '", vars[1], "'], datos[, '", vars[2], 
      "'], datos[, '", vars[3], "'], pch = 16, color = '", color, "')"))
  }
}

#' Funciones Distribuciones
#' @author Diego
#' @return functions
#' @export
#'
distribucion.numerico <- function(var, nombre.var, color){
  nf <- graphics::layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE),  height = c(3,1))
  par(mar=c(3.1, 3.1, 1.1, 2.1))
  hist(var, col = color, border=F, axes=F, main = nombre.var)
  axis(1, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025)
  axis(2, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025)
  boxplot(var, col = color, boxcol = color, boxlty = 1, boxlwd = 3,
          boxwex = 1.5, edcol = color, medlty = 1, medlwd = 8, axes=F,
          medcol = color, whiskcol = color, whisklty = 3, staplecol = color,
          staplelty = 1, staplelwd = 3, horizontal=TRUE, outline=TRUE,
          frame=F, whisklwd = 2.5, outpch = 20, outcex = 1.5, outcol = 'red')
}

distribucion.categorico <- function(var) {
  colores <- sapply(levels(var),
                    function(i) rgb(runif(1), runif(1), runif(1), 0.8))
  data <- data.frame(label = levels(var), value = summary(var))
  ggplot(data, aes(label, value)) +
    geom_bar(stat = 'identity', fill = colores) +
    geom_text(aes(label = value, y = value), vjust = -0.5, size = 4) +
    theme_minimal()
}

def.code.num <- function(variable, color) {
  paste0("distribucion.numerico(datos[, '", variable, "'], '",
         variable, "', color = '", color,"')")
}

def.code.cat <- function(
  variable, titulox = tr("cantidadcasos"), 
  tituloy = tr("categorias")) {
  paste0(
    "distribucion.categorico(datos[, '", variable,"']) + ", 
    "labs(title = '", variable, "', x = '", 
    titulox, "', y = '", tituloy, "')")
}

#' Funciones Correlaciones
#' @author Diego
#' @return functions
#' @export
#'
correlaciones <- function(metodo = 'circle', tipo = "lower") {
  return(paste0(
    "corrplot(cor(var.numericas(datos)), method='", metodo, "', shade.col=NA,",
    " tl.col='black',\n", "         tl.srt=20, addCoef.col='black', order='AOE',", 
    " type = '", tipo, "')"))
}

###################### PCA ####################################################
#' Modelo PCA
#' @author Diego
#' @return functions
#' @export
#'
def.pca.model <- function(data = "datos", scale.unit = T, npc = 5) {
  return(paste0("pca.modelo <- PCA(var.numericas(", data, "), scale.unit = ",
                scale.unit, ", ncp = ", npc, ", graph = FALSE)"))
}

#' PCA Individuos
#' @author Diego
#' @return functions
#' @export
#'
pca.individuos <- function(ind.cos = 0, color = '#696969', ejes = c(1, 2)) {
  return(paste0(
    "fviz_pca_ind(pca.modelo, pointsize = 2, pointshape = 16, axes = c(",
    ejes, "),\n", "    col.ind = '", color, "', select.ind = list(cos2 = ", 
    ind.cos, ")) + labs(title = '')"))
}

#' PCA Variables
#' @author Diego
#' @return functions
#' @export
#'
pca.variables <- function(var.cos = 0, color = 'steelblue', ejes = c(1, 2)) {
  return(paste0(
    "fviz_pca_var(pca.modelo, col.var= '", color, 
    "', select.var = list(cos2 = ", var.cos, "),\n",
    "    axes = c(", ejes, ")) + labs(title = '')"))
}

#' PCA Sobreposición
#' @author Diego
#' @return functions
#' @export
#'
pca.sobreposicion <- function(ind.cos = 0, var.cos = 0, col.ind = '#696969',
                              col.var = 'steelblue', ejes = c(1, 2)) {
  return(paste0(
    "fviz_pca_biplot(pca.modelo, pointsize = 2, pointshape = 16, col.var = '",
    col.var, "', \n", "    col.ind = '", col.ind, 
    "', select.ind = list(cos2 = ", ind.cos, "), ", 
    "select.var = list(cos2 = ", var.cos, "),\n axes = c(", 
    ejes, ")) + labs(title = '')"))
}

#' PCA Varianza Explicada por cada eje
#' @author Diego
#' @return functions
#' @export
#'
code.pca.vee <- function(titulo, titulox, tituloy){
  return(paste0(
    "fviz_eig(pca.modelo, addlabels = TRUE, \n", 
    "    ylab = '", tituloy, "',\n", 
    "    xlab = '", titulox, "', main = '", titulo, "')"))
}

#' PCA Cosenos cuadrados de los individuos
#' @author Diego
#' @return functions
#' @export
#'
code.pca.cci <- function(titulo, tituloy) {
  return(paste0(
    "fviz_cos2(pca.modelo, choice = 'ind', axes = 1:2, top = 20) +\n",
    "  labs(y = '", tituloy, "',\n",
    "       title = '", titulo, "')"))
}

#' PCA Cosenos cuadrados de las variables
#' @author Diego
#' @return functions
#' @export
#'
code.pca.ccv <- function(titulo, tituloy) {
  return(paste0(
    "fviz_cos2(pca.modelo, choice = 'var', axes = 1:2) +\n",
    "  labs(y = '", tituloy, "',\n",
    "       title = '", titulo, "')"))
}

#' PCA Correlación de las variables con las componentes principales
#' @author Diego
#' @return functions
#' @export
#'
code.pca.cvp <- function(metodo = "circle", titulo) {
  return(paste0(
    "corrplot(pca.modelo$var$cor, is.corr=FALSE, mar=c(0,0,1,0), shade.col=NA,\n",
    "         tl.col='black', addCoef.col='black', method='", metodo,"',\n",
    "         title = '", titulo, "')"))
}

#' PCA Contributions of variables
#' @author Diego
#' @return functions
#' @export
#'
code.pca.pc1 <- function(titulo, tituloy, dim = "1") {
  return(paste0(
    "fviz_contrib(pca.modelo, choice = 'var', axes = ", dim, ", top = 20) +\n",
    "  labs(y = '", tituloy, "', title = '", paste0(titulo, "-", dim), "')"))
}

###################### Clustering Jerarquico ##################################
#' Modelo
#' @author Diego
#' @return functions
#' @export
#'
def.model <- function(cant = "as.numeric(input$cant.cluster)", dist.method = "euclidean", 
                      hc.method = "complete", centrar = T) {
  if(centrar) {
    init <- "datos.num <- as.data.frame(scale(var.numericas(datos)))\n"
  } else {
    init <- "datos.num <- var.numericas(datos)\n"
  }
  return(paste0(
    init, "modelo <- hclust(dist(datos.num, method = '",
    dist.method, "'), method = '", hc.method, "')\n",
    "clusters <- as.factor(cutree(modelo, k = ", cant, "))\n",
    "centros <- calc.centros(var.numericas(datos), clusters)\n",
    "hc.modelo <- list(modelo = modelo, clusters = clusters, centros = centros)"))
}

calc.centros <- function(data, clusteres) {
  if(is.null(clusteres)) return(NULL)
  real <- lapply(levels(clusteres), function(i)
    colMeans(data[clusteres == i, ]))
  real <- as.data.frame(do.call('rbind', real))
  porcentual <- apply(real, 2, function(i) scales::rescale(i, to = c(0, 100)))
  porcentual <- as.data.frame(porcentual)
  return(list(real = real, porcentual = porcentual))
}

#' Inercia
#' @author Diego
#' @return functions
#' @export
#'
centros.total <- function(DF) {
  apply(DF, 2, mean)
}

calc.inercia <- function(total, individuo){
  return(inercia(0, 1, total, individuo))
}
inercia <- function(suma, i, total, individuo){
  if(i > length(total)){
    return(as.double(suma))
  }
  inercia(suma + ((total[i] - individuo[i])^2), i+1, total, individuo)
}

inercia.total <- function(DF) {
  c.total <- centros.total(DF)
  res <- apply(DF, 1, function(i) calc.inercia(c.total, i))
  return(sum(res))
}

BP <- function(DF, modelo, cant) {
  BP2(0, 1, DF, centros.total(DF), cant, cutree(modelo, k = cant))
}
BP2 <- function(suma, i, DF, c.total, cant, clusters) {
  if(i > cant){
    return(suma)
  }
  BP2(suma + (length(clusters[clusters == i]) *
                calc.inercia(c.total, centros.total(DF[clusters == i, ]))),
      i + 1, DF, c.total, cant, clusters)
}

WP <- function(DF, modelo, cant){
  clusters <- cutree(modelo, k = cant)
  centros.cluster <- lapply(1:cant, function(i) centros.total(DF[clusters == i, ]))
  res <- sapply(1:nrow(DF), function(i) 
    calc.inercia(DF[i, ], centros.cluster[[clusters[i]]]))
  return(sum(res))
}

#' Dendograma
#' @author Diego
#' @return functions
#' @export
#'
diagrama <- function(colores = "'steelblue'") {
  return(paste0(
    "dendograma <- dendro_data(hc.modelo$modelo, type='rectangle')\n",
    "order.labels <- data.frame(label=names(hc.modelo$clusters), clusters = hc.modelo$clusters)\n",
    "dendograma[['labels']] <- merge(dendograma[['labels']], order.labels, by='label')\n",
    "ggplot() + geom_segment(data=dendograma$segments, aes(x=x, y=y, xend=xend, yend=yend)) +\n",
    "  geom_text(data=dendograma$label, aes(x, y, label = label, hjust=1.1, color = clusters), \n",
    "            size = 4, angle = 90) +\n",
    "  scale_color_manual(values = c(", paste(colores, collapse = ","),
    ")) + expand_limits(y=-2)", " +\nlabs(x = '', y = '') + theme_minimal()"))
}


###################### K-medias ###############################################
#' Modelo
#' @author Diego
#' @return functions
#' @export
#'
def.k.model <- function(cant = "as.numeric(input$cant.kmeans.cluster)", iter.max = 200, 
                        nstart = 300, algorithm = "Hartigan-Wong", centrar = T) {
  if(centrar) {
    init <- "datos.num <- as.data.frame(scale(var.numericas(datos)))\n"
  } else {
    init <- "datos.num <- var.numericas(datos)\n"
  }
  return(paste0(init, "k.modelo <- kmeans(datos.num, centers = ",
                cant, ",\n    ", "iter.max = ", iter.max,", nstart = ",
                nstart,", algorithm = '", algorithm ,"')"))
}

#' Jambu
#' @author Diego
#' @return functions
#' @export
#'
calc.maxK <- function(datos) {
  ifelse(nrow(datos) < 40, return(as.integer(nrow(datos)/2)), return(20))
}

def.code.jambu <- function(k, metodo = "wss") {
  xtitle <- tr("cantcluster")
  ifelse(
    metodo == "wss", ytitle <- tr("inerciaintra"), ytitle <- tr("silhouette"))
  return(paste0(
    "Datos.Escalados <- scale(var.numericas(datos))\n",
    "fviz_nbclust(Datos.Escalados, kmeans, method = '", metodo, "', k.max = ",
    k, ") +\nlabs(title = '', x = '", xtitle, "', y = '", ytitle, "')"))
}


###################### Clustering Jerarquico & K-medias #######################
#' Panel Inercia
#' @author Diego
#' @return functions
#' @export
#'
panel.inercia <- function(modelo, cant.clusters, datos = NULL, esHC = T, centrar = F) {
  if (esHC) {
    if(centrar) datos <- as.data.frame(scale(var.numericas(datos)))
    total.clase <- sum(scale(var.numericas(datos), scale = FALSE)^2)
    inter.clase <- BP(var.numericas(datos), modelo, cant.clusters)
    intra.clase <- total.clase - inter.clase
  } else {
    total.clase <- modelo$totss
    inter.clase <- modelo$betweenss
    intra.clase <- modelo$tot.withinss
  }

  datos.numericos <- list(
    WP = list(
      id = "WP",
      Label = tags$span(`data-id`="inerciaintra", tr("inerciaintra")),
      Value = format(intra.clase, scientific = FALSE), color = "red"),
    BP = list(
      id = "BP", 
      Label = tags$span(`data-id`="inerciainter", tr("inerciainter")),
      Value = format(inter.clase, scientific = FALSE), color = "green"),
    total = list(
      id = "total", 
      Label = tags$span(`data-id`="inercia", tr("inercia")),
      Value = format(total.clase, scientific = FALSE), color = "blue")
  )

  res <- lapply(datos.numericos, function(i) {
    tags$div(
      class='shiny-html-output col-sm-12 shiny-bound-output', id=i$id,
      tags$div(
        class = paste0('small-box bg-', i$color), 
        tags$div(class='inner', tags$h3(i$Value), tags$p(i$Label)),
        tags$div(class='icon-large', tags$i(class=i$icon))
      )
    )
  })
  return(res)
}

#' Interpretación Mapa
#' @author Diego
#' @return functions
#' @export
#'
cluster.mapa <- function(colores = "'steelblue'", esHC = T) {
  code.clusters <-
    ifelse(esHC, "hc.modelo$clusters", "as.factor(k.modelo$cluster)")
  return(paste0(
    "fviz_pca_biplot(pca.modelo, col.ind = ", code.clusters, ",\n",
    "  col.var = 'steelblue', legend.title = 'Cluster') + labs(title = '') +\n", 
    "  scale_color_manual(values = c(", paste(colores, collapse = ","), "))"))
}

#' Interpretación Horizontal
#' @author Diego
#' @return functions
#' @export
#'
centros.horizontal.todos <- function(centros){
  colnames(centros) <- sapply(c(1:ncol(centros)), function(i)
    paste0('Cluster ', i))
  var <- row.names(centros)
  centros <- cbind(centros, var)
  centros <- melt.data.frame(centros, id.vars = 'var')
  ggplot(centros, aes(x=var, y=value)) +
    geom_bar(stat='identity', position='dodge', show.legend = F) +
    labs(x = '', y = '') + facet_wrap(~variable) + coord_flip() +
    theme(text = element_text(size = 20)) + aes(fill = variable)
}

cluster.horiz <- function(sel = "1", color = "steelblue", esHC = T, porc = T) {
  code.centros <-
    ifelse(esHC, "centros <- as.data.frame(hc.modelo$centros$real)",
           "centros <- as.data.frame(calc.centros(var.numericas(datos), as.factor(k.modelo$cluster))$real)")
  ifelse(porc, 
         code.centros <- paste0(code.centros, "\ncentros <- as.data.frame(t(apply(centros, 2, function(i) i/max(abs(i)))))"),
         code.centros <- paste0(code.centros, "\ncentros <- as.data.frame(t(centros))")
  )
  if(sel == "todos") {
    return(paste0(
      code.centros, "\ncentros.horizontal.todos(centros) +\n",
      "  scale_fill_manual(values = c(", paste(color, collapse = ","), "))"))
  } else {
    return(paste0(
      code.centros, "\nggplot(data = centros, aes(x = row.names(centros), ",
      "y = centros[, ", sel, "])) +\n",
      "  geom_bar(stat = 'identity', fill = ", color[as.numeric(sel)], ") +\n",
      "  scale_y_continuous(expand = c(.01,0,0,0)) +\n",
      "  labs(x = '', y = '') + coord_flip() + theme_minimal()"))
  }
}

#' Interpretación Vertical
#' @author Diego
#' @return functions
#' @export
#'
centros.vertical.todos <- function(centros){
  cluster <- c(1:nrow(centros))
  centros <- cbind(centros, cluster)
  centros <- melt.data.frame(centros, id.vars = 'cluster')
  ggplot(centros, aes(x=variable, y=value, fill=factor(cluster))) +
    geom_bar(stat='identity', position='dodge') + labs(x = '', y = '')
}

cluster.vert <- function(sel = "1", colores = "'steelblue'", esHC = T, porc = T) {
  code.centros <-
    ifelse(esHC, "centros <- hc.modelo$centros$real",
           "centros <- calc.centros(var.numericas(datos), as.factor(k.modelo$cluster))$real")
  if(porc) code.centros <- paste0(code.centros, "\ncentros <- data.frame(apply(centros, 2, function(i) i/max(abs(i))))")
  if(sel == "todos") {
    return(paste0(
      code.centros, "\ncentros.vertical.todos(centros) +",
      " theme_minimal() +\n  scale_fill_manual('Cluster', values = c(",
      paste(colores, collapse = ","), "))"))
  } else {
    return(paste0(
      code.centros, "\nggplot(data = centros, aes(x = row.names(centros), ",
      "y = centros[, '", sel, "'], fill = row.names(centros))) +\n",
      "  geom_bar(stat = 'identity') + labs(x = '', y = '') +\n",
      "  scale_fill_manual('Cluster', values = c(",
      paste(colores, collapse = ","), ")) + theme_minimal()"))
  }
}

#' Interpretación Radar
#' @author Diego
#' @return functions
#' @export
#'
centros.radar <- function(centros){
  res <- melt.array(t(centros), varnames = c('variables', 'clusteres'))
  res <- res[order(res$variables, decreasing = F), ]
  res$clusteres <- as.character(res$clusteres)
  ggplot(res, aes(x = variables, y = value)) +
    geom_polygon(aes(group = clusteres, color = clusteres, fill = clusteres),
                 alpha=0.3, size = 1, show.legend = FALSE) +
    geom_point(aes(group = clusteres, color = clusteres), size = 3) +
    theme(panel.background = element_rect(fill = 'transparent'),
          plot.background = element_rect(fill = 'transparent'),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = '#dddddd'),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_blank(), axis.ticks = element_blank()) +
    scale_y_continuous(limits=c(-10, 100), breaks=c(0, 25, 50, 75, 100)) +
    ggtitle('') + xlab('') + ylab('') +
    geom_text(aes(x = 0.5, y = 0, label = '0%'), size = 3.5,
              colour = '#dddddd', family = 'Arial') +
    geom_text(aes(x = 0.5, y = 25, label = '25%'), size = 3.5,
              colour = '#dddddd', family = 'Arial') +
    geom_text(aes(x = 0.5, y = 50, label = '50%'), size = 3.5,
              colour = '#dddddd', family = 'Arial') +
    geom_text(aes(x = 0.5, y = 75, label = '75%'), size = 3.5,
              colour = '#dddddd', family = 'Arial') +
    geom_text(aes(x = 0.5, y = 100, label = '100%'), size = 3.5,
              colour = '#dddddd', family = 'Arial') +
    ggproto("CordRadar", CoordPolar, theta = "x", r = "y", 
            start = 0, direction = sign(1))
}

cluster.radar <- function(colores = "'steelblue'", esHC = T){
  code.centros <-
    ifelse(
      esHC, "centros <- hc.modelo$centros$porcentual",
      paste0(
        "centros <- calc.centros(var.numericas(datos), as.factor(k.modelo$cluster))$porcentual"))
  return(paste0(
    code.centros, "\ncentros.radar(centros) + \n",
    "scale_color_manual('Clusters', values = c(", paste(colores, collapse = ","),
    ")) +\nscale_fill_manual('Clusters', values = c(",
    paste(colores, collapse = ","),"))"))
}

#' Interpretación Categóricos
#' @author Diego
#' @return functions
#' @export
#'
cluster.cat <- function(var, colores = "'steelblue'", esHC = T, porc = T) {
  code.clusters <-
    ifelse(esHC, "hc.modelo$clusters", "as.factor(k.modelo$cluster)")
  res <- paste0("NDatos <- cbind(datos, Cluster = ", code.clusters, ")\n")
  if(!porc) {
    return(paste0(
      res, 
      "plot(ggplot(NDatos, aes(x = ", var, ")) + geom_bar(aes(fill = Cluster)) +",
      "\n  scale_fill_manual('Cluster', values = c(",
      paste(colores, collapse = ","),")) +\n  ",
      "facet_wrap(~Cluster, labeller = label_both) +\n  ",
      "theme(text = element_text(size = 15)) +\n  ",
      "labs(x = '', y = '') + guides(fill = F))"
    ))
  } else {
    return(paste0(
      res, 
      "aux <- sapply(1:length(unique(NDatos$Cluster)), function(i) sum(NDatos$Cluster == i))\n",
      "ggplot(NDatos, aes(x = Cluster, fill = ", var, ")) + geom_bar(position = 'fill') +\n", 
      "geom_text(aes(label = paste0(..count.., '\n', scales::percent(..count../aux[..x..]))),\n", 
      "          stat = 'count', position = position_fill(vjust = 0.5)) +\n", 
      "coord_flip() + labs(y = '') +\n", 
      "theme(axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 20)) + \n",
      "scale_fill_brewer(palette = 'Dark2')"))
  }
}

clusters.variable <- function(datos, clusters, idioma, esHC = T) {
  tryCatch({
    if(esHC) {
      ifelse(idioma == "es", aux <- "CJ.", aux <- "HC.")
      cluster.var <- as.factor(paste0(aux, clusters))
    } else {
      ifelse(input$idioma == "es", aux <- "Kmedias.", aux <- "Kmeans.")
      cluster.var <- as.factor(paste0("K", clusters))
    } 
    datos[[paste0(aux, length(levels(cluster.var)))]] <- cluster.var
    updateSelects(datos)
    datos.reporte[[nombre.datos]] <<- datos
    shiny::showNotification(tr("msjclusters"), duration = 5, type = "message")
    return(datos)
  }, error = function(e) {
    mostrarError(e)
  })
}

###################### Reporte ################################################
#' Reporte
#' @author Diego
#' @return functions
#' @export
#'
init.replist <- function(datos) {
  env.report$codigo.reporte[[datos]][["basico"]]  <- list()
  env.report$codigo.reporte[[datos]][["acp"]]     <- list()
  env.report$codigo.reporte[[datos]][["rephc"]]   <- list()
  env.report$codigo.reporte[[datos]][["kmedias"]] <- list()
}

createLogBasico <- function(datos, titulo, codigo, vars = NULL) {
  if(is.null(vars)){
    env.report$codigo.reporte[[datos]][["basico"]][[titulo]] <- codigo
  } else {
    env.report$codigo.reporte[[datos]][["basico"]][[titulo]][[vars]] <- codigo
  }
}

createLogACP <- function(datos, codigo, rep.modelo, vars = NULL) {
  aux <- paste0("centrar:", rep.modelo[1], ";dimensiones:", rep.modelo[2])
  if(is.null(env.report$codigo.reporte[[datos]][["acp"]][[aux]])){
    env.report$codigo.reporte[[datos]][["acp"]][[aux]] <- list()
  }
  env.report$codigo.reporte[[datos]][["acp"]][[aux]][[vars]] <- codigo
}

createLogCJ <- function(datos, codigo, rep.modelo, vars = NULL) {
  aux <- paste0("Clusters:", rep.modelo[1], ";distancia:", rep.modelo[2],
                ";metodo:", rep.modelo[3])
  if(is.null(env.report$codigo.reporte[[datos]][["rephc"]][[aux]])){
    env.report$codigo.reporte[[datos]][["rephc"]][[aux]] <- list()
  }
  env.report$codigo.reporte[[datos]][["rephc"]][[aux]][[vars]] <- codigo
}

createLogK <- function(datos, codigo, rep.modelo, vars = NULL) {
  aux <- paste0("Clusters:", rep.modelo[1], ";iter:", rep.modelo[2],
                ";nstart:", rep.modelo[3], ";algoritmo:", rep.modelo[4])
  if(is.null(env.report$codigo.reporte[[datos]][["kmedias"]][[aux]])) {
    env.report$codigo.reporte[[datos]][["kmedias"]][[aux]] <- list()
  }
  env.report$codigo.reporte[[datos]][["kmedias"]][[aux]][[vars]] <- codigo
}

def.reporte <- function(titulo = "Sin Titulo", nombre = "PROMiDAT") {
  paste0(
    "---\n", "title: '", titulo, "'\n", "author: '", nombre, "'\n",
    "date: ", Sys.Date(), "\n", "output:\n  word_document:\n",
    "    df_print: paged\n---\n\n",
    "```{r setup, include=FALSE}\n",
    "knitr::opts_chunk$set(echo = FALSE,  fig.height = 10, fig.width = 15)\n",
    "```\n\n",
    "```{r message=FALSE, warning=FALSE, eval = FALSE}\n",
    "library(ggplot2)\nlibrary(stringi)\n",
    "library(rmarkdown)\nlibrary(factoextra)\n```\n\n", 
    "```{r}\n", extract.code("var.numericas"), "\n\n", 
    extract.code("var.categoricas"), "\n\n", extract.code("datos.disyuntivos"),
    "\n\n", extract.code("distribucion.numerico"), "\n\n", 
    extract.code("distribucion.categorico"), "\n\n", 
    extract.code("calc.centros"), "\n\n",
    extract.code("centros.horizontal.todos"), "\n\n", 
    extract.code("centros.vertical.todos"), "\n\n",  
    extract.code("centros.radar"), "\n```\n\n"
  )
}

limpiar.titulos <- function(x) {
  x <- gsub(x, pattern = "ward.D2", replacement = "wardD2")
  x <- unlist(strsplit(x, split = "\\."))
  if(length(x) == 3) {
    x <- c(x[1], paste(x[2:3], collapse = "."))
  }
  x[!grepl(x, pattern = ":")] <- tr(x[!grepl(x, pattern = ":")])
  params <- x[grepl(x, pattern = ":")]
  params <- sapply(params, function(i) {
    aux <- unlist(strsplit(i, split = ";"))
    nombres <- tr(gsub(aux, pattern = ":.*", replacement = ""))
    nombres[grepl(nombres, pattern = "=")] <- 
      paste(tr(unlist(strsplit(nombres[grepl(nombres, pattern = "=")],
                               split = "="))), collapse = " = ")
    nombres[!grepl(nombres, pattern = ":")] <- 
      paste0(nombres[!grepl(nombres, pattern = ":")], ":")
    valores <- gsub(aux, pattern = ".*:", replacement = "")
    paste(nombres, valores, collapse = ", ")
  })
  x[grepl(x, pattern = ":")] <- params
  x <- paste("### ", x, "\n\n", collapse = "")
  return(x)
}

user.reporte <- function() {
  res <- ""
  for (rep.datos in names(env.report$codigo.reporte)) {
    res <- paste0(
      res, "\n\n# ", tr("data"), ": ", rep.datos, "\n\n", 
      "```{r echo=FALSE}\ndatos.originales <<- datos.reporte[['", 
      rep.datos, "']]\ndatos <<- datos.originales\n```\n")
    for (analisis in names(env.report$codigo.reporte[[rep.datos]])) {
      rep.codigos <- unlist(env.report$codigo.reporte[[rep.datos]][[analisis]])
      if(!is.null(rep.codigos) & (analisis == "basico" | analisis == "acp" | length(rep.codigos) > 1)) {
        for (i in 1:length(rep.codigos)) {
          res <- paste0(res, "\n\n## ", tr(analisis), "\n\n")
          rep.titulos <- limpiar.titulos(names(rep.codigos)[i])
          res <- paste0(res, rep.titulos, "```{r}\n", rep.codigos[i], "\n```")
        }
      }
    }
  }
  return(res)
}

###################### Corrección tildes ######################################
info.sys <- .Platform$OS.type
if(toupper(info.sys) != "WINDOWS") {
  enc <<- "utf8"
} else {
  enc <<- "UTF-8"
}

