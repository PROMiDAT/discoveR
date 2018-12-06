
###################### Variables ##############################################
#' Carga de Variables
#' @author Diego
#' @return
#' @export
#'
contador <<- 0
datos <<- NULL
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

tr <- function(text) { 
  sapply(text, function(s) {
    ifelse(is.null(translation[[s]][[input$idioma]]), s,
           translation[[s]][[input$idioma]])
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

recover.cat <- function(){
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

error.variables <- function(idioma = "es", num = T){
  if(num){
    img <- raster::stack(paste0("www/", idioma, "_errorNum.png"))
  }else{
    img <- raster::stack(paste0("www/", idioma, "_errorCat.png"))
  }
  raster::plotRGB(img)
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
  res <- tags$input(type="radio", name=inputId, value=value)
  if(isSelected) {
    res$attribs$checked <- "checked"
  }
  return(res)
}

radioButtonsTr <- function(inputId, label, values, names){
  tags$div(
    id=inputId, class="form-group shiny-input-radiogroup shiny-input-container",
    tags$label(class="control-label", `for`=inputId, labelInput(label)),
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

campo.codigo <- function(runid, refid, fieldid, ...) {
  tags$div(
    class = "box box-solid bg-black",
    tags$div(
      style = "text-align:right;padding-right: 10px;",
      tags$button(
        id = runid, type = "button", class = "run-button action-button", 
        icon("play"), tags$a(labelInput("ejecutar"), style = "color:white"))),
    tags$div(
      class = "box-body", 
      aceEditor(fieldid, mode = "r", theme = "monokai", value = "", ...))
  )
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
}

updateinitSelects <- function(id, choices) {
  vars <- c()
  vars[tr("todos")] <- "todos"
  vars <- c(vars, choices)
  updateSelectInput(session, id, choices = vars)
}

updateMenu <- function(datos = NULL, init = F){
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

datos.disyuntivos <- function(data, vars){
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

code.carga <- function(nombre.filas = T, ruta = NULL, separador = ";",
                       sep.decimal = ",", encabezado = T) {
  if(!is.null(ruta)){
    ruta <- gsub("\\", "/", ruta, fixed = T)
  }
  res <- paste0(
    "read.table('", ruta, "', header=", encabezado, ", sep='", 
    separador, "', dec = '", sep.decimal, "'", 
    ifelse(nombre.filas, ", row.names = 1", ""), ")")
  return(res)
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

code.desactivar.aux <- function(variable){
  return(paste0("datos <- subset(datos, select = -", variable, ")"))
}

code.desactivar <- function(variables){
  return(paste0("datos <- subset(datos, select = -c(",
                paste(variables, collapse = ","), "))"))
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
default.normal <- function(data = "datos", vars = NULL, color = "#00FF22AA", 
                           labelcurva = "Curva Normal") {
  if(is.null(vars)){
    return(NULL)
  } else {
    return(paste0(
      "promedio <- mean(", data, "[, '", vars, "']) \n",
      "desviacion <- sd(", data, "[, '", vars, "']) \n",
      "values <- dnorm(", data, "[, '", vars, "'], mean = promedio, sd = desviacion) \n",
      "values <- c(values, hist(", data, "[, '", vars, "'],  plot = F)$density) \n",
      "hist(", data, "[, '", vars, "'], col = '", color, "', border=F, axes=F,\n",
      "  freq = F, ylim = range(0, max(values)), ylab = '', \n",
      "  main = '", vars, "') \n",
      "axis(1, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025) \n",
      "axis(2, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025) \n",
      "curve(dnorm(x, mean = promedio, sd = desviacion), add=T, col='blue', lwd=2)\n",
      "legend('bottom', legend = '", labelcurva, "', col = 'blue', lty=1, cex=1.5)"))
  }
}

fisher.calc <- function (x, na.rm = FALSE, ...) {
  if (!is.numeric(x)) {
    stop("argument 'x' is must be numeric")
  }
  if (na.rm)
    x <- x[!is.na(x)]
  nx <- length(x)
  
  sk <- sum((x - mean(x))^3/stats::sd(x)^3)/nx
  
  return(sk)
}

default.calc.normal <- function(
  data = "datos", labelsi = "Positiva", labelno = "Negativa", 
  labelsin = "Sin Asimetría") {
  return(paste0(
    "calc <- lapply(var.numericas(", data,"), function(i) fisher.calc(i)[1]) \n",
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
default.disp <- function(data = "datos", vars = NULL, color = "#FF0000AA") {
  if(length(vars) < 2) {
    return("NULL")
  } else if(length(vars) == 2) {
    return(paste0(
      "ggplot(data = ", data, ", aes(x = ", vars[1], ", y = ", vars[2],
      ", label = rownames(", data, "))) +\n", "geom_point(color = '",
      color, "', size = 3) + geom_text(vjust = -0.7) + theme_minimal()"))
  } else{
    return(paste0(
      "scatterplot3d(", data, "[, '", vars[1], "'], ", data, "[, '", vars[2], 
      "'], ", data, "[, '", vars[3], "'], pch = 16, color = '", color, "')"))
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

def.code.num <- function(data = "datos", variable, color) {
  paste0("distribucion.numerico(", data, "[, ", variable, "], ",
         variable, ", color = ", color,")")
}

def.code.cat <- function(
  data = "datos", variable, titulox = tr("cantidadcasos"), 
  tituloy = tr("categorias")) {
  paste0(
    "distribucion.categorico(", data, "[, '", variable,"']) + ", 
    "labs(title = '", variable, "', x = '", 
    titulox, "', y = '", tituloy, "')")
}

#' Funciones Correlaciones
#' @author Diego
#' @return functions
#' @export
#'
modelo.cor <- function(data = "datos") {
  return(paste0("correlacion <<- cor(var.numericas(", data, "))"))
}

correlaciones <- function(metodo = 'circle', tipo = "lower") {
  return(paste0(
    "corrplot(correlacion, method='", metodo,"', shade.col=NA, tl.col='black',\n",
    "         tl.srt=20, addCoef.col='black', order='AOE', type = '", tipo, "')"))
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
    paste(ejes, collapse = ","), "),\n", "    col.ind = '", color, 
    "', select.ind = list(cos2 = ", ind.cos, ")) + labs(title = '')"))
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
    "    axes = c(", paste(ejes, collapse = ","), ")) + labs(title = '')"))
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
    paste(ejes, collapse = ","), ")) + labs(title = '')"))
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

#' PCA Contributions of variables to PC1
#' @author Diego
#' @return functions
#' @export
#'
code.pca.pc1 <- function(titulo, tituloy) {
  return(paste0(
    "fviz_contrib(pca.modelo, choice = 'var', axes = 1, top = 20) +\n",
    "  labs(y = '", tituloy, "', title = '", titulo, "')"))
}

#' PCA Contributions of variables to PC2
#' @author Diego
#' @return functions
#' @export
#'
code.pca.pc2 <- function(titulo, tituloy) {
  return(paste0(
    "fviz_contrib(pca.modelo, choice = 'var', axes = 2, top = 20) +\n",
    "  labs(y = '", tituloy, "', title = '", titulo, "')"))
}

###################### Clustering Jerarquico ##################################
#' Modelo
#' @author Diego
#' @return functions
#' @export
#'
def.model <- function(data = "datos", cant = "as.numeric(input$cant.cluster)",
                      dist.method = "euclidean", hc.method = "complete") {
  return(paste0(
    "modelo <- hclust(dist(var.numericas(", data, "), method = '",
    dist.method, "'), method = '", hc.method, "')\n",
    "clusters <- as.factor(cutree(modelo, k = ", cant, "))\n",
    "centros <- calc.centros(var.numericas(", data, "), clusters)\n",
    "hc.modelo <- list(modelo = modelo, clusters = clusters, centros = centros)"))
}

calc.centros <- function(data, clusteres) {
  if(is.null(clusteres)) return(NULL)
  real <- lapply(unique(clusteres), function(i)
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
centros.total <- function(DF){
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
BP2 <- function(suma, i, DF, c.total, cant, clusters){
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
diagrama <- function(cant = "as.numeric(input$cant.cluster)", 
                     colores = "'steelblue'") {
  return(paste0(
    "dendograma <- dendro_data(hc.modelo$modelo, type='rectangle')\n",
    "order.labels <- data.frame(label=names(hc.modelo$clusters), clusters = hc.modelo$clusters)\n",
    "dendograma[['labels']] <- merge(dendograma[['labels']], order.labels, by='label')\n",
    "ggplot() + geom_segment(data=segment(dendograma), aes(x=x, y=y, xend=xend, yend=yend)) +\n",
    "  geom_text(data=label(dendograma), aes(x, y, label = label, hjust=1.1, color = clusters), \n",
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
def.k.model <- function(data = "datos", cant = "as.numeric(input$cant.kmeans.cluster)",
                        iter.max = 200, nstart = 300, algorithm = "Hartigan-Wong") {
  return(paste0("k.modelo <- kmeans(var.numericas(", data, "), centers = ",
                cant, ",\n    ", "iter.max = ", iter.max,", nstart = ",
                nstart,", algorithm = '", algorithm ,"')"))
}

#' Jambu
#' @author Diego
#' @return functions
#' @export
#'
lead <- function(x){
  out <- c(x[-seq_len(1)], rep(NA, 1))
  return(out)
}

codo.jambu <- function(data. = NULL, k. = NA_integer_, tituloy = "",
                       nstart. = 200, iter.max. = 5000, h. = 1.5){
  params <- list(k = k., data = list(data.))
  params <- purrr::cross(params)
  models <- purrr::map(params, ~future::future(
    kmeans(x = .$data, centers = .$k, iter.max = iter.max., nstart = nstart.)))
  models <- future::values(models)
  tot_withinss <- purrr::map_dbl(models, 'tot.withinss')
  model_index <- head(which(!tot_withinss/lead(tot_withinss) > h.), 1)
  if(length(model_index) == 0)
    model_index <- which.min(tot_withinss)
  best_model <- models[[model_index]]
  res.plot <- ggplot() + geom_point(aes(x = k., y = tot_withinss), size = 2) +
    geom_line(aes(x = k., y = tot_withinss), size = 1) +
    theme_minimal() + labs(x = 'k', y = tituloy) +
    scale_x_continuous(breaks = seq(1, length(k.), 1)) +
    scale_y_continuous(labels = scales::comma)
  return(plot(res.plot))
}

calc.maxK <- function(data) {
  ifelse(nrow(datos) < 40, return(as.integer(nrow(datos)/2)), return(20))
}

def.code.jambu <- function(data = "datos", k = 20, tituloy) {
  return(paste0(
    "codo.jambu(data. = var.numericas(", data, "), k. = 2:", k,
    ", tituloy = '", tituloy, "')"))
}


###################### Clustering Jerarquico & K-medias #######################
#' Panel Inercia
#' @author Diego
#' @return functions
#' @export
#'
panel.inercia <- function(modelo, cant.clusters, datos = NULL, esHC = T) {
  if (esHC) {
    total.clase <- inercia.total(var.numericas(datos))
    inter.clase <- BP(var.numericas(datos), modelo, cant.clusters)
    intra.clase <- total.clase - inter.clase
  } else {
    intra.clase <- modelo$tot.withinss
    inter.clase <- modelo$betweenss
    total.clase <- modelo$totss
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
        class=paste0('small-box bg-', i$color), 
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
    "  palette = c(", paste(colores, collapse = ","), "),\n",
    "  col.var = 'steelblue', legend.title = 'Cluster') + labs(title = '')"))
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
  centros <- melt(centros, id.vars = 'var')
  ggplot(centros, aes(x=var, y=value)) +
    geom_bar(stat='identity', position='dodge', show.legend = F) +
    labs(x = '', y = '') + facet_wrap(~variable) + coord_flip() +
    theme(text = element_text(size = 20)) + aes(fill = variable)
}

cluster.horiz <- function(sel = "1", colores = "'steelblue'",
                          color = "red", esHC = T) {
  code.centros <-
    ifelse(esHC, "centros <- as.data.frame(t(hc.modelo$centros$real))",
           "centros <- as.data.frame(t(k.modelo$centers))")
  if(sel == "todos") {
    return(paste0(
      code.centros, "\ncentros.horizontal.todos(centros) +\n",
      "  scale_fill_manual(values = c(", paste(colores, collapse = ","), "))"))
  } else {
    return(paste0(
      code.centros, "\nggplot(data = centros, aes(x = row.names(centros), ",
      "y = centros[, ", sel, "])) +\n",
      "  geom_bar(stat = 'identity', fill = ", color, ") +\n",
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
  centros <- melt(centros, id.vars = 'cluster')
  ggplot(centros, aes(x=variable, y=value, fill=factor(cluster))) +
    geom_bar(stat='identity', position='dodge') + labs(x = '', y = '')
}

cluster.vert <- function(sel = "1", colores = "'steelblue'", esHC = T) {
  code.centros <-
    ifelse(esHC, "centros <- hc.modelo$centros$real",
           "centros <- as.data.frame(k.modelo$centers)")
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
coord_radar <- function (theta = 'x', start = 0, direction = 1) {
  theta <- match.arg(theta, c('x', 'y'))
  r <- if (theta == 'x') 'y' else 'x'
  ggproto('CordRadar', CoordPolar, theta = theta, r = r, start = start,
          direction = sign(direction), is_linear = function(coord) TRUE)
}

centros.radar <- function(centros){
  res <- melt(t(centros), varnames = c('variables', 'clusteres'))
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
    coord_radar()
}

cluster.radar <- function(colores = "'steelblue'", esHC = T){
  code.centros <-
    ifelse(
      esHC, "centros <- hc.modelo$centros$porcentual",
      paste0(
        "centros <- as.data.frame(apply(k.modelo$centers, 2, function(i)\n",
        "  scales::rescale(i, to = c(0, 100))))"))
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
cluster.cat <- function(var, colores = "'steelblue'", esHC = T) {
  code.clusters <-
    ifelse(esHC, "hc.modelo$clusters", "as.factor(k.modelo$cluster)")
  return(paste0(
    "NDatos <- cbind(datos, Cluster = ", code.clusters, ")\n",
    "plot(ggplot(NDatos, aes(x = ", var, ")) + geom_bar(aes(fill = Cluster)) +",
    "\n  scale_fill_manual('Cluster', values = c(", 
    paste(colores, collapse = ","),")) +\n  ",
    "facet_wrap(~Cluster, labeller = label_both) +\n  ",
    "theme(text = element_text(size = 15)) +\n  ", 
    "labs(x = '', y = '') + guides(fill = F))"))
}


###################### Reporte ################################################
#' Reporte
#' @author Diego
#' @return functions
#' @export
#'
init.replist <- function(nombre) {
  env.report$codigo.reporte[[nombre]][["basico"]] <- list()
  env.report$codigo.reporte[[nombre]][["acp"]] <- list()
  env.report$codigo.reporte[[nombre]][["rephc"]] <- list()
  env.report$codigo.reporte[[nombre]][["kmedias"]] <- list()
}

createLog <- function(datos, modelo, titulo = NULL, codigo, params = NULL, vars = "vacio") {
  if(!is.null(params) & !is.null(vars)) {
    if(is.null(env.report$codigo.reporte[[datos]][[modelo]][[params]][[titulo]])) {
      env.report$codigo.reporte[[datos]][[modelo]][[params]][[titulo]] <- list()
    }
    env.report$codigo.reporte[[datos]][[modelo]][[params]][[titulo]][[vars]] <- codigo
  } else if(!is.null(params)) {
    if(is.null(env.report$codigo.reporte[[datos]][[modelo]][[params]])) {
      env.report$codigo.reporte[[datos]][[modelo]][[params]] <- list()
    }
    env.report$codigo.reporte[[datos]][[modelo]][[params]][[titulo]] <- codigo
  } else {
    if(is.null(env.report$codigo.reporte[[datos]][[modelo]][[titulo]])) {
      env.report$codigo.reporte[[datos]][[modelo]][[titulo]] <- list()
    }
    env.report$codigo.reporte[[datos]][[modelo]][[titulo]][[vars]] <- codigo
  }
}

def.reporte <- function(titulo = "Sin Titulo", nombre = "PROMiDAT") {
  paste0(
    "---\n", "title: '", titulo, "'\n", "author: '", nombre, "'\n",
    "date: ", Sys.Date(), "\n", "output:\n  word_document:\n",
    "    df_print: paged\n---\n\n",
    "```{r setup, include=FALSE}\n",
    "knitr::opts_chunk$set(echo = FALSE,  fig.height = 10, fig.width = 15)\n",
    "```\n\n",
    "```{r message=FALSE, warning=FALSE}\n",
    "library(promises)\nlibrary(ggplot2)\nlibrary(FactoMineR)\n",
    "library(FactoMineR)\nlibrary(factoextra)\nlibrary(reshape)\n",
    "library(corrplot)\nlibrary(dendextend)\nlibrary(scatterplot3d)\n",
    "library(stringr)\nlibrary(ggdendro)\nlibrary(modeest)\n",
    "```\n\n", "```{r}\n", extract.code("var.numericas"), "\n\n", 
    extract.code("var.categoricas"), "\n\n", extract.code("datos.disyuntivos"),
    "\n\n", extract.code("distribucion.numerico"), "\n\n", 
    extract.code("distribucion.categorico"), "\n\n", 
    extract.code("codo.jambu"), "\n\n", extract.code("calc.centros"), "\n\n",
    extract.code("centros.horizontal.todos"), "\n\n", 
    extract.code("centros.vertical.todos"), "\n\n",  
    extract.code("centros.radar"), "\n```"
  )
}

limpiar.titulos <- function(x) {
  x <- unlist(strsplit(x, split = "\\."))
  if(length(x) > 3) {
    x <- c(paste(x[1:2], collapse = "."), x[3:length(x)])
  }
  params <- x[grepl(x, pattern = "=")]
  params <- sapply(params, function(i) {
    aux <- unlist(strsplit(i, split = " "))
    aux <- tr(gsub(aux, pattern = "=", replacement = ""))
    paste(aux, collapse = " ")
  })
  x[grepl(x, pattern = "=")] <- params
  x <- tr(x[x != "vacio"])
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
      if(!is.null(rep.codigos)) {
        res <- paste0(res, "\n\n## ", tr(analisis), "\n\n")
        for (i in 1:length(rep.codigos)) {
          rep.titulos <- limpiar.titulos(names(rep.codigos)[i])
          res <- paste0(res, rep.titulos, "```{r}\n", rep.codigos[i], "\n```\n\n")
        }
      }
    }
  }
  return(res)
}




