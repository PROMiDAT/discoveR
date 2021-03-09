#' List outliers
#' @keywords internal
listoutliers  <- function (e, out) {
  x <- length(e$x$opts$series[[1]]$data) - 1
  x <- rep(x, length(out))
  matrix <- cbind(out, x)
  apply(unname(matrix), 1, as.list)
}

#' Add outliers Horizontal
#' @keywords internal
addoutliersh <- function (e, serie, i) {
  outliers <- get_outliers(e, serie, i)
  outliers <- listoutliers(e, outliers)
  scatter <- list(type = "scatter", data = outliers)
  if (length(e$x$opts$series) == 2) {
    e$x$opts$series[[2]]$data <- rev(append(e$x$opts$series[[2]]$data, 
                                            outliers))
  }
  else {
    e$x$opts$series <- rev(append(e$x$opts$series, list(scatter)))
  }
  e
}

#' Generate horizontal boxplot
#' @keywords internal
e_boxplot <- function (e, serie, name = NULL, outliers = TRUE, ...) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }
  for (i in 1:length(e$x$data)) {
    vector <- build_boxplot(e, serie, i)
    if (!e$x$tl) {
      nm <- name_it(e, serie, name, i)
      if (length(e$x$opts$series) >= 1) {
        e$x$opts$series[[1]]$data <- append(e$x$opts$series[[1]]$data, 
                                            list(vector))
      }
      else {
        box <- list(name = nm, type = "boxplot", data = list(vector), ...)
        e$x$opts$series <- append(e$x$opts$series, list(box))
      }
      if (isTRUE(outliers)) {
        e <- addoutliersh(e, serie, i)
      }
      e$x$opts$yAxis[[1]]$data <- append(e$x$opts$yAxis[[1]]$data, 
                                         list(nm))
      e$x$opts$yAxis[[1]]$type <- "category"
      e$x$opts$yAxis[[1]]$show <- F
      e$x$opts$xAxis[[1]]$splitLine <- list(show = F)
    }
    else {
      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, 
                                             list(list(data = vector)))
    }
  }
  if (isTRUE(e$x$tl)) {
    serie_opts <- list(type = "boxplot", ...)
    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, 
                                         list(serie_opts))
  }
  e
}

#' Histogram + boxplot
#'
#' @param data a numeric column of a data.frame.
#' @param var.name a character value specifying the name of the variable.
#' @param colorBar a color for the bars.
#' @param colorPoint a color for the points.
#' @param titulos a character vector of length 5 specifying the titles to use on legend.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_histboxplot
#' @import echarts4r
#' @examples
#' e_histboxplot(iris$Sepal.Width, "Sepal.Width")
#' 
e_histboxplot <- function(data, var.name, colorBar = "steelblue", colorPoint = "red",
                          titulos = c("Minimo", "Primer Cuartil", "Mediana", 
                                      "Tercer Cuartil", "Maximo")) {
  data <- data.frame(x = 1:length(data), y = data)
  colnames(data) <- c("x", var.name)
  
  r <- data %>% e_charts(x) %>% e_boxplot(var.name) %>% 
    e_histogram_(var.name, x_index = 1, y_index = 1) %>% 
    e_grid(height = "50%") %>% e_grid(height = "30%", top = "60%") %>% 
    e_y_axis(gridIndex = 1) %>% e_x_axis(gridIndex = 1) %>% 
    e_x_axis(scale = T) %>% e_tooltip() %>% e_datazoom(show = F) %>% 
    e_color(c(colorPoint, colorBar, colorBar)) %>% e_show_loading()
  
  r$x$opts$xAxis[[2]]$scale <- TRUE
  
  return(r)
}

############################### Generar Código ################################
code.dist.cat <- function(var) {
  paste0(
    "datos.plot <- data.frame (\n",
    "  label = levels(datos[['", var, "']]), value = summary(datos[['", var, "']],\n",
    "  maxsum = length(levels(datos[['", var, "']])))\n",
    ")\n\n",
    "datos.plot %>% e_charts(label) %>% e_bar(value, name = var) %>%\n",
    "  e_tooltip() %>% e_datazoom(show = F) %>% e_show_loading()\n"
  )
}








