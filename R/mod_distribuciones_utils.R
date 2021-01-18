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
  outliers <- echarts4r:::.get_outliers(e, serie, i)
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
e_hboxplot <- function (e, serie, name = NULL, outliers = TRUE, ...) {
  serie <- deparse(substitute(serie))
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }
  for (i in 1:length(e$x$data)) {
    vector <- echarts4r:::.build_boxplot(e, serie, i)
    if (!e$x$tl) {
      nm <- echarts4r:::.name_it(e, serie, name, i)
      if (length(e$x$opts$series) >= 1) {
        e$x$opts$series[[1]]$data <- append(e$x$opts$series[[1]]$data, 
                                            list(vector))
      }
      else {
        box <- list(name = nm, type = "boxplot", data = list(vector), 
                    ...)
        e$x$opts$series <- append(e$x$opts$series, list(box))
      }
      if (isTRUE(outliers)) {
        e <- addoutliersh(e, serie, i)
      }
      e$x$opts$yAxis[[1]]$data <- append(e$x$opts$yAxis[[1]]$data, 
                                         list(nm))
      e$x$opts$yAxis[[1]]$type <- "category"
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
#' @param nombrearchivo a character value specifying the name to use when the plot is downloaded.
#' @param colorBar a color for the bars.
#' @param colorPoint a color for the points.
#' @param outlier.name a character value specifying the name to use on the tooltip of points.
#' @param titulos a character vector of length 5 specifying the titles to use on legend.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return Highchart plot
#' @export hchistboxplot
#' @importFrom highcharter hchart hc_add_series hc_plotOptions hc_exporting hc_chart hc_xAxis hc_yAxis
#' 
hchistboxplot <- function(data, nombrearchivo = NULL, colorBar = "steelblue",
                          colorPoint = "red", outlier.name = "",
                          titulos = c("Mínimo", "Primer Cuartil", "Mediana", 
                                      "Tercer Cuartil", "Máximo")) {
  r <- data.frame(x = 1:length(data), y = data) %>% e_charts(x) %>% 
    e_hboxplot(y) %>% e_histogram(y, x_index = 1, y_index = 1) %>%
    e_grid(height = "50%") %>% e_grid(height = "30%", top = "60%") %>%
    e_y_axis(gridIndex = 1) %>% e_x_axis(gridIndex = 1) %>% 
    e_x_axis(scale = T) %>% e_tooltip() %>% e_datazoom(show = F)
  
  r$x$opts$xAxis[[2]]$scale <- TRUE
  
  return(r)
}

############################### Generar Código ################################
code.dist.cat <- function(var) {
  paste0(
    "datos.plot <- data.frame (\n",
    "  label = levels(datos[['", var, "']]),\n",
    "  value = summary(datos[['", var, "']], maxsum = length(levels(datos[['", var, "']])))\n",
    ")\n",
    "hchart(datos.plot, hcaes(x = label, y = value, color = label), type = 'column') %>%\n",
    "hc_tooltip(pointFormat = '<b>{point.name}:</b> {point.y}',\n",
    "           headerFormat = '') %>%\n",
    "hc_exporting(enabled = T, filename = 'distribucion_cat')\n"
  )
}








