#' Normal plot
#'
#' @param data a numeric column of a data.frame.
#' @param nombrearchivo a character value specifying the name to use when the plot is downloaded.
#' @param colorbar a color for the bars.
#' @param colorline a color for the line.
#' @param nombres a character vector of length 2 specifying the titles to use on legend.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return Highchart plot
#' @export hchistnormal
#' @importFrom highcharter hchart hc_add_series hc_plotOptions hc_exporting hc_chart hc_xAxis hc_yAxis
#'
hchistnormal <- function(data, nombrearchivo = NULL, colorbar = "steelblue", colorline = "gray",
                         nombres = c("Histograma", "Curva Normal")) {
  h <- hist(data, plot = F)
  x <- seq(min(h$mids, na.rm = T), max(h$mids, na.rm = T), length = length(h$mids))
  promedio <- mean(data, na.rm = T)
  desviacion <- sd(data, na.rm = T)
  normalidad <- dnorm(x, promedio, desviacion)
  
  d <- diff(h$breaks)[1]
  
  distribu <- data.frame(
    x = h$mids, d = h$density, n = normalidad,
    name = paste0("(", h$mids - d / 2, " - ", h$mids + d / 2, ")")
  )
  
  distribu %>% e_charts(x) %>% e_bar(d, name = nombres[1]) %>% 
    e_line(n, name = nombres[2]) %>% e_x_axis(scale = T) %>%
    e_tooltip() %>% e_datazoom(show = F)
}


#' Qplot + Qline
#'
#' @param data a numeric column of a data.frame.
#' @param nombrearchivo a character value specifying the name to use when the plot is downloaded.
#' @param colorpoint a color for the points.
#' @param colorline a color for the line.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return Highchart plot
#' @export hcqq
#' @importFrom highcharter hchart hc_add_series hc_plotOptions hc_exporting hc_chart hc_xAxis hc_yAxis
#'
hcqq <- function(data, nombrearchivo = NULL, colorpoint = "steelblue", colorline = "gray") {
  data <- data.frame(qqnorm(data, plot = F))
  
  y <- quantile(data$y, c(0.25, 0.75), names = F, na.rm = T)
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]

  data$z <- data$x * slope + int
  
  data %>% e_charts(x) %>% e_scatter(y, name = "QQplot", symbol_size = 8) %>%
    e_line(z, name = "QQline", symbol = 'none') %>% e_x_axis(scale = T) %>%
    e_y_axis(scale = T) %>% e_tooltip() %>% e_datazoom(show = F)
}

#' Normal data.frame
#'
#' @param data a data.frame object only with the numeric columns.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return data.frame
#' @export dfnormal
#'
dfnormal <- function(data) {
  data <- var.numericas(data)
  fisher  <- lapply(data, function(i) fisher.calc(i))
  pearson <- lapply(data, function(i) pearson.test(i))
  lillie  <- lapply(data, function(i) lillie.test(i))
  cvm     <- lapply(data, function(i) cvm.test(i))
  
  data.frame(cbind(fisher, pearson, lillie, cvm))
}








