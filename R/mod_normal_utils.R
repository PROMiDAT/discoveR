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
#' @importFrom graphics hist
#' @importFrom stats dnorm sd
#' @examples
#' hchistnormal(iris$Sepal.Length)
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
  
  r <- highchart() %>%
    hc_add_series(
      distribu, hcaes(x = x, y = d), type = "column", 
      name = nombres[1], color = colorbar,
      tooltip = list(pointFormat = "<b>{point.name}</b>: {point.y}",
                     headerFormat = "")
    ) %>%
    hc_add_series(
      distribu, hcaes(x = x, y = n), type = "spline",
      name = nombres[2], color = colorline,
      tooltip = list(pointFormat = "<b>{point.name}</b>: {point.y}",
                     headerFormat = "")
    ) %>%
    hc_plotOptions(
      spline = list(marker = list(enabled = F), enableMouseTracking = F),
      column = list(
        pointPadding = 0, borderWidth = 1, groupPadding = 0, shadow = F
      )
    )
  
  if(!is.null(nombrearchivo)) {
    r <- r %>% hc_exporting(enabled = T, filename = nombrearchivo)
  }
  
  return(r)
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
#' @importFrom stats qnorm qqnorm quantile
#' @examples
#' hcqq(iris$Sepal.Length)
#' 
hcqq <- function(data, nombrearchivo = NULL, colorpoint = "steelblue", colorline = "gray") {
  data <- data.frame(qqnorm(data, plot = F))
  
  y <- quantile(data$y, c(0.25, 0.75), names = F, na.rm = T)
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  linea   <- data.frame(x = c(min(data$x), max(data$x))) 
  linea$y <- linea$x * slope + int
  
  r <- highchart() %>%
    hc_add_series(
      data, hcaes(x = x, y = y), color = colorpoint, type = "point", 
      name = "QQPlot",
      tooltip = list(pointFormat = "({point.x:.2f}, {point.y:.2f})",
                     headerFormat = "")
    ) %>%
    hc_add_series(
      linea, hcaes(x = x, y = y), color = colorline, 
      type = "line", name = "QQLine") %>% 
    hc_plotOptions(line = list(enableMouseTracking = F)) %>%
    hc_chart(zoomType = "xy")
  
  if(!is.null(nombrearchivo)) {
    r <- r %>% hc_exporting(enabled = T, filename = nombrearchivo)
  }
  
  return(r)
}

#' Data.frame with normal test
#'
#' @param data a data.frame object only with the numeric columns.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return data.frame
#' @export dfnormal
#' @importFrom stats complete.cases pchisq pnorm shapiro.test
#' @examples
#' dfnormal(iris[, -5])
#' 
dfnormal <- function(data) {
  data    <- var.numericas(data)
  fisher  <- sapply(data, function(i) fisher.calc(i))
  pearson <- sapply(data, function(i) pearson.test(i))
  lillie  <- sapply(data, function(i) lillie.test(i))
  cvm     <- sapply(data, function(i) cvm.test(i))
  shapiro <- sapply(data, function(i) shapiro.test(i)$p.value)
  
  data.frame(cbind(fisher, pearson, lillie, cvm, shapiro))
}








