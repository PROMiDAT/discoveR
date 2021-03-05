#' Normal plot
#'
#' @param data a numeric column of a data.frame.
#' @param colorbar a color for the bars.
#' @param colorline a color for the line.
#' @param nombres a character vector of length 2 specifying the titles to use on legend.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_histnormal
#' @import echarts4r
#' @importFrom graphics hist
#' @importFrom stats dnorm sd
#' @examples
#' e_histnormal(iris$Sepal.Length)
#' 
e_histnormal <- function(data, colorbar = "steelblue", colorline = "gray",
                         nombres = c("Histograma", "Curva Normal")) {
  h <- hist(data, plot = F)
  x <- seq(min(h$mids, na.rm = T), max(h$mids, na.rm = T), length = length(h$mids))
  promedio <- mean(data, na.rm = T)
  desviacion <- sd(data, na.rm = T)
  normalidad <- dnorm(x, promedio, desviacion)
  
  d <- diff(h$breaks)[1]
  
  distribu <- data.frame(
    x = round(h$mids, 3), d = round(h$density, 3), n = round(normalidad, 3),
    name = paste0("(", h$mids - d / 2, " - ", h$mids + d / 2, ")")
  )
  
  distribu %>% e_charts(x) %>% e_bar(d, name = nombres[1]) %>% 
    e_line(n, name = nombres[2]) %>% e_x_axis(scale = T) %>%
    e_axis_labels(x = "", y = "Densidad") %>% 
    e_color(c(colorbar, colorline)) %>% e_tooltip() %>% 
    e_datazoom(show = F) %>% e_show_loading()
}

#' Qplot + Qline
#'
#' @param data a numeric column of a data.frame.
#' @param colorpoint a color for the points.
#' @param colorline a color for the line.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_qq
#' @import echarts4r
#' @importFrom stats qnorm qqnorm quantile
#' @examples
#' e_qq(iris$Sepal.Length)
#' 
e_qq <- function(data, colorpoint = "steelblue", colorline = "gray") {
  data <- data.frame(qqnorm(data, plot = F))
  
  y <- quantile(data$y, c(0.25, 0.75), names = F, na.rm = T)
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  data$z <- data$x * slope + int
  data <- round(data, 3)
  
  data %>% e_charts(x) %>% e_scatter(y, name = "QQplot", symbol_size = 8) %>%
    e_line(z, name = "QQline", symbol = 'none') %>% e_x_axis(scale = T) %>%
    e_y_axis(scale = T) %>% e_tooltip() %>% e_datazoom(show = F) %>% 
    e_color(c(colorpoint, colorline)) %>% e_show_loading()
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








