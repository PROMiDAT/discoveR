#' Calculate Histogram
#' @keywords internal
get_hist_data <- function(x) {
  aux <- hist(x, plot = F)
  d <- diff(aux$breaks)[1]
  df <- data.frame(
    x = aux$mids,
    y = aux$counts,
    name = paste0("(", aux$mids - d / 2, " - ", aux$mids + d / 2, ")")
  )
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
#' @importFrom graphics boxplot
#' @examples
#' hchistboxplot(iris$Sepal.Width)
#' 
hchistboxplot <- function(data, nombrearchivo = NULL, colorBar = "steelblue",
                          colorPoint = "red", outlier.name = "",
                          titulos = c("Minimo", "Primer Cuartil", "Mediana", 
                                      "Tercer Cuartil", "Maximo")) {
  atipicos <- boxplot(data, plot = F)
  distribu <- get_hist_data(data)
  
  pos    <- (max(distribu$y) * 0.20) * -1
  line_w <- c(2, 2, 7, 2, 2)
  line_s <- c(pos * 0.25, pos * 0.5, pos * 0.5, pos * 0.5, pos * 0.25)
  v      <- atipicos$stats
  l      <- titulos
  
  r <- highchart() %>%
    hc_add_series(
      distribu, hcaes(x = x, y = y), type = "column", color = colorBar,
      tooltip = list(pointFormat = "<b>{point.name}</b>: {point.y}", 
                     headerFormat = "")
    ) %>%
    hc_plotOptions(
      line = list(marker = list(enabled = F)),
      column = list(
        pointPadding = 0, borderWidth = 1, groupPadding = 0, shadow = F
      )
    ) %>% hc_legend(enabled = F)
  
  for (i in 1:length(v)) {
    r <- r %>% hc_add_series(
      type = "line", color = colorBar, lineWidth = line_w[i],
      marker = list(symbol = "triangle-down"),
      tooltip = list(pointFormat = paste0(l[i], ": {point.x}"), headerFormat = ""),
      data = list(list(x = v[i], y = pos + line_s[i]),
                  list(x = v[i], y = pos - line_s[i]))
    )
  }
  
  r <- r %>% hc_add_series(
    type = "line", color = colorBar, lineWidth = 2,
    enableMouseTracking = F,
    data = list(list(x = v[1], y = pos), list(x = v[2], y = pos))
  )
  
  r <- r %>% hc_add_series(
    type = "line", color = colorBar, lineWidth = 2,
    enableMouseTracking = F,
    data = list(list(x = v[2], y = pos * 1.5),
                list(x = v[4], y = pos * 1.5))
  )
  
  r <- r %>% hc_add_series(
    type = "line", color = colorBar, lineWidth = 2,
    enableMouseTracking = F,
    data = list(list(x = v[2], y = pos * 0.5),
                list(x = v[4], y = pos * 0.5))
  )
  
  r <- r %>% hc_add_series(
    type = "line", color = colorBar, lineWidth = 2,
    enableMouseTracking = F,
    data = list(list(x = v[4], y = pos), list(x = v[5], y = pos))
  )
  
  for (atipico in atipicos$out) {
    r <- r %>% hc_add_series(
      type = "scatter", color = colorPoint, marker = list(symbol = "circle"),
      tooltip = list(pointFormat = paste0(outlier.name, ": {point.x}"), headerFormat = ""),
      data = list(list(x = atipico, y = pos))
    )
  }
  
  r <- r %>% hc_yAxis(
    labels = list(
      formatter = JS(
        "function() {
          if(this.value < 0)
            return '';
          else
            return this.value;
        }")
    )
  )
  
  if(!is.null(nombrearchivo)) {
    r <- r %>% hc_exporting(enabled = T, filename = nombrearchivo)
  }
  
  r
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








