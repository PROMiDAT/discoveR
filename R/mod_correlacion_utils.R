#' Correlation plot
#'
#' @param x a data.frame with correlation values.
#' @param colors a vector of lenght 3 with color values.
#' 
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_cor
#' @import echarts4r
#' @examples
#' p <- round(cor(iris[, -5]), 3)
#' e_cor(p)
#' 
e_cor <- function(x, colors = c("#FF5733", "#F8F5F5", "#2E86C1")) {
  data <- vector(mode = "list", length = (nrow(x) * ncol(x)))
  i <- 1
  for (var in row.names(x)) {
    for (dim in colnames(x)) {
      data[[i]] <- list(value = c(dim, var, x[var, dim]))
      i <- i + 1
    }
  }
  
  opts <- list(
    xAxis = list(data = colnames(x)),
    yAxis = list(data = rev(row.names(x))),
    series = list(
      list(
        type = "heatmap", coordinateSystem = "cartesian2d",
        itemStyle = list(borderWidth = 2, borderColor = "#fff"),
        label = list(show = T), data = data
      )
    ),
    visualMap = list(
      min = -1, max = 1, label = T, calculable = T, type = "continuous",
      inRange   = list(color = colors), left = "right",
      itemStyle = list(borderWidth = 2, borderColor = "#fff")
    )
  )
  
  e_charts() |> e_list(opts) |> e_datazoom(show = F) %>% e_show_loading() |> e_tooltip(
    formatter = htmlwidgets::JS(paste0(
      "function(params) {\n",
      "  return(params.value[1] + ' ~ ' + params.value[0] + ': ' + params.value[2])\n", 
      "}"))
  )
}

############################### Generar Código ################################
code.cor <- function(colores) {
  paste0(
    "colores <- c('", colores[1], "', '", colores[2], "', '", colores[3], "')\n",
    "datos.plot <- round(cor(datos), 3)\n",
    "e_cor(datos.plot, colores)\n"
  )
}