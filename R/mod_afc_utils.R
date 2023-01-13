#' Balloonplot
#'
#' @param datos a data frame object.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_balloon
#' @import echarts4r
#' @examples
#' e_balloon(iris)
#'
e_balloon <- function(datos) {
  datos <- var.numericas(datos)
  escalados <- data.frame(scale(datos, center = F))
  
  r <- list()
  r[[1]] <- list(
    data = list(), type = "scatter", symbolSize = htmlwidgets::JS(paste0(
      "function(val) {
              return(val[3] * ", 2500 / (nrow(datos) * ncol(datos)), ");
          }")))
  pos <- 1
  for (j in 1:ncol(datos)) {
    for (i in 1:nrow(datos)) {
      r[[1]]$data[[pos]] <- list(
        value = c((j - 1), (i - 1), datos[i, j], escalados[i, j]),
        name = paste0(row.names(datos)[i], ", ", colnames(datos)[j]))
      pos <- pos + 1
    }
  }
  
  e_charts() |> e_list(list(
    xAxis = list(
      type = "category", position = "top", splitLine = list(show = T),
      axisLine = list(show = F), data = colnames(datos)),
    yAxis = list(
      type = "category", inverse = T, splitLine = list(show = T),
      axisLine = list(show = F), data = row.names(datos)),
    grid = list(containLabel = T, left = 5, bottom = 10, right = 10),
    series = r
  )) |> e_show_loading() |> e_datazoom(show = F) |>
    e_tooltip(formatter = htmlwidgets::JS(
      "function(params) {
              return('<strong>' + params.name + ':</strong> ' + 
                     params.value[2].toFixed(3))
          }"))
}

#' AFC plot of individuals
#'
#' @param modelo an object of class CA [FactoMineR].
#' @param axes a numeric vector of length 2 specifying the dimensions to be plotted.
#' @param colorRow a color for the individuals well represented.
#' @param cos2 a numeric value from 0 to 1 specifying the quality of the individuals.
#' @param colorCos a color for individuals badly represented.
#' @param titulos a character vector of length 2 specifying the titles to use on legend.
#' @param etq a boolean, whether to add label to graph or not.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_afcrow
#' @import echarts4r
#' @examples
#' p <- FactoMineR::CA(iris[, -5], graph = FALSE)
#' e_afcrow(p)
#' 
e_afcrow <- function(modelo, axes = c(1, 2), colorRow = "steelblue", cos2 = 0, 
                     colorCos = "firebrick",
                     titulos = c("Bien Representados", "Mal Representados"),
                     etq = T) {
  row <- data.frame(
    x = modelo$row$coord[, axes[1]], y = modelo$row$coord[, axes[2]], 
    cos2 = apply(modelo$row$cos2[, axes], 1, sum, na.rm = T))
  row$id <- row.names(row)
  row$cos <- factor(ifelse(row$cos2 >= cos2, 1, 0), levels = c(1, 0), 
                    labels = titulos)
  
  colores <- c(colorRow, colorCos)
  if(sum(row$cos == titulos[1]) == 0) colores <- colorCos
  if(sum(row$cos == titulos[2])  == 0) colores <- colorRow
  
  inercias <- round(modelo$eig[, 2], digits = 2)[axes]
  
  res <- row |> group_by(cos) |> e_charts(x) |> 
    e_scatter(y, label = list(show = F), symbol_size = 15, bind = id) |>
    e_x_axis(scale = T) |> e_y_axis(scale = T) |> e_datazoom(show = F) |>
    e_color(colores) |> e_show_loading() |>
    e_axis_labels(x = paste0("Dim.", axes[1], " (", inercias[1], ")"), 
                  y = paste0("Dim.", axes[2], " (", inercias[2], ")")) |> 
    e_tooltip(formatter = htmlwidgets::JS(
      "function(params) {
        return('<strong>' + params.name + '</strong>: (' + 
               params.value[0].toFixed(3) + ', ' + params.value[1].toFixed(3) + ')') 
      }"))
  
  if(etq) {
    res <- res |>
      e_labels(formatter = htmlwidgets::JS(
        "function(params) {
         return(params.name)
      }"))
  }
  
  return(res)
}

#' AFC plot of individuals in 3D
#'
#' @param modelo an object of class CA [FactoMineR].
#' @param axes a numeric vector of length 3 specifying the dimensions to be plotted.
#' @param colorRow a color for the individuals well represented.
#' @param cos2 a numeric value from 0 to 1 specifying the quality of the individuals.
#' @param colorCos a color for individuals badly represented.
#' @param titulos a character vector of length 2 specifying the titles to use on legend.
#' @param etq a boolean, whether to add label to graph or not.
#' 
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_afcrow_3D
#' @import echarts4r
#' @examples
#' p <- FactoMineR::CA(iris[, -5], graph = FALSE)
#' e_afcrow_3D(p)
#' 
e_afcrow_3D <- function(modelo, axes = c(1, 2, 3), colorRow = "steelblue", cos2 = 0,
                        colorCos = "firebrick", 
                        titulos = c("Bien Representados", "Mal Representados"),
                        etq = T) {
  row <- data.frame(
    x = modelo$row$coord[, axes[1]], y = modelo$row$coord[, axes[2]],
    z = modelo$row$coord[, axes[3]],
    cos2 = apply(modelo$row$cos2[, axes], 1, sum, na.rm = T))
  dims <- paste0("Dim.", axes)
  row$id <- row.names(row)
  row$cos = factor(ifelse(row$cos2 >= cos2, 1, 0), levels = c(1, 0), 
                   labels = titulos)
  inercias <- round(modelo$eig[axes, 2], digits = 2)
  
  colores <- c(colorRow, colorCos)
  if(sum(row$cos == titulos[1]) == 0) colores <- colorCos
  if(sum(row$cos == titulos[2]) == 0) colores <- colorRow
  
  res <- row |> group_by(cos) |> e_charts(x) |> 
    e_scatter_3d(y, z, label = list(show = F), symbol_size = 15, bind = id) |>
    e_color(colores) |> e_show_loading() |> e_theme("dark") |>
    e_legend(data = titulos) |> 
    e_x_axis_3d(name = paste0(dims[1], " (", inercias[1], ")"),
                axisLine = list(lineStyle = list(color = "white"))) |> 
    e_y_axis_3d(name = paste0(dims[2], " (", inercias[2], ")"),
                axisLine = list(lineStyle = list(color = "white"))) |>
    e_z_axis_3d(name = paste0(dims[3], " (", inercias[3], ")"),
                axisLine = list(lineStyle = list(color = "white"))) |> 
    e_tooltip(formatter = htmlwidgets::JS(
      "function(params) {
         return('<strong>' + params.name + '</strong>: (' + 
                params.value[0].toFixed(3) + ', ' + params.value[1].toFixed(3) + 
                ', ' + params.value[2].toFixed(3) + ')') 
      }"))
  
  if(etq) {
    res <- res |>
      e_labels(formatter = htmlwidgets::JS(
        "function(params) {
         return(params.name)
      }"))
  }
  
  return(res)
}

#' AFC plot of variables
#'
#' @param modelo an object of class CA [FactoMineR].
#' @param axes a numeric vector of length 2 specifying the dimensions to be plotted.
#' @param colorCol a color for the variables well represented.
#' @param cos2 a numeric value from 0 to 1 specifying the quality of the variables.
#' @param colorCos a color for the variables badly represented.
#' @param titulos a character vector of length 2 specifying the titles to use on legend.
#' 
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_afccol
#' @import echarts4r
#' @examples
#' p <- FactoMineR::CA(iris[, -5], graph = FALSE)
#' e_afccol(p)
#' 
e_afccol <- function(modelo, axes = c(1, 2), colorCol = "forestgreen", cos2 = 0, 
                     colorCos = "darkorchid",
                     titulos = c("Bien Representados", "Mal Representados")) {
  col <- data.frame(
    x = modelo$col$coord[, axes[1]], y = modelo$col$coord[, axes[2]], 
    cos2 = apply(modelo$col$cos2[, axes], 1, sum, na.rm = T))
  col$id <- row.names(col)
  col$cos <- factor(ifelse(col$cos2 >= cos2, 1, 0), levels = c(1, 0), 
                    labels = titulos)
  
  colores <- c(colorCol, colorCos)
  if(sum(col$cos == titulos[1]) == 0) colores <- colorCos
  if(sum(col$cos == titulos[2])  == 0) colores <- colorCol
  
  inercias <- round(modelo$eig[, 2], digits = 2)[axes]
  
  col |> group_by(cos) |> e_charts(x) |> 
    e_scatter(y, label = list(show = F), symbol_size = 15, symbol = "diamond", bind = id) |>
    e_x_axis(scale = T) |> e_y_axis(scale = T) |> e_datazoom(show = F) |>
    e_color(colores) |> e_show_loading() |>
    e_axis_labels(x = paste0("Dim.", axes[1], " (", inercias[1], ")"), 
                  y = paste0("Dim.", axes[2], " (", inercias[2], ")")) |> 
    e_tooltip(formatter = htmlwidgets::JS(
      "function(params) {
        return('<strong>' + params.name + '</strong>: (' + 
               params.value[0].toFixed(3) + ', ' + params.value[1].toFixed(3) + ')') 
      }")) |>
    e_labels(formatter = htmlwidgets::JS(
      "function(params) {
         return(params.name)
      }"))
}

#' AFC plot of variables in 3D
#'
#' @param modelo an object of class CA [FactoMineR].
#' @param axes a numeric vector of length 3 specifying the dimensions to be plotted.
#' @param colorCol a color for the variables well represented.
#' @param cos2 a numeric value from 0 to 1 specifying the quality of the variables.
#' @param colorCos a color for variables badly represented.
#' @param titulos a character vector of length 2 specifying the titles to use on legend.
#' 
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_afccol_3D
#' @import echarts4r
#' @examples
#' p <- FactoMineR::CA(iris[, -5], graph = FALSE)
#' e_afccol_3D(p)
#' 
e_afccol_3D <- function(modelo, axes = c(1, 2, 3), colorCol = "forestgreen",
                        cos2 = 0, colorCos = "darkorchid",
                        titulos = c("Bien Representados", "Mal Representados")) {
  col <- data.frame(
    x = modelo$col$coord[, axes[1]], y = modelo$col$coord[, axes[2]],
    z = modelo$col$coord[, axes[3]],
    cos2 = apply(modelo$col$cos2[, axes], 1, sum, na.rm = T))
  dims <- paste0("Dim.", axes)
  col$id <- row.names(col)
  col$cos = factor(ifelse(col$cos2 >= cos2, 1, 0), levels = c(1, 0), 
                   labels = titulos)
  inercias <- round(modelo$eig[axes, 2], digits = 2)
  
  colores <- c(colorCol, colorCos)
  if(sum(col$cos == titulos[1]) == 0) colores <- colorCos
  if(sum(col$cos == titulos[2]) == 0) colores <- colorCol
  
  col |> group_by(cos) |> e_charts(x) |> 
    e_scatter_3d(y, z, label = list(show = F), symbol_size = 15, symbol = "diamond", bind = id) |>
    e_color(colores) |> e_show_loading() |> e_theme("dark") |>
    e_legend(data = titulos) |> 
    e_x_axis_3d(name = paste0(dims[1], " (", inercias[1], ")"),
                axisLine = list(lineStyle = list(color = "white"))) |> 
    e_y_axis_3d(name = paste0(dims[2], " (", inercias[2], ")"),
                axisLine = list(lineStyle = list(color = "white"))) |>
    e_z_axis_3d(name = paste0(dims[3], " (", inercias[3], ")"),
                axisLine = list(lineStyle = list(color = "white"))) |> 
    e_tooltip(formatter = htmlwidgets::JS(
      "function(params) {
         return('<strong>' + params.name + '</strong>: (' + 
                params.value[0].toFixed(3) + ', ' + params.value[1].toFixed(3) + 
                ', ' + params.value[2].toFixed(3) + ')') 
      }")) |>
    e_labels(formatter = htmlwidgets::JS(
      "function(params) {
         return(params.name)
      }"))
}

#' AFC biplot
#'
#' @param modelo an object of class CA [FactoMineR].
#' @param axes a numeric vector of length 2 specifying the dimensions to be plotted.
#' @param colorRow a color for the individuals well represented.
#' @param colorCol a color for the variables well represented.
#' @param cos2Row a numeric value from 0 to 1 specifying the quality of the individuals.
#' @param cos2Col a numeric value from 0 to 1 specifying the quality of the variables.
#' @param colorRowCos a color for the individuals badly represented.
#' @param colorColCos a color for the variables badly represented.
#' @param titulos a character vector of length 2 specifying the titles to use on legend.
#' @param etq a boolean, whether to add label to graph or not.
#' 
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_afcbi
#' @import echarts4r
#' @examples
#' p <- FactoMineR::CA(iris[, -5], graph = FALSE)
#' e_afcbi(p)
#' 
e_afcbi <- function(modelo, axes = c(1, 2), colorRow = "steelblue", 
                    colorCol = "forestgreen", cos2Row = 0, cos2Col = 0, 
                    colorRowCos = "firebrick", colorColCos = "darkorchid",
                    titulos = c("Bien Representados", "Mal Representados"),
                    etq = T) {
  dims <- paste0("Dim.", axes)
  inercias <- round(modelo$eig[axes, 2], digits = 2)
  
  row <- data.frame(modelo$row$coord[, axes])
  col <- data.frame(modelo$col$coord[, axes])
  
  colnames(row) <- c("x", "y")
  row$id <- row.names(row)
  row$cos2 <- apply(modelo$row$cos2[, axes], 1, sum, na.rm = T)
  row$cos <- factor(ifelse(row$cos2 >= cos2Row, 1, 0), levels = c(1, 0), 
                    labels = paste0("Ind. ", titulos))
  row$color <- ifelse(row$cos2 >= cos2Row, colorRow, colorRowCos)
  row$forma  <- "circle"
  
  colnames(col) <- c("x", "y")
  col$id <- row.names(col)
  col$cos2 <- apply(modelo$col$cos2[, axes], 1, sum, na.rm = T)
  col$cos  <- factor(ifelse(col$cos2 >= cos2Col, 1, 0), levels = c(1, 0), 
                     labels = paste0("Var. ", titulos))
  col$color <- ifelse(col$cos2 >= cos2Col, colorCol, colorColCos)
  col$forma  <- "diamond"
  
  rowcol <- rbind(row, col)
  
  r <- lapply(unique(rowcol$cos), function(cos) {
    data <- rowcol[rowcol$cos == cos, ]
    forma <- data$forma[1]
    color <- data$color[1]
    data <- data[, 1:2]
    data$nombre <- row.names(data)
    data <- apply(data, 1, function(d) 
      list(value = as.numeric(d[c(1, 2)]), name = as.character(d[3])))
    names(data) <- NULL
    
    list(
      type = "scatter", name = cos, data = data, 
      symbolSize = 15, symbol = forma, color = color
    )
  })
  
  res <- e_charts() |> e_list(list(
    xAxis  = list(type = "value"),
    yAxis  = list(type = "value", show = T),
    series = r
  )) |> e_show_loading() |> e_datazoom(show = F) |> e_legend() |>
    e_axis_labels(x = paste0("Dim.", axes[1], " (", inercias[1], ")"),
                  y = paste0("Dim.", axes[2], " (", inercias[2], ")")) |>
    e_tooltip(formatter = htmlwidgets::JS(
      "function(params) {
         return('<strong>' + params.name + '</strong>: (' + 
                params.value[0].toFixed(3) + ', ' + params.value[1].toFixed(3) + ')') 
      }"))
  
  if(etq) {
    res <- res |>
      e_labels(formatter = htmlwidgets::JS(
        "function(params) {
         return(params.name)
      }"))
  }
  
  return(res)
}

#' AFC biplot in 3D
#'
#' @param modelo an object of class CA [FactoMineR].
#' @param axes a numeric vector of length 3 specifying the dimensions to be plotted.
#' @param colorRow a color for the individuals well represented.
#' @param colorCol a color for the variables well represented.
#' @param cos2Row a numeric value from 0 to 1 specifying the quality of the individuals.
#' @param cos2Col a numeric value from 0 to 1 specifying the quality of the variables.
#' @param colorRowCos a color for individuals badly represented.
#' @param colorColCos a color for variables badly represented.
#' @param titulos a character vector of length 2 specifying the titles to use on legend.
#' @param etq a boolean, whether to add label to graph or not.
#' 
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_afcbi_3D
#' @import echarts4r
#' @examples
#' p <- FactoMineR::CA(iris[, -5], graph = FALSE)
#' e_afcbi_3D(p)
#' 
e_afcbi_3D <- function(modelo, axes = c(1, 2, 3), colorRow = "steelblue",
                       colorCol = "forestgreen", cos2Row = 0,  cos2Col = 0,
                       colorRowCos = "firebrick", colorColCos = "darkorchid",
                       titulos = c("Bien Representados", "Mal Representados"),
                       etq = T) {
  dims <- paste0("Dim.", axes)
  inercias <- round(modelo$eig[axes, 2], digits = 2)
  
  row <- data.frame(modelo$row$coord[, axes])
  col <- data.frame(modelo$col$coord[, axes])
  
  colnames(row) <- c("x", "y")
  row$id <- row.names(row)
  row$cos2 <- apply(modelo$row$cos2[, axes], 1, sum, na.rm = T)
  row$cos <- factor(ifelse(row$cos2 >= cos2Row, 1, 0), levels = c(1, 0), 
                    labels = paste0("Ind. ", titulos))
  row$color <- ifelse(row$cos2 >= cos2Row, colorRow, colorRowCos)
  row$forma  <- "circle"
  
  colnames(col) <- c("x", "y")
  col$id <- row.names(col)
  col$cos2 <- apply(modelo$col$cos2[, axes], 1, sum, na.rm = T)
  col$cos  <- factor(ifelse(col$cos2 >= cos2Col, 1, 0), levels = c(1, 0), 
                     labels = paste0("Var. ", titulos))
  col$color <- ifelse(col$cos2 >= cos2Col, colorCol, colorColCos)
  col$forma  <- "diamond"
  
  rowcol <- rbind(row, col)
  
  r <- lapply(unique(rowcol$cos), function(cos) {
    data <- rowcol[rowcol$cos == cos, ]
    forma <- data$forma[1]
    color <- data$color[1]
    data <- data[, 1:3]
    data$nombre <- row.names(data)
    data <- apply(data, 1, function(d) 
      list(value = as.numeric(d[c(1, 2, 3)]), name = as.character(d[4])))
    names(data) <- NULL
    
    list(
      type = "scatter3D", coordinateSystem = "cartesian3D", 
      name = as.character(cos),
      data = data, symbol_size = 15, symbol = forma, color = color
    )
  })
  
  res <- e_charts() |> e_list(list(
    xAxis3D = list(
      type = "value", axisLine = list(lineStyle = list(color = "white")),
      name = paste0("Dim.", axes[1], " (", inercias[1], ")")),
    yAxis3D = list(
      type = "value", axisLine = list(lineStyle = list(color = "white")),
      name = paste0("Dim.", axes[2], " (", inercias[2], ")")),
    zAxis3D = list(
      type = "value", show = T, axisLine = list(lineStyle = list(color = "white")),
      name = paste0("Dim.", axes[3], " (", inercias[3], ")")),
    grid3D  = list(show = T),
    series = r
  )) |> e_show_loading() |> e_legend() |> e_theme("dark") |>
    e_tooltip(formatter = htmlwidgets::JS(
      "function(params) {
         return('<strong>' + params.name + '</strong>: (' +
                params.value[0].toFixed(3) + ', ' + params.value[1].toFixed(3) +
                ', ' + params.value[2].toFixed(3) + ')')
      }"))
  
  if(etq) {
    res <- res |>
      e_labels(formatter = htmlwidgets::JS(
        "function(params) {
         return(params.name)
      }"))
  }
  
  path <- system.file("htmlwidgets/lib/echarts-4.8.0", package = "echarts4r")
  dep <- htmltools::htmlDependency(name = "echarts-gl", version = "1.1.2", 
                                   src = c(file = path), script = "echarts-gl.min.js")
  res$dependencies <- append(res$dependencies, list(dep))
  
  return(res)
}
