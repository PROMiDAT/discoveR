#' PCA plot of individuals
#'
#' @param modelo an object of class PCA [FactoMineR].
#' @param axes a numeric vector of length 2 specifying the dimensions to be plotted.
#' @param colorInd a color for the individuals well represented.
#' @param cos2 a numeric value from 0 to 1 specifying the quality of the individuals.
#' @param colorCos a color for individuals badly represented.
#' @param titulos a character vector of length 2 specifying the titles to use on legend.
#' @param etq a boolean, whether to add label to graph or not.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_pcaind
#' @import echarts4r
#' @examples
#' p <- FactoMineR::PCA(iris[, -5], graph = FALSE)
#' e_pcaind(p)
#' 
e_pcaind <- function(modelo, axes = c(1, 2), colorInd = "steelblue", cos2 = 0, 
                     colorCos = "firebrick",
                     titulos = c("Bien Representados", "Mal Representados"),
                     etq = F) {
  ind <- data.frame(
    x = modelo$ind$coord[, axes[1]], y = modelo$ind$coord[, axes[2]], 
    cos2 = apply(modelo$ind$cos2[, axes], 1, sum, na.rm = T))
  ind$id <- row.names(ind)
  ind$cos <- factor(ifelse(ind$cos2 >= cos2, 1, 0), levels = c(1, 0), 
                    labels = titulos)
  
  colores <- c(colorInd, colorCos)
  if(sum(ind$cos == titulos[1]) == 0) colores <- colorCos
  if(sum(ind$cos == titulos[2])  == 0) colores <- colorInd
  
  inercias <- round(modelo$eig[, 2], digits = 2)[axes]
  
  res <- ind |> group_by(cos) |> e_charts(x) |> 
    e_scatter(y, label = list(show = F), symbol_size = 10, bind = id) |>
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

#' PCA plot of individuals in 3D
#'
#' @param modelo an object of class PCA [FactoMineR].
#' @param axes a numeric vector of length 3 specifying the dimensions to be plotted.
#' @param colorInd a color for the individuals well represented.
#' @param cos2 a numeric value from 0 to 1 specifying the quality of the individuals.
#' @param colorCos a color for individuals badly represented.
#' @param titulos a character vector of length 2 specifying the titles to use on legend.
#' @param etq a boolean, whether to add label to graph or not.
#' 
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_pcaind_3D
#' @import echarts4r
#' @examples
#' p <- FactoMineR::PCA(iris[, -5], graph = FALSE)
#' e_pcaind_3D(p)
#' 
e_pcaind_3D <- function(modelo, axes = c(1, 2, 3), colorInd = "steelblue", cos2 = 0,
                        colorCos = "firebrick", 
                        titulos = c("Bien Representados", "Mal Representados"),
                        etq = F) {
  ind <- data.frame(
    x = modelo$ind$coord[, axes[1]], y = modelo$ind$coord[, axes[2]],
    z = modelo$ind$coord[, axes[3]],
    cos2 = apply(modelo$ind$cos2[, axes], 1, sum, na.rm = T))
  dims <- paste0("Dim.", axes)
  ind$id <- row.names(ind)
  ind$cos = factor(ifelse(ind$cos2 >= cos2, 1, 0), levels = c(1, 0), 
                   labels = titulos)
  inercias <- round(modelo$eig[axes, 2], digits = 2)
  
  colores <- c(colorInd, colorCos)
  if(sum(ind$cos == titulos[1]) == 0) colores <- colorCos
  if(sum(ind$cos == titulos[2]) == 0) colores <- colorInd
  
  res <- ind |> group_by(cos) |> e_charts(x) |> 
    e_scatter_3d(y, z, label = list(show = F), symbol_size = 10, bind = id) |>
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

#' PCA plot of variables
#'
#' @param modelo an object of class PCA [FactoMineR].
#' @param axes a numeric vector of length 2 specifying the dimensions to be plotted.
#' @param colorVar a color for the variables well represented.
#' @param cos2 a numeric value from 0 to 1 specifying the quality of the variables.
#' @param colorCos a color for the variables badly represented.
#' @param titulos a character vector of length 2 specifying the titles to use on legend.
#' 
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_pcavar
#' @import echarts4r
#' @examples
#' p <- FactoMineR::PCA(iris[, -5], graph = FALSE)
#' e_pcavar(p)
#' 
e_pcavar <- function(modelo, axes = c(1, 2), colorVar = "forestgreen", cos2 = 0, 
                     colorCos = "darkorchid",
                     titulos = c("Bien Representados", "Mal Representados")) {
  var      <- modelo$var$coord
  var.cos2 <- modelo$var$cos2
  inercias <- round(modelo$eig[axes, 2], digits = 2)
  t <- seq(0, 2 * pi, length.out = 100)
  circulo <- data.frame(x = sin(t), y = cos(t))
  circulo <- apply(circulo, 2, function(i) i)
  
  lista <- list(list(type = 'line', data = circulo, symbol = 'none',
                     itemStyle = list(color = 'steelblue')))
  for (i in 1:nrow(var)) {
    nombre <- titulos[1]
    color  <- colorVar
    if(sum(var.cos2[i, axes]) < cos2) {
      nombre <- titulos[2]
      color  <- colorCos
    }
    lista[[i + 1]] <- list(
      type = 'line', name = nombre, itemStyle = list(color = color),
      label = list(show = T, position = 'top', color = "black", 
                   formatter = htmlwidgets::JS(paste0(
                     "function(data) {\n",
                     "  var v = data.value;\n",
                     "  if(v[0] == 0 && v[1] == 0)\n",
                     "    return '';\n",
                     "  else\n",
                     "    return '", row.names(var)[i], "';\n",
                     "}"
                   ))),
      data = matrix(c(0, var[i, axes[1]], 0, var[i, axes[2]]), nrow = 2))
  }
  
  opts <- list(
    xAxis  = list(min = -1, max = 1),
    yAxis  = list(min = -1, max = 1),
    grid   = list(left = 100, right = 100, top = 100, bottom = 100),
    legend = list(data = titulos),
    series = lista
  )
  
  e_charts() |> e_list(opts) |> e_datazoom(show = F) |>
    e_axis_labels(x = paste0("Dim.", axes[1], " (", inercias[1], ")"), 
                  y = paste0("Dim.", axes[2], " (", inercias[2], ")"))
}

#' PCA plot of variables in 3D
#'
#' @param modelo an object of class PCA [FactoMineR].
#' @param axes a numeric vector of length 3 specifying the dimensions to be plotted.
#' @param colorVar a color for the variables well represented.
#' @param cos2 a numeric value from 0 to 1 specifying the quality of the variables.
#' @param colorCos a color for variables badly represented.
#' @param titulos a character vector of length 2 specifying the titles to use on legend.
#' 
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_pcavar_3D
#' @import echarts4r
#' @examples
#' p <- FactoMineR::PCA(iris[, -5], graph = FALSE)
#' e_pcavar_3D(p)
#' 
e_pcavar_3D <- function(modelo, axes = c(1, 2, 3), colorVar = "forestgreen",
                        cos2 = 0, colorCos = "darkorchid",
                        titulos = c("Bien Representados", "Mal Representados")) {
  var      <- data.frame(modelo$var$coord[, c(1, 2, 3)])
  colnames(var) <- c("x", "y", "z")
  var$nombre <- row.names(var)
  var$cos2 <- apply(modelo$var$cos2[, c(1, 2, 3)], 1, sum, na.rm = T)
  var$cos  <- factor(ifelse(var$cos2 >= cos2, 1, 0), levels = c(1, 0), 
                     labels = titulos)
  
  dims <- paste0("Dim.", axes)
  inercias <- round(modelo$eig[axes, 2], digits = 2)
  
  r <- var |> group_by(cos) |>
    e_charts(x) |> 
    e_scatter_3d(y, z, bind = nombre, symbolSize = 1, label = list(
      show = T, position = 'top',
      formatter = htmlwidgets::JS(paste0(
        "function(data) {\n",
        "    return data.name;\n",
        "}"
      ))
    ))
  
  for (i in 1:nrow(var)) {
    nombre <- titulos[1]
    color  <- colorVar
    if(var$cos[i] == 'Mal Representados') {
      nombre <- titulos[2]
      color  <- colorCos
    }
    r$x$opts$series[[length(r$x$opts$series) + 1]] <- list(
      type = "line3D", name = nombre, smooth = TRUE,
      data = list(list(value = c(0, 0, 0)), 
                  list(value = c(var[i, 1], var[i, 2], var[i, 3]))),
      label = list(show = T,
                   formatter = htmlwidgets::JS(paste0(
                     "function(data) {\n",
                     "    return '", row.names(var)[i], "';\n",
                     "}"
                   ))),
      lineStyle = list(width = 5)
    )
  }
  
  r$x$opts$series[[length(r$x$opts$series) + 1]] <- list(
    type = "surface",
    shading = 'color',
    tooltip = list(show = FALSE),
    itemStyle = list(color = 'steelblue', opacity = 0.2),
    wireframe = list(show = FALSE),
    parametric = TRUE,
    parametricEquation = htmlwidgets::JS(
      "{
       u: {min: 0, max: 2 * Math.PI, step: Math.PI / 20},
       v: {min: 0, max: Math.PI, step: Math.PI / 20},
       x: function (u, v) {
            return Math.sin(v) * Math.sin(u);
       },
       y: function (u, v) {
            return Math.sin(v) * Math.cos(u);
       },
       z: function (u, v) {
            return Math.cos(v);
       }
     }"
    ),
    name = NULL, 
    smooth = TRUE
  )
  
  r$x$opts$legend$data <- titulos
  r |> e_color(c(colorVar, colorCos)) |> e_show_loading() |> 
    e_x_axis_3d(name = paste0(dims[1], " (", inercias[1], ")"),
                axisLine = list(lineStyle = list(color = "white"))) |> 
    e_y_axis_3d(name = paste0(dims[2], " (", inercias[2], ")"),
                axisLine = list(lineStyle = list(color = "white"))) |>
    e_z_axis_3d(name = paste0(dims[3], " (", inercias[3], ")"),
                axisLine = list(lineStyle = list(color = "white"))) |>
    e_theme("dark")
}

#' PCA biplot
#'
#' @param modelo an object of class PCA [FactoMineR].
#' @param axes a numeric vector of length 2 specifying the dimensions to be plotted.
#' @param colorInd a color for the individuals well represented.
#' @param colorVar a color for the variables well represented.
#' @param cos2Ind a numeric value from 0 to 1 specifying the quality of the individuals.
#' @param cos2Var a numeric value from 0 to 1 specifying the quality of the variables.
#' @param colorIndCos a color for the individuals badly represented.
#' @param colorVarCos a color for the variables badly represented.
#' @param titulos a character vector of length 2 specifying the titles to use on legend.
#' @param etq a boolean, whether to add label to graph or not.
#' 
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_pcabi
#' @import echarts4r
#' @examples
#' p <- FactoMineR::PCA(iris[, -5], graph = FALSE)
#' e_pcabi(p)
#' 
e_pcabi <- function(modelo, axes = c(1, 2), colorInd = "steelblue", 
                    colorVar = "forestgreen", cos2Ind = 0, cos2Var = 0, 
                    colorIndCos = "firebrick", colorVarCos = "darkorchid",
                    titulos = c("Bien Representados", "Mal Representados"),
                    etq = F) {
  dims <- paste0("Dim.", axes)
  inercias <- round(modelo$eig[axes, 2], digits = 2)
  
  ind <- data.frame(modelo$ind$coord[, axes])
  var <- data.frame(modelo$var$coord[, axes])
  ampliar <- min(
    (max(ind[, 1]) - min(ind[, 1])/(max(var[, 1]) - min(var[, 1]))), 
    (max(ind[, 2]) - min(ind[, 2])/(max(var[, 2]) - min(var[, 2])))
  ) * 0.7
  
  colnames(ind) <- c("x", "y")
  ind$id <- row.names(ind)
  ind$cos2 <- apply(modelo$ind$cos2[, axes], 1, sum, na.rm = T)
  ind$cos <- factor(ifelse(ind$cos2 >= cos2Ind, 1, 0), levels = c(1, 0), 
                    labels = paste0("Ind. ", titulos))
  
  var <- var * ampliar
  colnames(var) <- c("x", "y")
  var$id <- row.names(var)
  var$cos2 <- apply(modelo$var$cos2[, axes], 1, sum, na.rm = T)
  var$cos  <- factor(ifelse(var$cos2 >= cos2Var, 1, 0), levels = c(1, 0), 
                     labels = paste0("Var. ", titulos))
  var$color <- ifelse(var$cos2 >= cos2Var, colorVar, colorVarCos)
  
  leyenda <- c(paste0("Ind. ", titulos), paste0("Var. ", titulos))
  
  r <- ind |> group_by(cos) |> e_charts(x) |> 
    e_scatter(y, symbol_size = 10, bind = id) |> 
    e_color(c(colorInd, colorIndCos))
  
  if(etq) {
    r <- r |>
      e_labels(formatter = htmlwidgets::JS(
        "function(params) {
         return(params.name)
      }"))
  }
  
  for (i in 1:nrow(var)) {
    r$x$opts$series[[length(r$x$opts$series) + 1]] <- list(
      type = "line", name = var$cos[i], smooth = TRUE,
      data = list(list(value = c(0, 0, 0)), 
                  list(value = c(var[i, 1], var[i, 2], var[i, 3]))),
      lineStyle = list(width = 5, color = var$color[i]),
      itemStyle = list(normal = list(color = var$color[i])),
      label = list(show = T, position = 'top', color = "black", 
                   formatter = htmlwidgets::JS(paste0(
                     "function(data) {\n",
                     "  var v = data.value;\n",
                     "  if(v[0] == 0 && v[1] == 0)\n",
                     "    return '';\n",
                     "  else\n",
                     "    return '", row.names(var)[i], "';\n",
                     "}"
                   )))
    )
  }
  
  r$x$opts$legend$data <- leyenda
  
  res <- r |> e_show_loading() |> e_datazoom(show = F) |>
    e_axis_labels(x = paste0("Dim.", axes[1], " (", inercias[1], ")"), 
                  y = paste0("Dim.", axes[2], " (", inercias[2], ")")) |> 
    e_tooltip(formatter = htmlwidgets::JS(
      "function(params) {
         return('<strong>' + params.name + '</strong>: (' + 
               params.value[0].toFixed(3) + ', ' + params.value[1].toFixed(3) + ')') 
      }"))
  
  return(res)
}

#' PCA biplot in 3D
#'
#' @param modelo an object of class PCA [FactoMineR].
#' @param axes a numeric vector of length 3 specifying the dimensions to be plotted.
#' @param colorInd a color for the individuals well represented.
#' @param colorVar a color for the variables well represented.
#' @param cos2Ind a numeric value from 0 to 1 specifying the quality of the individuals.
#' @param cos2Var a numeric value from 0 to 1 specifying the quality of the variables.
#' @param colorIndCos a color for individuals badly represented.
#' @param colorVarCos a color for variables badly represented.
#' @param titulos a character vector of length 2 specifying the titles to use on legend.
#' @param etq a boolean, whether to add label to graph or not.
#' 
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_pcabi_3D
#' @import echarts4r
#' @examples
#' p <- FactoMineR::PCA(iris[, -5], graph = FALSE)
#' e_pcabi_3D(p)
#' 
e_pcabi_3D <- function(modelo, axes = c(1, 2, 3), colorInd = "steelblue",
                       colorVar = "forestgreen", cos2Ind = 0,  cos2Var = 0,
                       colorIndCos = "firebrick", colorVarCos = "darkorchid",
                       titulos = c("Bien Representados", "Mal Representados"),
                       etq = F) {
  dims <- paste0("Dim.", axes)
  inercias <- round(modelo$eig[axes, 2], digits = 2)
  
  ind <- data.frame(modelo$ind$coord[, axes])
  var <- data.frame(modelo$var$coord[, axes])
  ampliar <- min(
    (max(ind[, 1]) - min(ind[, 1])/(max(var[, 1]) - min(var[, 1]))), 
    (max(ind[, 2]) - min(ind[, 2])/(max(var[, 2]) - min(var[, 2]))),
    (max(ind[, 3]) - min(ind[, 3])/(max(var[, 3]) - min(var[, 3])))
  ) * 0.7
  
  colnames(ind) <- c("x", "y", "z")
  ind$id <- row.names(ind)
  ind$cos2 <- apply(modelo$ind$cos2[, axes], 1, sum, na.rm = T)
  ind$cos <- factor(ifelse(ind$cos2 >= cos2Ind, 1, 0), levels = c(1, 0), 
                    labels = paste0("Ind. ", titulos))
  
  var <- var * ampliar
  colnames(var) <- c("x", "y", "z")
  var$id <- row.names(var)
  var$cos2 <- apply(modelo$var$cos2[, axes], 1, sum, na.rm = T)
  var$cos  <- factor(ifelse(var$cos2 >= cos2Var, 1, 0), levels = c(1, 0), 
                     labels = paste0("Var. ", titulos))
  
  leyenda <- c(paste0("Ind. ", titulos), paste0("Var. ", titulos))
  colores <- c(colorInd, colorIndCos, colorVar, colorVarCos)
  colores <- colores[leyenda %in% c(as.character(unique(ind$cos)), as.character(unique(var$cos)))]
  
  r <- ind |> group_by(cos) |> e_charts(x) |> 
    e_scatter_3d(y, z, symbol_size = 10, bind = id)
  
  if(etq) {
    r <- r |>
      e_labels(formatter = htmlwidgets::JS(
        "function(params) {
         return(params.name)
      }"))
  }
  
  for (i in 1:nrow(var)) {
    r$x$opts$series[[length(r$x$opts$series) + 1]] <- list(
      type = "line3D", name = var$cos[i], smooth = TRUE,
      data = list(list(value = c(0, 0, 0)), 
                  list(value = c(var[i, 1], var[i, 2], var[i, 3]))),
      lineStyle = list(width = 5)
    )
    
    r$x$opts$series[[length(r$x$opts$series) + 1]] <- list(
      type = "scatter3D", name = var$cos[i], symbolSize = 1,
      data = list(list(value = c(var[i, 1], var[i, 2], var[i, 3]))),
      label = list(show = T, formatter = htmlwidgets::JS(paste0(
        "function(data) {\n",
        "    return '", var$id[i], "';\n",
        "}")))
    )
  }
  
  r$x$opts$legend$data <- leyenda
  
  res <- r |> e_color(colores) |> e_show_loading() |>
    e_x_axis_3d(name = paste0(dims[1], " (", inercias[1], ")"),
                axisLine = list(lineStyle = list(color = "white"))) |> 
    e_y_axis_3d(name = paste0(dims[2], " (", inercias[2], ")"),
                axisLine = list(lineStyle = list(color = "white"))) |>
    e_z_axis_3d(name = paste0(dims[3], " (", inercias[3], ")"),
                axisLine = list(lineStyle = list(color = "white"))) |>
    e_theme("dark") |> 
    e_tooltip(formatter = htmlwidgets::JS(
      "function(params) {
         return('<strong>' + params.name + '</strong>: (' + 
                params.value[0].toFixed(3) + ', ' + params.value[1].toFixed(3) + 
                ', ' + params.value[2].toFixed(3) + ')') 
      }"))
  
  return(res)
}
