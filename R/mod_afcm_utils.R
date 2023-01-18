#' AFCM plot of individuals
#'
#' @param modelo an object of class AFCM [FactoMineR].
#' @param axes a numeric vector of length 2 specifying the dimensions to be plotted.
#' @param colorInd a color for the individuals well represented.
#' @param cos2 a numeric value from 0 to 1 specifying the quality of the individuals.
#' @param colorCos a color for individuals badly represented.
#' @param titulos a character vector of length 2 specifying the titles to use on legend.
#' @param etq a boolean, whether to add label to graph or not.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_afcmind
#' @import echarts4r
#' @examples
#' data("poison", package = "FactoMineR")
#' poison.active <- poison[1:55, 5:15]
#' p <- FactoMineR::MCA(poison.active, graph = FALSE)
#' e_afcmind(p)
#' 
e_afcmind <- function(modelo, axes = c(1, 2), colorInd = "steelblue", cos2 = 0, 
                      colorCos = "firebrick",
                      titulos = c("Bien Representados", "Mal Representados"),
                      etq = T) {
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

#' AFCM plot of individuals in 3D
#'
#' @param modelo an object of class AFCM [FactoMineR].
#' @param axes a numeric vector of length 3 specifying the dimensions to be plotted.
#' @param colorInd a color for the individuals well represented.
#' @param cos2 a numeric value from 0 to 1 specifying the quality of the individuals.
#' @param colorCos a color for individuals badly represented.
#' @param titulos a character vector of length 2 specifying the titles to use on legend.
#' @param etq a boolean, whether to add label to graph or not.
#' 
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_afcmind_3D
#' @import echarts4r
#' @examples
#' data("poison", package = "FactoMineR")
#' poison.active <- poison[1:55, 5:15]
#' p <- FactoMineR::MCA(poison.active, graph = FALSE)
#' e_afcmind_3D(p)
#' 
e_afcmind_3D <- function(modelo, axes = c(1, 2, 3), colorInd = "steelblue", cos2 = 0,
                         colorCos = "firebrick", 
                         titulos = c("Bien Representados", "Mal Representados"),
                         etq = T) {
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

#' AFCM plot of variables
#'
#' @param modelo an object of class AFCM [FactoMineR].
#' @param axes a numeric vector of length 2 specifying the dimensions to be plotted.
#' @param colorVar a color for the variables.
#' 
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_afcmvar
#' @import echarts4r
#' @examples
#' data("poison", package = "FactoMineR")
#' poison.active <- poison[1:55, 5:15]
#' p <- FactoMineR::MCA(poison.active, graph = FALSE)
#' e_afcmvar(p)
#' 
e_afcmvar <- function(modelo, axes = c(1, 2), colorVar = "forestgreen") {
  var <- data.frame(
    x = modelo$var$eta2[, axes[1]], y = modelo$var$eta2[, axes[2]])
  var$id <- row.names(var)
  
  inercias <- round(modelo$eig[, 2], digits = 2)[axes]
  
  var |> e_charts(x) |> 
    e_scatter(y, label = list(show = F), symbol_size = 15, symbol = "diamond", bind = id) |>
    e_x_axis(scale = T) |> e_y_axis(scale = T) |> e_datazoom(show = F) |>
    e_color(colorVar) |> e_legend(show = FALSE) |> e_show_loading() |>
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

#' AFCM plot of variables in 3D
#'
#' @param modelo an object of class AFCM [FactoMineR].
#' @param axes a numeric vector of length 3 specifying the dimensions to be plotted.
#' @param colorVar a color for the variables well represented.
#' 
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_afcmvar_3D
#' @import echarts4r
#' @examples
#' data("poison", package = "FactoMineR")
#' poison.active <- poison[1:55, 5:15]
#' p <- FactoMineR::MCA(poison.active, graph = FALSE)
#' e_afcmvar_3D(p)
#' 
e_afcmvar_3D <- function(modelo, axes = c(1, 2, 3), colorVar = "forestgreen") {
  var <- data.frame(
    x = modelo$var$eta2[, axes[1]], y = modelo$var$eta2[, axes[2]],
    z = modelo$var$eta2[, axes[3]])
  dims <- paste0("Dim.", axes)
  var$id <- row.names(var)
  
  inercias <- round(modelo$eig[axes, 2], digits = 2)
  
  var |> e_charts(x) |> 
    e_scatter_3d(y, z, label = list(show = F), symbol_size = 15, symbol = "diamond", bind = id) |>
    e_color(colorVar) |> e_show_loading() |> e_theme("dark") |>
    #e_legend(data = titulos) |> 
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

#' AFCM plot of categories
#'
#' @param modelo an object of class AFCM [FactoMineR].
#' @param axes a numeric vector of length 2 specifying the dimensions to be plotted.
#' @param colorCat a color for the categories well represented.
#' @param cos2 a numeric value from 0 to 1 specifying the quality of the categories.
#' @param colorCos a color for the categories badly represented.
#' @param titulos a character vector of length 2 specifying the titles to use on legend.
#' 
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_afcmcat
#' @import echarts4r
#' @examples
#' data("poison", package = "FactoMineR")
#' poison.active <- poison[1:55, 5:15]
#' p <- FactoMineR::MCA(poison.active, graph = FALSE)
#' e_afcmcat(p)
#' 
e_afcmcat <- function(modelo, axes = c(1, 2), colorCat = "forestgreen", cos2 = 0, 
                      colorCos = "darkorchid",
                      titulos = c("Bien Representados", "Mal Representados")) {
  var <- data.frame(
    x = modelo$var$coord[, axes[1]], y = modelo$var$coord[, axes[2]], 
    cos2 = apply(modelo$var$cos2[, axes], 1, sum, na.rm = T))
  var$id <- row.names(var)
  var$cos <- factor(ifelse(var$cos2 >= cos2, 1, 0), levels = c(1, 0), 
                    labels = titulos)
  
  colores <- c(colorCat, colorCos)
  if(sum(var$cos == titulos[1]) == 0) colores <- colorCos
  if(sum(var$cos == titulos[2])  == 0) colores <- colorCat
  
  inercias <- round(modelo$eig[, 2], digits = 2)[axes]
  
  var |> group_by(cos) |> e_charts(x) |> 
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

#' AFCM plot of categories in 3D
#'
#' @param modelo an object of class AFCM [FactoMineR].
#' @param axes a numeric vector of length 3 specifying the dimensions to be plotted.
#' @param colorCat a color for the categories well represented.
#' @param cos2 a numeric value from 0 to 1 specifying the quality of the categories.
#' @param colorCos a color for categories badly represented.
#' @param titulos a character vector of length 2 specifying the titles to use on legend.
#' 
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_afcmcat_3D
#' @import echarts4r
#' @examples
#' data("poison", package = "FactoMineR")
#' poison.active <- poison[1:55, 5:15]
#' p <- FactoMineR::MCA(poison.active, graph = FALSE)
#' e_afcmcat_3D(p)
#' 
e_afcmcat_3D <- function(modelo, axes = c(1, 2, 3), colorCat = "forestgreen",
                         cos2 = 0, colorCos = "darkorchid",
                         titulos = c("Bien Representados", "Mal Representados")) {
  var <- data.frame(
    x = modelo$var$coord[, axes[1]], y = modelo$var$coord[, axes[2]],
    z = modelo$var$coord[, axes[3]],
    cos2 = apply(modelo$var$cos2[, axes], 1, sum, na.rm = T))
  dims <- paste0("Dim.", axes)
  var$id <- row.names(var)
  var$cos = factor(ifelse(var$cos2 >= cos2, 1, 0), levels = c(1, 0), 
                   labels = titulos)
  inercias <- round(modelo$eig[axes, 2], digits = 2)
  
  colores <- c(colorCat, colorCos)
  if(sum(var$cos == titulos[1]) == 0) colores <- colorCos
  if(sum(var$cos == titulos[2]) == 0) colores <- colorCat
  
  var |> group_by(cos) |> e_charts(x) |> 
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

#' AFCM biplot
#'
#' @param modelo an object of class AFCM [FactoMineR].
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
#' @export e_afcmbi
#' @import echarts4r
#' @examples
#' data("poison", package = "FactoMineR")
#' poison.active <- poison[1:55, 5:15]
#' p <- FactoMineR::MCA(poison.active, graph = FALSE)
#' e_afcmbi(p)
#' 
e_afcmbi <- function(modelo, axes = c(1, 2), colorInd = "steelblue", 
                     colorVar = "forestgreen", cos2Ind = 0, cos2Var = 0, 
                     colorIndCos = "firebrick", colorVarCos = "darkorchid",
                     titulos = c("Bien Representados", "Mal Representados"),
                     etq = T) {
  dims <- paste0("Dim.", axes)
  inercias <- round(modelo$eig[axes, 2], digits = 2)
  
  ind <- data.frame(modelo$ind$coord[, axes])
  var <- data.frame(modelo$var$coord[, axes])
  
  colnames(ind) <- c("x", "y")
  ind$id <- row.names(ind)
  ind$cos2 <- apply(modelo$ind$cos2[, axes], 1, sum, na.rm = T)
  ind$cos <- factor(ifelse(ind$cos2 >= cos2Ind, 1, 0), levels = c(1, 0), 
                    labels = paste0("Ind. ", titulos))
  ind$color <- ifelse(ind$cos2 >= cos2Ind, colorInd, colorIndCos)
  ind$forma  <- "circle"
  
  colnames(var) <- c("x", "y")
  var$id <- row.names(var)
  var$cos2 <- apply(modelo$var$cos2[, axes], 1, sum, na.rm = T)
  var$cos  <- factor(ifelse(var$cos2 >= cos2Var, 1, 0), levels = c(1, 0), 
                     labels = paste0("Cat. ", titulos))
  var$color <- ifelse(var$cos2 >= cos2Var, colorVar, colorVarCos)
  var$forma  <- "diamond"
  
  indvar <- rbind(ind, var)
  
  r <- lapply(unique(indvar$cos), function(cos) {
    data <- indvar[indvar$cos == cos, ]
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

#' AFCM biplot in 3D
#'
#' @param modelo an object of class AFCM [FactoMineR].
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
#' @export e_afcmbi_3D
#' @import echarts4r
#' @examples
#' data("poison", package = "FactoMineR")
#' poison.active <- poison[1:55, 5:15]
#' p <- FactoMineR::MCA(poison.active, graph = FALSE)
#' e_afcmbi_3D(p)
#' 
e_afcmbi_3D <- function(modelo, axes = c(1, 2, 3), colorInd = "steelblue",
                        colorVar = "forestgreen", cos2Ind = 0,  cos2Var = 0,
                        colorIndCos = "firebrick", colorVarCos = "darkorchid",
                        titulos = c("Bien Representados", "Mal Representados"),
                        etq = T) {
  dims <- paste0("Dim.", axes)
  inercias <- round(modelo$eig[axes, 2], digits = 2)
  
  ind <- data.frame(modelo$ind$coord[, axes])
  var <- data.frame(modelo$var$coord[, axes])
  
  colnames(ind) <- c("x", "y")
  ind$id <- row.names(ind)
  ind$cos2 <- apply(modelo$ind$cos2[, axes], 1, sum, na.rm = T)
  ind$cos <- factor(ifelse(ind$cos2 >= cos2Ind, 1, 0), levels = c(1, 0), 
                    labels = paste0("Ind. ", titulos))
  ind$color <- ifelse(ind$cos2 >= cos2Ind, colorInd, colorIndCos)
  ind$forma  <- "circle"
  
  colnames(var) <- c("x", "y")
  var$id <- row.names(var)
  var$cos2 <- apply(modelo$var$cos2[, axes], 1, sum, na.rm = T)
  var$cos  <- factor(ifelse(var$cos2 >= cos2Var, 1, 0), levels = c(1, 0), 
                     labels = paste0("Cat. ", titulos))
  var$color <- ifelse(var$cos2 >= cos2Var, colorVar, colorVarCos)
  var$forma  <- "diamond"
  
  indvar <- rbind(ind, var)
  
  r <- lapply(unique(indvar$cos), function(cos) {
    data <- indvar[indvar$cos == cos, ]
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

