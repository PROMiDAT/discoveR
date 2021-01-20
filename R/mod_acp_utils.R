#' PCA plot of individuals
#'
#' @param modelo an object of class PCA [FactoMineR].
#' @param axes a numeric vector of length 2 specifying the dimensions to be plotted.
#' @param nombrearchivo a character value specifying the name to use when the plot is downloaded.
#' @param colorInd a color for the individuals well represented.
#' @param cos2 a numeric value from 0 to 1 specifying the quality of the individuals.
#' @param colorCos a color for individuals badly represented.
#' @param titulos a character vector of length 2 specifying the titles to use on legend.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return Highchart plot
#' @export hcpcaind
#' @importFrom highcharter hchart hc_add_series hc_plotOptions hc_exporting hc_chart hc_xAxis hc_yAxis
#' 
hcpcaind <- function(modelo, axes = c(1, 2), nombrearchivo = NULL, 
                     colorInd = "steelblue", cos2 = 0, colorCos = "firebrick",
                     titulos = c("Bien Representados", "Mal Representados")) {
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
  
  ind %>% group_by(cos) %>% e_charts(x) %>% 
    e_scatter(y, label = list(show = F), symbol_size = 10) %>%
    e_x_axis(scale = T) %>% e_y_axis(scale = T) %>% e_datazoom(show = F) %>%
    e_color(colores) %>% e_tooltip() %>% e_show_loading() %>%
    e_axis_labels(x = paste0("Dim.", axes[1], " (", inercias[1], ")"), 
                  y = paste0("Dim.", axes[2], " (", inercias[2], ")"))
}

#' PCA plot of individuals in 3D
#'
#' @param modelo an object of class PCA [FactoMineR].
#' @param axes a numeric vector of length 3 specifying the dimensions to be plotted.
#' @param colorInd a color for the individuals well represented.
#' @param cos2 a numeric value from 0 to 1 specifying the quality of the individuals.
#' @param colorCos a color for individuals badly represented.
#' @param titulos a character vector of length 2 specifying the titles to use on legend.
#' 
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return plotly plot
#' @export plotly_pcaind
#' @importFrom plotly plot_ly config layout
#' 
plotly_pcaind <- function(modelo, axes = c(1, 2, 3), colorInd = "steelblue",
                          cos2 = 0, colorCos = "firebrick",
                          titulos = c("Bien Representados", "Mal Representados")) {
  ind <- data.frame(modelo$ind$coord[, axes])
  dims <- colnames(ind)
  ind$id <- row.names(ind)
  ind$cos2 <- apply(modelo$ind$cos2[, axes], 1, sum, na.rm = T)
  ind$cos = factor(ifelse(ind$cos2 >= cos2, 1, 0), levels = c(1, 0), 
                   labels = titulos)
  inercias <- round(modelo$eig[axes, 2], digits = 2)
  
  plot_ly(ind, x = as.formula(paste0("~", dims[1])), 
          y = as.formula(paste0("~", dims[2])),
          z = as.formula(paste0("~", dims[3])),
          text = ~id, type = 'scatter3d', mode = 'markers', 
          color = ~cos, colors = c(colorInd, colorCos),
          hovertemplate = "%{text}<extra></extra>",
          marker = list(size = 5)) %>% 
    config(displaylogo = F) %>%
    layout(
      legend = list(orientation = "h", xanchor = "center", x = 0.5),
      paper_bgcolor = "black", scene = list(
        xaxis = list(title = paste0(dims[1], " ", inercias[1]), gridcolor = "white"), 
        yaxis = list(title = paste0(dims[2], " ", inercias[2]), gridcolor = "white"),
        zaxis = list(title = paste0(dims[3], " ", inercias[3]), gridcolor = "white")))
}

#' PCA plot of variables
#'
#' @param modelo an object of class PCA [FactoMineR].
#' @param axes a numeric vector of length 2 specifying the dimensions to be plotted.
#' @param nombrearchivo a character value specifying the name to use when the plot is downloaded.
#' @param colorVar a color for the variables well represented.
#' @param cos2 a numeric value from 0 to 1 specifying the quality of the variables.
#' @param colorCos a color for the variables badly represented.
#' @param titulos a character vector of length 2 specifying the titles to use on legend.
#' 
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return Highchart plot
#' @export hcpcavar
#' @importFrom highcharter highchart hc_add_series hc_plotOptions hc_exporting hc_chart hc_xAxis hc_yAxis
#' 
hcpcavar <- function(modelo, axes = c(1, 2), nombrearchivo = NULL, 
                     colorVar = "forestgreen", cos2 = 0, colorCos = "darkorchid",
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
                   formatter = JS(paste0(
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
    xAxis = list(min = -2, max = 2),
    yAxis = list(min = -1, max = 1),
    legend = list(data = titulos),
    series = lista
  )
  
  e_charts() %>% e_list(opts) %>% e_datazoom(show = F) %>%
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
#' @return plotly plot
#' @export plotly_pcavar
#' @importFrom plotly plot_ly add_trace config layout
#' 
plotly_pcavar <- function(modelo, axes = c(1, 2, 3), colorVar = "forestgreen",
                          cos2 = 0, colorCos = "darkorchid",
                          titulos = c("Bien Representados", "Mal Representados")) {
  # Circulo
  theta <- seq(0, 2*pi, length.out = 50)
  phi   <- seq(0, pi, length.out = 50)
  x <- outer(cos(theta), sin(phi))
  y <- outer(sin(theta), sin(phi))
  z <- outer(seq(1, 1, length.out = 50), cos(phi))
  
  inercias <- round(modelo$eig[axes, 2], digits = 2)
  
  r <- plot_ly() %>% 
    add_trace(type = 'surface', x = x, y = y, z = z, opacity = 0.5,
              hoverinfo = 'skip', showscale = F, showlegend = F)
  
  etiquetas <- list()
  primerBR <- T
  primerMR <- T
  for (id in row.names(modelo$var$coord)) {
    var <- modelo$var$coord[id, axes]
    var <- data.frame(rbind(c(0, 0, 0), var))
    cos2Var <- sum(modelo$var$cos2[id, axes])
    
    if(cos2Var >= cos2) {
      r <- r %>% add_trace(
        x = var[[1]], y = var[[2]], z = var[[3]], mode = 'lines',
        name = titulos[1], legendgroup = titulos[1],
        type = 'scatter3d', hovertemplate = paste0(id, "<extra></extra>"),
        line = list(color = colorVar, width = 5), showlegend = primerBR)
      
      primerBR <- F
    } else {
      r <- r %>% add_trace(
        x = var[[1]], y = var[[2]], z = var[[3]], mode = 'lines',
        name = titulos[2], legendgroup = titulos[2],
        type = 'scatter3d', hovertemplate = paste0(id, "<extra></extra>"),
        line = list(color = colorCos, width = 5), showlegend = primerMR)
      
      primerMR <- F
    }
    
    etiquetas <- append(etiquetas, list(list(
      x = var[2, 1], y = var[2, 2], z = var[2, 3], text = id, 
      xanchor = "left", xshift = 10, opacity = 0.8, showarrow = F,
      font = list(color = 'white', size = 12)
    )))
  }
  
  r <- r %>% config(displaylogo = F) %>% layout(
    legend = list(orientation = "h", xanchor = "center", x = 0.5),
    paper_bgcolor = "black",
    scene = list(
      annotations = etiquetas,
      xaxis = list(title = paste0(names(var)[1], " ", inercias[1]), gridcolor = "white"), 
      yaxis = list(title = paste0(names(var)[2], " ", inercias[2]), gridcolor = "white"),
      zaxis = list(title = paste0(names(var)[3], " ", inercias[3]), gridcolor = "white")
    )
  )
  
  r
}

#' PCA biplot
#'
#' @param modelo an object of class PCA [FactoMineR].
#' @param axes a numeric vector of length 2 specifying the dimensions to be plotted.
#' @param nombrearchivo a character value specifying the name to use when the plot is downloaded.
#' @param colorInd a color for the individuals well represented.
#' @param colorVar a color for the variables well represented.
#' @param cos2Ind a numeric value from 0 to 1 specifying the quality of the individuals.
#' @param cos2Var a numeric value from 0 to 1 specifying the quality of the variables.
#' @param colorIndCos a color for the individuals badly represented.
#' @param colorVarCos a color for the variables badly represented.
#' @param titulos a character vector of length 2 specifying the titles to use on legend.
#' 
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return Highchart plot
#' @export hcpcabi
#' @importFrom highcharter highchart hc_add_series hc_plotOptions hc_exporting hc_chart hc_xAxis hc_yAxis
#' 
hcpcabi <- function(modelo, axes = c(1, 2), nombrearchivo = NULL, 
                    colorInd = "steelblue", colorVar = "forestgreen", 
                    cos2Ind = 0,  cos2Var = 0, colorIndCos = "firebrick", 
                    colorVarCos = "darkorchid",
                    titulos = c("Bien Representados", "Mal Representados")) {
  ind <- data.frame(modelo$ind$coord[, axes])
  var <- data.frame(modelo$var$coord[, axes])
  ampliar <- min(
    (max(ind[, 1]) - min(ind[, 1])/(max(var[, 1]) - min(var[, 1]))), 
    (max(ind[, 2]) - min(ind[, 2])/(max(var[, 2]) - min(var[, 2])))
  ) * 0.7
  var <- var * ampliar
  
  ind <- data.frame(
    x = ind[, axes[1]], y = ind[, axes[2]], 
    cos2 = apply(modelo$ind$cos2[, axes], 1, sum, na.rm = T))
  ind$id <- row.names(ind)
  ind$cos <- factor(ifelse(ind$cos2 >= cos2Ind, 1, 0), levels = c(1, 0), 
                    labels = titulos)
  
  lista <- list(list(
    type = 'scatter', data = apply(ind[ind$cos == titulos[1], c(1, 2)], 2, function(i) i), 
    itemStyle = list(color = colorInd), name = paste0("Ind. ", titulos[1])))
  lista[[2]] <- list(
    type = 'scatter', data = apply(ind[ind$cos == titulos[2], c(1, 2)], 2, function(i) i), 
    itemStyle = list(color = colorIndCos), name = paste0("Ind. ", titulos[2]))
  
  for (i in 1:nrow(var)) {
    nombre <- paste0("Var. ", titulos[1])
    color  <- colorVar
    if(sum(modelo$var$cos2[i, axes]) < cos2Var) {
      nombre <- paste0("Var. ", titulos[2])
      color  <- colorVarCos
    }
    lista[[i + 2]] <- list(
      type = 'line', name = nombre, itemStyle = list(color = color),
      label = list(show = T, position = 'top', color = "black", 
                   formatter = JS(paste0(
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
    xAxis = list(scale = T),
    yAxis = list(scale = T),
    legend = list(data = c(paste0("Ind. ", titulos), paste0("Var. ", titulos))),
    series = lista
  )
  
  e_charts() %>% e_list(opts) %>% e_datazoom(show = F)
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
#' 
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return plotly plot
#' @export plotly_pcabi
#' @importFrom plotly plot_ly add_trace config layout
#' 
plotly_pcabi <- function(modelo, axes = c(1, 2, 3), colorInd = "steelblue",
                         colorVar = "forestgreen", cos2Ind = 0,  cos2Var = 0,
                         colorIndCos = "firebrick", colorVarCos = "darkorchid",
                         titulos = c("Bien Representados", "Mal Representados")) {
  ind <- data.frame(modelo$ind$coord[, axes])
  var <- data.frame(modelo$var$coord[, axes])
  ampliar <- min(
    (max(ind[, 1]) - min(ind[, 1])/(max(var[, 1]) - min(var[, 1]))), 
    (max(ind[, 2]) - min(ind[, 2])/(max(var[, 2]) - min(var[, 2]))),
    (max(ind[, 3]) - min(ind[, 3])/(max(var[, 3]) - min(var[, 3])))
  ) * 0.7
  
  dims <- colnames(ind)
  ind$id <- row.names(ind)
  ind$cos2 <- apply(modelo$ind$cos2[, axes], 1, sum, na.rm = T)
  ind$cos = factor(ifelse(ind$cos2 >= cos2Ind, 1, 0), levels = c(1, 0), 
                   labels = titulos)
  inercias <- round(modelo$eig[axes, 2], digits = 2)
  
  r <- plot_ly() %>% add_trace(
    x = ind[[1]], y = ind[[2]], z = ind[[3]], text = ind$id, 
    type = 'scatter3d', mode = 'markers', color = ind$cos, 
    colors = c(colorInd, colorIndCos), hovertemplate = "%{text}<extra></extra>",
    marker = list(size = 5))
  
  etiquetas <- list()
  primerBR <- T
  primerMR <- T
  for (id in row.names(modelo$var$coord)) {
    var <- modelo$var$coord[id, axes] * ampliar
    var <- data.frame(rbind(c(0, 0, 0), var))
    cos2V <- sum(modelo$var$cos2[id, axes])
    
    if(cos2V >= cos2Var) {
      r <- r %>% add_trace(
        x = var[[1]], y = var[[2]], z = var[[3]], mode = 'lines',
        name = titulos[1], legendgroup = titulos[1],
        type = 'scatter3d', hovertemplate = paste0(id, "<extra></extra>"),
        line = list(color = colorVar, width = 5), showlegend = primerBR)
      
      primerBR <- F
    } else {
      r <- r %>% add_trace(
        x = var[[1]], y = var[[2]], z = var[[3]], mode = 'lines',
        name = titulos[2], legendgroup = titulos[2],
        type = 'scatter3d', hovertemplate = paste0(id, "<extra></extra>"),
        line = list(color = colorVarCos, width = 5), showlegend = primerMR)
      
      primerMR <- F
    }
    
    etiquetas <- append(etiquetas, list(list(
      x = var[2, 1], y = var[2, 2], z = var[2, 3], text = id, 
      xanchor = "left", xshift = 10, opacity = 0.8, showarrow = F,
      font = list(color = 'white', size = 12)
    )))
  }
  
  r <- r %>% config(displaylogo = F) %>% layout(
    legend = list(orientation = "h", xanchor = "center", x = 0.5),
    paper_bgcolor = "black",
    scene = list(
      annotations = etiquetas,
      xaxis = list(title = paste0(names(var)[1], " ", inercias[1]), gridcolor = "white"), 
      yaxis = list(title = paste0(names(var)[2], " ", inercias[2]), gridcolor = "white"),
      zaxis = list(title = paste0(names(var)[3], " ", inercias[3]), gridcolor = "white")
    )
  )
  
  r
}
