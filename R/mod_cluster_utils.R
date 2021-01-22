#' Calculation of the center of clusters
#'
#' @param data a data.frame object.
#' @param clusters a vector specifying the cluster of each individual.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return list
#' @export calc.centros
#' @importFrom highcharter hchart hc_add_series hc_plotOptions hc_exporting hc_chart hc_xAxis hc_yAxis
#' @examples
#' clusters <- factor(kmeans(iris[, -5], 3)$cluster)
#' calc.centros(iris[, -5], clusters)
#' 
calc.centros <- function(data, clusters) {
  if(is.null(clusters)) return(NULL)
  real <- lapply(levels(clusters), function(i)
    colMeans(data[clusters == i, ]))
  real <- as.data.frame(do.call('rbind', real))
  porcentual <- apply(real, 2, function(i) scales::rescale(i, to = c(0, 100)))
  porcentual <- as.data.frame(porcentual)
  return(list(real = real, porcentual = porcentual))
}

#' Inertia plot of clusterization
#'
#' @param data a data.frame object with the inertia values.
#' @param nombre.archivo a character value specifying the name to use when the plot is downloaded.
#' @param titulos a character vector of length 3 specifying the titles to use on legend.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return Highchart plot
#' @export hc_inercia
#' @importFrom highcharter hchart hc_add_series hc_plotOptions hc_exporting hc_chart hc_xAxis hc_yAxis
#' 
hc_inercia <- function(data, nombre.archivo, titulos = c(
  "Inercia", "Inercia Inter-Clase", "Inercia Inter-Clase")) {
  porc <- round(data/data$total * 100, 2)
  data <- round(data, 2)
  etiq <- paste0("function() {return ", porc, " + '%<br>' + ", data, ";}")
  highchart() %>%
    hc_chart(type = "bar") %>% hc_xAxis(visible = F) %>%
    hc_plotOptions(bar = list(stacking = "normal")) %>% 
    hc_add_series(
      porc$total, name = titulos[1], stack = "a", color = "#0073b7",
      dataLabels = list(enabled = T, formatter = JS(etiq[1]))
    ) %>%
    hc_add_series(
      porc$inter.clase, name = titulos[2], stack = "b", color = "#00a65a",
      dataLabels = list(enabled = T, formatter = JS(etiq[2]))
    ) %>%
    hc_add_series(
      porc$intra.clase, name = titulos[3], stack = "b", color = "#dd4b39",
      dataLabels = list(enabled = T, formatter = JS(etiq[3]))
    ) %>% hc_chart(zoomType = "xy") %>% hc_tooltip(enabled = FALSE) %>%
    hc_exporting(enabled = T, filename = nombre.archivo)
}

#' PCA plot of individuals colored by clusters
#'
#' @param pca.model an object of class PCA [FactoMineR].
#' @param clusters a vector specifying the cluster of each individual.
#' @param nombre.archivo a character value specifying the name to use when the plot is downloaded.
#' @param colores a vector of color for each cluster.
#' @param ejes a numeric vector of length 2 specifying the dimensions to be plotted.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return Highchart plot
#' @export hc_mapa
#' @importFrom highcharter hchart hc_add_series hc_plotOptions hc_exporting hc_chart hc_xAxis hc_yAxis
#' @examples
#' p <- discoveR:::PCA(iris[, -5], graph = FALSE)
#' clusters <- factor(kmeans(iris[, -5], 3)$cluster)
#' hc_mapa(p, clusters, "map", c("steelblue", "pink", "forestgreen"))
#' 
hc_mapa <- function(pca.model, clusters, nombre.archivo = NULL, colores = NULL, ejes = c(1, 2)) {
  ind <- data.frame(pca.model$ind$coord[, ejes])
  ind$id <- row.names(ind)
  ind$cluster <- paste0("Cluster", clusters)
  ind$cluster <- factor(ind$cluster, levels = unique(ind$cluster)[order(unique(ind$cluster))])
  
  var <- data.frame(pca.model$var$coord[, ejes])
  ampliar <- min(
    (max(ind[, 1]) - min(ind[, 1])/(max(var[, 1]) - min(var[, 1]))), 
    (max(ind[, 2]) - min(ind[, 2])/(max(var[, 2]) - min(var[, 2])))
  ) * 0.7
  
  label.js   <- JS("function() {return(this.point.x == 0 ? '' : this.point.id)}")
  
  inercias <- round(pca.model$eig[, 2], digits = 2)
  
  res <- hchart(ind, "scatter", hcaes(x = Dim.1, y = Dim.2, group = cluster), 
         color = colores)
  for (x in row.names(var)) {
    aux <- data.frame(x = c(0, var[x, 1] * ampliar), 
                      y = c(0, var[x, 2] * ampliar), id = x)
    
    res <- res %>% hc_add_series(
      aux, "line", hcaes(x = x, y = y, group = id), color = "steelblue",
      showInLegend = F)
  }
  
  res <- res %>% hc_xAxis(
      title = list(text = paste0(names(ind)[1], " (", inercias[1], ")")),
      plotLines = list(list(width = 2, value = 0, dashStyle = "shortdash"))) %>%
    hc_yAxis(
      title = list(text = paste0(names(ind)[2], " (", inercias[2], ")")),
      plotLines = list(list(width = 2, value = 0, dashStyle = "shortdash"))) %>%
    hc_chart(zoomType = "xy") %>% 
    hc_plotOptions(
      scatter = list(
        dataLabels = list(enabled = T, formatter = label.js)
      ),
      line = list(
        marker = list(symbol = "diamond"),
        dataLabels = list(
          enabled = T, allowOverlap = T, formatter = label.js
        )
      )
    ) %>%
    hc_tooltip(headerFormat = "", pointFormat = "<b>{point.id}</b>") %>% 
    hc_exporting(enabled = T, filename = nombre.archivo)
  
  return(res)
}

#' PCA plot of individuals colored by clusters
#'
#' @param pca.model an object of class PCA [FactoMineR].
#' @param clusters a vector specifying the cluster of each individual.
#' @param colores a vector of color for each cluster.
#' @param ejes a numeric vector of length 3 specifying the dimensions to be plotted.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return plotly plot
#' @export plotly_mapa
#' @importFrom plotly plot_ly config layout
#' @examples
#' p <- discoveR:::PCA(iris[, -5], graph = FALSE)
#' clusters <- factor(kmeans(iris[, -5], 3)$cluster)
#' plotly_mapa(p, clusters, c("steelblue", "pink", "forestgreen"))
#' 
plotly_mapa <- function(pca.model, clusters, colores = NULL, ejes = c(1, 2, 3)) {
  ind <- data.frame(pca.model$ind$coord[, ejes])
  var <- data.frame(pca.model$var$coord[, ejes])
  ampliar <- min(
    (max(ind[, 1]) - min(ind[, 1])/(max(var[, 1]) - min(var[, 1]))), 
    (max(ind[, 2]) - min(ind[, 2])/(max(var[, 2]) - min(var[, 2]))),
    (max(ind[, 3]) - min(ind[, 3])/(max(var[, 3]) - min(var[, 3])))
  ) * 0.7
  
  dims <- colnames(ind)
  ind$id <- row.names(ind)
  ind$cluster <- paste0("Cluster", clusters)
  inercias <- round(pca.model$eig[ejes, 2], digits = 2)
  
  r <- plot_ly() %>% add_trace(
    x = ind[[1]], y = ind[[2]], z = ind[[3]], text = ind$id, 
    type = 'scatter3d', mode = 'markers', color = ind$cluster, 
    colors = colores, hovertemplate = "%{text}<extra></extra>",
    marker = list(size = 5))
  
  etiquetas <- list()
  for (id in row.names(pca.model$var$coord)) {
    var <- pca.model$var$coord[id, ejes] * ampliar
    var <- data.frame(rbind(c(0, 0, 0), var))
    
    r <- r %>% add_trace(
      x = var[[1]], y = var[[2]], z = var[[3]], mode = 'lines',
      type = 'scatter3d', hovertemplate = paste0(id, "<extra></extra>"),
      line = list(color = "steelblue", width = 5), showlegend = F)
    
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

#' Horizontal representation for centers of clusters.
#'
#' @param centros a data.frame object with the centers of the clusters.
#' @param nombre.archivo a character value specifying the name to use when the plot is downloaded.
#' @param colors a vector of color for each cluster.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return Highchart plot
#' @export hc_horiz
#' @importFrom highcharter hchart hc_add_series hc_plotOptions hc_exporting hc_chart hc_xAxis hc_yAxis
#' @examples
#' clusters <- factor(kmeans(iris[, -5], 3)$cluster)
#' c <- calc.centros(iris[, -5], clusters)
#' hc_horiz(c$real, 'horizontal', c("steelblue", "pink", "forestgreen"))
#' 
hc_horiz <- function(centros, nombre.archivo = NULL, colors = NULL) {
  data <- data.frame(x = character(), y = numeric(), grupo = factor())
  
  for (i in 1:nrow(centros)) {
    data <- rbind(data, data.frame(
      x = paste0(colnames(centros), "%SEP%", i), y = as.numeric(centros[i, ]), 
      grupo = paste0("Cluster", i), color = colors[i], nombre = colnames(centros),
      row.names = NULL))
  }
  
  data$grupo <- factor(data$grupo, levels = unique(data$grupo))
  
  xaxis.js <- paste0(
    "function() {",
    "  var colores = ['", paste(colors, collapse = "','"), "'];",
    "  var aux = this.value.split('%SEP%');",
    "  return `<span style='color: ${colores[aux[1] - 1]}'>${aux[0]}</span>`;",
    "}"
  )
  
  res <- hchart(data, "bar", hcaes(x, y, group = grupo), color = colors, 
         pointPadding = 0.05, groupPadding = 0.05) %>%
    hc_xAxis(labels = list(formatter = JS(xaxis.js)), tickWidth = 1,
             title = list(text = NULL)) %>%
    hc_yAxis(title = list(text = "")) %>% 
    hc_plotOptions(bar = list(stacking = "normal")) %>% 
    hc_tooltip(headerFormat = "", pointFormat = "<b>{point.grupo}</b><br>{point.nombre}: {point.y:.2f}") %>%
    hc_chart(zoomType = "xy")
  
  if(!is.null(nombre.archivo)) {
    res <- res %>% hc_exporting(enabled = T, filename = nombre.archivo)
  }
  
  res
}

#' Vertical representation for centers of clusters.
#'
#' @param centros a data.frame object with the centers of the clusters.
#' @param nombre.archivo a character value specifying the name to use when the plot is downloaded.
#' @param colors a vector of color for each cluster.
#' @param btntext a character value for the label of button to back.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return Highchart plot
#' @export hc_vert
#' @importFrom highcharter hchart hc_add_series hc_plotOptions hc_exporting hc_chart hc_xAxis hc_yAxis
#' @examples
#' clusters <- factor(kmeans(iris[, -5], 3)$cluster)
#' c <- calc.centros(iris[, -5], clusters)
#' hc_vert(c$real, 'vertical', c("steelblue", "pink", "forestgreen"))
#' 
hc_vert <- function(centros, nombre.archivo = NULL, colors = NULL, btntext = "Volver") {
  lang <- getOption("highcharter.lang")
  lang$drillUpText <- btntext
  options(highcharter.lang = lang)
  
  res <- highchart() %>% hc_tooltip(pointFormat = "{point.y:.2f}") %>%
    hc_xAxis(type = "category") %>% hc_chart(zoomType = "xy")
  
  idsDrill <- c()
  
  for (i in 1:nrow(centros)) {
    idsDrill <- c(idsDrill, paste0(colnames(centros), "%SEP%", i))
    data <- data.frame(name = colnames(centros), y = as.numeric(centros[i, ]),
                       drilldown = paste0(colnames(centros), "%SEP%", i))
    res <- res %>% hc_add_series(
      data, hcaes(name = name, y = y), name = paste0("Cluster", i), 
      type = "column", color = colors[i])
  }
  
  res <- res %>%
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = lapply(idsDrill, function(x) {
        aux <- strsplit(x, "%SEP%")[[1]]
        list(
          id = x, type = "column", name = paste0("Cluster", aux[2]),
          data = list(list(aux[1], centros[aux[2], aux[1]]))
        )
      })
    )
  
  if(!is.null(nombre.archivo)) {
    res <- res %>% hc_exporting(enabled = T, filename = nombre.archivo)
  }
  
  res
}

#' Radar representation for centers of clusters.
#'
#' @param centros a data.frame object with the centers of the clusters.
#' @param nombre.archivo a character value specifying the name to use when the plot is downloaded.
#' @param colores a vector of color for each cluster.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return Highchart plot
#' @export hc_radar
#' @importFrom highcharter hchart hc_add_series hc_plotOptions hc_exporting hc_chart hc_xAxis hc_yAxis
#' @examples
#' clusters <- factor(kmeans(iris[, -5], 3)$cluster)
#' c <- calc.centros(iris[, -5], clusters)
#' hc_radar(c$porcentual, 'radar', c("steelblue", "pink", "forestgreen"))
#' 
hc_radar <- function(centros, nombre.archivo, colores = NULL) {
  res <- highchart() %>%
    hc_chart(polar = TRUE)
  
  for (i in 1:nrow(centros)) {
    data <- data.frame(x = colnames(centros), y = as.numeric(centros[i, ]))
    res <- res %>% hc_add_series(
      data, hcaes(x = x, y = y), name = paste0("Cluster", i), type = "area", 
      color = colores[i])
  }
  
  res %>%
    hc_tooltip(pointFormat = "{point.y:.2f}") %>%
    hc_xAxis(categories = colnames(centros)) %>% 
    hc_yAxis(tickPositions = c(-10, 0, 25, 50, 75, 100, 120), labels = list(
      formatter = JS(
        "function() {
              if(this.value < 0)
                return '';
              else
                return this.value;
            }")
    )) %>% 
    hc_chart(zoomType = "xy") %>% 
    hc_exporting(enabled = T, filename = nombre.archivo) 
}

#' Barplot for categoric variable by clusters.
#'
#' @param clusters a vector specifying the cluster of each individual.
#' @param var a factor column of a data.frame.
#' @param nombre.archivo a character value specifying the name to use when the plot is downloaded.
#' @param colores a vector of color for each cluster.
#' @param escalar a boolean value specifying if use percentage or real values.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return Highchart plot
#' @export hc_cat
#' @importFrom highcharter hchart hc_add_series hc_plotOptions hc_exporting hc_chart hc_xAxis hc_yAxis
#' @examples
#' clusters <- factor(kmeans(iris[, -5], 3)$cluster)
#' hc_cat(clusters, iris[, 5], colores = c("steelblue", "pink", "forestgreen"))
#' 
hc_cat <- function(clusters, var, nombre.archivo = NULL, colores = NULL, escalar = T) {
  data <- data.frame(table(clusters, var))
  colnames(data) <- c("grupo", "x", "y")
  
  data$nombre <- data$x
  data$x <- paste0(data$x, "%SEP%", data$grupo)
  
  data$grupo <- paste0("Cluster", data$grupo)
  data$grupo <- factor(data$grupo, levels = unique(data$grupo))
  data <- data[order(data$grupo), ]
  
  if(escalar) {
    for (x in unique(data$grupo)) {
      data[data$grupo == x, "y"] <- data[data$grupo == x, "y"] / 
        sum(data[data$grupo == x, "y"]) * 100
    }
  }
  
  xaxis.js <- paste0(
    "function() {",
    "  var colores = ['", paste(colores, collapse = "','"), "'];",
    "  var aux = this.value.split('%SEP%');",
    "  return `<span style='color: ${colores[aux[1] - 1]}'>${aux[0]}</span>`;",
    "}"
  )
  
  res <- hchart(data, "column", hcaes(x, y, group = grupo), color = colores, 
                pointPadding = 0.05, groupPadding = 0.05) %>%
    hc_xAxis(labels = list(formatter = JS(xaxis.js)), tickWidth = 1,
             title = list(text = NULL)) %>%
    hc_yAxis(title = list(text = "")) %>% 
    hc_plotOptions(column = list(stacking = "normal")) %>% 
    hc_tooltip(headerFormat = "", pointFormat = "<b>{point.grupo}</b><br>{point.nombre}: {point.y:.2f}") %>%
    hc_chart(zoomType = "xy")
  
  if(!is.null(nombre.archivo)) {
    res <- res %>% hc_exporting(enabled = T, filename = nombre.archivo)
  }
  
  res
}