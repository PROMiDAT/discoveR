dimensiones <- function(tam = 2) {
  n <- ceiling(sqrt(tam))
  diferencia <- Inf
  for (i in 1:n) {
    for (j in 1:n) {
      if((i * j) >= tam) {
        if((i * j) < diferencia) {
          res <- c(i, j)
          diferencia  <- (i * j)
        }
      }
    }
  }
  return(res)
}

#' Calculation of the center of clusters
#'
#' @param data a data.frame object.
#' @param clusters a vector specifying the cluster of each individual.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return list
#' @export calc.centros
#' @examples
#' clusters <- factor(kmeans(iris[, -5], 3)$cluster)
#' calc.centros(iris[, -5], clusters)
#' 
calc.centros <- function(data, clusters) {
  if(is.null(clusters)) return(NULL)
  real <- lapply(levels(clusters), function(i)
    colMeans(data[clusters == i, ]))
  real <- as.data.frame(do.call('rbind', real))
  porcentual <- apply(real, 2, function(i) (i - min(i)) / (max(i) - min(i)) * 100)
  porcentual <- as.data.frame(porcentual)
  return(list(real = real, porcentual = porcentual))
}

#' Inertia plot of clusterization
#'
#' @param data a data.frame object with the inertia values.
#' @param titulos a character vector of length 3 specifying the titles to use on legend.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_inercia
#' @import echarts4r
#' 
e_inercia <- function(data, titulos = c(
  "Inercia", "Inercia Inter-Clase", "Inercia Inter-Clase")) {
  porc <- round(data/data$total * 100, 2)
  data <- round(data, 2)
  
  opts <- list(
    legend = list(
      data = titulos
    ),
    xAxis = list(type = "value"),
    yAxis = list(type = "category", data = list("")),
    series = list(
      list(
        name  = titulos[1], type = "bar", stack = 'total',
        data  = list(porc$total),
        label = list(show = T, formatter = paste0(porc$total, "%\n", data$total))
      ),
      list(
        name  = titulos[2], type = "bar", stack = 'inercias',
        data  = list(porc$inter.clase),
        label = list(show = T, formatter = paste0(porc$inter.clase, "%\n", data$inter.clase))
      ),
      list(
        name  = titulos[3], type = "bar", stack = 'inercias',
        data  = list(porc$intra.clase),
        label = list(show = T, formatter = paste0(porc$intra.clase, "%\n", data$intra.clase))
      )
    )
  )
  
  e_charts() %>% e_list(opts)
}

#' PCA plot of individuals colored by clusters
#'
#' @param pca.model an object of class PCA [FactoMineR].
#' @param clusters a vector specifying the cluster of each individual.
#' @param colores a vector of color for each cluster.
#' @param ejes a numeric vector of length 2 specifying the dimensions to be plotted.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_mapa
#' @import echarts4r
#' @examples
#' p <- discoveR:::PCA(iris[, -5], graph = FALSE)
#' clusters <- factor(kmeans(iris[, -5], 3)$cluster)
#' e_mapa(p, clusters, c("steelblue", "pink", "forestgreen"))
#' 
e_mapa <- function(pca.model, clusters, colores = NULL, ejes = c(1, 2)) {
  dims <- paste0("Dim.", ejes)
  inercias <- round(pca.model$eig[ejes, 2], digits = 2)
  
  ind <- data.frame(pca.model$ind$coord[, ejes])
  var <- data.frame(pca.model$var$coord[, ejes])
  ampliar <- min(
    (max(ind[, 1]) - min(ind[, 1])/(max(var[, 1]) - min(var[, 1]))), 
    (max(ind[, 2]) - min(ind[, 2])/(max(var[, 2]) - min(var[, 2])))
  ) * 0.7
  
  colnames(ind) <- c("x", "y")
  ind$id <- row.names(ind)
  ind$cluster <- paste0("Cluster", clusters)
  ind$cluster <- factor(ind$cluster, levels = unique(ind$cluster))
  
  var <- var * ampliar
  colnames(var) <- c("x", "y")
  var$id <- row.names(var)
  
  r <- ind %>% group_by(cluster) %>% e_charts(x) %>% 
    e_scatter(y, symbol_size = 10, bind = id)
  
  for (i in 1:nrow(var)) {
    r$x$opts$series[[length(r$x$opts$series) + 1]] <- list(
      type = "line", name = var$cos[i], smooth = TRUE, color = "steelblue",
      data = list(list(value = c(0, 0, 0)), 
                  list(value = c(var[i, 1], var[i, 2], var[i, 3]))),
      lineStyle = list(width = 5),
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
  
  if(!is.null(colores)) {
    orden <- as.numeric(sapply(r$x$opts$legend$data, function(x) strsplit(x, "r")[[1]][2]))
    colores <- colores[orden]
    r <- r %>% e_color(colores)
  }
  
  label_clusters <- as.character(unique(ind$cluster))
  r$x$opts$legend$data <- lapply(label_clusters[order(label_clusters)], function(x) x)
  
  r %>% e_show_loading() %>% e_datazoom(show = F) %>%
    e_axis_labels(x = paste0(dims[1], " (", inercias[1], ")"), 
                  y = paste0(dims[2], " (", inercias[2], ")")) %>% 
    e_tooltip(formatter = htmlwidgets::JS(
      "function(params) {
        return('<strong>' + params.name + '</strong>: (' + 
               params.value[0].toFixed(3) + ', ' + params.value[1].toFixed(3) + ')') 
      }"))
}

#' PCA plot of individuals colored by clusters
#'
#' @param pca.model an object of class PCA [FactoMineR].
#' @param clusters a vector specifying the cluster of each individual.
#' @param colores a vector of color for each cluster.
#' @param ejes a numeric vector of length 3 specifying the dimensions to be plotted.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_mapa_3D
#' @import echarts4r
#' @examples
#' p <- discoveR:::PCA(iris[, -5], graph = FALSE)
#' clusters <- factor(kmeans(iris[, -5], 3)$cluster)
#' e_mapa_3D(p, clusters, c("steelblue", "pink", "forestgreen"))
#' 
e_mapa_3D <- function(pca.model, clusters, colores = NULL, ejes = c(1, 2, 3)) {
  dims <- paste0("Dim.", ejes)
  inercias <- round(pca.model$eig[ejes, 2], digits = 2)
  
  ind <- data.frame(pca.model$ind$coord[, ejes])
  var <- data.frame(pca.model$var$coord[, ejes])
  ampliar <- min(
    (max(ind[, 1]) - min(ind[, 1])/(max(var[, 1]) - min(var[, 1]))), 
    (max(ind[, 2]) - min(ind[, 2])/(max(var[, 2]) - min(var[, 2]))),
    (max(ind[, 3]) - min(ind[, 3])/(max(var[, 3]) - min(var[, 3])))
  ) * 0.7
  
  colnames(ind) <- c("x", "y", "z")
  ind$id <- row.names(ind)
  ind$cluster <- paste0("Cluster", clusters)
  ind$cluster <- factor(ind$cluster, levels = unique(ind$cluster))
  
  var <- var * ampliar
  colnames(var) <- c("x", "y", "z")
  var$id <- row.names(var)
  
  r <- ind %>% group_by(cluster) %>% e_charts(x) %>% 
    e_scatter_3d(y, z, symbol_size = 10, bind = id) %>% e_legend()
  
  for (i in 1:nrow(var)) {
    r$x$opts$series[[length(r$x$opts$series) + 1]] <- list(
      type = "line3D", name = var$cos[i], smooth = TRUE,
      data = list(list(value = c(0, 0, 0)),
                  list(value = c(var[i, 1], var[i, 2], var[i, 3]))),
      lineStyle = list(width = 5), color = "steelblue"
    )
    
    r$x$opts$series[[length(r$x$opts$series) + 1]] <- list(
      type = "scatter3D", name = var$cos[i], symbolSize = 1,
      data = list(list(value = c(var[i, 1], var[i, 2], var[i, 3]))),
      label = list(show = T, formatter = htmlwidgets::JS(paste0(
        "function(data) {\n",
        "    return '", var$id[i], "';\n",
        "}")), textStyle = list(color = "white"))
    )
  }
  
  if(!is.null(colores)) {
    orden <- as.numeric(sapply(names(r$x$data), function(x) strsplit(x, "r")[[1]][2]))
    colores <- colores[orden]
    r <- r %>% e_color(colores)
  }
  
  label_clusters <- as.character(unique(ind$cluster))
  r$x$opts$legend$data <- lapply(label_clusters[order(label_clusters)], function(x) x)
  
  r %>% e_show_loading() %>% e_theme("dark") %>% 
    e_x_axis_3d(name = paste0(dims[1], " (", inercias[1], ")"),
                axisLine = list(lineStyle = list(color = "white"))) %>% 
    e_y_axis_3d(name = paste0(dims[2], " (", inercias[2], ")"),
                axisLine = list(lineStyle = list(color = "white"))) %>%
    e_z_axis_3d(name = paste0(dims[3], " (", inercias[3], ")"),
                axisLine = list(lineStyle = list(color = "white"))) %>%
    e_tooltip(formatter = htmlwidgets::JS(
      "function(params) {
         return('<strong>' + params.name + '</strong>: (' + 
                params.value[0].toFixed(3) + ', ' + params.value[1].toFixed(3) + 
                ', ' + params.value[2].toFixed(3) + ')') 
      }"))
}

#' Horizontal representation for centers of clusters.
#'
#' @param centros a data.frame object with the centers of the clusters.
#' @param colores a vector of color for each cluster.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_horiz
#' @import echarts4r
#' @examples
#' clusters <- factor(kmeans(iris[, -5], 3)$cluster)
#' c <- calc.centros(iris[, -5], clusters)
#' e_horiz(c$real, c("steelblue", "pink", "forestgreen"))
#' 
e_horiz <- function(centros, colores = NULL) {
  data <- data.frame(t(centros))
  data <- round(data, 2)
  colnames(data) <- paste0("Cluster", 1:ncol(data))
  dims <- dimensiones(ncol(data))
  minimo <- ifelse(min(data) > 0, 0, floor(min(data)))
  maximo <- ceiling(max(data))
  data$vars <- row.names(data)
  
  x <- list()
  y <- list()
  g <- list()
  s <- list()
  n <- 0
  v <- round(80/dims[1])
  h <- round(80/dims[2])
  for (i in 1:dims[1]) {
    for (j in 1:dims[2]) {
      if((n+1) <= (ncol(data) - 1)) {
        x[[n+1]] <- list(gridIndex = n, min = minimo, max = maximo)
        
        if(j == 1) {
          y[[n+1]] <- list(type = 'category', gridIndex = n, data = data$vars)
        } else {
          y[[n+1]] <- list(type = 'category', gridIndex = n, 
                           data = data$vars, show = F)
        }
        
        izq <- paste0(((j-1) * h) + 12, "%")
        der <- paste0(((dims[2] - (j)) * h) + 12, "%")
        arr <- paste0(((i-1) * v) + 12, "%")
        baj <- paste0(((dims[1] - (i)) * v) + 12, "%")
        g[[n+1]] <- list(left = izq, right = der, top = arr, bottom = baj)
        
        if(is.null(colores)) {
          s[[n+1]] <- list(
            name = colnames(data)[n+1], type = "bar", data = data[[n+1]],
            xAxisIndex = n, yAxisIndex = n
          )
        } else {
          s[[n+1]] <- list(
            name = colnames(data)[n+1], type = "bar", data = data[[n+1]],
            xAxisIndex = n, yAxisIndex = n, color = colores[n+1]
          )
        }
        
        n <- n + 1
      }
    }
  }
  
  opts <- list(xAxis = x, yAxis = y, grid = g, series = s)
  
  e_charts() %>% e_list(opts) %>% e_legend() %>% 
    e_tooltip() %>% e_datazoom(show = F)
}

#' Vertical representation for centers of clusters.
#'
#' @param centros a data.frame object with the centers of the clusters.
#' @param colores a vector of color for each cluster.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_vert
#' @import echarts4r
#' @examples
#' clusters <- factor(kmeans(iris[, -5], 3)$cluster)
#' c <- calc.centros(iris[, -5], clusters)
#' e_vert(c$real, c("steelblue", "pink", "forestgreen"))
#' 
e_vert <- function(centros, colores = NULL) {
  data <- data.frame(t(centros))
  data <- round(data, 2)
  colnames(data) <- paste0("Cluster", 1:ncol(data))
  vars <- NULL
  data$vars <- row.names(data)
  
  r <- data %>% e_charts(vars) %>% e_tooltip() %>% 
    e_datazoom(type = 'slider', show = T) %>% 
    e_datazoom(type = 'inside', show = T)
  
  for (i in 1:(ncol(data) - 1)) {
    r <- r %>% e_bar_(colnames(data)[i], name = colnames(data)[i])
  }
  
  if(!is.null(colores)) {
    r <- r %>% e_color(colores)
  }
  
  r
}

#' Radar representation for centers of clusters.
#'
#' @param centros a data.frame object with the centers of the clusters.
#' @param colores a vector of color for each cluster.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_radar
#' @import echarts4r
#' @examples
#' clusters <- factor(kmeans(iris[, -5], 3)$cluster)
#' c <- calc.centros(iris[, -5], clusters)
#' e_radar(c$porcentual, c("steelblue", "pink", "forestgreen"))
#' 
e_radar <- function(centros, colores = NULL) {
  data <- data.frame(t(centros))
  data <- data + 10
  colnames(data) <- paste0("Cluster", 1:ncol(data))
  vars <- NULL
  data$vars <- row.names(data)
  
  text_tooltip <- ""
  for (i in 1:nrow(data)) {
    text_tooltip <- paste0(text_tooltip, "'<br/>", data$vars[i], ": ' + (params.value[", i-1, "]-10).toFixed(3)")
    if(i < nrow(data)) text_tooltip <- paste0(text_tooltip, " + ")
  }
  
  r <- data %>% e_charts(vars)
  
  for (i in 1:(ncol(data) - 1)) {
    r <- r %>% 
      e_radar_(colnames(data)[i], name = colnames(data)[i], max = 110,
               areaStyle = list())
  }
  
  r <- r %>%
    e_tooltip(
      formatter = htmlwidgets::JS(paste0(
        "function(params) {
          return(params.name + ", text_tooltip, ")
        }"
      )), trigger = "item"
    )
  
  if(!is.null(colores)) {
    r <- r %>% e_color(colores)
  }
  
  return(r)
}

#' Barplot for categoric variable by clusters.
#'
#' @param clusters a vector specifying the cluster of each individual.
#' @param var a factor column of a data.frame.
#' @param colores a vector of color for each cluster.
#' @param escalar a boolean value specifying if use percentage or real values.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_cat
#' @import echarts4r
#' @examples
#' clusters <- factor(kmeans(iris[, -5], 3)$cluster)
#' e_cat(clusters, iris[, 5], colores = c("steelblue", "pink", "forestgreen"))
#' 
e_cat <- function(clusters, var, colores = NULL, escalar = T) {
  data <- data.frame(table(clusters, var))
  colnames(data) <- c("grupo", "x", "y")
  
  cant <- length(unique(data$grupo))
  dims <- dimensiones(cant)
  minimo <- ifelse(escalar, 0, ifelse(min(data$y) > 0, 0, floor(min(data$y))))
  maximo <- ifelse(escalar, 100, ceiling(max(data$y)))
  
  x <- list()
  y <- list()
  g <- list()
  s <- list()
  n <- 0
  v <- round(80/dims[1])
  h <- round(80/dims[2])
  for (i in 1:dims[1]) {
    for (j in 1:dims[2]) {
      if((n+1) <= cant) {
        aux <- data[data$grupo == (n+1), ]
        if(escalar) {
          aux$y <- round(aux$y / sum(aux$y) * 100, 2)
        }
        
        x[[n+1]] <- list(gridIndex = n, min = minimo, max = maximo)
        
        if(j == 1) {
          y[[n+1]] <- list(type = 'category', gridIndex = n, data = aux$x)
        } else {
          y[[n+1]] <- list(type = 'category', gridIndex = n, 
                           data = aux$x, show = F)
        }
        
        izq <- paste0(((j-1) * h) + 12, "%")
        der <- paste0(((dims[2] - (j)) * h) + 12, "%")
        arr <- paste0(((i-1) * v) + 12, "%")
        baj <- paste0(((dims[1] - (i)) * v) + 12, "%")
        g[[n+1]] <- list(left = izq, right = der, top = arr, bottom = baj)
        
        if(is.null(colores)) {
          s[[n+1]] <- list(
            name = paste0("Cluster", (n+1)), type = "bar", data = aux$y,
            xAxisIndex = n, yAxisIndex = n
          )
        } else {
          s[[n+1]] <- list(
            name = paste0("Cluster", (n+1)), type = "bar", data = aux$y,
            xAxisIndex = n, yAxisIndex = n, color = colores[n+1]
          )
        }
        
        n <- n + 1
      }
    }
  }
  
  opts <- list(xAxis = x, yAxis = y, grid = g, series = s)
  
  r <- e_charts() %>% e_list(opts) %>% e_legend() %>% 
    e_tooltip() %>% e_datazoom(show = F)
  
  if(!is.null(colores)) {
    r <- r %>% e_color(colores)
  }
  
  r
}