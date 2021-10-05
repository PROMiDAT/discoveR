centros.total <- function(DF) {
  apply(DF, 2, mean)
}

calc.inercia <- function(total, individuo) {
  return(inercia(0, 1, total, individuo))
}
inercia <- function(suma, i, total, individuo){
  if(i > length(total)){
    return(as.double(suma))
  }
  inercia(suma + ((total[i] - individuo[i])^2), i+1, total, individuo)
}

#' Calculate total inertia
#'
#' @param DF a data.frame object.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return numeric
#' @export inercia.total
#' 
inercia.total <- function(DF) {
  sum(scale(DF, scale = FALSE)^2)
}

#' Calculate inter-class inertia
#'
#' @param DF a data.frame object.
#' @param clusters a vector specifying the cluster of each individual.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return numeric
#' @export BP
#' @examples
#' m <- hclust(dist(iris[, -5]))
#' BP(iris[, -5], cutree(m, 3))
#'
BP <- function(DF, clusters) {
  BP2(0, 1, DF, centros.total(DF), length(unique(clusters)), clusters)
}
BP2 <- function(suma, i, DF, c.total, cant, clusters) {
  if(i > cant){
    return(suma)
  }
  BP2(suma + (length(clusters[clusters == i]) *
                calc.inercia(c.total, centros.total(DF[clusters == i, ]))),
      i + 1, DF, c.total, cant, clusters)
}

#' Calculate intra-class inertia
#'
#' @param DF a data.frame object.
#' @param clusters a vector specifying the cluster of each individual.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return numeric
#' @export WP
#' @examples
#' m <- hclust(dist(iris[, -5]))
#' WP(iris[, -5], cutree(m, 3))
#'
WP <- function(DF, clusters) {
  cant <- length(unique(clusters))
  centros.cluster <- lapply(1:cant, function(i) centros.total(DF[clusters == i, ]))
  res <- sapply(1:nrow(DF), function(i) 
    calc.inercia(DF[i, ], centros.cluster[[clusters[i]]]))
  return(sum(res))
}

#' Dendrogram plot
#'
#' @param model an object of class hclust.
#' @param k a vector specifying the cluster of each individual.
#' @param colors a vector of color for each cluster.
#' 
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return ggplot
#' @export gg_dendrograma
#' @importFrom ggplot2 ggplot aes geom_segment geom_text scale_linetype_manual scale_color_manual labs theme_void theme element_text element_line coord_flip ylim
#' @importFrom ggdendro dendro_data segment label
#' @importFrom grDevices rgb
#' @importFrom stats runif
#' 
gg_dendrograma <- function(model, k, colors = NULL) {
  if(is.null(colors)) {
    colors <- sapply(1:k, function(i) rgb(runif(1), runif(1), runif(1), 0.8))
  }
  colors <- c(colors, "gray")
  
  dendro <- dendro_data(model, type = "rectangle")
  
  clusters <- as.factor(cutree(model, k))
  cluster  <- clusters[model$order]
  clusters <- rep(0L, nrow(dendro$segments))
  heights  <- sort(model$height, decreasing = TRUE)
  height   <- mean(c(heights[k], heights[k - 1L]), na.rm = TRUE)
  
  for (i in 1:k) {
    xi      <-  dendro$labels$x[cluster == i]
    idx1    <-  dendro$segments$x    >= min(xi) & dendro$segments$x    <= max(xi)
    idx2    <-  dendro$segments$xend >= min(xi) & dendro$segments$xend <= max(xi)
    idx3    <-  dendro$segments$yend < height
    idx     <-  idx1 & idx2 & idx3
    clusters[idx] <- i
  }
  
  cluster.linea <- factor(as.integer(clusters < 1))
  clusters <- factor(clusters)
  
  maximo.y <- max(dendro$segments$y)
  
  ggplot() +
    geom_segment(
      data =  segment(dendro),
      aes(x = x, y = y, xend = xend, yend = yend, 
          color = clusters, linetype = cluster.linea)) +
    geom_text(
      data = label(dendro), aes(x = x, y = y, label = label, colour = cluster),
      vjust = 0.5, hjust = -0.1, size = 3) + 
    scale_linetype_manual(values = c(1, 2), guide = "none") +
    scale_color_manual(
      breaks = as.character(1:k), labels = paste0("Cluster ", 1:k),
      values = colors) + 
    labs(color = "Clusters") +
    theme_void() + theme(
      axis.text = element_text(color = "#50505030"),
      panel.grid.major.x = element_line(color = "#50505030", size  = 0.25)) + 
    coord_flip() + ylim(floor(maximo.y * 1.1), -ceiling(maximo.y * 0.1))
}

############################### Generar Código ################################
code.cj <- function(centrar, dist.method, hc.method, cant.cluster) {
  res <- "datos.aux <- var.numericas(datos)\n"
  if(centrar) {
    res <- paste0(res, "modelo <- hclust(dist(as.data.frame(scale(datos.aux)), method = '", 
                  dist.method, "'), method = '", hc.method, "')\n")
  } else {
    res <- paste0(res, "modelo <- hclust(dist(datos.aux, method = '", dist.method, 
                  "'), method = '", hc.method, "')\n")
  }
  res <- paste0(
    res, "clusters <- as.factor(cutree(modelo, k = ", cant.cluster, "))\n",
    "centros <- calc.centros(datos.aux, clusters)\n",
    "modelo.cj <- list(modelo = modelo, clusters = clusters, centros = centros)\n"
  )
  
  return(res)
}

code.inercia <- function(titulos) {
  paste0(
    "inercias <- data.frame(\n",
    "  total = inercia.total(datos.aux),\n",
    "  inter.clase = BP(datos.aux, modelo.cj$modelo, modelo.cj$clusters)\n",
    ")\ninercias$intra.clase <- inercias$total - inercias$inter.clase\n\n",
    "hc_inercia(inercias, 'cj_inercia', c('", 
    paste(titulos, collapse = "', '"), "'))\n"
  )
}

code.dendro <- function(k, colores) {
  paste0(
    "p <- gg_dendrograma(modelo.cj$modelo, ", k, ", c('", paste(colores, collapse = "', '"), "'))\n",
    "ggplotly(p, tooltip = c('y', 'cluster', 'clusters', 'label')) %>% \n",
    "  layout(showlegend = F, xaxis = list(showline = F), yaxis = list(showline = F)) %>%\n",
    "  style(textposition = 'right') %>% config(displaylogo = F)\n"
  )
}




