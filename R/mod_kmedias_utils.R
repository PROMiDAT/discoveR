#' Jambu Elbow plot
#'
#' @param data a data.frame object.
#' @param max.clusters a numeric value specifying the number of times to generate the model.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_jambu
#' @import echarts4r
#' @examples
#' e_jambu(iris[, -5], 10)
#' 
e_jambu <- function(data, max.clusters) {
  if(nrow(data) <= max.clusters) {
    max.clusters <- nrow(data) - 1
  }
  v <- sapply(1:max.clusters, function(i) {
    k <- kmeans(data, centers = i, iter.max = 100, nstart = 100)
    k$tot.withinss
  })
  Jambu <- NULL
  v <- data.frame(x = 1:max.clusters, Jambu = v)
  v <- round(v, 2)
  
  v %>% e_charts(x) %>% e_line(Jambu) %>% e_tooltip(
    formatter = htmlwidgets::JS(
      "function(params) {
      return('Inercia Intra-Clase: ' + params.value[1])
    }")
  )
}

#' Silhouette plot
#'
#' @param data a data.frame object.
#' @param max.clusters a numeric value specifying the number of times to generate the model.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_silhouette
#' @import echarts4r
#' @importFrom stats dist
#' @importFrom cluster silhouette
#' @examples
#' e_silhouette(iris[, -5], 10)
#' 
e_silhouette <- function(data, max.clusters) {
  if(nrow(data) <= max.clusters) {
    max.clusters <- nrow(data) - 1
  }
  
  v <- sapply(2:max.clusters, function(i) {
    k <- kmeans(data, centers = i, iter.max = 100, nstart = 100)
    mean(silhouette(k$cluster, dist(data))[, 3])
  })
  Silhouette <- NULL
  v <- data.frame(x = 1:max.clusters, Silhouette = c(0, v))
  m <- which.max(v$Silhouette)
  v <- round(v, 2)
  
  v %>% e_charts(x) %>% e_line(Silhouette) %>% 
    e_tooltip(
      formatter = htmlwidgets::JS(
        "function(params) {
        return('Inercia Intra-Clase: ' + params.value[1])
      }")
    ) %>% e_mark_line(data = list(xAxis = m), title = "", silent = T,
                      lineStyle = list(normal = list(width = 3)))
}

############################### Generar Código ################################
code.k <- function(centrar, cant.cluster, num.iter, num.nstart, sel.algoritmo) {
  res <- "datos.aux <- var.numericas(datos)\n"
  if(centrar) {
    res <- paste0(res, "modelo <- kmeans(as.data.frame(scale(datos.aux)), centers = ", cant.cluster, ", iter.max = ",
                  num.iter, ", nstart = ", num.nstart, ", algorithm = '", sel.algoritmo, "')\n")
  } else {
    res <- paste0(res, "modelo <- kmeans(datos.aux, centers = ", cant.cluster, ", iter.max = ",
                  num.iter, ", nstart = ", num.nstart, ", algorithm = '", sel.algoritmo, "')\n")
  }
  res <- paste0(
    res, "clusters <- as.factor(modelo$cluster)\n",
    "centros  <- calc.centros(data, clusters)\n\n",
    "modelo.k <- list(modelo = modelo, clusters = clusters, centros = centros)\n"
  )
  
  return(res)
}

code.kinercia <- function(titulos) {
  paste0(
    "inercias <- data.frame(\n",
    "  total       = modelo.k$modelo$totss,\n",
    "  inter.clase = modelo.k$modelo$betweenss,\n",
    "  intra.clase = modelo.k$modelo$tot.withinss\n",
    ")\n\nhc_inercia(inercias, 'k_inercia', c('", 
    paste(titulos, collapse = "', '"), "'))\n"
  )
}