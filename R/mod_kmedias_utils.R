#' Jambu Elbow plot
#'
#' @param data a data.frame object.
#' @param max.clusters a numeric value specifying the number of times to generate the model.
#' @param nombre.archivo a character value specifying the name to use when the plot is downloaded.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return Highchart plot
#' @export hc_jambu
#' @importFrom highcharter hchart hc_add_series hc_plotOptions hc_exporting hc_chart hc_xAxis hc_yAxis
#' @examples
#' hc_jambu(iris[, -5], 10)
#' 
hc_jambu <- function(data, max.clusters, nombre.archivo = NULL) {
    if(nrow(data) <= max.clusters) {
      max.clusters <- nrow(data) - 1
    }
    v <- sapply(1:max.clusters, function(i) {
      k <- kmeans(data, centers = i, iter.max = 100, nstart = 100)
      k$tot.withinss
    })
    v <- data.frame(x = 1:max.clusters, y = v)
    
    res <- highchart() %>% 
      hc_add_series(v, hcaes(x, y), type = "line", name = "Jambu") %>%
      hc_tooltip(pointFormat = "<b>Inercia Intra-Clase:</b> {point.y:.2f}",
                 headerFormat = "")
    
    if(!is.null(nombre.archivo)) {
      res <- res %>% hc_exporting(enabled = T, filename = nombre.archivo)
    }
    
    res
}

#' Silhouette plot
#'
#' @param data a data.frame object.
#' @param max.clusters a numeric value specifying the number of times to generate the model.
#' @param nombre.archivo a character value specifying the name to use when the plot is downloaded.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return Highchart plot
#' @export hc_silhouette
#' @importFrom highcharter hchart hc_add_series hc_plotOptions hc_exporting hc_chart hc_xAxis hc_yAxis
#' @importFrom cluster silhouette
#' @importFrom stats dist
#' @examples
#' hc_silhouette(iris[, -5], 10)
#' 
hc_silhouette <- function(data, max.clusters, nombre.archivo = NULL) {
  if(nrow(data) <= max.clusters) {
    max.clusters <- nrow(data) - 1
  }
  
  v <- sapply(2:max.clusters, function(i) {
    k <- kmeans(data, centers = i, iter.max = 100, nstart = 100)
    mean(silhouette(k$cluster, dist(data))[, 3])
  })
  v <- data.frame(x = 1:max.clusters, y = c(0, v))
  
  res <- highchart() %>% 
    hc_add_series(v, hcaes(x, y), type = "line", name = "Silhouette") %>%
    hc_tooltip(pointFormat = "<b>Silhouette:</b> {point.y:.2f}",
               headerFormat = "") %>%
    hc_xAxis(plotLines = list(list(
      value = which.max(v$y), width = 2, dashStyle = "Dash")))
  
  if(!is.null(nombre.archivo)) {
    res <- res %>% hc_exporting(enabled = T, filename = nombre.archivo)
  }
  
  res
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