#' Inverted versions of in, is.null and is.na
#' 
#' @noRd
#' 
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#' 
#' @noRd
#' 
#' @example 
#' dropNulls(list(1, NULL, 2))
dropNulls <- function (x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

#' If x is `NULL`, return y, otherwise return x
#' 
#' @param x,y Two elements to test, one potentially `NULL`
#' 
#' @noRd
#' 
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y){
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#' 
#' @param x,y Two elements to test, one potentially `NA`
#' 
#' @noRd
#' 
#' @examples
#' NA %||% 1
"%|NA|%" <- function(x, y){
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#' 
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#' 
#' @noRd
rv <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList

########################################################

load("inst/app/lang/translation.bin")

tr <- function(text, idioma = "es") {
  sapply(text, function(s) {
    elem <- ifelse(is.null(translation[[s]][[idioma]]), s,
                   translation[[s]][[idioma]])
    Encoding(elem) <- "utf8"
    
    elem
  }, USE.NAMES = F)
}

cambiar.labels <- function() {
  x <- c('idioma', 'selidioma', 'data', 'basico', 'acp', 'jerarquico',
         'inercia', 'inerciaintra', 'inerciainter', 'silhouette', 'reporte',
         'acercade', 'cargar', 'trans', 'header', 'Rownames', 'separador', 'coma',
         'puntocoma', 'tab', 'separadordec', 'eliminana', 'eliminar', 'imputar',
         'jambu', 'sil', 'punto', 'si', 'no', 'cargarchivo', 'subir', 'descarga',
         'buscar', 'numerico', 'numericas', 'categorico', 'categoricas',
         'disyuntivo', 'variables', 'tipo', 'activa', 'aplicar', 'resumen',
         'normalidad', 'dispersion', 'distribuciones', 'distribucion', 'kmedias',
         'correlacion', 'resumenvar', 'selvar', 'selvars', 'plotnormal',
         'titulonormal', 'curvanormal', 'densidad', 'opciones', 'selcolor',
         'selcolores', 'ejecutar', 'titulodistribucion', 'codigo', 'funciones',
         'codigodistnum', 'codigodistcat', 'cantidadcasos', 'categorias',
         'selmetodo', 'seltipo', 'individuos', 'sobreposicion', 'ayudacp', 'vee',
         'cci', 'ccv', 'cvc', 'cp', 'resultados', 'centrar', 'escalar', 'nocentrar',
         'numerodim', 'selejes', 'cosind', 'cosvar', 'selcolorind', 'selcolorvar',
         'dendograma', 'mapa', 'horizontal', 'vertical', 'radar', 'alfa',
         'interpretacioncat', 'cantcluster', 'numcluster', 'metcluster', 'sigue',
         'indiceagrega', 'metododist', 'agregarcluster', 'todos', 'jambu', 'kiter',
         'nstart', 'niter', 'algoritmo', 'tituloreporte', 'titulo', 'nombre',
         'descargar', 'salida', 'copyright', 'info', 'version', 'siguiente',
         'anterior', 'primero', 'ultimo', 'nodata', 'codedist', 'codecentros',
         'codehoriz', 'codevert', 'coderadar', 'codejambu', 'q1', 'mediana', 'q3',
         'minimo', 'promedio', 'maximo', 'ds', 'dimensiones', 'porcvee',
         'calidadcos', 'contribucion', 'positivo', 'negativo', 'sinasimetria',
         'fisher', 'asimetria', 'msjclusters', 'reptransformar', 'repeliminacion',
         'repmodelo', 'rephc', 'repcor', 'repcaind', 'repcavar', 'repcabi',
         'rephoriz', 'repvert', 'repradar', 'repcat', 'repinter', 'distancia',
         'metodo', 'iter', 'coseno', 'ejes', 'repcosind', 'repcosvar', 'repdim',
         'codreporte', 'errornum', 'errorcat', 'selcolbar', 'selcolline',
         'selcolpoint', 'histograma', '2D', '3D', 'colindbien', 'colindmal', 
         'colvarbien', 'colvarmal', 'longerror', 'pvalue', 'porc', 'abs', 
         'tasim', 'ori', 'res')
  
  return(x)
}

# Función para generar diccionario.
# crear.traslation <- function() {
#   library(plyr)
#   archivo <- read.table("diccionario.csv", header = TRUE, sep = ";", as.is = TRUE)
#   translation <- dlply(archivo , .(key), function(s) key = as.list(s))
#   
#   save(translation, file = "translation.bin")
# }