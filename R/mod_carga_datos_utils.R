#' Filter numeric variables of a data.frame
#'
#' @param data a data.frame object.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return data.frame
#' @export var.numericas
#' @examples
#' var.numericas(iris)
#' 
var.numericas <- function(data) {
  if(is.null(data)) return(NULL)
  subset(data, select = sapply(data, class) %in% c('numeric', 'integer'))
}

#' Filter category variables of a data.frame
#'
#' @param data a data.frame object.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return data.frame
#' @export var.categoricas
#' @examples
#' var.categoricas(iris)
#' 
var.categoricas <- function(data) {
  if(is.null(data)) return(NULL)
  subset(data, select = !sapply(data, class) %in% c('numeric', 'integer'))
}

#' Create disjunctive columns to a data.frame.
#'
#' @param data a data.frame object.
#' @param var the column name to apply disjunctive code.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return data.frame
#' @export datos.disyuntivos
#' @examples
#' datos.disyuntivos(iris, "Species")
#' 
datos.disyuntivos <- function(data, var) {
  if(is.null(data)) {
    return(NULL)
  }
  
  for (categoria in unique(data[, var])) {
    nueva.var <- as.numeric(data[, var] == categoria)
    data[, paste0(var, '.', categoria)] <- nueva.var
  }
  
  return(data)
}

accion.NAs <- function(datos, deleteNA = T) {
  if(deleteNA) {
    return(na.omit(datos))
  } else {
    moda <- function(x) x[which.max(summary(x))]
    
    for (var in colnames(datos)) {
      if(any(is.na(datos[, var]))) {
        if(class(datos[, var]) %in% c('numeric', 'integer')) {
          datos[, var][is.na(datos[, var])] <- mean(datos[, var], na.rm = T)
        } else {
          datos[, var][is.na(datos[, var])] <- moda(datos[, var])
        }
      }
    }
    return(datos)
  }
}

carga.datos <- function(nombre.filas = T, ruta = NULL, separador = ";",
                        sep.decimal = ",", encabezado = T, deleteNA = T) {
  if(!is.null(ruta)) {
    ruta <- gsub("\\", "/", ruta, fixed = T)
  }
  if(nombre.filas) {
    res <- read.table(
      ruta, header = encabezado, sep = separador, dec = sep.decimal,
      stringsAsFactors = T, row.names = 1)
  } else {
    res <- read.table(
      ruta, header = encabezado, sep = separador, dec = sep.decimal,
      stringsAsFactors = T)
  }
  return(accion.NAs(res, deleteNA))
}

############################### Generar Código ################################
code.carga <- function(nombre.filas = T, ruta = NULL, separador = ";",
                       sep.decimal = ",", encabezado = T, incluir.NA = F) {
  res <- paste0(
    "datos <- read.table(stringsAsFactors = T, '", ruta, "', header=", encabezado, 
    ", sep='", separador, "', dec = '", sep.decimal, "'", 
    ifelse(nombre.filas, ", row.names = 1", ""), ")")
  res <- paste0(res, "\n", code.NA(incluir.NA))
  return(res)
}

code.NA <- function(deleteNA = T) {
  res <- ifelse(
    deleteNA, "datos <- na.omit(datos)\n",
    paste0(
      "Mode <- function(x) {\n  x[which.max(summary(x))]\n}\n",
      "for (var in colnames(datos)) {\n",
      "  if(any(is.na(datos[, var]))){\n",
      "    if(class(datos[, var]) %in% c('numeric', 'integer')) {\n",
      "      datos[, var][is.na(datos[, var])] <- mean(datos[, var], na.rm = T)\n",
      "    } else {\n",
      "      datos[, var][is.na(datos[, var])] <- Mode(datos[, var])\n",
      "    }\n  }\n}"))
  return(res)
}

code.trans <- function(var, nuevo.tipo) {
  if(nuevo.tipo == "categorico"){
    return(paste0(
      "datos[['", var, "']] <- as.factor(datos[['", var, "']])\n"))
  } else if(nuevo.tipo == "numerico") {
    return(paste0(
      "datos[['", var, "']] <- as.numeric(sub(',', '.', datos[['",
      var, "']], fixed = TRUE))\n"))
  } else {
    return(paste0(
      "datos <- datos.disyuntivos(datos, '", var,"')\n", 
      "datos[['", var, "']] <- NULL\n"))
  }
}