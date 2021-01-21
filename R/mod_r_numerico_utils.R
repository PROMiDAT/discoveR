###################### Estadisticas Basicas ###################################
#' Returns HTML for numeric data summary
#'
#' @return HTML
#' @noRd
#' 
#' @examples
#' resumen.numerico(iris, "Sepal.Length")
#' 
resumen.numerico <- function(data, variable, idioma = "es") {
  datos.numericos <- list(
    Q1 = list(
      id = "q1", Label = tags$span(`data-id`="q1", tr("q1", idioma)), color = "green",
      Value = format(round(quantile(data[, variable], .25), 3), scientific = F)
    ),
    Mediana = list(
      id = "mediana", Label = tags$span(`data-id`="mediana", tr("mediana", idioma)),
      Value = format(round(median(data[, variable]), 3), scientific = F),
      color = "orange"),
    Q3 = list(
      id = "q3", Label = tags$span(`data-id`="q3", tr("q3", idioma)), color = "maroon",
      Value = format(round(quantile(data[, variable], .75), 3), scientific = F)
    ),
    Minimo = list(
      id = "minimo", Label = tags$span(`data-id`="minimo", tr("minimo", idioma)),
      Value = format(round(min(data[, variable]), 3), scientific = F),
      color = "red"),
    Promedio = list(
      id = "promedio", Label = tags$span(`data-id`="promedio", tr("promedio", idioma)),
      Value = format(round(mean(data[, variable]), 3), scientific = F),
      color = "blue"),
    Maximo = list(
      id = "maximo", Label = tags$span(`data-id`="maximo", tr("maximo", idioma)),
      Value = format(round(max(data[, variable]), 3), scientific = F),
      color = "purple"),
    DS <- list(
      id = "ds", Label = tags$span(`data-id`="ds", tr("ds", idioma)), color = "yellow",
      Value = format(round(sd(data[, variable]), 3), scientific = FALSE, nsmall = 3)
    )
  )
  
  res <- lapply(datos.numericos, function(i) {
    tags$div(
      class='shiny-html-output col-sm-6 shiny-bound-output', id=i$id,
      tags$div(
        class=paste0('small-box bg-', i$color),
        tags$div(class='inner', tags$h3(i$Value), tags$p(i$Label)),
        tags$div(class='icon-large', tags$i(class=i$icon))
      )
    )
  })
  return(res)
}

#' Returns HTML for category data summary
#'
#' @return HTML
#' @noRd
#' 
#' @examples
#' resumen.categorico(iris, "Sepal.Length")
#' 
resumen.categorico <- function(data, variable){
  color <- c("red","yellow","aqua","navy","teal","olive","purple","maroon",
             "black","blue","lime","orange","light-blue","green","fuchsia")
  datos.categoricos <- levels(data[, variable])
  res <- lapply(datos.categoricos, function(i) {
    tags$div(
      class='shiny-html-output col-sm-6 shiny-bound-output', id=paste0(variable, i),
      tags$div(
        class=paste0('small-box bg-', sample(color, 1)),
        tags$div(class='inner', tags$h3(summary(data[, variable])[i]), tags$p(i))
      )
    )
  })
  return(res)
}
