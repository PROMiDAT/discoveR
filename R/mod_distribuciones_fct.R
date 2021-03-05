########################### grDevices #########################################

boxplot.stats <- function (x, coef = 1.5, do.conf = TRUE, do.out = TRUE) 
{
  if (coef < 0) 
    stop("'coef' must not be negative")
  nna <- !is.na(x)
  n <- sum(nna)
  stats <- stats::fivenum(x, na.rm = TRUE)
  iqr <- diff(stats[c(2, 4)])
  if (coef == 0) 
    do.out <- FALSE
  else {
    out <- if (!is.na(iqr)) {
      x < (stats[2L] - coef * iqr) | x > (stats[4L] + coef * iqr)
    }
    else !is.finite(x)
    if (any(out[nna], na.rm = TRUE)) 
      stats[c(1, 5)] <- range(x[!out], na.rm = TRUE)
  }
  conf <- if (do.conf) 
    stats[3L] + c(-1.58, 1.58) * iqr/sqrt(n)
  list(stats = stats, n = n, conf = conf, 
       out = if (do.out) x[out & nna] else numeric())
}

########################### echarts4r #########################################

get_data <- function (e, serie, i = 1) {
  e$x$data[[i]][, 2] %>% unname()
}

get_outliers <- function (e, serie, i) {
  x <- get_data(e, serie, i)
  boxplot.stats(x)$out
}

build_boxplot <- function (e, serie, i) {
  x <- get_data(e, serie, i)
  boxplot.stats(x)$stats
}

name_it <- function (e, serie, name, i) {
  if (is.null(name)) {
    if (!is.null(names(e$x$data)[i])) {
      nm <- names(e$x$data)[i]
    } else {
      nm <- serie
    }
  } else {
    nm <- name
  }
  return(nm)
}

















