########################## modeest ###########################################

fisher.calc <- function (x, na.rm = FALSE, ...) {
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  if (na.rm)
    x <- x[!is.na(x)]
  nx <- length(x)
  
  sk <- sum((x - mean(x))^3/stats::sd(x)^3)/nx
  
  return(sk)
}

########################## nortest ###########################################

pearson.test <- function (x, n.classes = ceiling(2 * (n^(2/5))), adjust = TRUE) {
  x <- x[complete.cases(x)]
  n <- length(x)
  if (adjust) {
    dfd <- 2
  }
  else {
    dfd <- 0
  }
  num <- floor(1 + n.classes * pnorm(x, mean(x), sd(x)))
  count <- tabulate(num, n.classes)
  prob <- rep(1/n.classes, n.classes)
  xpec <- n * prob
  h <- ((count - xpec)^2)/xpec
  P <- sum(h)
  pvalue <- pchisq(P, n.classes - dfd - 1, lower.tail = FALSE)
  
  return(pvalue)
}

lillie.test <- function (x) {
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  if (n < 5) 
    stop("sample size must be greater than 4")
  p <- pnorm((x - mean(x))/sd(x))
  Dplus <- max(seq(1:n)/n - p)
  Dminus <- max(p - (seq(1:n) - 1)/n)
  K <- max(Dplus, Dminus)
  if (n <= 100) {
    Kd <- K
    nd <- n
  }
  else {
    Kd <- K * ((n/100)^0.49)
    nd <- 100
  }
  pvalue <- exp(-7.01256 * Kd^2 * (nd + 2.78019) + 2.99587 * 
                  Kd * sqrt(nd + 2.78019) - 0.122119 + 0.974598/sqrt(nd) + 
                  1.67997/nd)
  
  return(pvalue)
}

cvm.test <- function (x) {
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  if (n < 8) 
    stop("sample size must be greater than 7")
  p <- pnorm((x - mean(x))/sd(x))
  W <- (1/(12 * n) + sum((p - (2 * seq(1:n) - 1)/(2 * n))^2))
  WW <- (1 + 0.5/n) * W
  if (WW < 0.0275) {
    pval <- 1 - exp(-13.953 + 775.5 * WW - 12542.61 * WW^2)
  }
  else if (WW < 0.051) {
    pval <- 1 - exp(-5.903 + 179.546 * WW - 1515.29 * WW^2)
  }
  else if (WW < 0.092) {
    pval <- exp(0.886 - 31.62 * WW + 10.897 * WW^2)
  }
  else if (WW < 1.1) {
    pval <- exp(1.111 - 34.242 * WW + 12.832 * WW^2)
  }
  else {
    pval <- 7.37e-10
  }
  return(pval)
}