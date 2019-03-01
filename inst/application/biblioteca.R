
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

########################## reshape ###########################################

melt.data.frame <- function (data, id.vars, measure.vars, variable_name = "variable", 
          na.rm = !preserve.na, preserve.na = TRUE, ...) 
{
  if (!missing(preserve.na)) 
    message("Use of preserve.na is now deprecated, please use na.rm instead")
  var <- melt_check(data, id.vars, measure.vars)
  if (length(var$measure) == 0) {
    return(data[, var$id, drop = FALSE])
  }
  ids <- data[, var$id, drop = FALSE]
  df <- do.call("rbind", lapply(var$measure, function(x) {
    data.frame(ids, x, data[, x])
  }))
  names(df) <- c(names(ids), variable_name, "value")
  df[[variable_name]] <- factor(df[[variable_name]], unique(df[[variable_name]]))
  if (na.rm) {
    df <- df[!is.na(df$value), , drop = FALSE]
  }
  rownames(df) <- NULL
  df
}

melt.array <- function (data, varnames = names(dimnames(data)), ...) 
{
  values <- as.vector(data)
  dn <- dimnames(data)
  if (is.null(dn)) 
    dn <- vector("list", length(dim(data)))
  dn_missing <- sapply(dn, is.null)
  dn[dn_missing] <- lapply(dim(data), function(x) 1:x)[dn_missing]
  char <- sapply(dn, is.character)
  dn[char] <- lapply(dn[char], type.convert)
  indices <- do.call(expand.grid, dn)
  names(indices) <- varnames
  data.frame(indices, value = values)
}

melt_check <- function (data, id.vars, measure.vars) 
{
  varnames <- names(data)
  if (!missing(id.vars) && is.numeric(id.vars)) 
    id.vars <- varnames[id.vars]
  if (!missing(measure.vars) && is.numeric(measure.vars)) 
    measure.vars <- varnames[measure.vars]
  if (!missing(id.vars)) {
    unknown <- setdiff(id.vars, varnames)
    if (length(unknown) > 0) {
      stop("id variables not found in data: ", paste(unknown, 
                                                     collapse = ", "), call. = FALSE)
    }
  }
  if (!missing(measure.vars)) {
    unknown <- setdiff(measure.vars, varnames)
    if (length(unknown) > 0) {
      stop("measure variables not found in data: ", paste(unknown, 
                                                          collapse = ", "), call. = FALSE)
    }
  }
  if (missing(id.vars) && missing(measure.vars)) {
    categorical <- sapply(data, function(x) class(x)[1]) %in% 
      c("factor", "ordered", "character")
    id.vars <- varnames[categorical]
    measure.vars <- varnames[!categorical]
    message("Using ", paste(id.vars, collapse = ", "), " as id variables")
  }
  if (missing(id.vars)) 
    id.vars <- varnames[!(varnames %in% c(measure.vars))]
  if (missing(measure.vars)) 
    measure.vars <- varnames[!(varnames %in% c(id.vars))]
  list(id = id.vars, measure = measure.vars)
}

########################## scatterplot3d ###########################################

scatterplot3d <- function (x, y = NULL, z = NULL, color = par("col"), pch = par("pch"), 
          main = NULL, sub = NULL, xlim = NULL, ylim = NULL, zlim = NULL, 
          xlab = NULL, ylab = NULL, zlab = NULL, scale.y = 1, angle = 40, 
          axis = TRUE, tick.marks = TRUE, label.tick.marks = TRUE, 
          x.ticklabs = NULL, y.ticklabs = NULL, z.ticklabs = NULL, 
          y.margin.add = 0, grid = TRUE, box = TRUE, lab = par("lab"), 
          lab.z = mean(lab[1:2]), type = "p", highlight.3d = FALSE, 
          mar = c(5, 3, 4, 3) + 0.1, bg = par("bg"), col.axis = par("col.axis"), 
          col.grid = "grey", col.lab = par("col.lab"), cex.symbols = par("cex"), 
          cex.axis = 0.8 * par("cex.axis"), cex.lab = par("cex.lab"), 
          font.axis = par("font.axis"), font.lab = par("font.lab"), 
          lty.axis = par("lty"), lty.grid = par("lty"), lty.hide = NULL, 
          lty.hplot = par("lty"), log = "", asp = NA, ...) 
{
  mem.par <- par(mar = mar)
  x.scal <- y.scal <- z.scal <- 1
  xlabel <- if (!missing(x)) 
    deparse(substitute(x))
  ylabel <- if (!missing(y)) 
    deparse(substitute(y))
  zlabel <- if (!missing(z)) 
    deparse(substitute(z))
  if (highlight.3d && !missing(color)) 
    warning("color is ignored when highlight.3d = TRUE")
  if (!is.null(d <- dim(x)) && (length(d) == 2) && (d[2] >= 
                                                    4)) 
    color <- x[, 4]
  else if (is.list(x) && !is.null(x$color)) 
    color <- x$color
  xyz <- xyz.coords(x = x, y = y, z = z, xlab = xlabel, ylab = ylabel, 
                    zlab = zlabel, log = log)
  if (is.null(xlab)) {
    xlab <- xyz$xlab
    if (is.null(xlab)) 
      xlab <- ""
  }
  if (is.null(ylab)) {
    ylab <- xyz$ylab
    if (is.null(ylab)) 
      ylab <- ""
  }
  if (is.null(zlab)) {
    zlab <- xyz$zlab
    if (is.null(zlab)) 
      zlab <- ""
  }
  if (length(color) == 1) 
    color <- rep(color, length(xyz$x))
  else if (length(color) != length(xyz$x)) 
    stop("length(color) ", "must be equal length(x) or 1")
  if (length(pch) == 1) 
    pch <- rep(pch, length(xyz$x))
  else if (length(pch) != length(xyz$x)) 
    stop("length(pch) ", "must be equal length(x) or 1")
  if (length(bg) == 1) 
    bg <- rep(bg, length(xyz$x))
  else if (length(bg) != length(xyz$x)) 
    stop("length(bg) ", "must be equal length(x) or 1")
  angle <- (angle%%360)/90
  yz.f <- scale.y * abs(if (angle < 1) angle else if (angle > 
                                                      3) angle - 4 else 2 - angle)
  yx.f <- scale.y * (if (angle < 2) 
    1 - angle
    else angle - 3)
  if (angle > 2) {
    temp <- xyz$x
    xyz$x <- xyz$y
    xyz$y <- temp
    temp <- xlab
    xlab <- ylab
    ylab <- temp
    temp <- xlim
    xlim <- ylim
    ylim <- temp
  }
  angle.1 <- (1 < angle && angle <= 2) || angle > 3
  angle.2 <- 1 < angle && angle <= 3
  dat <- data.frame(xyz[c("x", "y", "z")], col = color, pch = pch, 
                    bg = bg, stringsAsFactors = FALSE)
  if (!is.null(xlim)) {
    xlim <- range(xlim)
    dat <- dat[xlim[1] <= dat$x & dat$x <= xlim[2], , drop = FALSE]
  }
  if (!is.null(ylim)) {
    ylim <- range(ylim)
    dat <- dat[ylim[1] <= dat$y & dat$y <= ylim[2], , drop = FALSE]
  }
  if (!is.null(zlim)) {
    zlim <- range(zlim)
    dat <- dat[zlim[1] <= dat$z & dat$z <= zlim[2], , drop = FALSE]
  }
  n <- nrow(dat)
  if (n < 1) 
    stop("no data left within (x|y|z)lim")
  y.range <- range(dat$y[is.finite(dat$y)])
  if (type == "p" || type == "h") {
    y.ord <- rev(order(dat$y))
    dat <- dat[y.ord, ]
    if (length(cex.symbols) > 1) 
      if (length(cex.symbols) != length(y.ord)) 
        stop("length(cex.symbols) ", "must be equal length(x) or 1")
    else cex.symbols <- cex.symbols[y.ord]
    daty <- dat$y
    daty[!is.finite(daty)] <- mean(daty[is.finite(daty)])
    if (highlight.3d && !(all(diff(daty) == 0))) 
      dat$col <- rgb(red = seq(0, 1, length = n) * (y.range[2] - 
                                                      daty)/diff(y.range), green = 0, blue = 0)
  }
  p.lab <- par("lab")
  y.range <- y.range.fix <- range(dat$y[is.finite(dat$y)], 
                                  ylim)
  y.prty <- pretty(y.range, n = lab[2], min.n = max(1, min(0.5 * 
                                                             lab[2], p.lab[2])))
  y.scal <- round(diff(y.prty[1:2]), digits = 12)
  y.add <- min(y.prty)
  dat$y <- (dat$y - y.add)/y.scal
  y.max <- (max(y.prty) - y.add)/y.scal
  if (!is.null(ylim)) 
    y.max <- max(y.max, ceiling((ylim[2] - y.add)/y.scal))
  x.range <- x.range.fix <- range(dat$x[is.finite(dat$x)], 
                                  xlim)
  x.prty <- pretty(x.range, n = lab[1], min.n = max(1, min(0.5 * 
                                                             lab[1], p.lab[1])))
  x.scal <- round(diff(x.prty[1:2]), digits = 12)
  dat$x <- dat$x/x.scal
  x.range <- range(x.prty)/x.scal
  x.max <- ceiling(x.range[2])
  x.min <- floor(x.range[1])
  if (!is.null(xlim)) {
    x.max <- max(x.max, ceiling(xlim[2]/x.scal))
    x.min <- min(x.min, floor(xlim[1]/x.scal))
  }
  x.range <- range(x.min, x.max)
  z.range <- range(dat$z[is.finite(dat$z)], zlim)
  z.prty <- pretty(z.range, n = lab.z, min.n = max(1, min(0.5 * 
                                                            lab.z, p.lab[2])))
  z.scal <- round(diff(z.prty[1:2]), digits = 12)
  dat$z <- dat$z/z.scal
  z.range <- range(z.prty)/z.scal
  z.max <- ceiling(z.range[2])
  z.min <- floor(z.range[1])
  if (!is.null(zlim)) {
    z.max <- max(z.max, ceiling(zlim[2]/z.scal))
    z.min <- min(z.min, floor(zlim[1]/z.scal))
  }
  z.range <- range(z.min, z.max)
  if (!is.na(asp)) {
    x.i <- x.min:x.max
    z.i <- z.min:z.max
    range.x <- abs(diff(range(x.i * x.scal)))
    range.z <- abs(diff(range(z.i * z.scal)))
    asp <- asp * (range.z/(length(z.i) - 1))/(range.x/(length(x.i) - 
                                                         1))
  }
  plot.new()
  if (angle.2) {
    x1 <- x.min + yx.f * y.max
    x2 <- x.max
  }
  else {
    x1 <- x.min
    x2 <- x.max + yx.f * y.max
  }
  plot.window(c(x1, x2), c(z.min, z.max + yz.f * y.max), asp = asp)
  temp <- strwidth(format(rev(y.prty))[1], cex = cex.axis/par("cex"))
  lheight <- (strheight("\n") - strheight("M")) * asp
  lheight2 <- (strheight("\n") - strheight("M"))
  if (angle.2) 
    x1 <- x1 - temp - y.margin.add
  else x2 <- x2 + temp + y.margin.add
  plot.window(c(x1, x2), c(z.min, z.max + yz.f * y.max), asp = asp)
  if (angle > 2) 
    par(usr = par("usr")[c(2, 1, 3:4)])
  usr <- par("usr")
  title(main, sub, ...)
  if (grid) {
    i <- x.min:x.max
    segments(i, z.min, i + (yx.f * y.max), yz.f * y.max + 
               z.min, col = col.grid, lty = lty.grid)
    i <- 0:y.max
    segments(x.min + (i * yx.f), i * yz.f + z.min, x.max + 
               (i * yx.f), i * yz.f + z.min, col = col.grid, lty = lty.grid)
  }
  if (axis) {
    xx <- if (angle.2) 
      c(x.min, x.max)
    else c(x.max, x.min)
    if (tick.marks) {
      xtl <- (z.max - z.min) * (tcl <- -par("tcl"))/50
      ztl <- (x.max - x.min) * tcl/50
      mysegs <- function(x0, y0, x1, y1) segments(x0, y0, 
                                                  x1, y1, col = col.axis, lty = lty.axis)
      i.y <- 0:y.max
      mysegs(yx.f * i.y - ztl + xx[1], yz.f * i.y + z.min, 
             yx.f * i.y + ztl + xx[1], yz.f * i.y + z.min)
      i.x <- x.min:x.max
      mysegs(i.x, -xtl + z.min, i.x, xtl + z.min)
      i.z <- z.min:z.max
      mysegs(-ztl + xx[2], i.z, ztl + xx[2], i.z)
      if (label.tick.marks) {
        las <- par("las")
        mytext <- function(labels, side, at, line = -0.5, 
                           ...) mtext(text = labels, side = side, at = at, 
                                      line = line, col = col.lab, cex = cex.axis, 
                                      font = font.lab, ...)
        if (is.null(x.ticklabs)) 
          x.ticklabs <- format(i.x * x.scal)
        if (!is.na(asp)) {
          linepad <- (usr[3] - z.min)/lheight2 + 0.5
          mytext(x.ticklabs, side = 1, at = i.x, line = linepad)
        }
        else {
          mytext(x.ticklabs, side = 1, at = i.x)
        }
        if (is.null(z.ticklabs)) 
          z.ticklabs <- format(i.z * z.scal)
        if (!is.na(asp)) {
          if (angle.1) {
            if (angle > 2) {
              linepad <- (x2 - usr[1])/lheight + 0.5
            }
            else {
              linepad <- (x2 - usr[2])/lheight + 0.5
            }
          }
          else {
            if (angle > 2) {
              linepad <- (usr[2] - x1)/lheight + 0.5
            }
            else {
              linepad <- (usr[1] - x1)/lheight + 0.5
            }
          }
        }
        else {
          linepad = -0.5
        }
        mytext(z.ticklabs, side = if (angle.1) 
          4
          else 2, at = i.z, adj = if (0 < las && las < 
                                      3) 
            1
          else NA, line = linepad)
        temp <- if (angle > 2) 
          rev(i.y)
        else i.y
        if (is.null(y.ticklabs)) 
          y.ticklabs <- format(y.prty)
        else if (angle > 2) 
          y.ticklabs <- rev(y.ticklabs)
        text(i.y * yx.f + xx[1], i.y * yz.f + z.min, 
             y.ticklabs, pos = if (angle.1) 
               2
             else 4, offset = 1, col = col.lab, cex = cex.axis/par("cex"), 
             font = font.lab)
      }
    }
    if (!is.na(asp)) {
      if (angle.1) {
        if (angle > 2) {
          linepad <- (x2 - usr[1])/lheight + 0.5
        }
        else {
          linepad <- (x2 - usr[2])/lheight + 0.5
        }
      }
      else {
        if (angle > 2) {
          linepad <- (usr[2] - x1)/lheight + 0.5
        }
        else {
          linepad <- (usr[1] - x1)/lheight + 0.5
        }
      }
    }
    else {
      linepad = -0.5
    }
    mytext2 <- function(lab, side, line, at) mtext(lab, side = side, 
                                                   line = line, at = at, col = col.lab, cex = cex.lab, 
                                                   font = font.axis, las = 0)
    lines(c(x.min, x.max), c(z.min, z.min), col = col.axis, 
          lty = lty.axis)
    if (!is.na(asp)) {
      mytext2(xlab, 1, line = (usr[3] - z.min)/lheight2 + 
                1.5, at = mean(x.range))
    }
    else {
      mytext2(xlab, 1, line = 1.5, at = mean(x.range))
    }
    lines(xx[1] + c(0, y.max * yx.f), c(z.min, y.max * yz.f + 
                                          z.min), col = col.axis, lty = lty.axis)
    mytext2(ylab, if (angle.1) 
      2
      else 4, line = linepad + 1, at = z.min + y.max * yz.f)
    lines(xx[c(2, 2)], c(z.min, z.max), col = col.axis, lty = lty.axis)
    mytext2(zlab, if (angle.1) 
      4
      else 2, line = linepad + 2, at = mean(z.range))
    if (box) {
      if (is.null(lty.hide)) 
        lty.hide <- lty.axis
      temp <- yx.f * y.max
      temp1 <- yz.f * y.max
      lines(c(x.min + temp, x.max + temp), c(z.min + temp1, 
                                             z.min + temp1), col = col.axis, lty = lty.hide)
      lines(c(x.min + temp, x.max + temp), c(temp1 + z.max, 
                                             temp1 + z.max), col = col.axis, lty = lty.axis)
      temp <- c(0, y.max * yx.f)
      temp1 <- c(0, y.max * yz.f)
      lines(temp + xx[2], temp1 + z.min, col = col.axis, 
            lty = lty.hide)
      lines(temp + x.min, temp1 + z.max, col = col.axis, 
            lty = lty.axis)
      temp <- yx.f * y.max
      temp1 <- yz.f * y.max
      lines(c(temp + x.min, temp + x.min), c(z.min + temp1, 
                                             z.max + temp1), col = col.axis, lty = if (!angle.2) 
                                               lty.hide
            else lty.axis)
      lines(c(x.max + temp, x.max + temp), c(z.min + temp1, 
                                             z.max + temp1), col = col.axis, lty = if (angle.2) 
                                               lty.hide
            else lty.axis)
    }
  }
  x <- dat$x + (dat$y * yx.f)
  z <- dat$z + (dat$y * yz.f)
  col <- as.character(dat$col)
  if (type == "h") {
    z2 <- dat$y * yz.f + z.min
    segments(x, z, x, z2, col = col, cex = cex.symbols, lty = lty.hplot, 
             ...)
    points(x, z, type = "p", col = col, pch = dat$pch, bg = dat$bg, 
           cex = cex.symbols, ...)
  }
  else points(x, z, type = type, col = col, pch = dat$pch, 
              bg = dat$bg, cex = cex.symbols, ...)
  if (axis && box) {
    lines(c(x.min, x.max), c(z.max, z.max), col = col.axis, 
          lty = lty.axis)
    lines(c(0, y.max * yx.f) + x.max, c(0, y.max * yz.f) + 
            z.max, col = col.axis, lty = lty.axis)
    lines(xx[c(1, 1)], c(z.min, z.max), col = col.axis, lty = lty.axis)
  }
  ob <- ls()
  rm(list = ob[!ob %in% c("angle", "mar", "usr", "x.scal", 
                          "y.scal", "z.scal", "yx.f", "yz.f", "y.add", "z.min", 
                          "z.max", "x.min", "x.max", "y.max", "x.range.fix", "y.range.fix", 
                          "xlabel", "ylabel", "zlabel", "x.prty", "y.prty", "z.prty", 
                          "mem.par")])
  rm(ob)
  invisible(list(xyz.convert = function(x, y = NULL, z = NULL) {
    xyz <- xyz.coords(x, y, z)
    if (angle > 2) {
      temp <- xyz$x
      xyz$x <- xyz$y
      xyz$y <- temp
    }
    y <- (xyz$y - y.add)/y.scal
    return(list(x = xyz$x/x.scal + yx.f * y, y = xyz$z/z.scal + 
                  yz.f * y))
  }, points3d = function(x, y = NULL, z = NULL, type = "p", 
                         ...) {
    xyz <- xyz.coords(x, y, z)
    if (angle > 2) {
      temp <- xyz$x
      xyz$x <- xyz$y
      xyz$y <- temp
    }
    y2 <- (xyz$y - y.add)/y.scal
    x <- xyz$x/x.scal + yx.f * y2
    y <- xyz$z/z.scal + yz.f * y2
    mem.par <- par(mar = mar, usr = usr)
    if (type == "h") {
      y2 <- z.min + yz.f * y2
      segments(x, y, x, y2, ...)
      points(x, y, type = "p", ...)
    } else points(x, y, type = type, ...)
  }, plane3d = function(Intercept, x.coef = NULL, y.coef = NULL, 
                        lty = "dashed", lty.box = NULL, draw_lines = TRUE, draw_polygon = FALSE, 
                        polygon_args = list(border = NA, col = rgb(0, 0, 0, 0.2)), 
                        ...) {
    if (!is.atomic(Intercept) && !is.null(coef(Intercept))) {
      Intercept <- coef(Intercept)
      if (!("(Intercept)" %in% names(Intercept))) Intercept <- c(0, 
                                                                 Intercept)
    }
    if (is.null(lty.box)) lty.box <- lty
    if (is.null(x.coef) && length(Intercept) == 3) {
      x.coef <- Intercept[if (angle > 2) 3 else 2]
      y.coef <- Intercept[if (angle > 2) 2 else 3]
      Intercept <- Intercept[1]
    }
    mem.par <- par(mar = mar, usr = usr)
    x <- x.min:x.max
    y <- 0:y.max
    ltya <- c(lty.box, rep(lty, length(x) - 2), lty.box)
    x.coef <- x.coef * x.scal
    z1 <- (Intercept + x * x.coef + y.add * y.coef)/z.scal
    z2 <- (Intercept + x * x.coef + (y.max * y.scal + y.add) * 
             y.coef)/z.scal
    if (draw_polygon) do.call("polygon", c(list(c(x.min, 
                                                  x.min + y.max * yx.f, x.max + y.max * yx.f, x.max), 
                                                c(z1[1], z2[1] + yz.f * y.max, z2[length(z2)] + yz.f * 
                                                    y.max, z1[length(z1)])), polygon_args))
    if (draw_lines) segments(x, z1, x + y.max * yx.f, z2 + 
                               yz.f * y.max, lty = ltya, ...)
    ltya <- c(lty.box, rep(lty, length(y) - 2), lty.box)
    y.coef <- (y * y.scal + y.add) * y.coef
    z1 <- (Intercept + x.min * x.coef + y.coef)/z.scal
    z2 <- (Intercept + x.max * x.coef + y.coef)/z.scal
    if (draw_lines) segments(x.min + y * yx.f, z1 + y * yz.f, 
                             x.max + y * yx.f, z2 + y * yz.f, lty = ltya, ...)
  }, box3d = function(...) {
    mem.par <- par(mar = mar, usr = usr)
    lines(c(x.min, x.max), c(z.max, z.max), ...)
    lines(c(0, y.max * yx.f) + x.max, c(0, y.max * yz.f) + 
            z.max, ...)
    lines(c(0, y.max * yx.f) + x.min, c(0, y.max * yz.f) + 
            z.max, ...)
    lines(c(x.max, x.max), c(z.min, z.max), ...)
    lines(c(x.min, x.min), c(z.min, z.max), ...)
    lines(c(x.min, x.max), c(z.min, z.min), ...)
  }, contour3d = function(f, x.count = 10, y.count = 10, type = "l", 
                          lty = "24", x.resolution = 50, y.resolution = 50, ...) {
    if (class(f) == "lm") {
      vars <- all.vars(formula(f))
    } else vars <- c("z", "x", "y")
    for (x1 in seq(x.range.fix[1], x.range.fix[2], length = x.count)) {
      d <- data.frame(x1, seq(y.range.fix[1], y.range.fix[2], 
                              length = y.resolution))
      names(d) <- vars[-1]
      if (class(f) == "lm") {
        d[vars[1]] <- predict(f, newdata = d)
      } else d[vars[1]] <- f(d[[1]], d[[2]])
      xyz <- xyz.coords(d)
      if (angle > 2) {
        temp <- xyz$x
        xyz$x <- xyz$y
        xyz$y <- temp
      }
      y2 <- (xyz$y - y.add)/y.scal
      x <- xyz$x/x.scal + yx.f * y2
      y <- xyz$z/z.scal + yz.f * y2
      mem.par <- par(mar = mar, usr = usr)
      if (type == "h") {
        y2 <- z.min + yz.f * y2
        segments(x, y, x, y2, ...)
        points(x, y, type = "p", ...)
      } else points(x, y, type = type, lty = lty, ...)
    }
    for (x2 in seq(y.range.fix[1], y.range.fix[2], length = y.count)) {
      d <- data.frame(seq(x.range.fix[1], x.range.fix[2], 
                          length = x.resolution), x2)
      names(d) <- vars[-1]
      if (class(f) == "lm") {
        d[vars[1]] <- predict(f, newdata = d)
      } else d[vars[1]] <- f(d[[1]], d[[2]])
      xyz <- xyz.coords(d)
      if (angle > 2) {
        temp <- xyz$x
        xyz$x <- xyz$y
        xyz$y <- temp
      }
      y2 <- (xyz$y - y.add)/y.scal
      x <- xyz$x/x.scal + yx.f * y2
      y <- xyz$z/z.scal + yz.f * y2
      mem.par <- par(mar = mar, usr = usr)
      if (type == "h") {
        y2 <- z.min + yz.f * y2
        segments(x, y, x, y2, ...)
        points(x, y, type = "p", ...)
      } else points(x, y, type = type, lty = lty, ...)
    }
  }, par.mar = mem.par))
}

########################## corrplot ###########################################

corrplot <- function (corr, method = c("circle", "square", "ellipse", "number", 
                                       "shade", "color", "pie"), type = c("full", "lower", "upper"), 
                      add = FALSE, col = NULL, bg = "white", title = "", is.corr = TRUE, 
                      diag = TRUE, outline = FALSE, mar = c(0, 0, 0, 0), addgrid.col = NULL, 
                      addCoef.col = NULL, addCoefasPercent = FALSE, order = c(
                        "original", "AOE", "FPC", "hclust", "alphabet"), 
                      hclust.method = c(
                        "complete", "ward", "ward.D", "ward.D2", "single", 
                        "average", "mcquitty", "median", "centroid"), 
                      addrect = NULL, rect.col = "black", 
                      rect.lwd = 2, tl.pos = NULL, tl.cex = 1, tl.col = "red", 
                      tl.offset = 0.4, tl.srt = 90, cl.pos = NULL, cl.lim = NULL, 
                      cl.length = NULL, cl.cex = 0.8, cl.ratio = 0.15, cl.align.text = "c", 
                      cl.offset = 0.5, number.cex = 1, number.font = 2, number.digits = NULL, 
                      addshade = c("negative", "positive", "all"), shade.lwd = 1, 
                      shade.col = "white", p.mat = NULL, sig.level = 0.05, insig = c(
                        "pch", "p-value", "blank", "n", "label_sig"), pch = 4, pch.col = "black", 
                      pch.cex = 3, plotCI = c("n", "square", "circle", "rect"), 
                      lowCI.mat = NULL, uppCI.mat = NULL, na.label = "?", na.label.col = "black", 
                      win.asp = 1, ...) 
{
  method <- match.arg(method)
  type <- match.arg(type)
  order <- match.arg(order)
  hclust.method <- match.arg(hclust.method)
  addshade <- match.arg(addshade)
  insig <- match.arg(insig)
  plotCI <- match.arg(plotCI)
  if (win.asp != 1 && !(method %in% c("circle", "square"))) {
    stop("Parameter 'win.asp' is supported only for circle and square methods.")
  }
  asp_rescale_factor <- min(1, win.asp)/max(1, win.asp)
  stopifnot(asp_rescale_factor >= 0 && asp_rescale_factor <= 
              1)
  if (!is.matrix(corr) && !is.data.frame(corr)) {
    stop("Need a matrix or data frame!")
  }
  if (is.null(addgrid.col)) {
    addgrid.col <- switch(method, color = NA, shade = NA, 
                          "grey")
  }
  if (any(corr < cl.lim[1]) || any(corr > cl.lim[2])) {
    stop("color limits should cover matrix")
  }
  if (is.null(cl.lim)) {
    if (is.corr) {
      cl.lim <- c(-1, 1)
    }
    else {
      corr_tmp <- corr
      diag(corr_tmp) <- ifelse(diag, diag(corr_tmp), NA)
      cl.lim <- c(min(corr_tmp, na.rm = TRUE), max(corr_tmp, 
                                                   na.rm = TRUE))
    }
  }
  intercept <- 0
  zoom <- 1
  if (!is.corr) {
    c_max <- max(corr, na.rm = TRUE)
    c_min <- min(corr, na.rm = TRUE)
    if (c_max <= 0) {
      intercept <- -cl.lim[2]
      zoom <- 1/(diff(cl.lim))
    }
    else if (c_min >= 0) {
      intercept <- -cl.lim[1]
      zoom <- 1/(diff(cl.lim))
    }
    else {
      stopifnot(c_max * c_min < 0)
      stopifnot(c_min < 0 && c_max > 0)
      intercept <- 0
      zoom <- 1/max(abs(cl.lim))
    }
    if (zoom == Inf) {
      stopifnot(cl.lim[1] == 0 && cl.lim[2] == 0)
      zoom <- 0
    }
    corr <- (intercept + corr) * zoom
  }
  cl.lim2 <- (intercept + cl.lim) * zoom
  int <- intercept * zoom
  if (is.corr) {
    if (min(corr, na.rm = TRUE) < -1 - .Machine$double.eps^0.75 || 
        max(corr, na.rm = TRUE) > 1 + .Machine$double.eps^0.75) {
      stop("The matrix is not in [-1, 1]!")
    }
  }
  if (is.null(col)) {
    col <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", 
                              "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
                              "#4393C3", "#2166AC", "#053061"))(200)
  }
  n <- nrow(corr)
  m <- ncol(corr)
  min.nm <- min(n, m)
  ord <- seq_len(min.nm)
  if (order != "original") {
    ord <- corrMatOrder(corr, order = order, hclust.method = hclust.method)
    corr <- corr[ord, ord]
  }
  if (is.null(rownames(corr))) {
    rownames(corr) <- seq_len(n)
  }
  if (is.null(colnames(corr))) {
    colnames(corr) <- seq_len(m)
  }
  apply_mat_filter <- function(mat) {
    x <- matrix(1:n * m, nrow = n, ncol = m)
    switch(type, upper = mat[row(x) > col(x)] <- Inf, lower = mat[row(x) < 
                                                                    col(x)] <- Inf)
    if (!diag) {
      diag(mat) <- Inf
    }
    return(mat)
  }
  getPos.Dat <- function(mat) {
    tmp <- apply_mat_filter(mat)
    Dat <- tmp[is.finite(tmp)]
    ind <- which(is.finite(tmp), arr.ind = TRUE)
    Pos <- ind
    Pos[, 1] <- ind[, 2]
    Pos[, 2] <- -ind[, 1] + 1 + n
    return(list(Pos, Dat))
  }
  getPos.NAs <- function(mat) {
    tmp <- apply_mat_filter(mat)
    ind <- which(is.na(tmp), arr.ind = TRUE)
    Pos <- ind
    Pos[, 1] <- ind[, 2]
    Pos[, 2] <- -ind[, 1] + 1 + n
    return(Pos)
  }
  Pos <- getPos.Dat(corr)[[1]]
  if (any(is.na(corr)) && is.character(na.label)) {
    PosNA <- getPos.NAs(corr)
  }
  else {
    PosNA <- NULL
  }
  AllCoords <- rbind(Pos, PosNA)
  n2 <- max(AllCoords[, 2])
  n1 <- min(AllCoords[, 2])
  nn <- n2 - n1
  m2 <- max(AllCoords[, 1])
  m1 <- min(AllCoords[, 1])
  mm <- max(1, m2 - m1)
  expand_expression <- function(s) {
    ifelse(grepl("^[:=$]", s), parse(text = substring(s, 
                                                      2)), s)
  }
  newrownames <- sapply(rownames(corr)[(n + 1 - n2):(n + 1 - 
                                                       n1)], expand_expression)
  newcolnames <- sapply(colnames(corr)[m1:m2], expand_expression)
  DAT <- getPos.Dat(corr)[[2]]
  len.DAT <- length(DAT)
  rm(expand_expression)
  assign.color <- function(dat = DAT, color = col) {
    newcorr <- (dat + 1)/2
    newcorr[newcorr <= 0] <- 0
    newcorr[newcorr >= 1] <- 1 - 1e-16
    color[floor(newcorr * length(color)) + 1]
  }
  col.fill <- assign.color()
  isFALSE <- function(x) identical(x, FALSE)
  isTRUE <- function(x) identical(x, TRUE)
  if (isFALSE(tl.pos)) {
    tl.pos <- "n"
  }
  if (is.null(tl.pos) || isTRUE(tl.pos)) {
    tl.pos <- switch(type, full = "lt", lower = "ld", upper = "td")
  }
  if (isFALSE(cl.pos)) {
    cl.pos <- "n"
  }
  if (is.null(cl.pos) || isTRUE(cl.pos)) {
    cl.pos <- switch(type, full = "r", lower = "b", upper = "r")
  }
  if (isFALSE(outline)) {
    col.border <- col.fill
  }
  else if (isTRUE(outline)) {
    col.border <- "black"
  }
  else if (is.character(outline)) {
    col.border <- outline
  }
  else {
    stop("Unsupported value type for parameter outline")
  }
  oldpar <- par(mar = mar, bg = "white")
  on.exit(par(oldpar), add = TRUE)
  if (!add) {
    plot.new()
    xlabwidth <- max(strwidth(newrownames, cex = tl.cex))
    ylabwidth <- max(strwidth(newcolnames, cex = tl.cex))
    laboffset <- strwidth("W", cex = tl.cex) * tl.offset
    for (i in 1:50) {
      xlim <- c(m1 - 0.5 - laboffset - xlabwidth * (grepl("l", 
                                                          tl.pos) | grepl("d", tl.pos)), m2 + 0.5 + mm * 
                  cl.ratio * (cl.pos == "r") + xlabwidth * abs(cos(tl.srt * 
                                                                     pi/180)) * grepl("d", tl.pos)) + c(-0.35, 0.15) + 
        c(-1, 0) * grepl("l", tl.pos)
      ylim <- c(n1 - 0.5 - nn * cl.ratio * (cl.pos == "b") - 
                  laboffset, n2 + 0.5 + laboffset + ylabwidth * 
                  abs(sin(tl.srt * pi/180)) * grepl("t", tl.pos)) + 
        c(-0.15, 0) + c(0, -1) * (type == "upper" && 
                                    tl.pos != "n") + c(0, 1) * grepl("d", tl.pos)
      plot.window(xlim, ylim, asp = 1, xaxs = "i", yaxs = "i")
      x.tmp <- max(strwidth(newrownames, cex = tl.cex))
      y.tmp <- max(strwidth(newcolnames, cex = tl.cex))
      laboffset.tmp <- strwidth("W", cex = tl.cex) * tl.offset
      if (max(x.tmp - xlabwidth, y.tmp - ylabwidth, laboffset.tmp - 
              laboffset) < 0.001) {
        break
      }
      xlabwidth <- x.tmp
      ylabwidth <- y.tmp
      laboffset <- laboffset.tmp
      if (i == 50) {
        warning(c("Not been able to calculate text margin, ", 
                  "please try again with a clean new empty window using ", 
                  "{plot.new(); dev.off()} or reduce tl.cex"))
      }
    }
    if (.Platform$OS.type == "windows") {
      grDevices::windows.options(width = 7, height = 7 * 
                                   diff(ylim)/diff(xlim))
    }
    plot.window(xlim = xlim, ylim = ylim, asp = win.asp, 
                xlab = "", ylab = "", xaxs = "i", yaxs = "i")
  }
  laboffset <- strwidth("W", cex = tl.cex) * tl.offset
  symbols(Pos, add = TRUE, inches = FALSE, rectangles = matrix(1, 
                                                               len.DAT, 2), bg = bg, fg = bg)
  if (method == "circle" && plotCI == "n") {
    symbols(Pos, add = TRUE, inches = FALSE, circles = asp_rescale_factor * 
              0.9 * abs(DAT)^0.5/2, fg = col.border, bg = col.fill)
  }
  if (method == "ellipse" && plotCI == "n") {
    ell.dat <- function(rho, length = 99) {
      k <- seq(0, 2 * pi, length = length)
      x <- cos(k + acos(rho)/2)/2
      y <- cos(k - acos(rho)/2)/2
      cbind(rbind(x, y), c(NA, NA))
    }
    ELL.dat <- lapply(DAT, ell.dat)
    ELL.dat2 <- 0.85 * matrix(unlist(ELL.dat), ncol = 2, 
                              byrow = TRUE)
    ELL.dat2 <- ELL.dat2 + Pos[rep(1:length(DAT), each = 100), 
                               ]
    polygon(ELL.dat2, border = col.border, col = col.fill)
  }
  if (is.null(number.digits)) {
    number.digits <- switch(addCoefasPercent + 1, 2, 0)
  }
  stopifnot(number.digits%%1 == 0)
  stopifnot(number.digits >= 0)
  if (method == "number" && plotCI == "n") {
    text(Pos[, 1], Pos[, 2], font = number.font, col = col.fill, 
         labels = round((DAT - int) * ifelse(addCoefasPercent, 
                                             100, 1)/zoom, number.digits), cex = number.cex)
  }
  NA_LABEL_MAX_CHARS <- 2
  if (is.matrix(PosNA) && nrow(PosNA) > 0) {
    stopifnot(is.matrix(PosNA))
    if (na.label == "square") {
      symbols(PosNA, add = TRUE, inches = FALSE, squares = rep(1, 
                                                               nrow(PosNA)), bg = na.label.col, fg = na.label.col)
    }
    else if (nchar(na.label) %in% 1:NA_LABEL_MAX_CHARS) {
      symbols(PosNA, add = TRUE, inches = FALSE, squares = rep(1, 
                                                               nrow(PosNA)), fg = bg, bg = bg)
      text(PosNA[, 1], PosNA[, 2], font = number.font, 
           col = na.label.col, labels = na.label, cex = number.cex, 
           ...)
    }
    else {
      stop(paste("Maximum number of characters for NA label is:", 
                 NA_LABEL_MAX_CHARS))
    }
  }
  if (method == "pie" && plotCI == "n") {
    symbols(Pos, add = TRUE, inches = FALSE, circles = rep(0.5, 
                                                           len.DAT) * 0.85, fg = col.border)
    pie.dat <- function(theta, length = 100) {
      k <- seq(pi/2, pi/2 - theta, length = 0.5 * length * 
                 abs(theta)/pi)
      x <- c(0, cos(k)/2, 0)
      y <- c(0, sin(k)/2, 0)
      cbind(rbind(x, y), c(NA, NA))
    }
    PIE.dat <- lapply(DAT * 2 * pi, pie.dat)
    len.pie <- unlist(lapply(PIE.dat, length))/2
    PIE.dat2 <- 0.85 * matrix(unlist(PIE.dat), ncol = 2, 
                              byrow = TRUE)
    PIE.dat2 <- PIE.dat2 + Pos[rep(1:length(DAT), len.pie), 
                               ]
    polygon(PIE.dat2, border = "black", col = col.fill)
  }
  if (method == "shade" && plotCI == "n") {
    symbols(Pos, add = TRUE, inches = FALSE, squares = rep(1, 
                                                           len.DAT), bg = col.fill, fg = addgrid.col)
    shade.dat <- function(w) {
      x <- w[1]
      y <- w[2]
      rho <- w[3]
      x1 <- x - 0.5
      x2 <- x + 0.5
      y1 <- y - 0.5
      y2 <- y + 0.5
      dat <- NA
      if ((addshade == "positive" || addshade == "all") && 
          rho > 0) {
        dat <- cbind(c(x1, x1, x), c(y, y1, y1), c(x, 
                                                   x2, x2), c(y2, y2, y))
      }
      if ((addshade == "negative" || addshade == "all") && 
          rho < 0) {
        dat <- cbind(c(x1, x1, x), c(y, y2, y2), c(x, 
                                                   x2, x2), c(y1, y1, y))
      }
      return(t(dat))
    }
    pos_corr <- rbind(cbind(Pos, DAT))
    pos_corr2 <- split(pos_corr, 1:nrow(pos_corr))
    SHADE.dat <- matrix(na.omit(unlist(lapply(pos_corr2, 
                                              shade.dat))), byrow = TRUE, ncol = 4)
    segments(SHADE.dat[, 1], SHADE.dat[, 2], SHADE.dat[, 
                                                       3], SHADE.dat[, 4], col = shade.col, lwd = shade.lwd)
  }
  if (method == "square" && plotCI == "n") {
    draw_method_square(Pos, DAT, asp_rescale_factor, col.border, 
                       col.fill)
  }
  if (method == "color" && plotCI == "n") {
    draw_method_color(Pos, col.border, col.fill)
  }
  draw_grid(AllCoords, addgrid.col)
  if (plotCI != "n") {
    if (is.null(lowCI.mat) || is.null(uppCI.mat)) {
      stop("Need lowCI.mat and uppCI.mat!")
    }
    if (order != "original") {
      lowCI.mat <- lowCI.mat[ord, ord]
      uppCI.mat <- uppCI.mat[ord, ord]
    }
    pos.lowNew <- getPos.Dat(lowCI.mat)[[1]]
    lowNew <- getPos.Dat(lowCI.mat)[[2]]
    pos.uppNew <- getPos.Dat(uppCI.mat)[[1]]
    uppNew <- getPos.Dat(uppCI.mat)[[2]]
    if (!method %in% c("circle", "square")) {
      stop("Method shoud be circle or square if drawing confidence intervals.")
    }
    k1 <- (abs(uppNew) > abs(lowNew))
    bigabs <- uppNew
    bigabs[which(!k1)] <- lowNew[!k1]
    smallabs <- lowNew
    smallabs[which(!k1)] <- uppNew[!k1]
    sig <- sign(uppNew * lowNew)
    color_bigabs <- col[ceiling((bigabs + 1) * length(col)/2)]
    color_smallabs <- col[ceiling((smallabs + 1) * length(col)/2)]
    if (plotCI == "circle") {
      symbols(pos.uppNew[, 1], pos.uppNew[, 2], add = TRUE, 
              inches = FALSE, circles = 0.95 * abs(bigabs)^0.5/2, 
              bg = ifelse(sig > 0, col.fill, color_bigabs), 
              fg = ifelse(sig > 0, col.fill, color_bigabs))
      symbols(pos.lowNew[, 1], pos.lowNew[, 2], add = TRUE, 
              inches = FALSE, circles = 0.95 * abs(smallabs)^0.5/2, 
              bg = ifelse(sig > 0, bg, color_smallabs), fg = ifelse(sig > 
                                                                      0, col.fill, color_smallabs))
    }
    if (plotCI == "square") {
      symbols(pos.uppNew[, 1], pos.uppNew[, 2], add = TRUE, 
              inches = FALSE, squares = abs(bigabs)^0.5, bg = ifelse(
                sig > 0, col.fill, color_bigabs), fg = ifelse(sig > 0, col.fill, color_bigabs))
      symbols(pos.lowNew[, 1], pos.lowNew[, 2], add = TRUE, 
              inches = FALSE, squares = abs(smallabs)^0.5, 
              bg = ifelse(sig > 0, bg, color_smallabs), fg = ifelse(sig > 
                                                                      0, col.fill, color_smallabs))
    }
    if (plotCI == "rect") {
      rect.width <- 0.25
      rect(pos.uppNew[, 1] - rect.width, pos.uppNew[, 2] + 
             smallabs/2, pos.uppNew[, 1] + rect.width, pos.uppNew[, 
                                                                  2] + bigabs/2, col = col.fill, border = col.fill)
      segments(pos.lowNew[, 1] - rect.width, pos.lowNew[, 2] + 
                 DAT/2, pos.lowNew[, 1] + rect.width, pos.lowNew[, 2] + DAT/2, col = "black", lwd = 1)
      segments(pos.uppNew[, 1] - rect.width, pos.uppNew[, 
                                                        2] + uppNew/2, pos.uppNew[, 1] + rect.width, 
               pos.uppNew[, 2] + uppNew/2, col = "black", lwd = 1)
      segments(pos.lowNew[, 1] - rect.width, pos.lowNew[, 
                                                        2] + lowNew/2, pos.lowNew[, 1] + rect.width, 
               pos.lowNew[, 2] + lowNew/2, col = "black", lwd = 1)
      segments(pos.lowNew[, 1] - 0.5, pos.lowNew[, 2], 
               pos.lowNew[, 1] + 0.5, pos.lowNew[, 2], col = "grey70", 
               lty = 3)
    }
  }
  if (!is.null(p.mat) && insig != "n") {
    if (order != "original") {
      p.mat <- p.mat[ord, ord]
    }
    pos.pNew <- getPos.Dat(p.mat)[[1]]
    pNew <- getPos.Dat(p.mat)[[2]]
    if (insig == "label_sig") {
      if (!is.character(pch)) 
        pch <- "*"
      place_points <- function(sig.locs, point) {
        text(pos.pNew[, 1][sig.locs], pos.pNew[, 2][sig.locs], 
             labels = point, col = pch.col, cex = pch.cex, 
             lwd = 2)
      }
      if (length(sig.level) == 1) {
        place_points(sig.locs = which(pNew < sig.level), 
                     point = pch)
      }
      else {
        l <- length(sig.level)
        for (i in seq_along(sig.level)) {
          iter <- l + 1 - i
          pchTmp <- paste(rep(pch, i), collapse = "")
          if (i == length(sig.level)) {
            locs <- which(pNew < sig.level[iter])
            if (length(locs)) {
              place_points(sig.locs = locs, point = pchTmp)
            }
          }
          else {
            locs <- which(pNew < sig.level[iter] & pNew > 
                            sig.level[iter - 1])
            if (length(locs)) {
              place_points(sig.locs = locs, point = pchTmp)
            }
          }
        }
      }
    }
    else {
      ind.p <- which(pNew > sig.level)
      p_inSig <- length(ind.p) > 0
      if (insig == "pch" && p_inSig) {
        points(pos.pNew[, 1][ind.p], pos.pNew[, 2][ind.p], 
               pch = pch, col = pch.col, cex = pch.cex, lwd = 2)
      }
      if (insig == "p-value" && p_inSig) {
        text(pos.pNew[, 1][ind.p], pos.pNew[, 2][ind.p], 
             round(pNew[ind.p], 2), col = pch.col)
      }
      if (insig == "blank" && p_inSig) {
        symbols(pos.pNew[, 1][ind.p], pos.pNew[, 2][ind.p], 
                inches = FALSE, squares = rep(1, length(pos.pNew[, 
                                                                 1][ind.p])), fg = addgrid.col, bg = bg, add = TRUE)
      }
    }
  }
  if (cl.pos != "n") {
    colRange <- assign.color(dat = cl.lim2)
    ind1 <- which(col == colRange[1])
    ind2 <- which(col == colRange[2])
    colbar <- col[ind1:ind2]
    if (is.null(cl.length)) {
      cl.length <- ifelse(length(colbar) > 20, 11, length(colbar) + 
                            1)
    }
    labels <- seq(cl.lim[1], cl.lim[2], length = cl.length)
    if (cl.pos == "r") {
      vertical <- TRUE
      xlim <- c(m2 + 0.5 + mm * 0.02, m2 + 0.5 + mm * cl.ratio)
      ylim <- c(n1 - 0.5, n2 + 0.5)
    }
    if (cl.pos == "b") {
      vertical <- FALSE
      xlim <- c(m1 - 0.5, m2 + 0.5)
      ylim <- c(n1 - 0.5 - nn * cl.ratio, n1 - 0.5 - nn * 
                  0.02)
    }
    colorlegend(colbar = colbar, labels = round(labels, 2), 
                offset = cl.offset, ratio.colbar = 0.3, cex = cl.cex, 
                xlim = xlim, ylim = ylim, vertical = vertical, align = cl.align.text)
  }
  if (tl.pos != "n") {
    pos.xlabel <- cbind(m1:m2, n2 + 0.5 + laboffset)
    pos.ylabel <- cbind(m1 - 0.5, n2:n1)
    if (tl.pos == "td") {
      if (type != "upper") {
        stop("type should be \"upper\" if tl.pos is \"dt\".")
      }
      pos.ylabel <- cbind(m1:(m1 + nn) - 0.5, n2:n1)
    }
    if (tl.pos == "ld") {
      if (type != "lower") {
        stop("type should be \"lower\" if tl.pos is \"ld\".")
      }
      pos.xlabel <- cbind(m1:m2, n2:(n2 - mm) + 0.5 + laboffset)
    }
    if (tl.pos == "d") {
      pos.ylabel <- cbind(m1:(m1 + nn) - 0.5, n2:n1)
      pos.ylabel <- pos.ylabel[1:min(n, m), ]
      symbols(pos.ylabel[, 1] + 0.5, pos.ylabel[, 2], add = TRUE, 
              bg = bg, fg = addgrid.col, inches = FALSE, squares = rep(1, 
                                                                       length(pos.ylabel[, 1])))
      text(pos.ylabel[, 1] + 0.5, pos.ylabel[, 2], newcolnames[1:min(n, 
                                                                     m)], col = tl.col, cex = tl.cex, ...)
    }
    else {
      text(pos.xlabel[, 1], pos.xlabel[, 2], newcolnames, 
           srt = tl.srt, adj = ifelse(tl.srt == 0, c(0.5, 
                                                     0), c(0, 0)), col = tl.col, cex = tl.cex, offset = tl.offset, 
           ...)
      text(pos.ylabel[, 1], pos.ylabel[, 2], newrownames, 
           col = tl.col, cex = tl.cex, pos = 2, offset = tl.offset, 
           ...)
    }
  }
  title(title, ...)
  if (!is.null(addCoef.col) && method != "number") {
    text(Pos[, 1], Pos[, 2], col = addCoef.col, labels = round((DAT - 
                                                                  int) * ifelse(addCoefasPercent, 100, 1)/zoom, number.digits), 
         cex = number.cex, font = number.font)
  }
  if (type == "full" && plotCI == "n" && !is.null(addgrid.col)) {
    rect(m1 - 0.5, n1 - 0.5, m2 + 0.5, n2 + 0.5, border = addgrid.col)
  }
  if (!is.null(addrect) && order == "hclust" && type == "full") {
    corrRect.hclust(corr, k = addrect, method = hclust.method, 
                    col = rect.col, lwd = rect.lwd)
  }
  invisible(corr)
}

draw_grid <- function (coords, fg) 
{
  symbols(coords, add = TRUE, inches = FALSE, fg = fg, bg = NA, 
          rectangles = matrix(1, nrow = nrow(coords), ncol = 2))
}

colorlegend <- function (colbar, labels, at = NULL, xlim = c(0, 1), ylim = c(0, 
                                                              1), vertical = TRUE, ratio.colbar = 0.4, lim.segment = "auto", 
          align = c("c", "l", "r"), addlabels = TRUE, ...) 
{
  if (is.null(at) && addlabels) {
    at <- seq(0L, 1L, length = length(labels))
  }
  if (is.null(lim.segment) || lim.segment == "auto") {
    lim.segment <- ratio.colbar + c(0, ratio.colbar * 0.2)
  }
  if (any(at < 0L) || any(at > 1L)) {
    stop("at should be between 0 and 1")
  }
  if (length(lim.segment) != 2) {
    stop("lim.segment should be a vector of length 2")
  }
  if (any(lim.segment < 0L) || any(lim.segment > 1L)) {
    stop("lim.segment should be between 0 and 1")
  }
  align <- match.arg(align)
  xgap <- diff(xlim)
  ygap <- diff(ylim)
  len <- length(colbar)
  rat1 <- ratio.colbar
  rat2 <- lim.segment
  if (vertical) {
    at <- at * ygap + ylim[1]
    yyy <- seq(ylim[1], ylim[2], length = len + 1)
    rect(rep(xlim[1], len), yyy[1:len], rep(xlim[1] + xgap * 
                                              rat1, len), yyy[-1], col = colbar, border = colbar)
    rect(xlim[1], ylim[1], xlim[1] + xgap * rat1, ylim[2], 
         border = "black")
    segments(xlim[1] + xgap * rat2[1], at, xlim[1] + xgap * 
               rat2[2], at)
    if (addlabels) {
      pos.xlabel <- rep(xlim[1] + xgap * max(rat2, rat1), 
                        length(at))
      switch(align, l = text(
        pos.xlabel, y = at, labels = labels, pos = 4, ...), 
        r = text(xlim[2], y = at, labels = labels, pos = 2, ...),
        c = text((pos.xlabel + xlim[2])/2, y = at, labels = labels, ...), 
        stop("programming error - should not have reached this line!"))
    }
  }
  else {
    at <- at * xgap + xlim[1]
    xxx <- seq(xlim[1], xlim[2], length = len + 1)
    rect(xxx[1:len], rep(ylim[2] - rat1 * ygap, len), xxx[-1], 
         rep(ylim[2], len), col = colbar, border = colbar)
    rect(xlim[1], ylim[2] - rat1 * ygap, xlim[2], ylim[2], 
         border = "black")
    segments(at, ylim[2] - ygap * rat2[1], at, ylim[2] - 
               ygap * rat2[2])
    if (addlabels) {
      pos.ylabel <- rep(ylim[2] - ygap * max(rat2, rat1), 
                        length(at))
      switch(align, l = text(
        x = at, y = pos.ylabel, labels = labels, pos = 1, ...), 
        r = text(x = at, y = ylim[1], labels = labels, pos = 2, ...), 
        c = text(x = at, y = (pos.ylabel + ylim[1])/2, labels = labels, ...), 
        stop("programming error - should not have reached this line!"))
    }
  }
}

corrMatOrder <- function (corr, order = c("AOE", "FPC", "hclust", "alphabet"), 
          hclust.method = c("complete", "ward", "ward.D", "ward.D2", 
                            "single", "average", "mcquitty", "median", "centroid")) 
{
  order <- match.arg(order)
  hclust.method <- match.arg(hclust.method)
  switch(order, AOE = reorder_using_aoe(corr), FPC = reorder_using_fpc(corr), 
         hclust = reorder_using_hclust(corr, hclust.method), alphabet = sort(rownames(corr)))
}

reorder_using_aoe <- function (corr) 
{
  x.eigen <- eigen(corr)$vectors[, 1:2]
  e1 <- x.eigen[, 1]
  e2 <- x.eigen[, 2]
  alpha <- ifelse(e1 > 0, atan(e2/e1), atan(e2/e1) + pi)
  order(alpha)
}

draw_method_color <- function (coords, fg, bg) 
{
  symbols(coords, squares = rep(1, nrow(coords)), fg = fg, 
          bg = bg, add = TRUE, inches = FALSE)
}

draw_method_square <- function (coords, values, asp_rescale_factor, fg, bg) 
{
  symbols(coords, add = TRUE, inches = FALSE, squares = asp_rescale_factor * 
            abs(values)^0.5, bg = bg, fg = fg)
}

########################## FactoMineR #########################################

svd.triplet <- function (X, row.w = NULL, col.w = NULL, ncp = Inf) 
{
  tryCatch.W.E <- function(expr) {
    W <- NULL
    w.handler <- function(w) {
      W <<- w
      invokeRestart("muffleWarning")
    }
    list(value = withCallingHandlers(tryCatch(expr, error = function(e) e), 
                                     warning = w.handler), warning = W)
  }
  if (is.null(row.w)) 
    row.w <- rep(1/nrow(X), nrow(X))
  if (is.null(col.w)) 
    col.w <- rep(1, ncol(X))
  ncp <- min(ncp, nrow(X) - 1, ncol(X))
  row.w <- row.w/sum(row.w)
  X <- t(t(X) * sqrt(col.w)) * sqrt(row.w)
  if (ncol(X) < nrow(X)) {
    svd.usuelle <- tryCatch.W.E(svd(X, nu = ncp, nv = ncp))$val
    if (names(svd.usuelle)[[1]] == "message") {
      svd.usuelle <- tryCatch.W.E(svd(t(X), nu = ncp, nv = ncp))$val
      if (names(svd.usuelle)[[1]] == "d") {
        aux <- svd.usuelle$u
        svd.usuelle$u <- svd.usuelle$v
        svd.usuelle$v <- aux
      }
      else {
        bb <- eigen(crossprod(X, X), symmetric = TRUE)
        svd.usuelle <- vector(mode = "list", length = 3)
        svd.usuelle$d[svd.usuelle$d < 0] = 0
        svd.usuelle$d <- sqrt(svd.usuelle$d)
        svd.usuelle$v <- bb$vec[, 1:ncp]
        svd.usuelle$u <- t(t(crossprod(t(X), svd.usuelle$v))/svd.usuelle$d[1:ncp])
      }
    }
    U <- svd.usuelle$u
    V <- svd.usuelle$v
    if (ncp > 1) {
      mult <- sign(as.vector(crossprod(rep(1, nrow(V)), 
                                       as.matrix(V))))
      mult[mult == 0] <- 1
      U <- t(t(U) * mult)
      V <- t(t(V) * mult)
    }
    U <- U/sqrt(row.w)
    V <- V/sqrt(col.w)
  }
  else {
    svd.usuelle <- tryCatch.W.E(svd(t(X), nu = ncp, nv = ncp))$val
    if (names(svd.usuelle)[[1]] == "message") {
      svd.usuelle <- tryCatch.W.E(svd(X, nu = ncp, nv = ncp))$val
      if (names(svd.usuelle)[[1]] == "d") {
        aux <- svd.usuelle$u
        svd.usuelle$u <- svd.usuelle$v
        svd.usuelle$v <- aux
      }
      else {
        bb <- eigen(crossprod(t(X), t(X)), symmetric = TRUE)
        svd.usuelle <- vector(mode = "list", length = 3)
        svd.usuelle$d[svd.usuelle$d < 0] = 0
        svd.usuelle$d <- sqrt(svd.usuelle$d)
        svd.usuelle$v <- bb$vec[, 1:ncp]
        svd.usuelle$u <- t(t(crossprod(X, svd.usuelle$v))/svd.usuelle$d[1:ncp])
      }
    }
    U <- svd.usuelle$v
    V <- svd.usuelle$u
    mult <- sign(as.vector(crossprod(rep(1, nrow(V)), as.matrix(V))))
    mult[mult == 0] <- 1
    V <- t(t(V) * mult)/sqrt(col.w)
    U <- t(t(U) * mult)/sqrt(row.w)
  }
  vs <- svd.usuelle$d[1:min(ncol(X), nrow(X) - 1)]
  num <- which(vs[1:ncp] < 1e-15)
  if (length(num) == 1) {
    U[, num] <- U[, num, drop = FALSE] * vs[num]
    V[, num] <- V[, num, drop = FALSE] * vs[num]
  }
  if (length(num) > 1) {
    U[, num] <- t(t(U[, num]) * vs[num])
    V[, num] <- t(t(V[, num]) * vs[num])
  }
  res <- list(vs = vs, U = U, V = V)
  return(res)
}

PCA <- function (X, scale.unit = TRUE, ncp = 5, ind.sup = NULL, quanti.sup = NULL, 
                 quali.sup = NULL, row.w = NULL, col.w = NULL, graph = TRUE, 
                 axes = c(1, 2)) 
{
  moy.ptab <- function(V, poids) {
    as.vector(crossprod(poids/sum(poids), as.matrix(V)))
  }
  ec.tab <- function(V, poids) {
    ecart.type <- sqrt(as.vector(crossprod(poids/sum(poids), 
                                           as.matrix(V^2))))
    ecart.type[ecart.type <= 1e-16] <- 1
    return(ecart.type)
  }
  fct.eta2 <- function(vec, x, weights) {
    VB <- function(xx) {
      return(sum((colSums((tt * xx) * weights)^2)/ni))
    }
    tt <- tab.disjonctif(vec)
    ni <- colSums(tt * weights)
    unlist(lapply(as.data.frame(x), VB))/colSums(x * x * 
                                                   weights)
  }
  X <- as.data.frame(X)
  is.quali <- which(!unlist(lapply(X, is.numeric)))
  X[, is.quali] <- lapply(X[, is.quali, drop = FALSE], as.factor)
  X <- droplevels(X)
  if (any(is.na(X))) {
    warning("Missing values are imputed by the mean of the variable: you should use the imputePCA function of the missMDA package")
    if (is.null(quali.sup)) 
      X[is.na(X)] = matrix(colMeans(X, na.rm = TRUE), ncol = ncol(X), 
                           nrow = nrow(X), byrow = TRUE)[is.na(X)]
    else for (j in (1:ncol(X))[-quali.sup]) X[, j] <- replace(X[, 
                                                                j], is.na(X[, j]), mean(X[, j], na.rm = TRUE))
  }
  Xtot <- X
  if (!is.null(quali.sup)) 
    X <- X[, -quali.sup, drop = FALSE]
  auxi <- colnames(X)[!sapply(X, is.numeric)]
  if (length(auxi) > 0) 
    stop(paste("\nThe following variables are not quantitative: ", 
               auxi))
  todelete <- c(quali.sup, quanti.sup)
  if (!is.null(todelete)) 
    X <- Xtot[, -todelete, drop = FALSE]
  if (!is.null(ind.sup)) {
    X.ind.sup <- X[ind.sup, , drop = FALSE]
    X <- X[-ind.sup, , drop = FALSE]
  }
  ncp <- min(ncp, nrow(X) - 1, ncol(X))
  if (is.null(row.w)) 
    row.w <- rep(1, nrow(X))
  row.w.init <- row.w
  row.w <- row.w/sum(row.w)
  if (is.null(col.w)) 
    col.w <- rep(1, ncol(X))
  centre <- moy.ptab(X, row.w)
  data <- X
  X <- t(t(as.matrix(X)) - centre)
  if (is.null(attributes(X)$row.names)) 
    rownames(X) <- rownames(data)
  if (is.null(attributes(X)$names)) 
    colnames(X) <- colnames(data)
  if (scale.unit) {
    ecart.type <- ec.tab(X, row.w)
    X <- t(t(X)/ecart.type)
  }
  else ecart.type <- rep(1, length(centre))
  dist2.ind <- rowSums(t(t(X^2) * col.w))
  dist2.var <- as.vector(crossprod(rep(1, nrow(X)), as.matrix(X^2 * 
                                                                row.w)))
  res.call <- list(row.w = (row.w/sum(row.w)), col.w = col.w, 
                   scale.unit = scale.unit, ncp = ncp, centre = centre, 
                   ecart.type = ecart.type, X = Xtot, row.w.init = row.w.init, 
                   call = match.call())
  tmp <- svd.triplet(X, row.w = row.w, col.w = col.w, ncp = ncp)
  eig <- tmp$vs^2
  vp <- matrix(NA, length(eig), 3)
  rownames(vp) <- paste("comp", 1:length(eig))
  colnames(vp) <- c("eigenvalue", "percentage of variance", 
                    "cumulative percentage of variance")
  vp[, "eigenvalue"] <- eig
  vp[, "percentage of variance"] <- (eig/sum(eig)) * 100
  vp[, "cumulative percentage of variance"] <- cumsum(vp[, 
                                                         "percentage of variance"])
  V <- tmp$V
  U <- tmp$U
  eig <- eig[1:ncp]
  coord.ind <- t(t(as.matrix(U)) * sqrt(eig))
  coord.var <- t(t(as.matrix(V)) * sqrt(eig))
  contrib.var <- t(t(coord.var^2)/eig) * col.w
  dist2 <- dist2.var
  cor.var <- coord.var/sqrt(dist2)
  cos2.var <- cor.var^2
  rownames(coord.var) <- rownames(cos2.var) <- rownames(cor.var) <- rownames(contrib.var) <- colnames(X)
  colnames(coord.var) <- colnames(cos2.var) <- colnames(cor.var) <- colnames(contrib.var) <- paste("Dim", 
                                                                                                   c(1:ncol(V)), sep = ".")
  res.var <- list(coord = coord.var[, 1:ncp], cor = cor.var[, 
                                                            1:ncp], cos2 = cos2.var[, 1:ncp], contrib = contrib.var[, 
                                                                                                                    1:ncp] * 100)
  dist2 <- dist2.ind
  cos2.ind <- coord.ind^2/dist2
  contrib.ind <- t(t(coord.ind^2 * row.w/sum(row.w))/eig)
  rownames(coord.ind) <- rownames(cos2.ind) <- rownames(contrib.ind) <- names(dist2) <- rownames(X)
  colnames(coord.ind) <- colnames(cos2.ind) <- colnames(contrib.ind) <- paste("Dim", 
                                                                              c(1:ncol(U)), sep = ".")
  res.ind <- list(coord = coord.ind[, 1:ncp, drop = FALSE], 
                  cos2 = cos2.ind[, 1:ncp, drop = FALSE], contrib = contrib.ind[, 
                                                                                1:ncp, drop = FALSE] * 100, dist = sqrt(dist2))
  res <- list(eig = vp, var = res.var, ind = res.ind, svd = tmp)
  if (!is.null(ind.sup)) {
    if (is.null(ecart.type)) 
      ecart.type <- rep(1, length(centre))
    X.ind.sup <- t(t(as.matrix(X.ind.sup)) - centre)
    X.ind.sup <- t(t(X.ind.sup)/ecart.type)
    coord.ind.sup <- t(t(X.ind.sup) * col.w)
    coord.ind.sup <- crossprod(t(coord.ind.sup), tmp$V)
    dist2 <- rowSums(t(t(X.ind.sup^2) * col.w))
    cos2.ind.sup <- coord.ind.sup^2/dist2
    coord.ind.sup <- coord.ind.sup[, 1:ncp, drop = F]
    cos2.ind.sup <- cos2.ind.sup[, 1:ncp, drop = F]
    colnames(coord.ind.sup) <- colnames(cos2.ind.sup) <- paste("Dim", 
                                                               c(1:ncp), sep = ".")
    rownames(coord.ind.sup) <- rownames(cos2.ind.sup) <- names(dist2) <- rownames(X.ind.sup)
    res.ind.sup <- list(coord = coord.ind.sup, cos2 = cos2.ind.sup, 
                        dist = sqrt(dist2))
    res$ind.sup = res.ind.sup
    res.call$ind.sup = ind.sup
  }
  if (!is.null(quanti.sup)) {
    X.quanti.sup <- as.data.frame(Xtot[, quanti.sup, drop = FALSE])
    if (!is.null(ind.sup)) 
      X.quanti.sup <- as.data.frame(X.quanti.sup[-ind.sup, 
                                                 , drop = FALSE])
    colnames(X.quanti.sup) <- colnames(Xtot)[quanti.sup]
    res.call$quanti.sup = X.quanti.sup
    centre.sup <- moy.ptab(X.quanti.sup, row.w)
    X.quanti.sup <- t(t(as.matrix(X.quanti.sup)) - centre.sup)
    if (scale.unit) {
      ecart.type.sup <- ec.tab(X.quanti.sup, row.w)
      X.quanti.sup <- t(t(X.quanti.sup)/ecart.type.sup)
    }
    coord.vcs <- t(X.quanti.sup * row.w)
    coord.vcs <- crossprod(t(coord.vcs), tmp$U)
    col.w.vcs <- rep(1, ncol(coord.vcs))
    cor.vcs <- matrix(NA, ncol(X.quanti.sup), ncol(tmp$U))
    dist2 <- as.vector(crossprod(rep(1, nrow(X.quanti.sup)), 
                                 as.matrix(X.quanti.sup^2 * row.w)))
    cor.vcs <- coord.vcs/sqrt(dist2)
    cos2.vcs <- cor.vcs^2
    colnames(coord.vcs) <- colnames(cor.vcs) <- colnames(cos2.vcs) <- paste("Dim", 
                                                                            c(1:ncol(cor.vcs)), sep = ".")
    rownames(coord.vcs) <- rownames(cor.vcs) <- rownames(cos2.vcs) <- colnames(Xtot)[quanti.sup]
    res.quanti.sup <- list(coord = coord.vcs[, 1:ncp, drop = FALSE], 
                           cor = cor.vcs[, 1:ncp, drop = FALSE], cos2 = cos2.vcs[, 
                                                                                 1:ncp, drop = FALSE])
    res$quanti.sup = res.quanti.sup
  }
  if (!is.null(quali.sup)) {
    X.quali.sup <- as.data.frame(Xtot[, quali.sup, drop = FALSE])
    if (!is.null(ind.sup)) 
      X.quali.sup <- as.data.frame(X.quali.sup[-ind.sup, 
                                               , drop = FALSE])
    colnames(X.quali.sup) <- colnames(Xtot)[quali.sup]
    nombre <- modalite <- NULL
    if (ncp > 1) 
      eta2 <- t(sapply(X.quali.sup, fct.eta2, res$ind$coord, 
                       weights = row.w))
    else {
      eta2 <- as.matrix(sapply(X.quali.sup, fct.eta2, res$ind$coord, 
                               weights = row.w), ncol = ncp)
      colnames(eta2) = paste("Dim", 1:ncp)
      rownames(eta2) = colnames(X.quali.sup)
    }
    for (i in 1:ncol(X.quali.sup)) {
      var <- as.factor(X.quali.sup[, i])
      n.mod <- nlevels(var)
      modalite <- c(modalite, n.mod)
      bary <- matrix(NA, n.mod, ncol(X))
      for (j in 1:n.mod) {
        ind <- levels(var)[j]
        bary[j, ] <- moy.ptab(data[which(var == ind), 
                                   ], row.w[which(var == ind)])
        nombre <- c(nombre, sum(row.w.init[which(var == 
                                                   ind)]))
      }
      colnames(bary) <- colnames(X)
      if ((levels(var)[1] %in% (1:nrow(X))) | (levels(var)[1] %in% 
                                               c("y", "Y", "n", "N"))) 
        row.names(bary) <- paste(colnames(X.quali.sup)[i], 
                                 as.character(levels(var)))
      else row.names(bary) <- as.character(levels(var))
      if (i == 1) 
        barycentre <- bary
      else barycentre <- rbind(barycentre, bary)
    }
    bary <- t(t(barycentre) - centre)
    if (!is.null(ecart.type)) 
      bary <- t(t(bary)/ecart.type)
    dist2 <- rowSums(t(t(bary^2) * col.w))
    coord.barycentre <- t(t(bary) * col.w)
    coord.barycentre <- crossprod(t(coord.barycentre), tmp$V)
    colnames(coord.barycentre) <- paste("Dim", 1:ncol(coord.barycentre), 
                                        sep = ".")
    cos2.bary.sup <- coord.barycentre^2/dist2
    vtest <- t(t(coord.barycentre)/sqrt(eig))
    if (sum(row.w.init) > 1) 
      vtest <- vtest * sqrt(nombre/((sum(row.w.init) - 
                                       nombre)/(sum(row.w.init) - 1)))
    else vtest <- vtest * sqrt(nombre)
    cos2.bary.sup <- cos2.bary.sup[, 1:ncp, drop = FALSE]
    coord.barycentre <- coord.barycentre[, 1:ncp, drop = FALSE]
    vtest <- vtest[, 1:ncp, drop = FALSE]
    dimnames(cos2.bary.sup) <- dimnames(vtest) <- dimnames(coord.barycentre)
    names(dist2) <- rownames(coord.barycentre)
    res.quali.sup <- list(coord = coord.barycentre, cos2 = cos2.bary.sup, 
                          v.test = vtest, dist = sqrt(dist2), eta2 = eta2)
    call.quali.sup <- list(quali.sup = X.quali.sup, modalite = modalite, 
                           nombre = nombre, barycentre = as.data.frame(barycentre), 
                           numero = quali.sup)
    res$quali.sup = res.quali.sup
    res.call$quali.sup = call.quali.sup
  }
  res$call = res.call
  class(res) <- c("PCA", "list ")
  return(res)
}

########################## ggdendro ###########################################

as.dendro <- function (segments, labels, leaf_labels = NULL, class) 
{
  if (missing(class)) 
    stop("Missing class in as.dendro")
  x <- list(segments = segments, labels = labels, leaf_labels = leaf_labels, 
            class = class)
  class(x) <- "dendro"
  x
}

.midDend <- function (x) {
  if (is.null(mp <- attr(x, "midpoint"))) 0 else mp
}

.memberDend <- function (x) {
  r <- attr(x, "x.member")
  if (is.null(r)) {
    r <- attr(x, "members")
    if (is.null(r)) 
      r <- 1L
  }
  r
}

plotNodeLimit <- function (x1, x2, subtree, center) 
{
  inner <- !is.leaf(subtree) && x1 != x2
  if (inner) {
    K <- length(subtree)
    mTop <- .memberDend(subtree)
    limit <- integer(K)
    xx1 <- x1
    for (k in 1L:K) {
      m <- .memberDend(subtree[[k]])
      xx1 <- xx1 + (if (center) 
        (x2 - x1) * m/mTop
        else m)
      limit[k] <- xx1
    }
    limit <- c(x1, limit)
  }
  else {
    limit <- c(x1, x2)
  }
  mid <- attr(subtree, "midpoint")
  center <- center || (inner && !is.numeric(mid))
  x <- if (center) 
    mean(c(x1, x2))
  else x1 + (if (inner) 
    mid
    else 0)
  list(x = x, limit = limit)
}

dendrogram_data <- function (x, type = c("rectangle", "triangle"), ...) 
{
  leaflab <- "perpendicular"
  center <- FALSE
  xlab <- ""
  ylab <- ""
  horiz <- FALSE
  xaxt <- "n"
  yaxt <- "s"
  nodePar <- NULL
  edgePar <- list()
  dLeaf <- NULL
  edge.root <- is.leaf(x) || !is.null(attr(x, "edgetext"))
  type <- match.arg(type)
  hgt <- attr(x, "height")
  if (edge.root && is.logical(edge.root)) 
    edge.root <- 0.0625 * if (is.leaf(x)) 
      1
  else hgt
  mem.x <- .memberDend(x)
  yTop <- hgt + edge.root
  if (center) {
    x1 <- 0.5
    x2 <- mem.x + 0.5
  }
  else {
    x1 <- 1
    x2 <- mem.x
  }
  xl. <- c(x1 - 1/2, x2 + 1/2)
  yl. <- c(0, yTop)
  if (edge.root) {
    if (!is.null(et <- attr(x, "edgetext"))) {
      my <- mean(hgt, yTop)
    }
  }
  gg.plotNode <- function(x1, x2, subtree, type, center, leaflab, 
                          dLeaf, nodePar, edgePar, horiz = FALSE, ddsegments = NULL, 
                          ddlabels = NULL) {
    inner <- !is.leaf(subtree) && x1 != x2
    yTop <- attr(subtree, "height")
    bx <- plotNodeLimit(x1, x2, subtree, center)
    xTop <- bx$x
    hasP <- !is.null(nPar <- attr(subtree, "nodePar"))
    if (!hasP) 
      nPar <- nodePar
    Xtract <- function(nam, L, default, indx) rep(if (nam %in% 
                                                      names(L)) L[[nam]] else default, length.out = indx)[indx]
    asTxt <- function(x) {
      if (is.character(x) || is.expression(x)) 
        x
      else if (is.null(x)) 
        ""
      else as.character(x)
    }
    i <- if (inner || hasP) 
      1
    else 2
    if (!is.null(nPar)) {
      pch <- Xtract("pch", nPar, default = 1L:2, i)
      cex <- Xtract("cex", nPar, default = c(1, 1), i)
      col <- Xtract("col", nPar, default = par("col"), 
                    i)
      bg <- Xtract("bg", nPar, default = par("bg"), i)
      points(if (horiz) 
        cbind(yTop, xTop)
        else cbind(xTop, yTop), pch = pch, bg = bg, col = col, 
        cex = cex)
    }
    lab.cex <- 1
    if (is.leaf(subtree)) {
      if (leaflab == "perpendicular") {
        Y <- yTop - dLeaf * lab.cex
        X <- xTop
        srt <- 90
        adj <- 1
        nodeText <- asTxt(attr(subtree, "label"))
        ddlabels <- rbind(ddlabels, data.frame(x = X, 
                                               y = 0, text = nodeText))
      }
    }
    else if (inner) {
      segmentsHV <- function(x0, y0, x1, y1) {
        data.frame(x0, y0, x1, y1)
      }
      for (k in seq_along(subtree)) {
        child <- subtree[[k]]
        yBot <- attr(child, "height")
        if (getOption("verbose")) 
          cat("ch.", k, "@ h=", yBot, "; ")
        if (is.null(yBot)) 
          yBot <- 0
        xBot <- if (center) 
          mean(bx$limit[k:(k + 1)])
        else bx$limit[k] + .midDend(child)
        if (type == "triangle") {
          ddsegments <- rbind(ddsegments, segmentsHV(xTop, 
                                                     yTop, xBot, yBot))
        }
        else {
          ddsegments <- rbind(ddsegments, segmentsHV(xTop, 
                                                     yTop, xBot, yTop))
          ddsegments <- rbind(ddsegments, segmentsHV(xBot, 
                                                     yTop, xBot, yBot))
        }
        vln <- NULL
        if (!is.null(attr(child, "edgetext"))) {
          edgeText <- asTxt(attr(child, "edgetext"))
          if (!is.null(vln)) {
            mx <- if (type == "triangle") 
              (xTop + xBot + ((xTop - xBot)/(yTop - yBot)) * 
                 vln)/2
            else xBot
            my <- (yTop + yBot + 2 * vln)/2
          }
          else {
            mx <- if (type == "triangle") 
              (xTop + xBot)/2
            else xBot
            my <- (yTop + yBot)/2
          }
        }
        plotNode_result <- gg.plotNode(bx$limit[k], bx$limit[k + 
                                                               1], subtree = child, type, center, leaflab, 
                                       dLeaf, nodePar, edgePar, horiz, ddsegments, 
                                       ddlabels)
        ddsegments <- plotNode_result$segments
        ddlabels <- plotNode_result$labels
      }
    }
    return(list(segments = ddsegments, labels = ddlabels))
  }
  ret <- gg.plotNode(x1, x2, x, type = type, center = center, 
                     leaflab = leaflab, dLeaf = dLeaf, nodePar = nodePar, 
                     edgePar = edgePar, horiz = FALSE, ddsegments = NULL, 
                     ddlabels = NULL)
  names(ret$segments) <- c("x", "y", "xend", "yend")
  names(ret$labels) <- c("x", "y", "label")
  ret
}

dendro_data <- function (model, type = c("rectangle", "triangle"), ...) 
{
  dhc <- as.dendrogram(model)
  hcdata <- dendrogram_data(dhc, type = type, ...)
  as.dendro(segments = hcdata$segments, labels = hcdata$labels, 
            class = "hclust")
}
