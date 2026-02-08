#' SEM Path Diagram (Base R)
#'
#' Draws a simple SEM path diagram using base R graphics.
#' Only latent-latent edges (B, Phi) are weighted by coefficient size and sign.
#' Measurement paths are drawn with fixed style for readability.
#'
#' @param object A \code{jjstat_sem} object or a list containing matrices:
#'   \code{Lambda}, \code{B}, \code{Phi} (and optionally \code{Theta}).
#' @param main Title.
#' @param layout Layout for latent variables: tree/tree2/tree3/circle.
#' @param rotate Integer 1~4. Rotate coordinates by 90-degree steps (clockwise).
#' @param show_phi Logical. If TRUE, draw latent correlations (Phi).
#' @param show_theta Logical. If TRUE, draw residual loops for observed variables (Theta diagonal).
#' @param std Logical. If TRUE, uses correlation for Phi (standardized visualization).
#' @param digits Numeric. Rounding digits for coefficients.
#'
#' @param sizeLat,sizeLat2 Latent ellipse radii (x/y) scaling.
#' @param sizeMan,sizeMan2 Manifest rectangle half-width/half-height scaling.
#'
#' @param edge.width.range Numeric length-2. Width range for latent-latent edges.
#' @param edge.color.pos Color for positive coefficients.
#' @param edge.color.neg Color for negative coefficients.
#'
#' @param curve A single numeric alias to control Phi curvature intensity.
#'   Internally mapped to \code{curve.base}. (Recommended to use this.)
#' @param curve.base Base curvature parameter for Phi (distance-adjusted).
#' @param curve.max Maximum curvature for Phi.
#'
#' @param zigzag Logical. If TRUE, apply small zigzag offsets to latent positions.
#' @param zigzag.amp Numeric. Zigzag amplitude.
#'
#' @export
#'
#' @examples
#' #'
#' library(jjstat)
#' # 검증용 python data
#' df <- data.frame(
#'   x1 = c( 1.730516,  1.159965, -0.648269, -0.593655,  1.213306,  0.895361, -2.729817, -1.785706,  0.354516,  0.294027),
#'   x2 = c( 2.208429,  1.111993, -0.671010, -0.309758,  0.893759,  1.354703, -2.800578, -1.492191,  0.510968,  0.176220),
#'   x3 = c( 1.791936,  0.665374, -0.004232, -0.253564,  0.835220,  0.945070, -2.115333, -1.176772,  0.018319, -0.232088),
#'   y1 = c( 1.581619,  1.372783, -0.346918, -0.078805,  1.076466,  1.521254, -1.682830, -1.047084,  1.098595, -0.667011),
#'   y2 = c( 2.393086,  1.230622, -0.902185, -0.714770,  1.501231,  1.572681, -2.962813, -1.159968,  0.709137, -0.200424),
#'   y3 = c( 0.896092,  1.639311, -0.756823, -0.286587,  0.717714,  1.296822, -1.155036, -1.229061,  0.482557,  0.030295)
#' )
#'
#' mydata<-df
#'
#' mydata
#'
#'
#' latent2 <- list(
#'   n1 = c("x1","x2","x3"),
#'   n2 = c("y1","y2","y3")
#' )
#'
#' res2 <- jjstat_SEM(
#'   data = mydata,
#'   latent_list = latent2,
#'   structural = list(
#'     path(from="n1", to="n2")
#'   ),
#'   residual_cov = list(c("y1","y2")),
#'   nboot = 300,
#'   rmseaCI = 0.90
#' )
#'
#' res2
#'
#' summary(res2)
#'
#' res2$parameters
#'
#' jjstat_plot_path(res2)
#' # jjstat_plot_path(res, show_phi = TRUE, show_theta = TRUE, curve = 2, layout="circle")
jjstat_plot_path <- function(object,
                             main = "SEM Path Diagram",
                             layout = c("tree","tree2","tree3","circle"),
                             rotate = 1,

                             show_phi   = TRUE,
                             show_theta = FALSE,

                             std.all = TRUE,
                             digits = 3,

                             ## node size
                             sizeLat  = 8,
                             sizeLat2 = 4,
                             sizeMan  = 5,
                             sizeMan2 = 3,

                             ## latent–latent edge control
                             edge.width.range = c(1, 6),
                             edge.color.pos = "black",
                             edge.color.neg = "red",

                             ## Phi curvature
                             curve = 0.6,
                             curve.max = 2,

                             ## latent zigzag
                             zigzag = TRUE,
                             zigzag.amp = 0.035) {

  layout <- match.arg(layout)

  ## ---------------------------------------------------------
  ## partial matching 방지
  ## ---------------------------------------------------------
  old_opt <- options(warnPartialMatchArgs = TRUE)
  on.exit(options(old_opt), add = TRUE)

  ## ---------------------------------------------------------
  ## extract matrices
  ## ---------------------------------------------------------
  Lambda <- object$Lambda
  B      <- object$B
  Phi0   <- object$Phi
  Theta  <- object$Theta

  if (is.null(Lambda) || is.null(B) || is.null(Phi0)) {
    stop("object must contain Lambda, B, Phi matrices.")
  }

  ## std.all ONLY
  Phi <- if (isTRUE(std.all)) stats::cov2cor(Phi0) else Phi0

  lv <- colnames(Lambda)
  ov <- rownames(Lambda)

  nlv <- length(lv)
  nov <- length(ov)

  ## ---------------------------------------------------------
  ## helper functions
  ## ---------------------------------------------------------
  edge_col <- function(x) ifelse(x >= 0, edge.color.pos, edge.color.neg)

  scale_edge <- function(x, rng) {
    a <- abs(x)
    if (diff(range(a, na.rm = TRUE)) == 0) return(mean(rng))
    rng[1] + (a - min(a, na.rm = TRUE)) /
      (max(a, na.rm = TRUE) - min(a, na.rm = TRUE)) *
      (rng[2] - rng[1])
  }

  apply_rotate <- function(x, y, rotate) {
    rotate <- ((rotate - 1) %% 4) + 1
    for (k in seq_len(rotate - 1)) {
      tmp <- x; x <- y; y <- 1 - tmp
    }
    list(x = x, y = y)
  }

  latent_zigzag <- function(x, y, amp, axis = "y") {
    off <- rep(c(1, -1, 0), length.out = length(x))
    if (axis == "y") y <- y + amp * off
    if (axis == "x") x <- x + amp * off
    list(x = x, y = y)
  }

  clip_to_ellipse <- function(x1, y1, x2, y2, rx, ry) {
    dx <- x2 - x1; dy <- y2 - y1
    len <- sqrt((dx / rx)^2 + (dy / ry)^2)
    if (!is.finite(len) || len == 0) return(c(x1, y1))
    c(x1 + dx / len, y1 + dy / len)
  }

  normal_offset <- function(x1, y1, x2, y2, mag) {
    dx <- x2 - x1; dy <- y2 - y1
    len <- sqrt(dx^2 + dy^2)
    if (!is.finite(len) || len == 0) return(c(0, 0))
    c(-dy / len, dx / len) * mag
  }

  ## ---------------------------------------------------------
  ## layout
  ## ---------------------------------------------------------
  if (layout %in% c("tree","tree2")) {
    base_y <- ifelse(layout == "tree", 0.75, 0.25)
    lv_x <- seq(0.2, 0.8, length.out = nlv)
    lv_y <- rep(base_y, nlv)
    if (zigzag) {
      zz <- latent_zigzag(lv_x, lv_y, zigzag.amp, "y")
      lv_x <- zz$x; lv_y <- zz$y
    }

  } else if (layout == "tree3") {
    lv_x <- rep(0.3, nlv)
    lv_y <- seq(0.2, 0.8, length.out = nlv)
    if (zigzag) {
      zz <- latent_zigzag(lv_x, lv_y, zigzag.amp, "x")
      lv_x <- zz$x; lv_y <- zz$y
    }

  } else { ## circle
    ang <- seq(0, 2*pi, length.out = nlv + 1)[-1]
    lv_x <- 0.5 + 0.3 * cos(ang)
    lv_y <- 0.6 + 0.3 * sin(ang)
    if (zigzag) {
      zz <- latent_zigzag(lv_x, lv_y, zigzag.amp * 0.5, "y")
      lv_x <- zz$x; lv_y <- zz$y
    }
  }

  ov_x <- seq(0.05, 0.95, length.out = nov)
  ov_y <- rep(0.15, nov)

  ## rotate (final)
  rlv <- apply_rotate(lv_x, lv_y, rotate)
  rov <- apply_rotate(ov_x, ov_y, rotate)

  lv_xy <- data.frame(x = rlv$x, y = rlv$y, row.names = lv)
  ov_xy <- data.frame(x = rov$x, y = rov$y, row.names = ov)

  ## ---------------------------------------------------------
  ## plot base
  ## ---------------------------------------------------------
  graphics::par(bg = "white", mar = c(2,2,4,2))
  graphics::plot(NA, xlim = c(0,1), ylim = c(0,1),
                 axes = FALSE, xlab = "", ylab = "", main = main)

  rx <- sizeLat  / 200
  ry <- sizeLat2 / 200

  ## ---------------------------------------------------------
  ## measurement paths (fixed)
  ## ---------------------------------------------------------
  for (i in seq_len(nov)) for (j in seq_len(nlv)) {
    if (Lambda[i,j] != 0) {
      graphics::arrows(lv_xy$x[j], lv_xy$y[j] - ry,
                       ov_xy$x[i], ov_xy$y[i] + sizeMan2/200,
                       lwd = 1, length = 0.08)
      graphics::text(lv_xy$x[j]*0.6 + ov_xy$x[i]*0.4,
                     lv_xy$y[j]*0.6 + ov_xy$y[i]*0.4,
                     round(Lambda[i,j], digits), cex = 0.9)
    }
  }

  ## ---------------------------------------------------------
  ## structural paths B
  ## ---------------------------------------------------------
  for (i in seq_len(nlv)) for (j in seq_len(nlv)) {
    if (B[i,j] != 0) {

      s <- clip_to_ellipse(lv_xy$x[j], lv_xy$y[j],
                           lv_xy$x[i], lv_xy$y[i], rx, ry)
      e <- clip_to_ellipse(lv_xy$x[i], lv_xy$y[i],
                           lv_xy$x[j], lv_xy$y[j], rx, ry)

      w   <- scale_edge(B[i,j], edge.width.range)
      col <- edge_col(B[i,j])

      graphics::arrows(s[1], s[2], e[1], e[2], lwd = w, col = col)

      off <- normal_offset(s[1], s[2], e[1], e[2], 0.03)
      graphics::text(mean(c(s[1],e[1])) + off[1],
                     mean(c(s[2],e[2])) + off[2],
                     round(B[i,j], digits), col = col)
    }
  }

  ## ---------------------------------------------------------
  ## Phi paths (ONLY when no structural path exists)
  ## ---------------------------------------------------------
  if (isTRUE(show_phi)) {
    for (i in 1:(nlv-1)) for (j in (i+1):nlv) {

      ## ❗ 구조경로가 있으면 Phi는 그리지 않음
      if (B[i,j] != 0 || B[j,i] != 0) next

      r <- Phi[i,j]
      if (!is.finite(r) || r == 0) next

      x1 <- lv_xy$x[i]; y1 <- lv_xy$y[i]
      x2 <- lv_xy$x[j]; y2 <- lv_xy$y[j]

      dist <- sqrt((x1-x2)^2 + (y1-y2)^2)
      if (!is.finite(dist) || dist == 0) next

      curv <- min(curve.max, curve / dist)

      xs <- seq(x1, x2, length.out = 100)
      ys <- y1 + curv * (xs - x1) * (x2 - xs)

      w   <- scale_edge(r, edge.width.range)
      col <- edge_col(r)

      graphics::lines(xs, ys, lwd = w, col = col, lty = 2)

      mid <- 50
      off <- normal_offset(xs[mid], ys[mid],
                           xs[mid+1], ys[mid+1], 0.03)

      graphics::text(xs[mid] + off[1],
                     ys[mid] + off[2],
                     round(r, digits),
                     col = col)
    }
  }


  ## ---------------------------------------------------------
  ## Theta loops (observed residuals)
  ## ---------------------------------------------------------
  if (isTRUE(show_theta) && !is.null(Theta)) {
    th <- diag(Theta)
    th[!is.finite(th)] <- 0

    th_lwd <- if (diff(range(th)) == 0) rep(1, nov) else {
      z <- (th - min(th)) / (max(th) - min(th))
      1 + 2*z
    }

    tt <- seq(0, 2*pi, length.out = 120)
    rloop <- 0.022

    for (k in seq_along(ov)) {
      graphics::lines(ov_xy$x[k] + rloop*cos(tt),
                      ov_xy$y[k] + rloop*sin(tt),
                      lwd = th_lwd[k], col = "gray30")
      graphics::text(ov_xy$x[k], ov_xy$y[k] + rloop + 0.02,
                     round(th[k], digits), cex = 0.75, col = "gray30")
    }
  }

  ## ---------------------------------------------------------
  ## draw nodes
  ## ---------------------------------------------------------
  for (i in seq_along(lv)) {
    t <- seq(0, 2*pi, length.out = 200)
    graphics::polygon(lv_xy$x[i] + rx*cos(t),
                      lv_xy$y[i] + ry*sin(t),
                      col = "white", border = "black")
    graphics::text(lv_xy$x[i], lv_xy$y[i], lv[i], font = 2)
  }

  for (i in seq_along(ov)) {
    graphics::rect(ov_xy$x[i] - sizeMan/200,
                   ov_xy$y[i] - sizeMan2/200,
                   ov_xy$x[i] + sizeMan/200,
                   ov_xy$y[i] + sizeMan2/200)
    graphics::text(ov_xy$x[i], ov_xy$y[i], ov[i], cex = 0.8)
  }

  invisible(object)
}

