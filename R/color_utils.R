# Functions taken from `plotwidgets` package
# Website: <https://cran.r-project.org/web/packages/plotwidgets/index.html>
# Author: January Weiner <january.weiner@gmail.com>
# Version: 0.4
# License: GPL-2 | GPL-3 [expanded from: GPL (≥ 2.0)]

#' @export
rgb2hsl <- function (rgb) {
  if (nrow(rgb) == 4) {
    alpha <- rgb[4, , drop = F]
    rgb <- rgb[-4, , drop = F]
  }
  else {
    alpha <- NULL
  }
  rgb <- rgb / 255
  mins <- apply(rgb, 2, min)
  maxs <- apply(rgb, 2, max)
  d <- maxs - mins
  L <- (maxs + mins) / 2
  S <- d / (1 - abs(2 * L - 1))
  sel <- d == 0
  S[sel] <- 0
  wmax <- apply(rgb, 2, which.max)
  H <- L
  HR <- (rgb[2,] - rgb[3,]) / (maxs - mins)
  HG <- 2 + (rgb[3,] - rgb[1,]) / (maxs - mins)
  HB <- 4 + (rgb[1,] - rgb[2,]) / (maxs - mins)
  sel <- wmax == 1
  H[sel] <- HR[sel]
  sel <- wmax == 2
  H[sel] <- HG[sel]
  sel <- wmax == 3
  H[sel] <- HB[sel]
  H <- (H * 60) %% 360
  H[mins == maxs] <- 0
  ret <- rbind(H = H,
               S = S,
               L = L,
               alpha = alpha)
  return(ret)
}

#' @export
col2hsl <- function (col) {
  rgb2hsl(col2rgb.2(col))
}

#' @export
rgb2hsl <- function (rgb) {
  if (nrow(rgb) == 4) {
    alpha <- rgb[4, , drop = F]
    rgb <- rgb[-4, , drop = F]
  }
  else {
    alpha <- NULL
  }
  rgb <- rgb / 255
  mins <- apply(rgb, 2, min)
  maxs <- apply(rgb, 2, max)
  d <- maxs - mins
  L <- (maxs + mins) / 2
  S <- d / (1 - abs(2 * L - 1))
  sel <- d == 0
  S[sel] <- 0
  wmax <- apply(rgb, 2, which.max)
  H <- L
  HR <- (rgb[2,] - rgb[3,]) / (maxs - mins)
  HG <- 2 + (rgb[3,] - rgb[1,]) / (maxs - mins)
  HB <- 4 + (rgb[1,] - rgb[2,]) / (maxs - mins)
  sel <- wmax == 1
  H[sel] <- HR[sel]
  sel <- wmax == 2
  H[sel] <- HG[sel]
  sel <- wmax == 3
  H[sel] <- HB[sel]
  H <- (H * 60) %% 360
  H[mins == maxs] <- 0
  ret <- rbind(H = H,
               S = S,
               L = L,
               alpha = alpha)
  return(ret)
}

#' @export
col2rgb.2 <- function (col) {
  alphas <- rep(255, length(col))
  pat <- "^#([[:xdigit:]]{6})([[:xdigit:]]{2})"
  sel <- grep(pat, col)
  alphas[sel] <- strtoi(paste0("0X", gsub(pat, "\\2", col[sel])))
  if (all(alphas == 255)) {
    ret <- col2rgb(col)
  }
  else {
    ret <- rbind(col2rgb(col), alpha = alphas)
  }
  ret
}
