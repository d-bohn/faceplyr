# Expansion-Constriction ----
#' @export
calc_expanse_constrict <- function(image = NULL, compare_to = NULL) {


}

# Utility functions ----
#' @export
of <- function(image1, image2, pyr_scale = 0.5, levels = 3, winsize = 150,
               iterations = 3, poly_n = 7, poly_sigma = 1.5) {
  # require(Rvision)
  img1 <- Rvision::image(image1)
  img2 <- Rvision::image(image2)
  of <- Rvision::farneback(img1, img2, winsize = winsize)
  # x = ncol(of)
  # y = (nrow(of))
  return(of)
}

#' @export
plot_expanse_constrict <- function(image, optical_flow, gridsize = c(25, 25), thresh = 0,
                                   add = TRUE, arrow.ex = 0.08, xpd = FALSE, length = 0.05,
                                   ...) {
  # require(Rvision)
  Rvision:::plot.Rcpp_Image(Rvision::image(image))
  Rvision:::plot.OF_array(optical_flow, gridsize =gridsize, thresh = thresh,
       add = add, arrow.ex = arrow.ex, xpd = xpd, length = length, ..)
}
