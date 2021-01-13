# Face Roundness ----
# Adapted from
# Website: <https://www.researchgate.net/post/Does_any_one_know_about_Roundness>

#' Extract roundness of the face
#'
#' @param image Image to calculate race roundness
#' @param landmarks Optional. Landmarks provided via `read_landmarks` function.
#' @param point_select One of `c("face_interior","face_outline", "face_hair","mouth","right_eye","left_eye")` or
#' vector of point landmarks (0-68) to fit the ellipse to. Defaults to `face_outline` which is most appropriate
#' for full face roundness measures in most cases.
#' @param fit One of `c("taubin","direct")` for algorithm to use for fitting ellipse to points. Defaults to `taubin`.
#'
#' @return Dataframe with roundness and circularity values for the provided image. See details
#' for what each named value corresponds with.
#'
#' @details The `*1` returns indicate that the scale is set to 1; thus, the closer to 1 the value
#' is the more it is like a circle.
#' \itemize{
#'  \item{"roundness1}{Calculated as: `parimeter^2/(4*pi*area)`. Perfect circle = 1.}
#'  \item{"roundness2"}{Calculated as: `4*area/pi*R_major^2`. TBA}
#'  \item("circularity1"){Calculated as: `4*pi*(area/parimeter^2)`. Perfect circle = 1.}
#'  \item("circularity2"){Calculated as: `(4*pi*area)/parimeter`. TBA}
#' }
#'
#' @importFrom conicfit EllipseFitByTaubin EllipseDirectFit AtoG calculateEllipse
#' @import tidyverse
#'
#' @export
calc_roundness <- function(image, landmarks = NULL, point_select = "face_outline", fit = "taubin") {

  elip_meas <- ellipse_measures(
    image = image,
    landmarks = landmarks,
    point_select = point_select,
    fit = fit
  )

  # with R = 1 = perfect circle
  # R_hat = (perimeter squared)/(4*pi*area)
  roundness1 <- (elip_meas$parimeter^2)/(4*pi*elip_meas$area) # Compactness?
  circularity1 <-  4*pi*(elip_meas$area/elip_meas$parimeter^2)

  # TODO: look into these functions to see what scale they are on
  # (4 × Area)/(pi × Major axis^2)
  roundness2 <- (4 * elip_meas$area)/(pi * elip_meas$R_major^2)
  circularity2 <- (4*pi*elip_meas$area)/elip_meas$parimeter

  return(data.frame(
    roundess1 = roundness1,
    roundness2 = roundness2,
    circularity1 = circularity1,
    circularity2 = circularity2
  ))
}

# Face Angularity ----

#' Extract angularity of the face
#'
#' @param image Image to calculate race roundness
#' @param landmarks Optional. Landmarks provided via `read_landmarks` function.
#'
#' #' @return Dataframe with angularity values for the provided image. See details
#' for what each named value corresponds with.
#'
#' @import tidyverse
#'
#' @export
calc_angulrity <- function(image, landmarks = NULL) {
  # [right, mid, left] (x,y) points
  # Top half angle [1, (21+22)/2, 15]
  # Bottom half angle [1, 8, 15]

  stop("Not implemented yet.")
}

# Helper Functions ----
fit_ellipse <- function(image, landmarks = NULL,
                        point_select = c(
                          "face_interior",
                          "face_outline", "face_hair",
                          "mouth","right_eye","left_eye"),
                        fit = c("direct", "taubin")
) {

  if (!is.null(landmarks)) {
    landmarks <- landmarks

  } else if (is.null(landmarks)) {
    landmarks <- faceplyr::read_landmarks(image)[c('point','x','y')]
  }

  if (is.character(point_select)) {
    point_sel <- switch (point_select,
                         face_outline = c(0:16, 17, 26),
                         face_interior = c(0:20, 23:26),
                         face_hair = c(0:16),
                         mouth = c(48:59),
                         right_eye = c(36:41),
                         left_eye = c(42:47)
    )

  } else if (is.numeric(point_select)) {
    point_sel <- point_select
  }


  ellipse_points <- landmarks %>%
    as_tibble(.) %>%
    filter(., point %in% (point_sel))

  ellipse_pts <- ellipse_points[c('x','y')]

  ellipDirect <- switch(fit,
                        direct = EllipseDirectFit(as.matrix(ellipse_pts)),
                        taubin = EllipseFitByTaubin(as.matrix(ellipse_pts))
  )

  ellipDirectG <- AtoG(ellipDirect)$ParG # Center (x,y), axis a, axis b, angle
  ellipse_g <- calculateEllipse(ellipDirectG[1], ellipDirectG[2], ellipDirectG[3], ellipDirectG[4], 180/pi*ellipDirectG[5])

  return(list(
    ellipse_geometry = ellipDirectG,
    ellipse_pt_est = ellipse_g
  )
  )
}

ellipse_measures <- function(image, landmarks = NULL,
                             point_select = "face_outline",
                             fit = "taubin") {

  ems <- fit_ellipse(image = image,
                     landmarks = landmarks,
                     point_select = point_select,
                     fit = fit)

  R1 <- ems$ellipse_geometry[3]*2
  R2 <- ems$ellipse_geometry[4]*2

  if (R1 != R2) {
    R_major <- max(R1, R2)
    R_minor <- min(R1, R2)

  } else {
    R_major <- R1
    R_minor <- R2
  }

  # pi*R_major*R_minor
  area <- pi*R_major*R_minor

  # 2pi*sqrt((R_major^2+R_minor^2)/2)
  parimeter <- (2*pi)*sqrt((R_major^2+R_minor^2)/2)

  return(list(
    area = area,
    parimeter = parimeter,
    R_major = R_major,
    R_minor = R_minor,
    ellipse_geometry = ems$ellipse_geometry,
    ellipse_pt_est = ems$ellipse_pt_est
  ))

}
