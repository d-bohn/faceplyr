#' Mask an image to interior face only
#'
#' @param image
#' @param savename
#' @param return_img
#' @param transparent
#'
#' @return
#'
#' @export
face_mask <- function(image, crop = TRUE, savename = NULL, return_img = FALSE, transparent = TRUE) {
    # cv <- reticulate::import("cv2")

    # py_file <- system.file("python", "scripts", "face_mask.py", package = "faceplyr")
    # reticulate::source_python(py_file, convert = FALSE)

    landmarks <-
      system.file("extdata",
                  "shape_predictor_68_face_landmarks.dat",
                  package = "faceplyr")

    masked_face <- fp$utils$p_mask_face(image = image, landmarks = landmarks)

    if (crop) {
      masked_face <- fp$utils$p_crop_background(masked_face)

      if (transparent) {

        if (!return_img & tools::file_ext(savename) != "png") {
          stop ("transparency requires saving image as a PNG")

        }

        masked_face <- fp$utils$p_remove_background(masked_face)
      }
    }

    if (return_img) {
      return(masked_face)

    } else {

      if (!is.null(savename)) {
        fp$utils$cv2$imwrite(savename, masked_face)

      } else stop()

    }
}

#' Halve a face image and save
#'
#' @param image
#'
#' @return
#'
#' @export
halve_face <- function(image) {
  # cv <- reticulate::import("cv2")

  # py_file <- system.file("python", "halve_image.py", package = "faceplyr")
  # reticulate::source_python(py_file, convert = FALSE)

  fp$utils$p_halve_face(image = image)
}

#' Extract color histogram from face image
#'
#' @param img Image to be analyzed (png or jpg).
#' @param shape (blue, red, green) or (hue, saturation, and value) histogram bin sizes for `RGB` and `HSV`, respectively.
#' @param colorspace Whether to use `rgb` or `hsv` colorspace when computing histogram.
#'
#' @return Returns vector of size shape*3 in the same order as defined above (blue, green, red) for `rgb` and (hue, saturation, value) for `hsv`.
#'
#' @export
face_hist <- function(img, shape = c(8L, 8L, 8L), colorspace = "rgb") {
  # Some checks
  if (!(colorspace %in% c("rgb", "hsv"))) {
    stop("'colorspace' must be of value 'rgb' or 'hsv'")
  }

  if (colorspace == "hsv" &
      shape[1] > 180 | shape[1] < 1) {
    stop("'hsv' hue bin value must be between 1 and 180")
  }

  if (colorspace == "hsv"  &
      any(shape[2:3] > 255) | any(shape[2:3] < 1) ) {
    stop("'hsv' saturation and value bin values must be between 1 and 255")
  }

  if (colorspace == "rgb"  &
      any(shape > 255) | any(shape < 1) ) {
    stop("'rgb' red, green, and blue bin values bust be between 1 and 255")
  }

  # py_file <- system.file("python", "face_features.py", package = "faceplyr")
  # reticulate::source_python(py_file, convert = FALSE)

  hist <- fp$features$p_face_hist(img = img,
                    shape = shape,
                    colorspace = colorspace)
  return(hist)
}

#' Extract Haralick texture features from face image
#'
#' @param img
#'
#' @return
#'
#' @export
face_texture <- function(img) {
  # py_file <- system.file("python", "face_features.py", package = "faceplyr")
  # reticulate::source_python(py_file, convert = FALSE)

  textures <- fp$features$p_face_texture(img = img)
}

remove_background <- function(image, savename, return_img = FALSE) {
  # py_file <- system.file("python", "face_mask.py", package = "faceplyr")
  # reticulate::source_python(py_file, convert = FALSE)

  masked_face <- fp$utils$p_crop_background(image)

  if (tools::file_ext(savename) != "png") {
    stop ("transparency requires saving image as a PNG")

  }

  masked_face <- fp$utils$p_remove_background(masked_face)

  if (return_img) {
    return(masked_face)

  } else {
    # cv <- reticulate::import("cv2")
    fp$utils$cv2$imwrite(savename, masked_face)
  }
}
