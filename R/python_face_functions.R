#' Swap faces between two images
#'
#' @param reference Path to image with face to host swapped face from \code{target} image.
#' @param target Path to image with face to swap to \code{reference} image.
#' @param convert Logical. Convert returned image to be \code{R} compatible?
#' Defaults to \code{FALSE} and returns numpy matrix.
#'
#' @return Either a numpy matrix or EBImage object.
#'
#' @importFrom reticulate import source_python
#' @importFrom EBImage readImage
#'
#' @export
face_swap <- function(reference, target, convert = FALSE){

  py_file <- system.file("python", "face_warp_full.py", package = "faceplyr")
  reticulate::source_python(py_file, convert = FALSE)

  img <- out_image(reference = reference, target = target)

  if (convert == TRUE){
    on.exit(
      unlink(c(temp))
    )
    cv <- reticulate::import('cv2', convert = FALSE)

    temp <- tempfile(fileext = '.png')
    cv$imwrite(temp, img)

    out_im <- EBImage::readImage(temp)
    return(out_im)

  } else {
    return(img)
  }

}

#' Crop face from bounding box
#'
#' @param image
#' @param bb
#' @param savename
#' @param return_img
#' @param return_bb
#' @param wh
#' @param scale
#' @param scale_left
#' @param scale_top
#' @param scale_right
#' @param scale_bottom
#'
#' @return
face_crop <- function(image, bb = NULL, savename, return_img = FALSE,
                                      return_bb=FALSE, wh = 200L, scale = TRUE,
                                      scale_left = 0, scale_top=0.5,
                                      scale_right = 0.1, scale_bottom = 0.1) {

  cv <- reticulate::import("cv2")

  py_file <- system.file("python", "bb.py", package = "faceplyr")
  reticulate::source_python(py_file, convert = FALSE)

  landmarks <- system.file("extdata", "shape_predictor_68_face_landmarks.dat",
                           package = "faceplyr")

  if (!is.null(bb)) {
    bb <- bb

  } else {
    bb <- tf_bb(x = image, landmarks = landmarks, scale_left = 0, scale_top=0.5,
                scale_right = 0.1, scale_bottom = 0.1)
  }

  crop_img <- tf_crop(x = image, bb = bb)

  if (scale) {
    imutils <- reticulate::import("imutils")
    crop_img <- imutils$resize(crop_img, width = wh)
  }

  if (return_img) {
    return(crop_img)

  } else {
    cv$imwrite(savename, crop_img)
  }

  if (return_bb) {
    return(bb)
  }

  if (return_img & return_bb) {
    return(list(image = crop_img,
                bounding_box = bb))
  }
}

#' Mask an image to interior face only
#'
#' @param image
#' @param bb
#' @param savename
#' @param return_img
#' @param transparent
#'
#' @return
#'
#' @export
face_mask <- function(image, bb = NULL, crop = TRUE, savename = NULL, return_img = FALSE, transparent = TRUE) {
    cv <- reticulate::import("cv2")

    py_file <- system.file("python", "face_mask.py", package = "faceplyr")
    reticulate::source_python(py_file, convert = FALSE)

    landmarks <-
      system.file("extdata",
                  "shape_predictor_68_face_landmarks.dat",
                  package = "faceplyr")

    masked_face <- mask_face(image = image, landmarks = landmarks)

    if (crop) {
      masked_face <- crop_background(masked_face)

      if (transparent) {

        if (tools::file_ext(savename) != "png") {
          stop ("transparency requires saving image as a PNG")

        }
        masked_face <- remove_background(masked_face)
      }
    }

    if (return_img) {
      return(masked_face)

    } else {
      if (!is.null(savename)) {
        cv$imwrite(savename, masked_face)
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
  cv <- reticulate::import("cv2")

  py_file <- system.file("python", "halve_image.py", package = "faceplyr")
  reticulate::source_python(py_file, convert = FALSE)

  halve_face(image = image)
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
face_hist <- function(img, shape = c(8, 8, 8), colorspace = "rgb") {
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

  py_file <- system.file("python", "face_features.py", package = "faceplyr")
  reticulate::source_python(py_file, convert = FALSE)

  hist <- face_hist(img = img,
                    shape = shape,
                    colorspace = colorspace)
}

#' Extract Haralick texture features from face image
#'
#' @param img
#'
#' @return
#'
#' @export
face_texture <- function(img) {
  py_file <- system.file("python", "face_features.py", package = "faceplyr")
  reticulate::source_python(py_file, convert = FALSE)

  textures <- face_texture(img = img)
}

remove_background <- function(image, savename, return_img = FALSE) {
  py_file <- system.file("python", "face_mask.py", package = "faceplyr")
  reticulate::source_python(py_file, convert = FALSE)

  masked_face <- crop_background(image)

  if (tools::file_ext(savename) != "png") {
    stop ("transparency requires saving image as a PNG")

  }

  masked_face <- remove_background(masked_face)

  if (return_img) {
    return(masked_face)

  } else {
    # cv <- reticulate::import("cv2")
    cv$imwrite(savename, masked_face)
  }
}
