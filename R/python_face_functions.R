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
#' @export
#'
#' @examples
#'

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
