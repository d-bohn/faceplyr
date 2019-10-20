#' Read face landmarks
#'
#' @description
#' Functions to read/obtain facial landmarks from files or images.
#'
#' @param x Image or file to read. Acceptable files include JPsychomorph .tem files,
#' PNG, or JPG images.
#' @param ... Additional arguments to pass.
#'
#' @return Data frame of type \code{landmarks} with 68 facial landmarks.
#'
#' @importFrom reticulate import source_python
#' @importFrom tools file_ext
#' @export
#'
#' @examples
#' img <- system.file("extdata", "obama.png", package = "quantIm")
#' read_landmarks(img)
#' read_landmarks("template.tem")
#'
read_landmarks <- function(x, ...) {
  m <- tools::file_ext(x)
  class(x) <- append(m, class(x))

  if (m == "") class(x) <- append('invalid', class(x)[[1]])

  UseMethod('read_landmarks', x)
}

#' @export
read_landmarks.invalid <- function(x, ...) {
  stop('Please supply valid landmark file to read. \n',
       'Acceptable formats: .tem, .png, .jpg')
}

#' @export
read_landmarks.tem <- function(x, ...) {
  tem_df <- read.delim(x, header = FALSE, sep = ' ', fileEncoding = 'UTF-8', stringsAsFactors = FALSE)
  df <- tem_df[complete.cases(tem_df),]
  colnames(df) <- c('x','y')
  df$point <- seq(0,(nrow(df)-1))

  data <- data.frame(point = df$point, x = as.numeric(df$x), y = as.numeric(df$y))
  class(data) <- append(class(data),"landmarks")
  class(data) <- append(class(data),"from_tem")
  return(data)
}

#' @export
read_landmarks.jpg <- function(x, ...) {
  cv <- reticulate::import('cv2', convert = FALSE)
  img <- cv$imread(x)

  py_file <- system.file("python", "get_landmarks.py", package = "faceplyr")
  PREDICTOR_PATH = system.file("extdata", "shape_predictor_68_face_landmarks.dat", package = "faceplyr")

  reticulate::source_python(py_file)
  df <- get_landmarks(im = img, PREDICTOR_PATH = PREDICTOR_PATH)

  if (!is.null(df)) {
    landmarks <- data.frame(image_base = rep(basename(x),nrow(df)),
                            image_path = rep(x, nrow(df)),
                            point = seq(0,(nrow(df)-1)),
                            x = df[,1],
                            y = df[,2])
    class(landmarks) <- append(class(landmarks),"landmarks")
    return(landmarks)
  } else {
    warning("No faces found.")
  }
}

#' @export
read_landmarks.JPG <- function(x, ...) {
  cv <- reticulate::import('cv2', convert = FALSE)
  img <- cv$imread(x)

  py_file <- system.file("python", "get_landmarks.py", package = "faceplyr")
  PREDICTOR_PATH = system.file("extdata", "shape_predictor_68_face_landmarks.dat", package = "faceplyr")

  reticulate::source_python(py_file)
  df <- get_landmarks(im = img, PREDICTOR_PATH = PREDICTOR_PATH)

  if (!is.null(df)) {
    landmarks <- data.frame(image_base = rep(basename(x),nrow(df)),
                            image_path = rep(x, nrow(df)),
                            point = seq(0,(nrow(df)-1)),
                            x = df[,1],
                            y = df[,2])
    class(landmarks) <- append(class(landmarks),"landmarks")
    return(landmarks)
  } else {
    warning("No faces found.")
  }
}

#' @export
read_landmarks.tif <- function(x, ...) {
  cv <- reticulate::import('cv2', convert = FALSE)
  img <- cv$imread(x)

  py_file <- system.file("python", "get_landmarks.py", package = "faceplyr")
  PREDICTOR_PATH = system.file("extdata", "shape_predictor_68_face_landmarks.dat", package = "faceplyr")

  reticulate::source_python(py_file)
  df <- get_landmarks(im = img, PREDICTOR_PATH = PREDICTOR_PATH)

  if (!is.null(df)) {
    landmarks <- data.frame(image_base = rep(basename(x),nrow(df)),
                            image_path = rep(x, nrow(df)),
                            point = seq(0,(nrow(df)-1)),
                            x = df[,1],
                            y = df[,2])
    class(landmarks) <- append(class(landmarks),"landmarks")
    return(landmarks)
  } else {
    warning("No faces found.")
  }
}

#' @export
read_landmarks.jpeg <- function(x, ...) {
  cv <- reticulate::import('cv2', convert = FALSE)
  img <- cv$imread(x)

  py_file <- system.file("python", "get_landmarks.py", package = "faceplyr")
  PREDICTOR_PATH = system.file("extdata", "shape_predictor_68_face_landmarks.dat", package = "faceplyr")

  reticulate::source_python(py_file)
  df <- get_landmarks(im = img, PREDICTOR_PATH = PREDICTOR_PATH)

  if (!is.null(df)) {
    landmarks <- data.frame(image_base = rep(basename(x),nrow(df)),
                            image_path = rep(x, nrow(df)),
                            point = seq(0,(nrow(df)-1)),
                            x = df[,1],
                            y = df[,2])
    class(landmarks) <- append(class(landmarks),"landmarks")
    return(landmarks)
  } else {
    warning("No faces found.")
  }
}

#' @export
read_landmarks.png <- function(x, ...) {
  cv <- reticulate::import('cv2', convert = FALSE)
  img <- cv$imread(x)

  py_file <- system.file("python", "get_landmarks.py", package = "faceplyr")
  PREDICTOR_PATH = system.file("extdata", "shape_predictor_68_face_landmarks.dat", package = "faceplyr")

  reticulate::source_python(py_file)
  df <- get_landmarks(im = img, PREDICTOR_PATH = PREDICTOR_PATH)

  if (!is.null(df)) {
    landmarks <- data.frame(image_base = rep(basename(x),nrow(df)),
                            image_path = rep(x, nrow(df)),
                            point = seq(0,(nrow(df)-1)),
                            x = df[,1],
                            y = df[,2])
    class(landmarks) <- append(class(landmarks),"landmarks")
    return(landmarks)
  } else {
    warning("No faces found.")
  }
}

#' @export
read_landmarks.character <- function(x, ...) {
  cv <- reticulate::import('cv2', convert = FALSE)
  img <- cv$imread(x)

  py_file <- system.file("python", "get_landmarks.py", package = "faceplyr")
  PREDICTOR_PATH = system.file("extdata", "shape_predictor_68_face_landmarks.dat", package = "faceplyr")

  reticulate::source_python(py_file)
  df <- get_landmarks(im = img, PREDICTOR_PATH = PREDICTOR_PATH)

  if (!is.null(df)) {
    landmarks <- data.frame(image_base = rep(basename(x),nrow(df)),
                            image_path = rep(x, nrow(df)),
                            point = seq(0,(nrow(df)-1)),
                            x = df[,1],
                            y = df[,2])
    class(landmarks) <- append(class(landmarks),"landmarks")
    return(landmarks)
  } else {
    warning("No faces found.")
  }
}
