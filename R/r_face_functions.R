#' Create affine transformation
#'
#' @param landmarks1 Reference image to warp to.
#' @param landmarks2 Image to be warped.
#'
#' @return
#' @importFrom vegan procrustes
#'
#' @export
transform_points <- function(landmarks1, landmarks2, align_to = c("everything", "eyes")) {
  if (align_to == "everything") {
    landmarks1 <- landmarks1[c('x', 'y')]
    landmarks2 <- landmarks2[c('x', 'y')]

  } else if (align_to == "eyes") {
    landmarks1 <- landmarks1[c(36:47),c('x', 'y')]
    landmarks2 <- landmarks2[c(36:47), c('x', 'y')]

  } else {
    stop("'align_to' must be one of: 'everything', 'eyes'")
  }

  tmp <- vegan::procrustes(landmarks1, landmarks2)
  r <- unlist(tmp['rotation'])
  t <- unlist(tmp['translation'])

  affine <-
    matrix(c(r[[1]], r[[2]], t[[1]], r[[3]], r[[4]], t[[2]]), nrow = 3)
  return(affine)
}

#' Warp image with affine transformation
#'
#' @param image Path to/image to be warped.
#' @param m Affine matrix.
#'
#' @return
#' @importFrom EBImage affine readImage as.Image

#' @export
warp_face <- function(image, m) {

  if (is.character(image)) {
    img <- EBImage::readImage(image)
  } else {
    img <- EBImage::as.Image(image)
  }

  tmp <- EBImage::affine(
    x = img,
    m = m,
    bg.col = "#777777"
  )
  return(tmp)
}

#' Create an average template from multiple templates
#'
#' @param files List of files to be averaged.
#'
#' @return
#' @importfrom abind abind
#'
#' @export
create_average_template <- function(files) {
  mat3d <- NULL
  for (file in 1:length(files)) {
    landmarks <- read_landmarks(files[[file]])[c('x','y')]
    mat3d <- abind::abind(mat3d, landmarks, along = 3)
  }

  average_temp <- apply(mat3d, 1:2, mean)
  return(average_temp)
}

#' Converts landmarks to JPsychomorph tem file
#'
#' @param landmarks Landmarks returned from \link[faceplyr]{read_landmarks}
#' @param write_out If \code{TRUE} (default) write file to disk, else returns landmarks as json.
#' @param savename If supplied the name of the file to write out, else will derive directory from
#' landmarks file supplied.
#'
#' @return \code{.tem} file or data frame
#' @importFrom readr write_delim
#' @importFrom tools file_path_sans_ext
#' @export
#'
landmarks_to_tem <- function(landmarks, write_out = TRUE, savename) {
  if (!(is.data.frame(landmarks)) & !(is.character(landmarks))) {
    message("Please supply landmark dataframe or path")
  } else if (!(is.data.frame(landmarks)) & isTRUE(is.character(landmarks))) {
    landmark_file <- landmarks
    landmarks <- read.csv(landmarks)
    file <- tools::file_path_sans_ext(as.character(landmarks$image_base[[1]]))
    if (!(hasArg(savename))) {
      savename <- file.path(dirname(landmark_file),paste0(file,'.tem'))
    }
  } else if (isTRUE(is.data.frame(landmarks))) {
    landmarks <- landmarks
    file <- tools::file_path_sans_ext(as.character(landmarks$image_base[[1]]))
    if (!(hasArg(savename))) {
      savename <- file.path(dirname(as.character(landmarks$image_path[[1]])),paste0(file,'.tem'))
    }
  }

  df <- data.frame(x = landmarks$x, y = landmarks$y)
  data <- rbind(c((nrow(df)),NA),df,c(0,NA))

  if (write_out == TRUE) {
    readr::write_delim(data, savename, delim = ' ', na = '', col_names = FALSE)
  } else if (write_out == FALSE) {
    return(data)
  }
}

#' Convert JPsychomorph template to landmarks
#'
#' @description
#' Convert a \code{.tem} file created by JPsychomoprh back to a standard facial landmarks file.
#'
#' @param tem Template file
#' @param write_out Should the file be written out or returned?
#' @param savename File save name without extension (optional).
#'
#' @return
#'
#' @export
tem_to_landmarks <- function(tem, write_out = TRUE, savename) {

  if (endsWith(tem, '.tem')) {
    tem_df <- read.delim(tem, header = FALSE, sep = ' ', fileEncoding = 'UTF-8')

    if (!hasArg(savename)) {
      savename <- paste0(file_path_sans_ext(tem),'.csv')
    }

  } else if (is.character(tem) &
             isTRUE(is.data.frame(tem))) {
    tem_df <- tem

    if (!(hasArg(savename))) {
      time <- gsub("[: -]", "" , Sys.time(), perl=TRUE)
      savname <- paste0('landmarks_file',time,'.csv')
    }

  } else {
    stop('Please supply valid template file or dataframe.')
  }

  df <- tem_df[complete.cases(tem_df),]
  colnames(df) <- c('x','y')
  df$point <- seq(0,(nrow(df)-1))

  data <- data.frame(point = df$point, x = as.numeric(df$x), y = as.numeric(df$y))

  if (write_out == TRUE) {
    write.table(data, savename, sep = ',', row.names = FALSE, col.names = FALSE)

  } else if (write_out == FALSE) {
    return(data)
  }
}

#' Add a 69th point to face template
#'
#' @param template
#' @param write_out
#'
#' @return Face coordinates with 69 (instead of 68) face points. Can
#' be returned as a .tem for JPsychomorph.
#' @details The 69th point is estimated to be at the top of the head,
#' somewhere between the hairline and back of the head. This is useful
#' if one is using dlib's 68 face point auto detector but also wants
#' an extra point to expand image size to include more of the face.
#'
#' 69th point is estimated by taking the face width (dlib points 2 & 14) and
#' multiplying it by 1.5.
#'
#' @export
add_face_point <- function(template, write_out = c('tem','console')) {
  landmarks <- suppressWarnings( read_landmarks(template) )

  x1 <- landmarks[3,'x']
  y1 <- landmarks[3,'y']

  x2 <- landmarks[15,'x']
  y2 <- landmarks[15,'y']

  distance <- sqrt(
    (x2-x1)^2 +
      (y2-y1)^2
  )

  face_height <- distance*1.5

  x8 <- landmarks[9,'x']
  y8 <- landmarks[9,'y']

  newx <- x8
  newy <- round(y8-face_height,0)

  if (write_out == 'tem' & ("from_tem" %in% class(landmarks))) {
    landmarks <- rbind(landmarks, c(68, newx, newy))
    landmarks_to_tem(landmarks = landmarks, write_out = TRUE, savename = template)
  } else if (write_out == 'console' & ("from_tem" %in% class(landmarks))) {
      landmarks <- rbind(landmarks, c(68, newx, newy))
      tem <- landmarks_to_tem(landmarks = landmarks, write_out = FALSE)
      return(tem)
  }
}





