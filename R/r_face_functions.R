#' Create affine transformation
#'
#' @param landmarks1 Reference image to warp to.
#' @param landmarks2 Image to be warped.
#'
#' @return
#' @importFrom vegan procrustes
#' @export
#'
#' @examples
#'
transform_points <- function(landmarks1, landmarks2) {
    landmarks1 <- landmarks1[c('x','y')]
    landmarks2 <- landmarks2[c('x','y')]

  tmp <- vegan::procrustes(landmarks1, landmarks2)
  r <- unlist(tmp['rotation'])
  t <- unlist(tmp['translation'])

  affine <- matrix(c(r[[1]],r[[2]],t[[1]],r[[3]],r[[4]],t[[2]]), nrow=3)
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
#'
#' @examples
#'
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
#' @export
#'
#' @examples
#'
create_average_template <- function(files) {
  mat3d <- NULL
  for (file in 1:length(files)) {
    landmarks <- read_landmarks(files[[file]])[c('x','y')]
    mat3d <- abind::abind(mat3d, landmarks, along = 3)
  }

  average_temp <- apply(mat3d, 1:2, mean)
  return(average_temp)
}

#' Converts landmarks file to json file
#'
#' @param landmarks Landmarks returned from \link[quantIm]{read_landmarks}
#' @param write_out If \code{TRUE} (default) write file to disk, else returns landmarks as json.
#' @param savename If supplied the name of the file to write out, else will derive directory from
#' landmarks file supplied.
#'
#' @return File or landmarks as type \code{.json}
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom tools file_path_sans_ext
#' @export
#'
#' @examples
#' img <- system.file("extdata", "obama.png", package = "quantIm")
#' landmarks <- read_landmarks(img)
#' landmarks_to_json(landmarks)
#'
landmarks_to_json <- function(landmarks, write_out = TRUE, savename) {

  if (!(is.data.frame(landmarks)) & !(is.character(landmarks))) {
    message("Please supply landmark dataframe or path")
  } else if (!(is.data.frame(landmarks)) & isTRUE(is.character(landmarks))) {
    landmark_file <- landmarks
    landmarks <- read.csv(landmarks)
    file <- tools::file_path_sans_ext(as.character(landmarks$image_base[[1]]))
    if (!(hasArg(savename))) {
      savename <- file.path(dirname(landmark_file),paste0(file,'__labels.json'))
    }
  } else if (isTRUE(is.data.frame(landmarks))) {
    landmarks <- landmarks
    file <- tools::file_path_sans_ext(as.character(landmarks$image_base[[1]]))
    if (!(hasArg(savename))) {
      savename <- file.path(dirname(as.character(landmarks$image_path[[1]])),paste0(file,'__labels.json'))
    }
  }

  tem <- jsonlite::fromJSON(system.file("extdata", "dlib-landmark-mean__labels.json", package = "quantIm"))

  tem$image_filename <- landmarks$image_base[[1]]
  tem$labels$position$x <- landmarks$x
  tem$labels$position$y <- landmarks$y

  if (write_out == TRUE) {
    jsondf <- jsonlite::toJSON(tem, dataframe = 'row', raw = 'mongo')
    readr::write_lines(jsondf, savename)
  } else if (write_out == FALSE) {
    jsondf <- jsonlite::toJSON(tem, dataframe = 'row', raw = 'mongo')
    return(jsondf)
  }

}

#' Converts landmarks to JPsychomorph tem file
#'
#' @param landmarks Landmarks returned from \link[quantIm]{read_landmarks}
#' @param write_out If \code{TRUE} (default) write file to disk, else returns landmarks as json.
#' @param savename If supplied the name of the file to write out, else will derive directory from
#' landmarks file supplied.
#'
#' @return \code{.tem} file or data frame
#' @importFrom readr write_delim
#' @importFrom tools file_path_sans_ext
#' @export
#'
#' @examples
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
#' Convert a \code{.tem} file created by JPsychomoprh back to a standard facial landmarks file
#' for use with \code{quantIm}.
#'
#' @param tem Template file
#' @param write_out Should the file be written out or returned?
#' @param savename File save name without extension (optional).
#'
#' @return
#' @export
#'
#' @examples
#'
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

#' Crop a face and rescale
#'
#' @aliases Daniel N. Albohn
#'
#' @param landmarks
#' @param image
#' @param points
#' @param savename
#' @param wh
#' @param dist
#'
#' @return
#' @export
#'
#' @examples
face_crop_points <- function(landmarks, image, points = 'default', savename,
                             eye_dist = 125, rescale = TRUE, wh = 512, ...){

  on.exit(raster::removeTmpFiles(h=2))

  if (!hasArg(landmarks)) {
    coords <- quantIm::read_landmarks(image)
  }

  if (points == 'default'){
    points_list <- list(
      pupil_l = c(37,40,38,41),
      pupil_r = c(43,46,44,47),
      bridge = c(30,28),
      top_brows = 17:22,
      bottom_chin = 8L
    )
  }

  ## Check if it we have numeric values
  coords$x <- as.numeric(as.character(coords$x))
  coords$y <- as.numeric(as.character(coords$y))

  ## Function to estimate center of four points in a rectangle
  centerC <- function(point1,point2,point3,point4){

    point1x <- coords$x[coords$point==point1]; point1y <- coords$y[coords$point==point1]
    point2x <- coords$x[coords$point==point2]; point2y <- coords$y[coords$point==point2]
    point3x <- coords$x[coords$point==point3]; point3y <- coords$y[coords$point==point3]
    point4x <- coords$x[coords$point==point4]; point4y <- coords$y[coords$point==point4]

    mid1x <- (point1x+point2x) / 2
    mid2x <- (point3x+point4x) / 2
    mid1y <- (point1y+point2y) / 2
    mid2y <- (point3y+point4y) / 2

    centerx <- (mid1x + mid2x) / 2
    centery <- (mid1y + mid2y) / 2

    return(c(centerx,centery))

  }

  ## Estimate center of left pupil
  left_pupil <- centerC(points_list$pupil_l[[1]],points_list$pupil_l[[2]],
                        points_list$pupil_l[[3]],points_list$pupil_l[[4]])
  ## Estimate center of right pupil
  right_pupil <- centerC(points_list$pupil_r[[1]],points_list$pupil_r[[2]],
                         points_list$pupil_r[[3]],points_list$pupil_r[[4]])

  ## Get our values to crop by
  pupil_dist <- max(left_pupil[1],right_pupil[1]) - min(left_pupil[1],right_pupil[1])

  scale <- as.numeric(as.character(eye_dist)) / as.numeric(as.character(pupil_dist))
  wh_scale <- wh/scale

  centerx <- left_pupil[[1]]-(190/scale)
  centery <- left_pupil[[2]]-(256/scale)

  ## Now crop!
  image_sans <- tools::file_path_sans_ext(image)
  img <- magick::image_read(image)
  # wh2 <- paste(wh_scale,wh_scale,sep = 'x')
  w_h_x_y <- magick::geometry_area(wh_scale, wh_scale, centerx, centery)

  im <- magick::image_crop(img, w_h_x_y)
  # magick::image_browse(im)

  ## Load and rescale
  if (rescale==TRUE){
    percent = scale*100
    img <- magick::image_scale(im, paste0(percent,'%'))
    # magick::image_browse(img)

    if (hasArg(savename)) {
      savename <- paste0(image_sans,'_crop_scale.png')
    }

    attempt1 <- try( magick::image_write(img, path = savename, format = "png"), silent = TRUE)

    if (class(attempt1)=='try-error') {
      savename <- paste0(image_sans,'_crop_scale.jpg')
      attempt2 <- try( magick::image_write(img, path = savename, format = "jpg"), silent = TRUE )
      if (class(attempt2)=='try-error') message('Please provide an appropriate
                                                image type to be written.')
    }

    coords_new <- quantIm:::read_landmarks(image = savename)

    coords <- dplyr::bind_cols(coords, coords_new)

    return(coords)

  } else {
    if (!hasArg(savename)) {
      savename <- paste0(image_sans,'_crop.png')
    }

    attempt1 <- try( magick::image_write(im, path = savename, format = "png"), silent = TRUE)

    if (class(attempt1)=='try-error') {
      savename <- paste0(image_sans,'_crop.jpg')
      attempt2 <- try( magick::image_write(im, path = savename, format = "jpg"), silent = TRUE )
      if (class(attempt2)=='try-error') message('Please provide an appropriate
                                                image type to be written.')
    }
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
#' @export
#'
#' @examples
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





