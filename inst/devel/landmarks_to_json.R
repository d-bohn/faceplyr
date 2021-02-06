convert_landmarks.json <-
  function(landmarks, write_out = TRUE, savename) {
    if (isFALSE(is.data.frame(landmarks)) &
        isFALSE(is.character(landmarks))) {
      message("Please supply landmark dataframe or path")

    } else if (isFALSE(is.data.frame(landmarks)) &
               isTRUE(is.character(landmarks))) {
      landmark_file <- landmarks
      landmarks <- read.csv(landmarks)
      file <-
        tools::file_path_sans_ext(as.character(landmarks$image_base[[1]]))

      if (isFALSE(hasArg(savename))) {
        savename <-
          file.path(dirname(landmark_file), paste0(file, '__labels.json'))
      }

    } else if (isTRUE(is.data.frame(landmarks))) {
      landmarks <- landmarks
      file <-
        tools::file_path_sans_ext(as.character(landmarks$image_base[[1]]))

      if (isFALSE(hasArg(savename))) {
        savename <-
          file.path(dirname(as.character(landmarks$image_path[[1]])), paste0(file, '__labels.json'))
      }
    }

    tem <-
      jsonlite::fromJSON(system.file("extdata", "dlib-landmark-mean__labels.json", package = "faceplyr"))

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


#' Converts landmarks file to json file
#'
#' @param landmarks Landmarks returned from \link[faceplyr]{read_landmarks}
#' @param write_out If \code{TRUE} (default) write file to disk, else returns landmarks as json.
#' @param savename If supplied the name of the file to write out, else will derive directory from
#' landmarks file supplied.
#'
#' @return File or landmarks as type \code{.json}
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom tools file_path_sans_ext
#'
#' @examples
#' img <- system.file("extdata", "queen.png", package = "faceplyr")
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

  tem <- jsonlite::fromJSON(system.file("extdata", "dlib-landmark-mean__labels.json", package = "faceplyr"))

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
