convert_landmarks <- function(x, write_out = TRUE, savename) {
  if (endsWith(x, '.json')) {
    inherits(x, 'json')

  } else if (endsWith(x, '.tem')) {
    inherits(x, 'tem')

  } else if (endsWith(x, '.csv')) {
    inherits(x, 'landmarks')

  } else {
    inherits(x, 'landmarks')
  }

  UseMethod('convert_landmarks', x)
}

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

convert_landmarKs.tem <-
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
        savename <- file.path(dirname(landmark_file), paste0(file, '.tem'))
      }

    } else if (isTRUE(is.data.frame(landmarks))) {
      landmarks <- landmarks
      file <-
        tools::file_path_sans_ext(as.character(landmarks$image_base[[1]]))

      if (isFALSE(hasArg(savename))) {
        savename <-
          file.path(dirname(as.character(landmarks$image_path[[1]])), paste0(file, '.tem'))
      }
    }

    df <- data.frame(x = landmarks$x, y = landmarks$y)
    data <- rbind(c(68, NA), df, c(0, NA))

    if (write_out == TRUE) {
      readr::write_delim(
        data,
        savename,
        delim = ' ',
        na = '',
        col_names = FALSE
      )
    } else if (write_out == FALSE) {
      return(data)
    }
  }

convert_landmarks.landmarks <-
  function(tem, write_out = TRUE, savename) {
    if (isTRUE(endsWith(tem, '.tem'))) {
      tem_df <-
        read.delim(tem,
                   header = FALSE,
                   sep = ' ',
                   fileEncoding = 'UTF-8')

      if (isFALSE(hasArg(savename))) {
        savename <- paste0(file_path_sans_ext(tem), '.csv')
      }

    } else if (isFALSE(is.character(tem)) &
               isTRUE(is.data.frame(tem))) {
      tem_df <- tem

      if (isFALSE(hasArg(savename))) {
        time <- gsub("[: -]", "" , Sys.time(), perl = TRUE)
        savname <- paste0('landmarks_file', time, '.csv')
      }

    } else {
      stop('Please supply valid template file or dataframe.')
    }

    df <- tem_df[complete.cases(tem_df), ]
    colnames(df) <- c('x', 'y')
    df$point <- seq(0, 67)

    data <-
      data.frame(point = df$point,
                 x = as.numeric(df$x),
                 y = as.numeric(df$y))

    if (write_out == TRUE) {
      write.table(
        data,
        savename,
        sep = ',',
        row.names = FALSE,
        col.names = FALSE
      )

    } else if (write_out == FALSE) {
      return(data)
    }
  }
