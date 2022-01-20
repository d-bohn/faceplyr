# Startup helpers ----
m1arm_startup_helper <- function() {
  brew_exists <-
    ifelse(any(
      system2("/opt/homebrew/bin/brew", args = "--version",
              stdout = TRUE, stderr = FALSE) == ""),
    FALSE, TRUE)

  miniforge_exists_cask <- dir.exists("/opt/homebrew/Caskroom/miniforge")
  miniforge_exists_opt <- dir.exists("/opt/")

  if (!miniforge_exists_cask | !miniforge_exists_opt) {
    warning("Please make sure brew/miniforge is installed.")
  }

  if (miniforge_exists_cask & !miniforge_exists_opt) {
    file.symlink("/opt/homebrew/Caskroom/miniforge", "/opt/miniforge")
    message("miniforge symlinked to /opt/miniforge for reticulate.")
  }

  if (miniforge_exists_opt) {
    if (("faceplyr" %in% reticulate::conda_list()$name)) {
      Sys.unsetenv("RETICULATE_PYTHON")

      reticulate::use_condaenv("faceplyr", required = TRUE)

      path <- system.file("python", package = "faceplyr", mustWork = TRUE)
      fp <<- reticulate::import_from_path(
        "faceplyr",
        path = path,
        convert = FALSE
      )
    }
  }

}

# SCT helpers ----
#' @export
col_orange <- cli::make_ansi_style("orange")

#' @export
emo_labels <- function() {
  emo_labels1 <- "Angry|Anger|Disgust|Disgusted|Fear|Happy|Sad|Surprise|Surprised|Neutral"
  emo_labels2 <- tolower(emo_labels1)
  paste(emo_labels1, emo_labels2, sep = "|")
}

# remove_background <- function(image, savename, return_img = FALSE) {
#   # py_file <- system.file("python", "face_mask.py", package = "faceplyr")
#   # reticulate::source_python(py_file, convert = FALSE)
#
#   masked_face <- fp$utils$p_crop_background(image)
#
#   if (tools::file_ext(savename) != "png") {
#     stop ("transparency requires saving image as a PNG")
#
#   }
#
#   masked_face <- p_remove_background(masked_face)
#
#   if (return_img) {
#     return(masked_face)
#
#   } else {
#     cv <- reticulate::import("cv2")
#     cv$imwrite(savename, masked_face)
#   }
# }

#' @export
extract_vars <- function(image, extract_emotion = FALSE, data_save_folder){

  if (str_detect(image, "~")) {
    stop("Must supply absolute or relative path to image; no shortcuts (i.e., `~`)")
  }

  img <- image
  img_basename <- basename(img)

  if (is.null(data_save_folder)) {
    data_save_folder <- "."

  } else {
    data_save_folder <- data_save_folder
  }


  if (extract_emotion) {
    emotion <- str_extract(img, emo_labels()) %>% tools::toTitleCase(.)

    if (is.na(emotion)) {
      emotion <- "None"
    }

    savename <- file.path(
      data_save_folder,
      paste0(tools::file_path_sans_ext(img_basename),"_",emotion,"_cropped", ".png")
    )

  } else {
    emotion <- NA

    savename <- file.path(
      data_save_folder,
      paste0(tools::file_path_sans_ext(img_basename),"_cropped", ".png")
    )
  }

  vars <- list(
    img = img,
    img_basename = img_basename,
    emotion = emotion,
    savename = savename,
    data_save_folder = data_save_folder
  )

  if (!fs::dir_exists(data_save_folder)) fs::dir_create(data_save_folder)

  return(vars)

}

get_sqlite_db <- function(data_save_folder = NULL, dbname = NULL) {
  if (is.null(data_save_folder)) {
    data_save_folder <- getwd()

  } else {
    data_save_folder <- data_save_folder
  }

  if (is.null(dbname)) {
    dbname <- glue::glue("face_features.sqlite")

  } else {
    dbname <- paste0(gsub(".sqlite","",dbname),".sqlite")
  }

  return(fs::path(data_save_folder, dbname))

}

read_sqlite <- function(table = NULL, save_file) {
  on.exit(DBI::dbDisconnect(conn))

  conn <- DBI::dbConnect(RSQLite::SQLite(), save_file)

  if (is.null(table)) {
    return(RSQLite::dbListTables(conn))

  } else {
    table <- RSQLite::dbReadTable(conn, table)
    return(table)

  }
}

write_sqlite <- function(data = NULL, table = NULL, save_file, overwrite = FALSE, append = FALSE) {
  on.exit(DBI::dbDisconnect(conn))

  conn <- DBI::dbConnect(RSQLite::SQLite(), save_file)

  if (overwrite) {
    RSQLite::dbWriteTable(conn, table, data, ovewrite = TRUE, append = append)

  } else if (append) {
    RSQLite::dbWriteTable(conn, table, data, ovewrite = FALSE, append = TRUE)

  } else if (fs::file_exists(save_file)) {
    RSQLite::dbWriteTable(conn, table, data, ovewrite = FALSE, append = TRUE)

  } else {
    RSQLite::dbWriteTable(conn, table, data, ovewrite = overwrite, append = append)
  }

}

write_meta_sqlite <- function(image, save_file = NULL, append = FALSE) {
  if (stringr::str_detect(image, "~")) {
    stop("Must supply absolute or relative path to image; no shortcuts (i.e., `~`)")
  }

  if (is.null(save_file)) {
    save_file <- fs::path(getwd(), "face_features")
    save_folder <- dirname(save_file)

  } else {
    save_folder <- dirname(save_file)
  }

  image_base <- basename(image)
  image_ext <- tools::file_ext( basename(image) )
  image_sans_ext <- tools::file_path_sans_ext(image_base)

  meta <- tibble::tibble(
    image_path = image,
    image_base = image_base,
    image_sans_ext = image_sans_ext,
    image_ext = image_ext,
    cropped_image_path = glue::glue("{save_folder}/{image_sans_ext}_cropped.{image_ext}"),
    top_image_path = glue::glue("{save_folder}/{image_sans_ext}_cropped_top.{image_ext}"),
    bottom_image_path = glue::glue("{save_folder}/{image_sans_ext}_cropped_bottom.{image_ext}")
  )

  write_sqlite(data = meta, table = "meta", save_file = save_file)

}

# Plot face land marks ----
#' @export
plot_landmarks <- function(landmarks=NULL, image, save=NULL,
                           with_oval = FALSE,
                           with_angles = FALSE,
                           ...) {

  if (!is.null(landmarks)) {
    landmarks <- landmarks

  } else if (is.null(landmarks) & hasArg(image)) {
    landmarks <- faceplyr::read_landmarks(image)[c('point','x','y')]
  }

  img <- EBImage::readImage(image)

  if (Sys.info()[["sysname"]] == "Linux") {
    img <- EBImage::rotate(img, 180) %>%
      EBImage::flop(.)
  }

  # Plot with Oval
  if (with_oval) {
    if (!hasArg(point_select)) {
      point_select <- "face_outline"
    } else {
      point_select <- point_select
    }

    if (!hasArg(fit)) {
      fit  <- "taubin"
    } else {
      fit <- "taubin"
    }

    ellip_meas <- ellipse_measures(image = image, landmarks = landmarks,
                                   point_select = point_select,
                                   fit = fit)

    ellipse_g <- ellip_meas$ellipse_pt_est

    par(mar=c(0, 0, 0, 0))
    res = dim(img)[1:2]
    plot(1,1,xlim=c(0,res[1]),ylim=c(res[2],0),asp=1,type='n',xaxs='i',yaxs='i',
         xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    graphics::rasterImage(img,1,1,res[1],res[2])
    points(landmarks[2:3], pch = 20, col='red')
    text(landmarks[2:3],labels=landmarks$point)
    points(ellipse_g)
    lines(ellipse_g, col="red")

  }

  if (with_angles) {
    # TODO: Add angles to plots
    stop("Not implemented yet.")

  }

  if (!with_oval && !with_angles) {
    par(mar=c(0, 0, 0, 0))
    res = dim(img)[1:2]
    plot(1,1,xlim=c(0,res[1]),ylim=c(res[2],0),asp=1,type='n',xaxs='i',yaxs='i',
         xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    graphics::rasterImage(img,1,1,res[1],res[2])
    points(landmarks[2:3], pch = 20, col='red')

  }
}

