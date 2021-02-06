# SCT helpers ----
#' @export
col_orange <- cli::make_ansi_style("orange")

#' @export
emo_labels <- function() {
  emo_labels1 <- "Angry|Anger|Disgust|Disgusted|Fear|Happy|Sad|Surprise|Surprised|Neutral"
  emo_labels2 <- tolower(emo_labels1)
  paste(emo_labels1, emo_labels2, sep = "|")
}

#' @export
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
    cv <- reticulate::import("cv2")
    cv$imwrite(savename, masked_face)
  }
}

#' @export
extract_vars <- function(image, extract_emotion = FALSE, data_save_folder = "./output"){

  if (str_detect(image, "~")) {
    stop("Must supply absolute or relative path to image; no shortcuts (i.e., `~`)")
  }

  img <- image
  img_basename <- basename(img)

  data_save_folder <- data_save_folder

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

  if (with_oval & with_angles) {
    # TODO: Add angles to plots
    stop("Not implemented yet.")

  } else if (with_oval) {
    if (!hasArg(point_select)) {
      point_select <- "face_outline"
    }

    if (!hasArg(fit)) {
      fit  <- "taubin"
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

  } else if (with_angles) {
    # TODO: Add angles to plots
    stop("Not implemented yet.")

  } else {
    par(mar=c(0, 0, 0, 0))
    res = dim(img)[1:2]
    plot(1,1,xlim=c(0,res[1]),ylim=c(res[2],0),asp=1,type='n',xaxs='i',yaxs='i',
         xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    graphics::rasterImage(img,1,1,res[1],res[2])
    points(landmarks[2:3], pch = 20, col='red')

  }
}

