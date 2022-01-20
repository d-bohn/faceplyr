#' Prepare image for feature extraction
#'
#' @param image
#' @param data_save_folder
#' @param sqlite_db_name
#'
#' @return
#' @export
#'
#' @examples
extract_prep <- function(image,
                         data_save_folder = NULL,
                         sqlite_db_name = NULL) {

  if (is.null(data_save_folder) & is.null(sqlite_db_name)) {
    stop("Must supply a data save folder and database.")
  }

  savename <- fs::path(
    glue::glue("{data_save_folder}/{tools::file_path_sans_ext(basename(image))}_cropped.png")
  )

  mask <- tryCatch({
    face_mask(image = image,
              savename = savename,
              return_img = FALSE,
              crop = TRUE,
              transparent = TRUE)

  }, warning = function(w) {
    message(sprintf("Warning in %s: %s", deparse(w[["call"]]), w[["message"]]))

  }, error = function(e) {
    message(sprintf("Error in %s: %s", deparse(e[["call"]]), e[["message"]]))
  })

  if (!is.null(data_save_folder)) {
    fp$utils$cv2$imwrite(savename, mask)
  }

  if (!is.null(sqlite_db_name)) {
    # Setup
    sqlite_db_name <- tools::file_path_sans_ext(sqlite_db_name)
    dbname <- sqlite_db_name
    dt <- format(Sys.Date(), "%m-%d-%Y")
    sqlite_db <- faceplyr:::get_sqlite_db(data_save_folder = data_save_folder,
                                          dbname = glue::glue("{dbname}-{dt}.sqlite")
    )

    faceplyr:::write_meta_sqlite(image = image,
                                 save_file = sqlite_db
    )
  }

  return(c("db name" = sqlite_db))
}

#' Extract structure metric from interior of the face
#'
#' @param image
#' @param data_save_folder
#' @param sqlite_db
#' @param return_results
#'
#' @return
#'
#' @import tidyverse
#' @importFrom tools file_path_sans_ext
#' @import cli
#' @import glue
#'
#' @export
extract_structure <- function(image,
                              data_save_folder = NULL,
                              sqlite_db = NULL,
                              return_results = FALSE) {

  # TODO: Add option to not mask face
  # mask <- tempfile(fileext='.png')

  if (!is.null(sqlite_db)) {
    img_base <- read_sqlite("meta", save_file = sqlite_db)
    meta_key <- img_base[
      img_base$image_sans_ext == tools::file_path_sans_ext(basename(image)),]

    savename <- fs::path(
      meta_key$cropped_image_path
    )

  } else if(!is.null(data_save_folder)) {
    savename <- fs::path(
      glue::glue("{data_save_folder}/{tools::file_path_sans_ext(basename(image))}_cropped.png")
    )

  } else {
    stop("Must supply a data save folder or database")
  }

  # mask <- tryCatch({
  #   face_mask(image = image,
  #             savename = savename,
  #             return_img = FALSE,
  #             crop = TRUE,
  #             transparent = TRUE)
  #
  # }, warning = function(w) {
  #   message(sprintf("Warning in %s: %s", deparse(w[["call"]]), w[["message"]]))
  #
  # }, error = function(e) {
  #   message(sprintf("Error in %s: %s", deparse(e[["call"]]), e[["message"]]))
  # })

  if (file.exists(savename)) {

    # ** Extract keypoints ----
    struct <- tryCatch({
      mask_points <- faceplyr::read_landmarks(savename)

      structure <- mask_points |>
        tibble::as_tibble() |>
        dplyr::select(point, x, y) |>
        tidyr::gather(key, value, -point) |>
        tidyr::unite(key, key, point) |>
        tidyr::spread(key, value) |>
        dplyr::mutate(
          image = image,
          image_base = basename(image)) |>
        dplyr::select(image, image_base, everything())

    }, warning = function(w) {
      message(sprintf("Warning in %s: %s", deparse(w[["call"]]), w[["message"]]))

    }, error = function(e) {
      message(sprintf("Error in %s: %s", deparse(e[["call"]]), e[["message"]]))

    })

    if (!is.null(struct)) {

      if (return_results) {
        return(structure)

      } else if (!is.null(sqlite_db) & !return_results) {
        write_sqlite(structure, "structure", save_file = sqlite_db)

        if (getOption("faceplyr.verbose")) {
          cli::cli_alert(glue::glue(cli::col_blue("Image complete (structure): {savename}")))
        }

      } else {
        stop("must supply sqlite_db or return results TRUE.")
      }
    }
  }
}

#' Extract texture metric from interior face
#'
#' @param image
#' @param data_save_folder
#' @param sqlite_db
#' @param return_results
#'
#' @return
#'
#' @import tidyverse
#' @importFrom tools file_path_sans_ext
#' @import cli
#' @import glue
#'
#' @export
extract_texture <- function(image,
                            data_save_folder = NULL,
                            sqlite_db = NULL,
                            return_results = FALSE) {

  if (!is.null(sqlite_db)) {
    img_base <- read_sqlite("meta", save_file = sqlite_db)
    meta_key <- img_base[
      img_base$image_sans_ext == tools::file_path_sans_ext(basename(image)),]

    savename <- fs::path(
      meta_key$cropped_image_path
    )

  } else if(!is.null(data_save_folder)) {
    savename <- fs::path(
      glue::glue("{data_save_folder}/{tools::file_path_sans_ext(basename(image))}_cropped.png")
    )

  } else {
    stop("Must supply a data save folder or database")
  }

  if (fs::file_exists(savename)) {

    tex <- tryCatch({
      halve_face(savename)

      bot_img <- paste0(tools::file_path_sans_ext(savename),"_bottom.",tools::file_ext(savename))
      top_img <- paste0(tools::file_path_sans_ext(savename),"_top.",tools::file_ext(savename))

      remove_background(bot_img, savename = bot_img, return_img = FALSE)
      remove_background(top_img, savename = top_img, return_img = FALSE)

      texture_b <- face_texture(bot_img)
      texture_t <- face_texture(top_img)

      texture <- tibble::tibble(tex_bottom=array(texture_b),
                                tex_top=array(texture_t),
                                number = 1:13)

      texture2 <- texture |>
        tidyr::gather(key, value, -number) |>
        tidyr::unite(key, key, number) |>
        tidyr::spread(key, value) |>
        dplyr::mutate(
          image = image,
          image_base = basename(image)
        ) |>
        dplyr::select(image, image_base, everything())

    }, warning = function(w) {
      message(sprintf("Warning in %s: %s", deparse(w[["call"]]), w[["message"]]))

    }, error = function(e) {
      message(sprintf("Error in %s: %s", deparse(e[["call"]]), e[["message"]]))

    })

    if (!is.null(tex)) {

      if (return_results) {
        return(texture2)

      } else if (!is.null(sqlite_db) & !return_results) {
        write_sqlite(texture2, "texture", save_file = sqlite_db)

        if (getOption("faceplyr.verbose")) {
          cli::cli_alert(glue::glue(cli::col_green("Image complete (texture): {savename}")))
        }

      } else {
        stop("must supply sqlite_db or return results TRUE.")
      }
    }
  }
}

#' Extract kmeans color metric from interior face
#'
#' @param image
#' @param data_save_folder
#' @param sqlite_db
#' @param return_results
#' @param kclusts
#'
#' @return
#'
#' @import tidyverse
#' @importFrom imager load.image grayscale
#' @importFrom tools file_path_sans_ext
#' @import cli
#' @import glue
#'
#' @export
extract_kmean_colors <- function(image,
                                 data_save_folder = NULL,
                                 sqlite_db = NULL,
                                 return_results = FALSE,
                                 kclusts = 25) {

  if (!is.null(sqlite_db)) {
    img_base <- read_sqlite("meta", save_file = sqlite_db)
    meta_key <- img_base[
      img_base$image_sans_ext == tools::file_path_sans_ext(basename(image)),]

    savename <- fs::path(
      meta_key$cropped_image_path
    )

  } else if(!is.null(data_save_folder)) {
    savename <- fs::path(
      glue::glue("{data_save_folder}/{tools::file_path_sans_ext(basename(image))}_cropped.png")
    )

  } else {
    stop("Must supply a data save folder or database")
  }

  if (fs::file_exists(savename)) {
    # Load up the image using load.image function!
    im <- imager::load.image(savename)

    folder <- data_save_folder

    df_size <-dim(im)[1]*dim(im)[2]
    max_row_num <- 150000 ## number of maximum rows

    ## If df is too big, it's too slow to process on my computer, so shrink the image
    shrink_ratio  <- if(df_size > max_row_num) {max_row_num / df_size } else {1}
    im <- im |> imager::imresize(shrink_ratio)

    ## get RGB value at each pixel
    im_rgb <- im |>
      as.data.frame(wide="c") |>
      dplyr::rename(red=c.1,green=c.2,blue=c.3) |>
      dplyr::mutate(hexvalue = rgb(red,green,blue)) ## you can create hexvalue using red, green blue value!

    ## turn image into Grayscale and get luminance "value" too.
    {if (dim(im)[[4]] != 3) {
      im <- imager::rm.alpha(im)
    }}

    im_gray <- im |>
      imager::grayscale() |>
      as.data.frame()

    ## combine RGB info and Luminance Value Dataset together.
    im_df <- im_rgb |>
      dplyr::inner_join(im_gray)

    ## Pick k value to run kMean althorithm.
    ## But to extract colours, I'd pick k as number I want back!
    my_k <- kclusts

    ## Running kmeans algorithm on red, green and blue value to gather similar colour together
    kmean_rgb <- kmeans(im_df |> dplyr::select(red,green,blue), centers=my_k, iter.max = 25)

    if (kmean_rgb$ifault==4) { kmean_rgb = kmeans(im_df |> dplyr::select(red,green,blue),
                                                  centers=my_k, algorithm="MacQueen",
                                                  iter.max = 25) }

    ## append cluster id to im_df datasets.
    im_df$cluster_num <- kmean_rgb$cluster

    ## center values can be used as cluster colour!
    kmean_center <- kmean_rgb$centers |>
      as.data.frame() |>
      dplyr::mutate(cluster_num = dplyr::row_number()) |>
      dplyr::inner_join(im_df |> dplyr::count(cluster_num)) |>
      dplyr::mutate(image = image,
                    image_base = basename(image)) |>
      dplyr::select(image, image_base, everything())

    color_wide <- kmean_center |>
      dplyr::select(image, image_base, red, green, blue, cluster_num) |>
      tidyr::gather(color, value, -c("image", "image_base", "cluster_num")) |>
      tidyr::unite(color, color, cluster_num) |>
      tidyr::spread(color, value)

    if (return_results) {
      return(color_wide)

    } else if (!is.null(sqlite_db) & !return_results) {
      write_sqlite(color_wide, "color_kmeans", save_file = sqlite_db)

      if (getOption("faceplyr.verbose")) {
        cli::cli_alert(glue::glue(col_orange("Image complete (color kmeans): {savename}")))
      }
    } else {
      stop("must supply sqlite_db or return results TRUE.")
    }
  }
}

#' Extract color histogram metric from interior of face
#'
#' @param image
#' @param data_save_folder
#' @param sqlite_db
#' @param return_results
#'
#' @return
#'
#' @import tidyverse
#' @importFrom tools file_path_sans_ext
#' @import cli
#' @import glue
#' @importFrom fs file_exists
#'
#' @export
extract_hist_colors <- function(image,
                                data_save_folder = data_save_folder,
                                sqlite_db = NULL,
                                return_results = NULL,
                                bins = 32L) {

  if (!is.null(sqlite_db)) {
    img_base <- read_sqlite("meta", save_file = sqlite_db)
    meta_key <- img_base[
      img_base$image_sans_ext == tools::file_path_sans_ext(basename(image)),]

    savename <- fs::path(
      meta_key$cropped_image_path
    )

  } else if(!is.null(data_save_folder)) {
    savename <- fs::path(
      glue::glue("{data_save_folder}/{tools::file_path_sans_ext(basename(image))}_cropped.png")
    )

  } else {
    stop("Must supply a data save folder or database")
  }

  if (fs::file_exists(savename)) {
    shape <- bins

    col <- tryCatch({
      color <- face_hist(savename, shape = c(shape, shape, shape))
      color <- tibble::tibble(color=array(color),
                              number = 1:length(color))

      # color2 <- color %>%
      #   gather(., key, value, -number) %>%
      #   unite(., key, key, number) %>%
      #   spread(., key, value) %>%
      #   mutate(., image = unique(vars$img),
      #          image_base = unique(vars$img_basename),
      #          emotion = vars$emotion) %>%
      #   select(., image, image_base, emotion, everything())

      color2 <- color |>
        tidyr::gather(key, value, -number) |>
        dplyr::mutate(color = dplyr::case_when(
          number <= shape ~ "blue",
          shape < number & number <= shape*2 ~ "green",
          number > shape*2 ~ "red"
        )) |>
        dplyr::group_by(color) |>
        dplyr::mutate(bin = 1:shape) |>
        dplyr::mutate(image = image,
                      image_base = basename(image)) |>
        dplyr::select(image, image_base, everything(), -key)

    }, warning = function(w) {
      message(sprintf("Warning in %s: %s", deparse(w[["call"]]), w[["message"]]))

    }, error = function(e) {
      message(sprintf("Error in %s: %s", deparse(e[["call"]]), e[["message"]]))

    })

    color_hist <- color2 |>
      dplyr::select(image, image_base, color, number, value) |>
      tidyr::unite(color_num, color, number, sep = "_hist_") |>
      tidyr::spread(color_num, value)

    if (!is.null(col)) {
      if (return_results) {
        return(color_hist)

      } else if (!is.null(sqlite_db) & !return_results) {
        write_sqlite(color_hist, "color_hist", save_file = sqlite_db)

        if (getOption("faceplyr.verbose")) {
          cli::cli_alert(glue::glue(col_orange("Image complete (color hist): {savename}")))
        }
      }
    }
  }
}

#' Extract luminance, RGB, and HSV values from cheeks and forhead of face
#'
#' @param image
#' @param data_save_folder
#' @param sqlite_db
#' @param return_results
#' @param buffer
#'
#' @return
#'
#' @import tidyverse
#' @importFrom tools file_path_sans_ext
#' @import cli
#' @import glue
#' @import imager
#'
#' @export
extract_luminance <- function(image,
                              data_save_folder = NULL,
                              sqlite_db = NULL,
                              return_results = FALSE,
                              buffer = 2) {

  if (!is.null(sqlite_db)) {
    img_base <- read_sqlite("meta", save_file = sqlite_db)
    meta_key <- img_base[
      img_base$image_sans_ext == tools::file_path_sans_ext(basename(image)),]

    savename <- fs::path(
      meta_key$cropped_image_path
    )

  } else if(!is.null(data_save_folder)) {
    savename <- fs::path(
      glue::glue("{data_save_folder}/{tools::file_path_sans_ext(basename(image))}_cropped.png")
    )

  } else {
    stop("Must supply a data save folder or database")
  }

  # This requires facial landmarks
  # Look for dlib's points [x,y]:
  ## - Right cheek: [19, (2+30)/2]
  ## - Left cheek: [24, (14+30)/2]
  ## - Forehead: [27, (21+22)/2]

  if (fs::file_exists(savename)) {

    # Get landmarks for left, right cheek, forehead
    landmarks <- faceplyr::read_landmarks(savename)

    right_cheek_y1 <- landmarks[landmarks$point==2,"y"]
    right_cheek_y2 <- landmarks[landmarks$point==30,"y"]
    right_cheek_x <- landmarks[landmarks$point==19,"x"]
    right_cheek <- list(
      x = right_cheek_x,
      y = (right_cheek_y1+right_cheek_y2)/2
    )

    left_cheek_y1 <- landmarks[landmarks$point==14,"y"]
    left_cheek_y2 <- landmarks[landmarks$point==30,"y"]
    left_cheek_x <- landmarks[landmarks$point==24,"x"]
    left_cheek <- list(
      x = left_cheek_x,
      y = (left_cheek_y1+left_cheek_y2)/2
    )

    forehead_y1 <- landmarks[landmarks$point==21,"y"]
    forehead_y2 <- landmarks[landmarks$point==22,"y"]
    forehead_x <- landmarks[landmarks$point==27,"x"]
    forehead <- list(
      x = forehead_x,
      y = (forehead_y1+forehead_y2)/2
    )

    # Load image and extract RGBs
    img <- imager::load.image(savename)

    image_channels <- imager::channels(img)

    if (length(image_channels) == 3 | length(image_channels) == 4) {
      red_layer <- imager::R(img)
      green_layer <- imager::G(img)
      blue_layer <- imager::B(img)

      # red_layer <- image_channels$c.1
      # green_layer <- image_channels$c.2
      # blue_layer <- image_channels$c.3

      # Get point RGB values
      right_cheek_red <- red_layer[seq(right_cheek$x-buffer,right_cheek$x+buffer), seq(right_cheek$y-buffer,right_cheek$y+buffer), ,]
      right_cheek_green <- green_layer[seq(right_cheek$x-buffer,right_cheek$x+buffer), seq(right_cheek$y-buffer,right_cheek$y+buffer), ,]
      right_cheek_blue <- blue_layer[seq(right_cheek$x-buffer,right_cheek$x+buffer), seq(right_cheek$y-buffer,right_cheek$y+buffer), ,]

      left_cheek_red <- red_layer[seq(left_cheek$x-buffer,left_cheek$x+buffer), seq(left_cheek$y-buffer,left_cheek$y+buffer), ,]
      left_cheek_green <- green_layer[seq(left_cheek$x-buffer,left_cheek$x+buffer), seq(left_cheek$y-buffer,left_cheek$y+buffer), ,]
      left_cheek_blue <- blue_layer[seq(left_cheek$x-buffer,left_cheek$x+buffer), seq(left_cheek$y-buffer,left_cheek$y+buffer), ,]

      forehead_red <- red_layer[seq(forehead$x-buffer,forehead$x+buffer), seq(forehead$y-buffer,forehead$y+buffer), ,]
      forehead_green <- green_layer[seq(forehead$x-buffer,forehead$x+buffer), seq(forehead$y-buffer,forehead$y+buffer), ,]
      forehead_blue <- blue_layer[seq(forehead$x-buffer,forehead$x+buffer), seq(forehead$y-buffer,forehead$y+buffer), ,]

      # Convert to HEX codes
      right_cheek_hex <- rgb(mean(right_cheek_red),
                             mean(right_cheek_green),
                             mean(right_cheek_blue)
      )

      left_cheek_hex <- rgb(mean(left_cheek_red),
                            mean(left_cheek_green),
                            mean(left_cheek_blue)
      )

      forehead_hex <- rgb(mean(forehead_red),
                          mean(forehead_green),
                          mean(forehead_blue)
      )

      # Convert to HSL
      right_cheek_hsl <- col2hsl(right_cheek_hex)
      left_cheek_hsl <- col2hsl(left_cheek_hex)
      forehead_hsl <- col2hsl(forehead_hex)

      # Separate and put into a dataframe
      # right_cheek_h <- right_cheek_hsl["H",]
      # right_cheek_s <- right_cheek_hsl["S",]
      # right_cheek_l <- right_cheek_hsl["L",]
      #
      # left_cheek_h <- left_cheek_hsl["H",]
      # left_cheek_s <- left_cheek_hsl["S",]
      # left_cheek_l <- left_cheek_hsl["L",]
      #
      # forehead_h <- forehead_hsl["H",]
      # forehead_s <- forehead_hsl["S",]
      # forehead_l <- forehead_hsl["L",]

      # Combine raw RGB values and lum values
      lum2 <- tibble::tibble(
        image = image,
        image_base = basename(image),
        # RGB
        right_cheek_red = mean(right_cheek_red),
        right_cheek_green = mean(right_cheek_green),
        right_cheek_blue = mean(right_cheek_blue),
        left_cheek_red = mean(left_cheek_red),
        left_cheek_green = mean(left_cheek_green),
        left_cheek_blue = mean(left_cheek_blue),
        forehead_red = mean(forehead_red),
        forehead_green = mean(forehead_green),
        forehead_blue = mean(forehead_blue),
        # HEX
        right_cheek_hex = right_cheek_hex,
        left_cheek_hex = left_cheek_hex,
        forehead_hex = forehead_hex,
        # HSL
        right_cheek_h = right_cheek_hsl["H",],
        right_cheek_s = right_cheek_hsl["S",],
        right_cheek_l = right_cheek_hsl["L",],
        left_cheek_h = left_cheek_hsl["H",],
        left_cheek_s = left_cheek_hsl["S",],
        left_cheek_l = left_cheek_hsl["L",],
        forehead_h = forehead_hsl["H",],
        forehead_s = forehead_hsl["S",],
        forehead_l = forehead_hsl["L",],
        # Luminance
        right_cheek_stand_lum = ((0.2126*mean(right_cheek_red)) + (0.7152*mean(right_cheek_green)) + (0.0722*mean(right_cheek_blue))),
        left_cheek_stand_lum = ((0.2126*mean(left_cheek_red)) + (0.7152*mean(left_cheek_green)) + (0.0722*mean(left_cheek_blue))),
        forehead_stand_lum = ((0.2126*mean(forehead_red)) + (0.7152*mean(forehead_green)) + (0.0722*mean(forehead_blue))),

        right_cheek_perceived_lum = ((0.299*mean(right_cheek_red)) + (0.587*mean(right_cheek_green)) + (0.114*mean(right_cheek_blue))),
        left_cheek_perceived_lum = ((0.299*mean(left_cheek_red)) + (0.587*mean(left_cheek_green)) + (0.114*mean(left_cheek_blue))),
        forehead_perceived_lum = ((0.299*mean(forehead_red)) + (0.587*mean(forehead_green)) + (0.114*mean(forehead_blue)))
      )

    } else {
      # TODO: Grey scale images
      stop("Must supply color image.")
    }

    if (!is.null(lum2)) {
      if (return_results) {
        return(lum2)

      } else if (!is.null(sqlite_db) & !return_results) {
        write_sqlite(lum2, "color_lum", save_file = sqlite_db)

        if (getOption("faceplyr.verbose")) {
          cli::cli_alert(glue::glue(col_orange("Image complete (color luminance): {savename}")))
        }

      } else {
        stop("must supply sqlite_db or return results TRUE.")
      }
    }
  }
}

#' Extract face roundness
#'
#' @param image
#' @param landmarks
#' @param point_select
#' @param fit
#' @param data_save_folder
#' @param sqlite_db
#' @param return_results
#'
#' @return
#' @export
#'
#' @examples
extract_roundness <- function(image = image,
                              landmarks = NULL,
                              point_select = "face_outline",
                              fit = "taubin",
                              data_save_folder = NULL,
                              sqlite_db = NULL,
                              return_results = return_results) {

  if (!is.null(sqlite_db)) {
    img_base <- read_sqlite("meta", save_file = sqlite_db)
    meta_key <- img_base[
      img_base$image_sans_ext == tools::file_path_sans_ext(basename(image)),]

    savename <- fs::path(
      meta_key$cropped_image_path
    )

  } else if(!is.null(data_save_folder)) {
    savename <- fs::path(
      glue::glue("{data_save_folder}/{tools::file_path_sans_ext(basename(image))}_cropped.png")
    )

  } else {
    stop("Must supply a data save folder or database")
  }

  round <- calc_roundness(image = image, point_select = point_select,
                          landmarks = landmarks, fit = fit) |>
    dplyr::mutate(image = image,
                  image_base = basename(image)) |>
    dplyr::select(image, image_base, dplyr::everything())

  if (!is.null(round)) {
    if (return_results) {
      return(round)

    } else if (!is.null(sqlite_db) & !return_results) {
      write_sqlite(round, "roundness", save_file = sqlite_db)

      if (getOption("faceplyr.verbose")) {
        cli::cli_alert(glue::glue(cli::col_red("Image complete (roundness): {savename}")))
      }

    }
  }
}

#' Extract face angularity
#'
#' @param image
#' @param landmarks
#' @param data_save_folder
#' @param sqlite_db
#' @param return_results
#'
#' @return
#' @export
#'
#' @examples
extract_angularity <- function(image = image,
                               landmarks = NULL,
                               data_save_folder = NULL,
                               sqlite_db = NULL,
                               return_results = return_results) {

  if (!is.null(sqlite_db)) {
    img_base <- read_sqlite("meta", save_file = sqlite_db)
    meta_key <- img_base[
      img_base$image_sans_ext == tools::file_path_sans_ext(basename(image)),]

    savename <- fs::path(
      meta_key$cropped_image_path
    )

  } else if(!is.null(data_save_folder)) {
    savename <- fs::path(
      glue::glue("{data_save_folder}/{tools::file_path_sans_ext(basename(image))}_cropped.png")
    )

  } else {
    stop("Must supply a data save folder or database")
  }

  angle <- calc_angularity(image = image, landmarks = landmarks) |>
    dplyr::mutate(image = image,
                  image_base = basename(image)) |>
    dplyr::select(image, image_base, dplyr::everything())

  if (!is.null(angle)) {
    if (return_results) {
      return(angle)

    } else if (!is.null(sqlite_db) & !return_results) {
      write_sqlite(angle, "angularity", save_file = sqlite_db)

      if (getOption("faceplyr.verbose")) {
        cli::cli_alert(glue::glue(cli::col_red("Image complete (angularity): {savename}")))
      }

    } else {
      stop("must supply sqlite_db or return results TRUE.")
    }
  }
}

#' Wrapper function for metric extraction
#'
#' @param image
#' @param data_save_folder
#' @param return_results
#' @param dbname
#' @param landmarks
#' @param return_results
#'
#' @return
#'
#' @export
extract_elements <- function(image,
                             data_save_folder = NULL,
                             dbname = "face_features",
                             return_results = FALSE,
                             landmarks = NULL) {

  # Setup
  # dt <- format(Sys.Date(), "%m-%d-%Y")
  # data_save_folder <- data_save_folder
  # sqlite_db <- get_sqlite_db(data_save_folder = data_save_folder, dbname = glue::glue("{dbname}-{dt}"))
  # write_meta_sqlite(image = image, save_file = sqlite_db)

  sqlite_db <- extract_prep(image = image,
                            data_save_folder = data_save_folder,
                            sqlite_db_name = sbname)

  # Extraction
  extract_structure(image = image, data_save_folder = data_save_folder,
                    sqlite_db = sqlite_db, return_results = return_results)

  extract_texture(image = image, data_save_folder = data_save_folder,
                  sqlite_db = sqlite_db, return_results = return_results)

  extract_kmean_colors(image = image, data_save_folder = data_save_folder,
                       sqlite_db = sqlite_db, return_results = return_results,
                       kclust = 25)

  extract_hist_colors(image = image, data_save_folder = data_save_folder,
                      sqlite_db = sqlite_db, return_results = return_results)

  ## [x] TODO: add luminance function
  extract_luminance(image = image, data_save_folder = data_save_folder,
                    sqlite_db = sqlite_db, return_results = return_results)

  ## [x] TODO: add roundness/angularity
  if (!is.null(landmarks)) {
    landmarks <- landmarks
  }

  extract_roundness(image = image, landmarks = landmarks, data_save_folder = data_save_folder,
                    sqlite_db = sqlite_db, return_results = return_results)

  extract_angularity(image = image, landmarks = landmarks, data_save_folder = data_save_folder,
                     sqlite_db = sqlite_db, return_results = return_results)

  ## [] TODO: add expansion/constriction
  # extract_expand_constrict(image = image, landmarks = landmarks, return_results = return_results
}

