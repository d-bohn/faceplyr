#' Extract structure metric from interior of the face
#'
#' @param image
#' @param data_save_folder
#' @param return_results
#'
#' @return
#' @export
#'
#' @examples
#' @import tidyverse
#' @importFrom tools file_path_sans_ext
#' @import cli
#' @import glue
extract_structure <- function(image, data_save_folder, return_results = FALSE) {
  vars <- extract_vars(image,
                       data_save_folder = data_save_folder)

  # TODO: Add option to not mask face
  # mask <- tempfile(fileext='.png')
  mask <- tryCatch({
    face_mask(image = image, savename = vars$savename, crop = TRUE, transparent = TRUE)

  }, warning = function(w) {
    message(sprintf("Warning in %s: %s", deparse(w[["call"]]), w[["message"]]))

  }, error = function(e) {
    message(sprintf("Error in %s: %s", deparse(e[["call"]]), e[["message"]]))
  })

  if (!is.null(mask)) {

    # ** Extract keypoints ----
    struct <- tryCatch({
      mask_points <- faceplyr::read_landmarks(vars$savename)

      structure <- mask_points %>%
        as_tibble(.) %>%
        select(., point, x, y) %>%
        gather(., key, value, -point) %>%
        unite(., key, key, point) %>%
        spread(., key, value) %>%
        mutate(.,
               image = image,
               image_base = basename(image)) %>%
        select(., image, image_base, everything())

    }, warning = function(w) {
      message(sprintf("Warning in %s: %s", deparse(w[["call"]]), w[["message"]]))

    }, error = function(e) {
      message(sprintf("Error in %s: %s", deparse(e[["call"]]), e[["message"]]))

    })

    if (!is.null(struct)) {

      if (return_results) {
        return(structure)

        } else {
          saveRDS(struct, paste0(data_save_folder, "/",
                                 tools::file_path_sans_ext( vars$img_basename), "_structure.RDS"))

          cli::cli_alert(glue::glue(cli::col_blue("Image complete (structure): {vars$savename}")))
      }
    }
  }
}

#' Extract texture metric from interior face
#'
#' @param image
#' @param data_save_folder
#' @param return_results
#'
#' @return
#' @export
#'
#' @examples
#' @import tidyverse
#' @importFrom tools file_path_sans_ext
#' @import cli
#' @import glue
extract_texture <- function(image, data_save_folder, return_results = FALSE) {
  vars <- extract_vars(image,
                       data_save_folder = data_save_folder)

  if (fs::file_exists(vars$savename)) {

    tex <- tryCatch({
      halve_face(vars$savename)

      bot_img <- paste0(tools::file_path_sans_ext(vars$savename),"_bottom.",tools::file_ext(vars$savename))
      top_img <- paste0(tools::file_path_sans_ext(vars$savename),"_top.",tools::file_ext(vars$savename))

      remove_background(bot_img, savename = bot_img, return_img = FALSE)
      remove_background(top_img, savename = top_img, return_img = FALSE)

      texture_b <- face_texture(bot_img)
      texture_t <- face_texture(top_img)

      texture <- tibble(tex_bottom=array(texture_b),
                        tex_top=array(texture_t),
                        number = 1:13)

      texture2 <- texture %>%
        gather(., key, value, -number) %>%
        unite(., key, key, number) %>%
        spread(., key, value) %>%
        mutate(.,
               image = unique(vars$img),
               image_base = unique(vars$img_basename),
               emotion = vars$emotion
        ) %>%
        select(., image, image_base, emotion, everything())

    }, warning = function(w) {
      message(sprintf("Warning in %s: %s", deparse(w[["call"]]), w[["message"]]))

    }, error = function(e) {
      message(sprintf("Error in %s: %s", deparse(e[["call"]]), e[["message"]]))

    })

    if (!is.null(tex)) {

       if (return_results) {
        return(texture2)

        } else {
          saveRDS(texture2, paste0(data_save_folder, "/",
                                   tools::file_path_sans_ext( vars$img_basename), "_texture.RDS"))

          cli::cli_alert(glue::glue(cli::col_green("Image complete (texture): {vars$savename}")))
      }
    }
  }
}

#' Extract kmeans color metric from interior face
#'
#' @param image
#' @param data_save_folder
#' @param return_results
#' @param kclusts
#'
#' @return
#' @export
#'
#' @examples
#' @import tidyverse
#' @importFrom imager load.image grayscale
#' @importFrom tools file_path_sans_ext
#' @import cli
#' @import glue
extract_kmean_colors <- function(image, data_save_folder, return_results = FALSE,
                                 kclusts = 25) {

  vars <- extract_vars(image, data_save_folder = data_save_folder)

  if (fs::file_exists(vars$savename)) {
    # Load up the image using load.image function!
    im <- load.image(vars$savename)

    folder <- data_save_folder

    df_size <-dim(im)[1]*dim(im)[2]
    max_row_num <- 150000 ## number of maximum rows

    ## If df is too big, it's too slow to process on my computer, so shrink the image
    shrink_ratio  <- if(df_size > max_row_num) {max_row_num / df_size } else {1}
    im <- im %>% imresize(shrink_ratio)

    ## get RGB value at each pixel
    im_rgb <- im %>%
      as.data.frame(wide="c") %>%
      rename(red=c.1,green=c.2,blue=c.3) %>%
      mutate(hexvalue = rgb(red,green,blue)) ## you can create hexvalue using red, green blue value!

    ## turn image into Grayscale and get luminance "value" too.
    im_gray <- im %>%
      {if (dim(im)[[4]] != 3) {
        rm.alpha(im)
      }} %>%
      grayscale() %>%
      as.data.frame()

    ## combine RGB info and Luminance Value Dataset together.
    im_df <- im_rgb %>%
      inner_join(im_gray)

    ## Pick k value to run kMean althorithm.
    ## But to extract colours, I'd pick k as number I want back!
    my_k <- kclusts

    ## Running kmeans algorithm on red, green and blue value to gather similar colour together
    kmean_rgb <- kmeans(im_df %>% select(red,green,blue), centers=my_k, iter.max = 25)

    if (kmean_rgb$ifault==4) { kmean_rgb = kmeans(im_df %>% select(red,green,blue),
                                                  centers=my_k, algorithm="MacQueen",
                                                  iter.max = 25) }

    ## append cluster id to im_df datasets.
    im_df$cluster_num <- kmean_rgb$cluster

    ## center values can be used as cluster colour!
    kmean_center <- kmean_rgb$centers %>% as.data.frame() %>%
      mutate(cluster_num = row_number()) %>%
      inner_join(im_df %>% count(cluster_num)) %>%
      mutate(., image_base = vars$img_basename,
             path = vars$img) %>%
      select(., image_base, everything())

    if (return_results) {
      return(kmeans_center)
      } else {
        saveRDS(kmean_center, paste0(data_save_folder, "/",
                                     tools::file_path_sans_ext( vars$img_basename ), "_color.RDS"))
        # saveRDS(kmean_center, file.path(folder, savename))

        cli::cli_alert(glu::glue(col_orange("Image complete (color kmeans): {vars$savename}")))
    }
  }
}

#' Extract color histogram metric from interior of face
#'
#' @param image
#' @param data_save_folder
#' @param return_results
#'
#' @return
#' @export
#'
#' @examples
#' @import tidyverse
#' @importFrom tools file_path_sans_ext
#' @import cli
#' @import glue
#' @importFrom fs file_exists
extract_hist_colors <- function(image, data_save_folder, return_results = FALSE) {
  vars <- extract_vars(image,data_save_folder = data_save_folder)

  if (fs::file_exists(vars$savename)) {
    col <- tryCatch({
      color <- face_hist(vars$savename, shape = c(5,5,5))
      color <- tibble(color=array(color),
                      number = 1:length(color))

      color2 <- color %>%
        gather(., key, value, -number) %>%
        unite(., key, key, number) %>%
        spread(., key, value) %>%
        mutate(., image = unique(vars$img),
               image_base = unique(vars$img_basename),
               emotion = vars$emotion) %>%
        select(., image, image_base, emotion, everything())

    }, warning = function(w) {
      message(sprintf("Warning in %s: %s", deparse(w[["call"]]), w[["message"]]))
    }, error = function(e) {
      message(sprintf("Error in %s: %s", deparse(e[["call"]]), e[["message"]]))

    })

    if (!is.null(col)) {

      if (return_results) {
        return(col)

      } else {
        # color_metrics_train <- rbind(color_metrics_train, color2)
        saveRDS(col,
                paste0(data_save_folder, "/",
                       tools::file_path_sans_ext( vars$img_basename ), "_color_hist.RDS"))

        cli::cli_alert(glue::glue(col_orange("Image complete (color hist): {vars$savename}")))
      }
    }
  }
}

#' Extract luminance, RGB, and HSV values from cheeks and foreead of face
#'
#' @param image
#' @param data_save_folder
#' @param return_results
#' @param buffer
#'
#' @return
#' @export
#'
#' @examples
#' @import tidyverse
#' @importFrom tools file_path_sans_ext
#' @import cli
#' @import glue
#' @import imager
extract_luminance <- function(image, data_save_folder, return_results = FALSE, buffer = 2) {
  vars <- extract_vars(image, data_save_folder = data_save_folder)

  # This requires facial landmarks
  # Look for dlib's points [x,y]:
  ## - Right cheek: [19, (2+30)/2]
  ## - Left cheek: [24, (14+30)/2]
  ## - Forehead: [27, (21+22)/2]

  if (fs::file_exists(vars$savename)) {

    # Get landmarks for left, right cheek, forehead
    landmarks <- faceplyr::read_landmarks(vars$savename)

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
    image <- imager::load.image(vars$savename)

    image_channels <- imager::channels(image)

    if (length(image_channels) == 3 | length(image_channels) == 4) {
      red_layer <- imager::R(image)
      green_layer <- imager::G(image)
      blue_layer <- imager::B(image)

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
      lum2 <- tibble(
        image = vars$img,
        image_base = vars$img_basename,
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

      } else {
        saveRDS(lum2,
                paste0(data_save_folder, "/",
                       tools::file_path_sans_ext( vars$img_basename ), "_color_lum.RDS"))

        cli::cli_alert(glue::glue(col_orange("Image complete (color luminance): {vars$savename}")))
      }
    }
  }
}

#' Wrapper function for metric extraction
#'
#' @param image
#' @param data_save_folder
#' @param return_results
#'
#' @return
#' @export
#'
#' @examples
extract_elements <- function(image, data_save_folder = "./data", return_results = FALSE) {

  extract_structure(image = image, data_save_folder = data_save_folder,
                    return_results = return_results)

  extract_texture(image = image, data_save_folder = data_save_folder,
                  return_results = return_results)

  extract_kmean_colors(image = image, data_save_folder = data_save_folder,
                       return_results = return_results, kclust = 25)

  extract_hist_colors(image = image, data_save_folder = data_save_folder,
                      return_results = return_results)

  ## [x] TODO: add luminance function
  extract_luminanceee(image = image, data_save_folder = data_save_folder,
                      return_results = return_results)
}

