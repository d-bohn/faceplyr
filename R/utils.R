# Create a matrix of face points ----

create_matrix <- function(image, points, ntimes){

  points2 <- points %>%
    dplyr::select(., x,y,point) %>%
    dplyr::mutate(., x = as.numeric(as.character(x)),
                  y = as.numeric(as.character(y)),
                  point = as.numeric(as.character(point)))

  img <- try(jpeg::readJPEG(image))
  if (class(img) == 'try-error') img <- png::readPNG(image)
  if (class(img) == 'try-error') message('Please supply either a .jpg or .png image.')

  dims <- dim(img)
  a <- array(0L, dims)
  b <- as.matrix(points2[1:2])
  a[b] <- 1

  a <- rotate(a)

  points_rotated <- RSAGA::grid.to.xyz(a)

  points_rotated <- points_rotated[points_rotated$z != 0,]
}

# Resize face coordinate points ----
resize_points <- function(coords, image, width, height){

  info_o <- magick::image_info(magick::image_read(coords$image[1]))

  if(!missing(image)){
    info_n <- magick::image_info(magick::image_read(image))
  } else if(!missing(width) && !missing(height)){
    info_n <- data.frame(width=width,height=height)
  } else{
    message('No image or width/height specifications provided. Defaulting to 512x512')
    info_n <- data.frame(width=512L, height=512L)
  }

  old_w = info_o$width
  old_h = info_o$height
  new_w = info_n$width
  new_h = info_n$height

  d = data.frame( x_new=rep(0, nrow(coords)), y_new=rep(0,nrow(coords)))

  for (i in 1:nrow(coords)){
    x <- (new_w*coords[i,'x'])/old_w
    y <- (new_h*coords[i,'y'])/old_h
    d[i,] <- c(x,y)
  }
  return(coords <- cbind(coords,d))
}

# Plot face land marks ----
<<<<<<< HEAD
#' @export
plot_landmarks <- function(landmarks=NULL, image, save=NULL) {

  if (hasArg(landmarks) & hasArg(image)) {
    landmarks <- faceplyr::read_landmarks(landmarks)

  } else if (!(hasArg(landmarks)) & hasArg(image)) {
    landmarks <- faceplyr::read_landmarks(image)[c('point','x','y')]
  }

=======
plot_landmarks_old <- function(landmarks=NULL, image, save=NULL) {

  if (hasArg(landmarks) & hasArg(image)) {
    landmarks <- faceplyr::read_landmarks(landmarks)
  } else (!(hasArg(landmarks)) & hasArg(image)) {
    landmarks <- faceplyr::read_landmarks(image)[c('point','x','y')]
  }

  # img <- try(jpeg::readJPEG(image))
  # if (class(img) == 'try-error') img <- png::readPNG(image, native = TRUE)
  # if (class(img) == 'try-error') message('Please supply either a .jpg or .png image.')
  #
  # img <- readPNG(image, TRUE)
  # g <- grid::rasterGrob(img, interpolate=TRUE)
  # # g$width <- unit(1, "npc")
  # # g$height <- unit(1, "npc")
  #
  # g <- grid::grid.raster(img@.Data)
  # dim(img)
  # data <- coords[c('x_new','y_new')]
  # #data <- apply(data, 2, rev)
  # data <- as.data.frame(data) %>% mutate(., point=1:nrow(.),
  #                                        x=as.numeric(x),
  #                                        y=as.numeric(y))
  #
  #
  # library(ggplot2);library(grid)
  # plot <- ggplot(data, aes(x = x_new, y = y_new, label=point)) +
  #   annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  #   geom_point(show.legend=FALSE, na.rm = TRUE, aes(colour='red')) +
  #   geom_text() +
  #   theme_bw() +
  #   ylim(512,0) +
  #   xlim(0,512) +
  #   theme(
  #     panel.background = element_rect(fill = "transparent", colour = NA),
  #     panel.grid.minor = element_blank(),
  #     panel.grid.major = element_blank(),
  #     axis.title.x=element_blank(),
  #     axis.text.x=element_blank(),
  #     axis.ticks.x=element_blank(),
  #     axis.title.y=element_blank(),
  #     axis.text.y=element_blank(),
  #     axis.ticks.y=element_blank()
  #   )
  # plot

  # library(raster);library(dplyr)
>>>>>>> d8cfe55b622a8ca83495d2593a1910643ac4bb6b
  img <- EBImage::readImage(image)

  res = dim(img)[1:2]
  plot(1,1,xlim=c(0,res[1]),ylim=c(res[2],0),asp=1,type='n',xaxs='i',yaxs='i',
       xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  graphics::rasterImage(img,1,1,res[1],res[2])
  points(landmarks[2:3], pch = 20, col='red')
<<<<<<< HEAD
=======
}

#' @export
plot_landmarks <- function(landmarks=NULL, image, save=NULL) {

  if (hasArg(landmarks) & hasArg(image)) {
    landmarks <- faceplyr::read_landmarks(landmarks)
  } else (!(hasArg(landmarks)) & hasArg(image)) {
    landmarks <- faceplyr::read_landmarks(image)[c('point','x','y')]
  }

  img <- EBImage::readImage(image)

  res = dim(img)[1:2]
  plot(1,1,xlim=c(0,res[1]),ylim=c(res[2],0),asp=1,type='n',xaxs='i',yaxs='i',
       xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  graphics::rasterImage(img,1,1,res[1],res[2])
  points(landmarks[2:3], pch = 20, col='red')
>>>>>>> d8cfe55b622a8ca83495d2593a1910643ac4bb6b
}
