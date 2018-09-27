optical_flow <- function(target, reference, options = 'default', vis=TRUE){

  if(exists('db')==TRUE){
    #### FOR DEGUGGING NOT RUN
    target = here::here('inst/extdata/S001.Post.Anger.jpg')
    reference = here::here('inst/extdata/S001.Pre.Anger.jpg')
    output = here::here('test')
    save = stringr::str_extract(target, 'S\\d{3}')
    python = NULL
    options = 'default'
    ### END DEBUGGING NOT RUN
  }

  py_loc <- reticulate::use_python(quantIm::find_python(python), required = TRUE)
  reticulate::py_discover_config(required_module = 'pyflow')

  main <- reticulate::import_main()
  bi <- reticulate::import_builtins()
  np <- reticulate::import('numpy')
  pil <- reticulate::import('PIL.Image')
  pd <- reticulate::import('pandas')
  pyflow <- reticulate::import('pyflow', convert = FALSE)

  im1 = np$array(pil$open(reference))
  if( length(dim(im1)) != 3 ){
    im1 = np$expand_dims(im1, axis = 2L)
  }

  im2 = np$array(pil$open(target))
  if( length(dim(im2)) != 3 ){
    im2 = np$expand_dims(im2, axis = 2L)
  }

  im1 = im1 / 255L
  im2 = im2 / 255L

  im1_py <- reticulate::r_to_py(im1)
  im2_py <- reticulate::r_to_py(im2)

  im1_py = im1_py$copy(order='C')
  im2_py = im2_py$copy(order='C')
  # reticulate::array_reshape(im1_py, c(480,428), order = 'C')

  default_options <- list(
    # Flow Options:
    alpha = 0.012,
    ratio = 0.75,
    minWidth = 20L,
    nOuterFPIterations = 7L,
    nInnerFPIterations = 1L,
    nSORIterations = 30L,
    colType = 1L  # 0 or default:RGB, 1:GRAY (but pass gray image with shape (h,w,1))
  )

  option_names <- c('alpha','ratio','minWidth','nOuterFPIterations','nInnerFPIterations',
                    'nSORIterations','colType')

  if (class(options) != 'list' && options == 'default'){

    invisible(
      lapply(seq_along(default_options),
             function(x) {
               assign(option_names[x], default_options[[x]], envir=.GlobalEnv)
             }
      )
    )

  } else{

    for (i in seq_along(option_names)){
      if (option_names[[i]] %in% names(options)){
        default_options[[option_names[[i]]]] <- options[[option_names[[i]]]]
      } else{
        default_options[[option_names[[i]]]] <- default_options[[option_names[[i]]]]
      }
    }
    invisible(
      lapply(seq_along(default_options),
             function(x) {
               assign(option_names[x], default_options[[x]], envir=.GlobalEnv)
             }
      )
    )
  }

  pyflowPy <- pyflow$coarse2fine_flow(
    im1_py, im2_py, alpha, ratio, minWidth,
    nOuterFPIterations, nInnerFPIterations,
    nSORIterations, colType)

  pyflowR <- reticulate::py_to_r(pyflowPy)
  flowR <- abind::abind(pyflowR[[1]],pyflowR[[2]], along = 3)
  im2W <- pyflowR[[3]]
  # rotate <- function(x) t(apply(x, 2, rev)

  np$save(paste0(output,'/',save,'.npy'),
          reticulate::r_to_py(flowR))

  if ( viz == TRUE){

    script <- system.file("python", "vis_of.py", package = "quantIm")
    flow <-  paste0(output,'/',save,'.npy')
    command1 <- paste(py_loc, script,
                      '-r', reference,
                      '-s', save,
                      '-o', output,
                      '-f', flow,
                      sep = ' ')

    try(
      system(command1, intern = TRUE),
      silent = TRUE
    )

  }

  flow_list <- list(flow=pyflowR, flowR = florR, warped_image = im2W)
  return(flow_list)

}




# show_of <- function(flow,im2w){
#   cv2 <- reticulate::import('cv2')
#   hsv = np$zeros(im1_py$shape, dtype=np$uint8)
#   hsv2 <- r_to_py(hsv)
#   hsv[, , 1] = 255
#   flowR_Py <- reticulate::r_to_py(flowR)
#   mag_ang = cv2$cartToPolar(flowR[,1,], flowR[,2,])
#   hsv[,,1] = mag_ang[[2]] * 180 / np$pi / 2
#
#   cv2$imwrite(paste0(output,'/',save,'_flow.png'), hsv2)
# }

python_of <- function(reference, target, output, save, python=NULL){

  if(exists('db')==TRUE){
    #### FOR DEGUGGING NOT RUN
    target = here::here('inst/extdata/S001.Post.Anger.jpg')
    reference = here::here('inst/extdata/S001.Pre.Anger.jpg')
    output = here::here('test')
    save = stringr::str_extract(target, 'S\\d{3}')
    python = NULL
    ### END DEBUGGING NOT RUN
  }

  py_loc <- quantIm::find_python(python)

  ## Try to get try to get the script form the inst/python folder
  script <- system.file("python", "python_of.py", package = "quantIm")
  command1 <- paste(py_loc, script,
                    '-r', reference,
                    '-t', target,
                    '-o', output,
                    '-s', save,
                    '-v',
                    sep = ' ')

  try(
    system(command1, intern = TRUE),
    silent = TRUE
  )


}

face_crop_eyes <- function(coords=NULL, image, points = c(36,45), savename = NULL,
                           dist = '116', wh = 512, w=235, h = 135){

  if(exists('db')==TRUE){
    #### FOR DEGUGGING NOT RUN
    base <- '/Users/dalbohn/Documents/R_packages/quantIm/inst/extdata'
    file <- "nm0000114_rm129141504_1957-12-13_2009.jpg"
    file <- "nm0000100_rm46373120_1955-1-6_2003.jpg"
    image <- file.path(base,file)
    condaenv = 'quantIm'
    coords <- read_landmarks(image)
    dist = 116
    wh = 512;w=235;h=135
    points = c(36,45)
    savename = NULL
    ### END DEBUGGING NOT RUN
  }

  if (is.null(coords)==TRUE){
    coords <- read_landmarks(image=image)
  }

  # Check if it we have numeric values
  coords$x <- as.numeric(as.character(coords$x))
  coords$y <- as.numeric(as.character(coords$y))

  # Get a proportion to scale by
  point1=37;point2=41
  point3=38;point4=40
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
  left_pupil <- centerC(37,40,38,41)
  ## Estimate center of right pupil
  right_pupil <- centerC(43,46,44,47)

  # See what we need to scale it by scale
  pupily <- (left_pupil[2]+right_pupil[2])/2
  pupilx <- (left_pupil[1]+right_pupil[1])/2
  pupil_dist <- max(left_pupil[1],right_pupil[1]) - min(left_pupil[1],right_pupil[1])

  scale <- as.numeric(as.character(dist)) / as.numeric(as.character(pupil_dist))

  # Got to check if scale, wh, and dist are compatible
  dims <- as.numeric(dim(EBImage::readImage(image)))
  if( min(dims[1],dims[2])*scale <= wh){
    scale <- as.numeric(as.character(wh))/min(dims[1],dims[2])
  }

  # Get center coordinates
  # centerx <- round((coords$x[points[1]]))*scale
  # centery <- round((coords$y[points[2]]))*scale

  # Load and rescale
  percent <- scale*100
  img <- magick::image_read(image)
  img <- magick::image_scale(img, paste0(percent,'%'))

  # Now crop, fool, crop!
  if (is.null(wh)==TRUE) {
    wh <- as.numeric(dim(EBImage::readImage(image))[1])/2
    message('The image cropping will default to half of its width')
  }

  wh2 <- paste(w,h,sep = 'x')
  w_h_x_y <- paste0(wh2,'+',(pupilx-(as.numeric(wh)/2)),'+',(pupily-(as.numeric(wh)/2)))
  w_h_x_y <- paste0(wh2,'+',pupilx,'+',pupily)
  im <- magick::image_crop(img, w_h_x_y)
  magick::image_browse(im)

  if (is.null(savename)==TRUE) {
    image_sans <- tools::file_path_sans_ext(image)
    savename <- paste0(image_sans,'_crop_scale.png')
  }

  attempt1 <- try( magick::image_write(im, path = savename, format = "png"), silent = TRUE)

  if (class(attempt1)=='try-error') {
    savename <- paste0(image_sans,'_crop_scale.jpg')
    attempt2 <- try( magick::image_write(im, path = savename, format = "jpg"), silent = TRUE )
    if (class(attempt2)=='try-error') message('Please provide an appropriate image type to be written.')
  }
}
