library(faceplyr)
image <- system.file("extdata", "queen.png", package = "faceplyr")

df <- read_landmarks(x = image)

sn <- tempfile("fp-test", fileext = ".png")
# sn <- "/Users/dalbohn/Desktop/output/images/test.png"

dsf <- "/Users/dalbohn/Desktop/output/fp"

fs <- extract_structure(image = image, data_save_folder = dsf, return_results = TRUE)
ft <- extract_texture(image = image, data_save_folder = dsf, return_results = TRUE)
fch <- extract_hist_colors(image = image, data_save_folder = dsf, return_results = TRUE)
fck <- extract_kmean_colors(image = image, data_save_folder = dsf, return_results = TRUE)
fl <- extract_luminance(image = image, data_save_folder = dsf, return_results = TRUE)
fr <- extract_roundness(image = image, data_save_folder = dsf, return_results = TRUE)
fa <- extract_angularity(image = image, data_save_folder = dsf, return_results = TRUE)
# fec <- calc_expanse_constrict(image = image, data_save_folder = dsf, return_results = TRUE)

verbose = TRUE
extract_elements(
  image = image,
  data_save_folder = dsf
)

