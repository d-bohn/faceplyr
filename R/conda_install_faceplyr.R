#' @importFrom ROpenCVLite isOpenCVInstalled
#' @importFrom reticulate conda_binary conda_create conda_list conda_install
#' @export
conda_install_faceplyr <- function(pyV="3.8") {
  tmp <- try(system2(reticulate::conda_binary(), "--version", stdout = TRUE))

  if (class(tmp) == "try-error") {
    stop("Conda not isntalled. Install with `reticulate::install_miniconda()`")
  }

  if (!("faceplyr" %in% reticulate::conda_list()$name)) {
    reticulate::conda_create(envname = "faceplyr",
                             packages = glue::glue("python={pyV}")
                             )
  }

  requirements <- system.file("extdata", "requirements.txt", package = "faceplyr")

  reticulate::conda_install(
    packages = NULL,
    envname = "faceplyr",
    pip = TRUE,
    pip_options = glue::glue("install -r {requirements}")
  )

  install_opencv_source(batch = TRUE, conda_environment = "faceplyr")

  if (ROpenCVLite::isOpenCVInstalled()) {
    tmp <- utils::menu(c("yes", "no"), title = "Success! Would you like to install Rvision now?")

    if (tmp == 1) {
      remotes::install_github("d-bohn/Rvision")

    } else {
      stop()
    }
  }

}
