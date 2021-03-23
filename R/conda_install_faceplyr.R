#' @importFrom ROpenCVLite isOpenCVInstalled
#' @importFrom reticulate conda_binary conda_create conda_list conda_install
#' @export
conda_install_faceplyr <- function(pyV="3.8") {
  tmp <- try(system2(reticulate::conda_binary(), "--version", stdout = TRUE))

  if (class(tmp) == "try-error") {
    stop("Conda not isntalled. Install with `reticulate::install_miniconda()`")
  }

  if (!("faceplyr" %in% reticulate::conda_list()$name)) {
    reticulate::conda_create(envname = "test",
                             packages = glue::glue("python={pyV}")
                             )
  }

  install_opencv(build = "conda")

  requirements <- read.system.file("extdata", "requirements.txt", package = "faceplyr")

  reticulate::conda_install(
    envname = "faceplyr",
    pip = TRUE,
    pip_options = glue::glue("install -r {requirements}")
  )

  ROpenCVLite::isOpenCVInstalled()
}
