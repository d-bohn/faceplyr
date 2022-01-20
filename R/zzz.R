#' @export
fp <- NULL


faceplyr_default_options <- list(
  faceplyr.verbose = TRUE
)

.onLoad <- function(libname, pkgname) {
  if (Sys.info()["machine"] == "arm64") {
    # warning("WARNING: arm64 device detected. Attempting auto-setup...")

    m1arm_startup_helper()

  } else if ((Sys.info()["machine"] != "arm64")) {
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

  } else {
    if (!("faceplyr" %in% reticulate::conda_list()$name)) {
      stop("In order to use this package you must install a conda environment named `faceplyr`")
    }
  }

  op <- options()
  toset <- !(names(faceplyr_default_options) %in% names(op))
  if (any(toset)) options(faceplyr_default_options[toset])

  invisible()

}
