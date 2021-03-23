# https://stackoverflow.com/questions/63241608/install-opencv-from-source-to-conda-environment
# https://jayrobwilliams.com/files/html/OpenCV_Install.html

# Build opencv to conda env

install_opencv <- function(build = c("conda", "package"), conda_environment = "faceplyr") {
  
  if (build == "conda") {
    pkgPath <- tryCatch({
      find.package("ROpenCVLite")
    }, warning = function(w) {
      message(sprintf("Warning in %s: %s", deparse(w[["call"]]), w[["message"]]))
      
    }, error = function(e) {
      message("Need package ROpenCVLite: `install.packages('ROpenCVLite')`")
      
    })
    
    installPath <- gsub("ROpenCVLite", "", pkgPath)
    openCVPath <- paste0(installPath, "opencv")
    dir.create(openCVPath, showWarnings = FALSE)
    tmpDir <- base::tempdir()
    dir.create(tmpDir, showWarnings = FALSE)
    utils::download.file("https://github.com/opencv/opencv/archive/4.5.0.zip", 
                         paste0(tmpDir, "/opencv-4.5.0.zip"))
    utils::unzip(paste0(tmpDir, "/opencv-4.5.0.zip"), 
                 exdir = tmpDir)
    file.copy(paste0(pkgPath, "/OpenCVModule.4.5.0.cmake"), 
              paste0(tmpDir, "/opencv-4.5.0/cmake/OpenCVModule.cmake"), 
              overwrite = TRUE)
    sourceDir <- paste0(tmpDir, "/opencv-4.5.0/")
    buildDir <- paste0(sourceDir, "build")
    dir.create(buildDir, showWarnings = FALSE)
    
    pyV <- "3.8"
    pyv <- gsub("\\.","",pyV)
    system <- tolower(Sys.info()["sysname"])
    
    ctmp <- as.data.frame(reticulate::conda_list())
    conda_env <- gsub("/bin/python" , "", ctmp[ctmp$name == conda_environment, "python"])
    
    if (system == "darwin" | system == "linux") {
      cmd1 <- glue::glue("export CPLUS_INCLUDE_PATH={conda_env}/lib/python3 && cmake -DWITH_IPP=ON -DBUILD_opencv_world=OFF -DBUILD_opencv_contrib_world=OFF -DBUILD_opencv_matlab=OFF -DBUILD_opencv_java=OFF -DBUILD_opencv_python2=OFF -DOPENCV_EXTRA_MODULES_PATH={sourceDir}/modules -DBUILD_opencv_python3=ON -DCMAKE_INSTALL_PREFIX={openCVPath} -DPYTHON3_LIBRARY={conda_env}/lib/python{pyV}.dylib -DPYTHON3_INCLUDE_DIR={conda_env}/include/python{pyV} -DPYTHON3_EXECUTABLE={conda_env}/bin/python -DPYTHON3_PACKAGES_PATH={conda_env}/lib/python{pyV}/sit -DWITH_OPENMP=OFF -DBUILD_PERF_TESTS=OFF -DBUILD_TESTS=OFF -DINSTALL_CREATE_DISTRIB=ON -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX={openCVPath} -B{buildDir} -H{sourceDir}")
      cmd2 <- paste0("make -j", parallel::detectCores()-1, " -C ", buildDir)
      cmd3 <- paste0("make -C ", buildDir, " all install")
      
      system(cmd1)
      system(cmd2)
      system(cmd3)
      
      file.symlink(glue::glue("{conda_env}/lib/python{pyV}/sit/cv2/python-{pyV}/cv2.cpython-{pyv}-{system}.so"),
                   glue::glue("{conda_env}/lib/python{pyV}/site-packages/cv2.so"))
    } else {
      stop("No support for Windows conda yet. Try `install_opencv(build = 'package')`")
    }
    
  } else if (build == "package") {
    message("Untested. May not link to conda cv bindings")
    ROpenCVLite::installOpenCV(batch = TRUE)
    
  } else {
    stop("build must be one of c('conda','package')")
  }
}



