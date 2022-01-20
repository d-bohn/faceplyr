install_opencv_source <- function(batch = TRUE, conda_environment = "faceplyr") {

  pyV <- "3.8"
  pyv <- gsub("\\.","",pyV)
  system <- tolower(Sys.info()["sysname"])

  ctmp <- as.data.frame(reticulate::conda_list())
  conda_env <- gsub("/bin/python" , "", ctmp[ctmp$name == conda_environment, "python"])

  install <- 0

  if (interactive()) {
    if (isOpenCVInstalled()) {
      pkgVersion <- paste0(strsplit(as.character(utils::packageVersion("ROpenCVLite")), "\\.")[[1]][1:2], collapse = "")
      cvVersion <- gsub("\\D+", "", opencvVersion())

      if (pkgVersion == cvVersion) {
        install <- utils::menu(c("yes", "no"), title = "OpenCV is already installed on this system. Would you like to reinstall it now? This will take several minutes.")
      } else {
        install <- utils::menu(c("yes", "no"), title = "A new version of OpenCV is available. Would you like to install it now? This will take several minutes.")
      }
    } else {
      install <- utils::menu(c("yes", "no"), title = "OpenCV is not installed on this system. Would you like to install it now? This will take several minutes.")
    }
  } else {
    if (batch) {
      warning("OpenCV being installed in non-interactive mode!")
      install <- 1
    } else {
      warning("OpenCV can only be installed in interactive mode or if the batch mode is activated.")
    }
  }

  if (!isCmakeInstalled())
    install <- 0

  if (install == 1) {
    pkgPath <- find.package("ROpenCVLite")
    installPath <- gsub("ROpenCVLite", "", pkgPath)
    openCVPath <- paste0(installPath, "opencv")
    message(paste0("OpenCV will be installed in ", openCVPath))

    if (dir.exists(openCVPath)) {
      message("Removing old OpenCV installation.")
      unlink(openCVPath, recursive = TRUE)
    }

    Sys.setenv(CXX_STD = "CXX11")

    if (.Platform$OS.type == "windows") {
      dir.create(openCVPath, showWarnings = FALSE)
      tmpDir <- gsub("\\\\", "/", base::tempdir())
      dir.create(tmpDir, showWarnings = FALSE)

      utils::download.file("https://github.com/opencv/opencv/archive/4.5.2.tar.gz",
                           paste0(tmpDir, "/opencv-4.5.2.tar.gz"))
      utils::untar(paste0(tmpDir, "/opencv-4.5.2.tar.gz"),
                   exdir = tmpDir)

      arch <- c("64", "32")
      archAvail <- c(dir.exists(paste0(R.home(), "/bin/x64")),
                     dir.exists(paste0(R.home(), "/bin/i386")))

      if (any(archAvail)) {
        if (R.Version()$major >= "4") {
          rtools4 <- TRUE
          rtools4Path <- gsub("\\\\", "/", gsub("/usr/bin", "", pkgbuild::rtools_path()))

        } else {
          rtools4 <- FALSE
          chk_rtools <- utils::capture.output(pkgbuild::check_rtools(debug = TRUE))
          rtoolsPath <- gsub(" ", "", gsub("install_path: ", "", chk_rtools[grepl("install_path", chk_rtools)]))
        }

        for (i in 1:2) {
          if (archAvail[i] == TRUE) {
            sourceDir <- paste0(tmpDir, "/opencv-4.5.2/")
            buildDir <- paste0(sourceDir, "build", arch[i])
            dir.create(buildDir, showWarnings = FALSE)
            openCVArch <- if (arch[i] == 64) "x64" else "x86"

            if (rtools4) {
              gcc_path <- paste0(rtools4Path, "/mingw", arch[i], "/bin/gcc.exe")
              gpp_path <- paste0(rtools4Path, "/mingw", arch[i], "/bin/g++.exe")
              windres_path <- paste0(rtools4Path, "/mingw", arch[i], "/bin/windres.exe")
              make_path <- paste0(rtools4Path, "/usr/bin/make.exe")

              system(paste0('cmake -G "Unix Makefiles" -DCMAKE_C_COMPILER=', gcc_path, ' -DCMAKE_CXX_COMPILER=', gpp_path, ' -DCMAKE_RC_COMPILER=', windres_path, ' -DCMAKE_MAKE_PROGRAM=', make_path, ' -DENABLE_PRECOMPILED_HEADERS=OFF -DOpenCV_ARCH=', openCVArch, ' -DOpenCV_RUNTIME=mingw -DENABLE_CXX11=ON -DBUILD_ZLIB=ON -DBUILD_opencv_world=OFF -DBUILD_opencv_contrib_world=OFF -DBUILD_matlab=OFF -DBUILD_opencv_java=OFF -DBUILD_opencv_python2=OFF -DBUILD_opencv_python3=OFF -DWITH_OPENMP=ON -DWITH_TBB=ON -DWITH_FFMPEG=ON -DWITH_OPENCL=ON -DWITH_EIGEN=ON -DWITH_OPENCLAMDFFT=ON -DWITH_OPENCLAMDBLAS=ON -DBUILD_PERF_TESTS=OFF -DBUILD_TESTS=OFF -DWITH_DSHOW=ON -DBUILD_PROTOBUF=OFF -DOPENCV_ENABLE_ALLOCATOR_STATS=OFF -DBUILD_SHARED_LIBS=ON -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX=', openCVPath, ' -B', buildDir, ' -H', sourceDir))
            } else {
              gcc_path <- paste0(rtoolsPath, "/mingw_", arch[i], "/bin/gcc.exe")
              gpp_path <- paste0(rtoolsPath, "/mingw_", arch[i], "/bin/g++.exe")
              windres_path <- paste0(rtoolsPath, "/mingw_", arch[i], "/bin/windres.exe")
              make_path <- paste0(rtoolsPath, "/mingw_", arch[i], "/bin/mingw32-make.exe")

              system(paste0('cmake -G "Unix Makefiles" -DCMAKE_C_COMPILER=', gcc_path, ' -DCMAKE_CXX_COMPILER=', gpp_path, ' -DCMAKE_RC_COMPILER=', windres_path, ' -DCMAKE_MAKE_PROGRAM=', make_path, ' -DENABLE_PRECOMPILED_HEADERS=OFF -DOpenCV_ARCH=', openCVArch, ' -DOpenCV_RUNTIME=mingw -DENABLE_CXX11=ON -DBUILD_opencv_gapi=OFF -DBUILD_ZLIB=ON -DBUILD_opencv_world=OFF -DBUILD_opencv_contrib_world=OFF -DBUILD_matlab=OFF -DBUILD_opencv_java=OFF -DBUILD_opencv_python2=OFF -DBUILD_opencv_python3=OFF -DWITH_OPENMP=ON -DWITH_TBB=ON -DWITH_FFMPEG=ON -DWITH_OPENCL=ON -DWITH_EIGEN=ON -DWITH_OPENCLAMDFFT=ON -DWITH_OPENCLAMDBLAS=ON -DBUILD_PERF_TESTS=OFF -DBUILD_TESTS=OFF -DWITH_DSHOW=ON -DBUILD_PROTOBUF=OFF -DOPENCV_ENABLE_ALLOCATOR_STATS=OFF -DBUILD_SHARED_LIBS=ON -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX=', openCVPath, ' -B', buildDir, ' -H', sourceDir))
            }

            system(paste0(make_path, " -j", parallel::detectCores(), " -C ", buildDir))
            system(paste0(make_path, " -C", buildDir, " install"))
          }
        }
      }
    } else {
      dir.create(openCVPath, showWarnings = FALSE)
      tmpDir <- base::tempdir()
      dir.create(tmpDir, showWarnings = FALSE)

      utils::download.file("https://github.com/opencv/opencv/archive/4.5.2.zip",
                           paste0(tmpDir, "/opencv-4.5.2.zip"))
      utils::unzip(paste0(tmpDir, "/opencv-4.5.2.zip"),
                   exdir = tmpDir)

      file.copy(paste0(pkgPath, "/OpenCVModule.4.5.2.cmake"),
                paste0(tmpDir, "/opencv-4.5.2/cmake/OpenCVModule.cmake"),
                overwrite = TRUE)

      sourceDir <- paste0(tmpDir, "/opencv-4.5.2/")
      buildDir <- paste0(sourceDir, "build")
      dir.create(buildDir, showWarnings = FALSE)

      if (grepl("Apple clang", system("c++ --version", intern = TRUE))[1]) {
        system(paste0("cmake -DWITH_IPP=ON -DBUILD_opencv_world=OFF -DBUILD_opencv_contrib_world=OFF -DBUILD_opencv_matlab=OFF -DBUILD_opencv_java=OFF -DBUILD_opencv_python2=OFF -DBUILD_opencv_python3=ON -DPYTHON3_LIBRARY={conda_env}/lib/python{pyV}.dylib -DPYTHON3_INCLUDE_DIR={conda_env}/include/python{pyV} -DPYTHON3_EXECUTABLE={conda_env}/bin/python -DPYTHON3_PACKAGES_PATH={conda_env}/lib/python{pyV}/site-packages -DWITH_TBB=ON -DWITH_FFMPEG=ON -DWITH_AVFOUNDATION=ON -DWITH_OPENCL=ON -DWITH_EIGEN=ON -DWITH_OPENCLAMDFFT=ON -DWITH_OPENCLAMDBLAS=ON -DBUILD_PERF_TESTS=OFF -DBUILD_TESTS=OFF -DINSTALL_CREATE_DISTRIB=ON -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX=", openCVPath, " -B", buildDir, ' -H', sourceDir))

        # system(glue::glue("cmake -DENABLE_PRECOMPILED_HEADERS=OFF -DWITH_IPP=ON -DBUILD_opencv_world=OFF -DBUILD_opencv_contrib_world=OFF -DBUILD_opencv_matlab=OFF -DBUILD_opencv_java=OFF -DBUILD_opencv_python2=OFF -DBUILD_opencv_python3=ON -DPYTHON3_LIBRARY={conda_env}/lib/python{pyV}.dylib -DPYTHON3_INCLUDE_DIR={conda_env}/include/python{pyV} -DPYTHON3_EXECUTABLE={conda_env}/bin/python -DPYTHON3_PACKAGES_PATH={conda_env}/lib/python{pyV}/site-packages -DWITH_TBB=ON -DWITH_FFMPEG=OFF -DWITH_OPENCL=ON -DWITH_EIGEN=ON -DWITH_OPENCLAMDFFT=ON -DWITH_OPENCLAMDBLAS=ON -DBUILD_PERF_TESTS=OFF -DBUILD_TESTS=OFF -DINSTALL_CREATE_DISTRIB=ON -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX={openCVPath} -B{buildDir} -H{sourceDir}"))

      } else {
        system(paste0("cmake -DWITH_IPP=ON -DBUILD_opencv_world=OFF -DBUILD_opencv_contrib_world=OFF -DBUILD_opencv_matlab=OFF -DBUILD_opencv_java=OFF -DBUILD_opencv_python2=OFF -DBUILD_opencv_python3=OFF -DWITH_OPENMP=ON -DWITH_TBB=ON -DWITH_FFMPEG=ON -DWITH_V4L=ON -DWITH_OPENCL=ON -DWITH_EIGEN=ON -DWITH_OPENCLAMDFFT=ON -DWITH_OPENCLAMDBLAS=ON -DBUILD_PERF_TESTS=OFF -DBUILD_TESTS=OFF -DINSTALL_CREATE_DISTRIB=ON -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX=", openCVPath, " -B", buildDir, ' -H', sourceDir))

      }

      system(paste0("make -j", parallel::detectCores(), " -C ", buildDir))
      system(paste0("make -C ", buildDir, " all install"))
    }
  } else {
    message("OpenCV was not installed at this time. You can install it at any time by using the installOpenCV() function.")
  }

  isOpenCVInstalled()

}
