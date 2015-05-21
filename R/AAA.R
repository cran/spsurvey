.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\nVersion 3.0 of the spsurvey package was loaded successfully.\n")
}

.onUnload <- function(libpath) {
  library.dynam.unload("spsurvey", libpath)
}
