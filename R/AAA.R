.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\nVersion 3.4 of the spsurvey package was loaded successfully.\n")
}

.onUnload <- function(libpath) {
  library.dynam.unload("spsurvey", libpath)
}
