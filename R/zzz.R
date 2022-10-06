.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("Using SpaDES.config version ", utils::packageVersion("SpaDES.config"), ".")
  }
}
