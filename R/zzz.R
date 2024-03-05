.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("Using SpaDES.config version ", utils::packageVersion("SpaDES.config"), ".")
  }
}

.onLoad <- function(libname, pkgname) {
  shims_activate()
}
