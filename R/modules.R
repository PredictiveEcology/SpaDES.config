#' Use SpaDES module from project directory
#'
#' Allows use of SpaDES modules that make nested module calls (i.e., submodules).
#' For projects using version control with *git* submodules, this version
#' simply creates a symlink to the project's version of the module.
#' This ensures the entire project stays in sync with itself, and
#' doesn't require installation of an additional package.
#'
#' @param modules character string giving the name, or potential GitHub source of a module
#'                (e.g., `"Biomass_core"` or `"PredictiveEcology/Biomass_core@development"`)
#' @param modulePath path to the submodule
#' @param overwrite not used
#' @param verbose not used
#' @export
use_project_module <- function(modules, modulePath, overwrite, verbose) {
  ## TODO: is modules always length 1?

  modules <- strsplit(modules, "/")[[1]][2] |>
    strsplit("@") |>
    (function(x) x[[1]][1])()

  if (!dir.exists(modulePath)) {
    projModulePath <- file.path(findProjectPath(), "modules", modules) ## TODO
    file.symlink(projModulePath, modulePath) ## TODO: confirm this creates dir junction on windows
  }

  if (is.symlink(modulePath)) {
    successes <- modules
    failed <- character(0)
  } else {
    successes <- character(0)
    failed <- modules
  }

  return(list(success = successes, failed = failed))
}
