#' Symlink to project configs for tests
#'
#' @param prjMod character, specifying file path to project config module
#'
#' @returns NULL (invisibly)
#'
#' @keywords internal
.linkProject <- function(prjMod) {
  boxDir <- file.path("tests", "testthat", "box") |> normPath()
  boxMod <- file.path(boxDir, basename(prjMod))

  if (!dir.exists(boxDir)) {
    dir.create(boxDir)
  }
  if (!file.exists(boxMod)) {
    file.symlink(prjMod, boxMod)
  }
  stopifnot(is.symlink(boxMod))

  return(invisible(NULL))
}
