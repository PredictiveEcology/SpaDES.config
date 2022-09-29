#' Update LandWeb project context
#'
#' @param context a LandWeb context object created by `useContext("LandWeb", ...)`
#' @param config a `landwebConfig` object
#'
#' @return an updated context object
#' @export
updateLandWebContext <- function(context, config) {
  relOutDir <- .getRelativePath(config$paths$outputPath, config$paths$projectPath)

  context$runName <- paste(strsplit(relOutDir, "/")[[1]][-1], collapse = "_")

  return(context)
}
