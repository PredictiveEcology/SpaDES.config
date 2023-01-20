#' Set options based on project config
#'
#' @param config a `projConfig` object
#'
#' @return invoked for its side-effect of setting options
#'         (invisibly returns a list of the previously set options)
#'
#' @export
#' @importFrom utils osVersion
setProjectOptions <- function(config) {
  stopifnot(is(config, "projConfig"))

  if (requireNamespace("raster", quietly = TRUE)) {
    raster::rasterOptions(default = TRUE)
  } else {
    .needPkg("raster", "stop")
  }

  message("Setting options specified in config$options...")
  opts <- options(config$options)

  if (requireNamespace("raster", quietly = TRUE)) {
    message("Setting httr option `http_version = 0`, which is needed to download",
            " from from certain websites.")
    httr::set_config(httr::config(http_version = 0))

    message("Setting httr timeout to 10 seconds.")
    httr::timeout(seconds = 10)

  } else {
    .needPkg("httr", "stop")
  }

  if (requireNamespace("tiler", quietly = TRUE)) {
    os <- strsplit(utils::osVersion, " ")[[1]][1]
    osVersion <- numeric_version(strsplit(utils::osVersion, " ")[[1]][2])
    if (isTRUE(os == "Ubuntu") && isTRUE(osVersion >= "20.04")) {
      tiler::tiler_options(python = Sys.which("python3"))
      message("Additonally setting tiler option `python = python3`.")
    }
  }

  return(invisible(opts))
}
