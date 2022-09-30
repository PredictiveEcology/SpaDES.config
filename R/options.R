#' Set options based on project config
#'
#' @param config a `projConfig` object
#'
#' @return invoked for its side-effect of setting options
#'         (invisibly returns a list of the previously set options)
#'
#' @export
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
    httr::set_config(httr::config(http_version = 0)) ## TODO: is this still needed??
    message("Additonally setting httr option `http_version = 0`, which is needed to download",
            " from from certain websites.")
  } else {
    .needPkg("httr", "stop")
  }

  return(invisible(opts))
}
