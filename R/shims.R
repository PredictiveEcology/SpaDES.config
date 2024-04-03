## based on https://github.com/rstudio/renv/blob/main/R/shims.R

the$shims <- new.env(parent = emptyenv())

shim_create <- function(shim, sham) {
  formals(shim) <- formals(sham)
  shim
}

shims_activate <- function() {
  shims_deactivate()

  get_module_shim <- shim_create(shim_get_module, shim_get_module)
  assign("getModule", get_module_shim, envir = the$shims)

  args <- list(the$shims, name = "SpaDES.config:shims", warn.conflicts = FALSE)
  do.call(base::attach, args)
}

shims_deactivate <- function() {
  while ("SpaDES.config:shims" %in% search())
    detach("SpaDES.config:shims")
}

# determine whether can safely handle a call to `SpaDES.project::getModule()`
shim_get_module_compatible <- function() {
  matched <- match.call(SpaDES.project::getModule)
  ok <- c("", "modules", "modulePath", "overwrite", "verbose")
  unhandled <- setdiff(names(matched), ok)
  length(unhandled) == 0L
}

#'  Override `SpaDES.project::getModule()` to use the project's version of the module.
#'
#' @param modules See `?SpaDES.project::getModule`.
#' @param modulePath See `?SpaDES.project::getModule`.
#' @param overwrite See `?SpaDES.project::getModule`.
#' @param verbose See `?SpaDES.project::getModule`.
#'
#' @return See `?SpaDES.project::getModule`.
#'
shim_get_module <- function(modules, modulePath, overwrite, verbose) {
  if (!shim_get_module_compatible()) {
    call <- sys.call()
    call[[1L]] <- quote(SpaDES.project::getModule)
    return(eval(call, envir = parent.frame()))
  }

  ## otherwise, invoke our version
  call <- sys.call()
  call[[1L]] <- quote(SpaDES.config::use_project_module)

  ## evaluate call
  eval(call, envir = parent.frame())
}
