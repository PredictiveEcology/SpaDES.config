#' Default project paths
#'
#' @keywords internal
#' @rdname dot-basePaths
.baseCachePath <- "cache"

#' @keywords internal
#' @rdname dot-basePaths
.baseDataCachePath <- NULL ## TODO: standardize use of this!

#' @keywords internal
#' @rdname dot-basePaths
.baseModulePath <- "modules"

#' @keywords internal
#' @rdname dot-basePaths
.baseInputPath <- "inputs"

#' @keywords internal
#' @rdname dot-basePaths
.baseOutputPath <- "outputs"

#' @keywords internal
#' @rdname dot-basePaths
.baseLogPath <- file.path(.baseOutputPath, "log")

#' @rdname imports-internal
.isAbsolutePath <- function(pathnames) {
  keep <- is.character(pathnames)
  if (isFALSE(keep))
    stop("pathnames must be character")
  origPn <- pathnames
  nPathnames <- length(pathnames)
  if (nPathnames == 0L)
    return(logical(0L))
  if (nPathnames > 1L) {
    res <- sapply(pathnames, FUN = .isAbsolutePath)
    return(res)
  }
  if (is.na(pathnames))
    return(FALSE)
  if (regexpr("^~", pathnames) != -1L)
    return(TRUE)
  if (regexpr("^.:(/|\\\\)", pathnames) != -1L)
    return(TRUE)
  components <- strsplit(pathnames, split = "[/\\]")[[1L]]
  if (length(components) == 0L)
    return(FALSE)
  (components[1L] == "")
}

#' @keywords internal
.getRelativePath <- function(path, relativeToPath) {
  path <- normPath(path)
  relativeToPath <- normPath(relativeToPath)

  if (.isAbsolutePath(path)) {
    a <- unlist(strsplit(path, "/"))
    a <- a[nzchar(a)]

    b <- unlist(strsplit(relativeToPath, "/"))
    b <- b[nzchar(b)]

    id <- max(which(a %in% b)) ## assume most internal subdirectory is the matching one
    relPath <- do.call(file.path, as.list(a[(id + 1):length(a)]))
  } else {
    relPath <- path
  }
}

#' @keywords internal
.updateRelativePath <- function(path, relativeToPath) {
  if (is.null(path)) {
    path
  } else {
    if (.isAbsolutePath(path)) {
      path <- .getRelativePath(path, relativeToPath)
    }

    path
  }
}

#' @keywords internal
.updateLandWebOutputPath <- function(config) {
  .runName <- .landwebRunName(config$context, withRep = FALSE)

  if (config$context$mode == "postprocess") {
    file.path(.baseOutputPath, .runName)
  } else {
    file.path(.baseOutputPath, .runName, sprintf("rep%02d", config$context[["rep"]]))
  }
}

#' Get paths needed for SpaDES simulations
#'
#' Given a list of multiple project-related paths,
#' return the subset accepted by `SpaDES.core::setPaths()`.
#'
#' @param paths A named list of paths.
#'
#' @export
paths4spades <- function(paths) {
  if (requireNamespace("SpaDES.core", quietly = TRUE)) {
    want <- grep("Path$", names(formals(SpaDES.core::setPaths)), value = TRUE)
    paths[which(names(paths) %in% want)]
  } else {
    .needPkg("SpaDES.core", "stop")
  }
}