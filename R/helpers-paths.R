#' Default project paths
#'
#' @param type character. one of 'cache', 'modules', 'inputs', 'outputs', 'log'.
#'
#' @return character representing the project's default `type` path of type.
#'         if `type` is `NULL` (i.e., not specified), returns a named list of default paths.
#'
#' @export
#' @rdname projectPaths
projectPaths <- function(type = NULL) {
  ## allow some variations (i.e., plurals) to be used as aliases
  defaultPaths <- list(
    cache = "cache",
    module = "modules",
    modules = "modules",
    input = "inputs",
    inputs = "inputs",
    output = "outputs",
    outputs = "outputs",
    log = file.path("outputs", "log"),
    logs = file.path("outputs", "log")
  )

  if (is.null(type)) {
    defaultPaths
  } else {
    defaultPaths[[type]]
  }
}

#' Similar to e.g., `fs::path_rel` but from the 'other end' of the path,
#' i.e., working from the right (end) of the path instead of the left (beginning).
#' This allows extracting the project-relative paths for a project in different
#' directories, which is useful when some project files (e.g., cache, inputs, outputs)
#' are stored on a network drive and symlinked from the project directory.
#'
#' ```r
#' ## here, assume p0 is a symlink pointing to p1;
#' ## starting from the end, each of these paths have `myProject/` in common,
#' ## with `outputs/` being the result relative to this path.
#' p0 <- "~/GitHub/myProject/outputs" ## symlink to p1
#' p1 <- "/mnt/projects/myProject/outputs"
#' p2 <- "~/GitHub/myProject"
#' .getRelativePath(p0, p2) ## "outputs"
#' .getRelativePath(p1, p2) ## "outputs"
#' ```
#'
#' @importFrom fs is_absolute_path
#' @keywords internal
.getRelativePath <- function(path, relativeToPath) {
  path <- normPath(path)
  relativeToPath <- normPath(relativeToPath)

  if (fs::is_absolute_path(path)) {
    a <- unlist(strsplit(path, "/"))
    a <- a[nzchar(a)]

    b <- unlist(strsplit(relativeToPath, "/"))
    b <- b[nzchar(b)]

    ## assume most internal subdirectory is the matching one
    id <- max(which(a %in% b))
    relPath <- do.call(file.path, as.list(a[(id + 1):length(a)]))
  } else {
    relPath <- path
  }
}
.getRelativePath <- Vectorize(.getRelativePath, USE.NAMES = FALSE)

#' @keywords internal
.updateRelativePath <- function(path, relativeToPath) {
  if (is.null(path)) {
    path
  } else {
    if (fs::is_absolute_path(path)) {
      path <- .getRelativePath(path, relativeToPath)
    }

    path
  }
}
.updateRelativePath <- Vectorize(.updateRelativePath, USE.NAMES = FALSE)

#' Update project config output path
#'
#' @param config a `projConfig` object
#'
#' @param runNameFun function that computes a run name, returning a character string.
#'
#' @export
updateOutputPath <- function(config, runNameFun) {
  .runName <- runNameFun(config$context, withRep = FALSE)

  if ("postprocess" %in% config$context$mode) {
    file.path(projectPaths("outputs"), .runName)
  } else {
    file.path(projectPaths("outputs"), .runName, sprintf("rep%02d", config$context[["rep"]]))
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
    if (is.null(paths[["rasterPath"]]) && !is.null(paths[["scratchPath"]])) {
      paths[["rasterPath"]] <- file.path(paths[["scratchPath"]], "raster")
    }
    if (is.null(paths[["terraPath"]]) && !is.null(paths[["scratchPath"]])) {
      paths[["terraPath"]] <- file.path(paths[["scratchPath"]], "terra")
    }
    paths[which(names(paths) %in% want)]
  } else {
    needPkg("SpaDES.core", "stop")
  }
}

#' Find the project root directory
#'
#' Searches from current working directory for an Rstudio project file
#' or git repository, falling back on using the current working directory.
#'
#' @return `findProjectPath` returns an absolute path;
#'         `findProjectName` returns the basename of the path.
#'
#' @export
#' @importFrom rprojroot find_root from_wd is_git_root is_rstudio_project
#' @rdname findProject
findProjectPath <- function() {
  find_root(is_rstudio_project | is_git_root | from_wd, path = getwd())
}

#' @export
#' @rdname findProject
findProjectName <- function() {
  basename(findProjectPath())
}

#' Normalize a file path
#'
#' @param path character string giving a file path
#'
#' @export
#' @importFrom fs path_expand_r path_norm
normPath <- function(path) {
  unlist(path) |>
    fs::path_norm() |>
    fs::path_expand_r() |>
    normalizePath(winslash = "/", mustWork = FALSE)
}

## copied from example in `?Sys.readlink`
is.symlink <- function(paths) {
  isTRUE(nzchar(Sys.readlink(paths), keepNA = TRUE))
}
