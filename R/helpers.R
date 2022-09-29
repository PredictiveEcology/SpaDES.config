#' Notify the user of a missing package
#'
#' @param pkg character string of length 1 giving the name of the package
#' @param enforce character string; one of "message", "warning", "stop",
#'                which determines how the user is notified: by a message,
#'                a warning, or an error, respectively.
#'
#' @importFrom utils getFromNamespace
#' @keywords internal
#' @examples
#' \dontrun{
#'   .needPkg("raster", "message")
#'   .needPkg("raster", "warning")
#'   .needPkg("raster", "stop")
#' }
.needPkg <- function(pkg, enforce = "stop") {
  stopifnot(length(pkg) == 1, enforce %in% c("message", "warning", "stop"))

  msg <- paste("Package", pkg, "is not available.",
               "Please ensure it is installed.")
  enforceFun <- utils::getFromNamespace(enforce, "base")
  enforceFunArgs <- list(
    msg,
    call. = FALSE
  )
  if (enforce == "message") enforceFunArgs[["call."]] <- NULL
  do.call(enforceFun, enforceFunArgs)
}

#' Imports from other packages
#'
#' Unexported functions from other packages; used internally.
#'
#' @note `quickPlot` and `reproducible` are both dependencies of `SpaDES.core`
#'       and should therefore be available, but we've copied the functions here
#'       to avoid explicitly adding dependencies.
#'
#' @keywords internal
#' @rdname imports-internal
.isRstudioServer <- function() {
  isRstudioServer <- FALSE
  if (isTRUE("tools:rstudio" %in% search())) {
    rsAPIFn <- get(".rs.api.versionInfo", as.environment("tools:rstudio"))
    versionInfo <- rsAPIFn()
    if (!is.null(versionInfo)) {
      isRstudioServer <- identical("server", versionInfo$mode)
    }
  }
  isRstudioServer
}

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

    file.path(relativeToPath, path)
  }
}

#' Establish database connection for Cache
#'
#' @param type character string giving the Cache type.
#'             One of `"sql"` or `"postgresql"`.
#'
#' @export
#' @importFrom Require Require
dbConnCache <- function(type = "sql") {
  conn <- if (type == "sql") {
    if (requireNamespace("RSQLite", quietly = TRUE)) {
      NULL
    } else {
      .needPkg("RSQLite", "stop")
    }
  } else if (type == "postgresql") {
    if (requireNamespace("RPostgres", quietly = TRUE)) {
      if (requireNamespace("DBI", quietly = TRUE)) {
        DBI::dbConnect(drv = RPostgres::Postgres(),
                       host = Sys.getenv("PGHOST"),
                       port = Sys.getenv("PGPORT"),
                       dbname = Sys.getenv("PGDATABASE"),
                       user = Sys.getenv("PGUSER"),
                       password = Sys.getenv("PGPASSWORD"))
        } else {
          .needPkg("DBI", "stop")
        }
    } else {
      .needPkg("RPostgres", "stop")
    }
  }

  return(conn)
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

#' Print current run information
#'
#' Print (as a message) the current run context information.
#'
#' @param context A named list of run descriptors, including e.g.,
#'                the current user and machine names, the study area name, and replicate id.
#'
#' @export
printRunInfo <- function(context) {
  message(
    rep("*", getOption("width")), "\n",
    "Run info:\n",
    rep("-", getOption("width")), "\n",
    lapply(names(context), function(x) {
      paste(sprintf("  %16s:\t", x), context[[x]], "\n")
    }),
    rep("*", getOption("width"))
  )
}

#' Google Drive authentication
#'
#' Attempts authentication using Google Service Account Token if found,
#' falling back to the specified email address.
#'
#' @param tryToken the shorthand name for the token, corresponding to the token filename
#'                 (e.g., if filename is `exampleproject-123456-abcdef789012.json`,
#'                 use `tryToken = "exampleproject"`).
#'
#' @param tryEmail character string giving the user email address to use.
#'
#' @export
#' @importFrom crayon green red silver
authGoogle <- function(tryToken, tryEmail) {
  if (requireNamespace("googledrive", quietly = TRUE)) {
    if (hasToken(tryToken)) {
      googledrive::drive_auth(path = findToken(tryToken)) ## TODO: specify project path
    } else {
      message(crayon::red("No Google service account token found. Trying user authentication..."))
      googledrive::drive_auth(email = tryEmail, use_oob = .isRstudioServer())
    }

    message(crayon::silver("Authenticating as: "),
            crayon::green(googledrive::drive_user()$emailAddress))
  } else {
    .needPkg("googledrive", "stop")
  }
}

#' @importFrom Require normPath
#' @keywords internal
findToken <- function(name, path = ".") {
  normPath(list.files(path, paste0(name, "-.*[.]json")[1]))
}

#' @keywords internal
hasToken <- function(name) {
  token <- findToken(name) ## TODO: path argument
  all(isTRUE(length(token) == 1), !is.na(token))
}
