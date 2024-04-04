#' Identify user or machine
#'
#' @param name Optional character string giving user or machine name to match.
#'
#' @return if `name` is non-`NULL`, returns a logical indicating whether
#' the current user/machine matches `name`.
#' Otherwise returns a character string with the value of the current user/machine.
#'
#' @export
#' @rdname whoami
user <- function(name = NULL) {
  if (is.null(name)) {
    Sys.info()[["user"]]
  } else {
    identical(name, Sys.info()[["user"]])
  }
}

#' @export
#' @rdname whoami
machine <- function(name = NULL) {
  if (is.null(name)) {
    Sys.info()[["nodename"]]
  } else {
    grepl(name, Sys.info()[["nodename"]])
  }
}

#' Notify the user of a missing package
#'
#' @param pkg character string of length 1 giving the name of the package
#' @param enforce character string; one of "message", "warning", "stop",
#'                which determines how the user is notified: by a message,
#'                a warning, or an error, respectively.
#'
#' @export
#' @importFrom utils getFromNamespace
#' @keywords internal
#' @examples
#' \dontrun{
#'   needPkg("raster", "message")
#'   needPkg("raster", "warning")
#'   needPkg("raster", "stop")
#' }
needPkg <- function(pkg, enforce = "stop") {
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

#' Establish database connection for Cache
#'
#' @param type character string giving the Cache type.
#'             One of `"sqlite"` or `"postgresql"`.
#'
#' @export
dbConnCache <- function(type = "sqlite") {
  conn <- if (type == "sqlite") {
    if (requireNamespace("RSQLite", quietly = TRUE)) {
      NULL
    } else {
      needPkg("RSQLite", "stop")
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
          needPkg("DBI", "stop")
        }
    } else {
      needPkg("RPostgres", "stop")
    }
  }

  return(conn)
}

#' Print current run information
#'
#' Print (via message) the current run context information using markdown table syntax.
#'
#' @param context A named list of run descriptors, including e.g.,
#'                the current user and machine names, the study area name, and replicate id.
#'
#' @return Invoked for its side-effect of printing to the screen,
#'         but invisibly returns the message string so it can e.g., be written to a file by the user.
#' @export
printRunInfo <- function(context) {
  context$print()
}

#' Format context info for markdown
#'
#' @param cntxt a `projContext` object
#'
#' @export
context2md <- function(cntxt) {
  stopifnot(is(cntxt, "list"))

  col1width <- max(nchar(names(cntxt)))
  col2width <- max(nchar(cntxt), na.rm = TRUE)

  info <- paste0(c(
    "# Run info\n\n",
    "| ", rep("-", col1width), " | ", rep("-", col2width), " |\n",
    sapply(names(cntxt), function(x) {
      col1text <- formatC(x, width = col1width, format = "s")
      col2text <- formatC(paste(cntxt[[x]], collapse = ","), width = col2width, format = "s")

      paste("|", col1text, "|", col2text, "|\n")
    }),
    "| ", rep("-", col1width), " | ", rep("-", col2width), " |\n"
  ))

  return(invisible(info))
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
authGoogle <- function(tryToken = NULL, tryEmail = NULL) {
  if (requireNamespace("googledrive", quietly = TRUE)) {
    if (hasToken(tryToken)) {
      googledrive::drive_auth(path = findToken(tryToken)) ## TODO: specify project path
    } else {
      message(crayon::red("No Google service account token found. Trying user authentication..."))
      if (!is.null(tryEmail) && !nzchar(tryEmail)) tryEmail <- NULL
      googledrive::drive_auth(email = tryEmail, use_oob = .isRstudioServer())
    }

    message(crayon::silver("Authenticating as: "),
            crayon::green(googledrive::drive_user()$emailAddress))
  } else {
    needPkg("googledrive", "stop")
  }
}

#' @keywords internal
findToken <- function(name, path = ".") {
  normPath(list.files(path, paste0(name, "-.*[.]json")[1]))
}

#' @keywords internal
hasToken <- function(name) {
  token <- findToken(name) ## TODO: path argument
  all(isTRUE(length(token) == 1), !is.na(token))
}

#' Delay subsequent code execution by random interval
#'
#' @param t integer vector from which to randomly draw a time (in minutes) to delay for
#'
#' @export
delay_rnd <- function(t = 1L:15L) {
  sample(t, 1)
}

#' Recursively modify elements of a list
#'
#' Wrapper around `utils::modifyList()` that can deal with more than two lists.
#' Based on `Require::modifyList3`.
#'
#' @param ... named lists (possibly empty) where the elements of the first list will
#'            be updated based on the values of subsequent lists.
#'
#' @param keep.null logical indicating whether `NULL` elements should be kept or deleted.
#'
#' @export
#' @importFrom utils modifyList
#' @seealso [utils::modifyList()]
modList <- function(..., keep.null = TRUE) {
  dots <- list(...)
  dots <- dots[!unlist(lapply(dots, is.null))]
  do.call(Reduce, alist(modifyList, dots))
}
