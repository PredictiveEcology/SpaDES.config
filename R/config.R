#' Project config class
#'
#' @slot options Named list of R and R package options to be set.
#'
#' @slot params  Named list of named lists specifying simulation parameters.
#'               The names of the outermost list must correspond to modules
#'               (and may also include `.global`).
#'
#' @slot paths  Named list of paths, which should include (at minimum) the
#'              the paths in `SpaDES.core::setPaths`.
#'
#' @slot args   Named list of additional project arguments.
#'
#' @slot .Data  Not used.
#'
#' @author Alex Chubaty and Eliot McIntire
#' @exportClass projConfig
#' @rdname projConfig-class
setClass(
  "projConfig",
  contains = "list", ## TODO: use environment?
  slots = list(options = "list", params = "list", paths = "list", args = "list"),
  validity = function(object) {
    ## TODO
  }
)

### `initialize` generic is already defined in the methods package
#' Initialize a `projConfig` object
#'
#' Given the name or the definition of a class, plus optionally data to be
#' included in the object, `new` returns an object from that class.
#'
#' @inheritParams methods::initialize
#'
#' @param options Named list of R and R package options to be set.
#'
#' @param params  Named list of named lists specifying simulation parameters.
#'               The names of the outermost list must correspond to modules
#'               (and may also include `.global`).
#'
#' @param paths  Named list of paths, which should include (at minimum) the
#'              the paths in `SpaDES.core::setPaths`.
#'
#' @param args   Named list of additional project arguments.
#'
#' @export
#' @importFrom Require modifyList2 normPath
#' @rdname initialize-method
setMethod("initialize",
          signature(.Object = "projConfig"),
          definition = function(.Object, args = list(), options = list(),
                                params = list(), paths = list(), ...) {
            .Object <- callNextMethod()

            .Object@args <- args

            .Object@options <- options

            .Object@params <- modifyList2(list(.globals = list()), params)

            .Object@paths <- modifyList2(list(
              cachePath = "cache",
              inputPath = "inputs",
              modulePath = "modules",
              outputPath = "outputs",
              projectPath = normPath(".")
            ), paths)

            return(.Object)
})
